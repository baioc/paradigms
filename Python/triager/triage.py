#!/usr/bin/env python3

from collections import OrderedDict
from contextlib import closing
from datetime import datetime, timezone
from typing import Optional, Tuple
import json
import os
import sqlite3

from flask import Flask, request, jsonify, send_from_directory
from langchain_openai import ChatOpenAI
import requests


##
# app state
##

flask = Flask(__name__)

vt = None

llm = None

prompt = None

lang = None

db = None

www = None


##
# core logic
##

def get_virustotal(hash: str) -> Optional[dict]:
    r = vt.get(f"https://www.virustotal.com/api/v3/files/{hash}", timeout=(5, 15))
    if r.status_code != 200:
        return None

    attrs = r.json()["data"]["attributes"]
    data = {
        "hash": hash,
        "size": attrs["size"],
        "magic": attrs.get("magic", "data"),
        "type": attrs.get("type_description", "unknown"),
        "names": attrs.get("names", []),
        "tags": attrs["tags"],
        "first_submission": attrs["first_submission_date"],
        "last_submission": attrs["last_submission_date"],
        "analysis_date": attrs["last_analysis_date"],
        "threat_label": attrs.get("popular_threat_classification", {})
                             .get("suggested_threat_label", "inconclusive"),
        "stats": attrs["last_analysis_stats"],
        "sigma_results": attrs.get("sigma_analysis_results", []),
    }

    return data


def llm_analyze(data: dict) -> str:
    aimsg = llm.invoke([
        {"role": "system", "content": prompt + f"\nLanguage to respond in: {lang}"},
        {"role": "user", "content": json.dumps(data, ensure_ascii=False)},
        {"role": "system", "content": "Security analyst response: "},
    ])
    analysis = aimsg.content
    return analysis


def connect() -> sqlite3.Connection:
    conn = sqlite3.connect(db)
    conn.execute("PRAGMA foreign_keys = ON")
    conn.row_factory = sqlite3.Row
    return conn


__STATS_COLUMNS = OrderedDict([
    ("malicious", "stats_malicious"),
    ("suspicious", "stats_suspicious"),
    ("undetected", "stats_undetected"),
    ("harmless", "stats_harmless"),
    ("timeout", "stats_timeout"),
    ("confirmed-timeout", "stats_confirmed_timeout"),
    ("failure", "stats_failure"),
    ("type-unsupported", "stats_type_unsupported"),
])


def triage_store(data: dict, analysis: str) -> None:
    hash = data["hash"]
    date = data["analysis_date"]
    stats = [data["stats"].get(k) for k in __STATS_COLUMNS]

    with closing(connect()) as conn:
        conn.execute(
            """INSERT INTO files (hash, size, magic, type)
               VALUES (?, ?, ?, ?)
               ON CONFLICT(hash) DO UPDATE SET
                   size = excluded.size,
                   magic = excluded.magic,
                   type = excluded.type""",
            (
                hash,
                data["size"],
                data["magic"],
                data["type"],
            ),
        )

        conn.executemany(
            "INSERT OR IGNORE INTO file_names (hash, name) VALUES (?, ?)",
            [(hash, name) for name in data["names"]],
        )

        conn.executemany(
            "INSERT OR IGNORE INTO file_tags (hash, tag) VALUES (?, ?)",
            [(hash, tag) for tag in data["tags"]],
        )

        conn.execute(
            """INSERT INTO analyses (hash, date,
                                     raw_data,
                                     system_prompt, analyst_language, llm_output,
                                     first_submission, last_submission,
                                     threat_label,
                                     stats_malicious, stats_suspicious, stats_undetected,
                                     stats_harmless, stats_timeout, stats_confirmed_timeout,
                                     stats_failure, stats_type_unsupported, stats_total)
               SELECT ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?
               WHERE NOT EXISTS ( SELECT 1 FROM analyses WHERE hash = ? AND date >= ? )""",
            (
                hash, date,
                json.dumps(data, ensure_ascii=False),
                prompt, lang, analysis,
                data["first_submission"], data["last_submission"],
                data["threat_label"],
                *stats, sum(s or 0 for s in stats),
                hash, date,
            ),
        )

        conn.commit()


def triage_latest(hash: str) -> Optional[Tuple[dict, str]]:
    with closing(connect()) as conn:
        row = conn.execute(
            """SELECT raw_data, llm_output
               FROM analyses
               WHERE hash = ?
               ORDER BY date DESC
               LIMIT 1""",
            (hash,),
        ).fetchone()

    if row is None:
        return None

    data = json.loads(row["raw_data"])
    analysis = row["llm_output"]

    return data, analysis


def triage_list(limit: int = 100, offset: int = 0) -> tuple[list[str], int]:
    with closing(connect()) as conn:
        total = conn.execute("SELECT COUNT(*) FROM files").fetchone()[0]
        hashes = [row["hash"] for row in conn.execute(
            "SELECT hash FROM files ORDER BY rowid DESC LIMIT ? OFFSET ?",
            (limit, offset),
        )]

    return hashes, total


##
# api endpoints
##

@flask.route("/file", methods=["POST"])
def file():
    try:
        req = request.get_json()
        if not req or "hash" not in req:
            return jsonify({"error": "missing \"hash\" field in request body"}), 400

        hash = req["hash"]
        old_record = triage_latest(hash)
        data = get_virustotal(hash)

        if not data and not old_record:
            return jsonify({"error": f"could not find information on '{hash}'"}), 404
        elif not data and old_record:
            data, analysis = old_record
            return jsonify({"data": data, "analysis": analysis}), 200
        elif data and not old_record:
            analysis = llm_analyze(data)
            triage_store(data, analysis)
            return jsonify({"data": data, "analysis": analysis}), 200
        else:
            assert(data and old_record)
            old_data, old_analysis = old_record
            if data["analysis_date"] > old_data["analysis_date"]:
                analysis = llm_analyze(data)
                triage_store(data, analysis)
                return jsonify({"data": data, "analysis": analysis}), 200
            else:
                return jsonify({"data": old_data, "analysis": old_analysis}), 200

    except Exception as e:
        return jsonify({"error": str(e)}), 500


@flask.route("/files", methods=["GET"])
def files():
    try:
        offset = int(request.args.get("offset") or 0)
        limit = int(request.args.get("limit") or 100)
    except (TypeError, ValueError):
        return jsonify({"error": "\"limit\" and \"offset\" must be integers"}), 400

    limit = min(limit, 500)
    if limit < 1:
        return jsonify({"error": "\"limit\" must be at least 1"}), 400
    if offset < 0:
        return jsonify({"error": "\"offset\" cannot be negative"}), 400

    try:
        triaged, total = triage_list(limit, offset)
    except Exception as e:
        return jsonify({"error": str(e)}), 500

    return jsonify({"offset": offset, "limit": limit, "total": total, "files": triaged}), 200


@flask.route("/file/<hash>", methods=["GET"])
def stored_file(hash: str):
    try:
        record = triage_latest(hash)
        if not record:
            return jsonify({"error": f"no stored triage data for '{hash}'"}), 404

        data, analysis = record
        return jsonify({"data": data, "analysis": analysis}), 200

    except Exception as e:
        return jsonify({"error": str(e)}), 500

@flask.route("/")
def index():
    return send_from_directory(www, "index.html")


##
# main
##

vt_key = os.getenv("VIRUSTOTAL_API_KEY")
if not vt_key: raise RuntimeError("environment variable VIRUSTOTAL_API_KEY is not set")

vt = requests.Session()
vt.headers.update({
    "accept": "application/json",
    "x-apikey": vt_key,
})

base_url = os.getenv("OPENAI_BASE_URL")
api_key = os.getenv("OPENAI_API_KEY")
model = os.getenv("OPENAI_MODEL")

if not base_url: raise RuntimeError("environment variable OPENAI_BASE_URL is not set")
if not api_key: raise RuntimeError("environment variable OPENAI_API_KEY is not set")
if not model: raise RuntimeError("environment variable OPENAI_MODEL is not set")

llm = ChatOpenAI(
    base_url=base_url,
    api_key=api_key,
    model=model,
)

prompt = os.getenv("SYSTEM_PROMPT", (
    "Whether in the midst of an incident response process, or as a cyber threat intelligence analyst, "
    "you're given the following JSON-encoded data (enriched via multiple sources) about a suspicious file. "
    "You must respond in the language specified below, with a short summary of the data, a seriour verdict "
    "on what this file is, as well as some recommended steps (e. things that need to be further "
    "investigated, or immediate actions to take in an enterprise environment). Draw from your previous "
    "experiences and knowledge too, and remember to write in plain text paragraphs (not markdown either)."
))
lang = os.getenv("ANALYST_LANGUAGE", "pt-BR")

db = os.getenv("TRIAGE_DB_PATH", "triage.db")

schema = """
CREATE TABLE IF NOT EXISTS files (
    hash  TEXT PRIMARY KEY,
    size  INTEGER,
    magic TEXT,
    type  TEXT
);

CREATE TABLE IF NOT EXISTS file_names (
    hash TEXT NOT NULL REFERENCES files(hash) ON DELETE CASCADE,
    name TEXT NOT NULL,
    PRIMARY KEY (hash, name)
);

CREATE TABLE IF NOT EXISTS file_tags (
    hash TEXT NOT NULL REFERENCES files(hash) ON DELETE CASCADE,
    tag  TEXT NOT NULL,
    PRIMARY KEY (hash, tag)
);

CREATE TABLE IF NOT EXISTS analyses (
    hash                    TEXT NOT NULL REFERENCES files(hash) ON DELETE CASCADE,
    date                    INTEGER NOT NULL,
    raw_data                TEXT,
    system_prompt           TEXT,
    analyst_language        TEXT,
    llm_output              TEXT,
    first_submission        INTEGER,
    last_submission         INTEGER,
    threat_label            TEXT,
    stats_malicious         INTEGER,
    stats_suspicious        INTEGER,
    stats_undetected        INTEGER,
    stats_harmless          INTEGER,
    stats_timeout           INTEGER,
    stats_confirmed_timeout INTEGER,
    stats_failure           INTEGER,
    stats_type_unsupported  INTEGER,
    stats_total             INTEGER,
    PRIMARY KEY (hash, date)
);
"""

with closing(connect()) as conn:
    conn.executescript(schema)
    conn.commit()

www = os.getenv("SERVER_WWW_PATH", ".")

if __name__ == "__main__":
    port = int(os.getenv("SERVER_PORT", "8000"))

    print(
        f"SERVER_PORT={port}",
        f"OPENAI_BASE_URL={base_url}",
        f"OPENAI_MODEL={model}",
        f"SYSTEM_PROMPT={prompt}",
        f"ANALYST_LANGUAGE={lang}",
        f"TRIAGE_DB_PATH={os.path.abspath(db)}",
        sep='\n',
    )

    flask.run(host="0.0.0.0", port=port)
