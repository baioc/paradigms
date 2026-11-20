FROM python:3.14-slim

ENV SERVER_PORT=8000 \
    TRIAGE_DB_PATH=/data/triage.db \
    SERVER_WWW_PATH=/app/static \
    ANALYST_LANGUAGE=pt-BR \
    PYTHONUNBUFFERED=1

WORKDIR /app

COPY requirements.txt ./
RUN pip install --no-cache-dir -r requirements.txt

COPY triage.py ./
COPY index.html static/index.html

RUN mkdir -p /data

VOLUME ["/data"]
EXPOSE 8000

CMD ["sh", "-c", "/usr/local/bin/python triage.py 2>&1 | tee -a /data/server.log"]
