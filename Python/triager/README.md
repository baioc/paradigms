# triager.py

Servidor simples para triagem e análise de malware, que consulta um hash no
VirusTotal, pede a um modelo de linguagem um veredito sobre o arquivo, guarda o
histórico de triagens em um banco de dados SQLite e disponibiliza essas
funcionalidades através de uma interface web simples.

## API

| Endpoint           | Descrição                                                |
|--------------------|----------------------------------------------------------|
| `GET /`            | interface web                                            |
| `POST /file`       | submete um hash para análise; corpo `{"hash": "..."}`    |
| `GET /files`       | lista os arquivos analisados, com `?limit=` e `?offset=` |
| `GET /file/<hash>` | retorna a última análise armazenada de um hash           |

## Configuração

| Variável de ambiente | Descrição                                               | Obrigatória? |
|----------------------|---------------------------------------------------------|--------------|
| `VIRUSTOTAL_API_KEY` | chave de API do VirusTotal                              | **Sim**      |
| `OPENAI_BASE_URL`    | URL base de endpoint compatível com a API da OpenAI     | **Sim**      |
| `OPENAI_API_KEY`     | chave de API desse endpoint                             | **Sim**      |
| `OPENAI_MODEL`       | nome do modelo LLM a ser utilizado                      | **Sim**      |
| `SERVER_PORT`        | porta exposta pelo servidor HTTP (default=`8000`)       | Não          |
| `ANALYST_LANGUAGE`   | linguagem de saída do modelo (default=`pt-BR`)          | Não          |
| `SYSTEM_PROMPT`      | prompt de sistema para análise do malware               | Não          |
| `TRIAGE_DB_PATH`     | path para banco de dados sqlite (default=`./triage.db`) | Não          |
| `SERVER_WWW_PATH`    | path para a pasta da interface web (default=`.`)        | Não          |

## Rodando localmente

```bash
python3 -m venv .venv
source .venv/bin/activate
pip install -r requirements.txt

# dica: o OpenRouter possui uma API compativel e modelos gratuitos
export VIRUSTOTAL_API_KEY=...
export OPENAI_BASE_URL='https://openrouter.ai/api/v1'
export OPENAI_MODEL='openrouter/free'
export OPENAI_API_KEY=...

python3 triage.py
# o servidor estará disponível em http://localhost:8000
```

## Rodando com Docker

```bash
sudo docker build -t triager .
# verifique as variaveis de ambiente obrigatorias em .venv
sudo docker run -d --name triager -p 80:8000 -v ./data:/data --env-file .env triager
# o servidor estará exposto na porta 80 do host, com dados persistentes em ./data/
```

Um volume é mapeado na pasta local `./data` (que é criada caso não exista) para
persistência do banco de dados e de logs do servidor.
