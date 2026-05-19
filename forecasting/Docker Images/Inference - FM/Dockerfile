FROM python:3.10-slim

ENV PYTHONUNBUFFERED=1

RUN apt-get update && apt-get install -y \
    git \
    && rm -rf /var/lib/apt/lists/*


WORKDIR /app


COPY requirements_api_fm.txt .

RUN pip install --no-cache-dir -r requirements_api_fm.txt

COPY main_fm.py .

EXPOSE 8000

CMD ["uvicorn", "main_fm:app", "--host", "0.0.0.0", "--port", "8000"]
