FROM python:3.10-slim

# For Logs
ENV PYTHONUNBUFFERED=1

WORKDIR /app

# Dependencies for the system
RUN apt-get update && apt-get install -y \
    build-essential \
    git \
    && rm -rf /var/lib/apt/lists/*

# Requirements 
COPY requirements.txt .

RUN pip install --no-cache-dir -r requirements.txt

# Copy all needed code
COPY . .

# Streamlit Port
EXPOSE 8501

# App execution
CMD ["streamlit", "run", "app.py", "--server.port=8501", "--server.address=0.0.0.0"]