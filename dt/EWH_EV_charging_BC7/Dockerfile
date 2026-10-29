FROM python:3.12-slim

WORKDIR /app

RUN pip install --no-cache-dir pulp==4.0.0 highspy==1.15.1 requests paho-mqtt==2.1.0

COPY src/mpc/ ./src/mpc/
COPY run_mpc_service.py .

# -u: unbuffered, so `docker logs` shows messages immediately
CMD ["python", "-u", "run_mpc_service.py"]
