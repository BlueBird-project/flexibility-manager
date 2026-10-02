# Long-running inference service for the EV-charging DQN (service/inference_server.py).
# Does NOT include training: no pandas/openpyxl/matplotlib, no dataset. Retrain on the
# host with `python main.py train`, then mount the resulting saved_models/<run>/ folder
# into this container at /models (see MODEL_DIR below).

FROM python:3.11-slim

WORKDIR /app

# CPU-only torch: the network is a 2-layer MLP, instant on CPU, and the default PyPI
# wheel pulls in ~2GB of CUDA libraries this image never uses.
RUN pip install --no-cache-dir --index-url https://download.pytorch.org/whl/cpu torch==2.7.1

COPY service/requirements.txt ./service/requirements.txt
RUN pip install --no-cache-dir -r service/requirements.txt

# Only what inference needs: the network definition, not the training/env/dataset code.
COPY src/agent ./src/agent
COPY service ./service

ENV MODEL_DIR=/models \
    HOST=0.0.0.0 \
    PORT=8080 \
    PYTHONUNBUFFERED=1

EXPOSE 8080

HEALTHCHECK --interval=30s --timeout=3s --start-period=10s --retries=3 \
    CMD python -c "import os,sys,urllib.request; urllib.request.urlopen('http://127.0.0.1:' + os.environ.get('PORT','8080') + '/health', timeout=2)" || exit 1

CMD ["python", "-m", "service.inference_server"]
