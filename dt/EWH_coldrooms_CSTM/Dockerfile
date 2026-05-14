FROM r-base:4.4.2

RUN apt-get update && apt-get install -y --no-install-recommends \
        libgsl-dev \
        libcurl4-openssl-dev \
        libssl-dev \
    && rm -rf /var/lib/apt/lists/*

RUN Rscript -e "install.packages(c('ctsmTMB', 'jsonlite', 'dplyr'), repos='https://cloud.r-project.org', Ncpus=4)"

WORKDIR /dt

COPY . .

# /dt-volume is the shared volume where the output JSON is published for the FC
VOLUME ["/dt-volume"]

# After parameter estimation, copy output JSON to shared volume
CMD ["bash", "-c", \
     "Rscript ctsmTMB_parameterestimation_callable.R && \
      cp cold_room_model_cont.json /dt-volume/cold_room_model_continuous.json && \
      echo '[DT] Model written to /dt-volume/cold_room_model_continuous.json'"]
