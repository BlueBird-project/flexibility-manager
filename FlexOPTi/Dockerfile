FROM julia:1.12

WORKDIR /app

# Copy only Project.toml — Manifest is machine-specific; resolve fresh inside container
COPY Project.toml ./

# Add General registry then resolve + instantiate from Project.toml
RUN julia -e 'using Pkg; Pkg.Registry.add("General")' && \
    julia --project=. -e 'using Pkg; Pkg.resolve(); Pkg.instantiate()'

# Copy source and data
COPY src/              ./src/
COPY data/EWH/inputs/  ./data/EWH/inputs/
COPY data/cache/       ./data/cache/
COPY scripts/          ./scripts/
COPY settings.json     ./
COPY entrypoint.sh     ./

# Create output directories that mpc_main.jl writes to
RUN mkdir -p /app/data/EWH/outputs /app/data/montcada/outputs

# Precompile FlexOPTi so container startup is fast
RUN julia --project=. -e 'using FlexOPTi; println("Precompile OK")'

# /dt-volume is the shared volume; DT container writes here; FC reads from here
VOLUME ["/dt-volume"]

ENV DT_FILE=/dt-volume/cold_room_model_continuous.json

ENTRYPOINT ["julia", "--project=/app"]
CMD ["/app/scripts/ewh/dr_controller.jl"]
