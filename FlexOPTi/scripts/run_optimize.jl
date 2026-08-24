#!/usr/bin/env julia
#
# Command-line entry point for FlexOPTi.
#
#   julia --project=<FlexOPTi> scripts/run_optimize.jl config.json
#
# Reads a JSON config file describing one optimization run, calls
# `FlexOPTi.optimize`, and writes the parsed result to the requested
# output file. Intended for callers outside Julia (Python, shell, ...)
# that drive FlexOPTi as a subprocess.
#
# Required config keys:
#   dt_file, sensors_file, forecast_file, pilot, output_file
#
# Any other key is forwarded to `optimize` as a keyword argument, so the
# full kwarg surface (Hu, solver, market_country, ...) is reachable
# without changing this script.

import Pkg
Pkg.activate(joinpath(@__DIR__, ".."); io = devnull)

using FlexOPTi
using JSON

const REQUIRED = ("dt_file", "sensors_file", "forecast_file", "pilot", "output_file")

# Keys consumed here rather than forwarded to `optimize`.
const CONSUMED = ("dt_file", "sensors_file", "forecast_file", "output_file",
                  "only_next_step")

function main(args)
    if length(args) != 1
        println(stderr, "usage: run_optimize.jl <config.json>")
        return 2
    end

    cfg_path = args[1]
    isfile(cfg_path) || (println(stderr, "config file not found: $cfg_path"); return 2)

    cfg = JSON.parsefile(cfg_path)

    missing_keys = [k for k in REQUIRED if !haskey(cfg, k)]
    if !isempty(missing_keys)
        println(stderr, "missing required config keys: $(join(missing_keys, ", "))")
        return 2
    end

    # Everything not consumed here becomes a kwarg for `optimize`.
    kwargs = Dict(Symbol(k) => v for (k, v) in cfg if !(k in CONSUMED))

    oy = FlexOPTi.optimize(cfg["dt_file"], cfg["sensors_file"], cfg["forecast_file"];
                           kwargs...)

    # `optimize` resolves the pilot string into a type for multiple dispatch.
    pilot = oy[:o].pilot
    only_next_step = get(cfg, "only_next_step", false)
    json_data = FlexOPTi.parse_OPT_output(pilot, oy; only_next_step = only_next_step)

    FlexOPTi.write_outputs_to_file(json_data; file = cfg["output_file"])
    return 0
end

exit(main(ARGS))
