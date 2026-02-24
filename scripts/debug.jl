## Imports
import Pkg
Pkg.activate(".")

using Revise
using OhMyREPL
using Infiltrator
using Logging
using Debugger
using Printf

# include(joinpath(@__DIR__, "FM.jl" ))
using FlexOPTi

## Test
dt_file = joinpath("/home/kahka/DTU/BlueBird/flexmanager/FM/data/dynamics_estimator_results.json");
sensors_file = joinpath("/home/kahka/DTU/BlueBird/flexmanager/FM/data/df_predict.json");
forecast_file = joinpath("/home/kahka/DTU/BlueBird/flexmanager/FM/data/dynamics_estimator_results.json");
 
## optimize 
oy = optimize(dt_file,sensors_file, forecast_file; loglevel="info", Hu = 3, name = "Montcada");



## debug 
using TimeZones
const START_DATE_TIME = "2025-07-15T17:00:00+00:00"
const SENSITIVITY = 0.5 # C
const KELVIN_OFFSET   = FM.KELVIN_OFFSET
start = ZonedDateTime(START_DATE_TIME)

ox = oy[:ox]
digital_twin = ox.digital_twin;
forecasts    = ox.forecast;
sensors      = ox.sensors;

M = digital_twin["CoefsTempCooling"];

inputs = digital_twin["TransformedInputsTemperature"];

## Find index 
dates = ZonedDateTime.(getindex.(inputs, "start"));
idx   = findfirst(x->x == start, dates);
sensor_idx = FM.find_index_from_datetime(sensors, START_DATE_TIME)

input = inputs[idx  ];

##
function simulate_step(input, M, sensors, sensor_idx, digital_twin)
    ## Read from the sensor function 
    function read_and_add_lagg(sensor_dico::Dict{String, Any}, lagg::Int)

        filtered_dico = Dict();
        for (k,v) in sensor_dico
            if startswith(k, "TempSP") && (k ≠ "TempSP_22")
                splitedSP = split(k, "_")
                new_key = k 
                if lagg ≥ 1
                    new_key = k*"_l"*string(lagg)
                end
                filtered_dico[new_key] = v + KELVIN_OFFSET
            end
        end

        return filtered_dico
    end

    ## Read and  
    fd0 = read_and_add_lagg(sensors[sensor_idx], 0);
    fd1 = read_and_add_lagg(sensors[sensor_idx], 1);

    x_dico = merge(fd0, fd1, input);


    function shift_levels(d::Dict) 
        newd = Dict{Any, Any}()

        for (k, v) in d
            if startswith(k, "AmbTemp")
                m = match(r"^(.*)_l(\d+)$", k)

                if m === nothing
                    newd[k] = v
                else
                    base, lvl = m.captures
                    lvl = parse(Int, lvl)

                    newkey = if lvl == 1
                        base
                    else
                        "$(base)_l$(lvl - 1)"
                    end

                    newd[newkey] = v
                end
            else
                # non-AmbTemp keys copied verbatim
                newd[k] = v
            end
        end

        return newd
    end


    ## Transform the setpoints
    function transform_setpoints(x_dico::Dict, y_dico::Dict)
        # dico = merge(x_dico, y_dico)
        dico = shift_levels(x_dico)
        SP_dico = Dict{String, Float64}()

        for (k, sp_val) in dico
            # Only work on TempSP keys
            startswith(k, "TempSP") || continue
            k == "TempSP_22" && continue

            # Extract suffix (e.g. "_12_l1" or "_12")
            suffix = replace(k, "TempSP" => "")

            # Build matching AmbTemp key
            amb_key = "AmbTemp" * suffix

            # Only compare if AmbTemp exists
            if haskey(dico, amb_key)
                amb_val = dico[amb_key]

                @printf("%15s => %7.2f | %7.2f \n", amb_key, amb_val-KELVIN_OFFSET, sp_val - KELVIN_OFFSET)

                # Apply rule
                if sp_val >= amb_val + SENSITIVITY 
                    SP_dico[k] = 0.0
                else
                    SP_dico[k] = sp_val
                end
            else
                # No matching AmbTemp → keep original
                SP_dico[k] = sp_val
            end
            if "_79" == suffix || suffix == "79_l1"
                SP_dico[k] = 0.0
            end
        end

        return SP_dico
    end

    ## Transform and replace  
    y_dico  = Dict(k => v+KELVIN_OFFSET for (k,v) in sensors[end] if startswith(k, "AmbTemp"));
    # SP_dico = transform_setpoints(x_dico, y_dico);
    SP_dico = transform_setpoints(x_dico, y_dico);

    ## Rewrtie x dico
    for (k,v) in SP_dico
        x_dico[k] = v
    end

    ## Order the values function 
    function build_ordered_input(dico, col_names)    

        x = zeros(Float64, length(col_names))
        for (i, col) in enumerate(col_names)
            x[i] = dico[col]
        end

        return x
    end

    ## Call the function 
    col_names = digital_twin["CoefsTempCoolingColNames"]
    x = build_ordered_input(x_dico, col_names)

    ## Output 
    y = M*x

    y_known = last.(sort(collect(y_dico),
        by = x -> parse(Int, split(x[1], "_")[end])
    ))
    if length(y_known) == 37
        deleteat!(y_known, 22)
    end

    last_number(key) = parse(Int, match(r"(\d+)(?:_l\d+)?$", key).captures[1])
    SP_lag   = sort(
        [(k, v) for (k, v) in SP_dico if occursin(r"_l\d+$", k)];
        by = x -> last_number(x[1])
    ) 

    SP_nolag = sort(
        [(k, v) for (k, v) in SP_dico if !occursin(r"_l\d+$", k)];
        by = x -> last_number(x[1])
    ) 

    return x, y, SP_dico
end

##
x1, y1, SP_dico1 = simulate_step(inputs[idx  ], M, sensors,sensor_idx  , digital_twin);
x2, y2, SP_dico2 = simulate_step(inputs[idx+1], M, sensors,sensor_idx+1, digital_twin); 


## Add the room number 
function add_room_number(row_names::Vector{String})::Dict{Int, Int}

    idx_room_mapping = Dict()
    for (i,name) in enumerate(row_names)
        splitedSP = split(name, "_")
        idx_room_mapping[i] = parse(Int, splitedSP[2])
    end
    
    return idx_room_mapping
end    

##
row_names = digital_twin["CoefsTempCoolingRowNames"];
idx_room_mapping = add_room_number(string.(row_names));

## print 
open(joinpath(@__DIR__, "../data/debug.txt"), "w") do io
    println(io, "(idx, room number) = value [C]")
    for (i, val) in enumerate(y1)
        SP = last.(SP_lag1) .- KELVIN_OFFSET
        if SP[i] ≤ -273
            SP[i] = 0
        end 
        @printf(io, "(%3d, %3s)    %15.2f %15.2f %15.2f     \n", i, idx_room_mapping[i], val - KELVIN_OFFSET, y_known[i] - KELVIN_OFFSET, SP[i] )
    end
end


### Debug the poweor equation ###
## Power
y_prev_dico = filter(sensors[end-1]) do (k,v) 
    startswith(k, "AmbTemp") && v != 0 
end
y_prev = last.(sort(collect(y_prev_dico), by = p -> parse(Int, last(split(p.first, "_"))))) .+ KELVIN_OFFSET
ΔT = y .- y_prev 

# Get the multilpicative coefficients
energy_coefs   = digital_twin["CoefsHVACEnergyCooling"];
hvac_infos     = digital_twin["TransformedInputsHVACEnergy"][5];
p_extras_terms = getindex.(Ref(hvac_infos), ["intercept","ComfTempHeating","ComfTempCooling","nhvac"])

p = sum( p_extras_terms .* energy_coefs[1:4])  + 
    sum(-ΔT .* energy_coefs[5:end] ) 

# Conf temperature cooling 
