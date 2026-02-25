"""
    optimize(digital_twin_file, sensors_file, forecasts_file; kwargs...) -> Dict{Symbol,Any}

Run a one-step Model Predictive Control (MPC) optimization for a selected
building (pilot). This function acts as the main API entry point: it loads
model data, configures options, dispatches to the appropriate pilot-specific
MPC implementation, and returns structured optimization results.

# Arguments
- `digital_twin_file::AbstractString`  
  Path to the digital twin JSON file containing model structure,
  identified dynamics, and metadata.

- `sensors_file::AbstractString`  
  Path to the sensors JSON file containing current measurements
  used as initial conditions.

- `forecasts_file::AbstractString`  
  Path to the forecasts JSON file containing disturbance predictions
  (e.g., weather, occupancy, etc.).

# Keyword Arguments (kwargs...)

All keyword arguments override fields of the default options object `O`
(see `default_code_parameter()`).

## Core MPC Parameters
- `Hu::Int`  
  Control horizon (number of future time steps optimized).
  Default: `1`.

- `init_condition::Bool`  
  Whether to enforce special initial-condition handling logic.
  Default: `false`.

- `pilot::String` (**required**)  
  Name of the building/pilot to optimize. Used for multiple dispatch
  via `resolve_building(pilot)`.  
  Example: `pilot="Montcada"`.

- `solver::String`  
  MILP/LP solver name (must correspond to a loaded JuMP optimizer).
  Example: `"HiGHS"`, `"Gurobi"`, etc.  
  Default: `"HiGHS"`.

- `compute_datetime::ZonedDateTime`  
  Start time of the MPC horizon. If not provided, defaults to current
  UTC time.

## Logging Parameters
- `loglevel::String`  
  Logging verbosity (`"debug"`, `"info"`, `"warn"`, `"error"`).  
  Default: `"info"`.

- `logoutput::String`  
  Logging output mode (e.g., `"console"`, `"file"`, `"combined"`).  
  Default: `"combined"`.

- `logfile::String`  
  Log file name (used when file logging is enabled).  
  Default: `"fm.log"`.

- `log_with_time::Bool`  
  Whether to prepend timestamps to log entries.  
  Default: `true`.

## Output Parameters
- `output_file::String`  
  Filename where optimization results are exported.  
  Default: `"output.txt"`.

  # Returns
- `oy::Dict{Symbol,Any}` — Optimization results as documented in `mpc_update`.
  See `?mpc_update` for a full description of all returned fields
  (objective value, temperatures, setpoints, power flows, hybrid variables,
  solver status, options, and execution context).
"""
function optimize(digital_twin_file, sensors_file, forecasts_file ;
		  kwargs...)

	process_start_datetime = Dates.now()

	# Standard code parameters
	o = default_code_parameter()
	# Override defaults from keyword args (kwargs... is a NamedTuple)
	for (k, v) in kwargs
        if hasproperty(o, k)
            setproperty!(o, k, v)
        end
    end

	@info "Set logger to level $(o.loglevel)"
	set_logging(o)

	# Dispatch to the right pilot
	if o.pilot === nothing
		throw(ArgumentError("No pilot (Building) selected. Select a pilot as 
		        FM.optimize(args ; pilot = \"PilotName\", kwargs...)"))
	elseif o.pilot isa String
		@info "Working with pilot $(o.pilot)"
		o.pilot = resolve_building(o.pilot) # o.pilot data structure for multiple dispatch 
	end

	@info "Starting one step optimization"

	# TODO : With the API cach the lattest dynamics in a module digital twin and if the API gives nothing use the lattest catch data
	@info "Reading the digital twin from $digital_twin_file."
	digital_twin = parse_digital_twin(o.pilot, o, digital_twin_file)

	# Todo : Get from API/Database
	@info "Reading the sensors from $sensors_file."
	sensors = parse_sensors(o.pilot, sensors_file)

	# TODO : Similar with forecasts
	@info "Reading the forecasts from $forecasts_file."
	forecasts = parse_forecasts(o.pilot, o, forecasts_file)

	constraints = build_constraints(o.pilot)
	@info "Building the constraints."

	# Store in the input structure
	ox = OX(digital_twin, sensors, forecasts, constraints) 

	# Pass the configured Params `O` and inputs `OX` to the MPC
	opt_time = @elapsed begin
		oy = mpc_update(o.pilot, o, ox) # One step prediction
	end
	@info "Optimization terminated with status $(oy[:OPT_status]) in $(@sprintf("%.2g",opt_time)) seconds."

	process_end_datetime = Dates.now()
	process_elapsed_time_in_sec = (process_end_datetime - process_start_datetime) / Second(1)

	# Add time metadata
	add_date_time_metadata!(oy, 
		process_start_datetime, process_end_datetime,
		process_elapsed_time_in_sec)

	return oy
end


"""
  o::O = default_code_parameter()

  Create and return the default MPC options structure `O`.
  
  These parameters define horizon length, logging configuration,
  solver selection, output settings, and start time.
  All fields can be overridden via `kwargs...` in `optimize`.
"""
function default_code_parameter()

	# Code parameters
	Hu             = 1     # Controller horizon 
	init_condition = false
	pilot          = nothing

	# Logging parameters
	loglevel       = "info"
	logoutput      = "combined"
	logfile        = "fm.log"
	log_with_time  = true
	solver         = "HiGHS"

	# Miscelanous
	output_file = "output.txt"
	        
	compute_datetime = now(tz"UTC") # Use current time if not specified
	return O(Hu, init_condition, pilot,
		loglevel, logoutput, logfile, log_with_time, solver,
		output_file, compute_datetime)
end

function add_date_time_metadata!(oy::Dict{Symbol, Any},
	start_datetime::DateTime, end_datetime::DateTime, 
	process_elapsed_time_in_sec::Float64)::Dict{Symbol, Any}

	dt       = oy[:ox].digital_twin
	forecast = oy[:ox].forecast
	sensors  = oy[:ox].sensors

	oy[:OPT_start_date_time ] = string(start_datetime)
	oy[:OPT_end_date_time   ] = string(end_datetime)
	oy[:OPT_elapsed_time_sec] = process_elapsed_time_in_sec

	oy[:DT_datetime       ], _ = find_lattest_datetime(dt["TransformedInputsTemperature"])
	oy[:forecasts_datetime], _ = find_lattest_datetime(forecast["TransformedInputsTemperature"])
	oy[:sensors_datetime  ], _ = find_lattest_datetime(sensors)

	return oy
end