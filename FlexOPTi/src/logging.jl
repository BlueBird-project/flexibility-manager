"""
    set_logging(log_output="combined", log_level="info"; log_file="fm.log", datetime=True)

Configure and register a global logger for the module.

# Arguments
- `log_output::String = "combined"`  
  Where logs should be sent. Accepted values:
  - `"console"`  — log only to the console
  - `"file"`     — log only to a file
  - `"combined"` — log to both console and file

- `log_level::String = "info"`  
  Minimum log level to display. Accepted values:
  - `"debug"`, `"info"`, `"warn"`, `"error"`

- `log_file::String = "fm.log"`  
  Name of the file used when `log_output` includes file logging.

- `datetime::Bool = true`  
  Add logging datetime.

# Description
This function builds the appropriate logger (`ConsoleLogger`, `FileLogger`,
or a combined `TeeLogger`), wraps it with a `MinLevelLogger` to enforce
the desired logging level, and registers it as the global logger using
`global_logger`.

# Returns
The logger instance that was registered as the global logger.
"""
function set_logging(o::O)
    # Parameters
    log_output = o.logoutput
    log_level  = o.loglevel
    log_file   = o.logfile
    datetime   = o.log_with_time

    # Map log level string to Logging constant
    level_map = Dict(
        "debug" => Logging.Debug,
        "info"  => Logging.Info,
        "warn"  => Logging.Warn,
        "error" => Logging.Error,
    )

    # Resolve log level
    log_key = lowercase(log_level)
    min_level = get(level_map, log_key) do
        error("Invalid log_level: '$log_level'. Expected one of: $(join(keys(level_map), ", ")).")
    end

    # Construct destination loggers
    file_logger     = FileLogger(log_file)
    console_logger  = ConsoleLogger(stderr)
    combined_logger = TeeLogger(console_logger, file_logger)

    # Map output mode
    logger_map = Dict(
        "file"     => file_logger,
        "console"  => console_logger,
        "combined" => combined_logger,
    )

    # Resolve selected logger
    out_key = lowercase(log_output)
    selected_logger = get(logger_map, out_key) do
        error("Invalid log_output: '$log_output'. Expected 'console', 'file', or 'combined'.")
    end

    # Wrap logger to add timestamps
    if datetime == true
        selected_logger = TransformerLogger(selected_logger) do log
            timestamp = Dates.format(now(), dateformat"yyyy-mm-dd HH:MM:SS")
            new_msg = "[$timestamp] " * log.message
            return (; log..., message = new_msg)
        end
    end

    # hard reset current logger context
    # global_logger(Logging.SimpleLogger(stdout, Logging.Error))
    global_logger(NullLogger())

    # Register logger globally with minimum level
    configured_logger = MinLevelLogger(selected_logger, min_level)
    global_logger(configured_logger)

    @info "Logging set to level $(log_level)."

    return configured_logger
end # function


