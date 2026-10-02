"""
    _write_json_impl(dir_path::Union{String, Nothing}, data::Dict, data_name::String, file_name::String)

Helper function to handle path logic, directory creation, and file writing at runtime.
`dir_path` can be `nothing` if the user didn't specify a path, triggering the default behavior.
"""
function write_OPT_json(dir_path::Union{String, Nothing}, data::Dict, data_name::String, file_name::String)
    final_dir = if dir_path === nothing
        # Default case: Use @__DIR__ (location of the *script* where the macro is called) 
        # and append "data" folder
        default_path = joinpath(@__DIR__, "data")
        @info "No path specified. Using default directory: $default_path"
        default_path
    else
        # Use the provided path
        dir_path end

    # Ensure the directory exists. `mkpath` handles nested directories and checks for existence.
    mkpath(final_dir)

    final_filepath = joinpath(final_dir, file_name)

    @info "Writing the output in variable `$data_name` to file: `$final_filepath`"
    
    open(final_filepath, "w") do f
        JSON.print(f, data)
    end
end

"""
    @write_json(data_expr, [file_name_expr], [path_expr])

Macro to write dictionary data to a JSON file with optional file name and path arguments.
If no file name is provided, it defaults to the variable name + ".json".
If no path is provided, it defaults to `joinpath(@__DIR__, "data")`.

using JSON, Logging, Base.Filesystem
"""
macro write_json(data_expr, args...)
    data_name = string(data_expr)
    
    # Process optional arguments
    file_name_expr = if isempty(args)
        # Default file name: variable name + ".json"
        string(data_name, ".json")
    else
        # User-provided file name (evaluated at runtime)
        esc(args[1])
    end

    path_expr = if length(args) > 1
        # User-provided path (evaluated at runtime)
        esc(args[2])
    else
        # No path provided, pass `nothing` to the helper function
        nothing
    end

    # The macro returns a quoted expression to be evaluated at runtime
    quote
        # The helper function handles all runtime logic
        _write_json_impl($path_expr, $(esc(data_expr)), $data_name, $(esc(file_name_expr)))
    end
end
