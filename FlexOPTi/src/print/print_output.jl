"""
    opt_output_to_file(filename, opt_output)

Write the results of the optimization to a file,
excluding fields :o and :ox, transposing matrices and
showing original indices for matrices and vectors.
"""
function opt_output_to_file(filename::String, opt_output::Dict{Symbol, Any} ;
        kelvin::Bool=false
    )
    open(filename, "w") do io
        for (key, val) in opt_output
            if key in [:o, :ox]
                continue
            end

            if kelvin == false && key ∈ [:T, :SP, :SP_transformed]
                val = val .- KELVIN_OFFSET
            end

            println(io, "================ $key ================")

            if val isa AbstractMatrix
                mat = val'  # transposed for printing

                for i in axes(mat, 1)        # printed row index
                    for j in axes(mat, 2)    # printed col index
                        orig_row = j         # original (before transpose)
                        orig_col = i
                        @printf(io, "[%d,%2d] %8.2f  ", orig_row, orig_col, mat[i, j])
                    end
                    println(io)
                end

            elseif val isa AbstractVector{<:Number}
                for i in eachindex(val)
                    @printf(io, "[%d] %8.2f  ", i, val[i])
                end
                println(io)

            elseif val isa Number
                @printf(io, "%.2f\n", val)
            else
                # for non-numerical objects
                println(io, val)
            end

            println(io) # spacing
        end
    end

    @info "Printing Optimization Results to file $(filename)."
end
