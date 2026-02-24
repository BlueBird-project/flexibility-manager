#!/bin/bash

# Set Julia command with project environment
JULIA_CMD="julia --project=."

# Check if an argument was passed
if [ $# -eq 0 ]; then
    # No argument: run main.jl
    $JULIA_CMD ./scripts/simulate.jl
else
    # Argument passed: assume it's a script name
    SCRIPT="$1"
    if [[ "$SCRIPT" == *.jl && -f "$SCRIPT" ]]; then
        # If it ends in .jl and the file exists, run it
        $JULIA_CMD "$SCRIPT"
    else
        echo "Error: '$SCRIPT' is not a valid .jl file."
        exit 1
    fi
fi