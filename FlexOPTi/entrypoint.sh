#!/bin/bash
set -e
exec julia --project=/app /app/scripts/ewh/dr_controller.jl
