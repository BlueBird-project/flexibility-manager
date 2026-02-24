"""
    Based on parse_digital_twin. 
    help?> parse_digital_twin     
"""
function parse_forecasts(::Montcada, o::O, json_path::String)::Dict{String, Any}
    # For the moment the forecast is the same file in the future
    # This will be changed latter when the forecasts come from the 
    # forecasting team
    return parse_digital_twin(Montcada(), o, json_path)
end