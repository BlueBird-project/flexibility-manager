"""
    parse_OPT_output(::Ewh, oy) -> Parser

Return a Parser with the EWH room index mapping (1-to-1 for 5 rooms).
Called by the core parse_OPT_output to handle pilot-specific room numbering.
"""
function parse_OPT_output(::Ewh, oy)
    mapping = Dict(1 => 1, 2 => 2, 3 => 3, 4 => 4, 5 => 5)
    return Parser(mapping)
end
