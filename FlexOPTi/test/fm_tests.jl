# Simplified unit tests for Flexibility Manager (FlexOPTi. MPC-based control
# Focused on the core functionality that works with the current data structure
using Test
using FlexOPTi
using FlexOPTi: optimize, parse_digital_twin, parse_sensors,
        parse_forecasts, build_constraints, mpc_update

using MathOptInterface
const MOI = MathOptInterface

# Test data paths
const TEST_DIGITAL_TWIN_FILE = joinpath(@__DIR__, "..", "data", "test_dtforecast_file.json")
const TEST_SENSORS_FILE      = joinpath(@__DIR__, "..", "data", "test_sensor_file.json"    )
const TEST_FORECASTS_FILE    = joinpath(@__DIR__, "..", "data", "test_dtforecast_file.json")

const COMPUTE_OPT_DATETIME = "2025-07-15T17:00:00+00:00";

@testset "Flex Manager OPT Core Functionality Tests" begin

@testset "Basic Optimization Pipeline" begin
    # Test that the main optimize function works
    @testset "Optimize Function" begin
        @test_throws ArgumentError optimize(TEST_DIGITAL_TWIN_FILE, TEST_SENSORS_FILE, TEST_FORECASTS_FILE)
        result = optimize(TEST_DIGITAL_TWIN_FILE, TEST_SENSORS_FILE, TEST_FORECASTS_FILE;
                    pilot = "Montcada", loglevel = "warn", compute_datetime = COMPUTE_OPT_DATETIME);
        
        @test result isa Dict
        @test haskey(result, :OPT_status)
        @test haskey(result, :SP)
        @test haskey(result, :T)
        @test haskey(result, :p_HVAC)
    end
    
    @testset "Parameter Overrides" begin
        # Test that we can override parameters
        result = optimize(TEST_DIGITAL_TWIN_FILE, TEST_SENSORS_FILE, TEST_FORECASTS_FILE;
                            pilot = FlexOPTi.Montcada(), Hu = 2, solver = "HiGHS", loglevel = "warn", compute_datetime = COMPUTE_OPT_DATETIME)
            
            @test result isa Dict
            @test haskey(result, :OPT_status)
        end
    end
    
    @testset "Component Tests" begin
        @testset "Digital Twin Parsing" begin
            pilot = FlexOPTi.Montcada()
            # Default code parameters
            o = FlexOPTi.default_code_parameter()
            digital_twin = parse_digital_twin(pilot, o, TEST_DIGITAL_TWIN_FILE)
            
            @test digital_twin isa Dict
            @test haskey(digital_twin, "CoefsTempHeating")
            @test haskey(digital_twin, "CoefsTempCooling")
            @test haskey(digital_twin, "CoefsHVACEnergyHeating")
            @test haskey(digital_twin, "CoefsHVACEnergyHeating")
            
            # Test matrix dimensions
            @test size(digital_twin["CoefsTempHeating"], 1) == length(digital_twin["CoefsTempHeatingRowNames"])
            @test size(digital_twin["CoefsTempHeating"], 2) == length(digital_twin["CoefsTempHeatingColNames"])
            @test size(digital_twin["CoefsTempCooling"], 1) == length(digital_twin["CoefsTempCoolingRowNames"])
            @test size(digital_twin["CoefsTempCooling"], 2) == length(digital_twin["CoefsTempCoolingColNames"])
        end
        
        @testset "Sensor Parsing" begin
            pilot = FlexOPTi.Montcada()
            # Default code parameters
            o = FlexOPTi.default_code_parameter()
            sensors = parse_sensors(pilot, TEST_SENSORS_FILE)
            
            @test sensors isa Vector
            @test length(sensors) > 0
            
            # Test first sensor entry
            first_sensor = sensors[1]
            @test first_sensor isa Dict
            @test haskey(first_sensor, "TempSP_1")
            @test haskey(first_sensor, "AmbTemp_1")
        end
        
        @testset "Forecast Parsing" begin
            pilot = FlexOPTi.Montcada()
            o = FlexOPTi.default_code_parameter()
            forecasts = parse_forecasts(pilot, o, TEST_FORECASTS_FILE)
            
            @test forecasts isa Dict
            haskey(forecasts, "TransformedInputsTemperature")
            
            # Test disturbance matrix
            forecast_inputs = forecasts["TransformedInputsTemperature"]
            @test forecast_inputs isa Vector
            @test length(forecast_inputs) > 0 
            @test forecast_inputs[begin] isa Dict
            @test haskey(forecast_inputs[begin], "wsin2_25")
        end
        
        @testset "Constraint Building" begin
            pilot = FlexOPTi.Montcada()
            constraints = build_constraints(pilot)
            
            @test constraints isa Dict
            @test haskey(constraints, :T_low)
            @test haskey(constraints, :T_high)
            @test haskey(constraints, :SP_low)
            @test haskey(constraints, :SP_high)
        end
    end
    
    @testset "MPC Update" begin
        pilot = FlexOPTi.Montcada()
        # Get default parameters
        o = FlexOPTi.default_code_parameter()
        
        # Parse input data
        digital_twin = parse_digital_twin(pilot, o, TEST_DIGITAL_TWIN_FILE)
        sensors = parse_sensors(pilot, TEST_SENSORS_FILE)
        forecasts = parse_forecasts(pilot, o, TEST_FORECASTS_FILE)
        constraints = build_constraints(pilot)
        
        # Create OX structure
        ox = FlexOPTi.OX(digital_twin, sensors, forecasts, constraints)
        
        # Run MPC update
        oy = mpc_update(pilot, o, ox)
        
        # Test that we get a valid result
        @test oy isa Dict
        @test haskey(oy, :OPT_status  )
        @test oy[:OPT_status] == MOI.OPTIMAL
    end
    
end

println("\nFlexOPTi.Montcada tests completed successfully!")