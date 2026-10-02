# Include our FM tests
ti = time()
include("fm_tests.jl")
ti = time() - ti

# Run test suite
println("Starting FM tests...")

println("\nTest took total time of:")
println(round(ti/60, digits = 3), " minutes")
