# Flexibility Manager - Blue Bird

This code base is using the [Julia Language](https://julialang.org/) 

Requires julia > 1.12.
Julia installation https://julialang.org/

It is authored by kahka.

## Installation 

```julia 
   # From within 'flexibility_manager' folder
   $ julia
   julia> import Pkg
   julia> Pkg.activate("FlexOPTi")
   julia> using FlexOPTi 
```

To get help on a function call 

```julia 
   julia> ?
   help?> optimize # Or any API function 
``` 

#### For Python usage

```python
   $ julia
   julia> import Pkg
   julia> Pkg.add("PyCall")   

   # Install PyJulia
   $ pip install julia 

   # python 
   >>> import julia

   # Optional if julia is not already installed 
   >>> julia.install()

   >>> from julia import Pkg
   >>> Pkg.activate(".")
   >>> Pkg.develop(path="FlexOPTi")
   >>> from julia import FlexOPTi

   # Call API functions
   >>> out_dic = FlexOPTi.optimize(dt_file, sensors_file, forecast_file , kwargs...)
   >>> json_data = FlexOPTi.parse_output(out_dic , pilot = "Montcada", kwargs...)
   >>> json_file = FlexOPTi.write_outputs_to_file(json_data, file = filenamepath)
   # Note : You will need to define DT, sensors and Forecast files. 
```

Continuous Integration 

Make sure that the tests are running 
```julia
   $ julia 
   julia> import Pkg
   julia> Pkg.activate(".")
   julia> Pkg.test()
```

Enjoy !
