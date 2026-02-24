# Flexibility Manager - Blue Bird

This code base is using the [Julia Language](https://julialang.org/) and
[DrWatson](https://juliadynamics.github.io/DrWatson.jl/stable/)
to make a reproducible scientific project.

Requires julia > 1.11.

It is authored by kahka.

## Installation 

```julia 
   $ julia
   julia> using Pkg
   julia> Pkg.activate(".")
   julia> Pkg.add("https://github.com/adam-eva/flex_manager.git")
   julia> using FM 
```

Alternatively to use the package without installing it (developement mode)

```julia
   $ git clone https://github.com/adam-eva/flex_manager.git
   $ julia
   julia> using Pkg
   julia> Pkg.activate(".")
   julia> using DrWatson
   julia> @quickactivate FM
```

## Usage 

API functions are defined in index.html documentation

```julia
   julia> ?
   help?> optimize 
```

#### For Python usage

```python
   # Install PyJulia
   pip install julia
   import julia

   # Optional if julia is not already installed 
   julia.install()

   from julia import Pkg
   Pkg.add(url="https/or/ssh/git/repo")
   from julia import FM

   # Call API function 
   FM.optimize(A, B, F, x, d, x_low=x_low_, x_high=x_high_, u_low=u_low_, y_high=y_high_)
```

Enjoy !