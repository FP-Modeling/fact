# FFACT's Manual

This is a concise and pragmatic manual on how to create and run simulations using `FFACT`. For a deeper and more detailed description
of the internals of the DSL, including a walkthrough via examples, please consult (and generate via `literate.sh`) either `GraduationThesis` (for `FACT`) or `MasterThesis` (for `FFACT`).

## Models

A simulation model is defined using `mdo-notation` (check [recursive do](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/recursive_do.html)) to describe a system of _differential equations_. The current version of `FFACT` only supports 
continuous simulations, i.e., discrete or hybrid simulations are future work. Alongside the equations, one must provide _initial conditions_ for 
each individual equation, such as the following:

```haskell
lorenzModel :: Model [Double]
lorenzModel = mdo
   x <- integ (sigma * (y - x)) 1.0
   y <- integ (x * (rho - z) - y) 1.0
   z <- integ (x * y - beta * z) 1.0
   let sigma = 10.0
       rho = 28.0
       beta = 8.0 / 3.0
   return $ sequence [x, y, z]
```

In this example, `lorenzModel` will return the state variables of interest via a list, hence the model having the type `Model [Double]`.
Recursive monadic bindings are possible due to `mdo`, which makes the description of a model in code closer to its mathematical counterpart.

## Solver

Solver-specific configurations, e.g., which numerical method should be used and with which _time step_, which solver stage should it start with,
are configured **separately** from the model and from executing a simulation. This sort of configuration details are set via a separate record, such as the following:

```haskell
lorenzSolver = Solver { dt = 1,
                        method = RungeKutta2,
                        stage = SolverStage 0
                      }
```

Available numerical methods:
- `Euler`
- `RungeKutta2`
- `RungeKutta4`

## Simulation

A model and a record with solver's configuration are some of the _arguments_ to a _driver function_. A driver function runs the simulation starting from 0 until
a provided timestamp (in seconds). Currently, `runCTFinal` outputs the final result of the system
at the provided final time and `runCT` outputs a **list** of intermediate values from the start
until the provided final time spaced by the time step within the solver's configuration. 
The type signatures of these functions are the following (`Double` is the final time of choice):

```haskell
runCTFinal :: Model a -> Double -> Solver -> IO a
runCT :: Model a -> Double -> Solver -> IO [a]
```

## Interpolation

Both `FACT` and `FFACT` use **linear interpolation** to approximate results in requested timestamps that are not reachable via the chosen time step within
the solver's configuration. Driver functions automatically take care of detecting and running interpolations. 
The type signature of the provided interpolation function (and probably future extensions) is the following:

```haskell
interpolate :: CT Double -> CT Double
```

## Caching

Both `FACT` and `FFACT` employ a **memoization strategy** for caching, in order to speed up the simulation execution. Without this, simulations recompute previously
computed values multiple times, due to the recursive nature of the numerical methods available. A table is saved in memory with already calculated values, and lookups
are done instead of triggering a new computation.
The type signature of the provided memoization function (and probably future extensions) is the following:

```haskell
memo :: UMemo e => (CT e -> CT e) -> CT e -> CT (CT e)
```

The typeclass `UMemo` is provided custom typeclass.

## Example

Lorenz Attractor complete example:

```haskell
lorenzModel :: Model [Double]
lorenzModel = mdo
   x <- integ (sigma * (y - x)) 1.0
   y <- integ (x * (rho - z) - y) 1.0
   z <- integ (x * y - beta * z) 1.0
   let sigma = 10.0
       rho = 28.0
       beta = 8.0 / 3.0
   return $ sequence [x, y, z]
   
lorenzSolver = Solver { dt = 1,
                        method = RungeKutta2,
                        stage = SolverStage 0
                      }
					  
lorenz = runCTFinal lorenzModel 100 lorenzSolver
```
