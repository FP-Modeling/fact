\ignore{
\begin{code}
module GraduationThesis.Lhs.Interpolation where
import GraduationThesis.Lhs.Design

type Model a = Dynamics (Dynamics a)

iterToTime :: Interval -> Solver -> Int -> Int -> Double
iterToTime interv solver n st =
  if st < 0 then 
    error "Incorrect stage: iterToTime"
  else
    (startTime interv) + n' * (dt solver) + delta (method solver) st
      where n' = fromInteger (toInteger n)
            delta Euler       0 = 0
            delta RungeKutta2 0 = 0
            delta RungeKutta2 1 = dt solver
            delta RungeKutta4 0 = 0
            delta RungeKutta4 1 = dt solver / 2
            delta RungeKutta4 2 = dt solver / 2
            delta RungeKutta4 3 = dt solver                          

\end{code}
}

The previous chapter presented issues with the current implementation. This chapter, \textit{Weakening Discreteness}, tackles the problem related to the size of the time step, and how it affects the results at the stop time of the simulation. After this chapter, only the second problem will remain to be addressed. This task will be accomplished by chapter 7, \textit{The Speed Pill: Memoization}.

\section{Accepting Computer's Limitations}

The aforementioned time step problem araises from the \textbf{conversion} from continuous time to discrete iterations. A new look at one of the solver functions demonstrate that checking the iteration is a fundamental part of the used numerical methods. Below, the current iteration in being checked in the sixth line:

\begin{spec}
integEuler :: Dynamics Real
             -> Dynamics Real 
             -> Dynamics Real 
             -> Parameters -> IO Real
integEuler (Dynamics diff) (Dynamics init) (Dynamics y) params =
  case iteration params of
    0 -> 
      init params
    n -> do 
      let iv  = interval ps
          sl  = solver ps
          ty  = iterToTime iv sl (n - 1) 0
          psy = ps { time = ty, iteration = n - 1, solver = sl { stage = 0} }
      a <- y psy
      b <- diff psy
      let !v = a + dt (solver ps) * b
      return v
\end{spec}

Further, when the driver creates dynamic computations and start the execution process, it uses a list of integers to define which iteration that computation represents. The \textit{subRunDynamics} function uses function composition, represented by the function \texttt{(.)} in Haskell, to create a \texttt{Parameters} record from a number in the created list and the record is applied to the dynamic computation.

\begin{spec}
subRunDynamics :: Dynamics a -> Interval -> Solver -> [IO a]
subRunDynamics (Dynamics m) iv sl =
  do let (nl, nu) = iterationBnds iv (dt sl)
         parameterise n = Parameters { interval = iv,
                                       time = iterToTime iv sl n 0,
                                       iteration = n,
                                       solver = sl { stage = 0 }}
     map (m . parameterise) [nl .. nu]
\end{spec}

With the solver and driver in mind, it is easier to understand the dependency on the time step. When the function \textit{iterationBnds} calculates the first and last iterations to build the list, it uses the time interval and the solver time step. Also, the function \textit{iterToTime} uses these same dependencies in addition to the current iteration and stage. The code below shows the implementation of such functions:

\begin{spec}
iterationBnds :: Interval -> Double -> (Int, Int)
iterationBnds interv dt = (0, round ((stopTime interv - 
                               startTime interv) / dt))

iterToTime :: Interval -> Solver -> Int -> Int -> Double
iterToTime interv solver n st =
  if st < 0 then 
    error "Incorrect stage: iterToTime"
  else
    (startTime interv) + n' * (dt solver) + delta (method solver) st
      where n' = fromInteger (toInteger n)
            delta Euler       0 = 0
            delta RungeKutta2 0 = 0
            delta RungeKutta2 1 = dt solver
            delta RungeKutta4 0 = 0
            delta RungeKutta4 1 = dt solver / 2
            delta RungeKutta4 2 = dt solver / 2
            delta RungeKutta4 3 = dt solver                          
\end{spec}

The returned tuple in \textit{iterationBnds} represents how many iterations will be necessary to simulate the given interval using an arbitrary size of time step. The first of the tuple is always zero because only the total amount of iterations is described by the second member. This number can be calculated by discovering how many steps are in the interval. However, this can lead to floating number of iterations if the difference between \textit{stopTime} and \textit{startTime} is \textbf{not} a multiple of the size of the used time step. More importantly, because the \textit{round} function is based on this result, it means that a flooring or ceiling procedures will take place depending on this result, directly affecting how far the simulation will go.

In the second function, \textit{iterToTime}, a similar manipulation happens, with the difference being that the iteration and stage are being converted back to the time axis based on the solver method. This means that this conversion is not perfect, because all the values that cannot be represented by that specific sum --- initial time plus the time step times the iteration plus the solver stage internal step --- will be lost along the way.

The implications of this approach are twofold: the size of the time step directly affects the simulations results if it does not match the interval. For instance, using a time step of 1 in an interval from 0 to 5.3, \textit{iterationBnds} will generate a list from 0 to 5 iterations because of the rounding behaviour. Additionally, \textit{iterToTime} is uncapable of getting back any time that it is not multiple of the time step and cannot be obtained by using the internal solver steps, such as in the Runge-Kutta methods. Intuitively and at first glance, the solution seems simple: just shrink the time step enough to all the number in the interval be multiple of it, like 0.1 as the time step. This, however, is a temporary solution to a permanent problem.

The issue is that, regardless of the time step, it is always possible to make the \textbf{stop time} granular enough to not be a multiple of the time step. Thus, functions that depend on this property make adjustments that in fact calculates the wrong result for that specific time point of interest. The ideal solution would be to have an \textbf{infitesimal} size for the time step; an impossible achievment due to the computer's \textbf{limitation} of representing real numbers as floating points with double precision. Hence, it is not possible to completely cope with the problem, and it exemplifies one of the biggest challenges within the CPS domain: the mathematical requirements to model the continuous are just not available in digital computers.

Thus, the proposed solution described in the next section is not perfect, but atenuates the wrong outcome to some extent by changing the \textit{diffInteg} integrator function to check for this corner case, as well as adapting the driver to also be aware of this situation. In summary, if we can't grasp the time point exactly, then we will try to approximate the solution by \textbf{interpolating} two close results that we have access.

\section{Tweak I: Interpolation}

This tweak in the current implementation is divided into two parts: the driver and the integrator. Both entities need to cope with this limitation, and they will communicate with each other to properly adapt the outcome. As mentioned previously, we will add an interpolation function to alliviate the errors that arise when the end of the simulation is not achievable via the size of the time step. However, this interpolation procedure needs to occur only in special situations, when it is not possible to model that specific point in time is not a multiple of the time step. Otherwise, the execution should continue as it is.

So, the proposed mechanism is the following: the driver will identify these corner cases and communicate to the integrator --- via the \texttt{stage} field in the solver data type --- that the interpolation needs to be added into the pipeline of execution. When this flag is not activated, i.e., the \texttt{stage} informs to continue execution normally, the implementation goes as the previous chapters detailed, and this behaviour is altered \textbf{only} in particular scenarios, that the driver has control upon.

Naturally, it remains to re-implement the driver functions. The outer functions of the driver, e.g, \textit{runDynamics} and \textit{runDynamcsFinal}, are not affected by this modification, given that those functions only interact with the outer \texttt{Dynamics} of the \texttt{Model} alias. In contrast, their auxiliary functions, e.g., \textit{subRunDynamics} and \textit{subRunDynamicsFinal}, will be now responsible for telling the integrator that extra executions are required. The code below shows the new version of \textit{subRunDynamics}, as well as its new auxiliary function \textit{iterationBnds}:

\ignore{
\begin{code}
runDynamics :: Model a -> Interval -> Solver -> IO [a]
runDynamics (Dynamics m) iv sl = 
  do d <- m Parameters { interval = iv,
                         time = startTime iv,
                         iteration = 0,
                         solver = sl { stage = 0 }}
     sequence $ subRunDynamics d iv sl
     
iterationLoBnd :: Interval -> Double -> Int
iterationLoBnd interv dt = fst $ iterationBnds interv dt

iterationHiBnd :: Interval -> Double -> Int
iterationHiBnd interv dt = snd $ iterationBnds interv dt                   

\end{code}
}

\begin{code}
iterationBnds :: Interval -> Double -> (Int, Int)
iterationBnds interv dt = (0, ceiling ((stopTime interv - 
                               startTime interv) / dt))

subRunDynamics :: Dynamics a -> Interval -> Solver -> [IO a]
subRunDynamics (Dynamics m) iv sl =
  do let (nl, nu) = iterationBnds iv (dt sl)
         parameterise n = Parameters { interval = iv,
                                       time = iterToTime iv sl n 0,
                                       iteration = n,
                                       solver = sl { stage = 0 }}
         ps = Parameters { interval = iv,
                           time = stopTime iv,
                           iteration = nu,
                           solver = sl { stage = -1}}
     if (iterToTime iv sl nu 0) - (stopTime iv) < 0.00001
     then map (m . parameterise) [nl .. nu]
     else (init $ map (m . parameterise) [nl .. nu]) ++ [m ps]     
\end{code}

The new implementation of \textit{iterationBnds} is pretty similar to the previous one, with the difference being the replacement of the \textit{round} function for the \textit{ceiling} function. As explained in the previous section, the rounding not only adds some random behaviour to occur depeding on user input, but also can tell the integrator to stop executing before even surpassing the value of interest. For instance, the time 5.3 seconds will never be reached because its rounded version is 5. The opposite is true when using \textit{ceiling}: it is assured that the value of interest will be in the interval of computed values. So, when dealing with 5.3, the integrator will calculate all values up to 6 seconds if the time step is 1. Further, because our goal is to apply an interpolation function, having extra values that go beyond the request time comes in handy, as we will see shortly.

If we switch our focus to the new version of \textit{subRunDynamics}, it is clear that lines 4 to 10 are equal to the previous implementation. On line 11, a new record of type \texttt{Parameters} is being created, especifically to these special cases of mismatch between stop time and size of the time step. The differences within this special record are relevant: the time field is being fulfilled with the actual stop time and the stage field of the solver is being set to \textbf{-1}. The latter is how the driver tells the integrator to do something non-ordinary with this parametric record. Later, as we will see, the integrator will check for negative values for the stage and execute a slightly different pipeline.

This parameters record, however, will only be used if, and \textbf{only} if, we detect that a divergence took place, based on the configuration of the simulation. This is being checked in line 15, where it is being compared the conversion of the last iteration to time with the provided stop time in the \texttt{Interval} type. If they are not discrepant, it means that the simulation can proceed normally. Otherwise, the stop time is between the two last iterations, and cannot be represented because of a too big time step. So, the solution is to cut the last iteration, given that we know that it surpasses the end of the simulation, and add a new member to the list of outcomes, a computation where the \texttt{ps} record is being applied. This major step is happening on line 17.

Next, the integrator needs to be modified in order to cope with negative value in solver stages. The following \textit{interpolate} function will be an add-on to the integrator:

\begin{code}
interpolate :: Dynamics Real -> Dynamics Real
interpolate (Dynamics m) = 
  Dynamics $ \ps -> 
  if stage (solver ps) >= 0 then 
    m ps
  else 
    let iv = interval ps
        sl = solver ps
        t  = time ps
        st = dt sl
        x  = (t - startTime iv) / st
        n1 = max (floor x) (iterationLoBnd iv st)
        n2 = min (ceiling x) (iterationHiBnd iv st)
        t1 = iterToTime iv sl n1 0
        t2 = iterToTime iv sl n2 0
        z1 = m $ ps { time = t1,
                      iteration = n1,
                      solver = sl { stage = 0 } }
        z2 = m $ ps { time = t2,
                      iteration = n2,
                      solver = sl { stage = 0 } }        
    in do y1 <- z1
          y2 <- z2
          return $ y1 + (y2 - y1) * (t - t1) / (t2 - t1)
\end{code}

Lines 4 to 6 are the normal workflow for positive values in the \texttt{stage} field. If a corner comes in, the reminaing code applies \textbf{linear interpolation} to it. It accomplishes this by first comparing the next and previous times that the integrator can model based on the time step (lines 14 and 15). These time points are calculated by their correspondent iterations (lines 12 and 13) that comprise the time point of iterest (line 9). Then, the integrator calculates the outcomes in these two bounds, i.e., do applications of the previous and next modeled times points with their respective parametic records (lines 16 to 21). Finally, lines 22 to 24 execute the linear interpolation with the obtained values that contain the non-modeled time point. It is worth noting that this interpolation will \textbf{always} other in the special cases.

The last step in this tweak is to add this function into the integrator function \textit{diffInteg}. The code is almost identical to the one presented in chapter 3, \textit{The Side Effect Beast}. The main difference is in line 10, where the interpolation function is being applied to \texttt{z}:

\begin{spec}
diffInteg :: Integrator -> Dynamics Double -> Dynamics ()
diffInteg integ diff =
  do let z = Dynamics $ \ps ->
           do whatToDo <- readIORef (computation integ)
              let i = initial integ
              case method (solver ps) of
                Euler -> integEuler diff i whatToDo ps
                RungeKutta2 -> integRK2 diff i whatToDo ps
                RungeKutta4 -> integRK4 diff i whatToDo ps
     liftIO $ writeIORef (computation integ) (interpolate z)     
\end{spec}

This concludes the first tweak in \texttt{Rivika}. Now, the mismatches between the stop time of the simulation and the time step are being treated differently, reducing the error in the final result thanks to the added linear interpolation. Although not perfect, this adds more versatility, given that it is possible to use different interpolation functions, not just linear ones, to mitigate the approximation problem caused by the computer's discreteness. The next chapter, \textit{The Speed Pill: Caching}, explains why the program is still slow and how this can be fixed with a caching strategy.
