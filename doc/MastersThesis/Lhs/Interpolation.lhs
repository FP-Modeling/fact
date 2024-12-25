\ignore{
\begin{code}
module MastersThesis.Lhs.Interpolation where
import MastersThesis.Lhs.Design

type Model a = Dynamics (Dynamics a)

\end{code}
}

The previous chapter ended anouncing that drawbacks are present in the current implementation. This chapter will introduce the first concern: numerical methods do not reside in the continuous domain, the one we are actually interested in. After this chapter, this domain issue will be addressed via \textbf{interpolation}, with a few tweaks in the integrator and driver.

\section{Time Domains}

When dealing with continuous time, \texttt{Rivika} changes the domain in which \textbf{time} is being modeled. Figure \ref{fig:timeDomains} shows the domains that the implementation interact with during execution:

\figuraBib{TimeDomains}{During simulation, functions change the time domain to the one that better fits certain entities, such as the \texttt{Solver} and the driver. The image is heavily inspired by a figure in~\cite{Edil2017}}{}{fig:timeDomains}{width=.85\textwidth}%

The problems starts in the physical domain. The goal is to obtain a value of an unknown function $y(t)$ at time $t_x$. However, because the solution is based on \textbf{numerical methods} a sampling process occurs and the continuous time domain is transformed into a \textbf{discrete} time domain, where the solver methods reside --- those are represented by the functions \textit{integEuler}, \textit{integRK2} and \textit{integRK4}. A solver depends on the chosen time step to execute a numerical algorithm. Thus, time is modeled by the sum of $t_0$ with $n\Delta$, where $n$ is a natural number. Hence, from the solver perspective, time is always dependent on the time step, i.e., only values that can be described as $t_0 + n\Delta$ can be properly visualized by the solver. Finally, there's the \textbf{iteration} domain, used by the driver functions, \textit{runDynamics} and \textit{runDynamicsFinal}. When executing the driver, one of its first steps is to call the function \textit{iterationsBnds}, which converts the simulation time interval to a tuple of numbers that represent the amount of iterations based on the time step of the solver. This functions is presented bellow:


\begin{spec}
iterationBnds :: Interval -> Double -> (Int, Int)
iterationBnds interv dt = (0, round ((stopTime interv - 
                               startTime interv) / dt))
\end{spec}

To achieve the total number of iterations, the function \textit{iterationBnds} does a \textbf{round} operation on the sampled result of iterations, based on the time interval (\textit{startTime} and \textit{stopTime}) and the time step (\texttt{dt}). The second member of the tuple is always the answer, given that it is assumed that the first member of the tuple is always zero.

The function that allows us to go back to the discrete time domain being in the iteration axis is the \textit{iterToTime} function. It uses the solver information, the current iteration and the interval to transition back to time, as depicted by the following code:

\begin{code}
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

A transformation from iteration to time depends on the chosen solver method due to their next step functions. For instance, the second and forth order Runge-Kutta methods have more stages, and it uses fractions of the time step for more granular use of the derivative function. This is why lines 11 and 12 are using half of the time step. Moreover, all discrete time calculations assume that the value starts from the beginning of the simulation (\textit{startTime}). The result is obtained by the sum of the initial value, the solver-dependent \textit{delta} function and the iteration times the solver time step (line 6).

There is, however, a missing transition: from the discrete time domain to the domain of interest in CPS --- the continuous time axis. This means that if the time value $t_x$ is not present from the solver point of view, it is not possible to obtain $y(t_x)$. The proposed solution is to add an \textbf{interpolation} function into the pipeline, which addresses this transition. Thus, values in between solver steps will be transfered back to the continuous domain.

\section{Tweak I: Interpolation}

This tweak in the current implementation is divided into two parts: the driver and the integrator. These entities will communicate with each other to properly adapt the outcome. As mentioned previously, we will add an interpolation function to change from the discrete domain to the continuous one. However, this interpolation procedure needs to occur only in special situations: when it is not possible to model that specific point in time in the discrete time domain. Otherwise, the execution should continue as it is.

So, the proposed mechanism is the following: the driver will identify these corner cases and communicate to the integrator --- via the \texttt{stage} field in the \texttt{Solver} data type --- that the interpolation needs to be added into the pipeline of execution. When this flag is not on, i.e., the \texttt{stage} informs to continue execution normally, the implementation goes as the previous chapters detailed. This behaviour is altered \textbf{only} in particular scenarios, which the driver will be responsible for identifying.

Hence, it remains to re-implement the driver functions. The driver will notify the integrator that an interpolation needs to take place. Furthermore, the function \textit{iterationBnds} will also be modified to use \textit{ceiling} instead of \textit{round}. The reason will be explained further on the line. The code below shows these changes:

\ignore{
\begin{code}
iterationLoBnd :: Interval -> Double -> Int
iterationLoBnd interv dt = fst $ iterationBnds interv dt

iterationHiBnd :: Interval -> Double -> Int
iterationHiBnd interv dt = snd $ iterationBnds interv dt                   

iterationBnds :: Interval -> Double -> (Int, Int)
iterationBnds interv dt = (0, ceiling ((stopTime interv - 
                               startTime interv) / dt))

epslon = 0.00001                          
\end{code}
}

\begin{spec}
iterationBnds :: Interval -> Double -> (Int, Int)
iterationBnds interv dt = (0, ceiling ((stopTime interv - 
                               startTime interv) / dt))

epslon = 0.00001                          

runDynamics :: Model a -> Interval -> Solver -> IO [a]
runDynamics (Dynamics m) iv sl =
  do let (nl, nu) = basicIterationBnds iv (dt sl)
         parameterise n = Parameters { interval = iv,
                                       time = iterToTime iv sl n 0,
                                       iteration = n,
                                       solver = sl { stage = 0 }}
         ps = Parameters { interval = iv,
                           time = stopTime iv,
                           iteration = nu,
                           solver = sl { stage = -1}}
     if (iterToTime iv sl nu 0) - (stopTime iv) < epslon
     then sequence $ map (m . parameterise) [nl .. nu]
     else sequence $ ((init $ map (m . parameterise) [nl .. nu]) ++ [m ps])
\end{spec}

The new implementation of \textit{iterationBnds} is pretty similar to the previous one, with the difference being the replacement of the \textit{round} function for the \textit{ceiling} function. As explained in the previous section, the rounding is used to go to the iteration domain. However, because the interpolation \textbf{requires} both solver steps --- the one that came before $t_x$ and the one immediately
afterwards --- the number of iterations needs always to surpass the requested time. For instance, the time 5.3 seconds will demand the fifth and sixth iterations with a time step of 1 second. When using \textit{ceiling}, it is assured that the value of interest will be in the interval of computed values. So, when dealing with 5.3, the integrator will calculate all values up to 6 seconds.

Lines 5 to 11 are equal to the previous implementation of the \textit{runDynamics} function. On line 12, a new record of type \texttt{Parameters} is being created, especifically to these special cases of mismatch between discrete and continuous time. The differences within this special record are relevant: the time field is being fulfilled with the actual stop time and the stage field of the solver is being set to \textbf{-1}. The latter is how the driver tells the integrator to apply the interpolation function. Later, as we will see, the integrator will check for negative values in the \texttt{stage} field and it will execute a slightly different pipeline.

This parametric record, however, will only be used if, and \textbf{only} if, we detect that a divergence took place, based on the configuration of the simulation. This is being checked in line 16, where it is being compared the conversion of the last iteration to time with the provided stop time in the \texttt{Interval} type record. If they are not discrepant by an \texttt{epslon} value, it means that the simulation can proceed normally. Otherwise, the stop time is between the two last iterations, and cannot be represented discreetly. So, the solution is to cut the last iteration, given that we know that it surpasses the end of the simulation, and add a new member to the list of outcomes, a computation where the altered \texttt{ps} record is being applied. This major step is happening on line 18.

Next, the integrator needs to be modified in order to cope with negative value in solver stages. The following \textit{interpolate} function will be an added to the integrator:

\begin{code}
interpolate :: Dynamics Double -> Dynamics Double
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

Lines 4 to 6 are the normal workflow for positive values in the \texttt{stage} field. If a corner case comes in, the reminaing code applies \textbf{linear interpolation} to it. It accomplishes this by first comparing the next and previous discrete times (lines 14 and 15) relative to \texttt{x} (line 11) --- the discrete counterpart of the time of interest \texttt{t} (line 9). These time points are calculated by their correspondent iterations (lines 12 and 13). Then, the integrator calculates the outcomes at these two points, i.e., do applications of the previous and next modeled times points with their respective parametric records (lines 16 to 21). Finally, lines 22 to 24 execute the linear interpolation with the obtained values that surround the non-discrete time point. Figure \ref{fig:interpolate} illustrates the effect of the \textit{interpolate} function when converting domains.

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

\figuraBib{Interpolate}{Linear interpolation is a transformation that transition us back to the continuous domain}{}{fig:interpolate}{width=.7\textwidth}%

The last step in this tweak is to add this function into the integrator function \textit{diffInteg}. The code is almost identical to the one presented in chapter 3, \textit{Effectful Integrals}. The main difference is in line 10, where the interpolation function is being applied to \texttt{z}. Figure \ref{fig:diffInterpolate} shows the same visual representation for the \textit{diffInteg} function used in chapter 4, but with the aforementioned modifications.

\figuraBib{DiffIntegInterpolate}{The new \textit{diffInteg} function add linear interpolation to the pipeline when receiving a parametric record}{}{fig:diffInterpolate}{width=.9\textwidth}%

This concludes the first tweak in \texttt{Rivika}. Now, the mismatches between the stop time of the simulation and the time step are being treated differently, going back to the continuous domain thanks to the added linear interpolation. The next chapter, \textit{Caching the Speed Pill}, goes deep into the program's performance and how this can be fixed with a caching strategy.
