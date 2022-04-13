\ignore{
\begin{code}
module GraduationThesis.Lhs.Interpolation where
import GraduationThesis.Lhs.Design
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
integEuler (Dynamics diff) (Dynamics i) (Dynamics y) ps =
  case iteration ps of
    0 -> 
      i ps
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

\begin{code}
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
\end{code}

The returned tuple in \textit{iterationBnds} represents how many iterations will be necessary to simulate the given interval using an arbitrary size of time step. The first of the tuple is always zero because only the total amount of iterations is described by the second member. This number can be calculated by discovering how many steps are in the interval. However, this can lead to floating number of iterations if the difference between \textit{stopTime} and \textit{startTime} is \textbf{not} a multiple of the size of the used time step. More importantly, because the \textit{round} function is based on this result, it means that a flooring or ceiling procedures will take place depending on this result, directly affecting how far the simulation will go.

In the second function, \textit{iterToTime}, a similar manipulation happens, with the difference being that the iteration and stage are being converted back to the time axis based on the solver method. This means that this conversion is not perfect, because all the values that cannot be represented by that specific sum --- initial time plus the time step times the iteration plus the solver stage internal step --- will be lost along the way.

The implications of this approach are twofold: the size of the time step directly affects the simulations results if it does not match the interval. For instance, using a time step of 1 in an interval from 0 to 5.3, \textit{iterationBnds} will generate a list from 0 to 5 iterations because of the rounding behaviour. Additionally, \textit{iterToTime} is uncapable of getting back any time that it is not multiple of the time step and cannot be obtained by using the internal solver steps, such as in the Runge-Kutta methods. Intuitively and at first glance, the solution seems simple: just shrink the time step enough to all the number in the interval be multiple of it, like 0.1 as the time step. This, however, is a temporary solution to a permanent problem.

The issue is that, regardless of the time step, it is always possible to make the \textbf{stop time} granular enough to not be a multiple of the time step. Thus, the functions that depend on this property make adjustments that in fact calculates the wrong result for that specific time point of interest. The ideal solution would be to have an \textbf{infitesimal} size for the time step, an impossible achievment due to the computer's \textbf{limitation} of representing real numbers as floating points with double precision. Hence, it is not possible to completely cope with the problem, and it exemplifies one of the biggest challenges within the CPS domain, the mathematical requirements to model the continuous are just not available into digital computers.

Thus, the proposed solution described in the next section is not perfect, but atenuates the wrong outcome to some extent by changing the \textit{diffInteg} integrator function to check for this corner case, as well as adapting the driver to also be aware of this situation. In summary, if we can't grasp the time point exactly, then we will try to approximate the solution by \textbf{interpolating} two close results that we have access.

\section{Tweak I: Interpolation}

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
        r | t == t1   = z1
          | t == t2   = z2
          | otherwise = 
            do y1 <- z1
               y2 <- z2
               return $ y1 + (y2 - y1) * (t - t1) / (t2 - t1)
    in r
\end{code}
