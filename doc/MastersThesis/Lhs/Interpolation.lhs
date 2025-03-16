\ignore{
\begin{code}
module MastersThesis.Lhs.Interpolation where
import MastersThesis.Lhs.Design
import Control.Monad.Trans.Reader

type Model a = CT (CT a)

iterToTime :: Interval -> Solver -> Int -> Stage -> Double
iterToTime _ _ _ Interpolate = error "Incorrect stage: Interpolate"
iterToTime interv solver n (SolverStage st) =
  if st < 0 then 
    error "Incorrect solver stage in iterToTime"
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

The previous chapter ended anouncing that drawbacks are present in the current implementation. This chapter will introduce the first concern: numerical methods do not reside in the continuous domain, the one we are actually interested in. After this chapter, this domain issue will be addressed via \textit{interpolation}, with a few tweaks in the integrator and driver.

\section{Time Domains}

When dealing with continuous time, \texttt{FACT} changes the domain in which \textit{time} is being modeled. Figure \ref{fig:timeDomains} shows the domains that the implementation interact with during execution:

\figuraBib{TimeDomains}{During simulation, functions change the time domain to the one that better fits certain entities, such as the \texttt{Solver} and the driver. The image is heavily inspired by a figure in~\cite{Edil2017}}{}{fig:timeDomains}{width=.85\textwidth}%

The problems starts in the physical domain. The goal is to obtain a value of an unknown function $y(t)$ at time $t_x$. However, because the solution is based on \textit{numerical methods} a sampling process occurs and the continuous time domain is transformed into a \textit{discrete} time domain, where the solver methods reside --- those are represented by the functions \textit{integEuler}, \textit{integRK2} and \textit{integRK4}. A solver depends on the chosen time step to execute a numerical algorithm. Thus, time is modeled by the sum of $t_0$ with $n\Delta$, where $n$ is a natural number. Hence, from the solver perspective, time is always dependent on the time step, i.e., only values that can be described as $t_0 + n\Delta$ can be properly visualized by the solver. Finally, there's the \textit{iteration} domain, used by the driver functions, \textit{runCT} and \textit{runCTFinal}. When executing the driver, one of its first steps is to call the function \textit{iterationsBnds}, which converts the simulation time interval to a tuple of numbers that represent the amount of iterations based on the time step of the solver. This function is presented bellow:

\begin{spec}
iterationBnds :: Interval -> Double -> (Int, Int)
iterationBnds interv dt = (0, ceiling ((stopTime interv - startTime interv) / dt))
\end{spec}

To achieve the total number of iterations, the function \textit{iterationBnds} does a \textit{ceiling} operation on the sampled result of iterations, based on the time interval (\textit{startTime} and \textit{stopTime}) and the time step (\texttt{dt}). The second member of the tuple is always the answer, given that it is assumed that the first member of the tuple is always zero.

The function that allows us to go back to the discrete time domain being in the iteration axis is the \textit{iterToTime} function. It uses the solver information, the current iteration and the interval to transition back to time, as depicted by the following code:

\begin{spec}
iterToTime :: Interval -> Solver -> Int -> Int -> Double
iterToTime interv solver n st =
  if st < 0 then 
    error "Incorrect solver stage in iterToTime"
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

A transformation from iteration to time depends on and on the chosen solver method due to their next step functions.
For instance, the second and forth order Runge-Kutta methods have more stages, and it uses fractions of the time step for more granular use of the derivative function. This is why lines 11 and 12 are using half of the time step. Moreover, all discrete time calculations assume that the value starts from the beginning of the simulation (\textit{startTime}). The result is obtained by the sum of the initial value, the solver-dependent \textit{delta} function and the iteration times the solver time step (line 6).

There is, however, a missing transition: from the discrete time domain to the domain of interest in CPS --- the continuous time axis. This means that if the time value $t_x$ is not present from the solver point of view, it is not possible to obtain $y(t_x)$. The proposed solution is to add an \textit{interpolation} function into the pipeline, which addresses this transition. Thus, values in between solver steps will be transfered back to the continuous domain.

\section{Tweak I: Interpolation}

This tweak in the current implementation is divided into three parts: the types, the driver and the integrator. These entities will communicate with each other to properly adapt the outcome. As mentioned previously, we will add an interpolation function to change from the discrete domain to the continuous one. However, this interpolation procedure needs to occur only in special situations: when it is not possible to model that specific point in time in the discrete time domain. Otherwise, the execution should continue as it is.

Hence, there is a need to introduce a mechanism to identify these different situations. As the solution, we will add the new type depicted in Figure~\ref{fig:factAux}.

\begin{figure}[ht!]
\centering
\begin{minipage}{.62\textwidth}
  \centering
\begin{purespec}
data Stage = SolverStage Int
           | Interpolate
           deriving (Eq, Ord, Show)
\end{purespec}
\end{minipage}
\begin{minipage}{.37\textwidth}
  \centering
  \includegraphics[width=0.95\linewidth]{MastersThesis/img/FACTAuxTypes}
\end{minipage}
\caption{Updated auxiliary types for the \texttt{Parameters} type.}
\label{fig:factAux}
\end{figure}

The type \texttt{Stage} allows values to be either the normal flow of execution, marked by the use of \texttt{SolverStage}, or the
indication that an extra step for interpolation needs to be done, marked by the \texttt{Interpolate} tag. Moreover, previous types and functions
described in previous chapters, such as \textit{Design Philosophy}, and \textit{Effectful Integrals} need to be adapted to use this new
type instead of the original \texttt{Int} previously proposed (in chapter 2, \textit{Design Philosophy}). Types like \texttt{Parameters} and
functions like \textit{integEuler}, \textit{iterToTime}, and \textit{runCT} need to be updated accordingly. In all of those instances, processing will just continue
normally; \texttt{SolverStage} will be used.

Next, the driver needs to be updated. So, the proposed mechanism is the following: the driver will identify these corner cases and communicate to the integrator --- via the new \texttt{Stage} field in the \texttt{Solver} data type --- that the interpolation needs to be added into the pipeline of execution. When this flag is not on, i.e., the \texttt{Stage} informs to continue execution normally, the implementation goes as the previous chapters detailed. This behaviour is altered \textit{only} in particular scenarios, which the driver will be responsible for identifying.

It remains to re-implement the driver functions. The driver will notify the integrator that an interpolation needs to take place. The code below shows these changes:

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

runCT :: Model a -> Double -> Solver -> IO [a]
runCT m t sl =
  do let iv = Interval 0 t
         (nl, nu) = iterationBnds iv (dt sl)
         parameterize n =
            let time = iterToTime iv sl n (SolverStage 0)
                solver = sl {stage = SolverStage 0}
            in Parameters { interval = iv,
                            time = time,
                            iteration = n,
                            solver = solver }
         disct = iterToTime iv sl nu (SolverStage 0)
         values = map (runReaderT m . parameterize) [nl .. nu]
  sequence $
    if disct - t < epslon
    then values
    else let ps = Parameters { interval = iv,
                               time = t,
                               iteration = nu,
                               solver = sl {stage = Interpolate} }
         in init values ++ [runReaderT m ps]
\end{spec}

The implementation of \textit{iterationBnds} uses \textit{ceiling} function because this rounding is used to go to the iteration domain. However, given that the interpolation \textit{requires} both solver steps --- the one that came before $t_x$ and the one immediately
afterwards --- the number of iterations needs always to surpass the requested time. For instance, the time 5.3 seconds will demand the fifth and sixth iterations with a time step of 1 second. When using \textit{ceiling}, it is assured that the value of interest will be in the interval of computed values. So, when dealing with 5.3, the integrator will calculate all values up to 6 seconds.

Lines 5 to 15 are equal to the previous implementation of the \textit{runCT} function. On line 16, the discrete version of \texttt{t}, \texttt{disct}, will be used for detecting if an
interpolation will be needed. All the simulation values are being prepared on line 17 --- Haskell being a lazy language the label \texttt{values} will not necessarily
be evaluated strictly. Line 19 establishes a condition, checkiing if the difference between the time of interest \texttt{t} and \texttt{disct} is greater or not
than a value \texttt{epslon}, to identify if
the normal flow of execution can proceed. If it can't, on line 22 a new record of type \texttt{Parameters} is created (\texttt{ps}), especifically to these special cases of mismatch between discrete and continuous time. The main difference within this special record is relevant: the stage field of the solver is being set to \texttt{Interpolate}.
Finally, on line 25 the last element from the list of outputs \texttt{values} is removed and it is appended the simulation using the created \texttt{ps} with
interpolation configured.

\begin{code}
interpolate :: CT Double -> CT Double
interpolate m = do
  ps <- ask
  case stage $ solver ps of
    SolverStage _ -> m
    Interpolate   ->
      let iv = interval ps
          sl = solver ps
          t  = time ps
          st = dt sl
          x  = (t - startTime iv) / st
          n1 = max (floor x) (iterationLoBnd iv st)
          n2 = min (ceiling x) (iterationHiBnd iv st)
          t1 = iterToTime iv sl n1 (SolverStage 0)
          t2 = iterToTime iv sl n2 (SolverStage 0)
          ps1 = ps { time = t1,
                     iteration = n1,
                     solver = sl { stage = SolverStage 0 }}
          ps2 = ps { time = t2,
                     iteration = n2,
                     solver = sl { stage = SolverStage 0 }}
          z1 = local (const ps1) m
          z2 = local (const ps2) m
      in z1 + (z2 - z1) * pure ((t - t1) / (t2 - t1))
\end{code}

Lines 1 to 5 continues the simulation with the normal workflow. If a corner case comes in, the reminaing code applies \textit{linear interpolation} to it. It accomplishes this by first comparing the next and previous discrete times (lines 16 and 19) relative to \texttt{x} (line 11) --- the discrete counterpart of the time of interest \texttt{t} (line 9). These time points are calculated by their correspondent iterations (lines 12 and 13). Then, the integrator calculates the outcomes at these two points, i.e., do applications of the previous and next modeled times points with their respective parametric records (lines 22 and 23). Finally, line 24 executes the linear interpolation with the obtained values that surround the non-discrete time point. This particular interpolation was chosen for the sake of simplicity, but it can be replaced by higher order methods. Figure \ref{fig:interpolate} illustrates the effect of the \textit{interpolate} function when converting domains.

\begin{spec}
updateInteg :: Integrator -> CT Double -> CT ()
updateInteg integ diff = do
  let i = initial integ
      z = do
        ps <- ask
        whatToDo <- liftIO $ readIORef (computation integ)
        case method (solver ps) of
          Euler -> integEuler diff i whatToDo
          RungeKutta2 -> integRK2 diff i whatToDo
          RungeKutta4 -> integRK4 diff i whatToDo
  liftIO $ writeIORef (computation integ) (interpolate z)
\end{spec}

\figuraBib{Interpolate}{Linear interpolation is being used to transition us back to the continuous domain.}{}{fig:interpolate}{width=.7\textwidth}%

The last step in this tweak is to add this function into the integrator function \textit{updateInteg}. The code is almost identical to the one presented in chapter 3, \textit{Effectful Integrals}. The main difference is in line 11, where the interpolation function is being applied to \texttt{z}. Figure \ref{fig:diffInterpolate} shows the same visual representation for the \textit{updateInteg} function used in chapter 4, but with the aforementioned modifications.

\figuraBib{DiffIntegInterpolate}{The new \textit{updateInteg} function add linear interpolation to the pipeline when receiving a parametric record}{}{fig:diffInterpolate}{width=.9\textwidth}%

This concludes the first tweak in \texttt{FACT}. Now, the mismatches between the stop time of the simulation and the time step are being treated differently, going back to the continuous domain thanks to the added interpolation. The next chapter, \textit{Caching the Speed Pill}, goes deep into the program's performance and how this can be fixed with a caching strategy.
