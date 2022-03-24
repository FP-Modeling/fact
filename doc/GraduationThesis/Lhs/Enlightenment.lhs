\ignore{
\begin{code}
module GraduationThesis.Lhs.Englightenment where
import GraduationThesis.Lhs.Implementation
import GraduationThesis.Lhs.Design
\end{code}
}

Previously, the latter core type of the implementation, the \texttt{Integrator}, was presented and explained in detail as well as why it can model an integral when used with the \texttt{Dynamics} type. This chapter is a follow-up, and its objectives are twofold: reveal and describe which functions executes a given example and, with that final piece unraveled, execute and follow the execution of a set of differential equations to see some action and proof-of-concept.

\section{Who is driving the bus?}

A model is a function which builds, sets and manipulates all the aformentioned integrator functions; creates variables to set the system of differential equations; and produces the final list of values --- just like the lorenz example in chapter \ref{Introduction}.

\begin{code}
type Model a = Dynamics (Dynamics a)
\end{code}

A model is a value \texttt{a} wrapped with two \texttt{Dynamics} type. The former shell is responsable for grouping all the variables of interest, i.e., \textit{x}, \textit{y} and \textit{z} from the system of equation in the \texttt{lorenzModel}, and providing to them the same initial \texttt{Parameters} record. The latter wrapper is the computation of each variable, in a individual basis.

Below, there are an example of a model with one variable and its mathematical parallel:

\begin{figure}[H]
\centering
\begin{minipage}{.5\textwidth}
  \centering
\begin{code}
exampleModel :: Model [Double]
exampleModel =
  do integ <- newInteg 1
     let y = integValue integ
     integDiff integ y
     return $ sequence [y]
\end{code}
\captionof{figure}{Implementation of the system}
\label{rivika:example}
\end{minipage}%
\begin{minipage}{.5\textwidth}
  \centering
    $\dot{y} = y \quad \quad y(0) = 1$
  \captionof{figure}{Mathematical system}
\label{math:rivika}
\end{minipage}
\end{figure}

Similarly with the lorenz's model presented in the introduction, the \texttt{example} model follows the following structure:

\begin{itemize}
\item Create an integrator with 1 being the initial value;
\item Create a variable by reading the computation stored inside the integrator;
\item Change the integrator so it knows which differential equation will be used in the solver;
\item Traverse the result using the \texttt{sequence} function, so instead of having a list of \texttt{Dynamics Double}, we have the type \texttt{Dynamics [Double]}, i.e., a dynamic computation that will provide us a list of results in the specified time interval.
\end{itemize}

The remaining piece of the puzzle is to execute the simulation itself, making what is known to be \textbf{the driver} of the simulation. The function \texttt{runDynamics} picks a model and a specification of the simulation --- both of which were described in detail in the past sections --- and generates a list with the calculated answers.

\ignore{
\begin{code}
iterToTime :: Specs -> Int -> Int -> Double
iterToTime sc n st =
  if st < 0 then 
    error "Incorrect stage: iterToTime"
  else
    (startTime sc) + n' * (dt sc) + delta (method sc) st
      where n' = fromInteger (toInteger n)
            delta Euler       0 = 0
            delta RungeKutta2 0 = 0
            delta RungeKutta2 1 = dt sc
            delta RungeKutta4 0 = 0
            delta RungeKutta4 1 = dt sc / 2
            delta RungeKutta4 2 = dt sc / 2
            delta RungeKutta4 3 = dt sc
            delta _ _ = 0

iterationBnds :: Specs -> (Int, Int)
iterationBnds sc = (0, round ((stopTime sc - 
                               startTime sc) / dt sc))
\end{code}
}

\begin{code}
runDynamics :: Model a -> Specs -> IO [a]
runDynamics (Dynamics m) sc = 
  do d <- m Parameters { specs = sc,
                         time = startTime sc,
                         iteration = 0,
                         stage = 0 }
     sequence $ subrunDynamics d sc
     
subrunDynamics :: Dynamics a -> Specs -> [IO a]
subrunDynamics (Dynamics m) sc =
  do let (nl, nu) = iterationBnds sc
         parameterise n = Parameters { specs = sc,
                                       time = iterToTime sc n 0,
                                       iteration = n,
                                       stage = 0 }
     map (m . parameterise) [nl .. nu]
\end{code}

\ignore{
\begin{code}
iterationHiBnd :: Specs -> Int
iterationHiBnd sc = snd $ iterationBnds sc                   

runDynamicsFinal :: Model a -> Specs -> IO a
runDynamicsFinal (Dynamics m) sc = 
  do d <- m Parameters { specs = sc,
                         time = startTime sc,
                         iteration = 0,
                         stage = 0 }
     subrunDynamicsFinal d sc

subrunDynamicsFinal :: Dynamics a -> Specs -> IO a
subrunDynamicsFinal (Dynamics m) sc =
  do let n = iterationHiBnd sc
         t = iterToTime sc n 0
     m Parameters { specs = sc,
                    time = t,
                    iteration = n,
                    stage = 0 }
\end{code}
}

On line 3, it is being created the initial \texttt{Parameters} record; this record will be used in all computations for all variables simultaneously. There are two utilitarian functions; \texttt{iterToTime} (line 12) does a converstion from the domain of discrete steps to the domain of time and \texttt{iterationBnds} (line 10), which generates a tuple with the first and last iterations. The The main function ends by calling the \texttt{subrunDynamics} function. This auxiliary function calculates, \textbf{in sequence}, for all $\frac{stopTime - startTime}{timeStep}$ steps the result using the chosen solving method.

Additionally, there are analogous versions of these two functions, so-called \texttt{runDynamicsFinal} and \texttt{subrunDynamicsFinal}, that return only the final result of the simulation, i.e., $y(stopTime)$.

\section{Our best friend: an Example}
