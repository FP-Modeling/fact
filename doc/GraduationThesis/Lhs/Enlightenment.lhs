\ignore{
\begin{code}
module GraduationThesis.Lhs.Enlightenment where
import GraduationThesis.Lhs.Design
\end{code}
}

Previously, the latter core type of the implementation, the \texttt{Integrator}, was presented and explained in detail as well as why it can model an integral when used with the \texttt{Dynamics} type. This chapter is a follow-up, and its objectives are twofold: reveal and describe which functions executes a given example and, with that final piece unraveled, execute and follow the execution of a set of differential equations to see some action and proof-of-concept.

\section{Who is driving the bus?}

With the main functionality of the program out-of-the-way, it remains to understand how and who, i.e., which functions and their behaviour, executes a set of differential equations. When the system is written using the integrator functions described in the last chapter, the final product is called a \textbf{model}. This model comprises memory allocation for the integrator, set reader pointer and change the internal procedure of the integrator to an actual differential equation solving computation. However, the final line of the previous examples is still a mystery: what exactly the \textit{return} and \textit{sequence} do and what's the meaning behind it?

The $sequence$ function does a \textbf{traverse} operation, meaning that it inverts the order of nested monads inside a value. For instance, applying this function to a list of values of type \texttt{Maybe} would generate a single \texttt{Maybe} value in which its content is a list of the previous content individually wrapped by the \texttt{Maybe} type. This is only possible because the external monad, list in this case, has implemented the \texttt{Traverse} typeclass. Figure \ref{fig:sequence} depicts the example before and after applying the function.

\figuraBib{Sequence}{The \texttt{Traverse} typeclass allows the type to use the \textit{traverse} and \textit{sequence} function, in which both are related to apply inversion with structured data.}{}{fig:sequence}{width=.95\textwidth}%

The main use case of this function when building a model is to combine multiple dynamic computation, i.e., multiple calculation of differential equations, in one single \texttt{Dynamics} typed value. So, because the created readers, as explained in the previous chapter, have type \texttt{Dynamics Double}, the \textit{sequence} function picks the values wrapped inside a "bundler" type, such as list, and invert it to obtain a single result. Thus, a list of \texttt{Dynamics Double} is transformed into a value with the signature \texttt{Dynamics [Double]}. Moreover, only the external structure needs to have implemented the aforementioned \texttt{Traverse} typeclass; and the list monad has the implementation built into the language.

Finally, it is desireable, during a simulation, to initialize all the differential equations with the \textbf{same} initial condition to the same variable used in each differential equation. However, because we have multiple dynamic computations, this is not an easy-task to accomplish. The solution is to wrap all the written \texttt{Dynamics [Double]} with a new \texttt{Dynamics} shell. In this manner, whem applied with a value with the type \texttt{Parameters}, the same application will be happening to all the internal computations. This nested structure, a dynamic value inside another one, is the representation of a model in the project and has its own alias:

\begin{code}
type Model a = Dynamics (Dynamics a)
\end{code}

Figure \ref{fig:modelPipe} depicts an example of a mathematical system alongside its implementation with the \texttt{Model} alias. Below it, it is visiually presented the general pipeline used to create any model:

\begin{figure}[ht!]
\begin{minipage}{.5\textwidth}
\begin{spec}
exampleModel :: Model [Double]
exampleModel =
  do integ <- newInteg 1
     let y = readInteg integ
     diffInteg integ y
     return $ sequence [y]
\end{spec}
\end{minipage}
\begin{minipage}{.47\textwidth}
\begin{center}
$\dot{y} = y \quad \quad y(0) = 1$
\end{center}
\end{minipage}
\begin{center}
\includegraphics[width=0.95\linewidth]{GraduationThesis/img/ModelPipeline}
\end{center}
\caption{When building a model for simulation, the above pipeline is always used. After managing the integrator, switch the order of the shells using \textit{sequence} and wrap everything in \texttt{Dynamics} using \textit{return}.}
\label{fig:modelPipe}
\end{figure}

The remaining piece of the puzzle is grasp who picks the model and its simulation specification, e.g., start time, stop time, which method will be used, and provides the final result. In this sense, the function \texttt{runDynamics} is the \textbf{driver}, i.e., it generates a list with the calculated answers.

\begin{code}
runDynamics :: Model a -> Interval -> Solver -> IO [a]
runDynamics (Dynamics m) iv sl = 
  do d <- m Parameters { interval = iv,
                         time = startTime iv,
                         iteration = 0,
                         solver = sl { stage = 0 }}
     sequence $ subRunDynamics d iv sl
     
subRunDynamics :: Dynamics a -> Interval -> Solver -> [IO a]
subRunDynamics (Dynamics m) iv sl =
  do let (nl, nu) = iterationBnds iv (dt sl)
         parameterise n = Parameters { interval = iv,
                                       time = iterToTime iv sl n 0,
                                       iteration = n,
                                       solver = sl { stage = 0 }}
     map (m . parameterise) [nl .. nu]
\end{code}

On line 3, it is being created the initial \texttt{Parameters} record using the simulation related types, such as \texttt{Interval} and \texttt{Solver}, explained in chapter \textit{Design Philosophy}. This record will be used in all computations for all variables of the system simultaneously. The function ends by calling a second function, \texttt{subRunDynamics}. This auxiliary function calculates, in a \textbf{sequential} manner, the result using the chosen solving method for all iteration steps by applying a \textbf{map} operation. This procedure, being the \texttt{Functor} of the list monad, applying a function to the internal members of it (line 15). In this case, the \textit{parameterise} function (line 11) is being composed the the dynamic application, in which a custom value of the type \texttt{Parameters} is created to each iteration and this is applied to the received \texttt{Dynamics} value. The final result is a list of answers in order, each one wrapped in the \texttt{IO} monad.

There are two utilitarian functions that participate in this process. The \textit{iterationBnds} function (line 10) uses the established time step to convert the \textbf{time} interval to an \textbf{iteration} interval in the format of a tuple, i.e., the continuous interval becomes the tuple $(0, \frac{stopTime - startTime}{timeStep})$. Moreover, the \textit{iterToTime} function (line 12) converts from the domain of discrete steps to the domain of time. This conversion is based on the time step being used, as well as which method and in which stage it is for that specific iteration.

Additionally, there are analogous versions of these two functions, so-called \texttt{runDynamicsFinal} and \texttt{subRunDynamicsFinal}, that return only the final result of the simulation, i.e., $y(stopTime)$.

\ignore{
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

iterationBnds :: Interval -> Double -> (Int, Int)
iterationBnds interv dt = (0, round ((stopTime interv - 
                               startTime interv) / dt))

iterationHiBnd :: Interval -> Double -> Int
iterationHiBnd interv dt = snd $ iterationBnds interv dt

runDynamicsFinal :: Model a -> Interval -> Solver -> IO a
runDynamicsFinal (Dynamics m) iv sl = 
  do d <- m Parameters { interval = iv,
                         time = startTime iv,
                         iteration = 0,
                         solver = sl { stage = 0 }}
     subRunDynamicsFinal d iv sl

subRunDynamicsFinal :: Dynamics a -> Interval -> Solver -> IO a
subRunDynamicsFinal (Dynamics m) iv sl =
  do let n = iterationHiBnd iv (dt sl)
         t = iterToTime iv sl n 0
     m Parameters { interval = iv,
                    time = t,
                    iteration = n,
                    solver = sl { stage = 0 }}
\end{code}
}

\section{Our best friend: an Example}
