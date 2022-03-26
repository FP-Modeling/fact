\ignore{
\begin{code}
module GraduationThesis.Lhs.Englightenment where
import GraduationThesis.Lhs.Implementation
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

Figure \ref{fig:modelPipe} depicts an example of a mathematical system alongside its implementation with explained notion of \texttt{Model}. Below it, it is visiually presented the general pipeline used to create any model:

\begin{figure}[ht!]
\begin{minipage}{.5\textwidth}
\begin{code}
exampleModel :: Model [Double]
exampleModel =
  do integ <- newInteg 1
     let y = integValue integ
     integDiff integ y
     return $ sequence [y]
\end{code}
\end{minipage}
\begin{minipage}{.47\textwidth}
\begin{center}
$\dot{y} = y \quad \quad y(0) = 1$
\end{center}
\end{minipage}
\begin{center}
\includegraphics[width=0.95\linewidth]{GraduationThesis/img/ModelPipeline}
\end{center}
\caption{With the \texttt{Applicative} typeclass, it is possible to \textbf{compose} \texttt{Dynamics} types. The pure function wrapped with the type can be correctly plumbered to the second value inside the same shell type, generating the result.}
\label{fig:modelPipe}
\end{figure}

The remaining piece of the puzzle is grasp who picks the model and its simulation specification, e.g., start time, stop time, which method will be used, and provides the final result. In this sense, the function \texttt{runDynamics} is the \textbf{driver}, i.e., it generates a list with the calculated answers.

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
