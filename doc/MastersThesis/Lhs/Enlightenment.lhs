\ignore{
\begin{code}
module MastersThesis.Lhs.Enlightenment where
import MastersThesis.Lhs.Implementation
import MastersThesis.Lhs.Interpolation
import MastersThesis.Lhs.Caching
import MastersThesis.Lhs.Design

type Vector = [Double]

lorenzInterv = Interval { startTime = 0,
                          stopTime = 100 }

lorenzSolver = Solver { dt = 0.01,
                        method = RungeKutta2,
                        stage = 0
                      }

sigma = 10.0
rho = 28.0
beta = 8.0 / 3.0

lorenzModel :: Model Vector
lorenzModel =
  do integX <- newInteg 1.0
     integY <- newInteg 1.0
     integZ <- newInteg 1.0
     let x = readInteg integX
         y = readInteg integY
         z = readInteg integZ
     diffInteg integX (sigma * (y - x))
     diffInteg integY (x * (rho - z) - y)
     diffInteg integZ (x * y - beta * z)
     return $ sequence [x, y, z]
     
lorenzSystem = runDynamics lorenzModel lorenzInterv lorenzSolver
\end{code}
}

Previously, we presented in detail the latter core type of the implementation, the \texttt{Integrator}, as well as why it can model an integral when used with the \texttt{Dynamics} type. This chapter is a follow-up, and its objectives are threefold: describe how to map a set of differential equations to an executable model, reveal which functions execute a given example and present a guided-example as a proof-of-concept.

\section{From Models to Models}

Systems of differential equations reside in the mathematical domain. In order to \textbf{execute} using the \texttt{Rivika} DSL, this model needs to be converted into an executable model following the DSL's guidelines. Further, we saw that these requirements resemble FF-GPAC's description of its basic units and rules of composition. Thus, these mappings between the three worlds need to be established. Chapters 2 and 3 explained the mapping between \texttt{Rivika} and FF-GPAC. It remains to map the \textit{semantics} of the mathematical world to the \textit{operational} world of \texttt{Rivika}. This mapping goes as the following:

\begin{itemize}
  \item The relationship between the derivatives and their respective functions will be modeled by \textbf{feedback} loops with \texttt{Integrator} type.
  \item The initial condition will be modeled by the \texttt{initial} pointer within an integrator.
  \item Combinational aspects, such as addition and multiplication of constants and the time $t$, will be represented by typeclasses and the \texttt{Dynamics} type.
\end{itemize}

With that in mind, Figure \ref{fig:exampleSingle} illustrates an example of a model in \texttt{Rivika}, alongside its mathematical counterpart. Further, Figure \ref{fig:rivika2gpac} shows which FF-GPAC circuit each line is modeling. This pipeline effectively makes \texttt{Rivika} a bridge between a physical system, modeled by differential equations, and the FF-GPAC model proposed by Graça~\cite{Graca2003}.

\begin{figure}[ht!]
\begin{minipage}{.5\textwidth}
\begin{spec}
t :: Dynamics Double
t = Dynamics $ \ps -> return (time ps)

exampleModel :: Dynamics Double
exampleModel =
  do integ <- newInteg 1
     let y = readInteg integ
     diffInteg integ (y + t)
     y
\end{spec}
\end{minipage}
\begin{minipage}{.47\textwidth}
\begin{center}
$\dot{y} = y + t \quad \quad y(0) = 1$
\end{center}
\end{minipage}
\caption{The integrator functions are essential to create and interconnect combinational and feedback-dependent circuits.}
\label{fig:exampleSingle}
\end{figure}

\figuraBib{Rivika2GPAC}{The developed DSL translates a system described by differential equations to an executable model that resembles FF-GPAC's description}{}{fig:rivika2gpac}{width=.8\textwidth}%

In line 5, a record with type \texttt{Integrator} is created, with $1$ being the initial condition of the system. Line 6 creates a \textbf{state variable}, a label that gives us access to the output of an integrator, \texttt{integ} in this case. Afterward, in line 7, the \textit{diffInteg} function connects the inputs to a given integrator by creating a combinational circuit, \texttt{(y + t)}. Polynomial circuits and integrators' outputs can be used as available inputs, as well as the \textit{time} of the simulation. Finally, line 8 returns the state variable as the output for the \textbf{driver}, the main topic of the next section.

There is, however, an useful improvement to be made into the definition of a model within the DSL. The presented example used only a single state variable, although it is common to have \textbf{multiple} state variables, i.e., multiple integrators interacting with each other, modeling different aspects of a given scenario. Moreover, when dealing with multiple state variables, it is important to maintain \textbf{synchronization} between them, i.e., the same \texttt{Parameters} is being applied to \textbf{all} state variables at the same time.

To address both of these requirements, we will use the \textit{sequence} function, available in Haskell's standard library. This function manipulates \textbf{nested} structures and change their internal structure. The only requirement is that the outer type have to implement the \texttt{Traversable} typeclass. For instance, applying this function to a list of values of type \texttt{Maybe} would generate a single \texttt{Maybe} value in which its content is a list of the previous content individually wrapped by the \texttt{Maybe} type. This is only possible because the external or "bundler" type, list in this case, has implemented the \texttt{Traversable} typeclass. Figure \ref{fig:sequence} depicts the example before and after applying the function.

\figuraBib{Sequence}{Because the list implements the \texttt{Traversable} typeclass, it allows this type to use the \textit{traverse} and \textit{sequence} functions, in which both are related to changing the internal behaviour of the nested structures}{}{fig:sequence}{width=.95\textwidth}%

Similarly to the preceding example, the list structure will be used to involve all the state variables with type \texttt{Dynamics Double}. This tweak is effectively creating a \textbf{vector} of state variables whilst sharing the same notion of time across all of them. So, the final type signature of a model is \texttt{Dynamics [Double]} or, by using a type aliases for \texttt{[Double]} as \texttt{Vector}, \texttt{Dynamics Vector}. A second alias can be created to make it more descriptive, as exemplified in Figure \ref{fig:exampleMultiple}:

\begin{figure}[ht!]
\begin{minipage}{.5\textwidth}
\begin{spec}
type Vector = [Double]
type Model a = Dynamics a

exampleModel :: Model Vector
exampleModel =
  do integX <- newInteg 1
     integY <- newInteg 1
     let x = readInteg integX
         y = readInteg integY
     diffInteg integX (x * y)
     diffInteg integY (y + t)
     sequence [x, y]

\end{spec}
\end{minipage}
\begin{minipage}{.47\textwidth}
\begin{center}
$\dot{x} = y * x \quad \quad x(0) = 1$
$\dot{y} = y + t \quad \quad y(0) = 1$
\end{center}
\end{minipage}
\caption{A \textbf{state vector} comprises multiple state variables and requires the use of the \textit{sequence} function to sync time across all variables.}
\label{fig:exampleMultiple}
\end{figure}

Finally, when creating a model, the same steps have to be done in the same order, always starting with the integrator functions and finishing with the \textit{sequence} function being applied to a state vector. So, Figure \ref{fig:modelPipe} depicts the general pipeline used to create any model in both the semantics and operational perspectives:

\begin{figure}[H]
\begin{center}
\includegraphics[width=0.97\linewidth]{MastersThesis/img/ModelPipeline}
\end{center}
\caption{When building a model for simulation, the above pipeline is always used, from both points of view. The operations with meaning, i.e., the ones in the \texttt{Semantics} pipeline, are mapped to executable operations in the \texttt{Operational} pipeline, and vice-versa.}
\label{fig:modelPipe}
\end{figure}

\section{Driving the Model}

Given a physical model translated to an executable one, it remains to understand which functions drive the simulation, i.e., which functions take the simulations details into consideration and generate the output. The function \textit{runDynamics} fulfills this role:

\begin{spec}
runDynamics :: Model a -> Interval -> Solver -> IO [a]
runDynamics (Dynamics m) iv sl =
  do let (nl, nu) = iterationBnds iv (dt sl)
         parameterise n = Parameters { interval = iv,
                                       time = iterToTime iv sl n 0,
                                       iteration = n,
                                       solver = sl { stage = 0 }}
     sequence $ map (m . parameterise) [nl .. nu]
\end{spec}

On line 3, we convert the \textit{time} interval of the simulation to an \textit{iteration} interval in the format of a tuple, i.e., the continuous interval becomes the tuple $(0, \frac{stopTime - startTime}{timeStep})$, in which the second value of the tuple is \textbf{rounded}. From line 4 to line 7, we are defining an auxiliary function \textit{parameterise}. This function picks a natural number, which represents the iteration index, and creates a new record with the type \texttt{Parameters}. Additionally, it uses the auxiliary function \textit{iterToTime} (line 5), which converts the iteration number from the domain of discrete \textbf{steps} to the domain of \textbf{discrete time}, i.e., the time the solver methods can operate with (chapter 5 will explore more of this concept). This conversion is based on the time step being used, as well as which method and in which stage it is for that specific iteration. Finally, line 8 produces the outcome of the \textit{runDyanmics} function. The final result is the output from a function called \textit{map} piped it as an argument for the \textit{sequence} function.

The \textit{map} operation is provided by the \texttt{Functor} of the list monad, and it applies an arbitrary function to the internal members of a list in a \textbf{sequential} manner. In this case, the \textit{parameterise} function, composed with the dynamic application \texttt{m}, is the one being mapped. Thus, a custom value of the type \texttt{Parameters} is taking place of each natural natural number in the list, and this is being applied to the received \texttt{Dynamics} value. It produces a list of answers in order, each one wrapped in the \texttt{IO} monad. To abstract out the \texttt{IO}, thus getting \texttt{IO [a]} rather than \texttt{[IO a]}, the \textit{sequence} function finishes the implementation.

Additionally, there is an analogous implementation of this function, so-called \textit{runDynamicsFinal}, that return only the final result of the simulation, i.e., $y(stopTime)$, instead of the outputs at the time step samples.

\section{An attractive example}

For the example walkthrough, the same example introduced in the chapter \textit{Introduction} will be used in this section. So, we will be solving a system, composed by a set of chaotic solutions, called \textbf{the Lorenz Attractor}. In these types of systems, the ordinary differential equations are used to model chaotic systems, providing solutions based on parameter values and initial conditions. The original differential equations are presented bellow:

$$ \sigma = 10.0 $$
$$ \rho = 28.0 $$
$$ \beta = \frac{8.0}{3.0} $$
$$$$
$$$$
$$ \frac{dx}{dt} = \sigma(y(t) - x(t)) $$
$$ \frac{dy}{dt} = x(t)(\rho - z(t)) $$
$$ \frac{dz}{dt} = x(t)y(t) - \beta z(t) $$
$$$$

It is straight-forward to map it to the described domain-specific language (DSL). The remaining details are simulation-related, e.g., which solver method will be used, the interval of the simulation, as well as the size of the time step. Taking into account that the constants $\sigma$, $\rho$ and $\beta$ need to be set, the code below summarizes it, and Figure \ref{fig:gpacLorenz} shows its FF-GPAC circuit:

\begin{spec}
lorenzInterv = Interval { startTime = 0,
                          stopTime = 100 }

lorenzSolver = Solver { dt = 0.01,
                        method = RungeKutta2,
                        stage = 0
                      }

sigma = 10.0
rho = 28.0
beta = 8.0 / 3.0

lorenzModel :: Model Vector
lorenzModel =
  do integX <- newInteg 1.0
     integY <- newInteg 1.0
     integZ <- newInteg 1.0
     let x = readInteg integX
         y = readInteg integY
         z = readInteg integZ
     diffInteg integX (sigma * (y - x))
     diffInteg integY (x * (rho - z) - y)
     diffInteg integZ (x * y - beta * z)
     sequence [x, y, z]

lorenzSystem = runDynamics lorenzModel lorenzInterv lorenzSolver
\end{spec}

\figuraBib{GPACLorenz}{Using only FF-GPAC's basic units and their composition rules, it's possible to model the Lorenz Attractor example}{}{fig:gpacLorenz}{width=.90\textwidth}%

\newpage

The first two records, \texttt{Interval} and \texttt{Solver}, sets the environment (lines 1 to 6). The former determines the simulation interval (lines 1 and 2), from start to finish, and the latter configures the solver with $0.01$ seconds as the time step, whilst executing the second-order Runge-Kutta method from the initial stage (lines 3 to 6). The \textit{lorenzModel}, presented after setting the constants (lines 7 to 9), executes the aforementioned pipeline to create the model: allocate memory (lines 12 to 14), create read-only pointers (lines 15 to 17), change the computation (lines 18 to 20) and dispatch it (line 21). Finally, the function \textit{lorenzSystem} groups everything together calling the \textit{runDynamics} driver (line 22).

After this overview, let's follow the execution path used by the compiler. Haskell's compiler works in a lazily manner, meaning that it calls for execution only the necessary parts. So, the first step calling \textit{lorenzSystem} is to call the \textit{runDynamics} function with a model, interval and solver configurations. Following its path of execution, the \textit{map} function (inside the driver) forces the application of a parametric record generated by the \textit{parameterise} function to the provided model, \textit{lorenzModel} in this case. Thus, it needs to be executed in order to return from the \textit{runDynamics} function.

To understand the model, we need to follow the execution sequence of the output: \texttt{sequence [x, y, z]}, which requires executing all the lines before this line to obtain the all the state variables. For the sake of simplicity, we will follow the execution of the operations related to the $x$ variable, given that the remaining variables have an analogous execution walkthrough. First and foremost, memory is allocated for the integrator to work with (line 12). Figure \ref{fig:allocateExample} depicts this idea, as well as being a reminder of what the \textit{newInteg} and \textit{initialize} functions do, described in the chapter \textit{Effectful Integrals}. In this image, the integrator \texttt{integX} comprises two fields, \texttt{initial} and \texttt{computation}. The former is a simple value of the type \texttt{Dynamics Double} that, regardless of the parameters record it receives, it returns the initial condition of the system. The latter is a pointer or address that references a specific \texttt{Dynamics Double} computation in memory: in the case of receiving a parametric record \texttt{ps}, it fixes potential problems with it via the \texttt{initialize} block, and it applies this fixed value in order to get \texttt{i}, i.e., the initial value $1$, the same being saved in the other field of the record, \texttt{initial}.

\figuraBib{ExampleAllocate}{After \textit{newInteg}, this record is the final image of the integrator. The function \textit{initialize} gives us protecting against wrong records of the type \texttt{Parameters}, assuring it begins from the first iteration, i.e., $t_0$}{}{fig:allocateExample}{width=.90\textwidth}%

The next step is the creation of the independent state variable $x$ via \textit{readInteg} function (line 15). This variable will read the computations that are executing under the hood by the integrator. The core idea is to read from the computation pointer inside the integrator and create a new \texttt{Dynamics Double} value. Figure \ref{fig:readExample} portrays this mental image. When reading a value from an integrator, the computation pointer is being used to access the memory region previously allocated. Also, what's being stored in memory is a \texttt{Dynamics Double} value. The state variable, $x$ in this case, combines its received \texttt{Parameters} value, so-called \texttt{ps}, and \textbf{applies} it to the stored dynamic function. The result \texttt{v} is then returned.

\figuraBib{ExampleRead}{After \textit{readInteg}, the final floating point values is obtained by reading from memory a dynamic computation and passing to it the received parameters record. The result of this application, $v$, is the returned value}{}{fig:readExample}{width=.90\textwidth}%

The final step is to \textbf{change} the computation \textbf{inside} the memory region (line 18). Until this moment, the stored computation is always returning the value of the system at $t_0$, whilst changing the obtained parameters record to be correct via the \textit{initialize} function. Our goal is to modify this behaviour to the actual solution of the differential equations via using numerical methods, i.e., using the solver of the simulation. The function \textit{diffInteg} fulfills this role and its functionality is illustrated in Figure \ref{fig:changeExample}. With the integrator \texttt{integX} and the differential equation $\sigma (y - x)$ on hand, this function picks the provided parametric record \texttt{ps} and it returns the result of a step of the solver \texttt{RK2}, second-order Runge-Kutta method in this case. Additionally, the solver method receives as a dependency what is being pointed by the \texttt{computation} pointer, represented by \texttt{c} in the image, alongside the differential equation and initial value, pictured by \texttt{d} and \texttt{i} respectively.

\figuraBib{ExampleChange}{The \textit{diffInteg} function only does side effects, meaning that only affects memory. The internal variable \texttt{c} is a pointer to the computation \textit{itself}, i.e., the dynamic computation being created references this exact procedure}{}{fig:changeExample}{width=.90\textwidth}%

Figure \ref{fig:finalModelExample} shows the final image for state variable $x$ after until this point in the execution.

\figuraBib{ExampleFinalModel}{After setting up the environment, this is the final depiction of an independent variable. The reader $x$ reads the values computed by the procedure stored in memory, a second-order Runge-Kutta method in this case}{}{fig:finalModelExample}{width=.90\textwidth}%

Lastly, the state variable is wrapped inside a list and it is applied to the \textit{sequence} function, as explained in the previous section. This means that the list of variable(s) in the model, with the signature \texttt{[Dynamics Double]}, is transformed into a value with the type \texttt{Dynamics [Double]}. The transformation can be visually understood when looking at Figure \ref{fig:finalModelExample}. Instead of picking one \texttt{ps} of type \texttt{Parameters} and returning a value \textit{v}, the same parametric record returns a \textbf{list} of values, with the \textbf{same} parametric dependency being applied to all state variables inside $[x, y, z]$.

However, this only addresses \textbf{how} the driver triggers the entire execution, but does \textbf{not} explain how the differential equations are actually being calculated with the \texttt{RK2} numerical method. This is done by the solver functions (\textit{integEuler}, \textit{integRK2} and \textit{integRK4}) and those are all based on equation \ref{eq:solverEquation} regardless of the chosen method. The equation goes as the following:

$$y_{n+1} = y_n + hf(t_n,y_n) \rightarrow y_n = y_{n-1} + hf(t_{n-1}, y_{n-1})$$

The equation above makes the dependencies in the \texttt{RK2} example in Figure \ref{fig:finalModelExample} clear:

\begin{itemize}
 \item \texttt{d} $\Rightarrow$ Differential Equation that will be used to obtain the value of the previous iteration ($f(t_{n-1}, y_{n-1})$).
 \item \texttt{ps} $\Rightarrow$ Parametric record with solver information, such as the size of the time step ($h$).
 \item \texttt{i} and \texttt{c} $\Rightarrow$ The initial value of the system, as well as a solver step function, will be used to calculate the previous iteration result ($y_{n-1}$).
\end{itemize}

It is worth mentioning that the dependency \texttt{c} is a call of a \textbf{solver step}, meaning that it is capable of calculating the previous step $y_{n-1}$. This is accomplished in a \textbf{recursive} manner, since for every iteration the previous one is necessary. When the base case is achieved, by calculating the value at the first iteration using the \texttt{i} dependency, the recursion stops and the process folds, getting the final result for the iteration that has started the chain. This is the same pattern across all the implemented solvers (\texttt{Euler}, \texttt{RungeKutta2} and \texttt{RungeKutta4}).

\section{Lorenz's Butterfly}

After all the explained theory behind the project, it remains to be seen if this can be converted into practical results. With certain constant values, the generated graph of the Lorenz's Attractor example used in the last chapter is known for oscillation and getting the shape of two fixed point attractors, meaning that the system evolves to an oscillating state even if slightly disturbed. As showed in Figure \ref{fig:lorenzPlots}, the obtained graph from the Lorenz's Attractor model matches what was expected for a Lorenz's system. It is worth noting that changing the values of $\sigma$, $\rho$ and $\beta$ can produce completely different answers, destroying the resembled "butterfly" shape of the graph.

\begin{figure}[ht!]
\begin{minipage}{.60\textwidth}
\begin{center}
  \includegraphics[width=1.0\linewidth]{MastersThesis/img/LorenzPlot1}
\end{center}
\end{minipage}
\begin{minipage}{.39\textwidth}
\begin{center}
  \includegraphics[width=1.0\linewidth]{MastersThesis/img/LorenzPlot2}
\end{center}
\end{minipage}
\caption{The Lorenz's Attractor example has a very famous butterfly shape from certain angles and constant values in the graph generated by the solution of the differential equations.}
\label{fig:lorenzPlots}
\end{figure}

Although correct, the presented solution has a few drawbacks. The next two chapters will explain and address the two identified problems with the current implementation.
