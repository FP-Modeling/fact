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

For the guided journey, the same example described in the chapter \textit{Introduction} will be used in this section. So, we will be solving a Lorenz system, composed by a set of chaotic solutions, called \textbf{the Lorenz Attractor}. In these type of system, the ordinary differential equations are used to model chaotic systems, providing solutions based on parameter values and initial conditions. The original differential equations, as well as the recursive description, are presented in Figure \ref{fig:lorenzEq}:

\begin{figure}[ht!]
\begin{center}
$$\frac{dx}{dt} = \sigma(y(t) - x(t)) \rightarrow \int \frac{dx}{dt} = \int \sigma(y(t) - x(t)) dt \rightarrow x(t) = \int (y(t) - x(t)) dt$$
$$\frac{dy}{dt} = x(t)(\rho - z(t)) \rightarrow \int \frac{dy}{dt} = \int x(t)(\rho - z(t)) dt \rightarrow y(t) = \int x(t)(\rho - z(t)) dt$$
$$\frac{dz}{dt} = x(t)y(t) - \beta z(t) \rightarrow \int \frac{dz}{dt} = \int x(t)y(t) - \beta z(t) dt \rightarrow z(t) = \int x(t)y(t) - \beta z(t) dt$$
\end{center}
\caption{When applying integration of both sides of the equation, it is possible to obtain the \textbf{recursive} version of the Lorenz's equations}
\label{fig:lorenzEq}
\end{figure}

With the recursive versions of the systems on-hand, it is straight-forward to map it to the described domain-specific language (DSL). The remaining details are simulation-related, i.e., which solver method will be used, the interval of the simulation, as well as the size of the time step. Taking into account that the constants $\sigma$, $\rho$ and $\beta$ need to be set, the code below summarizes it:

\begin{spec}
lorenzInterv = Interval { startTime = 0,
                          stopTime = 40 }

lorenzSolver = Solver { dt = 0.01,
                        method = RungeKutta2,
                        stage = 0
                      }

sigma = 10.0
rho = 28.0
beta = 8.0 / 3.0

lorenzModel :: Model [Double]
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

mainLorenz =
  do ans <- runDynamics lorenzModel lorenzInterv lorenzSolver
     print ans
\end{spec}

The first two records, \texttt{Interval} and \texttt{Solver}, sets the enviornment (line 1 to 6). The former determines the simulation interval, from start to finish, and the latter configures the solver with $0.01$ as the time step, whilst executing the second-order Runge-Kutta method from the initial stage. The \textit{lorenzModel}, presented after setting the constants (line 7 to 9), executes the aforementioned pipeline to create the model: allocate memory, create readers, change the computation and dispatch it (line 10 to 21). Finally, the function \textit{mainLorenz} groups everything together calling the \textit{runDynamics} driver (line 22 and 23). Later, the result values are provided to the end user in the terminal (line 24).

After this overview, let's follow the execution path of the $x$ variable, given the other two variables are analogous. First and foremost, memory is allocated for the integrator to work with. Figure \ref{fig:allocateExample} depicts this idea, as well as being a reminder of what the \textit{newInteg} and \textit{initialize} functions do, described in the chapter \textit{The Side-Effect Beast}. Abstracted terms are being used to leverage intuition.

\figuraBib{ExampleAllocate}{After \textit{newInteg}, this record is the final result. The function \textit{initialize} gives us protecting against wrong records of the type \texttt{Parameters}, assuring it begins from the first iteration, i.e., $t_0$}{}{fig:allocateExample}{width=.90\textwidth}%

The new record, \textit{integX} has two fields: \texttt{initial} and \texttt{computation}. The former is a simple value of the type \texttt{Dynamics Double} that, regardless of the parameters record it receives, it returns the initial value of the system. The latter is a pointer or address that references in memory a specific \texttt{Dynamics Double} computation: in the case of receiving a parameters record, it fixes potential problems with it. Later, it applies this fixed value in order to get \texttt{i}, i.e., the initial value $1$, the same being saved in the other field of the record.

The creation of the independent variables of interest, $x$, $y$ and $z$, coincides with the next step, create readers via the \textit{readInteg} function. These variables will read the computations being executing under the hood by the integrator. The core idea is to read from the computation pointer inside the integrator and create a new \texttt{Dynamics Double} value. Figure \ref{fig:readExample} portrays this mental image.

\figuraBib{ExampleRead}{After \textit{readInteg}, the final floating point valus is obtained by reading from memory a dynamic computation and passing to it the received parameters record. The result of this application, $v$, is the returned value.}{}{fig:readExample}{width=.90\textwidth}%

When reading a value from a integrator, the computation pointer is being used to access the memory region previously allocated. Although the \textit{initilize} function is in the middle of the way, what's being stored in memory is a \texttt{Dynamics Double} value. The reader, $x$ in this case, combines its received \texttt{Parameters} value and, with the already read value from memory, \textbf{applies} it to the dynamic function. The result is then returned.

The final step before calling the driver is to \textbf{change} the computation \textbf{inside} the memory region. Until this moment, the stored computation is always returning the value of the system at $t_0$, whilst changing the obtained parameters record to be correct. Our goal is to modify this to actually calculate the solution of the differential equations by using numerical methods, i.e., using the solver of the simulation. The function \textit{diffInteg} fulfills this role and its functionality is illustrated in Figure \ref{fig:changeExample}.

\figuraBib{ExampleChange}{The \textit{diffInteg} function only does side-effects, meaning that only affects memory. The internal variable \texttt{c} is a pointer to the computation \textit{itself}, i.e., the dynamic computation being created references this exact procedure}{}{fig:changeExample}{width=.90\textwidth}%

The \textit{diffInteg} function uses integrator's information, such as initial value and computation pointer, to change the content pointed by the latter. As depicted in the image, the procedure of type \texttt{Dynamics Double} stored in the memory region creates an internal reference to \textbf{itself}, meaning that when the dynamic computation is triggered the function \textit{RK2} will have access to this exact behaviour. This is due to the implicit recursion that the solver uses, as will be explained in a few pages. Moreover, the initial value of the integrator is also being used by that function, as well as the differential equation itself, $\sigma(y - x)$ in this case. All those information will be used by the chosen solver method.

After establishing the environment in which the integrator will execute, the final image of the variable or readers is showed in Figure \ref{fig:finalModelExample}.

\figuraBib{ExampleFinalModel}{After setting up the environment, this is the final depiction of an independent variable. The reader $x$ reads the values computed by the procedure stored in memory, a second-order Runge-Kutta method in this case}{}{fig:finalModelExample}{width=.90\textwidth}%

As explained in the previous section, the last step of any given model is to traverse the monads and wrap it with a \texttt{Dynamics} monad. This means that the list of variables in the model, with the signature \texttt{[Dynamics Double]}, is transformed into a value with the type \texttt{Dynamics [Double]} after the \textit{sequence} function. The transformation can be visually understood when looking at Figure \ref{fig:finalModelExample}. Instead of picking one \textit{ps} of type \texttt{Parameters} and returning a value \textit{v}, the same parameters record returns a \textbf{list} of values, meaning that the \textbf{same} parametric dependency is being applied to $[x, y, z]$.

????????
At the end of the model, a second shell or wrapper is added, i.e., the value is wrapped by the \texttt{Dynamics} type.
????????

After this step, the driver of the simulation executes the built model. The function \textit{runDynamics} or \textit{runDynamicsFinal} only create and apply the initial parametric record to the model and call their respective auxiliary functions --- \textit{subRunDynamics} and \textit{subRunDynamicsFinal} respectively. The main goal of such functions is to calculate the result of the system at \textbf{every iteration} within the interval of interest. The chosen time step as well as solver method are metrics used in the transformation from the \textbf{time} axis to the \textbf{iteration} axis. For all the iterations contained in the time interval, an individual parametric record of type \texttt{Parameters} is created and applied to the previous explained dynamic computation, meaning that the \texttt{Dynamic} value in Figure \ref{fig:finalModelExample} is executed by supplying it with a dependency of type \texttt{Parameters}, one for each iteration in this case.

However, this only addresses \textbf{how} the driver triggers the entire execution and does explain how the differential equations are actually being calculated. This is done by the solver functions, such as \textit{integEuler} function described in chapter 3, \textit{The Side-Effect Beast}. These functions, regardless of the chosen method, are all based on equation \ref{eq:solverEquation}, also introduced in chapter 3. The equation goes as following:

$$y_{n+1} = y_n + hf(t_n,y_n) \rightarrow y_n = y_{n-1} + hf(t_{n-1}, y_{n-1})$$

The equation above makes the dependencies in the \textit{RK2} function more clear: the previous iteration result ($y_{n-1}$), the time step ($h$) and differential equation itself ($f(t_{n-1}, y_{n-1})$) of the previous iteration. Further, the dependency $d$ in the \textit{RK2} function is the differential equation, whilst $ps$ carries solver information, such as the size of the time step. Finally, the dependency $c$ is the call of a \textbf{solver step}, meaning that it is capable of calculating the previous step $y_{n-1}$. This is accomplished in a \textbf{recursive} manner, since for every iteration the previous one is necessary. When the base case is achieved, by calculating the value at the first iteration using the $i$ dependency, the recursion stops and the process folds with the final result for the iteration that has started the chain.

