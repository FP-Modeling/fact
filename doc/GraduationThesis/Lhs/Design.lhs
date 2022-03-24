\ignore{
\begin{code}
module GraduationThesis.Lhs.Design where
\end{code}
}

In the previous chapter, the importance of making a bridge between two different sets of abstractions, computers and the physical domain, was clearly established. In this chapter, the core philosophy behind the implementation of this link will be explained in detail, starting with an introduction to the strong type system used in Haskell, going all the way to understanding how to model the main entities of the problem. In the end, the presented modeling strategy will justify the types used in the solution, paving the floor for the next chapter \textit{Implementation}.

\section{The Shape of Information}
\label{sec:types}

Types in programming languages can be viewed as the format of information. This attribute is used to make constraints and add security around data manipulation. Figure \ref{fig:simpleTypes} illustrates pictorial representations of data types and Figure \ref{fig:functions} shows how types can be used to restrain which data can be plumbered into and from a function.

\begin{figure}[ht!]
\centering
\begin{minipage}[t]{.45\textwidth}
  \centering
  \includegraphics[width=0.85\linewidth]{GraduationThesis/img/SimpleTypes}
  \captionof{figure}{Types are not just labels; they enhance the manipulated data with new information. Their difference in shape can work as the interface of the data.}
  \label{fig:simpleTypes}
\end{minipage}
\hspace{1cm}
\begin{minipage}[t]{.45\textwidth}
  \centering
  \includegraphics[width=0.95\linewidth]{GraduationThesis/img/PictorialFunction}
  \captionof{figure}{Functions' signatures are contracts; they specify which shape the input information has as well as which shape the output information will have.}
  \label{fig:functions}
\end{minipage}
\end{figure}

Primitive types, e.g., \texttt{Int}, \texttt{Double} and \texttt{Char}, are not powerful enough to \textbf{model} more complicated data structures. This problem can be solved by using type \textbf{composition}, thus allowing the creation of more versatile and useful types. In this context, composition means binding or gluing existent types together to create more sophisticated abstractions, such as recursive structures and records of information. Two \textbf{algebraic data types} provide type composition in Haskell.

The sum type, also known as tagged union in type theory, is an algebraic data type that introduces \textbf{choice} across multiple options using a single label. For instance, the boolean data type has two options or representatives: \texttt{False} or \texttt{True}, where these are mutually exclusive. When using this type either of them will be of type \texttt{Bool}. A given type can have any number of representatives, but only one of them can be used at a given moment. Figure \ref{fig:sumType} depicts more use cases in which a given entry of the type can only assume one of the available possibilities.

\begin{figure}[ht!]
\centering
\begin{minipage}{.5\textwidth}
  \centering
  \begin{spec}
  data Parity = Even | Odd

  data DigitalStates = High | Low | Z
  \end{spec}
\end{minipage}
\begin{minipage}{.49\textwidth}
  \centering
  \includegraphics[width=0.95\linewidth]{GraduationThesis/img/SumType}
\end{minipage}
\caption{Sum types can be understood in terms of sets, in which the members of the set are available candidates for the outer shell type. Parity and possible values in digital states are examples.}
\label{fig:sumType}
\end{figure}

The second algebraic data type available is the product type, which does composition by \textbf{combining} types, using a type constructor. While the sum type adds choice in the language, this data type requires multiple types to assemble a new one in a mutually inclusive manner. For example, the type \texttt{Name} can be visualized as a combination of two separate strings, \texttt{Firstname} and \texttt{Lastname}, combined by the wrapper \texttt{Fullname}. In order to have any possible name, it is necessary to provide \textbf{both} parts. Effectively, the product type executes a cartesian product with its parts. Figure \ref{fig:productType} illustrates common examples of using combined data.

\begin{figure}[ht!]
\centering
\begin{minipage}{.57\textwidth}
  \centering
  \begin{spec}
  data ClockTime = Time Int Int

  data SpacePosition = Point Double Double Double

  data SpacePosition = Point { x :: Double,
                               y :: Double,
                               z :: Double }
  \end{spec}
\end{minipage}
\begin{minipage}{.4\textwidth}
  \centering
  \includegraphics[width=0.95\linewidth]{GraduationThesis/img/ProductType}
\end{minipage}
\caption{Product types are a combination of different sets, where you pick a representative from each one. Digital clocks' time and objects' coordinates in space are common use cases. In Haskell, a product type can be defined using a \textbf{record} alongside with the constructor, where the labels for each member inside it are explicit.}
\label{fig:productType}
\end{figure}

Within algebraic data types, it is possible to abstract the \textbf{structure} out, meaning that the outer shell of the type can be understood as a common pattern changing only the internal content. For instance, if a given application can take advantage of fractional values but want to use the same configuration as the one presented in the \texttt{SpacePosition} data type, it's possible to add this customization. This feature is known as \textit{parametric polymorphism}, a powerful tool which is available in Haskell. An example is presented in Figure \ref{fig:parametricPoly} using the \texttt{SpacePosition} type.

\begin{figure}[ht!]
\centering
\begin{minipage}{.57\textwidth}
  \centering
  \begin{spec}
  data SpacePosition a = Point a a a

  data SpacePosition a = Point { x :: a,
                                 y :: a,
                                 z :: a }
  \end{spec}
\end{minipage}
\begin{minipage}{.4\textwidth}
  \centering
  \includegraphics[width=0.95\linewidth]{GraduationThesis/img/ParametricPoly}
\end{minipage}
\caption{Depending on the application, different representations of the same structure need to used due to the domain of interest and/or memory contraints.}
\label{fig:parametricPoly}
\end{figure}

In some situations, changing the type of the structure is not the desired property of interest. There are applications where some sort of \textbf{behaviour} is a necessity, e.g., the ability of comparing two instances of a custom type, define common mathematical operations for the created type. This nature of polymorphism is known as \textit{ad hoc polymorphism}, which is implemented in Haskell via what is similar to java-like interfaces, so-called \textbf{typeclasses}. However, establishing a contract with a typeclass differs from an interface in a fundamental aspect: rather than inheritance being given to the type, \textbf{mathematical formalism} is assured for it. As an example, the implementation of the typeclass \texttt{Eq} gives to the type all comparable operations ($==$ and $!=$), as well as any theorems or proofs in regard to such operations. Figure \ref{fig:adHocPoly} shows the implementation of \texttt{Ord} typeclass for the presented \texttt{ClockTime}, giving it capabilities for sorting instances of such type.

\begin{figure}[ht!]
\centering
\begin{minipage}{.4\textwidth}
  \centering
  \begin{spec}
  data ClockTime = Time Int Int

  instance Ord ClockTime where
    (Time a b) <= (Time c d)
      = (a <= c) && (b <= d)

  \end{spec}
\end{minipage}
\begin{minipage}{.57\textwidth}
  \centering
  \includegraphics[width=0.95\linewidth]{GraduationThesis/img/AdHocPoly}
\end{minipage}
\caption{The minimum requirement for the \texttt{Ord} typeclass is the $<=$ operator, meaning that the functions $<$, $<=$, $>$, $>=$, \texttt{max} and \texttt{min} are now unlocked for the type \texttt{ClockTime} after the implementation.}
\label{fig:adHocPoly}
\end{figure}

As demonstrated, the algebraic data types, when combined with polymorphism, are a powerful tool in programming, being a useful way to model the domain of interest. However, both sum and product types cannot portray by themselves the intuition of a \textbf{procedure}. A data transformation process, as showed in Figure \ref{fig:functions}, can be utilized in a variety of different ways. Imagine, for instance, a system where validation can vary according to the current situation. Any validation algorithm would be using the same data, such as a record called \texttt{SystemData}, and returning a boolean as the result of the validation, but the internal guts of these functions would be totally different. This is being represented in Figure \ref{fig:pipeline}.

\figuraBib{Pipeline}{Replacements of the validation function within a pipeline like the above is common}{}{fig:pipeline}{width=.75\textwidth}%

In Haskell, this motivates the use of functions as \textbf{first class citizens}, meaning that they can be treated equally in comparison with data types that carries information, such as being used as arguments to another functions, so-called high order functions.

\section{Modeling Reality}
\label{sec:diff}

The continuous time problem explained in the introduction was initially addressed by mathematics, which represents physical quantities by \textbf{differential equations}. This set of equations establish a relationship between functions and their respective derivatives; the function express the variable of interest and its derivative describe how it changes over time. It is common in the engineering and physics domain to know the rate of change of a given metric, but the function itself is still unknown. Thus, numerical methods have been developed over the years in order to discover the second if the first is well-known at a given moment in time, within an interval of approximation.

While some differential equations have more than one independent variable per functions, being classified as a \textbf{partial diffential equation}, some phenomena can be modeled with only one indepedent variable per function in a given set, being described as modeled by a set of \textbf{ordinary differential equations}. These latter sets can be solved by numerical procedures, such as the Euler method and the Runge-Kutta method, both being numerical methods. These mechanisms \textbf{quantize} the physical time duration into an interval of floating point numbers, spaced by a \textbf{time step} and starting from an \textbf{initial value}. Afterward, the derivative is used to calculate the slope or the direction in which the tangent of the function is moving in time in order to predict the value of the next step, i.e., determine which point better represents the function in the next time step. The order of the method varies its precision during the prediction of the steps, e.g, the Runge-Kutta method of 4th order is more precise than the Euler method or the Runge-Kutta of 2nd order.

The first-order Euler method calculates the next step by the following relations:

\begin{equation}
\dot{y}(t) = f(t, y(t)) \quad y(t_0) = y_0
\label{eq:diffEq}
\end{equation}

This equality assumes that the next step following the derivative's direction will not be that different from the actual value of the function $y$ if the time step is small enough. Further, it is assumed that in case of a small enough time step, the diference between time samples is $h$, i.e., the time step, with the following equation representing one step of the method: 

\begin{equation}
y_{n + 1} = y_n + hf(t_n, y_n)
\label{eq:nextStep}
\end{equation}

So, the next step of the function $y_{n+1}$ can be computed by the sum of the previous step $y_n$ with the predicted value obtained by the derivative $f(t_n,y_n)$ multiplied by the time step $h$. Figure \ref{fig:eulerExample} provides an example of a step-by-step solution of one differential equation using the Euler method. In this case, the unknown function is the exponential function $e_t + t$ and the time of interest is $t = 5$.

\begin{figure}[H]
$$ \dot{y} = y + t \quad \quad y(0) = 1 $$
$$ \downarrow $$
$$ y_{n + 1} = y_n + hf(t_n, y_n) \quad h = 1 \quad t_{n + 1} = t_n + h \quad f(t,y) = y + t $$
$$ y_{1} = y_0 + 1 * f(0, y_0) \rightarrow y_{1} = 1 + 1 * 1 + 0 \rightarrow y_{1} = 2 $$
$$ y_{2} = y_1 + 1 * f(1, y_1) \rightarrow y_{2} = 2 + 1 * 2 + 1 \rightarrow y_{2} = 5 $$
$$ y_{3} = y_2 + 1 * f(2, y_2) \rightarrow y_{3} = 4 + 1 * 4 + 2 \rightarrow y_{3} = 10 $$
$$ y_{4} = y_3 + 1 * f(3, y_3) \rightarrow y_{4} = 8 + 1 * 8 + 3 \rightarrow y_{4} = 19 $$
$$ y_{5} = y_4 + 1 * f(4, y_4) \rightarrow y_{5} = 16 + 1 * 16 + 4 \rightarrow y_{5} = 36 $$
\caption{The initial value is used as a starting point for the procedure. The algorithm continues until the time of interest is reached in the unknown function. Due to its large time step, the final answer is really far-off from the expected result.}
\label{fig:eulerExample}
\end{figure}

As showed in equation \ref{eq:diffEq}, both the derivative and the function --- the mathematical formulation of the system --- varies according to \textbf{time}. Both acts as functions in which for a given time value, it produces an outcome, a numerical value in this case. These two entities, the rate of change and the behaviour, can be simplified into a single relationship, when applying integration on both sides of the equality:

$$ \frac{dy}{dt} = y + t $$
$$ \int \frac{dy}{dt} dt = \int (y(t) + t) dt $$
$$ y(t) = \int (y(t) + t) dt $$

When arranged in this manner the target integral is now \textbf{recursive}, meaning that the function of interest can be expressed in terms of itself. Also, the same aspect of being a function from time to outcome is preserved.

\section{Cybernization of Mathematics}

Our primary goal is to combine the knowledge levegered in section \ref{sec:types} --- how powerful a strong type system can be --- with the core notion of differential equations presented in section \ref{sec:diff}.

Any representation of a physical system that can be modeled by a set of differential equations, being written in terms the rate of change and the function or recursively, has an outcome value at any given moment of time. The type \texttt{Dynamics} in Figure \ref{fig:firstDynamics} is a first draft of representing the continuous physical dynamics~\cite{LeeModeling} --- the evolution of a system state in time:


\begin{figure}[ht!]
\centering
\begin{minipage}{.43\textwidth}
  \centering
  \begin{spec}
  type Time = Double
  type Outcome = Double
  data Dynamics = Time -> Outcome
  \end{spec}
\end{minipage}
\begin{minipage}{.56\textwidth}
  \centering
  \includegraphics[width=0.95\linewidth]{GraduationThesis/img/SimpleDynamics}
\end{minipage}
\caption{In Haskell, the \texttt{type} keyword works for alias. The first draft of the \texttt{Dynamics} type is a \textbf{function}, in which providing a floating point value as time returns another value as outcome.}
\label{fig:firstDynamics}
\end{figure}

This type seems to capture the concept, whilst being similar to the definition of a tagged system presented by Lee and Sangiovanni~\cite{LeeSangiovanni}. However, in order to use numerical methods to solve the problem, additional information at a given moment is necessary, such as which method is being used, in which stage the method is and when the final time will be reached. With this in mind, new types need to be introduced. Figure \ref{fig:dynamicsAux} shows the auxiliary types to biuld a new \texttt{Dynamics}:

\begin{figure}[ht!]
\centering
\begin{minipage}{.62\textwidth}
  \centering
\begin{code}
data Interval = Interval { startTime :: Double,
                           stopTime  :: Double 
                         } deriving (Eq, Ord, Show)

data Method = Euler
            | RungeKutta2
            | RungeKutta4
            deriving (Eq, Ord, Show)

data Solver = Solver { dt        :: Double,     
                       method    :: Method, 
                       stage     :: Int
                     } deriving (Eq, Ord, Show)

data Parameters = Parameters { interval  :: Interval,
                               solver    :: Solver,
                               time      :: Double,
                               iteration :: Int
                             } deriving (Eq, Show)

\end{code}
\end{minipage}
\begin{minipage}{.37\textwidth}
  \centering
  \includegraphics[width=0.95\linewidth]{GraduationThesis/img/DynamicsAuxTypes}
\end{minipage}
\caption{The \texttt{Parameters} type represents a given moment in time, carrying over all the necessary information to execute a solver step until the time limit is reached. Some useful typeclasses are being derived to these types, given that Haskell is capable of infering the implementation of typeclasses in simple cases.}
\label{fig:dynamicsAux}
\end{figure}

The above auxiliary types serve a common purpose: to provide at any given moment in time, all the information to execute a solver method until the end of the simulation. The type \texttt{Interval} determines when the simulation should start and when it should end. The \texttt{Method} sum type is used inside the \texttt{Solver} type to set solver sensible information, such as the size of the time step, which method will be used and in which stage the method is in at the current moment. Finally, the \texttt{Parameters} type combines everything together, alongside with the current time value as well as its discrete counterpart, iteration.

Further, the new \texttt{Dynamics} type can also be parametrically polymorphic, removing the limitation of only using \texttt{Double} values as the outcome. Figure \ref{fig:dynamics} depicts the final type for the physical dynamics. As illustrated, there are some details not explained yet, but, unfortunately, only in the next chapter enough context will be provided to fully understand the need of the \texttt{IO} wrapper.

\begin{figure}[ht!]
\centering
\begin{minipage}{.44\textwidth}
  \centering
\begin{code}
data Dynamics a =
     Dynamics (Parameters -> IO a)
\end{code}
\end{minipage}
\begin{minipage}{.55\textwidth}
  \centering
  \includegraphics[width=0.95\linewidth]{GraduationThesis/img/Dynamics}
\end{minipage}
\caption{The \texttt{Dynamics} type is a function of from time related information to an arbitraty outcome value.}
\label{fig:dynamics}
\end{figure}

This summarizes the bridge between the mathematical world and how the we are modeling this domain in Haskell. The next chapter, \textit{Implementation}, will start from this foundation, by adding typeclasses to the \texttt{Dynamics} type, and will later describe the last core type before explaining the solver execution: the \texttt{Integrator} type.
