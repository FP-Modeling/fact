\ignore{
\begin{code}
module MastersThesis.Lhs.Implementation where
import MastersThesis.Lhs.Interpolation
import MastersThesis.Lhs.Design
import Control.Monad.Trans
import Data.IORef
import Control.Monad.Trans.Reader
\end{code}
}

This chapter details the next steps to simulate continuous-time behaviours. It starts by enhancing the previously defined \texttt{CT} type by implementing some specific typeclasses. Next, the second core type of the simulation, the \texttt{Integrator} type, will be introduced alongside its functions. These improvements will then be compared to FF-GPAC's basic units, our source of formalism within the project. At the end of the chapter, an implicit recursion will be blended with a lot of effectful operations, making the \texttt{Integrator} type hard to digest. This will be addressed by a guided Lorenz Attractor example in the next chapter, \textit{Execution Walkthrough}.

\section{Uplifting the CT Type}
\label{sec:typeclasses}

The \texttt{CT} type needs \textbf{algebraic operations} to be better manipulated, i.e., useful operations that can be applied to the type preserving its external structure. These procedures are algebraic laws or properties that enhance the capabilities of the proposed function type wrapped by a \texttt{CT} shell. Towards this goal, a few typeclasses need to be implemented.

Across the spectrum of available typeclasses in Haskell, we are interested in the ones that allow data manipulation with a single or multiple \texttt{CT} and provide mathematical operations. To address the former group of operations, the typeclasses \texttt{Functor}, \texttt{Applicative}, \texttt{Monad} and \texttt{MonadIO} will be implemented. The later group of properties is dedicated to provide mathematical operations, such as $+$ and $\times$, and it can be acquired by implementing the typeclasses \texttt{Num}, \texttt{Fractional}, and \texttt{Floating}.

The typeclasses \texttt{Functor}, \texttt{Applicative} and \texttt{Monad} are all \textbf{lifting} operations, meaning that they allow functions to be lifted or involved by the chosen type. While they differ \textbf{which} functions will be lifted, i.e., each one of them lift a function with a different type signature, they share the intuition that these functions will be interacting with the \texttt{CT} type. This perspective is crucial for a practical understanding of these patterns. A function with a certain \textbf{shape} and details will be lifted using one of those typeclasses and their respective operators.

The \texttt{Functor} typeclass, when implemented for the type of interest, let the lifting of functions to be enclosed by the \texttt{CT} type. Thus, as depicted in Figure \ref{fig:functor}, the function \texttt{a -> b} that comes as a parameter has its values surrounded by the same values wrapped with the \texttt{CT} type, i.e., the outcome is a function with the signature \texttt{CT a -> CT b}. The code below shows the implementation of the \textit{fmap} function --- the minimum requirement to the \texttt{Functor} typeclass --- to the \texttt{CT} type. It is worth noting that, because this type uses an \texttt{IO} inside, a second \textit{fmap}, this time related to \texttt{IO}, needs to be used in the implementation.

\begin{figure}[ht!]
\begin{spec}
instance Functor CT where
  fmap f (CT da) = CT $ \ps -> fmap f (da ps)
\end{spec}
\begin{center}
\includegraphics[width=1\linewidth]{MastersThesis/img/LiftedFunctor}
\end{center}
\caption{Given a parametric record \texttt{ps} and a dynamic value \texttt{da}, the \textit{fmap} functor of the \texttt{CT} type applies the former to the latter. Because the final result is wrapped inside the \texttt{IO} shell, a second \textit{fmap} is necessary.}
\label{fig:functor}
\end{figure}

The next typeclass, \texttt{Applicative}, deals with functions that are inside the \texttt{CT} type. When implemented, this algebraic operation lifts this internal function, wrapped by the type of choice, applying the \textbf{external} type to its \textbf{internal} members, thus generating again a function with the signature \texttt{CT a -> CT b}. The minimum requirements for this typeclass is the function \textit{pure}, a function responsible for wrapping any value with the \texttt{CT} wrapper, and the \texttt{<*>} operator, which does the aforementioned interaction between the internal values with the outer shell. The implementation of this typeclass is presented in the code bellow, in which the dependency \texttt{df} has the signature \texttt{CT (a -> b)} and its internal function \texttt{a -> b} is being lifted to the \texttt{CT} type. Figure \ref{fig:applicative} illustrates the described lifting with \texttt{Applicative}.

\begin{figure}[ht!]
\begin{minipage}{.55\textwidth}
\begin{spec}
instance Applicative CT where
  pure a = CT $ const (return a)
  (<*>) = appComposition
\end{spec}
\end{minipage}
\begin{minipage}{.4\textwidth}
  \centering
  \includegraphics[width=0.95\linewidth]{MastersThesis/img/Pure}
\end{minipage}
\begin{spec}
appComposition :: CT (a -> b) -> CT a -> CT b
appComposition (CT df) (CT da)
  = CT $ \ps -> df ps >>= \f -> fmap f (da ps)
\end{spec}
\begin{center}
\includegraphics[width=1\linewidth]{MastersThesis/img/LiftedApplicative}
\end{center}
\caption{With the \texttt{Applicative} typeclass, it is possible to cope with functions inside the \texttt{CT} type. Again, the \textit{fmap} from \texttt{IO} is being used in the implementation.}
\label{fig:applicative}
\end{figure}

The third and final lifting is the \texttt{Monad} typeclass. In this case, the function being lifted \textbf{generates} structure as the outcome, although its dependency is a pure value. As Figure \ref{fig:monad} portrays, a function with the signature \texttt{a -> CT b} can be lifted to the signature \texttt{CT a -> CT b} by using the \texttt{Monad} typeclass. This new operation for lifting, so-called \textit{bind}, is written below, alongside the \textit{return} function, which is the same \textit{pure} function from the \texttt{Applicative} typeclass. Together, these two functions represent the minimum requirements of the \texttt{Monad} typeclass. Figure \ref{fig:monad} illustrates the aforementioned scenario.

\begin{figure}[ht!]
\begin{minipage}{.55\textwidth}
\begin{spec}
instance Monad CT where
  return a = pure a
  m >>= k = bind k m
\end{spec}
\end{minipage}
\begin{minipage}{.4\textwidth}
  \centering
  \includegraphics[width=0.95\linewidth]{MastersThesis/img/Pure}
\end{minipage}
\begin{spec}
bind :: (a -> CT b ) -> CT a -> CT b
bind k (CT m)
  = CT $ \ps -> m ps >>= \a -> (\(CT m') -> m' ps) $ k a
\end{spec}
\begin{center}
\includegraphics[width=1\linewidth]{MastersThesis/img/LiftedMonad}
\end{center}
\caption{The $>>=$ operator used in the implementation is the \textit{bind} from the \texttt{IO} shell. This indicates that when dealing with monads within monads, it is frequent to use the implementation of the internal members.}
\label{fig:monad}
\end{figure}

Aside from lifting operations, the final typeclass related to data manipulation is the \texttt{MonadIO} typeclass. It comprises only one function, \textit{liftIO}, and its purpose is to change the structure that is wrapping the value, going from an \texttt{IO} outer shell to the monad of interest, \texttt{CT} in this case. The usefulness of this typeclass will be more clear in the next topic, section \ref{sec:integrator}. The implementation is bellow, alongside its visual representation in Figure \ref{fig:monadIO}.

\begin{figure}[ht!]
\begin{minipage}{.45\textwidth}
\begin{spec}
instance MonadIO CT where
  liftIO m = CT $ const m
\end{spec}
\end{minipage}
\begin{minipage}{.5\textwidth}
\begin{center}
  \includegraphics[width=0.95\linewidth]{MastersThesis/img/MonadIO}
\end{center}
\end{minipage}
\caption{The typeclass \texttt{MonadIO} transforms a given value wrapped in \texttt{IO} into a different monad. In this case, the parameter \texttt{m} of the function is the output of the \texttt{CT} type.}
\label{fig:monadIO}
\end{figure}

Finally, there are the typeclasses related to mathematical operations. The typeclasses \texttt{Num}, \texttt{Fractional} and \texttt{Floating} provide unary and binary numerical operations, such as arithmetic operations and trigonometric functions. However, because we want to use them with the \texttt{CT} type, their implementation involve lifting. Further, the \texttt{Functor} and \texttt{Applicative} typeclasses allow us to execute this lifting, since they are designed for this purpose. The code bellow depicts the implementation for unary and binary operations, which are used in the requirements for those typeclasses:

\begin{spec}
unaryOP :: (a -> b) -> CT a -> CT b
unaryOP = fmap

binaryOP :: (a -> b -> c) -> CT a -> CT b -> CT c
binaryOP func da db = (fmap func da) <*> db
\end{spec}

\section{GPAC Bind I: CT}

After these improvements in the \texttt{CT} type, it is possible to map some of them to FF-GPAC's concepts. As we will see shortly, the implemented numerical typeclasses, when combined with the lifting typeclasses (\texttt{Functor}, \texttt{Applicative}, \texttt{Monad}), express three out of four FF-GPAC's basic circuits presented in Figure \ref{fig:gpacBasic} in the previous chapter.

First and foremost, all FF-GPAC units receive \textit{time} as an available input to compute. The \texttt{CT} type represents continuous physical dynamics~\cite{LeeModeling}, which means that it portrays a function from time to physical output. Hence, it already has time embedded into its definition; a record with type \texttt{Parameters} is received as a dependency to obtain the final result at that moment. Furthermore, it remains to model the FF-GPAC's black boxes and the composition rules that bind them together.

The simplest unit of all, \texttt{Constant Unit}, can be achieved via the implementation of the \texttt{Applicative} and \texttt{Num} typeclasses. First, this unit needs to receive the time of simulation at that point, which is an granted by the \texttt{CT} type. Next, it needs to return a constant value $k$ for all moments in time. The \texttt{Num} given the \texttt{CT} type the option of using number representations, such as the types \texttt{Int}, \texttt{Integer}, \texttt{Float} and \texttt{Double}. Further, the \texttt{Applicative} typeclass can lift those number-related functions to the desired type by using the \textit{pure} function.

Arithmetic basic units, such as the \texttt{Adder Unit} and the \texttt{Multiplier Unit}, are being modeled by the \texttt{Functor}, \texttt{Applicative} and \texttt{Num} typeclasses. Those two units use binary operations with physical signals. As demonstrated in the previous section, the combination of numerical and lifting typeclasses let us to model such operations. Figure \ref{fig:gpacBind1} shows FF-GPAC's analog circuits alongside their \texttt{Rivika} counterparts. The forth unit and the composition rules will be mapped after describing the second main type of \texttt{Rivika}: the \texttt{Integrator} type.

\figuraBib{GPACBind1}{The ability of lifting numerical values to the \texttt{CT} type resembles three FF-GPAC analog circuits: \texttt{Constant}, \texttt{Adder} and \texttt{Multiplier}}{}{fig:gpacBind1}{width=.9\textwidth}%

\section{Exploiting Impurity}
\label{sec:integrator}

The \texttt{CT} type directly interacts with a second type that intensively explores \textbf{side effects}. The notion of a side effect correlates to changing a \textbf{state}, i.e., if you see a computer program as a state machine, an operation that goes beyond returning a value --- it has an observable interference somewhere else --- is called a side effect operation or an \textbf{impure} functionality. Examples of common use cases goes from modifying memory regions to performing input-output procedures via system-calls. The nature of purity comes from the mathematical domain, in which a function is a procedure that is deterministic, meaning that the output value is always the same if the same input is provided --- a false assumption when programming with side effects. An example of an imaginary state machine can be viewed in Figure \ref{fig:stateMachine}.

\begin{figure}[ht!]
  \begin{minipage}[c]{0.67\textwidth}
    \includegraphics[width=0.95\linewidth]{MastersThesis/img/StateMachine}
  \end{minipage}\hfill
  \begin{minipage}[c]{0.32\textwidth}
    \caption{State Machines are a common abstraction in computer science due to its easy mapping between function calls and states. Memory regions and peripherals are embedded with the idea of a state, not only pure functions. Further, side effects can even act as the trigger to move from one state to another, meaning that executing a simple function can do more than return a value. Its internal guts can significantly modify the state machine.}
\label{fig:stateMachine}
  \end{minipage}
\end{figure}

In low-level and imperative languages, such as C and Fortran, impurity is present across the program and can be easily and naturally added via \textbf{pointers} --- addresses to memory regions where values, or even other pointers, can be stored. In contrast, functional programming languages advocate to a more explicit use of such aspect, given that it prioritizes pure and mathematical functions instead of allowing the developer to mix these two facets. So, the developer has to take extra effort to add an effectful function into the program, clearly separating these two different styles of programming.

The second core type of the present work, the \texttt{Integrator}, is based on this idea of side effect operations, manipulating data directly in memory, always consulting and modifying data in the impure world. Foremost, it represents a differential equation, as explained in chapter 2, \textit{Design Philosophy} section \ref{sec:diff}, meaning that the \texttt{Integrator} type models the calculation of an \textbf{integral}. It accomplishes this task by driving the numerical algorithms of a given solver method, implying that this is where the \textit{operational} semantics of our DSL reside.

With this in mind, the \texttt{Integrator} type is responsible for executing a given solver method to calculate a given integral. This type comprises the initial value of the system, i.e., the value of a given function at time $t_0$, and a pointer to a memory region for future use, called \texttt{computation}. In Haskell, something similar to a pointer and memory allocation can be made by using the \texttt{IORef} type. This memory region is being allocated to be used with the type \texttt{CT Double}. Also, the initial value is also represented by \texttt{CT Double}, and the initial condition can be lifted to this type because the typeclass \texttt{Num} is implemented (section \ref{sec:typeclasses}). It is worth noticing that these pointers are pointing to functions or \textbf{computations} and not to double precision values.

\begin{spec}
data Integrator = Integrator { initial     :: CT Double,
                               computation :: IORef (CT Double)
                             }
\end{spec}

There are three functions that involve the \texttt{Integrator} and the \texttt{CT} types together: the function \textit{createInteg}, responsible for allocating the memory that the pointer will pointer to, \textit{readInteg}, letting us to read from the pointer, and \textit{updateInteg}, a function that alters the content of the region being pointed. In summary, these functions allow us to create, read and update data from that region, if we have the pointer on-hand. All functions related to the integrator use what's known as \texttt{do-notation}, a syntax sugar of the \texttt{Monad} typeclass for the bind operator. The code bellow is the implementation of the \textit{createInteg} function, which creates an integrator:

\begin{spec}
createInteg :: CT Double -> CT Integrator
createInteg i = do
  r1 <- liftIO . newIORef $ initialize i
  r2 <- liftIO . newIORef $ initialize i
  let integ = Integrator { initial = i,
                           cache = r1,
                           computation  = r2 }
      z = do
        ps <- ask
        v <- liftIO $ readIORef (computation integ)
        local (const ps) v
  y <- memo interpolate z
  liftIO $ writeIORef (cache integ) y
  return integ
\end{spec}

The first step to create an integrator is to manage the initial value, which is a function with the type \texttt{Parameters -> IO Double} wrapped in \texttt{CT}. After acquiring a given initial value \texttt{i}, the integrator needs to assure that any given parameter record is the beginning of the computation process, i.e., it starts from $t_0$. The \texttt{initialize} function fulfills this role, doing a reset in \texttt{time}, \texttt{iteration} and \texttt{stage} in a given parameter record. This is necessary because all the implemented solvers presumes \textbf{sequential steps}, starting from the initial condition. So, in order to not allow this error-prone behaviour, the integrator makes sure that the initial state of the system is configured correctly. The next step is to allocate memory to this computation --- a procedure that will get you the initial value, while modifying the parameter record dependency of the function accordingly.

The following stage is to do a type conversion, given that in order to create the \texttt{Integrator} record, it is necessary to have the type \texttt{IORef (CT Double)}. At first glance, this can seem to be an issue because the result of the \textit{newIORef} function is wrapped with the \texttt{IO} monad~\footnote{\label{foot:IORef} \texttt{IORef} \href{https://hackage.haskell.org/package/base-4.16.1.0/docs/Data-IORef.html}{\textcolor{blue}{hackage documentation}}.}. This conversion is the reason why the \texttt{IO} monad is being used in the implementation, and hence forced us to implement the typeclass \texttt{MonadIO}. The function \texttt{liftIO} is capable of removing the \texttt{IO} wrapper and adding an arbitrary monad in its place, \texttt{CT} in this case. So, after line 3 the \texttt{comp} value has the desired \texttt{CT} type. The remaining step of this creation process is to construct the integrator itself by building up the record with the correct fields, e.g., the dynamic version of the initial value and the pointer to the constructed computation written in memory (lines 4 and 5).

\begin{spec}
readInteg :: Integrator -> CT Double
readInteg = join . liftIO . readIORef . computation
\end{spec}

To read the content of this region, it is necessary to provide the integrator to the $readInteg$ function. Its implementation is straightforward: build a new \texttt{CT} that applies the given record of \texttt{Parameters} (line 5) to what's being stored in the region (line 4). This is accomplished by using \texttt{do-notation} with the $readIORef$ function~\footref{foot:IORef}.

Finally, the function \textit{updateInteg} is a side-effect-only function that changes \textbf{which computation} will be used by the integrator. It is worth noticing that after the creation of the integrator, the \texttt{computation} pointer is addressing a simple and, initially, useless computation: given an arbitrary record of \texttt{Parameters}, it will fix it to assure it is starting at $t_0$, and it will return the initial value in form of a \texttt{CT Double}. To update this behaviour, the \textit{updateInteg} change the content being pointed by the integrator's pointer:

\begin{spec}
updateInteg :: Integrator -> CT Double -> CT ()
updateInteg integ diff = do
  let i = initial integ
      z = do
        ps <- ask
        let f = solverToFunction (method $ solver ps)
        y <- liftIO $ readIORef (cache integ)
        f diff i y
  liftIO $ writeIORef (computation integ) z
\end{spec}

In the beginning of the function (line 3), we create a new computation, so-called \texttt{z} --- a function wrapped in the \texttt{CT} type that receives a \texttt{Parameters} record and computes the result based on the solving method. In \texttt{z}, the first step is to build a copy of the \textbf{same process} being pointed by the \texttt{computation} pointer (line 4), and get the initial condition of the system (line 5). Finally, after checking the chosen solver (line 6), it is executed one iteration of the process by calling \textit{integEuler}, or \textit{integRK2} or \textit{integRK4}. After line 10, this entire process \texttt{z} is being pointed by the \texttt{computation} pointer, being done by the $writeIORef$ function~\footref{foot:IORef}. It may seem confusing that inside \texttt{z} we are \textbf{reading} what is being pointed and later, on the last line of \textit{updateInteg}, this is being used on the final line to update that same pointer. This is necessary, as it will be explained in the next chapter \textit{Execution Walkthrough}, to allow the use of an \textbf{implicit recursion} to assure the sequential aspect needed by the solvers. For now, the core idea is this: the \textit{updateInteg} function alters the \textbf{future} computations; it rewrites which procedure will be pointed by the \texttt{computation} pointer. This new procedure, which we called \texttt{z}, creates an intermediate computation, \texttt{whatToDo} (line 4), that \textbf{reads} what this pointer is addressing, which is \texttt{z} itself.

Initially, this strange behaviour may cause the idea that this computation will never halt. However, Haskell's \textit{laziness} assures that a given computation will not be computed unless it is necessary to continue execution and this is \textbf{not} the case in the current stage, given that we are just setting the environment in the memory to further calculate the solution of the system.

\section{GPAC Bind II: Integrator}

The \texttt{Integrator} type introduced in the previous section corresponds to FF-GPAC's forth and final basic unit, the integrator. The analog version of the integrator used in FF-GPAC had the goal of using physical systems (shafts and gears) that obeys the same mathematical relations that control other physical or technical phenomenon under investigation~\cite{Graca2004}. In contrast, the integrator modeled in {Rivika} uses pointers in a digital computer that point to iteration-based algorithms that can approximate the solution of the problem at a requested moment $t$ in time.

Lastly, there are the composition rules in FF-GPAC --- constraints that describe how the units can be interconnected. The following are the same composition rules presented in chapter 2, \textit{Design Philosophy}, section \ref{sec:gpac}:

\begin{enumerate}
  \item An input of a polynomial circuit should be the input $t$ or the output of an integrator. Feedback can only be done from the output of integrators to inputs of polynomial circuits.
  \item Each polynomial circuit admit multiple inputs
  \item Each integrand input of an integrator should be generated by the output of a polynomial unit.
  \item Each variable of integration of an integrator is the input \textit{t}.
\end{enumerate}

The preceding rules include defining connections with polynomial circuits --- an acyclic circuit composed only by constant functions, adders and multipliers. These special circuits are already being modeled in \texttt{Rivika} by the \texttt{CT} type with a set of typeclasses, as explained in the previous section about GPAC. The \textbf{integrator functions}, e.g., \textit{readInteg} and \textit{updateInteg}, represent the composition rules.

Going back to the type signature of the \textit{updateInteg}, \texttt{Integrator -> CT Double -> CT ()}, we can interpret this function as a \textbf{wiring} operation. This function connects as an input of the integrator, represented by the \textbf{Integrator} type, the output of a polynomial circuit, represented by the value with \texttt{CT Double} type. Because the operation is just setting up the connections between the two, the functions ends with the type \texttt{CT ()}.

A polynomial circuit can have the time $t$ or an output of another integrator as inputs, with restricted feedback (rule 1). This rule is being matched by the following: the \texttt{CT} type makes time available to the circuits, and the \textit{readInteg} function allows us to read the output of another integrators. The second rule, related to multiple inputs in the combinational circuit, is being followed because we can link inputs using arithmetic operations, feature provided by the \texttt{Num} typeclass. Moreover, because the sole purpose of \texttt{Rivika} is to solve differential equations, we are \textbf{only} interested in circuits that calculates integrals, meaning that it is guaranteed that the integrand of the integrator will always be the output of a polynomial unit (rule 3), as we saw with the type signature of the \textit{updateInteg} function. The forth rule is also being attended it, given that the solver methods inside the \textit{updateInteg} function always calculate the integral in respect to the time variable. Figure \ref{fig:gpacBind2} summarizes these last mappings between the implementation, and FF-GPAC's integrator and rules of composition.

\figuraBib{GPACBind2}{The integrator functions attend the rules of composition of FF-GPAC, whilst the \texttt{CT} and \texttt{Integrator} types match the four basic units}{}{fig:gpacBind2}{width=.9\textwidth}%

\newpage
\newpage

\section{Using Recursion to solve Math}

The remaining topic of this chapter is to describe in detail how the solver methods are being implemented. There are three solvers currently implemented:

\begin{itemize}
\item Euler Method or First-order Runge-Kutta Method
\item Second-order Runge-Kutta Method
\item Forth-order Runge-Kutta Method
\end{itemize}

To explain how the solvers work and their nuances, it is useful to go into the implementation of the simplest one --- the Euler method. However, the implementation of the solvers use a slightly different function for the next step or iteration in comparison to the one explained in chapter 2. Hence, it is worthwhile to remember how this method originally iterates in terms of its mathematical description and compare it to the new function. From equation \ref{eq:nextStep}, we can obtain a different function to next step, by subtracting the index from both sides of the equation:

\begin{equation}
y_{n+1} = y_n + hf(t_n,y_n) \rightarrow y_n = y_{n-1} + hf(t_{n-1}, y_{n-1})
\label{eq:solverEquation}
\end{equation}

The value of the current iteration, $y_n$, can be described in terms of the sum of the previous value and the product between the time step $h$ with the differential equation from the previous iteration and time. With this difference taken into account, the following code is the implementation of the Euler method. In terms of main functionality, the family of Runge-Kutta methods is analogous:

\begin{code}
integEuler :: CT Double
           -> CT Double
           -> CT Double
           -> CT Double
integEuler diff i y = do
  ps <- ask
  case iteration ps of
    0 -> i
    n -> do
      let iv  = interval ps
          sl  = solver ps
          ty  = iterToTime iv sl (n - 1) (SolverStage 0)
          psy = ps { time = ty, iteration = n - 1, solver = sl { stage = SolverStage 0} }
      a <- local (const psy) y
      b <- local (const psy) diff
      let !v = a + dt (solver ps) * b
      return v
\end{code}

On line 5, it is possible to see which functions are available in order to execute a step in the solver. The dependency \texttt{diff} is the representation of the differential equation itself. The initial value, $y(t_0)$, can be obtained by applying any \texttt{Parameters} record to the \texttt{init} dependency function. The next dependency, \texttt{compute}, execute everything previously defined in \textit{updateInteg}; thus effectively executing a new step using the \textbf{same} solver. The result of \texttt{compute} depends on which parametric record will be applied, meaning that we call a new and different solver step in the current one, potentially building a chain of solver step calls. This mechanism --- of executing again a solver step, inside the solver itself --- is the aforementioned implicit recursion, described in the earlier section. By changing the \texttt{ps} record to the \textbf{previous} moment and iteration with the solver starting from initial stage, it is guaranteed that for any step the previous one can be computed, a requirement when using numerical methods.

With this in mind, the solver function treats the initial value case as the base case of the recursion, whilst it treats normally the remaining ones (line 9). In the base case (lines 7 and 8), the calculation can be done by doing an application of \texttt{ps} to \texttt{init}. Otherwise, it is necessary to know the result from the previous iteration in order to generate the current one. To address this requirement, the solver builds another parametric record (lines 10 to 13) and call another solver step (line 14). Also, it calculates the value from applying this record to \texttt{diff} (line 15), the differential equation, and finally computes the result for the current iteration (line 16). It is worth noting that the use of \texttt{let!} is mandatory, given that it forces evaluation of the expression instead of lazily postponing the computation, making it execute everything in order to get the value \texttt{v} (line 17).

\ignore{
\begin{code}
integRK2 :: CT Double
         -> CT Double
         -> CT Double
         -> CT Double
integRK2 f i y = do
  ps <- ask
  case stage (solver ps) of
    SolverStage 0 -> case iteration ps of
                       0 -> i
                       n -> do
                         let iv = interval ps
                             sl = solver ps
                             ty = iterToTime iv sl (n - 1) (SolverStage 0)
                             t1 = ty
                             t2 = iterToTime iv sl (n - 1) (SolverStage 1)
                             psy = ps { time = ty, iteration = n - 1, solver = sl { stage = SolverStage 0 }}
                             ps1 = psy
                             ps2 = ps { time = t2, iteration = n - 1, solver = sl { stage = SolverStage 1 }}
                         vy <- local (const psy) y
                         k1 <- local (const ps1) f
                         k2 <- local (const ps2) f
                         let !v = vy + dt sl / 2.0 * (k1 + k2)
                         return v
    SolverStage 1 -> do
                  let iv = interval ps
                      sl = solver ps
                      n  = iteration ps
                      ty = iterToTime iv sl n (SolverStage 0)
                      t1 = ty
                      psy = ps { time = ty, iteration = n, solver = sl { stage = SolverStage 0 }}
                      ps1 = psy
                  vy <- local (const psy) y
                  k1 <- local (const ps1) f
                  let !v = vy + dt sl * k1
                  return v
    _ ->
      error "Incorrect stage: integRK2"

integRK4 :: CT Double
         -> CT Double
         -> CT Double
         -> CT Double
integRK4 f i y = do
  ps <- ask
  case stage (solver ps) of
    SolverStage 0 -> case iteration ps of
                       0 -> i
                       n -> do
                         let iv = interval ps
                             sl = solver ps
                             ty = iterToTime iv sl (n - 1) (SolverStage 0)
                             t1 = ty
                             t2 = iterToTime iv sl  (n - 1) (SolverStage 1)
                             t3 = iterToTime iv sl  (n - 1) (SolverStage 2)
                             t4 = iterToTime iv sl  (n - 1) (SolverStage 3)
                             psy = ps { time = ty, iteration = n - 1, solver = sl { stage = SolverStage 0 }}
                             ps1 = psy
                             ps2 = ps { time = t2, iteration = n - 1, solver = sl { stage = SolverStage 1 }}
                             ps3 = ps { time = t3, iteration = n - 1, solver = sl { stage = SolverStage 2 }}
                             ps4 = ps { time = t4, iteration = n - 1, solver = sl { stage = SolverStage 3 }}
                         vy <- local (const psy) y
                         k1 <- local (const ps1) f
                         k2 <- local (const ps2) f
                         k3 <- local (const ps3) f
                         k4 <- local (const ps4) f
                         let !v = vy + dt sl / 6.0 * (k1 + 2.0 * k2 + 2.0 * k3 + k4)
                         return v
    SolverStage 1 -> do
                  let iv = interval ps
                      sl = solver ps
                      n  = iteration ps
                      ty = iterToTime iv sl n (SolverStage 0)
                      t1 = ty
                      psy = ps { time = ty, iteration = n, solver = sl { stage = SolverStage 0 }}
                      ps1 = psy
                  vy <- local (const psy) y
                  k1 <- local (const ps1) f
                  let !v = vy + dt sl / 2.0 * k1
                  return v
    SolverStage 2 -> do
                  let iv = interval ps
                      sl = solver ps
                      n  = iteration ps
                      ty = iterToTime iv sl n (SolverStage 0)
                      t2 = iterToTime iv sl n (SolverStage 1)
                      psy = ps { time = ty, iteration = n, solver = sl { stage = SolverStage 0 }}
                      ps2 = ps { time = t2, iteration = n, solver = sl { stage = SolverStage 1 }}
                  vy <- local (const psy) y
                  k2 <- local (const ps2) f
                  let !v = vy + dt sl / 2.0 * k2
                  return v
    SolverStage 3 -> do
                  let iv = interval ps
                      sl = solver ps
                      n  = iteration ps
                      ty = iterToTime iv sl n (SolverStage 0)
                      t3 = iterToTime iv sl n (SolverStage 2)
                      psy = ps { time = ty, iteration = n, solver = sl { stage = SolverStage 0 }}
                      ps3 = ps { time = t3, iteration = n, solver = sl { stage = SolverStage 2 }}
                  vy <- local (const psy) y
                  k3 <- local (const ps3) f
                  let !v = vy + dt sl * k3
                  return v
    _ ->
      error "Incorrect stase: integRK4"
\end{code}
}

This finishes this chapter, where we incremented the capabilities of the \texttt{CT} type and used it in combination with a brand-new type, the \texttt{Integrator}. Together these types represent the mathematical integral operation. The solver methods are involved within this implementation, and they use an implicit recursion to maintain their sequential behaviour. Also, those abstractions were mapped to FF-GPAC's ideas in order to bring some formalism to the project. However, the used mechanisms, such as implicit recursion and memory manipulation, make it hard to visualize how to execute the project given a description of a physical system. The next chapter, \textit{Execution Walkthrough}, will introduce the \textbf{driver} of the simulation and present a step-by-step concrete example.
