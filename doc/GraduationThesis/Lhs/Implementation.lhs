\ignore{
\begin{code}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances, BangPatterns, ConstraintKinds, MonoLocalBinds #-}
module GraduationThesis.Lhs.Implementation where
import GraduationThesis.Lhs.Design
import Control.Monad.Trans
import Data.IORef
\end{code}
}

This chapter details the next steps to simulate continuous-time behaviours. It starts by enhancing the previously defined \texttt{Dynamics} type with typeclasses' implementation. Next, the second core type of the simulation, the \texttt{Integrator} type, will be introduced alongside its functions. At the end of the chapter, an implicit recursion will be blended with a lot of side effect operations, making the \texttt{Integrator} type hard to digest. This will be addressed by a guided Lorenz Attractor example in the next chapter, \textit{Enlightenment}.

\section{Uplifting the Dynamics Type}
\label{sec:typeclasses}

The \texttt{Dynamics} type needs \textbf{algebraic operations} to be better manipulated, i.e., useful operations that can be applied to the type preserving its external structure. These procedures are algebraic laws or properties that enhance the capabilities of the proposed function type wrapped with a \texttt{Dynamics} shell. Towards this goal a few typeclasses need to be implemented.

Across the spectrum of available typeclasses in Haskell, the ones responsible for data manipulation with a single or multiple \texttt{Dynamics} and for providing mathematical operations are the ones of interest. To address the former group of operations, the typeclasses \texttt{Functor}, \texttt{Applicative}, \texttt{Monad} and \texttt{MonadIO} need to be implemented. The later group of properties, dedicated to allow mathematical operations, can be acquired by implementing the typeclasses \texttt{Num}, \texttt{Fractional}, and \texttt{Floating}.

The \texttt{Functor} typeclass allow the type with its implementation to \textbf{apply} a pure function, i.e., a function that only involves values with no structure, \textbf{inside} of the shell, skipping the outer shell of the value. Thus, the content inside the \texttt{Dynamics} wrapper can be transformed still getting a \texttt{Dynamics} type as the outcome. Figure \ref{fig:functor} shows the implementation of the \textit{fmap} function, --- the minimum requirement to the \texttt{Functor} typeclass --- alongside its pictorial representation to provide intuition.

\begin{figure}[ht!]
\begin{code}
instance Functor Dynamics where
  fmap f (Dynamics da) = Dynamics $ \ps -> fmap f (da ps)
\end{code}
\begin{center}
\includegraphics[width=0.75\linewidth]{GraduationThesis/img/Functor}
\end{center}
\caption{Tthe \textit{fmap} function applies a pure function, \textit{f}, to the output value of a given \texttt{Dynamics}. Also, because the final value is wrapped in the \texttt{IO} monad, it is necessary to use its own \textit{fmap}, meaning that the one on the left is different from the one on the right; each \textit{fmap} corresponds to a different type.}
\label{fig:functor}
\end{figure}

The next typeclass, \texttt{Applicative}, is the first typeclass that deals with two \texttt{Dynamics}. When implemented, this algebraic operation allows us to pick a transformation contained inside the \texttt{Dynamics} and apply that operation to the second one and using that result as the output. So, a \textbf{lifted} function, i.e., it is involved with structure, can still be applied to another structured type. The minimum requirements for this typeclass is the function \textit{pure}, a function responsible for wrapping any value with the \texttt{Dynamics} wrapper, and the \texttt{<*>} operator, which does the aforementioned interaction between two values. Figure \ref{fig:applicative} shows its implementation as well as provide some intuition via a visual depiction.

\begin{figure}[ht!]
\begin{minipage}{.5\textwidth}
\begin{code}
instance Applicative Dynamics where
  pure a = Dynamics $ const (return a)
  (<*>) = appComposition
\end{code}
\end{minipage}
\begin{minipage}{.47\textwidth}
  \centering
  \includegraphics[width=0.95\linewidth]{GraduationThesis/img/Pure}
\end{minipage}
\begin{code}
appComposition :: Dynamics (a -> b) -> Dynamics a -> Dynamics b
appComposition (Dynamics df) (Dynamics da)
  = Dynamics $ \ps -> df ps >>= \f -> fmap f (da ps)
\end{code}
\begin{center}
\includegraphics[width=0.85\linewidth]{GraduationThesis/img/Applicative}
\end{center}
\caption{With the \texttt{Applicative} typeclass, it is possible to \textbf{compose} \texttt{Dynamics} types. The pure function wrapped with the type can be correctly plumbered to the second value inside the same shell type, generating the result.}
\label{fig:applicative}
\end{figure}

The \texttt{Monad} typeclass tackles the same issue as the \texttt{Applicative} does --- deal with the interaction of two \texttt{Dynamics} --- but with a different type of function to apply to the first \texttt{Dynamics} value. Instead of being a pure function wrapped within structure, the transformation function \textbf{produces} more structure, i.e., the signature of it goes from a value to the value surrounded by the type \texttt{Dynamics}. Hence, a new type that acts like a plumber needs to be taken place. This new operation, so-called \textit{bind}, when combined with the same \textit{pure} function from \texttt{Applicative}, here so-called \textit{return}, compose the requirements of the \texttt{Monad} typeclass. So, Figure \ref{fig:monad} illustrates its implementation for the \texttt{Dynamics} type in company of an visual description.


\begin{figure}[ht!]
\begin{minipage}{.5\textwidth}
\begin{code}
instance Monad Dynamics where
  return a = pure a
  m >>= k = bind m k
\end{code}
\end{minipage}
\begin{minipage}{.47\textwidth}
  \centering
  \includegraphics[width=0.95\linewidth]{GraduationThesis/img/Pure}
\end{minipage}
\begin{code}
bind :: Dynamics a -> (a -> Dynamics b) -> Dynamics b
bind (Dynamics m) k = Dynamics $ \ps -> do a <- m ps
                                           let Dynamics m' = k a
                                           m' ps
\end{code}
\begin{center}
\includegraphics[width=0.85\linewidth]{GraduationThesis/img/Monad}
\end{center}
\caption{The \texttt{Monad} typeclass does a different sort of \texttt{Dynamics} composition. With monads, it is possible to use a syntax sugar called \texttt{do} notation, which allows the developer to \textbf{unwrapp} a given type. In the implementation, this is being used to unwrapp the \texttt{IO} monad at the end of line 2 of the \textit{bind} function. After the $\leftarrow$ operator, the value \texttt{a} is not surrounded by \texttt{IO}.}
\label{fig:monad}
\end{figure}

The final typeclass related to data manipulation is the \texttt{MonadIO} typeclass. It comprises only one function, \textit{liftIO}, and its purpose it's to change the structure that is wrapping the value, going from an \texttt{IO} outer shell to the monad of interest, \texttt{Dynamics} in this case. The usefulness of this typeclass will be more clear in the next topic, section \ref{sec:integrator}. The implementation and its intuitive image can be checked in Figure \ref{fig:monadIO}.

\begin{figure}[ht!]
\begin{minipage}{.45\textwidth}
\begin{code}
instance MonadIO Dynamics where
  liftIO m = Dynamics $ const m
\end{code}
\end{minipage}
\begin{minipage}{.5\textwidth}
\begin{center}
  \includegraphics[width=0.95\linewidth]{GraduationThesis/img/MonadIO}
\end{center}
\end{minipage}
\caption{The typeclass \texttt{MonadIO} transforms a given value wrapped in \texttt{IO} into a different monad. In this case, the parameter of the function is the output of the \texttt{Dynamics} type.}
\label{fig:monadIO}
\end{figure}

Finally, there are the typeclasses related to the mathematical operations. All of them, \texttt{Num}, \texttt{Fractional}, \texttt{Floating}, will be based on the same function, \textit{liftOP}, described in the next figure, Figure \ref{fig:liftOP}. The main goal is to \textbf{lift} binary and tertiary operations to be using only \texttt{Dynamics} types, i.e., adding structure to these operations on both operands.

\begin{figure}[ht!]
\begin{minipage}{.58\textwidth}
\begin{code}
liftOP :: (a -> b) -> Dynamics a -> Dynamics b
liftOP f (Dynamics x) =
  Dynamics $ \ps -> fmap f (x ps)
\end{code}
\end{minipage}
\begin{minipage}{.41\textwidth}
\begin{center}
  \includegraphics[width=0.95\linewidth]{GraduationThesis/img/LiftOP}
\end{center}
\end{minipage}
\caption{To make binary and tertiary operations to work, it is necessary to wrapp the operands with the desired type, \texttt{Dynamics} in this case. The implementation of \textit{liftOP2}, which has the signature $(a \rightarrow b \rightarrow c) \rightarrow Dynamics\; a \rightarrow Dynamics\; b \rightarrow Dynamics\; c$, is analogous.}
\label{fig:liftOP}
\end{figure}

\ignore{
\begin{code}
instance Eq (Dynamics a) where
  _ == _ = error "Can't compare dynamics." 

instance Show (Dynamics a) where
  showsPrec _ _ = showString "<< Dynamics >>"

liftOP2 :: (a -> b -> c) -> Dynamics a -> Dynamics b -> Dynamics c
liftOP2 f (Dynamics x) (Dynamics y) =
  Dynamics $ \ps -> do { a <- x ps; b <- y ps; return $ f a b }

instance (Num a) => Num (Dynamics a) where
  x + y = liftOP2 (+) x y
  x - y = liftOP2 (-) x y
  x * y = liftOP2 (*) x y
  negate = liftOP negate
  abs = liftOP abs
  signum = liftOP signum
  fromInteger i = return $ fromInteger i

instance (Fractional a) => Fractional (Dynamics a) where
  x / y = liftOP2 (/) x y
  recip = liftOP recip
  fromRational t = return $ fromRational t

instance (Floating a) => Floating (Dynamics a) where
  pi = return pi
  exp = liftOP exp
  log = liftOP log
  sqrt = liftOP sqrt
  x ** y = liftOP2 (**) x y
  sin = liftOP sin
  cos = liftOP cos
  tan = liftOP tan
  asin = liftOP asin
  acos = liftOP acos
  atan = liftOP atan
  sinh = liftOP sinh
  cosh = liftOP cosh
  tanh = liftOP tanh
  asinh = liftOP asinh
  acosh = liftOP acosh
  atanh = liftOP atanh
\end{code}
}

This summarizes the boost in functionality for the \texttt{Dynamics} type. These operations will be necessary and useful in the next section, where \texttt{Dynamics} will be used in combination with a new core type of the simulation: the \texttt{Integrator} type.

\newpage
\newpage

\section{Exploiting Impurity}
\label{sec:integrator}

The \texttt{Dynamics} type directly interacts with a second type that intensively explores \textbf{side effects}. The notion of a side effect correlates to changing a \textbf{state}, i.e., if you see a computer program as a state machine, an operation that goes beyond returning a value, but has an observable interference somewhere else is called a side effect operation or an \textbf{impure} functionality. Examples of common use cases goes from modifying memory regions to performing input-output procedures via system-calls. The nature of purity comes from the mathematical domain, in which a function is a procedure that is determinist, meaning that the output value is always the same if the same input is provided; a false assumption when programming with side effects. An example of an imaginary state machine can be viewed in Figure \ref{fig:stateMachine}.

\begin{figure}[ht!]
  \begin{minipage}[c]{0.67\textwidth}
    \includegraphics[width=0.95\linewidth]{GraduationThesis/img/StateMachine}
  \end{minipage}\hfill
  \begin{minipage}[c]{0.32\textwidth}
    \caption{State Machines are a common abstraction in computer science due to its easy mapping between function calls and states. Memory regions and peripherals are embedded with the idea of a state, not only pure functions. Further, side effects can even act as the trigger to move from one state to another, meaning that executing a simple function can do more than return a value. Its internal guts can significantly modify the state machine.}
\label{fig:stateMachine}
  \end{minipage}
\end{figure}

In low-level and imperative languages, such as C and Fortran, impurity is present across the program and can be easily and naturally added to a program via what it's called \textbf{pointers} --- addresses to memory regions where values, or even other pointers, can be stored. In contrast, functional programming languages advocate to a more explicit use of such aspect, given that it prioritizes pure and mathematical functions instead of allowing the developer to mix these two facets. So, it makes it clear to the developer that he is sure of what he is doing and want to continue, clearly separating these two different styles of programming.

The second core type of the present work, the \texttt{Integrator}, is based on this idea of side effect operations, manipulating data directly in memory, always consulting and modifying data in the impure world. Foremost, it represents the recursive description of a differential equation, as explained in chapter 2, \textit{Design Philosophy} section \ref{sec:diff}, meaning that the \texttt{Integrator} models the calculation of an \textbf{integral}. It accomplishes this task by driving the numerical algorithms of a given solver method, implying that this is where the \textit{operational} semantics reside.

With this in mind, the \texttt{Integrator} type is for responsible executing a given solver method to calculate a given integral. This type comprises the initial value of the system, i.e., the value of a given function at time $t_0$, and a pointer to a memory region for future use, called \texttt{computation}. In Haskell, a pointer can be made by using the special monad \texttt{IORef}. This memory region is being allocated to be used with the type \texttt{Dynamics Double}. Also, the initial value is also represented by \texttt{Dynamics Double}, and a number can be directly lifted to this type because the typeclass \texttt{Num} is implemented (section \ref{sec:typeclasses}).

\begin{spec}
data Integrator = Integrator { initial     :: Dynamics Double,
                               computation :: IORef (Dynamics Double)
                             }
\end{spec}

There are three functions that involve the \texttt{Integrator} and the \texttt{Dynamics} types together: the function \textit{newInteg}, responsible for allocating the memory that the pointer will pointer to, \textit{readInteg}, allowing us to read from the pointer, and \textit{diffInteg}, a function that alters the content of the region being pointed. In summary, these functions allow us to create, read and update data from that region, if we have the pointer on-hand.

Following the example presented in the \textit{Introduction}, the first step in the chain is to allocate memory for the integrator, via the following \textit{newInteg} function:

\begin{spec}
newInteg :: Dynamics Double -> Dynamics Integrator
newInteg i = 
  do comp <- liftIO $ newIORef $ initialize i 
     let integ = Integrator { initial     = i, 
                              computation = comp }
     return integ
\end{spec}

The first step to create an integrator is to manage the initial value, which is a function with the type \texttt{Parameters -> IO Double} wrapped in \texttt{Dynamics}. After acquiring a given initial value \texttt{i}, the integrator needs to assure that any given parameter record is the beginning of the computation process, i.e., it starts from $t_0$. The \texttt{initialize} function fulfills this role, doing a reset in \texttt{time}, \texttt{iteration} and \texttt{stage} in a given parameter record. This is necessary because all the implemented solvers presumes \textbf{sequential steps}. So, in order to not allow this error-prone behaviour, the integrator makes sure that the initial state of the system is configured correctly. The next step is to allocate memory to this value, a procedure that will get you the initial value, while modifying the parameter record dependency of the function accordingly. This idea is important: what's being stored in this memory region is a \textbf{function} or a computation, and not data or values.

The following stage is to do a type conversion, given that in order to create the \texttt{Integrator} record, it is necessary to have the type \texttt{IORef (Dynamics Double)}. At first glance, this can seem to be an issue because the result of the \texttt{newIORef} is wrapped with the \texttt{IO} monad~\footnote{\label{foot:IORef}Check the \href{https://hackage.haskell.org/package/base-4.16.1.0/docs/Data-IORef.html}{documentation} of the \texttt{IORef} type.}. This conversion is the reason why the \texttt{IO} monad is being used in the implementation, and hence forced us to implement the typeclass \texttt{MonadIO} in the previous section. The function \texttt{liftIO} is capable of removing the \texttt{IO} wrapper and adding an arbitrary monad in its place, \texttt{Dynamics} in this case. After the first arrow in the \texttt{do-notation} block (given from the \texttt{Monad} typeclass), \texttt{comp} has the desired \texttt{Dynamics} type. The remaining step of this creation step is to construct the integrator itself by building up the record with the correct fields, e.g., the dynamic version of the initial value and the pointer to the constructed computation written in memory.

To read the content of this region, it is necessary to provide the integrator to the $readInteg$ function. Its implementation is straightforward: build a new \texttt{Dynamics} that applies the given record of \texttt{Parameters} to what's being stored in the region. This is accomplished by using \texttt{do-notation} with the $readIORef$ function~\footref{foot:IORef}.

\begin{spec}
readInteg :: Integrator -> Dynamics Double
readInteg integ = 
  Dynamics $ \ps ->
  do (Dynamics m) <- readIORef (computation integ)
     m ps
\end{spec}

Finally, the function \textit{diffInteg} is a side effect-only function that changes \textbf{which computation} will be used by the integrator. It is worth noticing that after the creation of the integrator, the \texttt{computation} pointer is addressing a simple and, initially, useless computation: given a random record of \texttt{Parameters}, it will fix it to assure it is starting at $t_0$, and will return the initial value in form of a \texttt{Dynamics Double}. To update this behaviour, the \textit{diffInteg} change the content being pointed by the integrator's pointer:

\begin{code}
diffInteg :: Integrator -> Dynamics Double -> Dynamics ()
diffInteg integ diff =
  do let z = Dynamics $ \ps ->
           do whatToDo <- readIORef (computation integ)
              let i = initial integ
              case method (solver ps) of
                Euler -> integEuler diff i whatToDo ps
                RungeKutta2 -> integRK2 diff i whatToDo ps
                RungeKutta4 -> integRK4 diff i whatToDo ps
     liftIO $ writeIORef (computation integ) z     
\end{code}

In the beginning of the function (line 3), it is being created a new computation, so-called \texttt{z} --- a function wrapped in the \texttt{Dynamics} type that receives a \texttt{Parameters} record and computes the result based on the solving method. In \texttt{z}, the first step is to build a copy of the \textbf{same process} being pointed by \texttt{computation} and getting the initial condition of the system (line 5). Finally, after checking the chosen solver, it is executed one iteration of the process by calling \textit{integEuler}, or \textit{integRK2} or \textit{integRK4}. After line 10, this entire process \texttt{z} is being pointed by the \texttt{computation} pointer, being done by the $writeIORef$ function~\footref{foot:IORef}. It may seem confusing that inside \texttt{z} we are \textbf{reading} what is being pointed and later, on the last line of \textit{diffInteg}, this is being used on the final line to update that same pointer. This is necessary, as it will be explained in the next chapter \textit{Enlightenment}, to allow the use of an \textbf{implicit recursion} to assure the sequential aspect needed by the solvers. For now, the core idea is this: the \textit{diffInteg} function alters the \textbf{future} computations; it rewrites which procedure will be pointed by the \texttt{computation} pointer. This new procedure, which we called \texttt{z}, creates an intermediate computation, \texttt{whatToDo} (line 4), that \textbf{reads} what this pointer is addressing, which is \texttt{z} itself.

Initially, this strange behaviour may cause the idea that this computation will never halt. However, Haskell's \textit{laziness} assures that a given computation will not be computed unless it is necessary to continue execution and this is \textbf{not} the case in the current stage, given that we are just setting the environment in the memory to further calculate the solution of the system. Figure \ref{fig:integDiff} illustrates this idea:

\figuraBib{integDiff}{Illustration of the current state of the integrator's pointer after the \textit{diffInteg} function. What's being pointed involves the same pointer again recursively.}{}{fig:integDiff}{width=.85\textwidth}%

\section{Using Recursion to solve Math}

The remaining topic of this chapter is to describe in detail how the solver methods are being implemented. There are three solvers, which use numerical methods, currently implemented:

\begin{itemize}
\item Euler Method or First-order Runge-Kutta Method
\item Second-order Runge-Kutta Method
\item Forth-order Runge-Kutta Method
\end{itemize}

To explain how the solvers work and their nuances, it is useful to go into the implementation of the simplest one --- the Euler method. However, the implementation of the solvers use a slightly different function for the next step or iteration in the algorithm. Hence, it is worthwhile to remember how this method originally iterates in terms of its mathematical description and compare it to the new function. From equation \ref{eq:nextStep} in chapter \textit{Design}, we can obtain a different function to next step, by subtracting the index from both sides of the equation:

\begin{equation}
y_{n+1} = y_n + hf(t_n,y_n) \rightarrow y_n = y_{n-1} + hf(t_{n-1}, y_{n-1})
\label{eq:solverEquation}
\end{equation}

The value of the current iteration, $y_n$, can be described in terms of the sum of the previous value and the product between the time step $h$ with the differential equation from the previous iteration and time. With this difference taken into account, the following code is the implementation of the Euler method. In terms of main functionality, the family of Runge-Kutta methods are analogous:

\begin{code}
integEuler :: Dynamics Double
             -> Dynamics Double 
             -> Dynamics Double 
             -> Parameters -> IO Double
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
\end{code}

On line 5, it is possible to see which functions are available in order to execute a step in the solver. The function \texttt{diff} is the representation of the differential equation itself. The initial value, i.e., $y(t_0)$, can be obtained by applying any \texttt{Parameters} record to the \texttt{init} function. The next function, \texttt{compute}, execute everything previously defined in \textit{diffInteg}; thus effectively executing a new step using the solver \textbf{again}. The main difference is which record we will apply in that computation, meaning that we call a new and different solver step in the current one. This mechanism --- of executing again a solver step, inside the solver itself --- is the aforementioned implicit recursion, described in the earlier section. By changing the record to the \textbf{previous} moment and iteration, it is guaranteed that for any step the previous one can be computed; a requirement when using numerical methods.

With this in mind, the solver function treats the initial value case as the base case of the recursion, whilst it treats normally the remaining ones (line 6). In the base case, the calculation can be done by doing an application of \texttt{params} to \texttt{init}. Otherwise, it is necessary to know the result from the previous iteration in order to generate the current one. To address this requirement, the solver builds another parameters record (line 10 to 13) and call another application of the solver with it (line 14). Also, it calculates the value from applying this record to \texttt{diff} (line 15), the differential equation, and finally computes the result for the current iteration (line 16). It is worth noting that the use of \texttt{let!} is mandatory, given that this overlap Haskell's laziness that is on by standard, making it execute everything in order to get the value \texttt{v} (line 17).

\ignore{
\begin{code}
integRK2 :: Dynamics Double
           -> Dynamics Double
           -> Dynamics Double
           -> Parameters -> IO Double
integRK2 (Dynamics f) (Dynamics i) (Dynamics y) ps =
  case stage (solver ps) of
    0 -> case iteration ps of
      0 ->
        i ps
      n -> do
        let iv = interval ps
            sl = solver ps
            ty = iterToTime iv sl (n - 1) 0
            t1 = ty
            t2 = iterToTime iv sl (n - 1) 1
            psy = ps { time = ty, iteration = n - 1, solver = sl { stage = 0 }}
            ps1 = psy
            ps2 = ps { time = t2, iteration = n - 1, solver = sl { stage = 1 }}
        vy <- y psy
        k1 <- f ps1
        k2 <- f ps2
        let !v = vy + dt sl / 2.0 * (k1 + k2)
        return v
    1 -> do
      let iv = interval ps
          sl = solver ps
          n  = iteration ps
          ty = iterToTime iv sl n 0
          t1 = ty
          psy = ps { time = ty, iteration = n, solver = sl { stage = 0 }}
          ps1 = psy
      vy <- y psy
      k1 <- f ps1
      let !v = vy + dt sl * k1
      return v
    _ -> 
      error "Incorrect stage: integRK2"

integRK4 :: Dynamics Double
           -> Dynamics Double
           -> Dynamics Double
           -> Parameters -> IO Double
integRK4 (Dynamics f) (Dynamics i) (Dynamics y) ps =
  case stage (solver ps) of
    0 -> case iteration ps of
      0 -> 
        i ps
      n -> do
        let iv = interval ps
            sl = solver ps
            ty = iterToTime iv sl (n - 1) 0
            t1 = ty
            t2 = iterToTime iv sl  (n - 1) 1
            t3 = iterToTime iv sl  (n - 1) 2
            t4 = iterToTime iv sl  (n - 1) 3
            psy = ps { time = ty, iteration = n - 1, solver = sl { stage = 0 }}
            ps1 = psy
            ps2 = ps { time = t2, iteration = n - 1, solver = sl { stage = 1 }}
            ps3 = ps { time = t3, iteration = n - 1, solver = sl { stage = 2 }}
            ps4 = ps { time = t4, iteration = n - 1, solver = sl { stage = 3 }}
        vy <- y psy
        k1 <- f ps1
        k2 <- f ps2
        k3 <- f ps3
        k4 <- f ps4
        let !v = vy + dt sl / 6.0 * (k1 + 2.0 * k2 + 2.0 * k3 + k4)
        return v
    1 -> do
      let iv = interval ps
          sl = solver ps
          n  = iteration ps
          ty = iterToTime iv sl n 0
          t1 = ty
          psy = ps { time = ty, iteration = n, solver = sl { stage = 0 }}
          ps1 = psy
      vy <- y psy
      k1 <- f ps1
      let !v = vy + dt sl / 2.0 * k1
      return v
    2 -> do
      let iv = interval ps
          sl = solver ps
          n  = iteration ps
          ty = iterToTime iv sl n 0
          t2 = iterToTime iv sl n 1
          psy = ps { time = ty, iteration = n, solver = sl { stage = 0 }}
          ps2 = ps { time = t2, iteration = n, solver = sl { stage = 1 }}
      vy <- y psy
      k2 <- f ps2
      let !v = vy + dt sl / 2.0 * k2
      return v
    3 -> do
      let iv = interval ps
          sl = solver ps
          n  = iteration ps
          ty = iterToTime iv sl n 0
          t3 = iterToTime iv sl n 2
          psy = ps { time = ty, iteration = n, solver = sl { stage = 0 }}
          ps3 = ps { time = t3, iteration = n, solver = sl { stage = 2 }}
      vy <- y psy
      k3 <- f ps3
      let !v = vy + dt sl * k3
      return v
    _ -> 
      error "Incorrect stage: integRK4"
\end{code}
}

This finishes this chapter, where we incremented the capabilities of the \texttt{Dynamics} type and used this enhanced version interacted to a brand-new type, \texttt{Integrator}. The combination of the two allowed the creation of a set of functions that together represents the mathematical integral operation. The solver methods are involved within this implementation, and they used an implicit recursion to maintain their sequential behaviour. When combined with side effects, the implicit recursion makes it hard to grasp an intuitive understanding. Further, it is still not clear how to use the presented abstractions to \textbf{execute} an use case. The next chapter, \textit{Enlightenment}, will be addressed both of those problems by introducing the \textbf{driver} of the simulation and by explaining a step-by-step a Lorenz Attractor example.
