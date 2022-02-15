\ignore{
\begin{code}
module GraduationThesis.Lhs.Explanation where
\end{code}
}

The code presented in chapter \ref{Introduction} demonstrates the developed Domain-specific language (DSL), simulating a Lorenz system. The data structures that represent the continuous behaviour, the environment of the simulation itself, and the integrator were modeled using Haskell's type system. Recursion and caching are the main tools used by the chosen solver of ordinary differential equations. Finally, a simple driver executes the simulation, calling the solver to execute the defined set of equations, given specifications of the simulation, such as initial time, when to stop, and the size of the time step being used.

\section{Types}

In regard to the simulation itself, its environment is modeled by two types: \texttt{Specs} and \texttt{Method}. The latter describes which solving method will be used in the simulation, while the former adds to that the time interval and the size of the time step. All numeric information is being represented with floating point numbers to provide more accuracy to the simulations.

\begin{code}
data Specs = Specs { startTime :: Double,
                     stopTime  :: Double,
                     dt        :: Double,
                     method    :: Method      
                   } deriving (Eq, Ord, Show)


data Method = Euler         
            | RungeKutta2   
            | RungeKutta4   
            deriving (Eq, Ord, Show)
\end{code}

The core type, which drives the entire simulation, is the type \texttt{Dynamics}; a shell for a function that takes a set of parameters and return a value wrapped inside an \texttt{IO} typeclass, i.e., it maps the set of \texttt{Parameters} to the set of values $a$ inside the \texttt{IO} type. The use of \texttt{IO} is justified by the intense use of \textbf{side-effects} througout the implementation, as we will see later.

\begin{code}
newtype Dynamics a = Dynamics {apply :: Parameters -> IO a}
\end{code}

\figuraBib{Dynamics}{Illustration of the \texttt{Dynamics} type}{}{fig:Dynamics}{width=.55\textwidth}%

It is established that if this function is receiving data of type \texttt{Parameters}, it is \textbf{applying} it in order to get the result. The aforementioned type is used across the whole simulation, being used at all steps of the solving process in all stages of any of the available solving methods. It carries the specifications of the simulation, the current time of that step in the solution process, its analogous in the iterations or discrete axis, and which stage of the solver it is at.

\begin{code}
data Parameters = Parameters { specs     :: Specs,
                               time      :: Double,
                               iteration :: Int,
                               stage     :: Int
                             } deriving (Eq, Show)
\end{code}

To extend the functionality of this type, the following typeclasses are also implemented:

\begin{itemize}
   \item \texttt{Eq}
   \item \texttt{Show}
   \item \texttt{Num}
   \item \texttt{Fractional}
   \item \texttt{Floating}
   \item \texttt{Functor}
   \item \texttt{Applicative}
   \item \texttt{Monad}
   \item \texttt{MonadIO}
\end{itemize}

These typeclasses take care from simple details, like allowing arithmetic operations and use of floating point values, to more complicated aspects, such as granting the use of \texttt{do-notation}, an essential tool to have during implementation. The last typeclass \texttt{MonadIO} is necessary, given that the project explores the \texttt{IO} monad to execute side effects, as we will see later.

\ignore{
\begin{code}

instance Functor Dynamics where
  fmap f (Dynamics da) = Dynamics $ \ps -> fmap f (da ps)

instance Applicative Dynamics where
  pure = returnD
  (Dynamics df) <*> (Dynamics da) = Dynamics $ \ps -> flip fmap (da ps) =<< df ps
  
instance Monad Dynamics where
  return  = returnD
  m >>= k = bindD m k

returnD :: a -> Dynamics a
returnD a = Dynamics $ const (return a)

bindD :: Dynamics a -> (a -> Dynamics b) -> Dynamics b
bindD (Dynamics m) k = 
  Dynamics $ \ps -> 
  do a <- m ps
     let Dynamics m' = k a
     m' ps

instance MonadIO Dynamics where
  liftIO m = Dynamics $ const m

instance Eq (Dynamics a) where
  x == y = error "Can't compare dynamics." 

instance Show (Dynamics a) where
  showsPrec _ x = showString "<< Dynamics >>"

liftMD :: (a -> b) -> Dynamics a -> Dynamics b
liftMD f (Dynamics x) =
  Dynamics $ \ps -> do { a <- x ps; return $ f a }

liftM2D :: (a -> b -> c) -> Dynamics a -> Dynamics b -> Dynamics c
liftM2D f (Dynamics x) (Dynamics y) =
  Dynamics $ \ps -> do { a <- x ps; b <- y ps; return $ f a b }

instance (Num a) => Num (Dynamics a) where
  x + y = liftM2D (+) x y
  x - y = liftM2D (-) x y
  x * y = liftM2D (*) x y
  negate = liftMD negate
  abs = liftMD abs
  signum = liftMD signum
  fromInteger i = return $ fromInteger i

instance (Fractional a) => Fractional (Dynamics a) where
  x / y = liftM2D (/) x y
  recip = liftMD recip
  fromRational t = return $ fromRational t

instance (Floating a) => Floating (Dynamics a) where
  pi = return pi
  exp = liftMD exp
  log = liftMD log
  sqrt = liftMD sqrt
  x ** y = liftM2D (**) x y
  sin = liftMD sin
  cos = liftMD cos
  tan = liftMD tan
  asin = liftMD asin
  acos = liftMD acos
  atan = liftMD atan
  sinh = liftMD sinh
  cosh = liftMD cosh
  tanh = liftMD tanh
  asinh = liftMD asinh
  acosh = liftMD acosh
  atanh = liftMD atanh
\end{code}
}

Finally and foremost, the \texttt{Integ} type represents the integrator and it is responsable for the core logic of the program, saving the initial value, i.e., the value of a given function at time $t_0$, and a pointer to the memory where the \textbf{dynamic computation} is being stored. The latter is achieved by using the \texttt{IORef} type being the representative of a pointer, and \texttt{Dynamics Double} being the computation itself.

\begin{spec}
data Integ = Integ { initial     :: Dynamics Double,
                     computation :: IORef (Dynamics Double)
                   }
\end{spec}

This concept is crucial: when doing a computation, we are waiting for a record with type \texttt{Parameters} in order to compute the final result, and this process is being appointed by the \texttt{computation} field in the above record.

\section{Integrator Functions}

Some functions, so-called \textit{integrator functions}, changes the behaviour of the integrator, represented by the type \texttt{Integ}. These functions use side-effects in their functionality, interacting with memory directly. There are three central functions for the integrator:

\begin{itemize}
\item \texttt{newInteg}
\item \texttt{integValue}
\item \texttt{integDiff}
\end{itemize}

The first step in the chain, following the example presented in section \ref{Introduction}, is to allocate memory for the integrator. And this is the role of the \texttt{newInteg} function.

\ignore{
\begin{code}
initialize :: Dynamics a -> Dynamics a
initialize (Dynamics m) =
  Dynamics $ \ps ->
  if iteration ps == 0 && stage ps == 0 then
    m ps
  else
    let sc = specs ps
    in m $ ps { time = iterToTime sc 0 0,
                iteration = 0,
                stage = 0 } 
\end{code}
}

\begin{spec}
newInteg :: Dynamics Double -> Dynamics Integ
newInteg i = 
  do comp <- liftIO $ newIORef $ initialize i 
     let integ = Integ { initial     = i, 
                         computation = comp }
     return integ
\end{spec}

The first step to create an integrator is to manage the initial value, which is is a function with the type \texttt{Parameters -> IO Double}. After acquiring a given initial value \texttt{i}, the integrator needs to assure that any given parameter record is the beginning of the computation process, i.e., it starts from $t_0$. This is necessary because all of the implemented solvers presumes \textbf{sequential steps}. So, in order to not allow this error prone behaviour, the integrator makes sure that the initial dependecy is configured correctly.

Thus, the \texttt{initialize} function fullfils this role, doing a reset in \texttt{time}, \texttt{iteration} and \texttt{stage} in a given paremeter record. This procedure is then allocated into memory, i.e, writing on memory a procedure or function that will get you the initial value, while modifying the parameter record dependency of the function.

The final step of this main phase is to do a type conversion, given that in order to compose the \texttt{Integ} datatype it is necessary to have the type \texttt{IORef (Dynamics Double)}, but the result of the \texttt{newIORef} is wrapped with the \texttt{IO} monad. This conversion is the reason why the \texttt{IO} monad is being used in the implementantion. The type class \texttt{MonadIO}, by using the function \texttt{liftIO} is capable of removing the \texttt{IO} wrapper and adding an arbitrary monad in its place:

\begin{spec}
liftIO :: Monad m => IO a -> m a
\end{spec}

With this approach, after the first arrow in the \texttt{do-notation} block, \texttt{comp} has the desired type. The remaining step of this function is to construct the integrator itself by building up the record with the correct fields, e.g., the dynamic version of the initial value and the pointer to the constructed computation written in memory.

In order to use the solver to calculate a differential equation, it is necessary to read from the integrator the calculated answer. The function \texttt{integValue} reads from the \texttt{computation} pointer the procedure and applies a given record of parameters:

\begin{spec}
integValue :: Integ -> Dynamics Double
integValue integ = 
  Dynamics $ \ps ->
  do (Dynamics m) <- readIORef (computation integ)
     m ps
\end{spec}

Finally, \texttt{integDiff} is a side-effect-only function that changes \textbf{which computation} will be used by the integrator. It is worth noticing that after the creation of the integrator, the \texttt{computation} pointer is addressing a simple and, at first glance, useless computation: given a random record of parameters, it will fix it to assure it is starting at $t_0$, and will return the initial value in form of a \texttt{Dynamics Double}.

To update this behaviour, the \texttt{integDiff} function change the content being pointed by the integrator:

\begin{spec}
integDiff :: Integ -> Dynamics Double -> Dynamics ()
integDiff integ diff =
  do let z = Dynamics $ \ps ->
           do whatToDo <- readIORef (computation integ)
              let i = initial integ
              case method (specs ps) of
                Euler -> integEuler diff i whatToDo ps
                RungeKutta2 -> integRK2 diff i whatToDo ps
                RungeKutta4 -> integRK4 diff i whatToDo ps
     liftIO $ writeIORef (computation integ) z
\end{spec}

In the beginning of the function (line 3), it is being created a new computation, so-called \texttt{z} - a function wrapped in the \texttt{Dynamics} type that receives a \texttt{Parameters} record and computes the result based on the solving method. In \texttt{z}, the first step is to build a copy of \textbf{this same process} being pointed by \texttt{computation} and getting the initial condition of the system. Finally, after checking the chosen solver, it is executed one iteration of the process. This is the entire process being pointed by the \texttt{computation} pointer after line 10. It is important to understand why we are, inside \texttt{z}, making a copy of the current process - reading what is being pointed - and later, on the last line of \texttt{integDiff}, this is being used on the final line.

The \texttt{integDiff} function alters the \textbf{future} computations; it rewrites which procedure will be pointed by \texttt{computation} - as well as executed in future steps. This new procedure, which we called \texttt{z}, creates an intermediate computation, \texttt{whatToDo}, that \textbf{reads} what this pointer is addressing, which is \texttt{z} itself. Thus, this function is adding an \textbf{implicit} recursion to the process, and it will be explained later that this assures the sequential aspect of the solving methods.

Initially, it may seem that this is computation will never halt. However, Haskell's \textit{lazyness} assures that a given computation will not be computed unless it is necessary to continue and in the current stage, \textbf{it is not} the case.

\figuraBib{integDiff}{Illustration of the \texttt{integDiff} function}{}{fig:integDiff}{width=.85\textwidth}%
