\ignore{
\begin{code}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances, BangPatterns, ConstraintKinds, MonoLocalBinds #-}
module GraduationThesis.Lhs.Implementation where
import Control.Monad.Trans
import Data.IORef
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
  _ == _ = error "Can't compare dynamics." 

instance Show (Dynamics a) where
  showsPrec _ _ = showString "<< Dynamics >>"

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

\begin{code}
data Integ = Integ { initial     :: Dynamics Double,
                     computation :: IORef (Dynamics Double)
                   }
\end{code}

This concept is crucial: when doing a computation, we are waiting for a record with type \texttt{Parameters} in order to compute the final result, and this process is being appointed by the \texttt{computation} field in the above record.

\section{Integrator Functions}

Some functions, so-called \textit{integrator functions}, changes the behaviour of the integrator, represented by the type \texttt{Integ}. These functions use side-effects in their functionality, interacting with memory directly. There are three central functions for the integrator:

\begin{itemize}
\item \texttt{newInteg}
\item \texttt{integValue}
\item \texttt{integDiff}
\end{itemize}

The first step in the chain, following the example presented in chapter \ref{Introduction}, is to allocate memory for the integrator. And this is the role of the \texttt{newInteg} function.

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

\begin{code}
newInteg :: Dynamics Double -> Dynamics Integ
newInteg i = 
  do comp <- liftIO $ newIORef $ initialize i 
     let integ = Integ { initial     = i, 
                         computation = comp }
     return integ
\end{code}

The first step to create an integrator is to manage the initial value, which is is a function with the type \texttt{Parameters -> IO Double}. After acquiring a given initial value \texttt{i}, the integrator needs to assure that any given parameter record is the beginning of the computation process, i.e., it starts from $t_0$. This is necessary because all of the implemented solvers presumes \textbf{sequential steps}. So, in order to not allow this error prone behaviour, the integrator makes sure that the initial dependecy is configured correctly.

Thus, the \texttt{initialize} function fullfils this role, doing a reset in \texttt{time}, \texttt{iteration} and \texttt{stage} in a given paremeter record. This procedure is then allocated into memory, i.e, writing on memory a procedure or function that will get you the initial value, while modifying the parameter record dependency of the function.

The final step of this main phase is to do a type conversion, given that in order to compose the \texttt{Integ} datatype it is necessary to have the type \texttt{IORef (Dynamics Double)}, but the result of the \texttt{newIORef} is wrapped with the \texttt{IO} monad. This conversion is the reason why the \texttt{IO} monad is being used in the implementantion. The type class \texttt{MonadIO}, by using the function \texttt{liftIO} is capable of removing the \texttt{IO} wrapper and adding an arbitrary monad in its place:

\begin{spec}
liftIO :: Monad m => IO a -> m a
\end{spec}

With this approach, after the first arrow in the \texttt{do-notation} block, \texttt{comp} has the desired type. The remaining step of this function is to construct the integrator itself by building up the record with the correct fields, e.g., the dynamic version of the initial value and the pointer to the constructed computation written in memory.

In order to use the solver to calculate a differential equation, it is necessary to read from the integrator the calculated answer. The function \texttt{integValue} reads from the \texttt{computation} pointer the procedure and applies a given record of parameters:

\begin{code}
integValue :: Integ -> Dynamics Double
integValue integ = 
  Dynamics $ \ps ->
  do (Dynamics m) <- readIORef (computation integ)
     m ps
\end{code}

Finally, \texttt{integDiff} is a side-effect-only function that changes \textbf{which computation} will be used by the integrator. It is worth noticing that after the creation of the integrator, the \texttt{computation} pointer is addressing a simple and, at first glance, useless computation: given a random record of parameters, it will fix it to assure it is starting at $t_0$, and will return the initial value in form of a \texttt{Dynamics Double}.

To update this behaviour, the \texttt{integDiff} function change the content being pointed by the integrator:

\begin{code}
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
\end{code}

In the beginning of the function (line 3), it is being created a new computation, so-called \texttt{z} --- a function wrapped in the \texttt{Dynamics} type that receives a \texttt{Parameters} record and computes the result based on the solving method. In \texttt{z}, the first step is to build a copy of \textbf{this same process} being pointed by \texttt{computation} and getting the initial condition of the system. Finally, after checking the chosen solver, it is executed one iteration of the process. This is the entire process being pointed by the \texttt{computation} pointer after line 10. It is important to understand why we are, inside \texttt{z}, making a copy of the current process --- reading what is being pointed --- and later, on the last line of \texttt{integDiff}, this is being used on the final line.

The \texttt{integDiff} function alters the \textbf{future} computations; it rewrites which procedure will be pointed by \texttt{computation} --- as well as executed in future steps. This new procedure, which we called \texttt{z}, creates an intermediate computation, \texttt{whatToDo}, that \textbf{reads} what this pointer is addressing, which is \texttt{z} itself. Thus, this function is adding an \textbf{implicit} recursion to the process, and it will be explained later that this assures the sequential aspect of the solving methods.

Initially, it may seem that this is computation will never halt. However, Haskell's \textit{lazyness} assures that a given computation will not be computed unless it is necessary to continue and in the current stage, \textbf{it is not} the case.

\figuraBib{integDiff}{Illustration of the \texttt{integDiff} function}{}{fig:integDiff}{width=.85\textwidth}%

\section{Solvers}

Here I'll do a brief explanation of the Euler method, so I can explain in more concrete terms where the implicit recursion is happening.

\begin{code}
integEuler :: Dynamics Double
             -> Dynamics Double 
             -> Dynamics Double 
             -> Parameters -> IO Double
integEuler (Dynamics f) (Dynamics i) (Dynamics y) ps =
  case iteration ps of
    0 -> 
      i ps
    n -> do 
      let sc  = specs ps
          ty  = iterToTime sc (n - 1) 0
          psy = ps { time = ty, iteration = n - 1, stage = 0 }
      a <- y psy
      b <- f psy
      let !v = a + dt (specs ps) * b
      return v
\end{code}

\ignore{
\begin{code}
integRK2 :: Dynamics Double
           -> Dynamics Double
           -> Dynamics Double
           -> Parameters -> IO Double
integRK2 (Dynamics f) (Dynamics i) (Dynamics y) ps =
  case stage ps of
    0 -> case iteration ps of
      0 ->
        i ps
      n -> do
        let sc = specs ps
            ty = iterToTime sc (n - 1) 0
            t2 = iterToTime sc (n - 1) 1
            psy = ps { time = ty, iteration = n - 1, stage = 0 }
            ps1 = psy
            ps2 = ps { time = t2, iteration = n - 1, stage = 1 }
        vy <- y psy
        k1 <- f ps1
        k2 <- f ps2
        let !v = vy + dt sc / 2.0 * (k1 + k2)
        return v
    1 -> do
      let sc = specs ps
          n  = iteration ps
          ty = iterToTime sc n 0
          psy = ps { time = ty, iteration = n, stage = 0 }
          ps1 = psy
      vy <- y psy
      k1 <- f ps1
      let !v = vy + dt sc * k1
      return v
    _ -> 
      error "Incorrect stase: integ"

integRK4 :: Dynamics Double
           -> Dynamics Double
           -> Dynamics Double
           -> Parameters -> IO Double
integRK4 (Dynamics f) (Dynamics i) (Dynamics y) ps =
  case stage ps of
    0 -> case iteration ps of
      0 -> 
        i ps
      n -> do
        let sc = specs ps
            ty = iterToTime sc (n - 1) 0
            t2 = iterToTime sc (n - 1) 1
            t3 = iterToTime sc (n - 1) 2
            t4 = iterToTime sc (n - 1) 3
            psy = ps { time = ty, iteration = n - 1, stage = 0 }
            ps1 = psy
            ps2 = ps { time = t2, iteration = n - 1, stage = 1 }
            ps3 = ps { time = t3, iteration = n - 1, stage = 2 }
            ps4 = ps { time = t4, iteration = n - 1, stage = 3 }
        vy <- y psy
        k1 <- f ps1
        k2 <- f ps2
        k3 <- f ps3
        k4 <- f ps4
        let !v = vy + dt sc / 6.0 * (k1 + 2.0 * k2 + 2.0 * k3 + k4)
        return v
    1 -> do
      let sc = specs ps
          n  = iteration ps
          ty = iterToTime sc n 0
          psy = ps { time = ty, iteration = n, stage = 0 }
          ps1 = psy
      vy <- y psy
      k1 <- f ps1
      let !v = vy + dt sc / 2.0 * k1
      return v
    2 -> do
      let sc = specs ps
          n  = iteration ps
          ty = iterToTime sc n 0
          t2 = iterToTime sc n 1
          psy = ps { time = ty, iteration = n, stage = 0 }
          ps2 = ps { time = t2, iteration = n, stage = 1 }
      vy <- y psy
      k2 <- f ps2
      let !v = vy + dt sc / 2.0 * k2
      return v
    3 -> do
      let sc = specs ps
          n  = iteration ps
          ty = iterToTime sc n 0
          t3 = iterToTime sc n 2
          psy = ps { time = ty, iteration = n, stage = 0 }
          ps3 = ps { time = t3, iteration = n, stage = 2 }
      vy <- y psy
      k3 <- f ps3
      let !v = vy + dt sc * k3
      return v
    _ -> 
      error "Incorrect stase: integ"
\end{code}
}

\section{Model}

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

\section{The Driver}

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

