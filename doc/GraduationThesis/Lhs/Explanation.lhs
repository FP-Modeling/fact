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

The core type, which drives the entire simulation, is the type \texttt{Machine}; a shell for a function that takes a set of parameters and return a value wrapped inside an \texttt{IO} typeclass.

\begin{code}
newtype Dynamics a = Dynamics {apply :: Parameters -> IO a}
\end{code}

It is established that if a machine is receiving data of type \texttt{Parameters}, it is \textbf{applying} it in order to get the result. The aforementioned type is used across the whole simulation, being used at all steps of the solving process in all stages of any of the available solving methods. It carries the specifications of the simulation, the current time of that step in the solution process, its analogous in the iterations or discrete axis, and which stage of the solver it is at.

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

Finally and foremost, the \texttt{Integ'} type represents the integrator and it is responsable for the core logic of the program, saving the initial value, i.e., the value of a given function at time $t_0$, and a pointer to the memory where the \textbf{machine computation} is being stored. The latter is achieved by using the \texttt{IORef} type being the representative of a pointer, and \texttt{Dynamics Double} being the computation itself.

\begin{code}
data Integ' = Integ' { initial     :: Dynamics Double,
                       computation :: IORef (Dynamics Double)
                     }
\end{code}

This concept is crucial: when doing a computation, we are waiting for a record with type \texttt{Parameters} in order to compute the final result, and this process is being appointed by the \texttt{computation} field in the above record.

\section{Integrator Functions}



