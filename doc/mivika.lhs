\documentclass{article}

%include colorcode.fmt

\usepackage{minted}
\newminted[code]{haskell}{}

\usepackage{hyperref}

\begin{document}

\section{A DSL for simulating the Continuous}

This project is dedicated to explore the concepts of simulating the continuous, based on one of the earliest version of \href{https://github.com/dsorokin/aivika}{aivika}. In principle, the DSL works as the following example:

\begin{code}
module Main where
import Control.Monad.Trans
import Data.IORef

spc = Specs { startTime = 0, 
              stopTime = 10, 
              dt = 1,
              method = RungeKutta4 }

model =
  do integA <- newInteg 100
     let a = integValue integA
     let ka = 1
     integDiff integA (- ka * a )
     return $ sequence a

main = 
  do a <- runDynamics1 model specs
     print a
     
\end{code}

The \texttt{spc} and \texttt{model} functions set sensible information, such as the time interval the simulation will occur, which solver method will be used, and the description of the differential equation, alongside the initial condition of such system.

In this demo, an integrator \texttt{integA} is created with an initial value of 100. The pointer \texttt{a} is responsable for picking the result in each iteration cached in system memory. Finally, the \texttt{integDiff} function set which differential equation the integrator will use, \texttt{- ka * a} in this case. The final line is just a wrapper in order to use a driver later

\section{Types}

\subsection{Representing the Continuous}

The most important type in the project is the \texttt{Dynamics} type, since it is the representation of a time continuous process. This type is a function that, given a parameter record, it will generate the answer wrapped in the IO monad.

\begin{code}
newtype Dynamics a = Dynamics {apply :: Parameters -> IO a}
\end{code}

There are important typeclasses for the \texttt{Dynamics} type, such as Functor, Applicative, Monad, and, because it is being used IO in the return type, MonadIO. However, because this type so similar to the \texttt{Reader} type, the implementations of these typeclasses are not complicated.

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

\end{code}

 Moreover, more typical typeclasses, such as Eq, Show, Num, Fractional and Floating are also implemented:

\begin{code}

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

\subsection{Representing the Environment}

The next data records represent information to execute the simulation. The \texttt{Specs} data type carries the time interval of the simulation, along with the size of the integration step and which method will be used, based on the \texttt{Method} type:

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

Finally, the \texttt{Parameters} type saves which specifications will be used across the simulation. Additionally, because it also used in the integration procedure itself, it also carries which time is the current time of a given calculation, which iteration is the current one, and in which stage of the solver method it is in a given moment.

\begin{code}
data Parameters = Parameters { specs     :: Specs,   
                               time      :: Double,   
                               iteration :: Int, 
                               stage     :: Int }    
                  deriving (Eq, Show)
\end{code}

\subsection{Representing the Integrator}

The integrator is being represented, intuitively, as a machine made of side effects. Inside this type, there are the following items: the initial condition of the system, one pointer to the memory managing caching and a second pointer resposable for calculating the current result:

\begin{code}
data Integ = Integ { initial     :: Dynamics Double,  
                     cache       :: IORef (Dynamics Double),
                     result      :: IORef (Dynamics Double) }
\end{code}

When building an integrator it is necessary to allocate memory to store the computed values, and point the pointers to the right place. The \texttt{newInteg} is responsable of doing this procedure, whilst the function \texttt{initD} wraps the initial value into a valid initial state of the integrator:

\begin{code}
initD :: Dynamics a -> Dynamics a
initD (Dynamics m) =
  Dynamics $ \ps ->
  if iteration ps == 0 && stage ps == 0 then
    m ps
  else
    let sc = specs ps
    in m $ ps { time = iterToTime sc 0 0,
                iteration = 0,
                stage = 0 } 

newInteg :: Dynamics Double -> Dynamics Integ
newInteg i = 
  do r1 <- liftIO $ newIORef $ initD i 
     r2 <- liftIO $ newIORef $ initD i 
     let integ = Integ { initial = i, 
                         cache   = r1,
                         result  = r2 }
         z = Dynamics $ \ps -> 
           do (Dynamics m) <- readIORef (result integ)
              m ps
     y <- umemo interpolate z
     liftIO $ writeIORef (cache integ) y
     return integ

\end{code}

\end{document}
