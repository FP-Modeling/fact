\documentclass{article}
%include colorcode.fmt
\usepackage[a4paper, total={6in, 8in}]{geometry}
\long\def\ignore#1{}

\usepackage{minted}
\newminted[code]{haskell}{breaklines}

\usepackage{hyperref}

\begin{document}

\section{A DSL for simulating the Continuous}
\label{DSL}

This project is dedicated to explore the concepts of simulating the continuous, based on one of the earliest version of \href{https://github.com/dsorokin/aivika}{aivika}. In principle, the DSL works as the following example:

\ignore{
\begin{code}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances, BangPatterns, ConstraintKinds, MonoLocalBinds #-}
module Main where
import Control.Monad.Trans
import Data.IORef
import Data.Array
import Data.Array.IO
\end{code}
}

\begin{code}
spc = Specs { startTime = 0, 
              stopTime = 10, 
              dt = 1,
              method = RungeKutta4 }

model =
  do integA <- newInteg 100
     let a = integValue integA
     let ka = 1
     integDiff integA (- ka * a )
     return $ sequence [a]

main = 
  do a <- runDynamics1 model spc
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

Finally, the \texttt{Parameters} type saves which specifications will be used across the simulation. Additionally, because it also used in the integration procedure itself, it also carries which time is the current time of a given calculation, which iteration is the current one, and in which stage of the solver method it is in.

\begin{code}
data Parameters = Parameters { specs     :: Specs,   
                               time      :: Double,   
                               iteration :: Int, 
                               stage     :: Int }    
                  deriving (Eq, Show)
\end{code}

\subsection{Representing the Integrator}

The integrator is being represented, intuitively, as a machine made of side effects. Inside this type, there are the following items: the initial condition of the system, one pointer to the memory managing caching and a second pointer resposable for calculating the current result:
2
\begin{code}
data Integ = Integ { initial     :: Dynamics Double,  
                     cache       :: IORef (Dynamics Double),
                     result      :: IORef (Dynamics Double) }
\end{code}

When building an integrator it is necessary to allocate memory to store the computed values, and point the pointers to the right place. The \texttt{newInteg} is responsable of doing this procedure, whilst the function \texttt{initD} wraps the initial value into a valid initial state of the integrator:

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

The function \texttt{initD} assures that the parameter value is configured right, setting the number of the current iteration, time and stage all to zero. This is necessary to guarantee that the computation follows the right sequence of steps, in order. The auxiliary function \texttt{iterToTime} transforms a given iteration $n$ to its correspondent in the time domain, based on the solver method and in which stage of the solving process the program is in.

The second function, \texttt{newInteg}, is, alongside all the major integrator functions, one of the \textbf{most complicated} of the functions being used. In the beginning, it is being created two pointers to the memory, \texttt{r1} and \texttt{r2}, both of which, regardless of the content of a given \texttt{ps} it will always return the \textbf{initial value}. Next step is to create the \texttt{integ} data type, as has been shown previously. The label \texttt{z} is then created, being a computation, i.e., a \texttt{Dynamics} in which reads first whatever the \texttt{result} is pointing to and uses it to apply a given set of \texttt{Parameters}. It is important to keep in mind that at the current moment, this pointer is pointer to \texttt{a process} in which, given anything with the correct type, it will give you the initial value.

The final lines are related to the caching and interpolation. Caching is important given that the current computation uses the previous computation and so forth. However, because these values are always the same, we only need to calculate them only once and save them in memory. This is the role of the \texttt{umemo} function.

\begin{code}
class (MArray IOUArray e IO) => UMemo e where
  newMemoUArray_ :: Ix i => (i, i) -> IO (IOUArray i e)

instance (MArray IOUArray e IO) => UMemo e where
  newMemoUArray_ = newArray_

stageBnds :: Specs -> (Int, Int)
stageBnds sc = 
  case method sc of
    Euler -> (0, 0)
    RungeKutta2 -> (0, 1)
    RungeKutta4 -> (0, 3)

stageHiBnd :: Specs -> Int
stageHiBnd sc = snd $ stageBnds sc

iterationBnds :: Specs -> (Int, Int)
iterationBnds sc = (0, round ((stopTime sc - 
                               startTime sc) / dt sc))

umemo :: UMemo e => (Dynamics e -> Dynamics e) -> Dynamics e 
        -> Dynamics (Dynamics e)
umemo tr (Dynamics m) = 
  Dynamics $ \ps ->
  do let sc = specs ps
         (stl, stu) = stageBnds sc
         (nl, nu)   = iterationBnds sc
     arr   <- newMemoUArray_ ((stl, nl), (stu, nu))
     nref  <- newIORef 0
     stref <- newIORef 0
     let r ps =
           do let sc  = specs ps
                  n   = iteration ps
                  st  = stage ps
                  stu = stageHiBnd sc 
                  loop n' st' = 
                    if (n' > n) || ((n' == n) && (st' > st)) 
                    then 
                      readArray arr (st, n)
                    else 
                      let ps' = ps { iteration = n', 
                                     stage = st',
                                     time = iterToTime sc n' st' }
                      in do a <- m ps'
                            a `seq` writeArray arr (st', n') a
                            if st' >= stu 
                              then do writeIORef stref 0
                                      writeIORef nref (n' + 1)
                                      loop (n' + 1) 0
                              else do writeIORef stref (st' + 1)
                                      loop n' (st' + 1)
              n'  <- readIORef nref
              st' <- readIORef stref
              loop n' st'
     return $ tr $ Dynamics r
\end{code}

% Add explanation of umemo here

Interpolation is important given that not every time the time of calculation is a multiple of the integration step, so it is necessary to interpolate between two known values.

\begin{code}
iterationLoBnd :: Specs -> Int
iterationLoBnd sc = fst $ iterationBnds sc

iterationHiBnd :: Specs -> Int
iterationHiBnd sc = snd $ iterationBnds sc                   

interpolate :: Dynamics Double -> Dynamics Double
interpolate (Dynamics m) = 
  Dynamics $ \ps -> 
  if stage ps >= 0 then 
    m ps
  else 
    let sc = specs ps
        t  = time ps
        x  = (t - startTime sc) / dt sc
        n1 = max (floor x) (iterationLoBnd sc)
        n2 = min (ceiling x) (iterationHiBnd sc)
        t1 = iterToTime sc n1 0
        t2 = iterToTime sc n2 0
        z1 = m $ ps { time = t1, 
                      iteration = n1, 
                      stage = 0 }
        z2 = m $ ps { time = t2,
                      iteration = n2,
                      stage = 0 }
        r | t == t1   = z1
          | t == t2   = z2
          | otherwise = 
            do y1 <- z1
               y2 <- z2
               return $ y1 + (y2 - y1) * (t - t1) / (t2 - t1)
    in r
\end{code}

% Add explanation of interpolate here

As shown in section \ref{DSL}, we are using the function \texttt{integValue} in the model of our example. This function reads whatever the \textit{cache} pointer is pointing to and applies the obtained \texttt{Dynamics} with a given \texttt{Parameters} record.

\begin{code}
integValue :: Integ -> Dynamics Double
integValue integ = 
  Dynamics $ \ps ->
  do (Dynamics m) <- readIORef (cache integ)
     m ps
\end{code}

Finally, there is the \texttt{integDiff} function, also used in the presentation section of the DSL. This function is the core of the integrator, since it is responsable of \textbf{changing which process the integrator will use}. When we create an integrator using \texttt{newInteg}, the \textit{result} pointer is pointing to a "dumb" computation (given any record with type \texttt{Parameters}, I will give you the initial value). This side-effect-only function changes which computation will be used during the solution of the system, according to the solver method.

\begin{code}
integDiff :: Integ -> Dynamics Double -> Dynamics ()
integDiff integ diff =
  do let z = Dynamics $ \ps ->
           do y <- readIORef (cache integ)
              let i = initial integ
              case method (specs ps) of
                Euler -> integEuler diff i y ps
                RungeKutta2 -> integRK2 diff i y ps
                RungeKutta4 -> integRK4 diff i y ps
     liftIO $ writeIORef (result integ) z
\end{code}

\section{Solver Methods}

There are three solver methods available in mivika. More methods can be used in the original project,  \href{https://github.com/dsorokin/aivika}{aivika}. Mivika supports the following methods for integration:

\begin{itemize}
  \item \href{https://en.wikipedia.org/wiki/Euler_method}{Euler}
  \item \href{https://en.wikipedia.org/wiki/Runge%E2%80%93Kutta_methods}{Runge-Kutta 2nd order}
  \item \href{https://en.wikipedia.org/wiki/Runge%E2%80%93Kutta_methods}{Runge-Kutta 4th order}
\end{itemize}

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
            t1 = ty
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
          t1 = ty
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
            t1 = ty
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
          t1 = ty
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

\section{The Driver}

The last piece of the puzzle is the top layer, responsable for acting as the driver of the system. The couple of functions \texttt{runDynamics1} and \texttt{runDynamics} fulfills this role. The former execute the simulation and shows only the result of the last time point, while the latter shows the values of the interval specied in the \texttt{Specs} record.

\begin{code}
runDynamics1 :: Dynamics (Dynamics a) -> Specs -> IO a
runDynamics1 (Dynamics m) sc = 
  do d <- m Parameters { specs = sc,
                         time = startTime sc,
                         iteration = 0,
                         stage = 0 }
     subrunDynamics1 d sc

runDynamics :: Dynamics (Dynamics a) -> Specs -> IO [a]
runDynamics (Dynamics m) sc = 
  do d <- m Parameters { specs = sc,
                         time = startTime sc,
                         iteration = 0,
                         stage = 0 }
     sequence $ subrunDynamics d sc

subrunDynamics1 :: Dynamics a -> Specs -> IO a
subrunDynamics1 (Dynamics m) sc =
  do let n = iterationHiBnd sc
         t = iterToTime sc n 0
     m Parameters { specs = sc,
                    time = t,
                    iteration = n,
                    stage = 0 }

subrunDynamics :: Dynamics a -> Specs -> [IO a]
subrunDynamics (Dynamics m) sc =
  do let (nl, nu) = iterationBnds sc
         parameterise n = Parameters { specs = sc,
                                       time = iterToTime sc n 0,
                                       iteration = n,
                                       stage = 0 }
     map (m . parameterise) [nl .. nu]
\end{code}

Use the operational amplifier as example of use of mfix

Cap 1: Introduction - GPAC Edil - Pega a intro de lá (FOCO)
                      George Date 2018 - Pega a intro de lá
                      George and Edil 2021 - Revista ACM Transaction on Embedded Computing systems - intro de lá
                      Escopo objetivos - DSL - search smart goals
                      Exemplo de caso particular - Lorentz

Cap 2: Exemplo sem caching! Bottom-up or Top-down? How to be faster?

Cap 3: Exemplo com caching e interpolate!

Cap 4: Fix and mfix - Amp Op Pure Example! GPAC inspired

Cap 5: Mfix + mivika

Conclusao
Cap 6: Future improvements (PURE) and related: Reactive programming

Cap 7: Side effects are bad!

\end{document}
