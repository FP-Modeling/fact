\ignore{
\begin{code}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances, ConstraintKinds, MonoLocalBinds #-}
module GraduationThesis.Lhs.Caching where
import GraduationThesis.Lhs.Implementation
import GraduationThesis.Lhs.Interpolation
import GraduationThesis.Lhs.Design
import Control.Monad.Trans
import Data.Array.IO
import Data.Array
import Data.IORef

initialize :: Dynamics a -> Dynamics a
initialize (Dynamics m) =
  Dynamics $ \ps ->
  if iteration ps == 0 && stage (solver ps) == 0 then
    m ps
  else
    let iv = interval ps
        sl = solver ps
    in m $ ps { time = iterToTime iv sl 0 0,
                iteration = 0,
                solver = sl { stage = 0 }}

class (MArray IOArray e IO) => Memo e where
  newMemoArray_ :: Ix i => (i, i) -> IO (IOArray i e)

class (MArray IOUArray e IO) => UMemo e where
  newMemoUArray_ :: Ix i => (i, i) -> IO (IOUArray i e)

instance Memo e where
  newMemoArray_ = newArray_
    
instance (MArray IOUArray e IO) => UMemo e where
  newMemoUArray_ = newArray_
  
stageBnds :: Solver -> (Int, Int)
stageBnds sl = 
  case method sl of
    Euler -> (0, 0)
    RungeKutta2 -> (0, 1)
    RungeKutta4 -> (0, 3)

stageLoBnd :: Solver -> Int
stageLoBnd sc = fst $ stageBnds sc
                  
stageHiBnd :: Solver -> Int
stageHiBnd sc = snd $ stageBnds sc
 
\end{code}
}

Chapter 5, \textit{Travelling across Domains}, leveraged leveraging a major concern with the proposed software: the solvers don't work in the domain of interest, continuous time. This chapter, \textit{Caching the Speed Pill}, addresses a second problem: the performance issue in \texttt{Rivika}. At the end of it, the simulation will be orders of magnitude faster by using a common modern caching strategy to speed up computing processes: memoization.

\section{Performance}

The simulations from \texttt{Rivika} take too long to execute. For instance, to execute the Lorenz's Attractor example using the second-order Runge-Kutta method, with an unrealistic time step size for real simulations (time step of $1$ second), the simulator can take around \textbf{8 seconds} to compute 0 to 5 seconds of the physical system with a testbench using a 6-th generation quad-core (\texttt{i5}) Intel processor and 16GB of RAM. Increasing this interval shows an exponential growth in execution time, as depicted by Table \ref{tab:execTimes} (values obtained with the interpolation tweak). Although the memory values are also impractical, it is hard to reason about those number due to Haskell's \textbf{garbage collector}~\footnote{Garbage Collector \href{https://wiki.haskell.org/GHC/Memory\_Management}{\textcolor{blue}{wiki page}}.}, a memory manager that deals with Haskell's \textbf{immutability}. Thus, the memory values serve just to solidify the notion that \texttt{Rivika} is inneficient, showing an exponentinal growth in resource use, which makes it impractical to execute longer simulations and diminishes the usability of the proposed software.

\begin{table}[H]
\centering
\begin{tabular}{ccc}
\hline
Total of Iterations & Execution Time (seconds) & Consumed Memory (Mb) \\ \hline
1                   & 0.00                     &  0.5      \\ \hline
2                   & 0.01                     &  1.6      \\ \hline
3                   & 0.06                     &  16.5     \\ \hline
4                   & 0.74                     &  211.3    \\ \hline
5                   & 8.84                     &  2761.2   \\ \hline
6                   & 120.94                   &  36140.1  \\ \hline
7                   & 1544.66                  &  473074.8 \\ \hline
8                   & 19970.17                 &       \\ \hline
\end{tabular}
\caption{\label{tab:execTimes}Small increases in the number of the iterations within the simulation provoke exponential penalties in performance.}
\end{table}

\section{The Saving Strategy}

Before explaining the solution, it is worth describing \textbf{why} and \textbf{where} this problem arises. First, we need to take a look back onto the solvers' functions, such as the \textit{integEuler} function, introduced in chapter 3, \textit{Effectcul Integrals}:


\begin{spec}
integEuler :: Dynamics Double
             -> Dynamics Double 
             -> Dynamics Double 
             -> Parameters -> IO Double
integEuler (Dynamics diff) (Dynamics init) (Dynamics compute) ps =
  case iteration ps of
    0 -> 
      init ps
    n -> do 
      let iv  = interval ps
          sl  = solver ps
          ty  = iterToTime iv sl (n - 1) 0
          prevPS = ps { time = ty, iteration = n - 1, solver = sl { stage = 0} }
      a <- compute prevPS
      b <- diff prevPS
      let !v = a + dt (solver ps) * b
      return v
\end{spec}

From chapter 3, we know that lines 10 to 13 serve the purpose of creating a new parameters record to execute a new solver step for the \textbf{previous} iteration, in order to calculate the current one. From chapter 4, this code section turned out to be where the implicit recursion came in, because the current iteration needs to calculate the previous one. Effectively, this means that for \textbf{all} iterations, \textbf{all} previous steps from each one needs to be calculated. The problem is now clear: unnecessary computations are being made for all iterations, because the same solvers steps are not being saved for future steps, although these values do \textbf{not} change. In other words, to calculate step 3 of the solver, steps 1 and 2 are the same to calculate step 4 as well, but these values are being lost during the simulation.

To estimate how this lack of optimization affects performance, we can calculate how many solver steps will be executed to simulate a similar example to the Lorenz's Attractor example used in chapter 4, \textit{Execution Walkthrough}. The Table \ref{tab:solverSteps} shows the total number of solver steps needed per iteration simulating the Lorenz example with the Euler method. In addition, the amount of steps also increase depending on which solver method is being used, given that in the higher order Runge-Kutta methods, multiple stages count as a new step as well.

\begin{table}[H]
\centering
\begin{tabular}{cc}
\hline
Iteration & Total Solver Steps \\ \hline
1         & 1            \\ \hline
2         & 3            \\ \hline
3         & 6            \\ \hline
4         & 10           \\ \hline
5         & 15           \\ \hline
6         & 21           \\ \hline
\end{tabular}
\caption{\label{tab:solverSteps}Because the previous solver steps are not saved, the total number of steps \textbf{per iteration} starts to accumullate following the numerical sequence of \textbf{triangular numbers} when using the Euler method.}
\end{table}

This is the cause of the imense hit in performance. However, it also clarifies the solution: if the previous solver steps are saved, the next iterations don't need to re-compute then in order to continue. In the computer domain, the act of saving previous steps that do not change is called \textbf{memoization} and is one form to execute \textbf{caching}. This optimization technique stores the values in a register or memory region and, instead of the process starts calculating the result again, it consults this region to quickly obtain the answer. Thus, this improvement will be added to \texttt{Rivika} in the next section.

\section{Tweak II: Model and Driver}

The first tweak of this chapter is related to executable models established in chapter 4. The code bellow is the same example model used in that chapter:

\begin{spec}
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

This preceding caching strategy assumes that the created mutable array will be available for the entire simulation. However, the proposed models will \textbf{always} discard the created table by the \textit{newInteg} function to the garbage collector~\footnote{Garbage Collector \href{https://wiki.haskell.org/GHC/Memory\_Management}{\textcolor{blue}{wiki page}}.}, after the \textit{sequence} function. Even worse, the table will be created again each time the model is being called and a parametric record is being provided. Thus, the proposed solution to address this problem is to update the \texttt{Model} alias to  a \textbf{function} of the model. This can be achieved by \textbf{wrapping} the state vector with a the \texttt{Dynamics} type, i.e., wrapping the model using the function \textit{pure} or \textit{return}. In this manner, the computation will be "placed" as side effect of the \texttt{IO} monad and Haskell's memory management system will not remove the caching table created initially, in the first computation. So, the following is the new type alias, alongside the previous example model using the \textit{return} function:

\begin{spec}
type Model a = Dynamics (Dynamics a)

exampleModel :: Model Vector
exampleModel =
  do integX <- newInteg 1
     integY <- newInteg 1
     let x = readInteg integX
         y = readInteg integY
     diffInteg integX (x * y)
     diffInteg integY (y + t)
     return $ sequence [x, y]
\end{spec}

Due to the new type signature, this change implies changing the driver, i.e., modify the function \textit{runDynamics} (the changes are analogus to the \textit{runDynamicsFinal} function variant).

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
         ps = Parameters { interval = iv,
                           time = stopTime iv,
                           iteration = nu,
                           solver = sl { stage = -1}}
     if (iterToTime iv sl nu 0) - (stopTime iv) < epslon
     then map (m . parameterise) [nl .. nu]
     else (init $ map (m . parameterise) [nl .. nu]) ++ [m ps]     
\end{code}

The main change is the division of the driver into two functions: one dedicated to "initiate" the simulation environment providing an initial record of the type \texttt{Parameters} (lines 3 to 6), and an auxiliary function doing the mapping to the iteration axis (lines 10 to 14, 20 and 21), as well as verifying the need for interpolation (lines 15 to 19). Thus, these two functions, \textit{runDynamics} and \textit{subRunDynamics} defines the final driver of \texttt{Rivika}.

\section{Tweak III: Memoization}

This tweak, \textit{Memoization}, alters the \texttt{Integrator} type. The integrator will now have a pointer to the memory region that stores the previous computed values, meaning that before executing a new computation, it will consult this region first. Because the process is executed in a \textbf{sequential} manner, it is guaranteed that the previous result will be used. Thus, the accumulation of the solver steps will be addressed, and the amount of steps will be equal to amount of iterations times how many stages the solver method uses.

The \textit{memo} function creates this memory region for storing values, as well as providing read access to it. This is the only function in \texttt{Rivika} that uses a \textit{constraint}, i.e., it restricts the polymorphic types to the ones that have implemented the requirement. In our case, this functions requires that the internal type of the provided values of type \texttt{Dynamics} have implemented the \texttt{UMemo} typeclass. Because this typeclass too complicated to be in the scope of this project, we will settle with the following explanation: the polymorphic values need to be contained inside a \textbf{mutable} array, which is the case for our \texttt{Double} values. As dependencies, the \textit{memo} function receives the dynamic computation, as well as the interpolation function that is assumed to be used, in order to attenuate the domain problem described in the previous chapter. This means that at the end, the final result needs to be interpolated by the provided function. 

\begin{code}
memo :: UMemo e => (Dynamics e -> Dynamics e) -> Dynamics e 
        -> Dynamics (Dynamics e)
memo tr (Dynamics m) = 
  Dynamics $ \ps ->
  do let sl = solver ps
         iv = interval ps
         (stl, stu) = stageBnds sl
         (nl, nu)   = iterationBnds iv (dt sl)
     arr   <- newMemoUArray_ ((stl, nl), (stu, nu))
     nref  <- newIORef 0
     stref <- newIORef 0
     let r ps =
           do let sl  = solver ps
                  iv  = interval ps
                  n   = iteration ps
                  st  = stage sl
                  stu = stageHiBnd sl 
                  loop n' st' = 
                    if (n' > n) || ((n' == n) && (st' > st)) 
                    then 
                      readArray arr (st, n)
                    else 
                      let ps' = ps { time = iterToTime iv sl n' st',
                                     iteration = n',
                                     solver = sl { stage = st' }}
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

The function starts by getting how many iterations will occur in the simulation, as well as how many stages the chosen method uses (lines 5 to 8). This is used to pre-allocate the minimum amount of memory required for the execution (line 9). This mutable array is two-dimensional and can be viewed as a table in which the number of iterations and stages determine the number of rows and columns. Pointers to iterate accross the table are declared as \textit{nref} and \textit{stref} (lines 10 and 11), to read iteration and stage values respectively. The code block from line 12 to line 36 is used in a similar manner to what the function \textit{diffInteg} does: this block is a procedure or computation that will only be used when needed, and this internal \textit{r} function is only being called at the end of the \textit{memo} function (line 37).

The next step is to understand what the internal functions does. From line 13 to line 17, auxiliar "variables", i.e., labels to read information, are created to facilitate manipulation of the solver (\textit{sl}), interval (\textit{iv}), current iteration (\textit{n}), current stage (\textit{st}) and the final stage used in a solver step (\textit{stu}). The definition of \textit{loop}, which starts at line 18 and closes at line 33, uses all the previously created labels. The conditional block (line 19 to 33) will store in the pre-allocated memory region the computed values and, because they are stored in a \textbf{sequential} way, the stop condition of the loop is one of the following: the iteration counter of the loop (\texttt{n'}) surpassed the current iteration \textbf{or} the iteration counter matches the current iteration \textbf{and} the stage counter (\texttt{st'}) reached the ceiling of stages of used solver method (line 19). The loop stops by \textbf{reading} from the allocated array the value of interest, given that it is guaranteed that is already in memory (line 21). If this condition is not true, it means that further iterations in the loop need to occur in one of the two axis, iteration or stage.

The first step towards that goal is to save the value of the current iteration and stage into memory. The dynamic computation \textit{m}, received as a dependency in line 3, is used to compute a new result with the current counters for iteration and stage (lines 23 to 26). Then, this new value is written into the array (line 27). The condition in line 28 checks if the current stage already achieved its maximum possible value, meaning that the counters for iteration and stage will be refreshed to the next iteration (line 30) and first stage (line 29) respectively, and the loop should continue (line 31). Otherwise, we need to advance to the next stage within the same iteration and an updated stage (line 32); the loop should continue with the same iteration counter but with the stage counter incremented (lines 32 and 33).

Lines 34 to 36 are the trigger to the beginning of the loop, with \textit{nref} and \textit{stref} being read. These values set the initial values for the counters used in the \textit{loop} function, and both of their values start at zero (lines 10 and 11).  All computations related to the \textit{loop} function will only be called when the \textit{r} function is called. Further, all of these impure computations (lines 12 to 36) compose the definition of \textit{r} (line 12), which is being returned in line 37 combined with the interpolation function \textit{tr} and being wrapped into an extra \texttt{Dynamics} shell via the \textit{return} function (provided by the \texttt{Monad} typeclass).

With this function on-hand, it remains to couple it to the \texttt{Integrator} type, meaning that \textbf{all} integrator functions need to be aware of this new caching strategy. First and foremost, a pointer to this memory region needs to be added to the integrator type:

\begin{code}
data Integrator = Integrator { initial     :: Dynamics Double,
                               cache       :: IORef (Dynamics Double),
                               computation :: IORef (Dynamics Double)
                             }
\end{code}

Next, two other function need to be adapted: \textit{newInteg} and \textit{readInteg}. In the former function, the new pointer will be used, \texttt{cache}, and it points to the region where the mutable array will be allocated. In the latter, instead of reading from the computation itself, the read-only pointer will be looking at the \textbf{cached} version, always reading from memory. These differences will be illustrated by using the same integrator and state variables used in the Lorenz's Attractor example, detailed in chapter 4, \textit{Execution Walkthrough}.

\begin{figure}[ht!]
\begin{code}
newInteg :: Dynamics Double -> Dynamics Integrator
newInteg i = 
  do comp <- liftIO $ newIORef $ initialize i 
     cachedComp <- liftIO $ newIORef $ initialize i 
     let integ = Integrator { initial      = i, 
                              cache        = cachedComp,
                              computation  = comp }
         readComp = Dynamics $ \ps ->
                  do (Dynamics m) <- readIORef (computation integ)
                     m ps
     interpCached <- memo interpolate readComp
     liftIO $ writeIORef (cache integ) interpCached
     return integ
\end{code}
\begin{center}
  \includegraphics[width=0.95\linewidth]{GraduationThesis/img/NewInteg}
\end{center}
\caption{The new \textit{newInteg} function relies on interpolation composed with memoization. Also, this combination \textbf{reads} the results located in the computation's memory region.}
\label{fig:newInteg}
\end{figure}

The main difference in the updated version of the \textit{newInteg} function is the inclusion of the new pointer that reads the cached memory (lines 4 to 7). The pointer \texttt{computation}, which will be changed by \textit{diffInteg} in a model to the differential equation, is being read in lines 8 to 10 and piped with interpolation and memoization in line 11. This approach maintains the interpolation, justified in the previous chapter, and adds the aforementioned caching strategy. Finally, the final result is the written in the memory region pointed by the caching pointer (line 12).

Figure \ref{fig:newInteg} shows that the updated version of the \textit{newInteg} function is similar to the previous implementation. The new field, \texttt{cached}, is a pointer that refers to \texttt{readComp}, which is the result of memoization (\texttt{memo}) combined with interpolation (\texttt{interpolate}) and the value obtained by the region pointed by the \texttt{computation} pointer. Given a parametric record \texttt{ps}, \texttt{readComp} gives this record to the dynamics value stored in the region where the computation is being executed. This result is then interpolated via the \texttt{interpolate} block and it used as a dependency for the \texttt{memo} block.

\begin{figure}[ht!]
\begin{code}
readInteg :: Integrator -> Dynamics Double
readInteg integ = 
  Dynamics $ \ps ->
  do (Dynamics m) <- readIORef (cache integ)
     m ps
\end{code}
\begin{center}
  \includegraphics[width=0.95\linewidth]{GraduationThesis/img/ReadInteg}
\end{center}
\caption{The function dedicated to reading information now collects data from the caching pointer, rather than reading the computation's memory region directly.}
\label{fig:readInteg}
\end{figure}

The modifications in the \textit{readInteg} function are being portrayed in Figure \ref{fig:readInteg}. As described earlier, the change is minor: instead of reading from the region pointed by the \texttt{computation} pointer, this function will the value contained in the region pointed by the \texttt{cache} (line 4). This means that the same \texttt{readComp}, described in the new \textit{newInteg} function, will be read and applied to a given \texttt{ps}. It is worth noticing that, just like with the \textit{newInteg} function, this cache pointer indirectly interacts with the same region pointed by the computation pointer (Figure \ref{fig:readInteg}).

\begin{figure}[ht!]
\begin{code}
diffInteg :: Integrator -> Dynamics Double -> Dynamics ()
diffInteg integ diff =
  do let z = Dynamics $ \ps ->
           do whatToDo <- readIORef (cache integ)
              let i = initial integ
              case method (solver ps) of
                Euler -> integEuler diff i y ps
                RungeKutta2 -> integRK2 diff i whatToDo ps
                RungeKutta4 -> integRK4 diff i whatToDo ps
     liftIO $ writeIORef (computation integ) z
\end{code}     
\begin{center}
  \includegraphics[width=0.95\linewidth]{GraduationThesis/img/DiffInteg}
\end{center}
\caption{The new \textit{diffInteg} function gives to the solver functions access to the region with the cached data.}
\label{fig:diffInteg}
\end{figure}

Lastly, Figure \ref{fig:diffInteg} depicts the new version of the \textit{diffInteg} function, and the tweaks in this function are minor, just as with the \textit{readInteg} function. Previously, the \texttt{whatToDo} label, used as a dependency in the solver methods, was being made by reading the content in the region pointed by the \texttt{computation} pointer. Now, this dependency reads the region related to the caching methodology via reading the \texttt{cache} pointer. Again, as illustrated by Figure \ref{fig:diffInteg}, the \texttt{readComp} pipeline is being used internally.

The solver functions, \textit{integEuler}, \textit{integRK2} and \textit{integRK4}, always need to calculate the value of the previous iteration. By giving them access to the cached region of the simulation, instead of starting a recursive chain of stack calls until it reaches the base case, the previous computation will be handled immediately. This is the key to achieve orders of magnitude of better performance.

\newpage

\section{Improvements}

The following table (Table \ref{tab:betterResults}) shows the same Lorenz's Attractor example used in the first section, but with the preceding tweaks in the \texttt{Integrator} type and the integrator functions. These modifications allows better and more complicated models to be simulated. For instance, the Lorenz example with a time step of $0.01$ going from 0 to 40 seconds took $1.53$ seconds and consumed 457.6 Mb of memory.

\begin{table}[H]
\centering
\begin{tabular}{ccc}
\hline
Total of Iterations & Execution Time (seconds) & Consumed Memory (Mb) \\ \hline
1                   & 0.00                     &  0.5  \\ \hline
2                   & 0.00                     &  0.6  \\ \hline
3                   & 0.00                     &  0.7  \\ \hline
4                   & 0.00                     &  0.8  \\ \hline
5                   & 0.00                     &  0.9  \\ \hline
6                   & 0.01                     &  1.1  \\ \hline
7                   & 0.01                     &  1.2  \\ \hline
8                   & 0.00                     &  1.3  \\ \hline
\end{tabular}
\caption{\label{tab:betterResults}These values were obtained using the same hardware. It shows that the caching strategy drastically improves \texttt{Rivika}'s performance. Again, the concrete memory values obtained from GHC should be considered as just an indicative of improvement due to the garbage collector interference.}
\end{table}

This summarizes the chapter, closing the arc about addressing drawbacks. The project is currently capable of executing interpolation as well as applying memoization to speed up results. These two solutions, detailed in chapter 5 and 6, adds practicality to \texttt{Rivika} as well as makes it more competitive. The final chapter, \textit{Conclusion}, will conclude this work, pointing out future improvements and final thoughts about the project.
