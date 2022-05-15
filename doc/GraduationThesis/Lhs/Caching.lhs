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

Chapter 5, \textit{Travelling across Domains}, leveraged a major concern with the proposed software: the solvers don't work in the domain of interest, continuous time. This chapter, \textit{Caching the Speed Pill}, addresses a second problem: the performance in \texttt{Rivika}. At the end of it, the simulation will be orders of magnitude faster by using a common modern caching strategy to speed up computing processes: memoization.

\section{Performance}

The simulations executed in \texttt{Rivika} take too long to run. For instance, to execute the Lorenz's Attractor example using the second-order Runge-Kutta method with an unrealistic time step size for real simulations (time step of $1$ second), the simulator can take around \textbf{10 seconds} to compute 0 to 5 seconds of the physical system with a testbench using a 6-th generation quad-core (\texttt{i5}) Intel processor and 16GB of RAM. Increasing this interval shows an exponential growth in execution time, as depicted by Table \ref{tab:execTimes} and by Figure \ref{fig:graph1} (values obtained after the interpolation tweak). Although the memory use is also problematic, it is hard to reason about those numbers due to Haskell's \textbf{garbage collector}~\footnote{Garbage Collector \href{https://wiki.haskell.org/GHC/Memory\_Management}{\textcolor{blue}{wiki page}}.}, a memory manager that deals with Haskell's \textbf{immutability}. Thus, the memory values serve just to solidify the notion that \texttt{Rivika} is inneficient, showing an exponentinal growth in resource use, which makes it impractical to execute longer simulations and diminishes the usability of the proposed software.

\begin{table}[H]
\centering
\begin{tabular}{ccc}
\hline
Total of Iterations & Execution Time (seconds) & Consumed Memory (MB) \\ \hline
1                   & 0.01                     &  0.5       \\ \hline
2                   & 0.01                     &  1.8       \\ \hline
3                   & 0.08                     &  19.1      \\ \hline
4                   & 0.79                     &  244.7     \\ \hline
5                   & 10.06                    &  3198.7    \\ \hline
6                   & 140.95                   &  41867.3   \\ \hline
7                   & 1798.16                  &  548045.8  \\ \hline
8                   & 23801.51                 &  7174008.0 \\ \hline
\end{tabular}
\caption{\label{tab:execTimes}Small increases in the number of the iterations within the simulation provoke exponential penalties in performance.}
\end{table}

\figuraBib{Graph1}{With just a few iterations, the exponential behaviour of the implementation is already noticeable}{}{fig:graph1}{width=.85\textwidth}%

\section{The Saving Strategy}

Before explaining the solution, it is worth describing \textbf{why} and \textbf{where} this problem arises. First, we need to take a look back onto the solvers' functions, such as the \textit{integEuler} function, introduced in chapter 3, \textit{Effectful Integrals}:


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

From chapter 3, we know that lines 10 to 13 serve the purpose of creating a new parametric record to execute a new solver step for the \textbf{previous} iteration, in order to calculate the current one. From chapter 4, this code section turned out to be where the implicit recursion came in, because the current iteration needs to calculate the previous one. Effectively, this means that for \textbf{all} iterations, \textbf{all} previous steps from each one needs to be calculated. The problem is now clear: unnecessary computations are being made for all iterations, because the same solvers steps are not being saved for future steps, although these values do \textbf{not} change. In other words, to calculate step 3 of the solver, steps 1 and 2 are the same to calculate step 4 as well, but these values are being lost during the simulation.

To estimate how this lack of optimization affects performance, we can calculate how many solver steps will be executed to simulate theLorenz's Attractor example used in chapter 4, \textit{Execution Walkthrough}. The Table \ref{tab:solverSteps} shows the total number of solver steps needed per iteration simulating the Lorenz example with the Euler method. In addition, the amount of steps also increase depending on which solver method is being used, given that in the higher order Runge-Kutta methods, multiple stages count as a new step as well.

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

This is the cause of the imense hit in performance. However, it also clarifies the solution: if the previous solver steps are saved, the next iterations don't need to re-compute them in order to continue. In the computer domain, the act of saving previous steps that do not change is called \textbf{memoization} and it is one form to execute \textbf{caching}. This optimization technique stores the values in a register or memory region and, instead of the process starts calculating the result again, it consults this region to quickly obtain the answer.

\section{Tweak II: Memoization}

The first tweak, \textit{Memoization}, alters the \texttt{Integrator} type. The integrator will now have a pointer to the memory region that stores the previous computed values, meaning that before executing a new computation, it will consult this region first. Because the process is executed in a \textbf{sequential} manner, it is guaranteed that the previous result will be used. Thus, the accumulation of the solver steps will be addressed, and the amount of steps will be equal to the amount of iterations times how many stages the solver method uses.

The \textit{memo} function creates this memory region for storing values, as well as providing read access to it. This is the only function in \texttt{Rivika} that uses a \textit{constraint}, i.e., it restricts the parametric types to the ones that have implemented the requirement. In our case, this function requires that the internal type \texttt{Dynamics} dependency has implemented the \texttt{UMemo} typeclass. Because this typeclass is too complicated to be in the scope of this project, we will settle with the following explanation: it is required that the parametric values are capable of being contained inside an \textbf{mutable} array, which is the case for our \texttt{Double} values. As dependencies, the \textit{memo} function receives the dynamic computation, as well as the interpolation function that is assumed to be used, in order to attenuate the domain problem described in the previous chapter. This means that at the end, the final result will be piped to the interpolation function. 

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

The function starts by getting how many iterations will occur in the simulation, as well as how many stages the chosen method uses (lines 5 to 8). This is used to pre-allocate the minimum amount of memory required for the execution (line 9). This mutable array is two-dimensional and can be viewed as a table in which the number of iterations and stages determine the number of rows and columns. Pointers to iterate accross the table are declared as \textit{nref} and \textit{stref} (lines 10 and 11), to read iteration and stage values respectively. The code block from line 12 to line 36 delimit a procedure or computation that will only be used when needed, and it is being called at the end of the \textit{memo} function (line 37).

The next step is to follow the exection of this internal function. From line 13 to line 17, auxiliar "variables", i.e., labels to read information, are created to facilitate manipulation of the solver (\texttt{sl}), interval (\texttt{iv}), current iteration (\texttt{n}), current stage (\texttt{st}) and the final stage used in a solver step (\texttt{stu}). The definition of \textit{loop}, which starts at line 18 and closes at line 33, uses all the previously created labels. The conditional block (line 19 to 33) will store in the pre-allocated memory region the computed values and, because they are stored in a \textbf{sequential} way, the stop condition of the loop is one of the following: the iteration counter of the loop (\texttt{n'}) surpassed the current iteration \textbf{or} the iteration counter matches the current iteration \textbf{and} the stage counter (\texttt{st'}) reached the ceiling of stages of used solver method (line 19). When the loop stops, it \textbf{reads} from the allocated array the value of interest (line 21), given that it is guaranteed that is already in memory. If this condition is not true, it means that further iterations in the loop need to occur in one of the two axis, iteration or stage.

The first step towards that goal is to save the value of the current iteration and stage into memory. The dynamic computation \texttt{m}, received as a dependency in line 3, is used to compute a new result with the current counters for iteration and stage (lines 23 to 26). Then, this new value is written into the array (line 27). The condition in line 28 checks if the current stage already achieved its maximum possible value. In that case, the counters for stage and iteration counters will be refreshed to the first stage (line 29) of the next iteration (line 30) respectively, and the loop should continue (line 31). Otherwise, we need to advance to the next stage within the same iteration and an updated stage (line 32). The loop should continue with the same iteration counter but with the stage counter incremented (lines 32 and 33).

Lines 34 to 36 are the trigger to the beginning of the loop, with \textit{nref} and \textit{stref} being read. These values set the initial values for the counters used in the \textit{loop} function, and both of their values start at zero (lines 10 and 11).  All computations related to the \textit{loop} function will only be called when the \textit{r} function is called. Further, all of these impure computations (lines 12 to 36) compose the definition of \textit{r} (line 12), which is being returned in line 37 combined with the interpolation function \textit{tr} and being wrapped with an extra \texttt{Dynamics} shell via the \textit{return} function (provided by the \texttt{Monad} typeclass).

With this function on-hand, it remains to couple it to the \texttt{Integrator} type, meaning that \textbf{all} integrator functions need to be aware of this new caching strategy. First and foremost, a pointer to this memory region needs to be added to the integrator type itself:

\newpage

\begin{code}
data Integrator = Integrator { initial     :: Dynamics Double,
                               cache       :: IORef (Dynamics Double),
                               computation :: IORef (Dynamics Double)
                             }
\end{code}

Next, two other functions need to be adapted: \textit{newInteg} and \textit{readInteg}. In the former function, the new pointer will be used, and it points to the region where the mutable array will be allocated. In the latter, instead of reading from the computation itself, the read-only pointer will be looking at the \textbf{cached} version. These differences will be illustrated by using the same integrator and state variables used in the Lorenz's Attractor example, detailed in chapter 4, \textit{Execution Walkthrough}.

The main difference in the updated version of the \textit{newInteg} function is the inclusion of the new pointer that reads the cached memory (lines 4 to 7). The pointer \texttt{computation}, which will be changed by \textit{diffInteg} in a model to the differential equation, is being read in lines 8 to 10 and piped with interpolation and memoization in line 11. This approach maintains the interpolation, justified in the previous chapter, and adds the aforementioned caching strategy. Finally, the final result is written in the memory region pointed by the caching pointer (line 12).

Figure \ref{fig:newInteg} shows that the updated version of the \textit{newInteg} function is similar to the previous implementation. The new field, \texttt{cached}, is a pointer that refers to \texttt{readComp} --- the result of memoization (\texttt{memo}), interpolation (\texttt{interpolate}) and the value obtained by the region pointed by the \texttt{computation} pointer. Given a parametric record \texttt{ps}, \texttt{readComp} gives this record to the dynamic value stored in the region pointed by \texttt{computation}. This result is then interpolated via the \texttt{interpolate} block and it is used as a dependency for the \texttt{memo} block.

The modifications in the \textit{readInteg} function are being portrayed in Figure \ref{fig:readInteg}. As described earlier, the change is minor: instead of reading from the region pointed by the \texttt{computation} pointer, this function will read the value contained in the region pointed by the \texttt{cache} pointer (line 4). This means that the same \texttt{readComp}, described in the new \textit{newInteg} function, will receive a given \texttt{ps}. It is worth noticing that, just like with the \textit{newInteg} function, this cache pointer indirectly interacts with the same memory location pointed by the \texttt{computation} pointer in the integrator (Figure \ref{fig:readInteg}).

\begin{figure}[t!]
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
\caption{The new \textit{newInteg} function relies on interpolation composed with memoization. Also, this combination \textbf{produces} results from the computation located in a different memory region, the one pointed by the \texttt{computation} pointer in the integrator.}
\label{fig:newInteg}
\end{figure}

\clearpage

\begin{figure}[t!]
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
\caption{The function \textbf{reads} information from the caching pointer, rather than the pointer where the solvers compute the results.}
\label{fig:readInteg}
\end{figure}

Lastly, Figure \ref{fig:diffInteg} depicts the new version of the \textit{diffInteg} function. Further, the tweaks in this function are minor, just as with the \textit{readInteg} function. Previously, the \texttt{whatToDo} label, used as a dependency in the solver methods, was being made by reading the content in the region pointed by the \texttt{computation} pointer. Now, this dependency reads the region related to the caching methodology via reading the \texttt{cache} pointer.

\begin{figure}[t]
\begin{code}
diffInteg :: Integrator -> Dynamics Double -> Dynamics ()
diffInteg integ diff =
  do let z = Dynamics $ \ps ->
           do whatToDo <- readIORef (cache integ)
              let i = initial integ
              case method (solver ps) of
                Euler -> integEuler diff i whatToDo ps
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

\newpage

The solver functions, \textit{integEuler}, \textit{integRK2} and \textit{integRK4}, always need to calculate the value of the previous iteration. By giving them access to the cached region of the simulation, instead of starting a recursive chain of stack calls, the previous computation will be handled immediately. This is the key to cut orders of magnitude in execution time during simulation.

\section{A change in Perspective}

Before the implementation of the described caching strategy, \textbf{all} the solver methods rely on implicit recursion to get the previous iteration value. Thus, performance was degraded due to this potentially long stack call. After caching, this mechanism is not only faster, but it \textbf{completely} changes how the solvers will get these past values.

For instance, when using the function \textit{runDynamicsFinal} as the driver, the simulation will start by the last iteration. Without caching, the solver would go from the current iteration to the previous ones, until it reaches the base case with the initial condition and starts backtracking the recursive calls to compute the result of the final iteration. On the other hand, with the caching strategy, the \textit{memo} function goes in the \textbf{opposite} direction: it starts from the beginning, with the counters at zero, and then incrementally proceeds until it reaches the desired iteration.

Figure \ref{fig:memoDirection} depicts this stark difference in approach when using memoization in \texttt{Rivika}. Instead of iterating through all iterations two times, one backtracking until the base case and another one to accumulate all computed values, the new version starts from the base case, i.e., at iteration 0, and stops when achieves the desired iteration, saving all the values along the way.

\figuraBib{MemoDirection}{Caching changes the direction of walking through the iteration axis. It also removes an entire pass through the previous iterations}{}{fig:memoDirection}{width=.95\textwidth}%

\section{Tweak III: Model and Driver}

The memoization added to \texttt{Rivika} needs a second tweak, related to the executable models established in chapter 4. The code bellow is the same example model used in that chapter:

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

The caching strategy assumes that the created mutable array will be available for the entire simulation. However, the proposed models will \textbf{always} discard the table created by the \textit{newInteg} function due to the garbage collector~\footnote{Garbage Collector \href{https://wiki.haskell.org/GHC/Memory\_Management}{\textcolor{blue}{wiki page}}.}, after the \textit{sequence} function. Even worse, the table will be created again each time the model is being called and a parametric record is being provided, which happens when using the driver. Thus, the proposed solution to address this problem is to update the \texttt{Model} alias to  a \textbf{function} of the model. This can be achieved by \textbf{wrapping} the state vector with a the \texttt{Dynamics} type, i.e., wrapping the model using the function \textit{pure} or \textit{return}. In this manner, the computation will be "placed" as a side effect of the \texttt{IO} monad and Haskell's memory management system will not remove the table used for caching, in the first computation. So, the following code is the new type alias, alongside the previous example model using the \textit{return} function:

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

Due to the new type signature, this change implies changing the driver, i.e., modify the function \textit{runDynamics} (the changes are analogus to the \textit{runDynamicsFinal} function variant). Further, a new auxiliary function was created, \textit{subRunDynamics}, to separate the environment into two functions. The \textit{runDynamics} will execute the mapping with the function \textit{parameterise} and the auxiliary function will address the need for interpolation.

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
     if (iterToTime iv sl nu 0) - (stopTime iv) < 0.00001
     then map (m . parameterise) [nl .. nu]
     else (init $ map (m . parameterise) [nl .. nu]) ++ [m ps]     
\end{code}

The main change is the division of the driver into two: one dedicated to "initiate" the simulation environment providing an initial record of the type \texttt{Parameters} (lines 3 to 6), and an auxiliary function doing the mapping to the iteration axis (lines 10 to 14, 20 and 21), as well as checking for interpolation (lines 15 to 19). Thus, this is the final implementation of the driver in \texttt{Rivika}.

\section{Results with Caching}

The following table (Table \ref{tab:betterResults}) shows the same Lorenz's Attractor example used in the first section, but with the preceding tweaks in the \texttt{Integrator} type and the integrator functions. These modifications allows better and more complicated models to be simulated. For instance, the Lorenz example with a variety of total number of iterations can be checked in Table \ref{tab:masterResults} and in Figure \ref{fig:graph2}.

\begin{table}[H]
\centering
\begin{tabular}{cccc}
\hline
\noalign{\vskip 2mm} 
\shortstack{Total\\ of\\ Iterations} & \shortstack{Previous\\ Execution\\ Time (seconds)} & \shortstack{Execution\\ Time (seconds)} & \shortstack{Consumed\\ Memory (MB)} \\ \hline
1                   &    0.01                                    &0.00                     &  0.5  \\ \hline
2                   &    0.01                                    &0.00                     &  0.6  \\ \hline
3                   &    0.08                                    &0.00                     &  0.7  \\ \hline
4                   &    0.79                                    &0.00                     &  0.8  \\ \hline
5                   &    10.06                                   &0.00                     &  0.9  \\ \hline
6                   &    140.95                                  &0.01                     &  1.1  \\ \hline
7                   &    1798.16                                 &0.01                     &  1.2  \\ \hline
8                   &    23801.51                                &0.00                     &  1.3  \\ \hline
\end{tabular}
\caption{\label{tab:betterResults}These values were obtained using the same hardware. It shows that the caching strategy drastically improves \texttt{Rivika}'s performance. Again, the concrete memory values obtained from GHC should be considered as just an indicative of improvement due to the garbage collector interference.}
\end{table}

\begin{table}[H]
\centering
\begin{tabular}{ccc}
\hline
Total of Iterations & Execution Time (seconds) & Consumed Memory (MB) \\ \hline
100                    & 0.02                     &  1.5  \\ \hline
1K                     & 0.04                     &  11.8  \\ \hline
10K                    & 0.30                     &  114.7  \\ \hline
100K                   & 3.28                     &  1143.3  \\ \hline
1M                     & 29.91                    &  11429.3  \\ \hline
10M                    & 307.66                   &  114289.7  \\ \hline
100M                   & 3205.06                  &  1142893.0  \\ \hline
\end{tabular}
\caption{\label{tab:masterResults}These values were obtained using the same hardware. More complicated simulations can be done with \texttt{Rivika} after adding memoization.}
\end{table}

\figuraBib{Graph2}{By using a logarithmic scale, we can see that the final implementation is performant with more than 100 million iterations in the simulation}{}{fig:graph2}{width=.85\textwidth}%

This summarizes the chapter, closing the arc about addressing drawbacks. The project is currently capable of executing interpolation as well as applying memoization to speed up results. These two solutions, detailed in chapter 5 and 6, adds practicality to \texttt{Rivika} as well as makes it more competitive. The final chapter, \textit{Conclusion}, will conclude this work, pointing out limitations of the project, as well as future improvements and final thoughts about the project.
