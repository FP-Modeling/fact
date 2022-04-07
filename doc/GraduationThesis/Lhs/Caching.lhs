\ignore{
\begin{code}
module GraduationThesis.Lhs.Caching where
import GraduationThesis.Lhs.Design
import GraduationThesis.Lhs.Interpolation
import Data.IORef
import Data.Array
import Data.Array.IO

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
\end{code}
}

Chapter 5, \textit{Drawbacks}, leveraged two major concerns with the proposed software: depending too much on the size of the time step and speed making the software impractical. This chapter, \textit{The Speed Pill: Caching}, addresses the latter: the sluggishness issue in \texttt{Rivika}. At the end of it, the simulation will be orders of magnitude faster by using a common modern caching strategy to speed up computing processes: memoization.

\section{The Saving Strategy}

Before explaining the solution, it is worth describing \textbf{why} and \textbf{where} this problem araises. First, we took need to take a look back onto the solvers' functions, such as the \textit{integEuler} function, introduced in chapter 3, \textit{The Side-Effect Beast}:

\begin{spec}
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
\end{spec}

From chapter 3, we know that lines 10 to 13 serve the purpose of creating a new parameters record to execute a new solver step for the \textbf{previous} iteration, in order to calculate the current one. From chapter 4, this code section turned out to be where the implicit recursion came in, because the current iteration needs to calculate the previous one. Effectively, this means that for \textbf{all} iterations, \textbf{all} previous steps from each one needs to be calculated. The problem is now clear: unnecessary computations are being made for all iterations, because the same solvers steps are not being saved across future steps. In other words, to calculate step 3 of the solver, steps 1 and 2 are the same to calculate step 4 as well, but these values are being lost during the simulation.

To estimate how this lack of optimization affects performance, we can calculate how many solver steps will be executed to simulate the Lorenz's Attractor example, in the conditions used in chapter 4, \textit{Enlightement}, section 2, \textit{Our best friend: an Example}. The Table \ref{tab:solverSteps} shows the total number of solver steps needed per iteration in the Euler method. In addition, the amount of steps also increase depending on which solver method is being used, given that in the higher order Runge-Kutta methods, multiple stages count as a new step as well.

\begin{table}[H]
\centering
\begin{tabular}{|c|c|}
\hline
Iteration & Total Solver Steps \\ \hline
1         & 1            \\ \hline
2         & 3            \\ \hline
3         & 6            \\ \hline
4         & 10           \\ \hline
5         & 15           \\ \hline
6         & 21           \\ \hline
\end{tabular}
\caption{\label{tab:solverSteps}Because the previous solver steps are not saved, the total number of steps per iteration starts to accumullate following the numerical sequence of \textbf{triangular numbers} when using the Euler method.}
\end{table}

This is the cause of the imense hit in performance. However, it also clarified the solution: if the previous solver steps saved the next iterations don't need to re-compute then in order to continue. In the computer domain, the act of saving previous steps that do not change is called \textbf{memoization} and is one form to execute \textbf{caching}. This optimization technique stores the values in a register or memory region and, instead of the process starts calculating the result again, it consults this region to quickly obtain the answer. Thus, this improvement will be added to \texttt{Rivika} in the next section.

\section{Tweak II: Memoization}

The first tweak, \textit{Memoization}, alters the \texttt{Integrator} type. The integrator will now have a pointer to the memory region that stores the previous computed values, meaning that before executing a new computation, it will consult this region first. Because the process is executed in a \textbf{sequential} manner, it is guaranteed that the previous result will be used. Thus, the accumulation of the solver steps will be vanished, and the amount of steps will be equal to amount of iterations times how many stages the solver method uses.

The \textit{memo} function creates this memory region for storing values, as well as providing read access to it. Additionally, it receives the dynamic computation, as well as the interpolation function that is assumed to be used, in order to attenuate the discreteness problem described in the previous chapter, \textit{Weakening Discreteness}. This means that at the end, the final result needs to be interpolated by the provided function. 

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

The function starts by getting how many iterations will occur in the simulation, as well as how many stages the chosen method uses (lines 5 to 8). This is used to allocate the minimum amount of memory required for the execution (line 9). This mutable array is bidimentional and can be viewed as a table in which the number of iterations and stages determine the number of rows and columns. Pointers to iterate accross the table are declared as \textit{nref} and \textit{stref} (line 10 and 11), to read iteration and stage values respectively. The code block from line 12 to line 36 is used in a similar manner to what the function \textit{diffInteg}: this block is a procedure or computation that will only be used when needed (line 37). From line 13 to line 17, auxiliar "variables", i.e., labels to read information, are created to facilitate manipulation of the solver (\textit{sl}), interval (\textit{iv}), current iteration (\textit{n}), current stage (\textit{st}) and the final stage used in a solver step (\textit{stu}).

The definition of \textit{loop} that starts at line 18 and closes at line 33 is where the loop effectively starts, and it uses all the previously created labels for easier access of the dependencies. The conditional block (line 19 to 33) will store in the pre-allocated memory region the computed values and, because they are stored in a \textbf{sequential} way, the stop condition of the loop is one of the following: if the iteration counter of the loop surpassed the current iteration \textbf{or} the iteration counter matches the current iteration \textbf{and} the stage counter reached the ceiling of stages of used solver method (line 19). The loop stops by \textbf{reading} from the allocated array the value of interest, given that it is guaranteed that is already in memory. If this condition is not true, it means that further iterations in the loop need to occur in one of the two axis, iteration or stage.

The first step towards that goal is to save the value of the current iteration and stage into memory. The dynamic computation \textit{m}, received as a dependency in line 3, is used to compute a new result with the current counters for iteration and stage (lines 23 to 26). Then, this new value is written into the array (line 27). The condition in line 28 checks if the current stage already achieved its maximum possible value, meaning that the next iteration (line 30) from the first stage (line 29) will be te new counters and the loop should continue (line 31). Otherwise, we need to advance to the next stage within the same iteration (line 32); the loop should continue with the same iteration counter but with the stage counter incremented (lines 32 and 33).

Lines 34 to 36 are the trigger to the beginning of the loop, with \textit{nref} and \textit{stref} being read. In the beginning, both of their values will be zero (lines 10 and 11). All of these impure computations compose the definition of \textit{r} (line 12), which is being returned in line 37 combined with the interpolation function \textit{tr} and being wrapped into an extra \texttt{Dynamics} shell.

With this function on-hand, it remains to couple it to the \texttt{Integrator} type, meaning that \textbf{all} integrator function need to be aware of this new caching strategy. First and foremost, a pointer to this memory region needs to be added to the integrator type:

\begin{code}
data Integrator = Integrator { initial     :: Dynamics Double,
                               cache       :: IORef (Dynamics Double)
                               computation :: IORef (Dynamics Double)
                             }
\end{code}

Next, two other function need to be adapted: \textit{newInteg} and \textit{readInteg}. In the former function, the new pointer, \texttt{cache}, needs to be updated to the region where the mutable array will be allocated. In the latter, instead of reading from the computation itself, the readers will be looking at the \textbf{cached} version, always reading from memory. These differences will be illustrated by using the same integrator and readers from the Lorenz's Attractor example, detailed in chapter 4, \textit{Enlightnement}.

Figure \ref{fig:newInteg} shows that the updated version of the \textit{newInteg} function is similar to the previous implementation. The main difference is the inclusion of the new pointer that reads the cached memory (lines 4 to 7). The computation pointer, which will be changed after \textit{diffInteg} in a model, is being read in lines 8 to 10 and piped with interpolation and memoization. This approach maintains the interpolation, justified in the previous chapter, and adds the aforementioned caching strategy. Finally, the final result is the written in the memory region pointed by the caching pointer (line 13). As depicted in the figure, the memory pointed by the caching pointer interacts with the same region pointed by the computation pointer, meaning that the former pointer acts as a \textbf{internal reader} of the computation saved in memory.

The tweaked \textit{readInteg} function is portrayed in Figure \ref{fig:readInteg}. Although very similar to the implementation introduced in chapter 3, \textit{The Side Effect Beast}, the new caching methodology changes which memory region is being read. Previously, the dynamic computation was obtained by reading the memory region pointed by the computation pointer, and now the caching pointer is chosen. It is worth noticing that, just like with the \textit{newInteg} function, this pointer indirectly interacts with the procedure pointed by the computation pointer.

This summarizes the chapter, closing the arc about addressing drawbacks. The project is currently capable of executing interpolation as well as applying memoization to speed up results. These two solutions, detailed in chapter 6 and 7, adds practicality to \texttt{Rivika} as well as makes it more competitive with state of the art simulators. The final chapter, \textit{Conclusion}, will conclude this work, pointing out future improvements for project for better and easier understanding and showing semantic similarities of the software with other DSLs with focus on modeling physical phenomena.

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
\caption{The reader function now collects data from the cached version, rather than reading the computation's memory region directly.}
\label{fig:readInteg}
\end{figure}
