\ignore{
\begin{code}
module GraduationThesis.Lhs.Interpolation where
import GraduationThesis.Lhs.Design
\end{code}
}

The previous chapter presented issues with the current implementation. This chapter, \textit{Weakening Discreteness}, tackles the problem related to the size of the time step, and how it affects the results at the stop time of the simulation. After this chapter, only the second problem will remain to be addressed. This task will be accomplished by chapter 7, \textit{The Speed Pill: Memoization}.

\section{Accepting Computer's Limitations}

\section{Tweak I: Interpolation}

\begin{code}
interpolate :: Dynamics Double -> Dynamics Double
interpolate (Dynamics m) = 
  Dynamics $ \ps -> 
  if stage (solver ps) >= 0 then 
    m ps
  else 
    let iv = interval ps
        sl = solver ps
        t  = time ps
        st = dt sl
        x  = (t - startTime iv) / st
        n1 = max (floor x) (iterationLoBnd iv st)
        n2 = min (ceiling x) (iterationHiBnd iv st)
        t1 = iterToTime iv sl n1 0
        t2 = iterToTime iv sl n2 0
        z1 = m $ ps { time = t1,
                      iteration = n1,
                      solver = sl { stage = 0 } }
        z2 = m $ ps { time = t2,
                      iteration = n2,
                      solver = sl { stage = 0 } }
        r | t == t1   = z1
          | t == t2   = z2
          | otherwise = 
            do y1 <- z1
               y2 <- z2
               return $ y1 + (y2 - y1) * (t - t1) / (t2 - t1)
    in r
\end{code}
