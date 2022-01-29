\documentclass{article}

%include colorcode.fmt

\usepackage{minted}
\newminted[code]{haskell}{}

\usepackage{hyperref}

\begin{document}

\section{A DSL for simulating the Continuous}

This project is dedicated to explore the concepts of simulating the continuous, based on one of the earliest version of \href{https://github.com/dsorokin/aivika}{aivika}. In principle, the DSL works as the following example:

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

\subsection{Representing the Environment}

The next data records represent information to execute the simulation. The \texttt{Specs} data type carries the time interval of the simulation, along with the size of the integration step and which method will be used, based on the \texttt{Method} type:

\begin{code}
data Specs = Specs { startTime :: Double,  
                     stopTime  :: Double,  
                     dt        :: Double,  
                     method :: Method      
                   } deriving (Eq, Ord, Show)


data Method = Euler         
            | RungeKutta2   
            | RungeKutta4   
            deriving (Eq, Ord, Show)
\end{code}

Finally, the \texttt{Parameters} type saves which specifications will be used across the simulation. Additionally, because it also used in the integration procedure itself, it also carries which time is the current time of a given calculation, which iteration is the current one, and in which stage of the solver method it is in a given moment.

\begin{code}
data Parameters = Parameters { specs :: Specs,   
                               time :: Double,   
                               iteration :: Int, 
                               stage :: Int }    
                  deriving (Eq, Show)
\end{code}

\end{document}
