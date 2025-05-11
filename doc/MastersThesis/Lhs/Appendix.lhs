\section{Literate Programming}

This dissertation made use of literate programming~\footnote{\href{https://en.wikipedia.org/wiki/Literate_programming}{\textcolor{blue}{Literate Programming}}.}, a concept
introduced by Donald Knuth~\cite{knuth1992}. Hence, this document can be executed using the same source files that the \texttt{PDF} is created

This process requires the following dependencies:
\begin{itemize}
\item \texttt{ghc} - minimum version \texttt{9.6.6}
\item \texttt{pdflatex} - minimum version \texttt{3.141592653-2.6-1.40.25}
\item \texttt{bibtex} - minimum version \texttt{0.99d}
\end{itemize}

The script located in \texttt{doc/literate.sh} is responsible to run all literate programming functionalities.
The available commands are (all of them need to run within the directory \texttt{doc}):
\begin{itemize}
\item \texttt{./literate colorful} - Generates the \texttt{PDF} named \texttt{thesisColorful} with the documentation with
colorful code.
\item \texttt{./literate gray} - Generates the \texttt{PDF} named \texttt{thesisGray} with the documentation with
verbatim code. An extra dependency, \texttt{lhs2Tex} is necessary to run this subcommand.
\item \texttt{./literate repl} - Enters the \texttt{ghc} REPL~\footnote{\href{https://en.wikipedia.org/wiki/Read-eval-print_loop}{\textcolor{blue}{Read–eval–print loop}}.}
with the code available for exploration.
\item \texttt{./literate compile} - Compiles an executable. Currently, the thesis is set to run the final version of \texttt{FACT} (\texttt{FFACT}) running
the latest iteration of the Lorenz Attractor example, time step of $0.01$ with the second-order Runge-Kutta method, with start time set to 0 and final time set to
100. All intermadiate values from the three state variables, $x$, $y$, and $z$, are displayed in \texttt{stdout}. Failures on commands for specific OSes, such as
commands for Windows when running in a Linux machine and vice-versa, should be ignored.
\end{itemize}

\section{FFACT's Manual}
\label{appendix:manual}

This is a concise and pragmatic manual on how to create and run simulations using \texttt{FFACT}. For a deeper and more detailed description
of the internals of the DSL, including a walkthrough via examples, please consult (and generate via \texttt{literate.sh}) either \texttt{GraduationThesis} (for \texttt{FACT}) or \texttt{MasterThesis} (for \texttt{FFACT}).

\subsection{Models}

A simulation model is defined using \texttt{mdo-notation}  (check
\href{https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/recursive_do.html}{recursive
do}) to describe a system of \textit{differential equations}. The current version of \texttt{FFACT} only supports 
continuous simulations, i.e., discrete or hybrid simulations are future work. Alongside the equations, one must provide \textit{initial conditions} for 
each individual equation, such as the following:

\begin{purespec}
lorenzModel :: Model [Double]
lorenzModel = mdo
   x <- integ (sigma * (y - x)) 1.0
   y <- integ (x * (rho - z) - y) 1.0
   z <- integ (x * y - beta * z) 1.0
   let sigma = 10.0
       rho = 28.0
       beta = 8.0 / 3.0
   return $ sequence [x, y, z]
\end{purespec}

In this example, \texttt{lorenzModel} will return the state variables of interest via a list, hence the model having the type \texttt{Model [Double]}.
Recursive monadic bindings are possible due to \texttt{mdo}, which makes the description of a model in code closer to its mathematical counterpart.

\subsection{Solver}

Solver-specific configurations, e.g., which numerical method should be used and with which \textit{time step}, which solver stage should it start with,
are configured \textit{separately} from the model and from executing a simulation. This sort of configuration details are set via a separate record, such as the following:

\begin{purespec}
lorenzSolver = Solver { dt = 1,
                        method = RungeKutta2,
                        stage = SolverStage 0
                      }
\end{purespec}

Available numerical methods:
\begin{itemize}
\item \texttt{Euler}
\item \texttt{RungeKutta2}
\item \texttt{RungeKutta4}
\end{itemize}

\subsection{Simulation}

A model and a record with solver's configuration are some of the \textit{arguments} to a \textit{driver function}. A driver function runs the simulation starting from 0 until
a provided timestamp (in seconds). Currently, \texttt{runCTFinal} outputs the final result of the system
at the provided final time and \texttt{runCT} outputs a \textit{list} of intermediate values from the start
until the provided final time spaced by the time step within the solver's configuration. 
The type signatures of these functions are the following (\texttt{Double} is the final time of choice):

\begin{purespec}
runCTFinal :: Model a -> Double -> Solver -> IO a
runCT :: Model a -> Double -> Solver -> IO [a]
\end{purespec}

\subsection{Interpolation}

Both \texttt{FACT} and \texttt{FFACT} use \textit{linear interpolation} to approximate results in requested timestamps that are not reachable via the chosen time step within
the solver's configuration. Driver functions automatically take care of detecting and running interpolations. 
The type signature of the provided interpolation function (and probably future extensions) is the following:

\begin{purespec}
interpolate :: CT Double -> CT Double
\end{purespec}

\subsection{Caching}

Both \texttt{FACT} and \texttt{FFACT} employ a \textit{memoization strategy} for caching, in order to speed up the simulation execution. Without this, simulations recompute previously
computed values multiple times, due to the recursive nature of the numerical methods available. A table is saved in memory with already calculated values, and lookups
are done instead of triggering a new computation.
The type signature of the provided memoization function (and probably future extensions) is the following:

\begin{purespec}
memo :: UMemo e => (CT e -> CT e) -> CT e -> CT (CT e)
\end{purespec}

The typeclass \texttt{UMemo} is provided custom typeclass.

\subsection{Example}

Lorenz Attractor complete example:

\begin{purespec}
lorenzModel :: Model [Double]
lorenzModel = mdo
   x <- integ (sigma * (y - x)) 1.0
   y <- integ (x * (rho - z) - y) 1.0
   z <- integ (x * y - beta * z) 1.0
   let sigma = 10.0
       rho = 28.0
       beta = 8.0 / 3.0
   return $ sequence [x, y, z]
   
lorenzSolver = Solver { dt = 1,
                        method = RungeKutta2,
                        stage = SolverStage 0
                      }
					  
lorenz = runCTFinal lorenzModel 100 lorenzSolver
\end{purespec}


