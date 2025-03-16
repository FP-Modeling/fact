
\section{Literate Programming}

This dissertation made use of literate programming~\footnote{\href{https://en.wikipedia.org/wiki/Literate_programming}{\textcolor{blue}{Literate Programming}}.}, a concept
introduced by Donald Knuth~\cite{knuth1992}. Hence, this thesis can be executed using the same source files that the \texttt{PDF} is created

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


