Our motivation was to mitigate the high difficulty of modeling
arbitrary closed feedback loops, using the DSL proposed by Medeiros et al.~\cite{Edil2018}. In their DSL, time-varying signals are abstracted by a function data type that updates the state of the system, and the topology of the system can only be described via a set of composition operators instead of dealing with the signals explicitly. In this work, we tackled this by proposing \texttt{FACT}: a reimplementation of the DSL based on a new implementation abstraction, called \texttt{CT}, whilst maintaining GPAC as the formal inspiration. This new data type holds the state of the system indirectly, thus allowing the user of the DSL to directly manipulate the signals when describing a system of equations. The guiding example used throughout this work, the Lorenz Attractor in Figure~\ref{fig:introExample}, is an example of a system with feedback loops that the former DSL could not express. Despite solving this expressivenness problem, \texttt{FACT} introduced an abstraction leaking, exposing to the users of the DSL internal implementation details. We solved this issue leveraging the monadic fixed-point combinator, resulting \texttt{FFACT} and thus improving the notation and usability. 

Chapter 2 established the foundation of the implementation, introducing
functional programming (FP) concepts and the necessary types
to model continuous time simulation --- with continuous time machines (\texttt{CT}) being the main type. Chapter 3 extended its power via
the implementation of typeclasses to add functionality for the \texttt{CT} type, such as binary operations and
numerical representation. Further, it also introduced the \texttt{Integrator}, a CRUD-like interface
for it, as well as the available numerical methods for simulation.
As a follow-up, Chapter 4 raised intuition and practical understanding of \texttt{FACT} via a detailed walkthrough of an example.
Chapter 5 explained and fixed the mix between different domains in the simulation, e.g., continuous time, discrete time and iterations,
via an additional linear interpolation when executing a model. Chapter 6 addressed performance concerns via a memoization strategy. Finally,
Chapter 7 introduced the fixed-point combinator and its monadic counterpart in order to increase conciseness of the HEDSL, bringing more familiarity to systems designers
experienced with the mathematical descriptions of their systems of interest. This notation enhancement is the defining feature between FACT and FFACT.

\section{Future Work}

The following subsections describe the three main areas for future improvements in \texttt{FFACT}: formalism, possible extensions, and code refactoring. 

\subsection{Formalism}

One of the main concerns is the \textit{correctness} of \texttt{FACT} between its specification and its final implementation, i.e., refinement. Shannon's GPAC concept acted as the specification of the project, whilst the proposed software attempted to implement it. The criteria used to verify that the software fulfilled its goal were by using it for simulation and via code inspection, both of which are based on human analysis. This connection, however, was \textit{not} formally verified --- no model checking tools were used for its validation. In order to know that the mathematical description of the problem is being correctly mapped onto a model representation some formal work needs to be done. This was not explored, and it was considered out of the scope for this work.

This lack of formalism extends to the typeclasses as well. The programming language of choice, Haskell, does \textit{not} provide any proofs that the created types actually follow the typeclasses' properties --- something that can be achieved with \textit{dependently typed} languages and/or tools such as Rocq, PVS, Agda, Idris and Lean. In Haskell, this burden is on the developer to manually write down such proofs, a non-explored aspect of this work. Hence, this work can be better understood as a \textit{proof of concept} for FFACT, and one potential improvement would be to port it to more powerful and specialized programming languages, such as the ones mentioned earlier. Because FP is highly encouraged in those languages, such port would not be a major roadblock. Thus, these tools would assure a solid mappping between the mathematical the description of the problem, GPAC's specification and FFACT's implementation, including the
use of the chosen typeclasses.

\subsection{Extensions}

As explained in Chapters 1 and 2, there are some extensions that increase the capabilities of Shannon's original GPAC model. One of these extensions, FF-GPAC, was the one chosen to be modeled via software. However, there are other extensions that not only expand the types of functions that can be modeled, e.g., hypertranscendental functions, but also explore new properties, such as Turing universality~\cite{Graca2004, Graca2016}. The proposed software didn't touch on those enhancements and restricted the set of functions to only algebraic functions. More recent extensions of GPAC should also be explored to simulate an even broader set of functions present in the continuous time domain.

In regards to numerical methods, one of the immediate improvements would be to use \textit{adaptive} size for the solver time step that \textit{change dynamically} in run time. This strategy controls the errors accumulated when using the derivative by adapting the size of the time step. Hence, it starts backtracking previous steps with smaller time steps until some error threshold is satisfied, thus providing finer and granular control to the numerical methods, coping with approximation errors due to larger time steps.

\subsection{Refactoring}

In terms of the used technology, some ideas come to mind related to abstracting out duplicated \textit{patterns} across the code base. The proposed software used a mix of high level abstractions, such as algebraic types and typeclasses, with some low level abstractions, e.g., explicit memory manipulation. One potential improvement would be to explore an entirely \textit{pure} based approach, meaning that all the necessary side effects would be handled \textit{only} by high-level concepts internally, hence decreasing complexity of the software. For instance, the memory allocated via the \texttt{memo} function acts as a \textit{state} of the numerical solver. Other Haskell abstractions, such as the \texttt{ST} monad~\footnote{\texttt{ST} Monad \href{https://wiki.haskell.org/State\_Monad}{\textcolor{blue}{wiki page}}.}, could be considered for future improvements towards purity. Going even further, given that \texttt{FACT}
already uses \texttt{ReaderT}, a combination of monads could be used to better unify all different behavior -- in Haskell, an option would be to use \textit{monad transformers}.
For instance, if the reader and state monads, something like the \texttt{RWS} monad~\footnote{\texttt{RWS} Monad \href{https://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-RWS-Lazy.html}{\textcolor{blue}{hackage documentation}}.}, a monad that combines the monads \texttt{Reader}, \texttt{Writer} and \texttt{ST}, may be the final goal for a completely pure but effective solution.

Also, there's GPAC and its mapping to Haskell features. As explained previously, some basic units of GPAC are being modeled by the \texttt{Num} typeclass, present in Haskell's \texttt{Prelude} module. By using more specific and customized numerical typeclasses~\footnote{Examples of \href{https://guide.aelve.com/haskell/alternative-preludes-zr69k1hc}{\textcolor{blue}{alternative preludes}}.}, it might be possible to better express these basic units and take advantage of better performance and convenience that these alternatives provide.

