\ignore{
\begin{code}
module GraduationThesis.Lhs.Introduction where
import GraduationThesis.Lhs.Implementation
\end{code}
}

As human tecnologies advance, we are interested in convering a broader spectrum of real world phenomena, including physical behaviours. However, current computers' discrete nature adds a limitation in modeling the continuous aspect of such phenomena; thus becoming a still-unsolved challenge. Cyber physical systems (CPS) --- the integration of computers and physical processes~\cite{LeeModeling, LeeChallenges} --- tackles this problem by attempting to include into the \textit{semantics} of computing~\cite{LeeChallenges} the physical notion of \textit{time}, i.e., treating time as a measurement of \textit{correctness}, not \textit{performance}~\cite{LeeModeling} or just an accident of implementation~\cite{LeeChallenges}. Additionally, many systems occur in parallel, requiring precise and sensitive management of time by the chosen abstractions; a non-achievable goal by using general-purpose computing abstractions, e.g., \textit{threads}~\cite{LeeChallenges}.

Examples of these concepts are older than the digital computer; analog computers were used to model the set of differential equations of battleships' fire system and core functionalities of fly-by-wire aircrafts. The mechanical metrics involved in these problems change continuously, such as space, speed and area, e.g., the firing's range and velocity are crucial in fire systems, and surfaces of control are indispensable to model aircraft's flaps. The main goal of such models was, and still is, to abstract away the continuous facet of the problem to the computer. In this manner, the human in the loop aspect only matters when interfacing with the computer, with all the heavy-lifting being done by formalized use of shafts and gears in analog machines~\cite{Shannon}, and by \textbf{software} after the digital era.

Within software, the aformentioned problems --- the lack of time semantics and wrong tools for implementing concurrency --- are only a glimpse of serious concerns orbiting around CPS. The fact that today's computing primarily focus on matching software demands and does not express essential aspects of physical systems is the main villan~\cite{LeeChallenges, LeeComponent}. Further, its sidekick is the weak formalism surrounding the semantics of model-based design tools; modeling languages whose semantics are defined by the tools rather than by the language itself~\cite{LeeComponent}, encouraging ad-hoc design practices. So, in~\cite{LeeChallenges, LeeComponent} it is establised that to leverage better formal abstractions, that embrace the physical world, taking into account predictability, reliability and interoperability, is a necessity in order to advance continuous modelling.

The development of a \textit{model of computation} (MoC) when building models is the major hero towards this goal, given that it provides clear, formal and well-defined semantics~\cite{LeeModeling}. These MoCs determin how concurrency works in the model, choose which communication protocols will be used, define whether different components share the notion of time, as well as whether and how they share state~\cite{LeeModeling, LeeComponent}. Also, in~\cite{LeeSangiovanni}, the authors proposed a formalized denotational framework to allow understanding and comparison between mixtures of MoCs, thus solving the heterogeneity issue that raises naturally in many situations during design~\cite{LeeModeling, LeeComponent}. Moreover, the framework also describes how to compose different MoCs, along with addressing the absence of time in models, via what is defined as \textit{tag systems} --- a relationship between a \textit{tag}, generally used to order events, and an output value.

To cope with the incompatibility of these sets of abstractions~\cite{LeeChallenges} --- the discreteness of digital computers with the continuous nature of physical phenomena --- is the main goal of the present work.

%END

%In~\cite{LeeModeling}, there are a few reasons to use Haskell:
%\begin{itemize}
%\item Preventing Misconnected Model Components + Great interface between different MoCs => Strong Type system help
%\item Enable composition of MoCs => Functional Composition
%\item It should be possible to conjoin a functional model with an implementation model => Referential Transparency
%\item All too often the semantics emerge accidentally from the software implementation rather than being built-in from the start => Denotative Approach
%\item The challenge is to define MoCs that are sufficiently expressive and have strong formal properties that enable systematic validation of designs and %correct-by-construction synthesis of implementations => Functional Languages
%\end{itemize}

\ignore{
\begin{code}
lorenzSpecs = Specs { startTime = 0,
                      stopTime  = 10,
                      dt        = 1,
                      method    = Euler
                    }

sigma = 10.0
rho = 28.0
beta = 8.0 / 3.0

lorenzModel :: Model [Double]
lorenzModel =
  do integX <- newInteg 1.0
     integY <- newInteg 1.0
     integZ <- newInteg 1.0
     let x = integValue integX
         y = integValue integY
         z = integValue integZ
     integDiff integX (sigma*(y-x))
     integDiff integY (x*(rho-z)-y)
     integDiff integZ (x*y-beta*z)
     return $ sequence [x,y,z]

executeLorenz =
  do ans <- runDynamicsFinal lorenzModel lorenzSpecs
     print ans
\end{code}
}
