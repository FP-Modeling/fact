\ignore{
\begin{code}
module GraduationThesis.Lhs.Introduction where
import GraduationThesis.Lhs.Design
import GraduationThesis.Lhs.Implementation
import GraduationThesis.Lhs.Enlightenment
\end{code}
}

Continuous behaviours are deeply embedded into the real world. However, even our most advanced tools, \textit{computers}, are not capable of completely modeling such phenomena due to its discrete nature; thus becoming a still-unsolved challenge. Cyber-physical systems (CPS) --- the integration of computers and physical processes~\cite{LeeModeling, LeeChallenges} --- tackles this problem by attempting to include into the \textit{semantics} of computing the physical notion of \textit{time}~\cite{LeeChallenges}, i.e., treating time as a measurement of \textit{correctness}, not \textit{performance}~\cite{LeeModeling} nor just an accident of implementation~\cite{LeeChallenges}. Additionally, many systems occur in parallel, which requires precise and sensitive management of time; a non-achievable goal by using general-purpose computing abstractions, e.g., \textit{threads}~\cite{LeeChallenges}.

Examples of these concepts are older than the digital computer; analog computers were used to model battleships' fire system and core functionalities of fly-by-wire aircraft. The mechanical metrics involved in these problems change continuously, such as space, speed and area, e.g., the firing's range and velocity are crucial in fire systems, and surfaces of control are indispensable to model aircraft's flaps. The main goal of such models was, and still is, to abstract away the continuous facet of the scenario to the computer. In this manner, the human in the loop aspect only matters when interfacing with the computer, with all the heavy-lifting being done by formalized use of shafts and gears in analog machines~\cite{Shannon}, and by \textbf{software} after the digital era.

Within software, the aforementioned issues --- the lack of time semantics and wrong tools for implementing concurrency --- are only a glimpse of serious concerns orbiting around CPS. The main villain is that today's computing primarily focuses on matching software demands and does not express essential aspects of physical systems~\cite{LeeChallenges, LeeComponent}. Further, its sidekick is the weak formalism surrounding the semantics of model-based design tools; modeling languages whose semantics are defined by the tools rather than by the language itself~\cite{LeeComponent}, encouraging ad-hoc design practices. With this in mind, Lee in~\cite{LeeChallenges, LeeComponent} advocated that leveraging better formal abstractions is the paramount goal to advance continuous time modeling. More importantly, these new ideas need to embrace the physical world, taking into account predictability, reliability and interoperability.

The development of a \textit{model of computation} (MoC) when building models is the major hero towards this goal, given that it provides clear, formal and well-defined semantics~\cite{LeeModeling}. These MoCs determine how concurrency works in the model, choose which communication protocols will be used, define whether different components share the notion of time, as well as whether and how they share state~\cite{LeeModeling, LeeComponent}. Also, Sangiovanni and Lee~\cite{LeeSangiovanni} proposed a formalized denotational framework to allow understanding and comparison between mixtures of MoCs, thus solving the heterogeneity issue that araises naturally in many situations during design~\cite{LeeModeling, LeeComponent}. Moreover, the framework also describes how to compose different MoCs, along with addressing the absence of time in models, via what is defined as \textit{tagged systems} --- a relationship between a \textit{tag}, generally used to order events, and an output value.

Sander and Ingo went even further~\cite{Sander2017}, by presenting an example of a tagged system known as \textit{ForSyDe}. The framework's main goal is to push system design to a higher level of abstraction, by combining MoCs with the functional programming paradigm. The technique separates the design into two phases, specification and systhesis. The former stage, specification, focus on creating a high level abstraction model, in which mathematical formalism is taken into account. The latter part, systhesis, is responsible for applying design transformations --- the model is adapted to ForSyDe's semantics --- and mapping this result onto a chosen architecture for later be implemented in a target programming language~\cite{Sander2017}. Afterward, Seyed-Hosein and Ingo~\cite{Seyed2020} created a co-simulation architecture for multiple models based on ForSyDe's methodology, addresing heterogeneity across languages and tools with different semantics. One example of such tools treated in the contribution is Simulink~\footnote{http://www.mathworks.com/products/simulink/}, the de facto stardard graphical model-based design tool that lacks a formal semantics basis~\cite{Seyed2020}.

The main goal of the present work is to provide an alternative tool for simulating continuous-time systems, similar to Simulink, but addresing the absence of a formal basis. Thus, this tool, by being a set of well-defined \textit{operational} semantics, will help to cope with the incompatibility of the already mentioned sets of abstractions~\cite{LeeChallenges} --- the discreteness of digital computers with the continuous nature of physical phenomena.


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
\begin{spec}
sigma = 10.0
rho = 28.0
beta = 8.0 / 3.0

lorenzModel :: Model [Double]
lorenzModel =
  do integX <- newInteg 1.0
     integY <- newInteg 1.0
     integZ <- newInteg 1.0
     let x = readInteg integX
         y = readInteg integY
         z = readInteg integZ
     diffInteg integX (sigma * (y - x))
     diffInteg integY (x * (rho - z) - y)
     diffInteg integZ (x * y - beta * z)
     return $ sequence [x, y, z]
\end{spec}
}
