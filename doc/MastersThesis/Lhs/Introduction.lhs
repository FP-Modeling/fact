\ignore{
\begin{code}
module MastersThesis.Lhs.Introduction where
import MastersThesis.Lhs.Design
import MastersThesis.Lhs.Implementation
import MastersThesis.Lhs.Enlightenment
\end{code}
}

\section{Context}

Continuous behaviours are deeply embedded into the real world. However, even our most advanced computers are not capable of completely modeling such phenomena due to its discrete nature; thus becoming a still-unsolved challenge. Cyber-physical systems (CPS) --- the integration of computers and physical processes~\cite{LeeModeling, LeeChallenges} --- tackles this problem by attempting to include into the \textit{semantics} of computing the physical notion of \textit{time}~\cite{LeeChallenges, Lee2016, Lee2014, Ungureanu2018, Seyed2020, Edil2021}, i.e., treating time as a measurement of \textit{correctness}, not \textit{performance}~\cite{LeeModeling} nor just an accident of implementation~\cite{LeeChallenges}. Additionally, many systems perform in parallel, which requires precise and sensitive management of time; a non-achievable goal by using traditional computing abstractions, e.g., \textit{threads}~\cite{LeeChallenges}.

Examples of these concepts are older than the digital computers; analog computers were used to model battleships' fire systems and core functionalities of fly-by-wire aircraft~\cite{Graca2003}. The mechanical metrics involved in these problems change continuously, such as space, speed and area, e.g., the firing's range and velocity are crucial in fire systems, and surfaces of control are indispensable to model aircraft's flaps. The main goal of such models was, and still is, to abstract away the continuous facet of the scenario to the computer. In this manner, the human in the loop aspect only matters when interfacing with the computer, with all the heavy-lifting being done by formalized use of shafts and gears in analog machines~\cite{Shannon, Bush1931, Graca2003}, and by \textbf{software} after the digital era.

Within software, the aforementioned issues --- the lack of time semantics and the wrong tools for implementing concurrency --- are only a glimpse of serious concerns orbiting around CPS. The main villain is that today's computer science and engineering primarily focus on matching software demands, not expressing essential aspects of physical systems~\cite{LeeChallenges, LeeComponent}. Further, its sidekick is the weak formalism surrounding the semantics of model-based design tools; modeling languages whose semantics are defined by the tools rather than by the language itself~\cite{LeeComponent}, encouraging ad-hoc design practices, thus adding inertia into a dangerous legacy we want to avoid~\cite{Churchill1943}. With this in mind, Lee advocated that leveraging better formal abstractions is the paramount goal to advance continuous time modeling~\cite{LeeChallenges, LeeComponent}. More importantly, these new ideas need to embrace the physical world, taking into account predictability, reliability and interoperability.

The development of a \textit{model of computation} (MoC) to define and express models is the major hero towards this better set of abstractions, given that it provides clear, formal and well-defined semantics~\cite{LeeModeling} on how engineering artifacts should behave~\cite{Lee2016}. These MoCs determine how concurrency works in the model, choose which communication protocols will be used, define whether different components share the notion of time, as well as whether and how they share state~\cite{LeeModeling, LeeComponent}. Also, Sangiovanni and Lee~\cite{LeeSangiovanni} proposed a formalized denotational framework to allow understanding and comparison between mixtures of MoCs, thus solving the heterogeneity issue that raises naturally in many situations during design~\cite{LeeModeling, LeeComponent}. Moreover, their framework also describes how to compose different MoCs, along with addressing the absence of time in models, via what is defined as \textit{tagged systems}~\cite{Chupin2019, Perez2023, Rovers2011} --- a relationship between a \textit{tag}, generally used to order events, and an output value.

Ingo et al. went even further~\cite{Sander2017} by presenting an example of a framework based on the idea of tagged systems, known as \textit{ForSyDe}. The tool's main goal is to push system design to a higher level of abstraction, by combining MoCs with the functional programming paradigm. The technique separates the design into two phases, specification and synthesis. The former stage, specification, focus on creating a high-level abstraction model, in which mathematical formalism is taken into account. The latter part, synthesis, is responsible for applying design transformations --- the model is adapted to ForSyDe's semantics --- and mapping this result onto a chosen architecture for later be implemented in a target programming language or hardware platform~\cite{Sander2017}. Afterward, Seyed-Hosein and Ingo~\cite{Seyed2020} created a co-simulation architecture for multiple models based on ForSyDe's methodology, addressing heterogeneity across languages and tools with different semantics. One example of such tools treated in the reference is Simulink~\footnote{Simulink \href{http://www.mathworks.com/products/simulink/}{\textcolor{blue}{documentation}}.}, the de facto model-based design tool that lacks a formal semantics basis~\cite{Seyed2020}. Simulink being the standard tool for modeling means that, despite all the effort into utilizing a formal approach to model-based design, this is still an open problem.

\section{Proposal}

The aforementioned work --- the formal notion of MoCs, the ForSyDe framework and its interaction with modeling-related tools like Simulink --- comprises the domain of model-based design or \textbf{model-based engineering}. Furthermore, the main goal of the present work contribute to this area of CPS by creating a domain-specific language tool (DSL) for simulating continuous-time systems that addresses the absence of a formal basis. Thus, this tool will help to cope with the incompatibility of the mentioned sets of abstractions~\cite{LeeChallenges} --- the discreteness of digital computers with the continuous nature of physical phenomena.

The proposed DSL has three special properties of interest: it needs to be a set of well-defined \textit{operational} semantics, thus being \textbf{executable}; it needs to be related to a \textit{formalized} reasoning process; and it brings familiarity in its use to the \textit{system's designer} -- the pilot of the DSL which strives to execute a given specification or golden model. The first aspect provides \textbf{verification via simulation}, a type of verification that is useful when dealing with \textbf{non-preserving} semantic transformations, i.e., modifications and tweaks in the model that do not assure that properties are being preserved. Such phenomena are common within the engineering domain, given that a lot of refinement goes into the modeling process in which previous proof-proved properties are not guaranteed to be maintained after iterations with the model. A work-around solution for this problem would be to prove again that the features are in fact present in the new model; an impractical activity when models start to scale in size and complexity. Thus, by using an executable tool as a virtual workbench, models that suffered from those transformations could be extensively tested and verified.

In order to address the second property, a solid and formal foundation, the tool is inspired by the general-purpose analog computer (GPAC) formal guidelines, proposed by Shannon~\cite{Shannon} in 1941. This concept was developed to model a Differential Analyzer --- an analog computer composed by a set of interconnected gears and shafts intended to solve numerical problems~\cite{Graca2004}. The mechanical parts represents \textit{physical quantities} and their interaction results in solving differential equations, a common activity in engineering, physics and other branches of science~\cite{Shannon}. The model was based on a set of black boxes, so-called \textit{circuits} or \textit{analog units}, and a set of proved theorems that guarantees that the composition of these units are the minimum necessary to model the system, given some conditions. For instance, if a system is composed by a set of \textit{differentially algebraic} equations with prescribed initial conditions~\cite{Graca2003}, then a GPAC circuit can be built to model it. Later on, some extensions of the original GPAC were developed, going from solving unaddressed problems contained in the original scope of the model~\cite{Graca2003} all the way to make GPAC capable of expressing generable functions, Turing universality and hypertranscendental functions~\cite{Graca2004, Graca2016}. Furthermore, although the analog computer has been forgotten in favor of its digital counterpart~\cite{Graca2003}, recent studies in the development of hybrid systems~\cite{Edil2018} brought GPAC back to the spotlight in the CPS domain.

Finally, the third property of interest, the designer's familiarity between the mathematical specification and
the DSL's usability, will be assured by the use of the \textit{fixed-point combinator}; a mathematical construct used in the DSL's machineary to hide implementation details noise from the user's perspective, keeping on the surface only the constructs that matter from the designer's point of view. Hence, it is expected that one with less programming experience but familiar with the system's mathematical description will be able to leverage the DSL either when improving the system's description, using the DSL as a refinment tool, or as a way to execute an already specified system. The present work being a direct continuation~\cite{Lemos2022}, it is important to highlight that this final property is the differentiating factor between the two pieces.

With these three core properties in mind, the proposed DSL will translate GPAC's original set of black boxes to some executable software leveraging mathematical constructs to simplify its usability.

\section{Goal}

The main goal of the present work is to build an executable software that can solve differential equations and resembles the core idea of the GPAC model. The programming language of choice was \textbf{Haskell}, due to a variety of different reasons. First, the approach of making specialized programming languages, or \textit{vocabularies}, within consistent and well-defined host programming languages has already proven to be valuable, as noted by Landin~\cite{Landin1966}. Second, this strategy is already being used in the CPS domain in some degree, as showed by the ForSyDe framework~\cite{Sander2017, Seyed2020}. Third, Lee describes a lot of properties~\cite{LeeModeling} that matches the functional programming paradigm almost perfectly:

\begin{itemize}
 \item Prevent misconnected MoCs by using great interfaces in between $\Rightarrow$ Such interfaces can be built using Haskell's \textbf{strong type system}
 \item Enable composition of MoCs $\Rightarrow$ Composition is a first-class feature in functional programming languages
 \item It should be possible to conjoin a functional model with an implementation model $\Rightarrow$ Functions programming languages makes a clear the separation between the \textit{denotational} aspect of the program, i.e., its meaning, from the \textit{operational} functionality
 \item All too often the semantics emerge accidentally from the software implementation rather than being built-in from the start $\Rightarrow$ A denotative approach with no regard for implementation details is common in the functional paradigm
 \item The challenge is to define MoCs that are sufficiently expressive and have strong formal properties that enable systematic validation of designs and correct-by-construction synthesis of implementations $\Rightarrow$ Functional languages are commonly used for formal mathematical applications, such as proof of theorems and properties, as well as also being known for "correct-by-construction" approaches 
\end{itemize}

The recognition that the functional paradigm (FP) provides better well-defined, mathematical and rigourous abstractions has been by Backus~\cite{Backus1978} in his Turing Award lecture; where he argued that FP is the path to liberate computing from the limitations of the \textit{von Neumann style} when thinking about systems.
Thus, continuous time being specified in mathematical terms,  we believe that the use of functional programming for modeling continuous time is not a coincidence; properties that are established as fundamental to leverage better abstractions for CPS simulation seem to be within or better described in the functional programming paradigm. Furthermore, this implementation is based on \texttt{Aivika}~\footnote{\texttt{Aivika} \href{https://github.com/dsorokin/aivika}{\textcolor{blue}{source code}}.} --- an open source multi-method library for simulating a variety of paradigms, including partial support for physical dynamics, written in Haskell. Our version is modified for our needs, such as demonstrating similarities between the implementation and GPAC, shrinking some functionality in favor of focusing on continuous time modeling, and re-thinking the overall organization of the project for better understanding, alongside code refactoring using other Haskell's abstractions. So, this reduced and refactored version of \texttt{Aivika}, so-called \texttt{FACT}~\footnote{\texttt{FACT} \href{https://github.com/FP-Modeling/fact/releases/tag/3.0}{\textcolor{blue}{source code}}.}, will be a Haskell Embedded Domain-Specific Language (HEDSL) within the model-based engineering domain. The built DSL will explore Haskell's specific features and details, such as the type system and typeclasses, to solve differential equations. Figure \ref{fig:introExample} shows a side-by-side comparison between the original implementation of Lorenz Attractor in FACT, presented in~\cite{Lemos2022}, and its final form for the same physical system.

\begin{figure}[ht!]
  \begin{minipage}{0.45\linewidth}
% \vspace{-0.8cm}
    \begin{purespec}
        -- Original version of FACT
        lorenzModel = do
        integX <- createInteg 1.0
        integY <- createInteg 1.0
        integZ <- createInteg 1.0
        let x = readInteg integX
            y = readInteg integY
            z = readInteg integZ
            sigma = 10.0
            rho = 28.0
            beta = 8.0 / 3.0
        updateInteg integX (sigma * (y - x))
        updateInteg integY (x * (rho - z) - y)
        updateInteg integZ (x * y - beta * z)
        return $ sequence [x, y, z]
    \end{purespec}
  \end{minipage} \;\;\;\;\;\;\;\;\;\;\;\;\;\;\;\;
  \begin{minipage}{0.45\linewidth}
    \begin{purespec}
        -- Final version of FACT
        lorenzModel = mdo
          x <- integ (sigma * (y - x)) 1.0
          y <- integ (x * (rho - z) - y) 1.0
          z <- integ (x * y - beta * z) 1.0
          let sigma = 10.0
              rho = 28.0
              beta = 8.0 / 3.0
          return $ sequence [x, y, z]          
    \end{purespec}
  \end{minipage}
\caption{The translation between the world of software and the mathematical description of differential equations are explicit in the final version of \texttt{FACT}.}
\label{fig:introExample}
\end{figure}

\section{Outline}

Although written in Haskell, a high level programming language, \texttt{FACT} explores a mix of advanced abstractions with some direct memory manipulation --- usually associated with low level programming languages. Hence, the proposed software will be explained in multiple chapters, each one with a separate and concrete objective.

Chapter 2, \textit{Design Philosophy}, will explain basic Haskell concepts, such as the type system and different sorts of polymorphism, and it will bind them to numerical methods and GPAC's circuits. The next chapter, \textit{Effectful Integrals}, is dedicated to introduce GPAC's integrator representative in software, alongside further improvements in the overall modeling of physical systems. The next chapter, \textit{Execution Walkthrough}, will discuss how the proposed types aligns with mathematical definitions introduced in Chapter 2. Moreover, how to execute a simulation as well as a guided example are presented. At the end, some issues will be identified with the implementation at that point. Chapters 6 and 7, \textit{Travelling across Domains} and \textit{Caching the Speed Pill} respectively, address these concerns. The follow-up chapter and main contribution of this work, \textit{Fixing Recursion}, will introduce the fixed-point combinator, alongside its use in Haskell, to upgrade the so-far developed DSL, simplifying its design and usability. Finally, limitations, future improvements and final thoughts are drawn in chapter 8, \textit{Conclusion}.
