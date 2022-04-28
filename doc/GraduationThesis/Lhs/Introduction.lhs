\ignore{
\begin{code}
module GraduationThesis.Lhs.Introduction where
import GraduationThesis.Lhs.Design
import GraduationThesis.Lhs.Implementation
import GraduationThesis.Lhs.Enlightenment
\end{code}
}

\section{Context}

Continuous behaviours are deeply embedded into the real world. However, even our most advanced computers are not capable of completely modeling such phenomena due to its discrete nature; thus becoming a still-unsolved challenge. Cyber-physical systems (CPS) --- the integration of computers and physical processes~\cite{LeeModeling, LeeChallenges} --- tackles this problem by attempting to include into the \textit{semantics} of computing the physical notion of \textit{time}~\cite{LeeChallenges}, i.e., treating time as a measurement of \textit{correctness}, not \textit{performance}~\cite{LeeModeling} nor just an accident of implementation~\cite{LeeChallenges}. Additionally, many systems perform in parallel, which requires precise and sensitive management of time; a non-achievable goal by using traditional computing abstractions, e.g., \textit{threads}~\cite{LeeChallenges}.

Examples of these concepts are older than the digital computers; analog computers were used to model battleships' fire systems and core functionalities of fly-by-wire aircraft~\cite{Graca2003}. The mechanical metrics involved in these problems change continuously, such as space, speed and area, e.g., the firing's range and velocity are crucial in fire systems, and surfaces of control are indispensable to model aircraft's flaps. The main goal of such models was, and still is, to abstract away the continuous facet of the scenario to the computer. In this manner, the human in the loop aspect only matters when interfacing with the computer, with all the heavy-lifting being done by formalized use of shafts and gears in analog machines~\cite{Shannon}, and by \textbf{software} after the digital era.

Within software, the aforementioned issues --- the lack of time semantics and the wrong tools for implementing concurrency --- are only a glimpse of serious concerns orbiting around CPS. The main villain is that today's computer science and engineering primarily focus on matching software demands and does not express essential aspects of physical systems~\cite{LeeChallenges, LeeComponent}. Further, its sidekick is the weak formalism surrounding the semantics of model-based design tools; modeling languages whose semantics are defined by the tools rather than by the language itself~\cite{LeeComponent}, encouraging ad-hoc design practices. With this in mind, Lee advocated that leveraging better formal abstractions is the paramount goal to advance continuous time modeling~\cite{LeeChallenges, LeeComponent}. More importantly, these new ideas need to embrace the physical world, taking into account predictability, reliability and interoperability.

The development of a \textit{model of computation} (MoC) to define and express models is the major hero towards this better set of abstractions, given that it provides clear, formal and well-defined semantics~\cite{LeeModeling}. These MoCs determine how concurrency works in the model, choose which communication protocols will be used, define whether different components share the notion of time, as well as whether and how they share state~\cite{LeeModeling, LeeComponent}. Also, Sangiovanni and Lee~\cite{LeeSangiovanni} proposed a formalized denotational framework to allow understanding and comparison between mixtures of MoCs, thus solving the heterogeneity issue that raises naturally in many situations during design~\cite{LeeModeling, LeeComponent}. Moreover, their framework also describes how to compose different MoCs, along with addressing the absence of time in models, via what is defined as \textit{tagged systems} --- a relationship between a \textit{tag}, generally used to order events, and an output value.

Ingo et al. went even further~\cite{Sander2017} by presenting an example of a framework based on the idea of tagged systems, known as \textit{ForSyDe}. The tool's main goal is to push system design to a higher level of abstraction, by combining MoCs with the functional programming paradigm. The technique separates the design into two phases, specification and synthesis. The former stage, specification, focus on creating a high level abstraction model, in which mathematical formalism is taken into account. The latter part, synthesis, is responsible for applying design transformations --- the model is adapted to ForSyDe's semantics --- and mapping this result onto a chosen architecture for later be implemented in a target programming language or hardware platform~\cite{Sander2017}. Afterward, Seyed-Hosein and Ingo~\cite{Seyed2020} created a co-simulation architecture for multiple models based on ForSyDe's methodology, addressing heterogeneity across languages and tools with different semantics. One example of such tools treated in the reference Simulink~\footnote{http://www.mathworks.com/products/simulink/}, the de facto model-based design tool that lacks a formal semantics basis~\cite{Seyed2020}.

\section{Proposal}

The aforementioned work --- the formal notion of MoCs, the ForSyDe framework and its interaction with modeling-related tools like Simulink --- comprises the domain of model-based design or \textbf{model-based engineering}. Furthermore, the main goal of the present work contribute to this area of CPS by creating a domain-specific language tool (DSL) for simulating continuous-time systems that addresses the absence of a formal basis. Thus, this tool will help to cope with the incompatibility of the mentioned sets of abstractions~\cite{LeeChallenges} --- the discreteness of digital computers with the continuous nature of physical phenomena.

The proposed DSL has two special properties of interest: it needs to be a set of well-defined \textit{operational} semantics, thus being \textbf{executable}, and it needs to be related to a \textbf{formalized} reasoning process. The former aspect provides \textbf{verification via simulation}, a type of verification that is useful when dealing with \textbf{non-preserving} semantic transformations, i.e., modifications and tweaks in the model. Such phenomena are common within the engineering domain, given that a lot of refinement goes into the modeling process in which previous proof-proved properties are not guaranteed to be maintained after iterations with the model. A work-around solution for this problem would be to prove again that the features are in fact present in the new model; an impractical activity when models start to scale in size and complexity. Thus, by using an executable tool as a virtual workbench, models that suffered from those transformations could be extensively tested and verified.

In order to address the latter property, a solid and formal foundation, the tool is inspired by the general-purpose analog computer (GPAC) formal guidelines, proposed by Shannon~\cite{Shannon} in 1941. This concept was developed to model a Differential Analyzer --- an analog computer composed by a set of interconnected gears and shafts intended to solve numerical problems~\cite{Graca2004}. The mechanical parts used represents \textit{physical quantities} and their interaction results in solving differential equations, a common activity in engineering, physics and other branches of science~\cite{Shannon}. The model was based on a set of black boxes, so-called \textit{circuits} or \textit{analog units}, and a mathematical set of proved theorems that guarantees that the composition of these units are the minimum necessary to model the system, if it follows some conditions. For instance, if a system is composed by a set of \textit{differentially algebraic} equations with prescribed initial conditions~\cite{Graca2003}, then a GPAC circuit can be built to model it. Later on, some extensions of the original GPAC were developed, going from solving unaddressed problems contained in the original scope of the model~\cite{Graca2003} all the way to make GPAC capable of expressing generable functions, Turing universality and hypertranscendental functions~\cite{Graca2004, Graca2016}. Furthermore, although the analog computer has been forgotten in favor of its digital counterpart~\cite{Graca2003}, recent studies in the development of hybrid systems~\cite{Edil2018} brought GPAC back to the spotlight in the CPS domain.

With these two core properties in mind, the proposed DSL will attempt to translate the GPAC's original set of black boxes to some executable software.

\section{Goal}

The main goal of the present work is to build an executable software that can solve differential equations and resembles the core idea of the GPAC model. The programming language of choice was \textbf{Haskell}, due to a variety of different reasons. First, this is already being used in the CPS domain in some degree, as showed by the ForSyDe framework~\cite{Sander2017, Seyed2020}. Second, Lee describes a lot of properties~\cite{LeeModeling} that matches the functional programming paradigm almost perfectly:

\begin{itemize}
 \item Prevent misconnected MoCs by using great interfaces in between $\Rightarrow$ Such interfaces can be built by using Haskell's \textbf{strong type system}
 \item Enable composition of MoCs $\Rightarrow$ Composition is a robust feature in functional programming languages
 \item It should be possible to conjoin a functional model with an implementation model $\Rightarrow$ Functions programming languages makes the separation between the \textit{denotational} aspect of the program, i.e., its meaning, from the \textit{operational} aspect clear
 \item All too often the semantics emerge accidentally from the software implementation rather than being built-in from the start $\Rightarrow$ A denotative approach with no regard for implementation details is common in the functional paradigm
 \item The challenge is to define MoCs that are sufficiently expressive and have strong formal properties that enable systematic validation of designs and correct-by-construction synthesis of implementations $\Rightarrow$ Functional languages are commonly used for formal mathematical applications, such as proof of theorems and properties, as well as also being known for "correct-by-construction" approaches 
\end{itemize}

Thus, we believe that the use of functional programming for modeling continuous time is not a coincidence; properties that are established as fundamental to leverage better abstractions for CPS simulation seem to be within the functional programming paradigm. Furthermore, this implementation is heavily based on \texttt{Aivika}~\footnote{https://github.com/dsorokin/aivika} --- an open source multi-method library for simulating a variety of paradigms, including partial support for physical dynamics, written in Haskell. Our version is modified for our needs, such as demonstrating similarities between the implementation and GPAC, shrinking some functionality in favor of focusing on continuous time modeling, and re-thinking the overall organization of the project for better understanding. So, this reduced and refactored version of \texttt{Aivika}, so-called \texttt{Rivika}, will be a Haskell Embedded Domain-Specific Language (HEDSL) within the model-based engineering domain. So, the built DSL will explore Haskell's specific features and details, such as the type system and typeclasses, to solve differential equations. Figure \ref{fig:introExample} shows a side-by-side comparison between a physical system that follows the guidelines settled by an enhanced version of GPAC and a model created by the final version of \texttt{Rivika}.

\begin{figure}[ht!]
\begin{minipage}[t]{.65\textwidth}
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
\end{minipage}
\begin{minipage}[t]{.3\textwidth}
\ifdefined\iscolorful
  \vspace{-1.25cm}
\fi
\begin{center}
$$ \sigma = 10.0 $$
$$ \rho = 28.0 $$
$$ \beta = \frac{8.0}{3.0}$$
$$$$
$$$$
$$\frac{dx}{dt} = \sigma(y - x) $$
$$\frac{dy}{dt} = x(\rho - z) $$
$$\frac{dz}{dt} = xy - \beta z $$
\end{center}
\end{minipage}
\caption{The translation between the world of software and the mathematical description of differential equations are explicit in \texttt{Rivika}.}
\label{fig:introExample}
\end{figure}

\section{Outline}

Although written in Haskell, a high level programming language, \texttt{Rivika} explores a mix of advanced abstractions with some direct memory manipulation --- usually associated with low level programming languages. Hence, the proposed software will be explained in multiple chapters, each one with a separate and concrete objective.

Chapter 2, \textit{Design Philosophy}, will explain basic Haskell concepts, such as the type system and different sorts of polymorphism, and it will bind them to numerical methods and GPAC's circuits. The next chapter, \textit{Effectful Integrals}, is dedicated to introduce GPAC's integrator representative in software, alongside further improvements in the overall modeling of physical systems. The follow-up chapter, \textit{Execution Walkthrough}, will discuss how the proposed integrator aligns with unaddressed mathematical definitions from Chapter 2. Moreover, how to execute a simulation as well as a guided example are presented to leverage some intuition and practicality in \texttt{Rivika}. At the end, some issues will be identified with the implementation at that point. Chapters 5 and 6, \textit{Weakening Discreteness} and \textit{Caching the Speed Pill} respectively, address these concerns. Finally, future improvements and final thoughts are drawn in chapter 7, \textit{Conclusion}.
