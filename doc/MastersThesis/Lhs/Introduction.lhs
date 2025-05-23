\ignore{
\begin{code}
module MastersThesis.Lhs.Introduction where
import MastersThesis.Lhs.Design
import MastersThesis.Lhs.Implementation
import MastersThesis.Lhs.Enlightenment
\end{code}
}

Continuous behaviours are deeply embedded into the real world. However, even our most advanced computers are not capable of completely modeling such phenomena due to its discrete nature; thus continuing to be a challenge. Cyber-physical systems (CPS) --- the integration of computers and physical processes~\cite{LeeModeling, LeeChallenges} --- tackles this problem by attempting to include into the \textit{semantics} of computing the physical notion of \textit{time}~\cite{LeeChallenges, Lee2016, Lee2014, Ungureanu2018, Seyed2020, Edil2021}, i.e., treating time as a measurement of \textit{correctness}, not \textit{performance}~\cite{LeeModeling} nor just an accident of implementation~\cite{LeeChallenges}. Additionally, many systems perform in parallel, which requires precise and sensitive management of time; a non-achievable goal by using traditional computing abstractions, e.g., \textit{threads}~\cite{LeeChallenges}.

Examples of these concepts are older than the digital computers; analog computers were used to model battleships' fire systems and core functionalities of fly-by-wire aircraft~\cite{Graca2003}. The mechanical metrics involved in these problems change continuously, such as space, speed and area, e.g., the firing's range and velocity are crucial in fire systems, and surfaces of control are indispensable to model aircraft's flaps. The main goal of such models was, and still is, to abstract away the continuous facet of the scenario to the computer. In this manner, the human in the loop aspect only matters when interfacing with the computer, with all the heavy-lifting being done by formalized use of shafts and gears in analog machines~\cite{Shannon, Bush1931, Graca2003}, and by \textit{software} after the digital era.

Within software, the aforementioned issues --- the lack of time semantics and the wrong tools for implementing concurrency --- are only a glimpse of serious concerns orbiting around CPS. The main villain is that today's computer science and engineering primarily focus on matching software demands, not expressing essential aspects of physical systems~\cite{LeeChallenges, LeeComponent}. Further, its sidekick is the weak formalism surrounding the semantics of model-based design tools; modeling languages whose semantics are defined by the tools rather than by the language itself~\cite{LeeComponent}, encouraging ad-hoc design practices, thus adding inertia into a dangerous legacy we want to avoid~\cite{Churchill1943}. With this in mind, Lee advocated that leveraging better formal abstractions is the paramount goal to advance continuous time modeling~\cite{LeeChallenges, LeeComponent}. More importantly, these new ideas need to embrace the physical world, taking into account predictability, reliability and interoperability.

The development of a \textit{model of computation} (MoC) to define and express models is the major hero towards this better set of abstractions, given that it provides clear, formal and well-defined semantics~\cite{LeeModeling} on how engineering artifacts should behave~\cite{Lee2016}. These MoCs determine how concurrency works in the model, choose which communication protocols will be used, define whether different components share the notion of time, as well as whether and how they share state~\cite{LeeModeling, LeeComponent}. Also, Sangiovanni and Lee~\cite{LeeSangiovanni} proposed a formalized denotational framework to allow understanding and comparison between mixtures of MoCs, thus solving the heterogeneity issue that raises naturally in many situations during design~\cite{LeeModeling, LeeComponent}. Moreover, their framework also describes how to compose different MoCs, along with addressing the absence of time in models, via what is defined as \textit{tagged systems}~\cite{Chupin2019, Perez2023, Rovers2011} --- a relationship between a \textit{tag}, generally used to order events, and an output value.

Ingo et al. went even further~\cite{Sander2017} by presenting a framework based on the idea of tagged systems, known as \textit{ForSyDe}. The tool's main goal is to push system design to a higher level of abstraction, by combining MoCs with the functional programming paradigm. The technique separates the design into two phases, specification and synthesis. The former stage, specification, focus on creating a high-level abstraction model, in which mathematical formalism is taken into account. The latter part, synthesis, is responsible for applying design transformations --- the model is adapted to ForSyDe's semantics --- and mapping this result onto a chosen architecture to be implemented later in a target programming language or hardware platform~\cite{Sander2017}. Afterward, Seyed-Hosein and Ingo~\cite{Seyed2020} created a co-simulation architecture for multiple models based on ForSyDe's methodology, addressing heterogeneity across languages and tools with different semantics. One example of such tools treated in the reference is Simulink~\footnote{Simulink \href{http://www.mathworks.com/products/simulink/}{\textcolor{blue}{documentation}}.}, the de facto model-based design tool~\cite{Seyed2020}. Simulink being the standard tool for modeling means that, despite all the effort into utilizing a formal approach to model-based design, there is still room for improvement.

\section{Contribution}
\label{sec:intro}

The aforementioned works --- the formal notion of MoCs, the ForSyDe framework and its interaction with modeling-related tools like Simulink --- comprise the domain of model-based design or \textit{model-based engineering}. Furthermore, the main goal of this work is to contribute to this sub-area of CPS by creating a domain-specific language tool (DSL) for simulating continuous-time systems that addresses inspired by a mathematical model of computation. Thus, this tool will serve as the foundation to deal with the incompatibility of the mentioned sets of abstractions~\cite{LeeChallenges} --- the discreteness of digital computers with the continuous nature of physical phenomena.

The proposed DSL has three special properties of interest:

\begin{enumerate}
\item it needs to have well-defined \textit{operational} semantics, as well as being a piece of \textit{executable} software;
\item it needs to be related or inspired by a \textit{formal} model, moving past \textit{ad-hoc} implementations;
\item it should be \textit{concise}; its lack of noise will bring familiarity to the \textit{system's designer} --- the pilot of the DSL which strives to execute a given specification or golden model.
\end{enumerate}

\subsection{Executable Simulation}

By making an executable software capable of running continuous time simulations, \textit{verification via simulation} will be available --- a type of verification that is useful when dealing with \textit{non-preserving} semantic transformations, i.e., modifications and tweaks in the model that do not assure that properties are being preserved. Such phenomena are common within the engineering domain, given that a lot of refinement goes into the modeling process in which previous proof-proved properties are not guaranteed to be maintained after iterations with the model. A work-around solution for this problem would be to prove again that the features are in fact present in the new model; an impractical activity when models start to scale in size and complexity. Thus, by using an executable tool as a virtual workbench, models that suffered from those transformations could be extensively tested and verified.

Furthermore, this implementation is based on \texttt{Aivika}~\footnote{\texttt{Aivika} \href{https://github.com/dsorokin/aivika}{\textcolor{blue}{source code}}.} --- an open source multi-method library for simulating a variety of paradigms, including partial support for physical dynamics, written in Haskell. Our version is modified for our needs, such as demonstrating similarities between the implementation and GPAC, shrinking some functionality in favor of focusing on continuous time modeling, and re-thinking the overall organization of the project for better understanding, alongside code refactoring using other Haskell's abstractions. So, this reduced and refactored version of \texttt{Aivika}, so-called \texttt{FACT}~\footnote{\texttt{FACT} \href{https://github.com/FP-Modeling/fact/releases/tag/4.0}{\textcolor{blue}{source code}}.}, will be a Haskell Embedded Domain-Specific Language (HEDSL) within the model-based engineering domain. The built DSL will explore Haskell's specific features and details, such as the type system and typeclasses, to solve differential equations. Figure~\ref{fig:introExample} shows a side-by-side comparison between the original implementation of Lorenz Attractor in FACT, presented in~\cite{Lemos2022}, and its final form, so-called FFACT, for the same physical system.

\begin{figure}[ht!]
  \begin{minipage}{0.5\linewidth}
    \begin{purespec}
        -- FACT
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
  \begin{minipage}{0.49\linewidth}
    \begin{purespec}
        -- FFACT
        lorenzModel =
          mdo x <- integ (sigma * (y - x)) 1.0
              y <- integ (x * (rho - z) - y) 1.0
              z <- integ (x * y - beta * z) 1.0
              let sigma = 10.0
                  rho = 28.0
                  beta = 8.0 / 3.0
              return $ sequence [x, y, z]          
    \end{purespec}
  \end{minipage}
\caption{The translation between the world of software and the mathematical description of differential equations are more concise and explicit in \texttt{FFACT}.}
\label{fig:introExample}
\end{figure}

\subsection{GPAC: inspiration for a Formal Model}

This work and its artifact (a functional DSL to execute simulations) is a direct continuation of the work
made by Edil Medeiros et al.~\cite{Edil2018}. Their work tackled CPS-modeling via a DSL, which used the general-purpose
analog computer (GPAC), proposed by Shannon~\cite{Shannon} in 1941, as a guideline for a solid and formal foundation.

Hence, the tool we propose is also inspired by GPAC. This concept was developed to model a Differential Analyzer --- an analog computer composed by a set of interconnected gears and shafts intended to solve numerical problems~\cite{Graca2004}. The mechanical parts represents \textit{physical quantities} and their interaction results in solving differential equations, a common activity in engineering, physics and other branches of science~\cite{Shannon}. The model was based on a set of black boxes, so-called \textit{circuits} or \textit{analog units}, and a set of proved theorems that guarantees that the composition of these units are the minimum necessary to model the system, given some conditions. For instance, if a system is composed by a set of \textit{differentially algebraic} equations with prescribed initial conditions~\cite{Graca2003}, then a GPAC circuit can be built to model it. Later on, some extensions of the original GPAC were developed, going from solving unaddressed problems contained in the original scope of the model~\cite{Graca2003} all the way to make GPAC capable of expressing generable functions, Turing universality and hypertranscendental functions~\cite{Graca2004, Graca2016}. Furthermore, although the analog computer has been forgotten in favor of its digital counterpart~\cite{Graca2003}, recent studies in the development of hybrid systems~\cite{Edil2018} brought GPAC back to the spotlight in the CPS domain.

During the design of the DSL, parallels will establish some resemblance between GPAC's constructs and the implementation. With this strategy, all the mathematical formalism leveraged for analog computers will drive the implementation in the digital computer. However, it is worth noting that we do not formally prove this mapping
holds using dedicated tools, such as proof assistants or dependently-typed prograamming languages. GPAC serves as an initial specification in which
the generated artifact (executable models for simulation) attempts to follow. Although outside of the scope of this work, this pursue for a formal foundation
can be developed in the future.

With that in mind, the HEDSL will strive to translate GPAC's original set of black boxes to some executable software leveraging mathematical constructs to simplify its usability. The programming language of choice was \textit{Haskell} --- a well known language in the functional paradigm (FP). The recognition that such paradigm provides better well-defined, mathematical and rigourous abstractions has been proposed by Backus~\cite{Backus1978} in his Turing Award lecture; where he argued that FP is the path to liberate computing from the limitations of the \textit{von Neumann style} when thinking about systems. Thus, continuous time being specified in mathematical terms,  we believe that the use of functional programming for modeling continuous time is not a coincidence; properties that are established as fundamental to leverage better abstractions for CPS simulation seem to be within or better described in FP.
Lee describes a lot of properties~\cite{LeeModeling} that matches this programming
paradigm almost perfectly:

\begin{enumerate}
 \item Prevent misconnected MoCs by using great interfaces in between $\Rightarrow$ Such interfaces can be built using Haskell's \textit{strong type system}
 \item Enable composition of MoCs $\Rightarrow$ Composition is a first-class feature in functional programming languages
 \item It should be possible to conjoin a functional model with an implementation model $\Rightarrow$ Functions programming languages makes a clear the separation between the \textit{denotational} aspect of the program, i.e., its meaning, from the \textit{operational} functionality
 \item All too often the semantics emerge accidentally from the software implementation rather than being built-in from the start $\Rightarrow$ A denotative approach with no regard for implementation details is common in the functional paradigm
 \item The challenge is to define MoCs that are sufficiently expressive and have strong formal properties that enable systematic validation of designs and correct-by-construction synthesis of implementations $\Rightarrow$ Functional languages are commonly used for formal mathematical applications, such as proof of theorems and properties, as well as also being known for "correct-by-construction" approaches 
\end{enumerate}

In terms of the DSL being \textit{embedded} in Haskell, this approach of making specialized programming languages, or \textit{vocabularies}, within consistent and well-defined host programming languages, has already proven to be valuable, as noted by Landin~\cite{Landin1966}. Further, this strategy is already being used in the CPS domain in some degree, as showed by the ForSyDe framework~\cite{Sander2017, Seyed2020}.

\subsection{Expressiveness and Conciseness}

This dissertation being a step in a broader story, started in 2018 by Medeiros et al.~\cite{Edil2018}, one of the goals is to
improve on the identified limitations in the first proposed DSL, such as the lack or high difficulty on
expressing systems via explicit signal manipulation for arbitrary closed feedback loops. Later publications addressed this issue~\cite{Lemos2022, EdilLemos2023} whilst introducing or keeping known problems, such as noisy and overloaded notation when using the DSL (Figure~\ref{fig:comparison})-- a consequence of an \textit{abstraction leaking} -- and
not being able to model hybrid systems; systems with changes in continuous behavior based on discrete events.

\begin{figure}[ht!]
\begin{subfigure}{1\linewidth}
  \begin{minipage}{0.62\linewidth}
    \begin{purespec}
        sineModel = intCT rk4 0 0 p1
          where
            p1 = (constCT (-1) *** idCT) >>> multCT >>> integ
            integ = intCT rk4 0 1 loopBreaker
            loopBreaker = (idCT *** constCT 0) >>> adderCT
    \end{purespec}
  \end{minipage} \;\;\;\;\;\;\;\;\;\;\;\;\;\;\;\;
  \begin{minipage}{0.17\linewidth}
    \begin{equation*}
      \begin{cases} \dot{y}(t) = z(t) \\
        \dot{z}(t) = -y(t)
      \end{cases}
    \end{equation*}
  \end{minipage}
\end{subfigure}
\begin{subfigure}{1\linewidth}
  \vspace{1cm}
  \begin{minipage}{0.4\linewidth}
    \begin{purespec}
        sineModel =
          do integY <- createInteg 1
             integZ <- createInteg 0
             let y = readInteg integY
                 z = readInteg integZ
             updateInteg integY z
             updateInteg integZ (-y)
             return $ sequence [y, z]
    \end{purespec}
  \end{minipage} \;\;\;\;\;\;\;\;\;\;\;\;\;\;\;\;
  \begin{minipage}{0.59\linewidth}
      \centering
      \includegraphics[width=0.91\linewidth]{MastersThesis/img/sine_circuit.pdf}
  \end{minipage}
\end{subfigure}
\caption{Comparison between the original proposed DSL~\cite{Edil2018} and the first version of \texttt{FACT}~\cite{Lemos2022, EdilLemos2023}
using the same sine model, alongside its mathematical and GPAC descriptions.}
\label{fig:comparison}
\end{figure}

So, to address the aforementioned abstraction leaking and improve the DSL's conciseness, this work uses the \textit{fixed-point combinator}; a mathematical construct. The goal is to make the DSL's machinery hide implementation details noise from the user's perspective, keeping on the surface only the constructs that matter from the designer's point of view. Once the leak is solved, it is expected that the \textit{target audience} --- system's designers with less programming experience but familiar with the system's mathematical description --- will be able to leverage the DSL either when improving the system's description, using the DSL as a refinement tool, or as a way to execute an already specified system. Given that the present work, FFACT, being a direct continuation of FACT~\cite{Lemos2022}, it is important to highlight that this final property is the main differentiating factor between the two pieces.

When comparing models in \texttt{FFACT} to other implementations in other ecosystems and programming languages, \texttt{FFACT}'s conciseness brings more familiarity, i.e.,
one using the HEDSL needs less knowledge about the host programming language, Haskell in our case, \textit{and} one can more easily bridge the gap between a mathematical
description of the problem and its analogous written in \texttt{FFACT}, due to less syntatical burden and noise from a user's perpective. Examples and comparisons will be
depicted in Chapter 7, \textit{Fixing Recursion}, Section~\ref{sec:examples}.

\section{Outline}

Chapter 2, \textit{Design Philosophy}, presents the foundation of this work, started in 2018~\cite{Edil2018}. Although the artifacts presented in the
original work and this work are far apart, the mathematical base is the same. Chapters 3 to 6 describe future improvements made in 2022~\cite{Lemos2022} and
2023~\cite{EdilLemos2023}. These chapters go in detail about the DSL's implementation details, such as the used abstractions, going through executable examples,
pointing out and addressing problems in its usability and design. Issues like performance, and continuous time implementation are explained
and then addressed. Whilst the implementation of Chapters 2 to 6 were vastly improved during the making of this dissertation, alongside improvements
on the writing of their respective chapters,
the latest inclusion to this research is
concentrated in Chapter 7, \textit{Fixing Recursion}, which dedicates itself to improving an abstraction
leak in the most recent published version of the DSL~\cite{EdilLemos2023}. Those improvements leverage the \textit{fixed point combinator} to eliminate
abstraction leaks, thus making the DSL more concise and familiar to a system's designer.
These enhacements were submitted and are waiting approval in a related journal~\footnote{\href{https://www.cambridge.org/core/journals/journal-of-functional-programming}{\textcolor{blue}{Journal of Functional Programming}}.}. Finally, limitations, future improvements and final thoughts are drawn in Chapter 8, \textit{Conclusion}.
