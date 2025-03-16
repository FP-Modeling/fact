\ignore{
\begin{code}
{-# LANGUAGE RecursiveDo #-}
module MastersThesis.Lhs.Fixing where
import MastersThesis.Lhs.Implementation
import MastersThesis.Lhs.Interpolation
import MastersThesis.Lhs.Design
import MastersThesis.Lhs.Caching
import MastersThesis.Lhs.Enlightenment

import Control.Monad.Trans.Reader
import Control.Monad
import Control.Monad.Fix
\end{code}
}

The last improvement for FACT is in terms of \textit{familiarity}. When someone is using the DSL, so-called \textbf{designer} of the system,
the main goal should be that the least amount of friction when using the simulation software, the better.
Hence, the requirement of knowing implementation details or programming language details is something we would like to avoid, given that
it leaks noise into the designer's mind. The designer's concern should be to pay attention to the system's description and FACT having an extra
step of translation or noisy setups just adds an extra burden with no real gains on the engineering of simulating continuous time. This chapter
will present \textit{FFACT}, an evolution of FACT which aims to reduce the noise even further.

It is worth noting that the term \textit{fixed-point} has different meanings in the domains of engineering and mathematics. When referecing the
fractional representations within a computer, one may use the \textit{fixed-point method}. Thus, to avoid confusion, section~\ref{subsec:fix} starts
by defining the term as a mathematical combinator that can be used to implement recursion.

\section{Integrator's Noise}

Chapter 4, \textit{Execution Walkthrough}, described the semantics and usability on an example of a system in mathematical specification
and its mapping to a simulation-ready description provided by FACT.
Below we have this example modeled using FACT (same code as provided in section~\ref{sec:intro}):
%
\vspace{0.1cm}
\begin{spec}
sigma = 10.0
rho = 28.0
beta = 8.0 / 3.0

lorenzModel :: Model Vector
lorenzModel =
  do integX <- createInteg 1.0
     integY <- createInteg 1.0
     integZ <- createInteg 1.0
     let x = readInteg integX
         y = readInteg integY
         z = readInteg integZ
     updateInteg integX (sigma * (y - x))
     updateInteg integY (x * (rho - z) - y)
     updateInteg integZ (x * y - beta * z)
     return $ sequence [x, y, z]
\end{spec}
\vspace{0.1cm}
% $

It is noticeable, however, that FACT imposes a significant amount of overhead from the user's perspective due to the \textbf{explicit use of integrators} for most memory-required simulations.
When creating stateful circuits, an user of FACT is obligated to use the integrator's API, i.e., use the functions \texttt{createInteg} (lines 6 to 8), \texttt{readInteg} (lines 9 to 11), and \texttt{updateInteg} (lines 12 to 14). Although these functions remove the
management of the aforementioned implicit mutual recursion mentioned in chapter 3, \textit{Effectful Integrals}, from the user, it is still required to follow
a specific sequence of steps to complete a model for any simulation:
%
\begin{enumerate}
\item Create Integrators for future use setting initial conditions (via the use of \textit{createInteg});
\item Retrieve access to state variables by reading integrators (via the use of \textit{readInteg});
\item Update integrators with the actual ODEs of interest (via the use of \textit{updateInteg}).
\end{enumerate}

Visually, this step-by-step list for FACT's models follow the pattern detailed in Figure~\ref{fig:modelPipe} in chapter 4, \textit{Execution Walkthrough}.
More importantly, \emph{all} those steps are visible and transparent from an usability's point of view.
Hence, a system's designer \emph{must} be aware of this \emph{entire} sequence of mandatory steps, even if his interest probably only relates to lines 12 to 14.
Although one's goal is being able to specify a system and start a simulation, there is no escape -- one has to bear the noise created due to
the implementation details of the DSL.
In fact, this \emph{abtraction leak} of exposing operational semantics is a major obstacle that keeps FACT away from one of its goals:
to provide a direct map between the mathematical description of the system and its software counterpart.

To address this, FACT was upgraded to FFACT: a \emph{fixed-point} based version of FACT. FFACT leverages the \emph{fixed-point combinator} from the
realm of mathematics to significantly reduce the surface noise when using the DSL. This, combined with Haskell's \emph{laziness}, is the
required piece to get rid of the \texttt{Integrator} type, thus also removing its noise.

\section{The Fixed-Point Combinator}
\label{subsec:fix}

On the surface, the fixed-point combinator is a simple mapping that fulfills the following property:
a point \emph{p} is a fixed-point of a function \emph{f} if \emph{f(p)} lies on the identity function, i.e., \emph{f(p) = p}.
Not all functions have fixed-points, and some functions may have more than one~\cite{tennent1991}.
Further, we seek to establish theorems and algorithms in which one can guarantees fixed-points and their uniqueness, such as the Banach fixed-point theorem~\cite{bryant1985}.
In programming terms, by following specific requirements one could find the fixed-point of a function via an iterative process
that involves going back and forth between it and the identity function until the difference in outcomes is less than or equal to an arbitrary~$\epsilon$.

%
% \begin{figure}[ht!]
% \centering
% \includegraphics[width=0.65\columnwidth]{../figs/fix-point-graph}
% \caption{Example of iteration process converging to fixed-point}
% \label{fig:fixpointgraph}
% \end{figure}
%
Of particular interest is to find \emph{the least fixed-point}, a mathematical construct useful when describing the denotational semantics of recursive definitions~\cite{denotational1977, tennent1991}.
Within mathematics, you can find fixed-points in domains such as lattices~\cite{denotational1977}, metric spaces~\cite{bryant1985}, lambda calculus, among others areas that study convergence and stability of processes.

In the Haskell programming language, the fixed-point combinator is under the \texttt{Data.Function}~\footnote{\texttt{Data.Function} \href{https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-Function.html}{\textcolor{blue}{hackage documentation}}.} package with the following implementation:
%
\vspace{-0.1cm}
\begin{purespec}
  fix :: (a -> a) -> a
  fix f = let x = f x in x
\end{purespec}
\vspace{-0.1cm}
%
This function allows the definition of recursive functions without the use of self-reference, such as:
%
\vspace{-0.1cm}
\begin{purespec}
  factorial :: Int -> Int
  factorial = fix (\f n -> if n == 1 then 1 else n * f (n - 1))
\end{purespec}
\vspace{-0.1cm}

For readers unfamiliar with the use of this combinator, equational reasoning~\cite{tennent1991} can help understanding its meaning.
%
\begin{lstlisting}[basicstyle=\footnotesize]
  factorial 5
= {definition of factorial, alpha equivalence to remove clashes on f}
  fix (\g n -> if n == 1 then 1 else n * g (n - 1)) 5
= {definition of fix}
  (\f -> f (f (f (...)))) (\g n -> if n == 1 then 1 else n * g (n - 1)) 5
= {function application, f = (\g n -> if n == 1 then 1 else n * g (n - 1))}
  (\g n -> if n == 1 then 1 else n * g (n - 1)) (f (f (...))) 5
= {function application, g = (f (f (...)))}
  (\n -> if n == 1 then 1 else n * f (f (...)) (n - 1)) 5
= {function application, n = 5}
  if 5 == 1 then 1 else 5 * f (f (...)) 4
= {replace f with its binding}
  if 5 == 1 then 1
  else 5 * ((\g n -> if n == 1 then 1 else n * g (n - 1)) (f (...))) 4
= {function application, g = (f (...))}
  if 5 == 1 then 1
  else 5 * ((\n -> if n == 1 then 1 else n * f (...)) (n - 1)) 4
= {function application, n = 4}
  if 5 == 1 then 1
  else 5 * (if 4 == 1 then 1 else 4 * f (...) 3)
...
\end{lstlisting}

The result of this process will yield the factorial of 5, i.e., 120.
When using \texttt{fix} to define recursive processes, the function being \emph{applied} to it must be the one defining the convergence criteria for the iterative process of looking for the fixed-point.
In our factorial case, this is done via the conditional check at the beginning of body of the lambda.
The fixed point combinator's responsibility is to keep the \emph{repetition} process going -- something that may diverge and run out of computer resources.

Furthermore, this process can be used in conjunction with monadic operations as identified by Levent~\cite{leventThesis}:
%
\vspace{-0.1cm}
\begin{purespec}
  countDown :: Int -> IO ()
  countDown = fix (\f n -> if n == 0
                           then print "Done!"
                           else do print n
                           f (n - 1))
\end{purespec}
\vspace{-0.1cm}
%
This combination, however, cannot address \emph{all} cases when using side-effects.
In the above, executing the side-effect in \texttt{countDown} do not contribute to its own \emph{definition}.
There is no construct or variable that requires the side-effect to be executed in order to determine its meaning.
This ability -- being able to set values based on the result of running side-effects whilst keep the fixed-point running -- is something of interest because, as we are about to see, this allows the use of \emph{cyclic} definitions.

\section{Value Recursion with Fixed-Points}
\label{sec:value-recursion}

Consider the following block diagram as the representation of a resettable circuit in hardware:
\figuraBib{binary-counter}{Resettable counter in hardware, inspired by Levent's works~\cite{levent2000, levent2002}}{}{fig:binary-counter}{width=.75\textwidth}%

When attempting to model the circuit in Figure~\ref{fig:binary-counter} in programming languages other than specific hardware description languages (e.g. Verilog or VHDL), a very natural first draft of the implementation may look like:
%
\newpage
\begin{purespec}
counter :: Signal Bool -> Circuit (Signal Int)
counter reset = do next <- delay 0 inc
                   inc <- lift1 (+1) out
                   out <- mux reset zero next
                   zero <- lift0 0
\end{purespec}

This example, although idiomatic with the visual representation, does not work in its current state.
This is due to the presence of \emph{cyclic definitions} within the \texttt{do}-block, which are not allowed by Haskell's desugaring rules.
This implementation detail forbid us from describing the implementation of feedback-loops in this natural way, even though it better represents the problem on-hand.

Effectively, we wish to have the flexibility of \emph{letrec}~\cite{leventThesis, report5Scheme}, in which bindings can be defined with variables out of scope when reading sequentially.
By allowing this behavior, mutually recursive bindings are made possible and thus more natural implementations are available.
Haskell's vanilla \texttt{let} already acts like a \texttt{letrec}, and it would be useful to replicate this property to monadic bindings as well.

In the case of the \texttt{counter} example, the execution of a side-effect is mandatory to evaluate the values of the bindings, such as \texttt{next}, \texttt{inc}, \texttt{out}, and \texttt{zero} (lines 2 to 5).
In contrast, the example \texttt{countDown} in section~\ref{subsec:fix} has none of its bindings locked by side-effects, e.g, the bindings \texttt{f} and \texttt{n} have nothing to do with the effect of printing a message on \texttt{stdout}.
When dealing with the latter of these cases, the usual fixed-point combinator is enough to model its recursion.
The former case, however, needs a special kind of recursion, so-called \emph{value recursion}~\cite{leventThesis}.

As we are about to understand on Section~\ref{sec:ffact}, the use of value recursion to have monadic's bindings with the same convenience of \texttt{letrec} will be the key to our improvement on FFACT over FACT.
Fundamentally, it will \emph{tie the recursion knot} done in FACT via the complicated implicit recursion mentioned in Section~\ref{sec:integrator}.
In terms of implementation, this is being achieved by the use of the \texttt{mfix} construct~\cite{levent2000}, which is accompained by a \emph{recursive do} syntax sugar~\cite{levent2002}, with the caveat of not being able to do shadowing -- much like the \texttt{let} and \texttt{where} clauses in Haskell.
In order for a type to be able to use this construct, it should follow specific algebraic laws~\cite{leventThesis} to then implement the \texttt{MonadFix} type class found in \texttt{Control.Monad.Fix}~\footnote{\texttt{Control.Monad.Fix} \href{https://hackage.haskell.org/package/base-4.21.0.0/docs/Control-Monad-Fix.html}{\textcolor{blue}{hackage documentation}}.} package:
%
%% \vspace{-0.8cm}
\begin{purespec}
  class (Monad m) => MonadFix m where
    mfix :: (a -> m a) -> m a
\end{purespec}
\vspace{0.1cm}

\begin{purespec}
createInteg :: CT Double -> CT Integrator
createInteg i = do
  r1 <- liftIO . newIORef $ initialize i
  r2 <- liftIO . newIORef $ initialize i
  let integ = Integrator { initial = i,
                           cache = r1,
                           computation  = r2 }
      z = do
        ps <- ask
        v <- liftIO $ readIORef (computation integ)
        local (const ps) v
  y <- memo interpolate z
  liftIO $ writeIORef (cache integ) y
  return integ

readInteg :: Integrator -> CT Double
readInteg = join . liftIO . readIORef . cache

updateInteg :: Integrator -> CT Double -> CT ()
updateInteg integ diff = do
  let i = initial integ
      z = do
        ps <- ask
        let f =
              case (method $ solver ps) of
                Euler -> integEuler
                RungeKutta2 -> integRK2
                RungeKutta4 -> integRK4
        y <- liftIO $ readIORef (cache integ)
        f diff i y
  liftIO $ writeIORef (computation integ) z
\end{purespec}

\figuraBib{createInteg}{Diagram of \texttt{createInteg} primitive for intuition.}{}{fig:createIntegDiagram}{width=.97\textwidth}%

\section{Tweak IV: Fixing FACT}
\label{sec:ffact}

% Let's first revisit FACT's original implementation over its integrator primitives, e.g., \texttt{createInteg}, \texttt{readInteg}, and \texttt{updateInteg}.
% Instead of relying solely on their implementation, we will strive for Figure~\ref{fig:createIntegDiagram} as the main guide for grasping
% the aforementioned implicit recursion.
% With that in mind, the solution using the monadic fixed-point combinator, will more naturally fill its place.

The primitive \texttt{createInteg} is the one that first establishes what we are calling implicit recursion.
This scheme can be better perceived by Figure~\ref{fig:createIntegDiagram}.
The mutable references \texttt{cache} and \texttt{computation} target memory regions that \emph{reference each other}.
When creating an integrator, the field \texttt{computation} references a memory region that holds a continuous machine which yields the initial value for the ODE.
Later on, the primitive \texttt{updateInteg} will mutate this region to use the proper differential equation of interest.
On the other hand, the field \texttt{cache} references a continuous machine called \texttt{z}.
This machine will take care of both interpolation and memoization strategies.
Notice that the \texttt{z} machine \emph{reads} from the memory region that \texttt{computation} references.
The red arrows in the figure should help with this visualization: these \texttt{IORef} fields are indirectly interacting with each other; a change in one of them affects the other.
This process, however, is completely hidden from an usability's point of view -- the user of the FACT will not and \emph{should} not interact with this behavior.

Both remaining primitives work in a simpler manner.
When retriving a state variable, via the primitive \texttt{readInteg}, the function \texttt{readInteg} hooks onto the layout created by
the previous primitive and exposes its value when using FACT.
The final step, plugging the differential equations in integrators, is simply a writing operation on the reference of \texttt{computation} with the correct continuous machine; replacing the one with the initial value settled when we created the integrator initially.
Notice that the differential equation needs to interact with what is being referenced via the \texttt{cache} field -- it will leverage the use of memoization and interpolation using the same \texttt{z} machine from before.

As previously detailed, these primitives are not only \emph{required} to model with FACT, but the \emph{order} in which the user needs to write them also is due to highly imperative nature of this implementation.
Differential equations of interest use state variables, provided by \texttt{readInteg}, and the only way FACT allows one to retrive those is via the use of an integrator.
In order to have one of those, the user is obligated to first invoke \texttt{createInteg}, although what we are really interested in -- transposing mathematical descriptions of differential equations to software -- can only happens at step \texttt{updateInteg}.
Further, because a model description in FACT uses a \texttt{do}-notation block, the \emph{sequential} behavior imposed by bind, a requirement for the \texttt{Monad} type class, cannot be transpassed -- regardless if doing so would be more \emph{natural} from a modeling perspective.
Hence, FACT solved this problem by introducing the \texttt{Integrator} data type with its mutable reference fields.
This way allowed the use of good translation of differential equations into Haskell, via the continuous machines, with the trade-off of exposing and forcing the use the integrator's primitives at the user level.

In contrast, FFACT's use of \texttt{mdo} removes this existing limitation in FACT's \texttt{do}.
With \texttt{letrec}'s flexibility via the type class \texttt{MonadFix}, one can use order-independent bindings, which may need effects to be defined, as one would do with a piece of paper.
From an usability's perspective, value recursion is closing the gap between the informal notion of bindings mathematicians have and the more restricted notion programmers have, which usually vary from programming language to programming language and each one comes with its own different quirks and solutions to not incur into scoping issues.

With this context in mind, below we present the definition of \texttt{mfix} in the \texttt{MonadFix} type class for the
\texttt{ReaderT} type, the underlying type of the \texttt{CT} type alias, present in the \texttt{Control.Monad.Trans.Reader}~\footnote{\texttt{Control.Monad.Trans.Reader} \href{https://hackage.haskell.org/package/mtl-2.3.1/docs/Control-Monad-Reader.html}{\textcolor{blue}{hackage documentation}}.} package:
%
\vspace{-0.2cm}
\begin{purespec}
  instance (MonadFix m) => MonadFix (ReaderT r m) where
    mfix f = ReaderT $ \ r -> mfix $ \ a -> runReaderT (f a) r
\end{purespec}
\vspace{-0.2cm}
%$
Because a continuous machine has embedded \texttt{IO} in its data type definition, the monadic fix implementation of a continuous machine monad, here presented by the \texttt{ReaderT}, also leverages the instances of its internal, e.g., \texttt{IO}, for the same type class.
Further, because a continuous machine type is a type alias to \texttt{ReaderT}, which is a generalization to the Reader (or Environment) monad, it can be shown that continuous machine's implementation of \texttt{MonadFix} satifies the laws mentioned at the end of Section~\ref{sec:value-recursion}~\cite{leventThesis}.
With access to \texttt{mdo} syntax sugar, a new function, called \texttt{integ}, can therefore be implemented to perform what FACT's integrator was accomplishing instead:
%
\vspace{-0.2cm}
\begin{code}
integ :: CT Double -> CT Double -> CT (CT Double)
integ diff i =
  mdo y <- memo interpolate z
      z <- do ps <- ask
              let f =
                    case (method $ solver ps) of
                      Euler -> integEuler
                      RungeKutta2 -> integRK2
                      RungeKutta4 -> integRK4
              pure $ f diff i y
      return y
\end{code}
\vspace{-0.2cm}
%
This new functin received the differential equation of interest, named \texttt{diff}, and the initial condition of the simulation, identified
as \texttt{i}, on line 2. Interpolation and memoization requirements from FACT are being maintained, as shown on line 3. Lines 3 to 6 demonstrate the use case for FFACT's \texttt{mdo}.
A continuous machine created by the memoization function (line 3), \texttt{y}, uses another continuous machine, \texttt{z}, yet to be defined.
This continuous machine, defined on line 4, retrieves the numerical method chosen by a value of type \texttt{Parameters}, via the function \texttt{f}.
The outcome of the function \texttt{integ} is the outcome of running the simulation of interest in the context of memoization and interpolation.
As a final note, just as with \texttt{fix}, there is a need for the function being applied to the combinator to terminate the recursive process: this is being done via the function \texttt{memo} within the \texttt{integ} function.

Finally, the Lorenz Attractor example is rewritten as the following:

\begin{code}
lorenzModel :: Model Vector
lorenzModel = mdo
   x <- integ (sigma * (y - x)) 1.0
   y <- integ (x * (rho - z) - y) 1.0
   z <- integ (x * y - beta * z) 1.0
   return $ sequence [x, y, z]          

lorenzSystem = runCT lorenzModel 100 lorenzSolver
\end{code}

Not surprisingly, the results of this new approach using the monadic fixed-point combinator are very similar to the
performance metrics depicted in chapter 6, \textit{Caching the Speed Pill} --- indicating that we are \textit{not} trading performance
for a gain in conciseness. Figure~\ref{fig:fixed-graph} shows the new results:

\figuraBib{Graph3}{Results of FFACT are similar to the final version of FACT.}{}{fig:fixed-graph}{width=.97\textwidth}%

The function \texttt{integ} alone in FFACT ties the recursion knot previously done via the \texttt{computation} and \texttt{cache} fields from the original integrator data type in FACT.
Hence, a lot of implementation noise of the DSL is kept away from the user --- the designer of the system --- when using FFACT. With this chapter, we addressed
the third and final concerned explained in chapter 1, \textit{Introduction}. The final chapter, \textit{Conclusion}, will conclude this work, pointing out limitations of the project, as well as future improvements and final thoughts about the project.
