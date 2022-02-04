\documentclass[engenharia]{UnB-CIC}%
%include polycode.fmt

%\usepackage{pdfpages}% incluir PDFs, usado no apêndice
\usepackage{hyperref}

\long\def\ignore#1{}
\usepackage{minted}
\newminted[code]{haskell}{breaklines}

\orientador{\prof \dr José Edil Guimarães}{CIC/UnB}%
\coordenador[a]{\prof[a] \dr[a] Ada Lovelace}{Bibliothèque universelle de Genève}%
\diamesano{00}{onzembro}{2022}%

\membrobanca{\prof \dr Donald Knuth}{Stanford University}%
\membrobanca{\dr Leslie Lamport}{Microsoft Research}%

\autor{Eduardo L.}{Rocha}%

\titulo{The side effect hell on Earth}%

%\palavraschave{differential equations, metodologia científica, trabalho de conclusão de curso}%
\keywords{differential equations, continuous systems, fix point, feedback loop}%

\newcommand{\unbcic}{\texttt{UnB-CIC}}%

\begin{document}


\literateChapter{Introduction}
%include lhs/Introduction.lhs

\literateChapter{Section 2}
%include lhs/Section2.lhs



Isto é um teste para checar o programa.


\ignore{
\begin{code}
module Main where
import Lhs.Introduction
\end{code}
}


\begin{code}
main :: IO ()
main = printForMe
\end{code}

\end{document}
