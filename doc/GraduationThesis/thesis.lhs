\documentclass[engenharia]{UnB-CIC}%
%include polycode.fmt
\selectlanguage{american}\entrue\brfalse

\usepackage{pdfpages}% incluir PDFs, usado no apêndice
\usepackage{hyperref}
\usepackage{dcolumn}

\long\def\ignore#1{}
\usepackage[outputdir=GraduationThesis/]{minted}
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
%include graduationThesis/Lhs/Introduction.lhs

\ifdefined\iscolorful
  \input{graduationThesis/Lhs/Introduction.lhs}
\fi

\literateChapter{Section 2}
%include graduationThesis/Lhs/Section2.lhs

\ifdefined\iscolorful
  \input{graduationThesis/Lhs/Section2.lhs}
\fi

Isto é um teste para checar o programa.


\ignore{
\begin{code}
module Main where
import GraduationThesis.Lhs.Introduction
\end{code}
}


\begin{code}
main :: IO ()
main = printForMe
\end{code}

\end{document}
