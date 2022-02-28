\documentclass[engenharia]{UnB-CIC}

%include polycode.fmt
%include GraduationThesis/lineno.fmt
%separation 1
%latency 1


\ifdefined\iscolorful\else
  \renewcommand{\hscodestyle}{\small}
\fi

\long\def\ignore#1{}

\selectlanguage{american}\entrue\brfalse
\usepackage{pdfpages}% incluir PDFs, usado no apêndice
\usepackage{hyperref}
\usepackage{dcolumn}
\usepackage{amsmath, systeme}
\usepackage{float}
\usepackage{caption}
\usepackage{subcaption}
\usepackage{amsmath}

\usepackage[outputdir=GraduationThesis/]{minted}
\newminted[code]{haskell}{breaklines,autogobble,linenos=true, numberblanklines=false, fontsize=\footnotesize}
\newminted[spec]{haskell}{breaklines,autogobble,linenos=true, numberblanklines=false, fontsize=\footnotesize}


\orientador{\prof José Edil Guimarães}{CIC/UnB}%
\coordenador[a]{\prof[a] Ada Lovelace}{Bibliothèque universelle de Genève}%
\diamesano{00}{onzembro}{2022}%

\membrobanca{\prof Donald Knuth}{Stanford University}%
\membrobanca{\dr Leslie Lamport}{Microsoft Research}%

\autor{Eduardo L.}{Rocha}%

\titulo{The side effect hell on Earth}%

\palavraschave{equações diferenciais, sistemas contínuos, ponto fixo, retroalimentação}
\keywords{differential equations, continuous systems, fix point, feedback loop}%

\newcommand{\unbcic}{\texttt{UnB-CIC}}%

\begin{document}

\literateChapter{Introduction}
%include GraduationThesis/Lhs/Introduction.lhs

\ifdefined\iscolorful
  \input{GraduationThesis/Lhs/Introduction.lhs}
\fi

\literateChapter{Implementation}
%include GraduationThesis/Lhs/Implementation.lhs

\ifdefined\iscolorful
  \input{GraduationThesis/Lhs/Implementation.lhs}
\fi

\ignore{
\begin{code}
module Main where
import GraduationThesis.Lhs.Introduction
import GraduationThesis.Lhs.Implementation

main :: IO ()
main = executeLorenz
\end{code}
}

\end{document}
