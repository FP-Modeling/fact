\documentclass[mestrado,ppgi]{UnB-CIC}

%include polycode.fmt
%include MastersThesis/lineno.fmt
%separation 1
%latency 1

\ifdefined\iscolorful\else
  \renewcommand{\hscodestyle}{\small}
\fi

\ifdefined\doubleSpaced
  \openup 2em
\fi

\long\def\ignore#1{}

\selectlanguage{american}\entrue\brfalse
\usepackage{pdfpages}% incluir PDFs, usado no apêndice
\usepackage{hyperref}
\usepackage{dcolumn}
\usepackage{amsmath, systeme}
\usepackage{float}
\usepackage[font=footnotesize]{caption}
\usepackage{subcaption}
\usepackage{amsmath}
\usepackage{footmisc}
\usepackage{xcolor}
\usepackage{listings}

\usepackage[outputdir=MastersThesis/]{minted}
\newminted[code]{haskell}{breaklines,autogobble,linenos=true, numberblanklines=false, fontsize=\footnotesize}
\newminted[spec]{haskell}{breaklines,autogobble,linenos=true, numberblanklines=false, fontsize=\footnotesize}
\newminted[purespec]{haskell}{breaklines,autogobble,linenos=false, numberblanklines=false, fontsize=\footnotesize}

\orientador{\prof Eduardo Peixoto}{CIC/UnB}%
\coorientador{\prof José Edil Guimarães}{ENE/UnB}%
\coordenador{\prof João José Costa Gondim}{CIC/UnB}%
\diamesano{11}{Feb}{2025}%

\membrobanca{\prof Vander Ramos Alves}{CIC/UnB}%
\membrobanca{\dr George Ungureanu}{Ericsson Sweden}%

\autor{Eduardo L.}{Rocha}%

\titulo{Continuous Time Modeling Made Functional: Fixing Differential Equations with Haskell}%

\palavraschave{equações diferenciais, sistemas contínuos, GPAC, integrador, ponto fixo, recursão monádica}
\keywords{differential equations, continuous systems, GPAC, integrator, fixed-point, fixed-point combinator, monadic recursion}%

\newcommand{\unbcic}{\texttt{UnB-CIC}}%

\begin{document}

\literateChapter{Introduction}
%include MastersThesis/Lhs/Introduction.lhs

\ifdefined\iscolorful
  \input{MastersThesis/Lhs/Introduction.lhs}
\fi

\literateChapter{Design Philosophy}
%include MastersThesis/Lhs/Design.lhs

\ifdefined\iscolorful
  \input{MastersThesis/Lhs/Design.lhs}
\fi

\literateChapter{Effectful Integrals}
%include MastersThesis/Lhs/Implementation.lhs

\ifdefined\iscolorful
  \input{MastersThesis/Lhs/Implementation.lhs}
\fi

\literateChapter{Execution Walkthrough}
%include MastersThesis/Lhs/Enlightenment.lhs

\ifdefined\iscolorful
  \input{MastersThesis/Lhs/Enlightenment.lhs}
\fi

\literateChapter{Travelling across Domains}
%include MastersThesis/Lhs/Interpolation.lhs

\ifdefined\iscolorful
  \input{MastersThesis/Lhs/Interpolation.lhs}
\fi

\literateChapter{Caching the Speed Pill}
%include MastersThesis/Lhs/Caching.lhs

\ifdefined\iscolorful
  \input{MastersThesis/Lhs/Caching.lhs}
\fi

\literateChapter{Fixing Recursion}
%include MastersThesis/Lhs/Fixing.lhs

\ifdefined\iscolorful
  \input{MastersThesis/Lhs/Fixing.lhs}
\fi

\literateChapter{Conclusion}
%include MastersThesis/Lhs/Conclusion.lhs

\ifdefined\iscolorful
  \input{MastersThesis/Lhs/Conclusion.lhs}
\fi

\literateChapter{Appendix}
%include MastersThesis/Lhs/Appendix.lhs

\ifdefined\iscolorful
  \input{MastersThesis/Lhs/Appendix.lhs}
\fi

\ignore{
\begin{code}
module Main where
import MastersThesis.Lhs.Implementation
import MastersThesis.Lhs.Enlightenment
import MastersThesis.Lhs.Interpolation
import MastersThesis.Lhs.Introduction
import MastersThesis.Lhs.Caching
import MastersThesis.Lhs.Design
import MastersThesis.Lhs.Fixing

main :: IO ()
main =
  do ans <- lorenzSystem
     print ans
\end{code}
}

\end{document}
