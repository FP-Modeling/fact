\documentclass[engenharia]{UnB-CIC}

%include polycode.fmt
%include GraduationThesis/lineno.fmt
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

\usepackage[outputdir=GraduationThesis/]{minted}
\newminted[code]{haskell}{breaklines,autogobble,linenos=true, numberblanklines=false, fontsize=\footnotesize}
\newminted[spec]{haskell}{breaklines,autogobble,linenos=true, numberblanklines=false, fontsize=\footnotesize}

\orientador{\prof José Edil Guimarães}{CIC/UnB}%
\coordenador[a]{\prof[a] Ada Lovelace}{Bibliothèque universelle de Genève}%
\diamesano{00}{onzembro}{2022}%

\membrobanca{\prof Donald Knuth}{Stanford University}%
\membrobanca{\dr Leslie Lamport}{Microsoft Research}%

\autor{Eduardo L.}{Rocha}%

\titulo{Continuous Time Modeling Made Functional: Tackle Cyber-Physical Systems with Haskell}%

\palavraschave{equações diferenciais, sistemas contínuos, ponto fixo, retroalimentação}
\keywords{differential equations, continuous systems, fix point, feedback loop}%

\newcommand{\unbcic}{\texttt{UnB-CIC}}%

\begin{document}

\literateChapter{Introduction}
%include GraduationThesis/Lhs/Introduction.lhs

\ifdefined\iscolorful
  \input{GraduationThesis/Lhs/Introduction.lhs}
\fi

\literateChapter{Design Philosophy}
%include GraduationThesis/Lhs/Design.lhs

\ifdefined\iscolorful
  \input{GraduationThesis/Lhs/Design.lhs}
\fi

\literateChapter{The Side Effect Beast}
%include GraduationThesis/Lhs/Implementation.lhs

\ifdefined\iscolorful
  \input{GraduationThesis/Lhs/Implementation.lhs}
\fi

\literateChapter{Denotative Understanding}
%include GraduationThesis/Lhs/Enlightenment.lhs

\ifdefined\iscolorful
  \input{GraduationThesis/Lhs/Enlightenment.lhs}
\fi

\literateChapter{Drawbacks}
%include GraduationThesis/Lhs/Drawbacks.lhs

\ifdefined\iscolorful
  \input{GraduationThesis/Lhs/Drawbacks.lhs}
\fi

\literateChapter{Weakening Discreteness}
%include GraduationThesis/Lhs/Interpolation.lhs

\ifdefined\iscolorful
  \input{GraduationThesis/Lhs/Interpolation.lhs}
\fi

\literateChapter{The Speed Pill: Caching}
%include GraduationThesis/Lhs/Caching.lhs

\ifdefined\iscolorful
  \input{GraduationThesis/Lhs/Caching.lhs}
\fi

\literateChapter{Conclusion}
%include GraduationThesis/Lhs/Conclusion.lhs

\ifdefined\iscolorful
  \input{GraduationThesis/Lhs/Conclusion.lhs}
\fi

\ignore{
\begin{code}
module Main where
import GraduationThesis.Lhs.Implementation
import GraduationThesis.Lhs.Enlightenment
import GraduationThesis.Lhs.Interpolation
import GraduationThesis.Lhs.Introduction
import GraduationThesis.Lhs.Caching
import GraduationThesis.Lhs.Design

main :: IO ()
main = mainLorenz
\end{code}
}

\end{document}
