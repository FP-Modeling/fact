\documentclass[engenharia]{UnB-CIC}%
%include polycode.fmt
%include GraduationThesis/lineno.fmt
%separation 1
%latency 1
\selectlanguage{american}\entrue\brfalse

\usepackage{pdfpages}% incluir PDFs, usado no apêndice
\usepackage{hyperref}
\usepackage{dcolumn}

\long\def\ignore#1{}
\usepackage[outputdir=GraduationThesis/]{minted}
\newminted[code]{haskell}{breaklines,autogobble,linenos=true}
\newminted[spec]{haskell}{breaklines,autogobble,linenos=true}


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
%include GraduationThesis/Lhs/Introduction.lhs

\ifdefined\iscolorful
  \input{GraduationThesis/Lhs/Introduction.lhs}
\fi

\literateChapter{Rivika}
%include GraduationThesis/Lhs/Explanation.lhs

\ifdefined\iscolorful
  \input{GraduationThesis/Lhs/Explanation.lhs}
\fi

\ignore{
\begin{code}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances, BangPatterns, ConstraintKinds, MonoLocalBinds #-}
module Main where
import Control.Monad.Trans
import Data.IORef
import Data.Array
import GraduationThesis.Lhs.Introduction
\end{code}
}

\end{document}
