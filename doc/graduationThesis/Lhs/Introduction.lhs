Este documento serve de exemplo da utilização da classe \unbcic\ para escrever
um texto cujo objetivo é apresentar os resultados de um trabalho científico. A
sequência de ideias apresentada deve fluir claramente, de modo que o leitor
consiga compreender os principais conceitos e resultados apresentados, bem como
encontrar informações sobre conceitos secundários.

%\url{http://www.escritacientifica.com/}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section{Trabalho de Conclusão de Curso}%
Todos os cursos do \acrfull{CIC} da \acrfull{UnB} exigem a produção de um
texto científico como requisito para formação. %(veja \refAnexo{NormasGerais} para mais detalhes)
As etapas desta monografia/dissertação/tese devem seguir o \emph{método científico}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section{Metodologia Científica}%
Ciência (do Latim \emph{scientia}, traduzido como ``conhecimento'') é uma forma
sistemática de produzir conhecimento (via método científico), ou o nome dado a
estrutura organizada do conhecimento obtido.

O método científico é um conjunto de regras básicas de como proceder para produzir
conhecimento, criando algo novo ou corrigindo/incrementando conhecimentos
pré-existentes. Consiste em juntar evidências empíricas verificáveis baseadas na
observação sistemática e controlada, geralmente resultantes de experiências ou
pesquisa de campo, e analisá-las logicamente.

Esta ideia foi formalizada por Newton em sua obra \emph{Philosophiae Naturalis
Principia Mathematica}~\cite{newton1833philosophiae} da seguinte forma:
\begin{enumerate}
	\item Não se deve admitir causas das coisas naturais além daquelas
	que sejam verdadeiras e sejam suficientes para explicar seus fenômenos.
	\item Efeitos naturais do mesmo gênero devem ser atribuídos as mesmas causas.
	\item Características de corpos são consideradas universais.
	\item Proposições deduzidas da observação de fenômenos são
	consideradas corretas até que outro fenômeno mostre o contrário.
\end{enumerate}%

Uma abordagem para esta metodologia é seguir os seguintes passos:
\begin{description}
	\item[Caracterização do Problema:] Qual a pergunta a ser respondida? Quais
informações/recursos necessários na investigação?
	\item[Formulação da Hípotese:] Quais explicações possíveis para o que foi observado?
	\item[Previsão:] Dadas explicações [corretas] para as observações, quais os
	resultados previstos?
	\item[Experimentos:] \ \\\vspace{-2em}
		\begin{enumerate}
			\item Execute testes [reproduzíveis] da hipótese, coletando dados.
			\item Analise os dados.
			\item Interprete os dados e tire conclusões:
				\begin{itemize}
				\item que comprovam a hipótese;
				\item que invalidam a hipótese \emph{ou levam a uma nova hipótese}.
				\end{itemize}
		\end{enumerate}
	\item[Documentação:] Registre e divulgue os resultados.
	\item[Revisão de Resultados:] Validação dos resultados por outras pessoas
	[capacitadas].
\end{description}%

Geralmente se começa com a revisão sistemática, uma metodologia de pesquisa
específica para juntar e avaliar material relevante a determinado tópico~\cite{Biolchini_2005_Systematicreviewin}.

\subsection{Veja Também}
\begin{itemize}
	\item Google Acadêmico
		\\\url{http://scholar.google.com.br/}%
	\item ACM Digital Library
		\\\url{http://dl.acm.org/}%
	\item Portal \acrshort{CAPES}
		\\\url{http://www.periodicos.capes.gov.br/}%
	\item IEEE Xplore
		\\\url{http://ieeexplore.ieee.org/Xplore/home.jsp}%
	\item ScienceDirect
		\\\url{http://www.sciencedirect.com/}%
	\item Springer Link
		\\\url{http://link.springer.com/}%
\end{itemize}

Para buscar referências, \emph{The DBLP Computer Science Bibliography}\footnote{\url{http://dblp.uni-trier.de/}}
é um ótimo recurso. Veja o \refApendice{Apendice_Fichamento} para instruções
sobre como organizar as informações de artigos científicos.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section{\LaTeX}%

\TeX\ é ``\emph{a typesetting system intended for the creation of beautiful books
 - and especially for books that contain a lot of mathematics}''~\cite{Knuth_1986_texbook},
 um sistema de tipografia muito utilizado na produção de textos técnicos devido
 a qualidade final, principalmente das fórmulas e símbolos matemáticos gerados.

\LaTeX\ é um conjunto de macros para facilitar o uso de \TeX~\cite{lamport_latex:_1994},
cujos pacotes (a maioria centralizada na rede \acrshort{CTAN}~\cite{greenwade93}), oferecem
inúmeras possibilidades. Este sistema tipográfico visa explorar as potencialidades
da impressão digital, sem que o resultado seja alterado em função de diferenças
entre plataformas/sistemas.

Em uma publicação, um \emph{autor} entrega o texto a uma editor que define a
formatação do documento (tamanho da fonte, largura de colunas, espaçamento, etc.)
e passa as instruções (e o manuscrito) ao tipógrafo, que as executa. Neste processo,
\LaTeX\ assume os papéis de editor e tipógrafo, mas por ser ``apenas'' um programa
de computador, o autor deve prover algumas informações adicionais ~\cite{Oetiker_1995_notsoshort},
geralmente por meio de marcações (comandos).

Esta abordagem de linguagem de marcação (em que se indica como o texto deve ser
formatado) é diferente da abordagem OQVVEOQVO (``o que você vê é o que você
obtém\footnote{Do inglês WYSIWYG - ``What You See Is What You Get''.}'') de programas
para edição de texto tradicionais (como MS Word, LibreOffice Write, etc.).
Apesar destes programas serem extremamente úteis para gerar textos simples, que
são a grande maioria dos documentos, eles geralmente não têm a capacidade de lidar
corretamente com documentos complexos (como dissertações ou teses), conforme ilustrado
na \refFig{latexvsword}.%

\figuraBib{miktex}{\LaTeX\ vs MS Word}{pinteric_latex_2004}{latexvsword}{width=.45\textwidth}%

Existem diversas discussões quanto ao uso de editores de texto\footnote{Por exemplo:
\emph{Word Processors: Stupid and Inefficient} \url{http://ricardo.ecn.wfu.edu/~cottrell/wp.html}},
não há um consenso quanto a melhor forma de se gerar um documento de qualidade,
e a maioria das mídias científicas disponibiliza modelos para ambas.

Mas pode-se dizer que \LaTeX\ é mais indicado para:
\begin{itemize}
	\item notação matemática;
	\item referências cruzadas;
	\item separação clara entre conteúdo e formatação.
\end{itemize}

Enquanto os editores tradicionais são indicados para:
\begin{itemize}
	\item edição colaborativa (são mais populares);
	\item produção imediata (leve curva de aprendizado).
\end{itemize}

\subsection{Veja Também}
\begin{itemize}
	\item Introdução ao \LaTeX
		\\\url{http://latexbr.blogspot.com.br/2010/04/introducao-ao-latex.html}
	\item \LaTeX\ - A document preparation system
		\\\url{http://www.latex-project.org/}
	\item The \acrlong{CTAN}
		\\\url{http://ctan.org}
	\item \TeX Users Group
		\\\url{http://tug.org}
	\item \TeX\ - \LaTeX\ Stack Exchange
		\\\url{http://tex.stackexchange.com}
	\item \LaTeX\ Wikibook
		\\\url{http://en.wikibooks.org/wiki/LaTeX}
	\item write\LaTeX
		\\\url{http://www.writelatex.com}
\end{itemize}



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section{Plágio}%
O JusBrasil\footnote{\url{http://www.jusbrasil.com.br}} define plágio como
``reprodução, total ou parcial, da propriedade intelectual de alguém, inculcando-se
o criador da idéia ou da forma. Constitui crime contra a propriedade imaterial
violar direito de autor de obra literária, científica ou artística.''


A \acrfull{CEP} da Presidência da República decidiu  ``pela
aplicação de sanção ética aos servidores públicos que incorrerem na prática de
plágio"\footnote{\url{http://www.comissaodeetica.unb.br/index.php?view=article&id=8:plagio-academico}},
e a \acrfull{CAPES} recomenda que se adote políticas de conscientização e informação
sobre a propriedade intelectual, baseando-se na Proposição 2010.19.07379-01,
referente ao plágio nas instituições de ensino\footnote{\url{https://www.capes.gov.br/images/stories/download/diversos/OrientacoesCapes_CombateAoPlagio.pdf}}.


\subsection{Veja Também}
\begin{itemize}
	\item IEEE Plagiarism FAQ
		\\\url{http://www.ieee.org/publications_standards/publications/rights/plagiarism_FAQ.html}
	\item Relatório da Comissão de Integridade de Pesquisa do CNPq
		\\\url{http://www.cnpq.br/web/guest/documentos-do-cic}
\end{itemize}



%\section{Normas CIC}
% \href{http://monografias.cic.unb.br/dspace/normasGerais.pdf}{Política de Publicação de Monografias e Dissertações no Repositório Digital do CIC}%
% \href{http://monografias.cic.unb.br/dspace/}{Repositório do Departamento de Ciência da Computação da UnB}

% \href{http://bdm.bce.unb.br/}{Biblioteca Digital de Monografias de Graduação e Especialização}


\ignore{
\begin{code}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances, BangPatterns, ConstraintKinds, MonoLocalBinds #-}
module GraduationThesis.Lhs.Introduction where
import GraduationThesis.Lhs.Section2
\end{code}
}


\begin{code}

printForMe :: IO ()
printForMe = print aString

\end{code}
