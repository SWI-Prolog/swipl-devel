% Standard LaTeX stuff

\documentstyle[-]{-}
\documentclass[-]{-}
\usepackage[-]{-}[-]=prolog
\makeindex
\title{+}
\booktitle{+}
\author{+}
\maketitle
\psdirectories{-}
\thispagestyle{-}
\pagenumbering{-}
\bibliographystyle{-}
\bibliography{-}
{document}
{letter}

% Include
\include{-}
\input{-}

% Environments

{tabular}{-}
{array}{-}
{tabbing}
{minipage}{-}
{center}
{abstract}
{quote}
{verbatim}=verbatim
{code}=verbatim
{titlepage}
{minipage}{-}

% Placing control

\clearpage
\cleardoublepage
\nopagebreak
\pagebreak
\linebreak
\newpage
\hyphenpenalty{-}
\centerline{+}
\rightline{+}

% floats

{table}[-]=float
{figure}[-]=float

% Lists

{itemize}=list
{description}=list
{enumerate}=list
{shortlist}=list
\item[+]=item		1 0

% Normal commands

\part{+}		2 2
\chapter*{+}		2 2
\section*{+}		2 2
\subsection*{+}		2 2
\subsubsection*{+}	2 2
\paragraph{+}		2 1
\subparagraph{+}	2 1
\footnote{+}		% 0
\footnotetext[-]{+}	1 1
\newlength{-}
\setlength{-}{-}
\itemsep{-}
\settowidth{-}{-}
\newcommand{-}[-]{-}=prolog	1 1
\renewcommand{-}[-]{-}=prolog	1 1
\newenvironment{-}{-}{-} 1 1
\addcontentsline{-}{-}{+}
\parskip{d}
\parindent{d}
\def{-}{-}		1 1
\setcounter{-}{-}
\label{-}
\caption{+}		1 1
\ref{-}
\pageref{-}
\cite{-}
\yearcite{-}
\opencite{-}
\index{-}
\idx{-}
\tableofcontents
\printindex
\appendix
\string{-}
\mbox{+}
\makebox[-][-]{+}
\parbox[-]{d}{+}
\begin=begin		2 1
\end{-}=end		1 2
\\[d]			0 1
\ 
\verb=verb

% table stuff

\hline
\multicolumn{-}{-}{+}
\cline{-}
\>

% Font switch/style commands

\ul
\bf
\em
\sc
\sl
\sf
\it
\rm
\tt
\Huge
\Large
\LARGE
\large
\small
\tiny
\normalsize
\footnotesize
\scriptsize

% BibTeX support

{thebibliography}{-}=list
\bibitem[+]{-}=item
\newblock
\,
\protect
\citename{+}

% Dimensions

\textwidth
\linewidth
\vfil
\vfill
\hfill
\vskip{d}
\vspace{d}
\hspace{d}
\headheight{d}
\footheight{d}
\setlength{-}{d}
\vbox{-}
\noindent
\sloppy

% Special characters

\{
\}
\&
\$
\%
\'{-}
\"{-}
\#
\copyright
\year

% Arithmetic (math mode)

\times
\equiv
\pi
\leq
\geq
\neq
\sin{+}
\cos{+}
\tan{+}
\arcsin{+}
\arccos{+}
\arctan{+}
\ln{+}
\lg{+}
\not{-}
\frac{+}{+}
\pow{+}{+}
\sqrt{+}
\mod{+}{+}
\rem{+}{+}
\div
\longrightarrow
\rightarrow
\Rightarrow
\Leftrightarrow
\alpha
\beta
\exists
\emptyset
\subset
\langle
\rangle
\wedge


% fancy styles support

\fancyplain{-}
\pagestyle{-}
\rightmark
\leftmark
\lhead[-]{-}
\chead[-]{-}
\rhead[-]{-}
\lfoot[-]{-}
\cfoot[-]{-}
\rfoot[-]{-}
\footrulewidth{d}

% package{psfig} support

\psfig{-}

% Changebar support

\chgbarbegin
\chgbarend
\cbstart
\cbend

% Text producing commands

\ldots

% References

\chapref{-}
\Chapref{-}
\figref{-}
\Figref{-}
\secref{-}
\Secref{-}
\tabref{-}
\Tabref{-}

% Stuff from package{html}

\tick{+}=item
\email[+]{-}
\url[+]{-}
\onefile
{htmlonly}
{dlist}=list
{itemlist}=list
\excludecomment{-}
\obeylines
\raggedright
