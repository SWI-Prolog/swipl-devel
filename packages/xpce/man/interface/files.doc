\section{The PCE installation directory}

This section describes the files relevant to the interface.  The files
are named {\tt xpce/...} where {\tt xpce} is the top directory
of PCE.  {\tt MACHINE} refers to the machine identifier:

\begin{center}
\begin{tabular}{|l|l|}
\hline
    sun3	& MC680x0 based machine running SunOs \\
    sun4	& SPARC based machine running SunOs \\
    rs6000	& IBM RS6000 running AIX \\
    mips	& DEC MIPS station running Ultrix \\
    sgi		& Silicon Graphics machine running Ultrix \\
\hline
\end{tabular}
\end{center}

The constant {\tt HOST} is an identifier for the host-language:

\begin{center}
\begin{tabular}{|l|l|}
\hline
    pl		& SWI-Prolog \\
    sisctus	& SISCtus Prolog \\
\hline
\end{tabular}
\end{center}

The following files from the XPCE directory hierarchy are relevant to
the interface:

\begin{itemize}
    \tick{\file{xpce/machine}}
C-shell script that returns the identifier name of the machine used.
This shell-script may be expanded when a new machine is added.
    \tick{\file{xpce/build}}
C-shell script to build PCE and link it to a specified language.  Calls
\file{xpce/make} to build \file{xpce/MACHINE/XPCE.o}.  Next calls
\file{xpce/HOST/make} to create the interface.
    \tick{\file{xpce/src/itf-interface.h}}
C-header file with declarations for the functions, variables and types
used in the XPCE-C interface.
    \tick{\file{xpce/MACHINE/XPCE.o}}
Combined {\tt .o} file (using `ld -r') from all PCE's C-modules.
Created by \file {xpce/src/make}.
    \tick{\file{xpce/src/files}}
List of PCE's {\tt .o} modules separated by newlines.  Created by
\file {xpce/src/make}. 
    \tick{\file{xpce/prolog/c/interface.c}}
Generic Prolog interface for SWI-Prolog and SICStus Prolog.
Probably applicable to any Prolog system that allows for analysing and
constructing Prolog terms in C.  See also \file{xpce/pl/src
/interface.c} and \file{xpce/sicstus/src/interface.c} 
    \tick{\file{xpce/prolog/c/interface.h}}
Declarations for the generic interface.
    \tick{\file{xpce/HOST/src/make}}
(C-)shell script called to create the interface for some language.  See
also the bottom of \file{xpce/build}.
\end{itemize}
