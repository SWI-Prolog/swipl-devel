\documentclass[11pt]{article}
\usepackage{pl}
\usepackage{html}
%\onefile
\sloppy
\makeindex

\onefile
\htmloutput{.}				% Output directory
\htmlmainfile{macosx}			% Main document file
\bodycolor{white}			% Page colour

\renewcommand{\runningtitle}{SWI-Prolog for MacOS X (Darwin)}

\begin{document}

\title{SWI-Prolog for MacOS X}
\author{Jan Wielemaker \\
	Paulo Moura
       }

\maketitle

\begin{abstract}
This document briefly explains the MacOS X specific issues for
SWI-Prolog. This is by no means a manual or Prolog tutorial. The
reference manual is available online or can be downloaded in HTML and
PDF format from the \href{http://www.swi-prolog.org/}{SWI-Prolog
website}, which also provides links to books, online tutorials and other
Prolog related material.
\end{abstract}


\tableofcontents

\section{Introduction -- MacPorts}
\label{sec:macosx-intro}

SWI-Prolog is by origin an Unix application, and not a native Macintosh
application. Initially, it was ported to MacOS using
\href{https://www.macports.org/}{MacPorts}. We maintain the MacPorts
\texttt{Portfile}. There is also a \textit{Formulae} for
\href{https://brew.sh/}{Homebrew} named \texttt{swi-prolog}. In
addition, we provide a universal binary \textit{bundle}	that is
built using dependencies compiled using \file{scripts/macos-deps.sh}
from the source distribution.

Starting with version 9.3.26, graphics are based on
\href{https://www.libsdl.org/}{SDL3},
\href{https://www.cairographics.org/}{Cairo} and
\href{https://www.gtk.org/docs/architecture/pango}{Pango} and use native
MacOS cocoa and quartz.


\section{Installation}
\label{sec:macosx-install}

\subsection{Using MacPorts}
\label{sec:macosx-using}

Users of the MacPorts system can install the system just like any port
using the command below. In addition to the port named
\const{swi-prolog} providing the stable version, there is a port called
\const{swi-prolog-devel} providing the development version.

\begin{code}
% sudo port -v selfupdate       # make sure we have the latest portfiles
% sudo port install swi-prolog
\end{code}

\subsection{From the installer}
\label{sec:macosx-installer}

The \href{https://www.swi-prolog.org/Download.html}{download} page
provides a universal (arm64 and x86_64) MacOS bundle.  The app can
be executed directly from the installer or be installed by dropping
it in the \texttt{Application} directory.


\section{Running SWI-Prolog}
\label{sec:macosx-running}

SWI-Prolog for MacOS comes with a commandline program \program{swipl}
and a GUI program called \program{swipl-win}. If the system is installed
using Macports or Homebrew, these should be available on your
\texttt{PATH}. When using the bundle, the GUI can be started by opening
the application.  You can use the commandline tools from the bundle by
adding \file{Contents/MacOS} to your \texttt{PATH}.

The commandline version (\program{swipl}) can be started in a terminal:

\begin{code}
> swipl
Welcome to SWI-Prolog (threaded, 64 bits, version 9.3.26
SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software.
Please run ?- license. for legal details.

For online help and background, visit https://www.swi-prolog.org
For built-in help, use ?- help(Topic). or ?- apropos(Word).

?-
\end{code}

\subsection{Loading a program}
\label{sec:macosx-loading}

When using the commandline version, a program is normally loaded by
giving it as an argument, e.g.

\begin{code}
> swipl myprogram.pl
<banner>
?-
\end{code}

Alternatively, Prolog may be started without a program loaded and
a source file can be loaded by specifying its filename between
\verb$[]$.  In addition to a plain filename, files may be searched on
a named search-path%
	\footnote{See file_search_path/2 in the manual for details.}
using the notation \term{SearchPath}{File}. Two defined paths are
\const{library} for the Prolog library and \const{swi} for the Prolog
installation directory. Below we load the file \file{likes.pl} from the
\file{demo} directory in the installation directory, Be sure to get the
quotes right and terminate the command with a full-stop (\chr{.}).

\begin{code}
?- [swi('demo/likes')].
\end{code}

\subsection{Executing a query}			\label{sec:execquery}
\label{sec:macosx-query}

After loading a program, one can ask Prolog queries about the program.
The query below asks Prolog what food `sam' likes. The system responds
with \mbox{\tt X = <value>} if it can prove the goal for a certain
\arg{X}. The user can type the semi-colon (;)%
	\footnote{On most installations, single-character commands are
		  executed without waiting for the {\sc return} key.}
if (s)he wants another solution, or {\sc return} if (s)he is satisfied,
after which Prolog will say {\bf Yes}. If Prolog answers {\bf No}, it
indicates it cannot find any (more) answers to the query. Finally,
Prolog can answer using an error message to indicate the query or
program contains an error.

\begin{code}
?- likes(sam, X).

X = dahl ;

X = tandoori ;

...

X = chips ;

No
?-
\end{code}


\subsection{Editing Prolog programs}		\label{sec:edit}
\label{sec:macosx-edit}

There are three options for editing. One is to run an editor of choice
in a separate window and use the below described make/0 command to
reload modified files. In addition to this option Prolog can be used to
locate predicates, modules and loaded files by specifying the editor of
choice for use with the edit/1 command described below. This is achieved
by editing the personalisation file \file{~/config/swi-prolog/init.pl}.
A commented template is in the directory \file{customize} of the
SWI-Prolog installation directory.

Finally, you may wish to use the built-in editor called \emph{PceEmacs}.
This editor provides colourisation support based on real-time parsing
and cross-reference analysis of the program. It is started using the
command \verb$?- emacs.$. PceEmacs is the default editor used by edit/1.


\subsection{Some useful commands}
\label{sec:macosx-commands}

This section provides a very brief overview of important or
commonly used SWI-Prolog predicates to control the environment.

\begin{description}
    \predicate{consult}{1}{+File}
Load a source-file. A Prolog list ([\ldots]) can be used to abbreviate
the consult command. The file-extension (\fileext{pl} can be omitted.
Here are some examples:

\begin{center}
\begin{tabular}{ll}
\tt ?- consult(likes). & Load \file{likes.pl} from the current folder
			 (see pwd/0). \\
\tt ?- ['/opt/local/lib/swipl-5.6.0/demo/likes'] & Load \file{likes.pl} using
			absolute path. \\
\end{tabular}
\end{center}

    \predicate{pwd}{0}{}
Print working directory (folder).

    \predicate{ls}{0}{}
List files in current directory.

    \predicate{edit}{0}{}
If Prolog is started by opening a \fileext{pl} file in the explorer,
edit this file.  Also available from the menu.

    \predicate{edit}{1}{+Spec}
Edit file, predicate, module, etc.\ with the given name.  If multiple
items are named \arg{Spec} it prompts for the desired alternative.

    \predicate{make}{0}{}
Reload all files that have been changed since they where last loaded.
Normally used after editing one or more files.

    \predicate{trace}{0}{}
Start the interactive debugger.  There are three ways to use this.
Entered as a single goal at the top-level, the next query will be
traced.  Alternatively it can be used in conjunction with the goal
to be debugged: \exam{?- trace, run.} and finally you can include it
in your program to start tracing at a particular point or under a
particular condition:

\begin{code}
	...,
	(var(X) -> trace ; true),
	...,
\end{code}

    \predicate{gtrace}{0}{}
Same as trace, but forces the use of the graphical (source-level)
debugger.

    \predicate{apropos}{1}{+Keyword}
Search for all predicates that contain \arg{Keyword} in their name
or short description.  If a GUI environment is available the results
are hyperlinks.  Otherwise use help/1 to get details on selected
hits.

    \predicate{help}{1}{+Spec}
Give help on \arg{Spec}, which is normally the name of a predicate or
C interface function.

    \predicate{explain}{1}{+Term}
Prints information on \arg{Term}.  \arg{Term} can be any term.  This
is particularly useful to find where a predicate is defined and where
it is called by passing either just the name of the predicate or a
term \arg{Name}/\arg{Arity}.
\end{description}


\section{Using SWI-Prolog with C/C++}
\label{sec:macosx-cpp}

To use SWI-Prolog with C or C++ code you must install Apples Xcode
environment, providing \program{gcc}. If all paths are properly
installed, programs can be linked using the \program{swipl-ld} command
described in the manual.


\section{Known problems}
\label{sec:macosx-issues}

\begin{itemlist}
    \item [Key bindings]
The GUI tools find their origin on Unix, where key bindings are
primarily based on GNU-Emacs.  GNU-Emacs uses a \jargon{Meta key}
as \jargon{modifier}.  On MacOS, the \textbf{option} key acts as
meta key.  This choice is copied from GNU-Emacs for MacOS.  The
conventional MacOS \textbf{command} key bindings are supported.

    \item [Threaded XPCE programs]
Using SDL graphics, the GUI runs in the main thread.  This implies
that when using graphical tools from \program{swipl}, these are
unresponsive while Prolog executes for a longer period of time.
This problem does not apply for \program{swipl-win}.  In this
configuration the main thread exclusively processes the GUI and
user queries are executed in another thread.
\end{itemlist}


\section{The SWI-Prolog community}
\label{sec:macosx-intro}

\subsection{Web-site and mailing lists}
\label{sec:macosx-mailinglist}

The SWI-Prolog web-site is located at \url{https://www.swi-prolog.org/}.
SWI-Prolog has an active forum at
\url{https://swi-prolog.discourse.group/}.


\subsection{About license conditions}
\label{sec:macosx-license}

The SWI-Prolog license allows it to be used in a wide variety of
environments, including closed-source commercial applications. In
practice, redistribution and embedding is allowed, as long as
\emph{modifications} to the SWI-Prolog source are published following
the Free Software rules.

SWI-Prolog is licensed under the \emph{BSD-2} license.  Depending on
the configuration, it may load libraries with different license terms.
Notably

\begin{itemize}
    \item \href{https://gmplib.org/}{libGMP} is available under the
    LGPL license.  libGMP can be avoided at built time, replacing
    it with \href{https://bellard.org/libbf/}{LibBF}.  This provides
    the same functionality, but typically looses performance when
    large integers and notably rational numbers are used.
    \item \href{https://www.gtk.org/docs/architecture/pango}{Pango}
    also has the LGPL license.  It used by that graphics subsystem
    and	a core component thereof.
\end{itemize}

The predicate license/0 may be used to display additional information
including all components that have no liberal open source license. BSD,
MIT and Apache are considered liberal licenses.


\subsection{Supporting SWI-Prolog}
\label{sec:macosx-support}

There are several ways to support SWI-Prolog:

\begin{itemize}
    \item Extend the system with contributions
    \item Improve the system by submitting bug reports and patches.
    \item Link to \url{https://www.swi-prolog.org} and refer to SWI-Prolog
          in publications.
    \item Ask for commercial development or support at
          \url{https://www.swi-prolog.com/}
    \item Support development financially at
          \url{https://github.com/sponsors/SWI-Prolog}.
\end{itemize}

\end{document}
