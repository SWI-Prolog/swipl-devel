%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\chapter{Multiprocess Applications}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

In this chapter we will discuss the usage of XPCE-4 for applications
spread over multiple Unix processes, possibly running on multiple
systems.  We distinguish the following designs:

\begin{itemize}
    \tick{Calling non-interactive data-processing applications}
With `non-interactive data-processing applications' we refer to
applications that take an input specification from a file and
dump the result of there activities on another file without
user interaction.
    \tick{Front-end for terminal-based interactive applications}
Traditional examples from the Unix/X11 world are xdbx/xgdb as a
graphical front-end for dbx/gdb and various graphical mailtools
acting as front-ends for their terminal based counterparts.
    \tick{Client-Server applications}
In these applications, XPCE/Prolog will generally be used as a
graphical front-end for one or more applications to which it
communicates over the network.
\end{itemize}

\section{XPCE building blocks}

XPCE provides the application programmer with the following
building-blocks for multi-process and/or multi-user applications:

\begin{itemize}
    \tick{Class process}
Class process defines the interaction between XPCE and both
synchronously and asynchronously running Unix processes.
Communication can be provided using Unix {\em pipes} as well as
using a Unix {\em pseudo-tty}.

The process controlled this way can only be started as a child
process of XPCE/Prolog.  Class {\em socket} discussed below allows
for non-child-process communication.
    \tick{Class socket}
The XPCE class socket makes Unix sockets available to the XPCE
programmer.  It supports both Unix-domain sockets (sockets that
make an {\em end-point} in the Unix file-structure and can only
be used to communicate between processes on the same machine) and
{\em inet} domain sockets (sockets that make an {\em internet}
end-point and can be used to communicate with other processes
running anywhere on the internet).
    \tick{Class display and class display_manager}
XPCE can communicate with multiple X11-displays simultaneously.%
	\footnote{Provided the application is purely event-driven}
Thus, cooperative applications ({\em CSCW}) may be implemented as
client-server applications as well as using a single XPCE/Prolog
application managing windows for multiple users.
\end{itemize}

\section{Calling non-interactive data-processing applications}

There are various options for calling such applications:

\begin{itemize}
    \tick{Using class process}
The communication between the child-process and XPCE is arranged
using Unix pipes, and optionally using a Unix pseudo-tty.  Data
may be sent and received incrementally.  XPCE can assign a call-back
to be executed for newly available data and/or termination of
the process or wait for either of these events.
    \tick{Using Prolog's unix/1}
Generally only allows for the schema ``prepare input-file, call
process, parse output-file''.  On most Prolog systems, XPCE
event-dispatching will not ocur during execution of the
external application.
\end{itemize}

The proper choice from the above depends on the characteristics of the
application: 

\begin{itemize}
    \tick{The application can read/write to Unix standard I/O}
This will allow you to handle the output incrementally as it is
produced by the external process, simutaneously handling user-events.
    \tick{Execution time of the application}
Applications with short execution-times may be called using Prolog's
unix/1 predicate as well as using XPCE process objects with XPCE
waiting for termination of the application.
    \tick{Amount of data produced, nature and destination of it}
Can output of the application be parsed in separate records?
\end{itemize}

\section{Examples}

The examples below generally take trivial standard Unix applications.
Many of the manipulations could be established inside Prolog or XPCE.

\subsection{Calling simple Unix output-only utilities}

In this example, we will define the Prolog predicate call_unix/3,
which takes the following arguments:

\begin{shortlist}
    \tick{Name of the utility}
    \tick{List of arguments to it}
    \tick{Output: Prolog list of ASCII values} 
\end{shortlist}

\begin{code}
call_unix(Utl, Args, Output) :-
	NewTerm =.. [process, Utl | Args],
	new(P, NewTerm),
	send(P, use_tty, @off),
	send(P, open),
	new(OS, string),
	repeat,
	    (   get(P, read_line, Line)
	    ->	send(OS, append, Line),
		fail
	    ;   !
	    ),
	pce_string_to_list(OS, Output).

pce_string_to_list(S, L) :-
	pce_string_to_list(S, 0, L).

pce_string_to_list(S, I, [C|T]) :-
	get(S, character, I, C), !,
	NI is I + 1,
	pce_string_to_list(S, NI, T).
pce_string_to_list(_, _, []).
\end{code}

This version of the call will block and not dispatch X11 events 
during the execution of call_unix/3.  Below we use another definition
that processes normal user events during the execution.  We borrow
the definition of pce_string_to_list/2 from the previous example.

The call `process ->wait' runs the XPCE event-loop until all data
from the process has been handled.

\begin{code}
call_unix(Utl, Args, Output) :-
	NewTerm =.. [process, Utl | Args],
	new(P, NewTerm),
	send(P, use_tty, @off),
	new(OS, string),
	send(P, record_separator, @nil),
	send(P, input_message, message(OS, append, @arg1)),
	send(P, open),
	send(P, wait),
	pce_string_to_list(OS, Output),
	send(P, done),
	send(OS, done).
\end{code}

\begin{exercises}
\exercise{%
What is the function of `process->record_separator' and why is it set
to @nil in the example above?
}
\exercise{%
Write a predicate call_unix({\em +Utility, +Args, +Input, -Output})
that pipes the data from the Prolog string {\em Input} through the
command and unifies {\em Output} with a Prolog string representing
the output of the process.  Use call_unix/3 above as a starting point.
}
\end{exercises}


\section{Front-end for terminal-based interactive applications}

In the example for this type of application we will define a simple
front-end for the Unix ftp(1) program.  Below is a screen-dump
of this demo-application:

\postscriptfig[width=\textwidth]{ftp}{Screendump for the `FTP tool'}

\input ftp.tex

\begin{exercises}
\exercise{%
Add a facility to download a file to your current directory.
}
\end{exercises}



