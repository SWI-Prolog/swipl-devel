\section{Initialisation}

Before calling any of the interface functions described in this
document the function \cfunc{pceInitialise()} must be called.  This
function creates the initial object-structure of PCE (e.i.\ the
classes with their methods, @pce, @host, etc.); installs signal
handlers (see \cfunc{hostAction()}); etc.

\begin{description}
    \cfunction{int}{pceInitialise}{int handles, int argc, int argv}
Initialises PCE.  \arg{Handles} defines the number of `alien' handles
reserved in each pceITFSymbol (see section~\ref{sec:data}).  It is
generally 0 for host-systems that do not provide the C-interface with
access to the internal representation of textual constants; 1 for
host-systems that use the same (textual) constant to refer to PCE names
and PCE named objects (Prolog: both use a Prolog atom) and 2 for
host-systems that use distinct constants for names and named objects
(Lisp: keywords for names and Lisp structures for named objects).

The pair \arg{argc}, \arg{argv} are the command-line arguments used to
start the process.  When not available, pass ``0, \const{NULL}''.  PCE
processes the standard X-arguments to set the display or X-resources.
\end{description}

There are three ways to call this function.

\begin{itemize}
    \tick{Before creating a saved-state}
Many symbolic programming environments allow the user to create a
saved-state.  If this state includes the foreign functions and
foreign data, the PCE/Host system may be created using the following
steps:
    \begin{itemize}
	\item Load \file{XPCE.o}; the C-part of the interface and
	      the X-libraries
	\item Call \cfunc{pceInitialise()}
	\item Load the host-language written part of the interface
	\item Create a saved-state
    \end{itemize}
When the state is restarted, \cfunc{pceInitialise()} must be called
again to pass the new command-line arguments.  A second call to
\cfunc{pceInitialise()} only registers the command-line arguments.
The \arg{handles} argument is ignored.
    \tick{After creating a saved-state}
Some environments do not allow for foreign data in the saved state.
If they do allow for foreign functions one could load the foreign
part of PCE as well as the host-language written part before creating
the saved-state.  When the state is started \cfunc{pceInitialise()}
should be called before any of the other interface functions.
\end{itemize}


\section{PCE Garbage collection}		\label{sec:gc}

PCE offers an incremental garbage collector.  The interface defines
functions to exploit the grabage collector.  Consider the call (in
Prolog format):

\begin{code}
1 ?- new(@b, box(10, 10)).
2 ?- send(@b, move, point(100, 100)).
\end{code}

The term ``\verb$point(100, 100)$'' is translated into a point object.
The `box ->move' method copies the X- and Y- coordinates from the
argument point object, but does not require the point-object to
exist afterwards. PCE offers \cfunc{markAnswerStack()} and \cfunc
{rewindAnswerStack()} for this purpose.  Using these calls, the
translation of the above Prolog code is:

\begin{code}
{ AnswerMark mark;
  PceObject b;
  PceObject point;
  ArgVector(argv, 2);

  markAnswerStack(mark);
  argv[0] = argv[1] = cToPceInteger(100);
  point = pceNew(NULL, cToPceName("point"), 2, argv);
  b = cToPceAssoc("b");
  pceSend(b, cToPceName("move"), 2, argv);
  rewindAnswerStack(mark, NULL);
}
\end{code}


The function \cfunc{rewindAnswerStack()} removes any PCE object created
after the corresponding \cfunc{markAnswerStack()} that has no references
from other objects and is not explicitly locked against the PCE garbage
collector.  The second argument of \cfunc{rewindAnswerStack()} is the
one and only object that should {\em not} be removed by this call.  This
is used to avoid the return-value of \cfunc{pceGet()} and \cfunc{pceNew()}
to be removed.

Because the overhead of these calls is neglectable, they should be
used as local as possible.  The calls \cfunc{markAnswerStack()} and
\cfunc{rewindAnswerStack()} must be properly nested.  Not every
\cfunc{markAnswerStack()} however has to be followed by a
\cfunc{rewindAnswerStack()}.  Consider:

\begin{code}
{ AnswerMark m1, m2;

  markAnswerStack(m1);
  ...
  markAnswerStack(m2);
  ...
  rewindAnswerStack(m1, rval);
}
\end{code}

This calling sequence is legal.  After this sequence the mark \arg{m2}
is invalid and calling ``rewindAnswerStack(m2)'' is illegal.


\section{Mempry allocation}

PCE defines its own amemory allocation schema for memory segments smaller
than 1024 bytes.  It calls \cfunc{malloc()} for 10240 bytes if the pool
for allocation small memory chunks is exhausted.  Larger allocations and
allocations done by X-windows use \cfunc{malloc()} resp.\ \cfunc
{Xmalloc()}.

Local arrays for storing an (argument) vector of PceObject's may be alocated
using the macro \cfunc{ArgVector()} defined in
\file{xpce/src/itf-interface.h}:

\begin{description}
    \cfunction{(macro)}{ArgVector}{name, int size}
Allocate an array of PceObject's of specified size on the C-stack.  When
using \idx{gcc} this uses gcc's dynamic array declarations.  Otherwise
\cfunc{alloca()} is used for this purpose.

This declaration must be at a legal place for defining (C-) local variables.
See section!~\ref{sec:examples}.
\end{description}
