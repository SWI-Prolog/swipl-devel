\section{Conversion of Principal Data Types}	\label{sec:data}

The host language and PCE have incompatible data representation.  We
assume the host language to be able to represent data types for
integers (C: int), floating point numbers (C: float) and character
arrays (C: char~*).  Integers and floating point numbers are
available in many host languages, character arrays are represented as
atoms in Prolog and keywords or strings in Lisp.


\subsection{Textual constants: interface symbol table}	\label{sec:itftable}

Most symbolic languages have unique representation for textual
constants (atoms in Prolog; symbols and keywords in Lisp).  PCE
defines {\em names} for this purpose.  One way to convert
host-language textual constants into PCE names is to convert the
host-language constant into a C char~* and then the C char~* into
a PCE name.  Older version of the PCE external interface enforced
this mechanism.  Performance analysis has indicated that direct
mapping improves the performance of message-passing from the
host-language to PCE by a factor of 3.  For this reason PCE offers the
possibility to exploit direct mapping of host-language textual
constants into PCE names.

For each name that 1) has been made visible to the host-language or
2) is the name of a named PCE object (i.e.\ @pce) PCE defines an
{\em interface symbol}.  The definition of an \idx{pceITFSymbol}
is given below:

\begin{code}
typedef struct pceITFSymbol
{ PceObject	object;			/* global object associated */
  PceName	name;			/* Pce name associated */
  hostHandle	handle[0];		/* Alien handles to operate on */
} *PceITFSymbol;
\end{code}

The \arg{object} field is the global object associated with
\arg{name}.  If no object is associated to this name it is the
constant \const {PCE_FAIL}.  The \arg{handle} array is an array of
anonymous (void~*) 32 bit fields that may be used by the host-language
interface to store the corresponding atom/keyword/... and/or
object-reference.  The allocated size of this array is determined by
the first argument of \cfunc{pceInitialise()}
(page~\pageref{func:pceInitialise}).

Implementations of host-languages that do not support a symbol-table
for textual constants cannot use these handles and should pass 0
to \cfunc{pceInitialise()}.  The generic Prolog interface in
\file{xpce/prolog/c/interface.c} uses one handle to save a reference
to the corresponding Prolog atom.  The generic Lisp interface uses
two handles.  One to store a Lisp keyword that corresponds to the
PCE name and one to store a Lisp structure that represents the
\arg{object} of the interface symbol.

The following functions may be used to access this table:

\begin{description}
    \cfunction{PceITFSymbol}{pceLookupHandle}{int which, hostHandle handle}
Return a pointer to the interface symbol for which symbol->handle[which]
equals \arg{handle}.  Return NULL if no such symbol exists.
    \cfunction{void}{pceRegisterName}{int which, hostHandle handle,
				      PceName name}
Create an interface symbol for \arg{name} if this does not exist and
assign \\ symbol->handle[\arg{which}] = \arg{handle};
    \cfunction{void}{pceRegisterAssoc}{int which, hostHandle handle,
				       PceObject obj}
Assign symbol->handle[\arg{which}] = \arg{handle} in the interface
symbol representing \arg{obj}.
    \cfunction{PceITFSymbol}{getITFSymbolName}{PceName name}
Return interface symbol for the given PCE \arg{name}.  If \arg{name} does
not yet have an interface symbol \cfunc{getITFSymbolName()} creates one.
\end{description}

Section~\ref{sec:examples} illustrates how the interface table may be
used.


\subsection{Converting C Data types to PCE Objects}

Data types and ``proper objects'' are exchanged and converted to their
appropriate representation by using the conversion functions
described.  below. The type `PceObject' is defined in the header file
\file{xpce/src/itf-interface.h} and refers to an anonymous PCE datum.

\begin{description}
    \cfunction{PceObject}{cToPceName}{char * text}
Return a PCE name-object for the given \arg{text}.  A PCE name-object
is a unique representation of its associated text.  If the same name
is needed several times in the interface the return-value of this
function may be saved in a global variable for later usage.  \cfunc
{cToPceName()} always succeeds.  If the name was not previously
known to PCE this function may call \cfunc{malloc()} to allocate
memory for the new name-object.  The contents of \arg{text} is
copied to PCE's data area.

Note that \cfunc{pceRegisterName()}/\cfunc{pceLookupHandle()} provide
an alternative to map host-language textual constants on PCE names.
See section~\ref{sec:examples}.

    \cfunction{PceObject}{cToPceInteger}{long value}
Convert a C-integer into a PCE integer.  PCE integers are signed
31 bit values.  \arg{cToPceInteger()} always succeeds and never
calls \cfunc{malloc()}.

    \cfunction{PceObject}{cToPceReal}{double value}
Convert a C-integer into a PCE real-object.  Real object are
garbage-collected.  Calling this function twice with the same value
will yield different instances of class real.  The function
\cfunc{cToPceReal()} always succeed and might call \cfunc{malloc()}.

    \cfunction{PceObject}{cToPceString}{char *assoc, char *text}
Convert the C char~* \arg{text} into a PCE string object.
When not NULL, \arg{assoc} will be the named reference given to the
object.  See also \cfunc{cToPceAssoc()}, \cfunc{pceNew()}.
The function \cfunc{cToPceString()} always succeed and might call
\cfunc{malloc()}.  The example below creates a string with
<-text ``Hello World!'' and reference @s:
\begin{code}
cToPceString("s", "Hello World!");
\end{code}
\end{description}

The Lisp interface defines a PCE class \class{lisp_string} for
representing Lisp strings in PCE.  The PCE interface defines the
following functions to initialise such user-defined representation
of a string.

\begin{description}
    \cfunction{PceObject}{cToPceTmpCharArray}{char *text}
Return a reference to a temporary instance of class \class{char_array}.
The `char_array <-text' field is filled with \arg{text}.  Unlike the
\cfunc{cToPceName()} and \cfunc{cToPceString()} however, the contents
of \arg{text} is {\em not} copied.  The returned object should therefore
{\em not} be modified in PCE.

The PCE interface defines a pool of 10 char_array objects used for
this purpose.  After finishing with the temporary object it should
be freed using \cfunc{donePceTmpCharArray()}.
    \cfunction{void}{donePceTmpCharArray}{PceObject}
Prepare the argument \class{char_array} object for reuse.  The argument
{\em must} be created using \cfunc{cToPceTmpCharArray()}.
\end{description}


The host language needs to define how a reference to a PCE object is
represented.  For Prolog as the host language it is usual to represent
object references as @Atom or @Integer.  See also \cite{PCE:Prolog}
and \cite{PCE:Lisp}.  The following calls may be used to convert a
reference in the host-language representation into a PCE object.

\begin{description}
    \cfunction{PceObject}{cToPceAssoc}{char * assoc}
Return the PCE object with the given name association.  If no such
object is available it returns NULL.  This call might perform
\cfunc{malloc()} and might call-back the host-language from the
PCE exception mechanism for undefined assocs.  See `@pce
<-exception_handlers'.
    \cfunction{PceObject}{cToPceReference}{unsigned long reference}
Return PCE object from an integer reference.  Returns NULL if the
given reference is not defined.  This function {\em never} performs
\cfunc{malloc()}.  Note that the detection of invalid integer
references is heuristic: PCE might return an invalid object
from an invalid integer reference.
\end{description}

Section~\ref{sec:examples} contains typical calling sequences and
examples of how to use the cToPce... functions.

\subsection{Converting PCE Objects to C Data types}

Conversions from PCE data types to C data-types are performed by 
\cfunc{pceToC()}. The declaration is:

\begin{code}
int
pceToC(obj, value)
    PceObject obj;
    PceCValue *value;
\end{code}

where \arg{PceCValue} is a union of the possible C-return-values:

\begin{code}
typedef union
{ char		character;
  long		integer;
  float 	real;
  char *	string;
  PceITFSymbol	itf_symbol;
} PceCValue;
\end{code}

The first argument of \cfunc{pceToC()} is an object obtained from one
of the principal interface functions.  \cfunc{pceToC()} stores the C
representation in the second argument.  The return value indicates
the actual type of the \arg{CValue} as shown in table~\ref{tab:pceToC}.


\begin{table}
\begin{center}
\begin{tabular}{|l|l|}
\hline
{\sl type}		& {\sl PceCValue union field}	\\
\hline
{\tt PCE_INTEGER}       & {\tt integer}			\\
{\tt PCE_REAL}          & {\tt real}			\\
{\tt PCE_NAME}          & {\tt itf_symbol}		\\
{\tt PCE_ASSOC}         & {\tt itf_symbol}		\\
{\tt PCE_REFERENCE}     & {\tt integer}			\\
\hline
\end{tabular}
\end{center}
	\caption{pceToC() return values and corresponding type}
	\label{tab:pceToC}
\end{table}



The PCE interface defines two additional C-functions to convert
text from PCE to C:

\begin{description}
    \cfunction{char *}{pceStringToC}{PceObject datum}
If the argument is an instance of the PCE class \class{string}, return
a char * representing the value of the string.  Otherwise return
\const{NULL}.
    \cfunction{char *}{pceCharArrayToC}{PceObject datum}
If the argument is an instance of the PCE class \class{char_array}, return
a char * representing the value of the string.  Otherwise return
\const{NULL}.
\end{description}

For both functions, the returned char~* is a pointer into PCE's data-area.
The contents of the character array may not be changed and the data should
be copied if it needs to be retained.


\subsection{Testing existence of objects}

Whether or not an object exists may be tested using the functions
defined below.  These calls are used by the Prolog predicate object/1
and the Lisp function \lfunc{pce-object-p}

\begin{description}
    \cfunction{int}{pceExistsAssoc}{char *assoc}
Returns non-zero iff \arg{assoc} is the name of an existing object.  This
test is different from
\begin{code}
if ( cToPceAssoc(assoc) ) ...
\end{code}
as \cfunc{cToPceAssoc()} will raise an exception if the object does not
exists, while this \cfunc{pceExistsAssoc()} simply fails in this case.
See also `@pce <-exception_handlers'.
    \cfunction{int}{pceExistsReference}{unsigned long reference}
Returns non-zero iff \arg{reference} is a valid integer reference.  This
call is equivalent to testing \cfunc{cToPceReference()} and only exists
for symetry with \cfunc{pceExistsAssoc()}.
\end{description}


\subsection{Type testing}

\begin{description}
    \cfunction{int}{pceInstanceOf}{PceObject obj, PceObject class}
Test if \arg{obj} is an instance of class \arg{class} or a subclass
thereof.  Returns \const{PCE_SUCCEED} or \const{PCE_FAIL}.
\end{description}
