\section{Calling the PCE Virtual Machine}	\label{sec:functions}

All functionality in PCE is accessible through three instructions which
together are called the PCE virtual machine.  The conversion functions
described in the previous section are used to create the appropriate
arguments for a call to one of the virtual machine instructions.


\begin{description}
    \cfunction{int}{pceSend}{PceObject rec, PceObject sel, int argc,
			     PceObject argv[]}
Invoke a send-operation \arg{sel} on \arg{rec}.  The arguments are
passed as a C-array of PceObject's holding \arg{argc} PCE object.
The PceObject arguments should be legal PCE objects returned by one
of the other interface functions.

The return value of \cfunc{pceSend()} is \const{PCE_SUCCEED}, indicating the
message was successfully received by receiver and \const{PCE_FAIL}
otherwise.
    \cfunction{PceObject}{pceGet}{PceObject rec, PceObject sel, int argc,
			          PceObject argv[]}
Invoke a get-operation \arg{sel} on \arg{rec}.  The arguments are
passed as a C-array of PceObject's holding \arg{argc} PCE object.
The PceObject arguments should be legal PCE objects returned by one
of the other interface functions.

The return value is \const{PCE_FAIL} if the get-operation failed or a
PCE datum otherwise.  If the return value is not \const{PCE_FAIL} it
may be passed to \cfunc{pceToC()} to convert it into a representation
suitable for the host-language.  See section~\ref{sec:examples}.
    \cfunction{PceObject}{pceNew}{char *assoc, PceObject class,
				  int argc, PceObject *argv}
Create a PCE object.  The argument \arg{assoc} defines the name
association used for this object or NULL if the object should not be
named (i.e.\ has an integer reference).  \arg{Class} is either a PCE
class or a PCE name that represents a class-name.  The \arg{argc},
\arg{argv} pair provides the initialisation arguments as with
\cfunc{pceSend()}.
\end{description}
