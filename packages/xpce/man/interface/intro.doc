\section{Introduction}

XPCE is an object-oriented environment used in cooperation with a
symbolic programming language.  The combination provides a hybrid
environment with a small but powerful and transparent interface.  In
this document we describe the interface of PCE to a symbolic
programming language.

PCE can be viewed as a three instruction virtual machine for
object-oriented programming.  PCE can be connected to an existing
programming language (called the ``host language'' in this document)
if the host language provides:


\begin{itemize}
\item
    An interface with the PCE virtual machine instructions.  What PCE
    expects the language to provide is described in this document.  Basic
    assumptions are the host language: (a) has some representation for
    integers, floating point numbers and character arrays; (b) can 
    call C-functions; and (c) allows predicates/functions to be 
    called dynamically.  The last requirement implies the host language is
    symbolic (e.g. Prolog, Lisp, and so forth).
\item
    A programming interface for application programmers using the host
    language.  In general, the language must define how the PCE virtual
    machine is made available to programmers.  An example language
    extension for Prolog is given in \cite{PCE:Prolog,PCE:Lisp}.  Not
    further addressed in this document.
\end{itemize}

The interface consists of a small set of functions in the C language
that make PCE accessible from the host language.  This document
describes these functions and how they are used.

Section~\ref{sec:data} gives the functions for converting between PCE
and host language data.  Section~\ref{sec:functions} gives the
functions allowing the host language to call the PCE virtual machine.
Section~\ref{sec:callback} describes how PCE call the host
language and section~\ref{sec:event} is about the control-structure
of hybrid environment.


\subsection*{Acknowledgements}

The development of XPCE was  started by Anjo Anjewierden.  The package
was  then called PCE.  He designed  and implemented  version  1 and 2.
Version 3 is the result of a joint effort from  Anjo Anjewierden and
Jan Wielemaker.

Richard O'Keefe at Quintus Computer Systems suggested the ``language
independent approach'' this specification embodies.  He also pointed
out the original specification would not work with Quintus Prolog
due to the fact that pointers (to Prolog terms) where exchanged.  This
specification attempts to avoid any such host language dependent
features.

XPCE, offering support for X-windows and user-defined classes, has
been implemented by the Jan Wielemaker.  The implementation of
user-defined classes was initiated when he was guest at SERC (Software
Engineering Research Centre).  Gert Florijn has contributed in the
initial discussions on user-defined classes.  Frans Heeman has been
the first user.

(X)PCE has been used by many people.  They have often been puzzled by
bugs, incompatibilities with older versions, etc.  I would like to
thank them for their patience and remarks.

