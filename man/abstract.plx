SWI-Prolog is a comprehensive and portable implementation of the Prolog
programming language. SWI-Prolog aims to be a robust and scalable
implementation supporting a wide range of applications. In particular,
it ships with a wide range of interface libraries, providing interfaces
to other languages, databases, graphics and networking. It provides
extensive support for managing HTML/SGML/XML, JSON, YAML and RDF
documents. The system is particularly suited for server applications due
to robust support for multithreading and HTTP server libraries.

SWI-Prolog extends Prolog with \jargon{tabling} (SGL resolution).
Tabling provides better termination properties and avoids repetitive
recomputation. Following XSB, SWI-Prolog's tabling supports sound
negation using the \jargon{Well Founded Semantics}. \jargon{Incremental
tabling} supports usage as a \jargon{Deductive database}.

SWI-Prolog is designed in the `Edinburgh tradition'. In addition to the
ISO Prolog standard it is largely compatible to Quintus, SICStus and YAP
Prolog.  SWI-Prolog provides a compatibility framework developed in
cooperation with YAP and instantiated for YAP, SICStus, IF/Prolog and
XSB.

SWI-Prolog aims at providing a rich development environment, including
extensive editor support, graphical source-level debugger, autoloading,
a `make' facility to reload edited files and much more. GNU-Emacs,
SWI-Prolog editor for Windows, the PDT plugin for Eclipse or a Visual
Studio Code plugin provide alternative environments.
\href{https://swish.swi-prolog.org}{SWISH} provides a web based
environment.

This document gives an overview of the features, system limits and
built-in predicates.
