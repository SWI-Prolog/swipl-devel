/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 2001 University of Amsterdam. All rights reserved.
*/

:- module(swi_ide,
	  [ prolog_ide/1		% +Action
	  ]).
:- use_module(library(pce)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module defines  the  application   @prolog_ide  and  the  predicate
prolog_ide(+Action). The major motivation is be   able  to delay loading
the IDE components to the autoloading of one single predicate.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

		 /*******************************
		 *    AUTOLOAD OF COMPONENTS	*
		 *******************************/

:- pce_image_directory(library('trace/icons')).

:- pce_autoload(prolog_debug_status, library('trace/status')).
:- pce_autoload(prolog_navigator,    library('trace/browse')).
:- pce_autoload(prolog_query_frame,  library('trace/query')).



		 /*******************************
		 *	      TOPLEVEL		*
		 *******************************/

%	prolog_ide(+Action)
%
%	Invoke an action on the (SWI-)Prolog IDE application.  This is a
%	predicate to ensure optimal delaying of loading and object creation
%	for accessing the various components of the Prolog Integrated
%	Development Environment.

prolog_ide(Action) :-
	send(@prolog_ide, Action).


		 /*******************************
		 *	   THE IDE CLASS	*
		 *******************************/

:- pce_global(@prolog_ide, new(prolog_ide)).

:- pce_begin_class(prolog_ide, application, "Prolog IDE application").

initialise(IDE) :->
	"Create as service application"::
	send_super(IDE, initialise, prolog_ide),
	send(IDE, kind, service).

open_debug_status(IDE) :->
	"Open/show the status of the debugger"::
	(   get(IDE, member, prolog_debug_status, W)
	->  send(W, expose)
	;   send(prolog_debug_status(IDE), open)
	).

open_navigator(IDE, Where:[directory|source_location]) :->
	"Open Source Navigator"::
	(   send(Where, instance_of, directory)
	->  get(IDE, navigator, Where, Navigator),
	    send(Navigator, directory, directory)
	;   send(Where, instance_of, source_location)
	->  get(Where, file_name, File),
	    file_directory_name(File, Dir),
	    get(Where, line_no, Line),
	    (	integer(Line)
	    ->	LineNo = Line
	    ;	LineNo = 1
	    ),
	    get(IDE, navigator, Dir, Navigator),
	    send(Navigator, goto, File, LineNo)
	;   get(IDE, navigator, directory('.'), Navigator)
	),
	send(Navigator, expose).


navigator(IDE, Dir:[directory], Navigator:prolog_navigator) :<-
	"Create or return existing navigator"::
	(   get(IDE, member, prolog_navigator, Navigator)
	->  true
	;   new(Navigator, prolog_navigator(Dir)),
	    send(Navigator, application, IDE)
	).

open_query_window(IDE) :->
	"Open window to enter a query"::
	(   get(IDE, member, prolog_query_frame, QF)
	->  true
	;   new(QF, prolog_query_frame),
	    send(QF, application, IDE)
	),
	send(QF, expose).

:- pce_end_class(prolog_ide).
