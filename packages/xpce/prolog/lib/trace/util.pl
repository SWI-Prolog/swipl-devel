/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

:- module(prolog_trace_utils,
	  [ trace_setting/2,		% ?Name, ?Value
	    trace_setting/3,		% +Name, -Old, +New
	    setting/2,			% +Name, +Value

	    canonical_source_file/2,	% +RawFile, -CanonicalFile

	    find_source/3,		% +Head, -File|TextBuffer, -Line

	    debug/2			% +Fmt, +Args
	  ]).
:- use_module(clause).

		 /*******************************
		 *	     SETTINGS		*
		 *******************************/

:- dynamic
	setting/2.			% what, value

%setting(verbose, 	   true).	% debugging this package
setting(verbose, 	   false).	% debugging this package
setting(active,	 	   true).	% actually use this tracer
setting(show_unbound,	   false).	% show unbound variables
setting(cluster_variables, true).	% cluster variables
setting(list_max_clauses,  25).		% only list this amount of clauses
setting(stack_depth, 	   10).		% # frames shown
setting(choice_depth,	   10).		% # choice-points shown
setting(term_depth,	   2).		% nesting for printing terms
setting(auto_raise,	   true).	% automatically raise the frame
setting(console_actions,   false).	% map actions from the console
setting(use_pce_emacs,	   true).	% use PceEmacs editor

trace_setting(Name, Value) :-
	setting(Name, Value).
trace_setting(Name, Old, New) :-
	setting(Name, Old), !,
	retractall(setting(Name, Old)),
	assert(setting(Name, New)).


		 /*******************************
		 *      SOURCE LOCATIONS	*
		 *******************************/

%	find_source(+HeadTerm, -File, -Line)
%
%	Finds the source-location of the predicate.  If the predicate
%	is not defined, it will list the predicate on @dynamic_source_buffer
%	and return this buffer.	

find_source(Predicate, File, Line) :-
	predicate_property(Predicate, file(File)),
	predicate_property(Predicate, line_count(Line)), !.
find_source(Predicate, File, 1) :-
	debug('No source for ~p~n', Predicate),
	File = @dynamic_source_buffer,
	send(File, clear),
	pce_open(File, write, Fd),
	telling(Old), set_output(Fd),
	list_predicate(Predicate),
	tell(Old),
	close(Fd).

list_predicate(Predicate) :-
	predicate_property(Predicate, foreign), !,
	predicate_name(user:Predicate, PrintName),
	send(@dynamic_source_buffer, attribute, comment,
	     string('Can''t show foreign predicate %s', PrintName)).
list_predicate(Predicate) :-
	predicate_name(user:Predicate, PrintName),
	setting(list_max_clauses, Max),
	'$get_predicate_attribute'(Predicate, number_of_clauses, Num),
	(   Num > Max
	->  Upto is Max - 1,
	    list_clauses(Predicate, 1, Upto),
	    Skipped is Num - Max,
	    format('~n% <skipped ~d clauses>~n~n', [Skipped]),
	    list_clauses(Predicate, Num, Num),
	    send(@dynamic_source_buffer, attribute, comment,
		 string('Partial decompiled listing of %s', PrintName))
	;   list_clauses(Predicate, 1, Num),
	    send(@dynamic_source_buffer, attribute, comment,
		 string('Decompiled listing of %s', PrintName))
	).
	    

list_clauses(Predicate, From, To) :-
	between(From, To, Nth),
	    nth_clause(Predicate, Nth, Ref),
	    clause(RawHead, Body, Ref),
	    strip_module(user:RawHead, Module, Head),
	    tag_module(Module),
	    portray_clause((Head :- Body)),
	fail.
list_clauses(_, _, _).

tag_module(Module) :-
	prolog_clause:hidden_module(Module), !. % dubious
tag_module(Module) :-
	format('~q:', Module).

		 /*******************************
		 *	     SOURCE FILE	*
		 *******************************/

%	canonical_source_file(+Raw, -Cononical)
%
%	Determine the internal canonical filename from a raw file.

canonical_source_file(Source, File) :-
	absolute_file_name(Source, Canonical),
	(   source_file(Canonical)
	->  File = Canonical
	;   file_base_name(Source, Base),
	    source_file(File),
	    file_base_name(File, Base),
	    same_file(Source, File)
	->  true
	;   File = Source		% system source files
	).


		 /*******************************
		 *	       DEBUG		*
		 *******************************/

debug(Fmt, Args) :-
	setting(verbose, true), !,
	format(Fmt, Args).
debug(_, _).
