/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (C): 1985-2002, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
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
