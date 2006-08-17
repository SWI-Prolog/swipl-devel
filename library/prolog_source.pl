/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2005, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(prolog_source,
	  [ prolog_read_source_term/4,	% +Stream, -Term, -Expanded, +Options
	    prolog_open_source/2,	% +Source, -Stream
	    prolog_close_source/1,	% +Stream
	    prolog_canonical_source/2	% +Spec, -Id
	  ]).
:- use_module(operators).
:- use_module(debug).

/** <module> Examine Prolog source-files

The modile prolog_source.pl provides predicates to  open, close and read
terms from Prolog source-files. This may  seem   easy,  but  there are a
couple of problems that must be taken care of.

	* Source files may start with #!, supporting PrologScript
	* Embeded operators declarations must be taken into account
	* Style-check options must be taken into account
	* Operators and style-check options may be implied by directives
	* On behalf of the development environment we also wish to
	  parse PceEmacs buffers

This module concentrates these issues  in   a  single  library. Intended
users of the library are:

	$ prolog_xref.pl : The Prolog cross-referencer
	$ PceEmacs :	   Emacs syntax-colouring
	$ PlDoc :	   The documentation framework
*/

:- thread_local
	open_source/2.		% Stream, State

:- multifile
	requires_library/2,
	prolog:xref_source_identifier/2,	% +Source, -Id
	prolog:xref_open_source/2.		% +SourceId, -Stream


		 /*******************************
		 *	     READING		*
		 *******************************/

%%	prolog_read_source_term(+In, -Term, -Expanded, +Options) is det.
%
%	Read a term from a Prolog source-file.  Options is a option list
%	as normally provided to read_term/3.
%	
%	@param Term	Term read
%	@param Expanded	Result of term-expansion on the term

prolog_read_source_term(In, Term, Expanded, Options) :-
	'$set_source_module'(SM, SM),
	read_term(In, Term,
		  [ module(SM)
		  | Options
		  ]),
	expand(Term, Expanded),
	update_state(Expanded).

expand(Var, Var) :-
	var(Var), !.
expand(Term, _) :-
	requires_library(Term, Lib),
	ensure_loaded(user:Lib),
	fail.
expand('$:-'(X), '$:-'(X)) :- !,	% boot module
	style_check(+dollar).
expand(Term, Expanded) :-
	expand_term(Term, Expanded).

%%	requires_library(+Term, -Library)
%
%	known expansion hooks.  May be expanded as multifile predicate.

requires_library((:- emacs_begin_mode(_,_,_,_,_)), library(emacs_extend)).
requires_library((:- draw_begin_shape(_,_,_,_)), library(pcedraw)).

%%	update_state(+Expanded) is det.
%
%	Update operators and style-check options from the expanded term.

update_state([]) :- !.
update_state([H|T]) :- !,
	update_state(H),
	update_state(T).
update_state(:- Directive) :- !,
	update_directive(Directive).
update_state(?- Directive) :- !,
	update_directive(Directive).
update_state(_).

update_directive(module(Module, Public)) :- !,
	'$set_source_module'(_, Module),
	public_operators(Public).
update_directive(op(P,T,N)) :- !,
	'$set_source_module'(SM, SM),
	push_op(P,T,SM:N).
update_directive(style_check(Style)) :-
	style_check(Style), !.
update_directive(_).

public_operators([]).
public_operators([H|T]) :- !,
	(   H = op(_,_,_)
	->  update_directive(H)
	;   true
	),
	public_operators(T).


		 /*******************************
		 *	     SOURCES		*
		 *******************************/

%%	prolog_open_source(+CanonicalId:atomic, -Stream:stream) is det.
%	
%	Open     source     with     given     canonical     id     (see
%	prolog_canonical_source/2)  and  remove  the  #!  line  if  any.
%	Streams  opened  using  this  predicate  must  be  closed  using
%	prolog_close_source/1. Typically using the skeleton below. Using
%	this   skeleton,   operator   and    style-check   options   are
%	automatically restored to the values before opening the source.
%	
%	==
%	process_source(Src) :-
%		prolog_open_source(Src, In),
%		call_cleanup(process(Src), prolog_close_source(In)).
%	==

prolog_open_source(Src, Fd) :-
	(   prolog:xref_open_source(Src, Fd)
	->  true
	;   open(Src, read, Fd)
	),
	(   peek_char(Fd, #)		% Deal with #! script
	->  skip(Fd, 10)
	;   true
	),
	push_operators([]),
	'$set_source_module'(SM, SM),
	'$style_check'(Style, Style),
	asserta(open_source(Fd, state(Style, SM))).


%%	prolog_close_source(+In:stream) is det.
%
%	Close  a  stream  opened  using  prolog_open_source/2.  Restores
%	operator and style options.

prolog_close_source(In) :-
	pop_operators,
	(   retract(open_source(In, state(Style, SM)))
	->  '$style_check'(_, Style),
	    '$set_source_module'(_, SM)
	;   assertion(fail)
	),
	close(In).


%%	prolog_canonical_source(+SourceSpec:ground, -Id:atomic) is det.
%	
%	Given a user-specification of a source,   generate  a unique and
%	indexable  identifier  for   it.   For    files   we   use   the
%	prolog_canonical absolute filename.

prolog_canonical_source(Src, Id) :-		% Call hook
	prolog:xref_source_identifier(Src, Id), !.
prolog_canonical_source(User, user) :-
	User == user, !.
prolog_canonical_source(Source, Src) :-
	absolute_file_name(Source,
			   [ file_type(prolog),
			     access(read),
			     file_errors(fail)
			   ],
			   Src), !.
prolog_canonical_source(Source, Src) :-
	var(Source), !,
	Src = Source.
