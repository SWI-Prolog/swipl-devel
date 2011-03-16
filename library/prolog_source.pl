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
	    prolog_canonical_source/2,	% +Spec, -Id

	    file_name_on_path/2,	% +File, -PathSpec
	    file_alias_path/2,		% ?Alias, ?Dir
	    path_segments_atom/2	% ?Segments, ?Atom
	  ]).
:- use_module(operators).
:- use_module(debug).
:- use_module(option).

/** <module> Examine Prolog source-files

This module provides predicates  to  open,   close  and  read terms from
Prolog source-files. This may seem  easy,  but   there  are  a couple of
problems that must be taken care of.

	* Source files may start with #!, supporting PrologScript
	* Embedded operators declarations must be taken into account
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
	open_source/2,		% Stream, State
	mode/2.			% Stream, Data

:- multifile
	requires_library/2,
	prolog:xref_source_identifier/2,	% +Source, -Id
	prolog:xref_source_time/2,		% +Source, -Modified
	prolog:xref_open_source/2.		% +SourceId, -Stream


		 /*******************************
		 *	     READING		*
		 *******************************/

%%	prolog_read_source_term(+In, -Term, -Expanded, +Options) is det.
%
%	Read a term from a Prolog source-file.  Options is a option list
%	that is forwarded to read_term/3.  In addition, it accepts:
%
%	    * process_comment(+Boolean)
%	    If =true=, process structured comments for PlDoc
%
%	@param Term	Term read
%	@param Expanded	Result of term-expansion on the term

prolog_read_source_term(In, Term, Expanded, Options) :-
	read_source_term(In, Term, Options),
	expand(Term, In, Expanded),
	update_state(Expanded).

:- multifile
	prolog:comment_hook/3.

read_source_term(In, Term, Options) :-
	'$get_predicate_attribute'(prolog:comment_hook(_,_,_),
				   number_of_clauses, N),
	N > 0,
	option(process_comment(true), Options), !,
	select_option(term_position(TermPos), Options, Options1, _),
	'$set_source_module'(SM, SM),
	read_term(In, Term,
		  [ comments(Comments),
		    module(SM),
		    term_position(TermPos)
		  | Options1
		  ]),
	(   catch(prolog:comment_hook(Comments, TermPos, Term), E,
		  print_message(error, E))
	->  true
	;   true
	).
read_source_term(In, Term, Options) :-
	'$set_source_module'(SM, SM),
	read_term(In, Term,
		  [ module(SM)
		  | Options
		  ]).


expand(Var, _, Var) :-
	var(Var), !.
expand(Term, _, Term) :-
	no_expand(Term), !.
expand(Term, _, _) :-
	requires_library(Term, Lib),
	ensure_loaded(user:Lib),
	fail.
expand(Term, In, Term) :-
	chr_expandable(Term, In), !.
expand(Term, _, Expanded) :-
	expand_term(Term, Expanded).

no_expand((:- if(_))).
no_expand((:- elif(_))).
no_expand((:- else)).
no_expand((:- endif)).
no_expand((:- require(_))).

chr_expandable((:- chr_constraint(_)), In) :-
	add_mode(In, chr).
chr_expandable((handler(_)), In) :-
	mode(In, chr).
chr_expandable((rules(_)), In) :-
	mode(In, chr).
chr_expandable(<=>(_, _), In) :-
	mode(In, chr).
chr_expandable(@(_, _), In) :-
	mode(In, chr).
chr_expandable(==>(_, _), In) :-
	mode(In, chr).
chr_expandable(pragma(_, _), In) :-
	mode(In, chr).
chr_expandable(option(_, _), In) :-
	mode(In, chr).

add_mode(Stream, Mode) :-
	mode(Stream, Mode), !.
add_mode(Stream, Mode) :-
	asserta(mode(Stream, Mode)).

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
update_state((:- Directive)) :- !,
	update_directive(Directive).
update_state((?- Directive)) :- !,
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
update_directive(expects_dialect(sicstus)) :-
	style_check(-atom), !.
update_directive(use_module(Spec)) :-
	catch(module_decl(Spec, Public), _, fail), !,
	public_operators(Public).
update_directive(_).

public_operators([]).
public_operators([H|T]) :- !,
	(   nonvar(H),
	    H = op(_,_,_)
	->  update_directive(H)
	;   true
	),
	public_operators(T).

module_decl(Spec, Decl) :-
	absolute_file_name(Spec, Path,
			   [ file_type(prolog),
			     file_errors(fail),
			     access(read)
			   ]),
	setup_call_cleanup(prolog_open_source(Path, In),
			   read(In, (:- module(_, Decl))),
			   close(In)).


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
	'$save_lex_state'(LexState),
	asserta(open_source(Fd, state(LexState, SM))).


%%	prolog_close_source(+In:stream) is det.
%
%	Close  a  stream  opened  using  prolog_open_source/2.  Restores
%	operator and style options. If the stream   has not been read to
%	the end, we call expand_term(end_of_file,  _) to allow expansion
%	modules to clean-up.

prolog_close_source(In) :-
	(   at_end_of_stream(In)
	->  true
	;   ignore(catch(expand(end_of_file, In, _), _, true))
	),
	pop_operators,
	retractall(mode(In, _)),
	(   retract(open_source(In, state(LexState, SM)))
	->  '$restore_lex_state'(LexState),
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
	var(Source), !,
	Src = Source.
prolog_canonical_source(Source, Src) :-
	absolute_file_name(Source,
			   [ file_type(prolog),
			     access(read),
			     file_errors(fail)
			   ],
			   Src), !.


%%	file_name_on_path(+File:atom, -OnPath) is det.
%
%	True if OnPath a description of File   based  on the file search
%	path. This performs the inverse of absolute_file_name/3.

file_name_on_path(Path, ShortId) :-
	(   file_alias_path(Alias, Dir),
	    atom_concat(Dir, Local, Path)
	->  (   Alias == '.'
	    ->  ShortId = Local
	    ;   file_name_extension(Base, pl, Local)
	    ->  ShortId =.. [Alias, Base]
	    ;   ShortId =.. [Alias, Local]
	    )
	;   ShortId = Path
	).


%%	file_alias_path(-Alias, ?Dir) is nondet.
%
%	True if file Alias points to Dir.  Multiple solutions are
%	generated with the longest directory first.

:- dynamic
	alias_cache/2.

file_alias_path(Alias, Dir) :-
	(   alias_cache(_, _)
	->  true
	;   build_alias_cache
	),
	(   nonvar(Dir)
	->  ensure_slash(Dir, DirSlash),
	    alias_cache(Alias, DirSlash)
	;   alias_cache(Alias, Dir)
	).

build_alias_cache :-
	findall(t(DirLen, AliasLen, Alias, Dir),
		search_path(Alias, Dir, AliasLen, DirLen), Ts),
	sort(Ts, List0),
	reverse(List0, List),
	forall(member(t(_, _, Alias, Dir), List),
	       assert(alias_cache(Alias, Dir))).

search_path('.', Here, 999, DirLen) :-
	working_directory(Here0, Here0),
	ensure_slash(Here0, Here),
	atom_length(Here, DirLen).
search_path(Alias, Dir, AliasLen, DirLen) :-
	user:file_search_path(Alias, _),
	Spec =.. [Alias,'.'],
	atom_length(Alias, AliasLen0),
	AliasLen is 1000 - AliasLen0,	% must do reverse sort
	absolute_file_name(Spec, Dir0,
			   [ file_type(directory),
			     access(read),
			     solutions(all),
			     file_errors(fail)
			   ]),
	ensure_slash(Dir0, Dir),
	atom_length(Dir, DirLen).

ensure_slash(Dir, Dir) :-
	sub_atom(Dir, _, _, 0, /), !.
ensure_slash(Dir0, Dir) :-
	atom_concat(Dir0, /, Dir).


%%	path_segments_atom(+Segments, -Atom) is det.
%%	path_segments_atom(-Segments, +Atom) is det.
%
%	Translate between a path  represented  as   a/b/c  and  an  atom
%	representing the same path. For example:
%
%	  ==
%	  ?- path_segments_atom(a/b/c, X).
%	  X = 'a/b/c'.
%	  ?- path_segments_atom(S, 'a/b/c'), display(S).
%	  /(/(a,b),c)
%	  S = a/b/c.
%	  ==
%
%	This predicate is part of  the   Prolog  source  library because
%	SWI-Prolog  allows  writing  paths   as    /-nested   terms  and
%	source-code analysis programs often need this.

path_segments_atom(Segments, Atom) :-
	var(Atom), !,
	(   atomic(Segments)
	->  Atom = Segments
	;   segments_to_list(Segments, List, [])
	->  atomic_list_concat(List, /, Atom)
	;   throw(error(type_error(file_path, Segments), _))
	).
path_segments_atom(Segments, Atom) :-
	atomic_list_concat(List, /, Atom),
	parts_to_path(List, Segments).

segments_to_list(Var, _, _) :-
	var(Var), !, fail.
segments_to_list(A/B, H, T) :-
	segments_to_list(A, H, T0),
	segments_to_list(B, T0, T).
segments_to_list(A, [A|T], T) :-
	atomic(A).

parts_to_path([One], One) :- !.
parts_to_path(List, More/T) :-
	(   append(H, [T], List)
	->  parts_to_path(H, More)
	).


