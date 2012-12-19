/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2012, University of Amsterdam
			      Vu University Amsterdam

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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(prolog_source,
	  [ prolog_read_source_term/4,	% +Stream, -Term, -Expanded, +Options
	    read_source_term_at_location/3, %Stream, -Term, +Options
	    prolog_open_source/2,	% +Source, -Stream
	    prolog_close_source/1,	% +Stream
	    prolog_canonical_source/2,	% +Spec, -Id

	    file_name_on_path/2,	% +File, -PathSpec
	    file_alias_path/2,		% ?Alias, ?Dir
	    path_segments_atom/2,	% ?Segments, ?Atom
	    directory_source_files/3	% +Dir, -Files, +Options
	  ]).
:- use_module(operators).
:- use_module(lists).
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

	$ prolog_xref.pl :   The Prolog cross-referencer
	$ prolog_clause.pl : Get details about (compiled) clauses
	$ prolog_colour.pl : Colourise source-code
	$ PceEmacs :	     Emacs syntax-colouring
	$ PlDoc :	     The documentation framework
*/

:- thread_local
	open_source/2,		% Stream, State
	mode/2.			% Stream, Data

:- multifile
	requires_library/2,
	prolog:xref_source_identifier/2, % +Source, -Id
	prolog:xref_source_time/2,	 % +Source, -Modified
	prolog:xref_open_source/2,	 % +SourceId, -Stream
	prolog:alternate_syntax/4.	 % Syntax, +Module, -Setup, -Restore


:- predicate_options(prolog_read_source_term/4, 4,
		     [ pass_to(system:read_clause/3, 3)
		     ]).
:- predicate_options(read_source_term_at_location/3, 3,
		     [ line(integer),
		       offset(integer),
		       module(atom),
		       operators(list),
		       error(-any),
		       pass_to(system:read_term/3, 3)
		     ]).
:- predicate_options(directory_source_files/3, 3,
		     [ recursive(boolean),
		       if(oneof([true,loaded])),
		       pass_to(system:absolute_file_name/3,3)
		     ]).


		 /*******************************
		 *	     READING		*
		 *******************************/

%%	prolog_read_source_term(+In, -Term, -Expanded, +Options) is det.
%
%	Read a term from a Prolog source-file.  Options is a option list
%	that is forwarded to read_clause/3.
%
%	This predicate is intended to read the   file from the start. It
%	tracks directives to update its notion of the currently effectie
%	syntax (e.g., declared operators).
%
%	@param Term	Term read
%	@param Expanded	Result of term-expansion on the term
%	@see   read_source_term_at_location/3 for reading at an
%	       arbitrary location.

prolog_read_source_term(In, Term, Expanded, Options) :-
	read_clause(In, Term, Options),
	expand(Term, In, Expanded),
	update_state(Expanded).

:- public
	expand/3.			% Used by Prolog colour

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


%%	read_source_term_at_location(+Stream, -Term, +Options) is semidet.
%
%	Try to read a Prolog term form   an  arbitrary location inside a
%	file. Due to Prolog's dynamic  syntax,   e.g.,  due  to operator
%	declarations that may change anywhere inside   the file, this is
%	theoreticaly   impossible.   Therefore,   this    predicate   is
%	fundamentally _heuristic_ and may fail.   This predicate is used
%	by e.g., clause_info/4 and by  PceEmacs   to  colour the current
%	clause.
%
%	This predicate has two ways to  find   the  right syntax. If the
%	file is loaded, it can be  passed   the  module using the module
%	option. This deals with  module  files   that  define  the  used
%	operators globally for  the  file.  Second,   there  is  a  hook
%	prolog:alternate_syntax/4 that can be used to temporary redefine
%	the syntax.
%
%	The options below are processed in   addition  to the options of
%	read_term/3. Note that  the  =line=   and  =offset=  options are
%	mutually exclusive.
%
%	  * line(+Line)
%	  If present, start reading at line Line.
%	  * offset(+Characters)
%	  Use seek/4 to go to the indicated location.  See seek/4
%	  for limitations of seeking in text-files.
%	  * module(+Module)
%	  Use syntax from the given module. Default is the current
%	  `source module'.
%	  * operators(+List)
%	  List of additional operator declarations to enforce while
%	  reading the term.
%	  * error(-Error)
%	  If no correct parse can be found, unify Error with a term
%	  Offset:Message that indicates the (character) location of
%	  the error and the related message.  Adding this option
%	  makes read_source_term_at_location/3 deterministic.
%
%	@see Use read_source_term/4 to read a file from the start.
%	@see prolog:alternate_syntax/4 for locally scoped operators.

:- thread_local
	last_syntax_error/2.		% location, message

read_source_term_at_location(Stream, Term, Options) :-
	retractall(last_syntax_error(_,_)),
	seek_to_start(Stream, Options),
	stream_property(Stream, position(Here)),
	'$set_source_module'(DefModule, DefModule),
	option(module(Module), Options, DefModule),
	option(operators(Ops), Options, []),
	alternate_syntax(Syntax, Module, Setup, Restore),
	set_stream_position(Stream, Here),
	peek_char(Stream, X),
	debug(read, 'Using syntax ~w (c=~w)', [Syntax, X]),
	push_operators(Module:Ops),
	Setup,
	asserta(user:thread_message_hook(_,_,_), Ref), % silence messages
	catch(read_term(Stream, Term0,
			[ module(Module)
			| Options
			]),
	      Error,
	      true),
	erase(Ref),
	Restore,
	pop_operators,
	(   var(Error)
	->  Term = Term0
	;   assert_error(Error, Options),
	    fail
	).
read_source_term_at_location(_, _, Options) :-
	option(error(Error), Options), !,
	setof(CharNo:Msg, retract(last_syntax_error(CharNo, Msg)), Pairs),
	last(Pairs, Error).

assert_error(Error, Options) :-
	option(error(_), Options), !,
	(   (   Error = error(syntax_error(Id),
			      stream(_S1, _Line1, _LinePos1, CharNo))
	    ;	Error = error(syntax_error(Id),
			      file(_S2, _Line2, _LinePos2, CharNo))
	    )
	->  message_to_string(error(syntax_error(Id), _), Msg),
	    assertz(last_syntax_error(CharNo, Msg))
	;   debug(read, 'Error: ~q', [Error]),
	    throw(Error)
	).
assert_error(_, _).


%%	alternate_syntax(?Syntax, +Module, -Setup, -Restore) is nondet.
%
%	Define an alternative  syntax  to  try   reading  a  term  at an
%	arbitrary location in module Module.
%
%	Calls the hook prolog:alternate_syntax/4 with the same signature
%	to allow for user-defined extensions.
%
%	@param	Setup is a deterministic goal to enable this syntax in
%		module.
%	@param	Restore is a deterministic goal to revert the actions of
%		Setup.

alternate_syntax(prolog, _, true,  true).
alternate_syntax(Syntax, M, Setup, Restore) :-
	prolog:alternate_syntax(Syntax, M, Setup, Restore).


%%	seek_to_start(+Stream, +Options) is det.
%
%	Go to the location from where to start reading.

seek_to_start(Stream, Options) :-
	option(line(Line), Options), !,
	seek(Stream, 0, bof, _),
	seek_to_line(Stream, Line).
seek_to_start(Stream, Options) :-
	option(offset(Start), Options), !,
	seek(Stream, Start, bof, _).
seek_to_start(_, _).

%%	seek_to_line(+Stream, +Line)
%
%	Seek to indicated line-number.

seek_to_line(Fd, N) :-
	N > 1, !,
	skip(Fd, 10),
	NN is N - 1,
	seek_to_line(Fd, NN).
seek_to_line(_, _).


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


%%	prolog_canonical_source(+SourceSpec:ground, -Id:atomic) is semidet.
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
	absolute_file_name(Source, Src,
			   [ file_type(prolog),
			     access(read),
			     file_errors(fail)
			   ]), !.


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
	Alias \== autoload,
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

%%	directory_source_files(+Dir, -Files, +Options) is det.
%
%	True when Files is a sorted list  of Prolog source files in Dir.
%	Options:
%
%	  * recursive(boolean)
%	  If =true= (default =false=), recurse into subdirectories
%	  * if(Condition)
%	  If =true= (default =loaded=), only report loaded files.
%
%	Other  options  are  passed    to  absolute_file_name/3,  unless
%	loaded(true) is passed.

directory_source_files(Dir, SrcFiles, Options) :-
	option(if(loaded), Options, loaded), !,
	absolute_file_name(Dir, AbsDir, [file_type(directory), access(read)]),
	(   option(recursive(true), Options)
	->  ensure_slash(AbsDir, Prefix),
	    findall(F, (  source_file(F),
			  sub_atom(F, 0, _, _, Prefix)
		       ),
		    SrcFiles)
	;   findall(F, ( source_file(F),
			 file_directory_name(F, AbsDir)
		       ),
		    SrcFiles)
	).
directory_source_files(Dir, SrcFiles, Options) :-
	absolute_file_name(Dir, AbsDir, [file_type(directory), access(read)]),
	directory_files(AbsDir, Files),
	phrase(src_files(Files, AbsDir, Options), SrcFiles).

src_files([], _, _) -->
	[].
src_files([H|T], Dir, Options) -->
	{ file_name_extension(_, Ext, H),
	  user:prolog_file_type(Ext, prolog),
	  \+ user:prolog_file_type(Ext, qlf),
	  directory_file_path(Dir, H, File0),
	  absolute_file_name(File0, File,
			     [ file_errors(fail)
			     | Options
			     ])
	}, !,
	[File],
	src_files(T, Dir, Options).
src_files([H|T], Dir, Options) -->
	{ \+ special(H),
	  option(recursive(true), Options),
	  directory_file_path(Dir, H, SubDir),
	  exists_directory(SubDir), !,
	  catch(directory_files(SubDir, Files), _, fail)
	}, !,
	src_files(Files, SubDir, Options),
	src_files(T, Dir, Options).
src_files([_|T], Dir, Options) -->
	src_files(T, Dir, Options).

special(.).
special(..).
