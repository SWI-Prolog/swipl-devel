/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1999 University of Amsterdam. All rights reserved.
*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The module sp_compat defines a  compatibility   layer  for  running XPCE
under SICStus. It provides all predicates  that   are  used in XPCE, its
demo programs and development environment and   are not part of SICStus.
These predicates are imported into the XPCE modules using require/1.

This code can only be distributed as part of XPCE.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- module(sp_compat,
	  [ catch/3,			% ISO catch/throw
	    throw/1,
	    between/3,			% between(+Low, +High, ?Int)
	    pow/3,			% A is X**Y (quintus compatibility)
	    sqrt/2,
	    ceiling/2,
	    strip_module/3,		% +Term, -Qualifier, -Rest
	    genarg/3,			% ?Index, +Term, ?Arg
	    forall/2,			% :Cond, :Action
	    gensym/2,			% +Id, -Unique
	    flatten/2,			% +NestedList, -FlatList
	    list_to_set/2,		% Remove duplicates
	    nth1/3,			% sicstus nth/3
	    subtract/3,			% +List, -DeleteList, -Rest
	    subset/2,			% +Set, +Subset (test for subset)
	    maplist/2,			% :Goal, +InList, -OutList
	    maplist/3,			% :Goal, +InList, -OutList
	    sublist/3,			% :Goal, +InList, -OutList
	    call/2,			% :Goal, +Arg1
	    call/3,			% :Goal, +Arg1, +Arg2
	    ignore/1,			% :Goal
	    auto_call/1,		% :Goal
	    callable_predicate/1,	% :Goal
	    absolute_file_name/3,	% +Spec, +Options, -Path
	    source_location/2,		% -File, -Line (if loading)
	    is_absolute_file_name/1,	% +Atom
	    file_directory_name/2,	% +Path, -Directory
	    file_base_name/2,		% +Path, -File
	    file_name_extension/3,	% ?Base, ?Ext, -File
	    unix/1,			% unix(argv(Argv))
	    term_to_atom/2,		% ?Term, ?Atom
	    atom_to_term/3,		% +Atom, -Term, -Bindings
	    concat/3,			% ?A, ?B, ?AB
	    concat_atom/2,		% +List, -Atom
	    concat_atom/3,		% +List, +Separator, -Atom
	    atom_length/2,		% +Atom, -Length
	    sformat/3			% -Atom, +Format, +ArgList
	  ]).
:- meta_predicate
	catch(:, +, +),
	forall(:, :),
	call(:, +),
	call(:, +, +),
	ignore(:),
	auto_call(:),
	maplist(:, +),
	maplist(:, +, -),
	sublist(:, +, -),
	callable_predicate(:).

:- require([ file_exists/2
	   , member/2
	   , memberchk/2
	   , with_output_to_chars/2
	   , format_to_chars/3
	   , append/3
	   , nth/3
	   , term_variables/2
	   ]).

		 /*******************************
		 *	      EXPANSION		*
		 *******************************/

:- op(1150, fx, discontiguous).

%	These are macro-expanded to allow for further macro-expansion
%	of theire arguments.  cf. ignore(send_super(...))

:- multifile
	user:goal_expansion/3.

user:goal_expansion(catch(G, T, R),    M, on_exception(T, M:G, R)).
user:goal_expansion(ignore(G),         M, (M:G -> true ; true)).
user:goal_expansion(initialization(G), M, sp_compat:swi_initialization(M:G)) :-
	M \== sp_compat.
user:goal_expansion(discontiguous(_),  _, true).


		 /*******************************
		 *	    EXCEPTIONS		*
		 *******************************/

catch(Goal, Catcher, Recovery) :-
	on_exception(Catcher, Goal, Recovery).

throw(Ball) :-
	raise_exception(Ball).

		 /*******************************
		 *	     ARITHMETIC		*
		 *******************************/

%	between(+Low, +High, ?Index)
%
%	Succeeds of Los <= Index <= High, or enumerate these values.

between(Low, High, Index) :-
	integer(Index), !,
	Low =< Index,
	Index =< High.
between(Low, High, Index) :-
	Low =< High,
	gen_between(Low, High, Index).

gen_between(Low, _, Low).
gen_between(Low, High, Index) :-
	NewLow is Low + 1,
	NewLow =< High,
	gen_between(NewLow, High, Index).

%	pow(X, Y, Z) Z is X to the power Y.

pow(X, Y, Z) :-
	Z is exp(X, Y).

sqrt(X, Result) :-
	Result is sqrt(X).

ceiling(X, Result) :-
	Result is ceiling(X).

%	strip_module(+Raw, -Module, -Plain)
%
%	Strips module qualifier and unify the module with Module and
%	the unqualified term with Plain.

strip_module(RT, M, T) :-
	strip_module(RT, T, M, user).

strip_module(Module:RT2, T, M, _) :-
	atom(Module), !,
	strip_module(RT2, T, M, Module).
strip_module(T, T, M, M).

%	genarg(?Index, +Term, -Arg)
%
%	non-deterministic arg/3.

genarg(Index, Term, Arg) :-
	nonvar(Index), !,
	arg(Index, Term, Arg).
genarg(Index, Term, Arg) :-
	functor(Term, _, Arity),
	gen_between(1, Arity, Index),
	arg(Index, Term, Arg).

%	forall(+Enumertor, +Condition)

forall(E, C) :-
	\+ (E, \+ C).

%	gensym(+Id, -Atom)
%
%	Generate unique atoms

:- dynamic
	gensym_counter/2.

gensym(Id, Unique) :-
	(   retract(gensym_counter(Id, Counter))
	->  UniqueId is Counter + 1
	;   UniqueId = 1
	),
	asserta(gensym_counter(Id, UniqueId)),
	concat(Id, UniqueId, Unique).

		    
		 /*******************************
		 *	       LISTS		*
		 *******************************/

%	flatten(+List1, ?List2)
%	Is true when Lis2 is a non nested version of List1.

flatten(List, FlatList) :-
	flatten_(List, [], FlatList0), !,
	FlatList = FlatList0.

flatten_(Var, Tl, [Var|Tl]) :-
	var(Var), !.
flatten_([], Tl, Tl) :- !.
flatten_([Hd|Tl], Tail, List) :-
	flatten_(Hd, FlatHeadTail, List), 
	flatten_(Tl, Tail, FlatHeadTail).
flatten_(Atom, Tl, [Atom|Tl]).

%	list_to_set(+List, ?Set)
%	is true when Set has the same element as List in the same order

list_to_set(List, Set) :-
	list_to_set_(List, Set0),
	Set = Set0.

list_to_set_([], R) :-
	close_list(R).
list_to_set_([H|T], R) :-
	memberchk(H, R), !, 
	list_to_set_(T, R).

close_list([]) :- !.
close_list([_|T]) :-
	close_list(T).

%	nth1(?Index, ?List, ?Element)

nth1(Index, List, Element) :-
	nth(Index, List, Element).

%	subtract(+Set, +Delete, -Result)
%	Delete all elements from `Set' that occur in `Delete' (a set) and
%	unify the result with `Result'.

subtract([], _, []) :- !.
subtract([E|T], D, R) :-
	memberchk(E, D), !,
	subtract(T, D, R).
subtract([H|T], D, [H|R]) :-
	subtract(T, D, R).

%	subset(+SubSet, +Set)
%	Succeeds if all elements of SubSet belong to Set as well.

subset([], _) :- !.
subset([E|R], Set) :-
	memberchk(E, Set), 
	subset(R, Set).

%	maplist(+Goal, +List1, ?List2)
%	True if Goal can succesfully be applied to all succesive pairs
%	of elements of List1 and List2.

maplist(Goal, List1, List2) :-
	maplist2(List1, List2, Goal).

maplist2([], [], _).
maplist2([Elem1|Tail1], [Elem2|Tail2], Goal) :-
	call(Goal, Elem1, Elem2), 
	maplist2(Tail1, Tail2, Goal).

%	sublist(:Goal, +List1, ?List2)
%	Succeeds if List2 unifies with a list holding those terms for wich
%	apply(Goal, Elem) succeeds.

sublist(_, [], []) :- !.
sublist(Goal, [H|T], Sub) :-
	call(Goal, H), !, 
	Sub = [H|R], 
	sublist(Goal, T, R).
sublist(Goal, [_|T], R) :-
	sublist(Goal, T, R).

%	maplist(+Goal, +List)
%
%	True if Goal can succesfully be applied on all elements of List.
%	Arguments are reordered to gain performance as well as to make
%	the predicate deterministic under normal circumstances.

maplist(Goal, List) :-
	maplist2(List, Goal).

maplist2([], _).
maplist2([Elem|Tail], Goal) :-
	call(Goal, Elem), 
	maplist2(Tail, Goal).

%	call(+Goal, +Arg ...)

call(Goal0, A0) :-
	strip_module(Goal0, M, Goal),
	Goal =.. L,
	append(L, [A0], L2),
	NewGoal =.. L2,
	M:NewGoal.
call(Goal0, A0, A1) :-
	strip_module(Goal0, M, Goal),
	Goal =.. L,
	append(L, [A0, A1], L2),
	NewGoal =.. L2,
	M:NewGoal.

%	ignore(:Goal)
%
%	Try Goal, but succeed even if goal fails.

ignore(G) :-
	(   G
	->  true
	;   true
	).

%	auto_call(:Goal)
%
%	Auto-load Goal if required and call it.

auto_call(G) :-
	current_predicate(_, G), !,
	G.
auto_call(G0) :-
	strip_module(G0, M, Goal),
	functor(Goal, Name, Arity),
	require(M:Name/Arity),
	G0.

%	callable_predicate(:Goal)
%
%	Succeeds of Goal is callable.  Id autoloading is provided, it should
%	check whether the predicate is auto-loadable.

callable_predicate(Goal) :-
	current_predicate(_, Goal).

		 /*******************************
		 *	  INITIALIZATION	*
		 *******************************/

:- op(1150, fx, initialization).

:- dynamic
	initialization_hook/1.

swi_initialization(Goal) :-
	assert(initialization_hook(Goal)),
	Goal.

run_initialization_hooks :-
	initialization_hook(Goal),
	(   catch(Goal, E, (print_message(error, E), fail))
	->  true
	;   pce_error(initialization_failed(Goal))
	),
	fail.
run_initialization_hooks.

:- initialization(run_initialization_hooks).

		 /*******************************
		 *      ABSOLUTE_FILE_NAME	*
		 *******************************/

%	absolute_file_name(+Spec, +Options, -Path)
%
%	Partial implementation of Quintus and SWI absolute_file_name/3
%	to support XPCE.

absolute_file_name(Spec, Options, Path) :-
	memberchk(solutions(all), Options), !,
	extensions(Options, Exts),
	generate_base(Spec, Base),
	member(Ext, Exts),
	file_name_extension(Base, Ext, Path),
	check_options(Options, Path).
absolute_file_name(Spec, Options, Path) :-
	extensions(Options, Exts),
	generate_base(Spec, Base),
	member(Ext, Exts),
	file_name_extension(Base, Ext, Path),
	check_options(Options, Path), !.
%	format('absolute_file_name(~w)~n~t~8|~w~n', [Spec, Path]).
absolute_file_name(_, Options, _) :-
	memberchk(file_errors(fail), Options), !.
absolute_file_name(Spec, _, _) :-
	raise_exception(error(existence_error(source_sink, Spec), _)).
		      
extensions(Options, Exts) :-
	member(extensions(Exts), Options), !.
extensions(Options, Exts) :-
	member(file_type(Type), Options),
	file_type_extensions(Type, Exts), !.
extensions(_, ['']).

file_type_extensions(prolog, [pl, ql]).

generate_base(Spec, Base) :-
	functor(Spec, Name, 1), !,
	user:file_search_path(Name, Spec2),
	generate_base(Spec2, ParentBase),
	arg(1, Spec, Sub),
	concat_atom([ParentBase, Sub], '/', Base).
generate_base(Spec, Absolute) :-
	absolute_file_name(Spec, Absolute). 	% TBD: just make relative
						% absolute!


check_options([], _).
check_options([H|T], Path) :-
	check_option(H, Path),
	check_options(T, Path).

check_option(access(Access), Path) :- !,
	file_exists(Path, Access).
check_option(_, _).

		 /*******************************
		 *	    FILE NAMES		*
		 *******************************/

% source_location(-Path, -LineNo)
%
% Unify Path and LineNo with the filename and line number of the
% location where the last term has been read.  Used inside
% term_expansion.

source_location(File, Line) :-
	prolog_load_context(file, File),
	prolog_load_context(term_position,
		'$stream_position'(_, Line, _, _)).

%	is_absolute_file_name(+Atom)
%	Succeeds if Atom describes an absolute filename.  This port is
%	Unix-only.

is_absolute_file_name(Atom) :-
	atom_chars(Atom, [0'/|_]).

%	file_directory_name(+Path, +Dir)
%	Finds directory-name from path-name

file_directory_name(Path, Dir) :-
	atom_chars(Path, PathChars),
	(   append(DirChars, BaseChars, PathChars),
	    \+ member(0'/, BaseChars),
	    BaseChars \== []
	->  atom_chars(Dir, DirChars)
	;   PathChars = [0'/|_]
	->  Dir = /
	;   Dir = ''
	).

file_base_name(Path, Base) :-
	atom_chars(Path, PathChars),
	(   append(_, BaseChars, PathChars),
	    \+ member(0'/, BaseChars),
	    BaseChars \== []
	->  atom_chars(Base, BaseChars)
	;   Base = Path
	).

%	file_name_extension(?Base, ?Ext, ?Path).
%
%	BUG: should take care of case-insensitive and other OS
%	dependencies.  This port of the SWI-Prolog predicate only
%	works on Unix.

file_name_extension(Base, DotExt, Path) :-
	nonvar(DotExt),
	atom_chars(DotExt, [0'.|EC]), !,
	atom_chars(Ext, EC),
	file_name_extension2(Base, Ext, Path).
file_name_extension(Base, Ext, Path) :-
	file_name_extension2(Base, Ext, Path).
	
file_name_extension2(Base, Ext, Path) :-	% -, -, +
	nonvar(Path), !,
	(   atom_chars(Path, PC),
	    append(BC, [0'.|EC], PC),
	    \+ member(0'/, EC),
	    \+ member(0'., EC)
	->  atom_chars(Base, BC),
	    atom_chars(Ext, EC)
	;   Ext = '',
	    Base = Path
	).
file_name_extension2(Base, '', Out) :- !,
	Out = Base.
file_name_extension2(Base, Ext, Path) :- % +, +, -
	nonvar(Base),
	nonvar(Ext), !,
	atom_chars(Base, BC),
	atom_chars(Ext, EC),
	(   append(_, [0'.|EC], BC)
	->  Path = Base
	;   append(BC, [0'.|EC], PC),
	    atom_chars(Path, PC)
	).

		 /*******************************
		 *	       UNIX		*
		 *******************************/

%	unix(Command)
%
%	*very* incomplete version of Quintus/SWI unix/1 predicate

unix(argv(Argv)) :- !,
	prolog_flag(argv, Argv).
unix(Term) :-
	throw(error(domain_error(system_command, Term), _)).


		 /*******************************
		 *	  TERM <-> ATOM		*
		 *******************************/

% term_to_atom(+Term, ?Atom)
% term_to_atom(-Term, +Atom)
%
% If Term may be a variable, if Atom is an atom.
 
term_to_atom(Term, Atom) :-
        ( var(Term) ->
                atom_to_term(Atom, Term)
        ; copy_term(Term, TempTerm),
          numbervars(TempTerm, 0, _),
          with_output_to_chars(print(TempTerm), Chars),
          atom_chars(Atom, Chars)
        ).

% atom_to_term(+Atom, -Term)
% atom_to_term(+Atom, -Term, -Bindings)
%
% Convert an atom to a term, possibly saving original variable names.
% Fails if 1st argument is not an atom.
 
atom_to_term(Atom, Term) :-
        atom_to_term(Atom, Term, _).
 
atom_to_term(Atom, Term, Bindings) :-
        ( Atom == '' ->
                Term = Atom,
                Bindings = []
        ; atom(Atom) ->
                atom_to_term_1(Atom, Term, Bindings)
        ).
 
atom_to_term_1(Atom, Term, Bindings) :-
	open_atom(Atom, StreamCode),
	stream_code(Stream, StreamCode),
	(   catch(read_term(Stream, Term,
			    [ syntax_errors(quiet),
			      variable_names(Bindings)
			    ]),
		  _, fail)
	->  at_end_of_stream(Stream),
	    close(Stream)
	;   close(Stream),
	    fail
	).

		 /*******************************
		 *	   FOREIGN STUFF	*
		 *******************************/

foreign(pl_concat,	 concat(+term, +term, +term, [-integer])).
foreign(pl_concat_atom3, concat_atom3(+term, +term, +term, [-integer])).
foreign(pl_concat_atom,  concat_atom2(+term, +term, [-integer])).
foreign(pl_atom_length,  atom_length(+atom, [-integer])).
foreign(pl_open_atom,	 open_atom(+atom, -address('SP_stream'))).
foreign_resource(sp_compat,
		 [ pl_concat,
		   pl_concat_atom3,
		   pl_concat_atom,
		   pl_atom_length,
		   pl_open_atom
		 ]).
:- load_foreign_resource(sp_compat).
	
concat(A, B, C) :-
	concat(A, B, C, 1).
concat_atom(L, S, A) :-
	concat_atom3(L, S, A, 1).
concat_atom(L, A) :-
	concat_atom2(L, A, 1).

		 /*******************************
		 *	      ATOMS		*
		 *******************************/

sformat(OnTo, Fmt, Args) :-
	format_to_chars(Fmt, Args, Chars),
	atom_chars(OnTo, Chars).

		 /*******************************
		 *	      RESOURCES		*
		 *******************************/

%	locate_resource(+Name, +Class, +Mode, -File)
%
%	Locate resource in an external file.

swi_:locate_resource(Module, Name, Class, Mode, File) :-
	mode_to_access(Mode, Access),
	(   current_predicate(_, Module:resource(_,_,_)),
	    Module:resource(Name, Class, Spec)
	;   current_predicate(_, user:resource(_,_,_)),
	    user:resource(Name, Class, Spec)
	),
	absolute_file_name(Spec, [access(Access)], File).
	
mode_to_access(Mode, read) :-
	atom_chars(Mode, [0'r|_]), !.
mode_to_access(Mode, write) :-
	atom_chars(Mode, [0'w|_]).
