/*  $Id$

    Part of SWI-Prolog
    Designed and implemented by Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1998 University of Amsterdam. All rights reserved.
*/

:- module(prolog_edit,
	  [ edit/1			% +Spec
	  ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module implements the generic editor  interface. It consists of two
extensible parts with little  in  between.   The  first  part deals with
translating the input into source-location, and the second with starting
an editor.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- multifile
	locate/3,			% +Partial, -FullSpec, -Location
	locate/2,			% +FullSpec, -Location
	select_location/3,		% +Pairs, +Spec, -Location
	edit_source/1,			% +Location
	edit_command/2,			% +Editor, -Command
	load/0.				% provides load-hooks

%	edit(+Spec)
%
%	Edit indicated object.

edit(Spec) :-
	var(Spec), !,
	throw(error(instantiation_error, _)).
edit(Spec) :-
	findall(Location-FullSpec,
		locate(Spec, FullSpec, Location),
		Pairs0),
	merge_locations(Pairs0, Pairs),
	select_location(Pairs, Spec, Location),
	do_edit_source(Location).


		 /*******************************
		 *	      LOCATE		*
		 *******************************/

%	locate(+Spec, -FullSpec, -Location)

locate(Path, file(Path), [file(Path)]) :-
	atom(Path),
	exists_file(Path),
	\+ exists_directory(Path).
locate(FileBase, file(File), [file(File)]) :-
	atom(FileBase),
	absolute_file_name(FileBase,
			   [ file_type(prolog),
			     access(read),
			     file_errors(fail)
			   ],
			   File),
	\+ exists_directory(File).
locate(FileSpec, file(File), [file(File)]) :-
	ground(FileSpec),
	absolute_file_name(FileSpec,
			   [ file_type(prolog),
			     access(read),
			     file_errors(fail)
			   ],
			   File).
locate(FileBase, source_file(Path), [file(Path)]) :-
	atom(FileBase),
	source_file(Path),
	file_base_name(Path, File),
	file_name_extension(FileBase, pl, File).
locate(Name, Module:Name/Arity, Location) :-
	atom(Name),
	locate(Module:Name/Arity, Location).
locate(Name/Arity, Module:Name/Arity, Location) :-
	locate(Module:Name/Arity, Location).
locate(Module:Name, Module:Name/Arity, Location) :-
	locate(Module:Name/Arity, Location).
locate(Spec, module(Spec), Location) :-
	locate(module(Spec), Location).
locate(Spec, Spec, Location) :-
	locate(Spec, Location).

%	locate(+Spec, -Location)
%
%	Locate object from the specified location.

locate(file(File, line(Line)), [file(File), line(Line)]).
locate(file(File), [file(File)]).
locate(Module:Name/Arity, [file(File), line(Line)]) :-
	(   nonvar(Arity)
	->  functor(Head, Name, Arity)
	;   true
	),
	(   var(Module)
	->  NonImport = true
	;   NonImport = false
	),
	current_predicate(Name, Module:Head),
	\+ (   NonImport == true,
	       Module \== system,
	       predicate_property(Module:Head, imported_from(_))
	   ),
	functor(Head, Name, Arity),	% bind arity
	predicate_property(Module:Head, file(File)),
	predicate_property(Module:Head, line_count(Line)).
locate(module(Module), [file(Path)]) :-
	current_module(Module, Path).


		 /*******************************
		 *	       EDIT		*
		 *******************************/

do_edit_source(Location) :-
	edit_source(Location), !.
do_edit_source(Location) :-
	external_edit_command(Location, Command),
	catch(shell(Command), E,
	      (print_message(warning, E),
	       fail)),
	make.

external_edit_command(Location, Command) :-
	memberchk(file(File), Location),
	memberchk(line(Line), Location),
	editor(Editor),
	file_base_name(Editor, EditorFile),
	file_name_extension(Base, _, EditorFile),
	edit_command(Base, Cmd),
	prolog_to_os_filename(File, OsFile),
	atom_codes(Cmd, S0),
	substitute("%e", Editor, S0, S1),
	substitute("%f", OsFile, S1, S2),
	substitute("%d", Line,   S2, S), !,
	atom_codes(Command, S).
external_edit_command(Location, Command) :-
	memberchk(file(File), Location),
	editor(Editor),
	file_base_name(Editor, EditorFile),
	file_name_extension(Base, _, EditorFile),
	edit_command(Base, Cmd),
	prolog_to_os_filename(File, OsFile),
	atom_codes(Cmd, S0),
	substitute("%e", Editor, S0, S1),
	substitute("%f", OsFile, S1, S),
	\+ substitute("%d", 1, S, _), !,
	atom_codes(Command, S).
external_edit_command(Location, Command) :-
	memberchk(file(File), Location),
	editor(Editor),
	concat_atom(['"', Editor, '" "', File, '"'], Command).

editor(Editor) :-
	getenv('EDITOR', Editor), !.
editor(vi) :-
	current_prolog_flag(unix, true), !.
editor(notepad) :-
	current_prolog_flag(windows, true), !.
editor(_) :-
	throw(error(existence_error(editor), _)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
edit_command(+Editor, -Command)

This predicate should specify the  shell-command   called  to invoke the
user's editor.  The following substitutions will be made:

	%e		Path name of the editor
	%f		Path name of the file to be edited
	%d		Line number of the target
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

edit_command(vi,	  '%e +%d ''%f''').
edit_command(vi,	  '%e ''%f''').
edit_command(emacs,	  '%e +%d ''%f''').
edit_command(emacs,	  '%e ''%f''').
edit_command(notepad,     '"%e" "%f"').
edit_command(edit,        '%e %f:%d').		% private stuff
edit_command(edit,        '%e %f').

edit_command(emacsclient, Command) :- edit_command(emacs, Command).
edit_command(vim,         Command) :- edit_command(vi,    Command).

substitute(From, ToAtom, Old, New) :-
	name(ToAtom, To),
	append(Pre, S0, Old),
	append(From, Post, S0) ->
	append(Pre, To, S1),
	append(S1, Post, New), !.
substitute(_, _, Old, Old).


		 /*******************************
		 *	      SELECT		*
		 *******************************/

merge_locations(Pairs0, Pairs) :-
	keysort(Pairs0, Pairs1),
	merge_locations2(Pairs1, Pairs).

merge_locations2([], []).
merge_locations2([H0|T0], [H|T]) :-
	remove_same_location(H0, H, T0, T1),
	merge_locations2(T1, T).

remove_same_location(Pair0, H, [Pair1|T0], L) :-
	merge_locations(Pair0, Pair1, Pair2), !,
	remove_same_location(Pair2, H, T0, L).
remove_same_location(H, H, L, L).

merge_locations(Loc-Spec1, Loc-Spec2, Loc-Spec) :- !,
	(   merge_specs(Spec1, Spec2, Spec)
	;   merge_specs(Spec2, Spec1, Spec)
	;   Spec = Spec1
	), !.

merge_specs(source_file(Path), _, source_file(Path)).

%	select_location(+Pairs, +UserSpec, -Location)

select_location([], Spec, _) :- !,
	print_message(warning, edit(not_found(Spec))),
	fail.
select_location([Location-_Spec], _, Location) :- !.
%select_location(Pairs, _, Location) :-
%	length(Pairs, N),
%	N > 20, !,
select_location(Pairs, _, Location) :-
	print_message(help, edit(select)),
	list_pairs(Pairs, 1),
	print_message(help, edit(prompt_select)),
	read_number(N),
	nth1(N, Pairs, Location-_Spec), !.

list_pairs([], _).
list_pairs([H|T], N) :-
	list_pair(H, N),
	NN is N + 1,
	list_pairs(T, NN).

list_pair(Pair, N) :-
	print_message(help, edit(target(Pair, N))).

	
read_number(X) :-
	read_line(Chars),
	name(X, Chars),
	integer(X).

read_line(Chars) :-
	get0(user_input, C0),
	read_line(C0, Chars).

read_line(10, []) :- !.
read_line(-1, []) :- !.
read_line(C, [C|T]) :-
	get0(user_input, C1),
	read_line(C1, T).


		 /*******************************
		 *	       MESSAGES		*
		 *******************************/

:- multifile
	prolog:message/3.

prolog:message(edit(not_found(Spec))) -->
	[ 'Cannot find anything to edit from "~p"'-[Spec] ].
prolog:message(edit(select)) -->
	[ 'Please select item to edit:', nl, nl ].
prolog:message(edit(prompt_select)) -->
	[ nl, 'Your choice? ', flush ].
prolog:message(edit(target(Location-Spec, N))) -->
	[ '~t~d~3| '-[N]],
	edit_specifier(Spec),
	[ '~t~32|' ],
	edit_location(Location).

edit_specifier(Module:Name/Arity) -->
	[ '~w:~w/~w'-[Module, Name, Arity] ].
edit_specifier(file(_Path)) -->
	[ '<file>' ].
edit_specifier(source_file(_Path)) -->
	[ '<loaded file>' ].
edit_specifier(Term) -->
	[ '~p'-[Term] ].

edit_location(Location) -->
	{ memberchk(file(File), Location),
	  memberchk(line(Line), Location),
	  short_filename(File, Spec)
	}, !,
	[ '~q:~d'-[Spec, Line] ].
edit_location(Location) -->
	{ memberchk(file(File), Location),
	  short_filename(File, Spec)
	}, !,
	[ '~q'-[Spec] ].

short_filename(Path, Spec) :-
	setof(Id, Spec^file_search_path(Id, Spec), Ids),
	member(Id, Ids),
	Term =.. [Id, '.'],
	absolute_file_name(Term,
			   [ file_type(directory),
			     file_errors(fail)
			   ], Prefix),
	atom_concat(Prefix, Local0, Path), !,
	remove_leading_slash(Local0, Local),
	Spec =.. [Id, Local].
short_filename(Path, Path).
	

remove_leading_slash(Path, Local) :-
	atom_concat(/, Local, Path), !.
remove_leading_slash(Path, Path).


		 /*******************************
		 *	  LOAD EXTENSIONS	*
		 *******************************/

load_extensions :-
	load,
	fail.
load_extensions.

:- load_extensions.
