/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    jan@swi.psy.uva.nl

    Purpose: editor interface
*/

:- module($edit,
	[ edit/1
	, edit/0
	, ed/1
	, ed/0
	, make/0
	]).

:- dynamic
	user:edit_source/1.
:- multifile
	user:edit_source/1.

:- module_transparent
	edit/1, 
	ed/1.

edit(File) :-
	$check_file(File, Path),
	\+ qlf_file(Path), !,
	$record_last($edit_file, File), 
	$edit_load(File).
edit(File) :-
	$confirm('No such file.  Edit new file ~w', [File]),
	$record_last($edit_file, File), 
	$edit(File).

edit :-
	recorded($edit_file, File), 
	$confirm('Edit file `~w''', [File]), !,
	edit(File).

ed(Spec) :-
	$find_predicate(Spec, Preds),
	(   Preds = [Head]
	;   member(Head, Preds), 
	    $predicate_name(Head, PredName),
	    $confirm('Edit predicate `~w''', [PredName])
	), !, 
	$define_predicate(Head),
	source_file(Head, File), 
	predicate_property(Head, line_count(LineNo)),
	$strip_module(Head, Module, Term), 
	functor(Term, Name, Arity), 
	$record_last($edit_predicate, Module:Name/Arity), 
	$edit_load(File:LineNo:Name/Arity).

ed :-
	$module(TypeIn, TypeIn), 
	recorded($edit_predicate, TypeIn:Name/Arity), !, 
	$confirm('Edit predicate `~w/~w''', [Name, Arity]), !, 
	ed(TypeIn:Name/Arity).
ed :-
	recorded($edit_predicate, Module:Name/Arity), !, 
	$confirm('Edit predicate `~w:~w/~w''', [Module, Name, Arity]), !, 
	ed(Module:Name/Arity).
ed :-
	$warning('ed/0: You can only use ed/0 after ed/1!').

qlf_file(Path) :-
	concat(_, '.qlf', Path), !.

$record_last(Key, Term) :-
	recorded(Key, Last) -> 
	Last = Term, !.
$record_last(Key, Term) :-
	recorda(Key, Term).

$edit_load(File:Predicate) :-
	(   $check_file(File, Path), 
	    \+ qlf_file(Path)
	->  $edit(Path:Predicate), !
	).
$edit_load(File) :-
	File \= _:_,
	(   $check_file(File, Path), 
	    \+ qlf_file(Path)
	->  $edit(Path), !
	).

$edit(Spec) :-
	user:edit_source(Spec), !.
$edit(File:LineNo:Name/_Arity) :- !,
	(   (   getenv('EDITOR', Editor)
	    ;   $default_editor(Editor)
	    )
	->  (	(   edit_command(Editor, File, LineNo, Name, Command)
		;   edit_command(Editor, File, LineNo, $nopredicate, Command)
		)
	    ->  shell(Command),
		make
	    )
	).
$edit(File) :-
	(   (   getenv('EDITOR', Editor)
	    ;   $default_editor(Editor)
	    )
	->  edit_command(Editor, File, 1, $nopredicate, Command),
	    shell(Command),
	    make
	).

edit_command(Editor, File, _Line, $nopredicate, Command) :- !,
	prolog_to_os_filename(File, OsFile),
	file_base_name(Editor, Base),
	(   edit_command(Base, nosearch, Cmd)
	->  name(Cmd, S0),
	    substitute("%e", Editor, S0, S1),
	    substitute("%f", OsFile, S1, S),
	    name(Command, S)
	;   $warning('Don''t know how to use editor `~w''', [Editor])
	).
edit_command(Editor, File, Line, Pred, Command) :-
	prolog_to_os_filename(File, OsFile),
	file_base_name(Editor, Base),
	(   edit_command(Base, search, Cmd)
	->  name(Cmd, S0),
	    substitute("%e", Editor, S0, S1),
	    substitute("%f", OsFile, S1, S2),
	    substitute("%s", Pred, S2, S3),
	    substitute("%d", Line, S3, S),
	    name(Command, S)
	).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
edit_command(+Editor, +search, -Command)

This predicate should specify the  shell-command   called  to invoke the
user's editor.  The following substitutions will be made:

	%e		Path name of the editor
	%f		Path name of the file to be edited
	%s		Name of the predicate (only for `search')
	%d		Line number of the predicate (only for `search')

To locate a predicate in  a   source-file,  two mechanisms are provided.
The first will *search* for the   predicate definition, while the second
uses the *line-number* info from the Prolog system.  Searching generally
only handles finding the  definition  of   the  first  occurrence of the
predicate, disregarding the arity information.    Using  line-numbers is
not sensitive to changes.  Smart editors  may pass both informations and
search nearby the given line number.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

edit_command(top,	  	search,   '%e ''%f'' ''-^%s''').
edit_command(vi,		search,   '%e ''+/^%s'' ''%f''').
edit_command(emacs,   		search,   '%e +%d ''%f''').
edit_command(emacsclient,   	search,   '%e +%d ''%f''').
edit_command(notepad, 		nosearch, '%e %f').
edit_command(_,       		nosearch, '%e ''%f''').

substitute(From, ToAtom, Old, New) :-
	name(ToAtom, To),
	append(Pre, S0, Old),
	append(From, Post, S0) ->
	append(Pre, To, S1),
	append(S1, Post, New), !.
substitute(_, _, Old, Old).


		/********************************
		*              MAKE             *
		*********************************/

%	make
%	Reload all source files that have been changed since they were
%	loaded.

make :-
	$update_library_index,
	$time_source_file(File, LoadTime),
	time_file(File, Modified),
	Modified @> LoadTime,
	reload(File),
	fail.
make.


%	reload(File)
%
%	Reload file into the proper module.  Note that if the file is loaded
%	into multiple modules this should be handled more carefully.

reload(File) :-
	findall(Context, $load_context_module(File, Context), Modules),
	(   Modules = []
	->  consult(user:File)
	;   Modules = [Module]
	->  consult(Module:File)
	;   Modules = [First|_Rest],
	    consult(First:File)
	).
	
