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

:- user:dynamic
	edit_source/1.
:- user:multifile
	edit_source/1.

:- module_transparent
	edit/1, 
	ed/1.

edit(File) :-
	$break($check_file(File, _)), !, 
	$record_last($edit_file, File), 
	$edit_load(File).
edit(File) :-
	$confirm('Edit new file ~w', [File]),
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
	source_file(Head, File), 
	$strip_module(Head, Module, Term), 
	functor(Term, Name, Arity), 
	$record_last($edit_predicate, Module:Name/Arity), 
	$edit_load(File:Name/Arity).

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
	$break($warning('ed/0: You can only use ed/0 after ed/1!')).

$record_last(Key, Term) :-
	recorded(Key, Last) -> 
	Last = Term, !.
$record_last(Key, Term) :-
	recorda(Key, Term).

$edit_load(File:Predicate) :-
	$check_file(File, Path), 
	$edit(Path:Predicate), !, 
	make.
$edit_load(File) :-
	File \= _:_,
	$check_file(File, Path), 
	$edit(Path), !, 
	make.
$edit_load(_, _).

$edit(Spec) :-
	user:edit_source(Spec), !.
$edit(File:Name/_Arity) :- !,
	(   getenv('EDITOR', Editor)
	;   $default_editor(Editor)
	) ->
	edit_command(Editor, File, Name, Command), !, 
	shell(Command).
$edit(File) :-
	(   getenv('EDITOR', Editor)
	;   $default_editor(Editor)
	) ->
	edit_command(Editor, File, $nopredicate, Command), !, 
	shell(Command).

thief(File, $nopredicate) :- !,
	call($thief(['-f', File])).			% avoid undefined
thief(File, Predicate) :-
	concat('-^', Predicate, Search),
	call($thief(['-f', File, Search])).		% idem

edit_command(Editor, File, $nopredicate, Command) :-
	$file_base_name(Editor, Base),
	(   edit_command(Base, nosearch, Cmd)
	->  name(Cmd, S0),
	    substitute("%e", Editor, S0, S1),
	    substitute("%f", File, S1, S2),
	    name(Command, S2)
	;   $warning('Don''t know how to use editor `~w''', [Editor])
	).
edit_command(Editor, File, Pred, Command) :-
	$file_base_name(Editor, Base),
	(   edit_command(Base, search, Cmd)
	->  name(Cmd, S0),
	    substitute("%e", Editor, S0, S1),
	    substitute("%f", File, S1, S2),
	    substitute("%s", Pred, S2, S3),
	    name(Command, S3)
	;   $warning('Don''t know how to use editor `~w''', [Editor])
	).

edit_command(top, search,   '%e ''%f'' ''-^%s''').
edit_command(top, nosearch, '%e ''%f''').
edit_command(vi,  search,   '%e ''+/^%s'' ''%f''').
edit_command(vi,  nosearch, '%e ''%f''').

substitute(From, ToAtom, Old, New) :-
	name(ToAtom, To),
	append(Pre, S0, Old),
	append(From, Post, S0) ->
	append(Pre, To, S1),
	append(S1, Post, New), !.


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
%	Reload file in proper module.  Note that if the file is loaded
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
	).
	
