/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
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

:- module($toplevel,
	[ $initialise/0			% start Prolog (does not return)
	, $toplevel/0			% Prolog top-level (re-entrant)
	, $abort/0 			% restart after an abort
	, $break/0 			% live in a break
	, $compile/0 			% `-c' toplevel
	, $welcome/0			% banner
	, prolog/0 			% user toplevel predicate
	, $set_prompt/1			% set the main prompt
	, at_initialization/1		% goals to run at initialization
	, (initialization)/1		% initialization goal (directive)
	]).


		/********************************
		*         INITIALISATION        *
		*********************************/

:- dynamic
	loaded_init_file/1.		% already loaded init files

$welcome :-
	print_message(banner, welcome).

$load_init_file(none) :- !.
$load_init_file(Base) :-
	loaded_init_file(Base), !.
$load_init_file(InitFile) :-
	is_absolute_file_name(InitFile), !,
	ensure_loaded(user:InitFile).
$load_init_file(Base) :-
	absolute_file_name(user_profile(Base),
			   [ access(read),
			     file_errors(fail)
			   ], InitFile),
	asserta(loaded_init_file(Base)),
	ensure_loaded(user:InitFile).
$load_init_file(_).

$load_system_init_file :-
	loaded_init_file(system), !.
$load_system_init_file :-
	$option(system_init_file, Base, Base),
	(   Base == none
	->  asserta(loaded_init_file(system))
	;   current_prolog_flag(home, Home),
	    file_name_extension(Base, rc, Name),
	    concat_atom([Home, '/', Name], File),
	    access_file(File, read),
	    asserta(loaded_init_file(system)),
	    load_files(user:File, [silent(true)]), !
	).
$load_system_init_file.

$load_script_file :-
	loaded_init_file(script), !.
$load_script_file :-
	$option(script_file, OsFile, OsFile),
	OsFile \== '',
	prolog_to_os_filename(File, OsFile),
	(   exists_file(File)		% avoid expanding on extensions
	->  asserta(loaded_init_file(script)),
	    ensure_loaded(user:File)
	;   throw(error(existence_error(script_file, File), _))
	).
$load_script_file.

$load_gnu_emacs_interface :-
	getenv('EMACS', t),
	current_prolog_flag(argv, Args),
	memberchk('+C', Args), !,
	ensure_loaded(user:library(emacs_interface)).
$load_gnu_emacs_interface.

		 /*******************************
		 *	 AT_INITIALISATION	*
		 *******************************/

:- module_transparent
	at_initialization/1,
	(initialization)/1.
:- dynamic
	$at_initialization/1.

at_initialization(Spec) :-
	$strip_module(Spec, Module, Goal),
	'$toplevel':assert($at_initialization(Module:Goal)).

$run_at_initialization :-
	\+ current_prolog_flag(saved_program, true), !.
$run_at_initialization :-
	(   $at_initialization(Goal),
	    (   catch(Goal, E,
		      print_message(error, initialization_exception(Goal, E)))
	    ->  fail
	    ;   print_message(warning, goal_failed(at_initialization, Goal)),
		fail
	    )
	;   true
	).

%	initialization(+Goal)
%
%	Runs `Goal' both a load and initialization time.

initialization(Goal) :-
	at_initialization(Goal),
	Goal.


		 /*******************************
		 *     FILE SEARCH PATH (-p)	*
		 *******************************/

$set_file_search_paths :-
	current_prolog_flag(argv, Argv),
	append(H, ['-p', Path|_], Argv),
	\+ memberchk(--, H),
	(   atom_chars(Path, Chars),
	    (	phrase($search_path(Name, Aliases), Chars)
	    ->	reverse(Aliases, Aliases1),
	        forall(member(Alias, Aliases1),
		       asserta(user:file_search_path(Name, Alias)))
	    ;   print_message(error, commandline_arg_type(p, Path))
	    )
	->  true
	),
	fail ; true.

$search_path(Name, Aliases) -->
	$string(NameChars),
	[=], !,
	{atom_chars(Name, NameChars)},
	$search_aliases(Aliases).

$search_aliases([Alias|More]) -->
	$string(AliasChars),
	path_sep, !,
	{ $make_alias(AliasChars, Alias) },
	$search_aliases(More).
$search_aliases([Alias]) -->
	$string(AliasChars),
	$eos, !,
	{ $make_alias(AliasChars, Alias) }.

path_sep -->
	{ current_prolog_flag(windows, true)
	}, !,
	[;].
path_sep -->
	[:].

$string(X) --> {X=[_|_]}, X.

$eos([], []).

$make_alias(Chars, Alias) :-
	catch(term_to_atom(Alias, Chars), _, fail),
	(   atom(Alias)
	;   functor(Alias, F, 1),
	    F \== /
	), !.
$make_alias(Chars, Alias) :-
	atom_chars(Alias, Chars).


		 /*******************************
		 *   LOADING ASSIOCIATED FILES	*
		 *******************************/

%	set_associated_file/0
%	
%	If SWI-Prolog is started as <exe> <file>.<ext>, where <ext> is
%	the extension registered for associated files, set the Prolog
%	flag associated_file, switch to the directory holding the file
%	and -if possible- adjust the window title.

set_associated_file :-
	$set_prolog_file_extension,
	current_prolog_flag(associate, Ext),
	current_prolog_flag(argv, Argv),
	append(Pre, [OsFile], Argv),
	\+ memberchk(--, Pre),
	prolog_to_os_filename(File, OsFile),
	file_name_extension(_, Ext, File),
	access_file(File, read),
	file_directory_name(File, Dir),
	working_directory(_, Dir),
	set_prolog_flag(associated_file, File),
	atom_concat('SWI-Prolog -- ', File, Title),
	(   '$c_current_predicate'(_, system:window_title(_, _))
	->  system:window_title(_, Title)
	;   true
	).
set_associated_file.


%	load_associated_file/0
%	
%	Load the file-name set by set_associated_file/0 from the
%	commandline arguments. Not the expand(false) to avoid expanding
%	special characters in the filename.

load_associated_file :-
	current_prolog_flag(associated_file, File),
	load_files(user:File, [expand(false)]).
load_associated_file.


hkey('HKEY_CURRENT_USER/Software/SWI/Prolog').
hkey('HKEY_LOCAL_MACHINE/Software/SWI/Prolog').

$set_prolog_file_extension :-
	'$c_current_predicate'(_, system:win_registry_get_value(_,_,_)),
	hkey(Key),
	catch(win_registry_get_value(Key, fileExtension, Ext0),
	      _, fail), !,
	(   atom_concat('.', Ext, Ext0)
	->  true
	;   Ext = Ext0
	),
	set_prolog_flag(associate, Ext).
$set_prolog_file_extension.


		/********************************
		*        TOPLEVEL GOALS         *
		*********************************/

:- flag($banner_goal, _, $welcome).

$initialise :-
	catch(initialise_prolog, E,
	      (print_message(error, initialization_exception(E)),
	       fail
	      )).

initialise_prolog :-
	$clean_history,
	set_associated_file,
	$set_file_search_paths,
	set_prolog_flag(toplevel_print_options,
			[quoted(true), portray(true), max_depth(10)]),
	set_prolog_flag(debugger_print_options,
			[quoted(true), portray(true), max_depth(10)]),
	$run_at_initialization,
	$load_system_init_file,
	$load_gnu_emacs_interface,
	$option(init_file, OsFile, OsFile),
	prolog_to_os_filename(File, OsFile),
	$load_init_file(File), 
	$load_script_file,
	load_associated_file,
	$option(goal, GoalAtom, GoalAtom), 
	term_to_atom(Goal, GoalAtom), 
	(   Goal == $welcome
	->  flag($banner_goal, TheGoal, TheGoal)
	;   TheGoal = Goal
	),
	ignore(user:TheGoal).

$abort :-
	see(user), 
	tell(user), 
	flag($break_level, _, 0), 
	flag($compilation_level, _, 0),
	$calleventhook(abort),
	print_message(informational, '$aborted'),
	$toplevel.

$break :-
	flag($break_level, Old, Old+1), 
	flag($break_level, New, New), 
	print_message(informational, break(enter(New))),
	$runtoplevel,
	print_message(informational, break(exit(New))),
	flag($break_level, _, Old), !.

:- $hide($toplevel, 0).			% avoid in the GUI stacktrace
:- $hide($abort, 0).			% same after an abort

$toplevel :-
	$runtoplevel,
	print_message(query, query(eof)),
	print_message(informational, halt).

%	Actually run the toplevel.  If there is a syntax error in the
%	goal there is no reason to persue.  Something like that should
%	happen to repetitive exceptions in the toplevel as well, but
%	how do we distinguish between a stupid user and a program
%	crashing in a loop?

$runtoplevel :-
	$option(toplevel, TopLevelAtom, TopLevelAtom), 
	catch(term_to_atom(TopLevel, TopLevelAtom), E,
	      (print_message(error, E),
	       halt(1))),
	user:TopLevel.

%	$compile
%	Toplevel called when invoked with -c option.

$compile :-
	$run_at_initialization,
	$load_system_init_file,
	$set_file_search_paths,
	catch($compile_wic, E, (print_message(error, E), halt(1))).


		/********************************
		*    USER INTERACTIVE LOOP      *
		*********************************/

prolog :-
	flag($tracing, _, off), 
	flag($break_level, BreakLev, BreakLev), 
	repeat, 
	    (   $module(TypeIn, TypeIn), 
		$system_prompt(TypeIn, BreakLev, Prompt),
		prompt(Old, '|    '), 
		trim_stacks,
		read_query(Prompt, Goal, Bindings),
		prompt(_, Old),
		call_expand_query(Goal, ExpandedGoal,
				  Bindings, ExpandedBindings)
	    ->  $execute(ExpandedGoal, ExpandedBindings)
	    ), !.


read_query(Prompt, Goal, Bindings) :-
	current_prolog_flag(history, N),
	integer(N),
	N =< 0, !,
	remove_history_prompt(Prompt, Prompt1),
	repeat,				% over syntax errors
	prompt1(Prompt1),
	catch($raw_read(user_input, Line), E,
	      (print_message(error, E),
	       (   E = error(syntax_error(_), _)
	       ->  fail
	       ;   throw(E)
	       ))),
	atom_concat(Line, '.', CompleteLine),
	(   current_predicate(_, user:rl_add_history(_))
	->  call(user:rl_add_history(CompleteLine))
	;   true
	),
	catch(atom_to_term(Line, Goal, Bindings), E,
	      (   print_message(error, E),
		  fail
	      )), !,
	$save_history(Line).
read_query(Prompt, Goal, Bindings) :-
	seeing(Old), see(user_input),
	(   read_history(h, '!h', 
			 [trace, end_of_file], 
			 Prompt, Goal, Bindings)
	->  see(Old)
	;   see(Old),
	    fail
	).

remove_history_prompt(Prompt0, Prompt) :-
	atom_chars(Prompt0, Chars0),
	clean_history_prompt_chars(Chars0, Chars1),
	delete_leading_blanks(Chars1, Chars),
	atom_chars(Prompt, Chars).

clean_history_prompt_chars([], []).
clean_history_prompt_chars(['%', !|T], T) :- !.
clean_history_prompt_chars([H|T0], [H|T]) :-
	clean_history_prompt_chars(T0, T).
 
delete_leading_blanks([' '|T0], T) :- !,
	delete_leading_blanks(T0, T).
delete_leading_blanks(L, L).


set_default_history :-
	(   current_prolog_flag(readline, true)
	->  set_prolog_flag(history, 0)
	;   set_prolog_flag(history, 25)
	).

:- initialization set_default_history.


		/********************************
		*            PROMPTING		*
		********************************/

:- dynamic
	$prompt/1.

$prompt("%m%d%l%! ?- ").

$set_prompt(P) :-
	atom_codes(P, S),
	retractall($prompt(_)),
	assert($prompt(S)).


$system_prompt(Module, BrekLev, Prompt) :-
	$prompt(P0),
	(    Module \== user
	->   $substitute("%m", [Module, ": "], P0, P1)
	;    $substitute("%m", [], P0, P1)
	),
	(    BrekLev \== 0
	->   $substitute("%l", ["[", BrekLev, "] "], P1, P2)
	;    $substitute("%l", [], P1, P2)
	),
	(    tracing
	->   $substitute("%d", ["[trace] "], P2, P3)
	;    current_prolog_flag(debug, true)
	->   $substitute("%d", ["[debug] "], P2, P3)
	;    $substitute("%d", [], P2, P3)
	),
	atom_chars(Prompt, P3).
	
$substitute(From, T, Old, New) :-
	convert_to(T, T0),
	flatten(T0, To),
	append(Pre, S0, Old),
	append(From, Post, S0) ->
	append(Pre, To, S1),
	append(S1, Post, New), !.
$substitute(_, _, Old, Old).
	
convert_to([], []).
convert_to([A|T], [S|R]) :-
	atomic(A), !,
	name(A, S),
	convert_to(T, R).
convert_to([S|T], [S|R]) :-
	convert_to(T, R).

		/********************************
		*           EXECUTION		*
		********************************/

$execute(Var, _) :-
	var(Var), !,
	print_message(informational, var_query(Var)),
	fail.
$execute(end_of_file, _) :- !.
$execute(Goal, Bindings) :-
	$module(TypeIn, TypeIn), 
	TypeIn:$dwim_correct_goal(Goal, Bindings, Corrected), !, 
	$execute_goal(Corrected, Bindings).
$execute(_, _) :-
	notrace, 
	print_message(query, query(no)),
	fail.

$execute_goal(trace, []) :-
	trace, 
	print_message(query, query(yes)), !,
	fail.
$execute_goal(Goal, Bindings) :-
	$module(TypeIn, TypeIn), 
	(   TypeIn:Goal,
	    flush_output(user_output),
	    call_expand_answer(Bindings, NewBindings),
	    (	write_bindings(NewBindings)
	    ->	!,
	        notrace,
		fail
	    )
	;   notrace, 
	    print_message(query, query(no)),
	    fail
	).

write_bindings([]) :- !, 
	print_message(query, query(yes)).
write_bindings(Bindings) :-
	repeat,
	    print_message(query, query(yes, Bindings)),
	    get_respons(Action),
	(   Action == redo
	->  !, fail
	;   Action == show_again
	->  fail
	;   !,
	    print_message(query, query(yes))
	).

:- flag($toplevel_print_predicate, _, print).

get_respons(Action) :-
	repeat,
	    flush_output(user_output),
	    get_single_char(Char),
	    answer_respons(Char, Action),
	    (   Action == again
	    ->  print_message(query, query(action)),
		fail
	    ;   !
	    ).

answer_respons(Char, again) :-
	memberchk(Char, "?h"), !,
	print_message(help, query(help)).
answer_respons(Char, redo) :-
	memberchk(Char, ";nrNR"), !,
	print_message(query, if_tty(';')).
answer_respons(Char, redo) :-
	memberchk(Char, "tT"), !,
	trace,
	print_message(query, if_tty('; [trace]')).
answer_respons(Char, continue) :-
	memberchk(Char, [0'c, 0'a, 0' , 10, 13, 0'y, 0'Y]), !.
answer_respons(0'b, show_again) :- !,
	break.
answer_respons(Char, show_again) :-
	print_predicate(Char, Pred, Options), !,
	print_message(query, if_tty(Pred)),
	set_prolog_flag(toplevel_print_options, Options).
answer_respons(-1, show_again) :- !,
	print_message(query, halt('EOF')),
	halt(0).
answer_respons(Char, again) :-
	print_message(query, no_action(Char)).

print_predicate(0'w, [write], [quoted(true)]).
print_predicate(0'p, [print], [quoted(true), portray(true), max_depth(10)]).


		 /*******************************
		 *	    EXPANSION		*
		 *******************************/

:- user:dynamic(expand_query/4).
:- user:multifile(expand_query/4).

call_expand_query(Goal, Expanded, Bindings, ExpandedBindings) :-
	user:expand_query(Goal, Expanded, Bindings, ExpandedBindings), !.
call_expand_query(Goal, Goal, Bindings, Bindings).


:- user:dynamic(expand_answer/2).
:- user:multifile(expand_answer/2).

call_expand_answer(Goal, Expanded) :-
	user:expand_answer(Goal, Expanded), !.
call_expand_answer(Goal, Goal).
