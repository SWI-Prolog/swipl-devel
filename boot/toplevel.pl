/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    jan@swi.psy.uva.nl

    Purpose: top level user interaction
*/

:- module($toplevel,
	[ $init/0 			% start Prolog (does not return)
	, $init_return/0		% initialise Prolog and return
	, $toplevel/0			% Prolog top-level (re-entrant)
	, $abort/0 			% restart after an abort
	, $break/0 			% live in a break
	, $compile/0 			% `-c' toplevel
	, $welcome/0			% banner
	, prolog/0 			% user toplevel predicate
	, time/1			% time query
	, $set_prompt/1			% set the main prompt
	, at_initialisation/1		% goals to run at initialisation
	]).


		/********************************
		*         INITIALISATION        *
		*********************************/

$welcome :-
	feature(version, Version),
	(   feature(runtime, true)
	->  $ttyformat('Welcome to SWI-Prolog RUNTIME (Version ~w)~n',
		       [Version])
	;   $ttyformat('Welcome to SWI-Prolog (Version ~w)~n', [Version])
	),
	$ttyformat('Copyright (c) 1993-1995 University of Amsterdam.  '),
	$ttyformat('All rights reserved.~n~n').

$load_init_file(none) :- !.
$load_init_file(Base) :-
	member(Prefix, ['', '~/']),
	concat(Prefix, Base, InitFile), 
	exists_file(InitFile), !, 
	user:ensure_loaded(InitFile).
$load_init_file(_).

$check_novice :-
	$novice(on, on), 
	getenv('PROLOGCHILD', _), !, 
	format('Cannot start Prolog from a child process running under Prolog~n'), 
	format('Please type Control-D or `exit'' to return to Prolog~n'), 
	halt.
$check_novice.


$load_gnu_emacs_interface :-
	getenv('EMACS', t),
	$argv(Args),
	memberchk('+C', Args), !,
	user:ensure_loaded(library(emacs_interface)).
$load_gnu_emacs_interface.

		 /*******************************
		 *	 AT_INITIALISATION	*
		 *******************************/

:- module_transparent
	at_initialisation/1.
:- dynamic
	$at_initialisation/1.

at_initialisation(Spec) :-
	$strip_module(Spec, Module, Goal),
	'$toplevel':assert($at_initialisation(Module:Goal)).

$run_at_initialisation :-
	$at_initialisation(Goal),
	(   Goal
	->  fail
	;   $warning('at_initialisation goal ~p failed~n', [Goal]),
	    fail
	).
$run_at_initialisation.


		/********************************
		*        TOPLEVEL GOALS         *
		*********************************/

$init :-
	$init_return,
	$toplevel.

$init_return :-
	$check_novice, 
	$clean_history,
	$load_gnu_emacs_interface,
	(   feature(runtime, true)
	->  true
	;   $option(init_file, File, File), 
	    $load_init_file(File)
	),
	$run_at_initialisation,
	$option(goal, GoalAtom, GoalAtom), 
	term_to_atom(Goal, GoalAtom), 
	ignore(user:Goal).

$abort :-
	see(user), 
	tell(user), 
	flag($break_level, _, 0), 
	flag($compilation_level, _, 0),
	$ttyformat('~nExecution Aborted~n~n'),
	$toplevel.

$break :-
	flag($break_level, Old, Old), 
	succ(Old, New), 
	flag($break_level, _, New), 
	$ttyformat('Break Level [~w]~n', [New]),
	$toplevel,
	$ttyformat('Exit Break Level [~w]~n', [New]),
	flag($break_level, _, Old), !.

$toplevel :-
	$option(top_level, TopLevelAtom, TopLevelAtom), 
	term_to_atom(TopLevel, TopLevelAtom), 
	user:TopLevel.

%	$compile
%	Tolpevel called when invoked with -c option.

$compile :-
	$compile_wic.


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
		read_history(h, '!h', 
			      [trace, end_of_file], 
			      Prompt, Goal, Bindings), 
		prompt(_, Old),
		call_expand_query(Goal, ExpandedGoal,
				  Bindings, ExpandedBindings)
	    ->  $execute(ExpandedGoal, ExpandedBindings)
	    ), !.

		/********************************
		*            PROMPTING		*
		********************************/

:- dynamic
	$prompt/1.

$prompt("%m%l%! ?- ").

$set_prompt(P) :-
	name(P, S),
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
	name(Prompt, P2).
	
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
	$ttyformat('... 1,000,000 ............ 10,000,000 years later~n~n'),
	$ttyformat('~t~8|>> 42 << (last release gives the question)~n'),
	fail.
$execute(end_of_file, _) :-
 	$ttyformat('~N'), !.
$execute(Goal, Bindings) :-
	$module(TypeIn, TypeIn), 
	TypeIn:$dwim_correct_goal(Goal, Bindings, Corrected), !, 
	$execute_goal(Corrected, Bindings).
$execute(_, _) :-
	notrace, 
	$ttyformat('~nNo~n'),
	fail.

$execute_goal(trace, []) :-
	trace, 
	$ttyformat('~n'),
	$write_bindings([]), !, 
	fail.
$execute_goal(Goal, Bindings) :-
	$module(TypeIn, TypeIn), 
	$user_call(TypeIn:Goal),
	call_expand_answer(Bindings, NewBindings),
	$ttyformat('~n'),
	$write_bindings(NewBindings), !, 
	notrace, 
	fail.
$execute_goal(_, _) :-
	notrace, 
	$ttyformat('~nNo~n'),
	fail.

$user_call(Goal) :-
	Goal.

:- $hide($user_call, 1),
   $show_childs($user_call, 1),
   $set_predicate_attribute($user_call(_), system, 0).

$write_bindings([]) :- !, 
	$ttyformat('Yes~n').
$write_bindings(Bindings) :-
	repeat,
	    $output_bindings(Bindings),
	    get_respons(Action),
	(   Action == redo
	->  !, fail
	;   Action == show_again
	->  fail
	;   !, format(user_output, '~n~nYes~n', [])
	).

:- flag($toplevel_print_predicate, _, print).

$output_bindings([]) :- !,
	$ttyformat('Yes~n').
$output_bindings([Name = Var]) :- !,
	$output_binding(Name, Var),
	write(user_output, ' '),
	ttyflush.
$output_bindings([Name = Var|Rest]) :-
	$output_binding(Name, Var),
	nl(user_output),
	$output_bindings(Rest).

$output_binding(Name, Var) :-
	write(user_output, Name),
	write(user_output, ' = '),
	flag($toplevel_print_predicate, Pred, Pred),
	Goal =.. [Pred, user_output, Var],
	Goal.

get_respons(Action) :-
	repeat,
	    ttyflush,
	    get_single_char(Char),
	    answer_respons(Char, Action),
	    (   Action == again
	    ->  $ttyformat('Action? '),
		fail
	    ;   !
	    ).

answer_respons(Char, again) :-
	memberchk(Char, "?h"), !,
	show_toplevel_usage.
answer_respons(Char, redo) :-
	memberchk(Char, ";nrNR"), !,
	$format_if_tty(';~n').
answer_respons(Char, redo) :-
	memberchk(Char, "tT"), !,
	trace,
	$format_if_tty('; [trace]~n').
answer_respons(Char, continue) :-
	memberchk(Char, [0'c, 0' , 10, 13, 0'y, 0'Y]), !.
answer_respons(0'b, show_again) :- !,
	break.
answer_respons(Char, show_again) :-
	print_predicate(Char, Pred), !,
	$format_if_tty('~w~n', [Pred]),
	flag($toplevel_print_predicate, _, Pred).
answer_respons(_, again) :-
	$ttyformat('~nUnknown action (h for help)~nAction? '),
	ttyflush.

print_predicate(0'd, display).
print_predicate(0'w, write).
print_predicate(0'p, print).

show_toplevel_usage :-
	$ttyformat('~nActions:~n'),
	$ttyformat('; (n, r):     redo    t:               trace & redo~n'),
	$ttyformat('b:            break   c (ret, space):  continue~n'),
	$ttyformat('d:            display p                print~n'),
	$ttyformat('w:            write   h (?):           help~n').

$format_if_tty(Fmt) :-
	$format_if_tty(Fmt, []).
$format_if_tty(Fmt, Args) :-
	$tty, !,
	$ttyformat(Fmt, Args).
$format_if_tty(_, _).

:- module_transparent
	time/1, 
	$time_call/2.

time(Goal) :-
	statistics(cputime, OldTime), 
	statistics(inferences, OldInferences), 
	$time_call(Goal, Result), 
	statistics(inferences, NewInferences), 
	statistics(cputime, NewTime), 
	UsedTime is NewTime - OldTime, 
	UsedInf  is NewInferences - OldInferences, 
	(   UsedTime =:= 0
	->  Lips = 'Infinite'
	;   Lips is integer(UsedInf / UsedTime)
	), 
	$ttyformat('~D inferences in ~2f seconds (~w Lips)~n',
			[UsedInf, UsedTime, Lips]),
	Result == yes.

$time_call(Goal, yes) :-
	Goal, !.
$time_call(_Goal, no).


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

