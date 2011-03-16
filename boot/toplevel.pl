/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemakjan@science.uva.nl
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

:- module('$toplevel',
	  [ '$initialise'/0,		% start Prolog (does not return)
	    '$toplevel'/0,		% Prolog top-level (re-entrant)
	    '$abort'/0,			% restart after an abort
	    '$break'/0,			% live in a break
	    '$compile'/0,		% `-c' toplevel
	    '$welcome'/0,		% banner
	    prolog/0, 			% user toplevel predicate
	    '$set_prompt'/1,		% set the main prompt
	    (initialization)/1,		% initialization goal (directive)
	    '$thread_init'/0,		% initialise thread
	    (thread_initialization)/1	% thread initialization goal
	    ]).


		/********************************
		*         INITIALISATION        *
		*********************************/

%	note: loaded_init_file/2 is used by prolog_load_context/2 to
%	confirm we are loading a script.

:- dynamic
	loaded_init_file/2.		% already loaded init files

'$welcome' :-
	print_message(banner, welcome).

'$load_init_file'(none) :- !.
'$load_init_file'(Base) :-
	loaded_init_file(Base, _), !.
'$load_init_file'(InitFile) :-
	is_absolute_file_name(InitFile), !,
	ensure_loaded(user:InitFile).
'$load_init_file'(Base) :-
	absolute_file_name(user_profile(Base),
			   [ access(read),
			     file_errors(fail)
			   ], InitFile),
	asserta(loaded_init_file(Base, InitFile)),
	ensure_loaded(user:InitFile).
'$load_init_file'(_).

'$load_system_init_file' :-
	loaded_init_file(system, _), !.
'$load_system_init_file' :-
	'$option'(system_init_file, Base, Base),
	Base \== none,
	current_prolog_flag(home, Home),
	file_name_extension(Base, rc, Name),
	atomic_list_concat([Home, '/', Name], File),
	absolute_file_name(File, Path,
			   [ file_type(prolog),
			     access(read),
			     file_errors(fail)
			   ]),
	asserta(loaded_init_file(system, Path)),
	load_files(user:Path, [silent(true)]), !.
'$load_system_init_file'.

'$load_script_file' :-
	loaded_init_file(script, _), !.
'$load_script_file' :-
	'$option'(script_file, OsFile, OsFile),
	OsFile \== '',
	prolog_to_os_filename(File, OsFile),
	(   absolute_file_name(File, Path,
			       [ file_type(prolog),
				 access(read),
				 file_errors(fail)
			       ])
	->  asserta(loaded_init_file(script, Path)),
	    load_files(user:Path, [])
	;   throw(error(existence_error(script_file, File), _))
	).
'$load_script_file'.

'$load_gnu_emacs_interface' :-
	(   getenv('EMACS', t),
	    current_prolog_flag(argv, Args),
	    memberchk('+C', Args)
	->  ensure_loaded(user:library(emacs_interface))
	;   true
	).


		 /*******************************
		 *	 AT_INITIALISATION	*
		 *******************************/

:- meta_predicate
	initialization(0).

:- '$iso'((initialization)/1).

%%	initialization(:Goal)
%
%	Runs Goal after loading the file in which this directive
%	appears as well as after restoring a saved state.
%
%	@see initialization/2

initialization(Goal) :-
	Goal = _:G,
	prolog:initialize_now(G, Use), !,
	print_message(warning, initialize_now(G, Use)),
	initialization(Goal, now).
initialization(Goal) :-
	initialization(Goal, after_load).

:- multifile
	prolog:initialization_now/2,
	prolog:message//1.

prolog:initialize_now(load_foreign_library(_),
		      'use :- use_foreign_library/1 instead').
prolog:initialize_now(load_foreign_library(_,_),
		      'use :- use_foreign_library/2 instead').

prolog:message(initialize_now(Goal, Use)) -->
	[ 'Initialization goal ~p will be executed'-[Goal],nl,
	  'immediately for backward compatibility reasons', nl,
	  '~w'-[Use]
	].

'$run_initialization' :-
	'$run_initialization'(_),
	'$thread_init'.


		 /*******************************
		 *     THREAD INITIALIZATION	*
		 *******************************/

:- meta_predicate
	thread_initialization(0).
:- dynamic
	'$at_thread_initialization'/1.

%%	thread_initialization(:Goal)
%
%	Run Goal now and everytime a new thread is created.

thread_initialization(Goal) :-
	assert('$at_thread_initialization'(Goal)),
	call(Goal), !.

'$thread_init' :-
	(   '$at_thread_initialization'(Goal),
	    (	call(Goal)
	    ->	fail
	    ;	fail
	    )
	;   true
	).


		 /*******************************
		 *     FILE SEARCH PATH (-p)	*
		 *******************************/

'$set_file_search_paths' :-
	current_prolog_flag(argv, Argv),
	'$append'(H, ['-p', Path|_], Argv),
	\+ memberchk(--, H),
	(   atom_chars(Path, Chars),
	    (	phrase('$search_path'(Name, Aliases), Chars)
	    ->	'$reverse'(Aliases, Aliases1),
	        forall('$member'(Alias, Aliases1),
		       asserta(user:file_search_path(Name, Alias)))
	    ;   print_message(error, commandline_arg_type(p, Path))
	    )
	->  true
	),
	fail ; true.

'$search_path'(Name, Aliases) -->
	'$string'(NameChars),
	[=], !,
	{atom_chars(Name, NameChars)},
	'$search_aliases'(Aliases).

'$search_aliases'([Alias|More]) -->
	'$string'(AliasChars),
	path_sep, !,
	{ '$make_alias'(AliasChars, Alias) },
	'$search_aliases'(More).
'$search_aliases'([Alias]) -->
	'$string'(AliasChars),
	'$eos', !,
	{ '$make_alias'(AliasChars, Alias) }.

path_sep -->
	{ current_prolog_flag(windows, true)
	}, !,
	[;].
path_sep -->
	[:].

'$string'([]) --> [].
'$string'([H|T]) --> [H], '$string'(T).

'$eos'([], []).

'$make_alias'(Chars, Alias) :-
	catch(term_to_atom(Alias, Chars), _, fail),
	(   atom(Alias)
	;   functor(Alias, F, 1),
	    F \== /
	), !.
'$make_alias'(Chars, Alias) :-
	atom_chars(Alias, Chars).


		 /*******************************
		 *   LOADING ASSIOCIATED FILES	*
		 *******************************/

%%	set_associated_file
%
%	If SWI-Prolog is started as <exe> <file>.<ext>, where <ext> is
%	the extension registered for associated files, set the Prolog
%	flag associated_file, switch to the directory holding the file
%	and -if possible- adjust the window title.

set_associated_file :-
	current_prolog_flag(saved_program_class, runtime), !.
set_associated_file :-
	'$set_prolog_file_extension',
	current_prolog_flag(associate, Ext),
	current_prolog_flag(argv, Argv),
	'$append'(Pre, [OsFile], Argv),
	\+ memberchk(--, Pre),
	\+ '$append'(_, ['-f'], Pre),	% Avoid loading twice
	prolog_to_os_filename(File, OsFile),
	file_name_extension(_, Ext, File),
	access_file(File, read), !,
	file_directory_name(File, Dir),
	working_directory(_, Dir),
	create_prolog_flag(associated_file, File, []),
	(   current_predicate(system:window_title/2)
	->  atom_concat('SWI-Prolog -- ', File, Title),
	    system:window_title(_, Title)
	;   true
	).
set_associated_file.


%%	start_pldoc
%
%	If the option  =|--pldoc[=port]|=  is   given,  load  the  PlDoc
%	system.

start_pldoc :-
	current_prolog_flag(argv, Argv),
	'$member'(Av, Argv),
	(   Av == (--)
	->  !
	;   atom_concat('--pldoc', Rest, Av)
	->  (   Rest == ''
	    ->	call((doc_server(_),
		      doc_browser))
	    ;	atom_concat(=, PortAtom, Rest),
		catch(atom_number(PortAtom, Port), _, fail)
	    ->	call(doc_server(Port))
	    ;	print_message(error, option_usage(pldoc)),
		halt(1)
	    )
	).
start_pldoc.


%%	load_associated_file
%
%	Load  the  file-name  set  by   set_associated_file/0  from  the
%	commandline arguments. Note the expand(false) to avoid expanding
%	special characters in the filename.

load_associated_file :-
	current_prolog_flag(associated_file, File),
	load_files(user:File, [expand(false)]).
load_associated_file.


hkey('HKEY_CURRENT_USER/Software/SWI/Prolog').
hkey('HKEY_LOCAL_MACHINE/Software/SWI/Prolog').

'$set_prolog_file_extension' :-
	'$c_current_predicate'(_, system:win_registry_get_value(_,_,_)),
	hkey(Key),
	catch(win_registry_get_value(Key, fileExtension, Ext0),
	      _, fail), !,
	(   atom_concat('.', Ext, Ext0)
	->  true
	;   Ext = Ext0
	),
	create_prolog_flag(associate, Ext, []).
'$set_prolog_file_extension'.


		/********************************
		*        TOPLEVEL GOALS         *
		*********************************/

:- flag('$banner_goal', _, '$welcome').

'$initialise' :-
	catch(initialise_prolog, E, initialise_error(E)).

initialise_error('$aborted') :- !.
initialise_error(E) :-
	print_message(error, initialization_exception(E)),
	fail.

initialise_prolog :-
	'$clean_history',
	set_associated_file,
	'$set_file_search_paths',
	once(print_predicate(_, [print], PrintOptions)),
	create_prolog_flag(toplevel_print_options, PrintOptions, []),
	create_prolog_flag(prompt_alternatives_on, determinism, []),
	create_prolog_flag(toplevel_extra_white_line, true, []),
	create_prolog_flag(toplevel_print_factorized, false, []),
	'$set_debugger_print_options'(print),
	'$run_initialization',
	'$load_system_init_file',
	'$load_gnu_emacs_interface',
	'$option'(init_file, OsFile, OsFile),
	prolog_to_os_filename(File, OsFile),
	'$load_init_file'(File),
	start_pldoc,
	'$load_script_file',
	load_associated_file,
	'$option'(goal, GoalAtom, GoalAtom),
	term_to_atom(Goal, GoalAtom),
	(   Goal == '$welcome'
	->  flag('$banner_goal', TheGoal, TheGoal)
	;   TheGoal = Goal
	),
	ignore(user:TheGoal).

'$abort' :-
	see(user),
	tell(user),
	flag('$break_level', _, 0),
	flag('$compilation_level', _, 0),
	'$calleventhook'(abort),
	print_message(informational, '$aborted'),
	'$toplevel'.

'$break' :-
	flag('$break_level', Old, Old+1),
	flag('$break_level', New, New),
	print_message(informational, break(enter(New))),
	'$runtoplevel',
	print_message(informational, break(exit(New))),
	flag('$break_level', _, Old), !.

:- '$hide'('$toplevel'/0).		% avoid in the GUI stacktrace
:- '$hide'('$abort'/0).			% same after an abort

'$toplevel' :-
	'$runtoplevel',
	print_message(informational, halt).

%	Actually run the toplevel.  If there is a syntax error in the
%	goal there is no reason to persue.  Something like that should
%	happen to repetitive exceptions in the toplevel as well, but
%	how do we distinguish between a stupid user and a program
%	crashing in a loop?

'$runtoplevel' :-
	'$option'(toplevel, TopLevelAtom, TopLevelAtom),
	catch(term_to_atom(TopLevel, TopLevelAtom), E,
	      (print_message(error, E),
	       halt(1))),
	user:TopLevel.

%	'$compile'
%	Toplevel called when invoked with -c option.

'$compile' :-
	'$run_initialization',
	'$load_system_init_file',
	'$set_file_search_paths',
	catch('$compile_wic', E, (print_message(error, E), halt(1))).


		/********************************
		*    USER INTERACTIVE LOOP      *
		*********************************/

prolog :-
	flag('$tracing', _, off),
	flag('$break_level', BreakLev, BreakLev),
	repeat,
	    (   '$module'(TypeIn, TypeIn),
		(   stream_property(user_input, tty(true))
		->  '$system_prompt'(TypeIn, BreakLev, Prompt),
		    prompt(Old, '|    ')
		;   Prompt = '',
		    prompt(Old, '')
		),
		trim_stacks,
		read_query(Prompt, Goal, Bindings),
		prompt(_, Old),
		call_expand_query(Goal, ExpandedGoal,
				  Bindings, ExpandedBindings)
	    ->  '$execute'(ExpandedGoal, ExpandedBindings)
	    ), !.


read_query(Prompt, Goal, Bindings) :-
	current_prolog_flag(history, N),
	integer(N),
	N =< 0, !,
	remove_history_prompt(Prompt, Prompt1),
	repeat,				% over syntax errors
	prompt1(Prompt1),
	catch('$raw_read'(user_input, Line), E,
	      (print_message(error, E),
	       (   E = error(syntax_error(_), _)
	       ->  fail
	       ;   throw(E)
	       ))),
	(   current_predicate(_, user:rl_add_history(_))
	->  format(atom(CompleteLine), '~W~W',
		   [ Line, [partial(true)],
		     '.', [partial(true)]
		   ]),
	    call(user:rl_add_history(CompleteLine))
	;   true
	),
	catch(atom_to_term(Line, Goal, Bindings), E,
	      (   print_message(error, E),
		  fail
	      )), !,
	'$save_history'(Line).
read_query(Prompt, Goal, Bindings) :-
	seeing(Old), see(user_input),
	(   read_history(h, '!h',
			 [trace, end_of_file],
			 Prompt, Goal, Bindings)
	->  see(Old)
	;   see(Old),
	    fail
	).

remove_history_prompt('', '') :- !.
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
	(   (   current_prolog_flag(readline, true)
	    ;	current_prolog_flag(emacs_inferior_process, true)
	    )
	->  create_prolog_flag(history, 0, [])
	;   create_prolog_flag(history, 25, [])
	).

:- initialization set_default_history.


		 /*******************************
		 *	  TOPLEVEL DEBUG	*
		 *******************************/

save_debug :-
	(   tracing,
	    notrace
	->  Tracing = true
	;   Tracing = false
	),
	current_prolog_flag(debug, Debugging),
	set_prolog_flag(debug, false),
	create_prolog_flag(query_debug_settings,
			   debug(Debugging, Tracing), []).

restore_debug :-
	current_prolog_flag(query_debug_settings, debug(Debugging, Tracing)),
	set_prolog_flag(debug, Debugging),
	(   Tracing == true
	->  trace
	;   true
	).

:- initialization
	create_prolog_flag(query_debug_settings, debug(false, false), []).


		/********************************
		*            PROMPTING		*
		********************************/

:- dynamic
	'$prompt'/1.

'$prompt'("%m%d%l%! ?- ").

'$set_prompt'(P) :-
	atom_codes(P, S),
	retractall('$prompt'(_)),
	assert('$prompt'(S)).


'$system_prompt'(Module, BrekLev, Prompt) :-
	'$prompt'(P0),
	(    Module \== user
	->   '$substitute'("%m", [Module, ": "], P0, P1)
	;    '$substitute'("%m", [], P0, P1)
	),
	(    BrekLev \== 0
	->   '$substitute'("%l", ["[", BrekLev, "] "], P1, P2)
	;    '$substitute'("%l", [], P1, P2)
	),
	current_prolog_flag(query_debug_settings, debug(Debugging, Tracing)),
	(    Tracing == true
	->   '$substitute'("%d", ["[trace] "], P2, P3)
	;    Debugging == true
	->   '$substitute'("%d", ["[debug] "], P2, P3)
	;    '$substitute'("%d", [], P2, P3)
	),
	atom_chars(Prompt, P3).

'$substitute'(From, T, Old, New) :-
	phrase(subst_chars(T), T0),
	'$append'(Pre, S0, Old),
	'$append'(From, Post, S0) ->
	'$append'(Pre, T0, S1),
	'$append'(S1, Post, New), !.
'$substitute'(_, _, Old, Old).

subst_chars([]) -->
	[].
subst_chars([H|T]) -->
	{ atomic(H), !,
	  atom_codes(H, Codes)
	},
	Codes,
	subst_chars(T).
subst_chars([H|T]) -->
	H,
	subst_chars(T).


		/********************************
		*           EXECUTION		*
		********************************/

'$execute'(Var, _) :-
	var(Var), !,
	print_message(informational, var_query(Var)),
	fail.
'$execute'(end_of_file, _) :- !,
	print_message(query, query(eof)).
'$execute'(Goal, Bindings) :-
	'$module'(TypeIn, TypeIn),
	'$dwim_correct_goal'(TypeIn:Goal, Bindings, Corrected), !,
	setup_call_cleanup('$set_source_module'(M0, TypeIn),
			   expand_goal(Corrected, Expanded),
			   '$set_source_module'(_, M0)),
	print_message(silent, toplevel_goal(Expanded, Bindings)),
	'$execute_goal2'(Expanded, Bindings).
'$execute'(_, _) :-
	notrace,
	print_message(query, query(no)),
	fail.

'$execute_goal2'(Goal, Bindings) :-
	restore_debug,
	Goal,
	deterministic(Det),
	(   save_debug
	;   restore_debug, fail
	),
	flush_output(user_output),
	call_expand_answer(Bindings, NewBindings),
	(    \+ \+ write_bindings(NewBindings, Det)
	->   !, fail
	).
'$execute_goal2'(_, _) :-
	save_debug,
	print_message(query, query(no)),
	fail.

%%	write_bindings(+Bindings, +Deterministic)
%
%	Write   bindings   resulting   from   a     query.    The   flag
%	prompt_alternatives_on determines whether the   user is prompted
%	for alternatives. =groundness= gives   the  classical behaviour,
%	=determinism= is considered more adequate and informative.

write_bindings(Bindings, Det) :-
	\+ term_attvars(Bindings, []), !,
	copy_term(Bindings, Bindings1, Residuals0),
	'$module'(TypeIn, TypeIn),
	omit_qualifiers(Residuals0, TypeIn, Residuals),
	join_same_bindings(Bindings1, Bindings2),
	factorize_bindings(Bindings2, Bindings3),
	bind_vars(Bindings3, Bindings4),
	filter_bindings(Bindings4, Bindings5),
	write_bindings2(Bindings5, Residuals, Det).
write_bindings(Bindings, Det) :-
	join_same_bindings(Bindings, Bindings1),
	factorize_bindings(Bindings1, Bindings2),
	bind_vars(Bindings2, Bindings3),
	filter_bindings(Bindings3, Bindings4),
	write_bindings2(Bindings4, [], Det).

write_bindings2([], Residuals, _) :-
	current_prolog_flag(prompt_alternatives_on, groundness), !,
	print_message(query, query(yes(Residuals))).
write_bindings2(Bindings, Residuals, true) :-
	current_prolog_flag(prompt_alternatives_on, determinism), !,
	print_message(query, query(yes(Bindings, Residuals))).
write_bindings2(Bindings, Residuals, _Det) :-
	repeat,
	    print_message(query, query(more(Bindings, Residuals))),
	    get_respons(Action),
	(   Action == redo
	->  !, fail
	;   Action == show_again
	->  fail
	;   !,
	    print_message(query, query(done))
	).


%%	join_same_bindings(Bindings0, Bindings)
%
%	Join variables that are bound to the   same  value. Note that we
%	return the _last_ value. This is   because the factorization may
%	be different and ultimately the names will   be  printed as V1 =
%	V2, ... VN = Value. Using the  last, Value has the factorization
%	of VN.

join_same_bindings([], []).
join_same_bindings([Name=Val|T0], [[Name|Names]=V|T]) :-
	take_same_bindings(Val, V, T0, Names, T1),
	join_same_bindings(T1, T).

take_same_bindings(Val, V, [Name=V1|T0], L, T) :-
	V1 == Val, !,
	L = [Name|Names],
	take_same_bindings(V1, V, T0, Names, T).
take_same_bindings(V, V, L, [], L).


%%	omit_qualifiers(+QGoals, +TypeIn, -Goals) is det.
%
%	Omit unneeded module qualifiers  from   QGoals  relative  to the
%	given module TypeIn.


omit_qualifiers([], _, []).
omit_qualifiers([Goal0|Goals0], TypeIn, [Goal|Goals]) :-
	omit_qualifier(Goal0, TypeIn, Goal),
	omit_qualifiers(Goals0, TypeIn, Goals).

omit_qualifier(M:G0, TypeIn, G) :-
	M == TypeIn, !,
	omit_meta_qualifiers(G0, TypeIn, G).
omit_qualifier(M:G0, TypeIn, G) :-
	predicate_property(TypeIn:G0, imported_from(M)),
	\+ predicate_property(G0, transparent), !,
	G0 = G.
omit_qualifier(_:G0, _, G) :-
	predicate_property(G0, built_in),
	\+ predicate_property(G0, transparent), !,
	G0 = G.
omit_qualifier(M:G0, _, M:G) :-
	atom(M), !,
	omit_meta_qualifiers(G0, M, G).
omit_qualifier(G0, TypeIn, G) :-
	omit_meta_qualifiers(G0, TypeIn, G).

omit_meta_qualifiers(V, _, V) :-
	var(V), !.
omit_meta_qualifiers((QA,QB), TypeIn, (A,B)) :- !,
	omit_qualifier(QA, TypeIn, A),
	omit_qualifier(QB, TypeIn, B).
omit_meta_qualifiers(freeze(V, QGoal), TypeIn, freeze(V, Goal)) :-
	callable(QGoal), !,
	omit_qualifier(QGoal, TypeIn, Goal).
omit_meta_qualifiers(when(Cond, QGoal), TypeIn, when(Cond, Goal)) :-
	callable(QGoal), !,
	omit_qualifier(QGoal, TypeIn, Goal).
omit_meta_qualifiers(G, _, G).


%%	bind_vars(+BindingsIn, -Bindings)
%
%	Bind variables to '$VAR'(Name), so they are printed by the names
%	used in the query. Note that by   binding  in the reverse order,
%	variables bound to one another come out in the natural order.

bind_vars(Bindings0, Bindings) :-
	bind_query_vars(Bindings0, Bindings, SNames),
	bind_skel_vars(Bindings, Bindings, SNames, 1, _).

bind_query_vars([], [], []).
bind_query_vars([binding(Names,Var,[Var2=Cycle])|T0],
		[binding(Names,Cycle,[])|T], [Name|SNames]) :-
	Var == Var2, !,			% also implies var(Var)
	'$last'(Names, Name),
	Var = '$VAR'(Name),
	bind_query_vars(T0, T, SNames).
bind_query_vars([B|T0], [B|T], AllNames) :-
	B = binding(Names,Var,Skel),
	bind_query_vars(T0, T, SNames),
	(   var(Var), \+ attvar(Var), Skel == []
	->  AllNames = [Name|SNames],
	    '$last'(Names, Name),
	    Var = '$VAR'(Name)
	;   AllNames = SNames
	).



bind_skel_vars([], _, _, N, N).
bind_skel_vars([binding(_,_,Skel)|T], Bindings, SNames, N0, N) :-
	bind_one_skel_vars(Skel, Bindings, SNames, N0, N1),
	bind_skel_vars(T, Bindings, SNames, N1, N).

%%	bind_one_skel_vars(+Subst, +Bindings, +VarName, +N0, -N)
%
%	Give names to the factorized variables that   do not have a name
%	yet. This introduces names  _S<N>,   avoiding  duplicates.  If a
%	factorized variable shares with another binding, use the name of
%	that variable.
%
%	@tbd	Consider the call below. We could remove either of the
%		A = x(1).  Which is best?
%
%		==
%		?- A = x(1), B = a(A,A).
%		A = x(1),
%		B = a(A, A), % where
%		    A = x(1).
%		==

bind_one_skel_vars([], _, _, N, N).
bind_one_skel_vars([Var=Value|T], Bindings, Names, N0, N) :-
	(   var(Var)
	->  (	'$member'(binding(Names, VVal, []), Bindings),
	        same_term(Value, VVal)
	    ->	'$last'(Names, VName),
		Var = '$VAR'(VName),
		N2 = N0
	    ;	between(N0, infinite, N1),
	        atom_concat('_S', N1, Name),
		\+ memberchk(Name, Names), !,
		Var = '$VAR'(Name),
		N2 is N1 + 1
	    )
	;   N2 = N0
	),
	bind_one_skel_vars(T, Bindings, Names, N2, N).


%%	factorize_bindings(+Bindings0, -Factorized)
%
%	Factorize cycles and sharing in the bindings.

factorize_bindings([], []).
factorize_bindings([Name=Value|T0], [binding(Name, Skel, Subst)|T]) :-
	F = f(Value),
	'$factorize_term'(F, Subst0),
	(   current_prolog_flag(toplevel_print_factorized, true)
	->  Subst = Subst0
	;   only_cycles(Subst0, Subst)
	),
	arg(1, F, Skel),
	factorize_bindings(T0, T).


only_cycles([], []).
only_cycles([B|T0], List) :-
	(   B = (Var=Value),
	    Var = Value,
	    acyclic_term(Var)
	->  only_cycles(T0, List)
	;   List = [B|T],
	    only_cycles(T0, T)
	).


%%	filter_bindings(+Bindings0, -Bindings)
%
%	Remove bindings that must not be printed. There are two of them:
%	Variables whose name start with '_'  and variables that are only
%	bound to themselves (or, unbound).

filter_bindings([], []).
filter_bindings([H0|T0], T) :-
	hide_vars(H0, H),
	(   (   arg(1, H, [])
	    ;   self_bounded(H)
	    )
	->  filter_bindings(T0, T)
	;   T = [H|T1],
	    filter_bindings(T0, T1)
	).

hide_vars(binding(Names0, Skel, Subst), binding(Names, Skel, Subst)) :-
	hide_names(Names0, Skel, Subst, Names).

hide_names([], _, _, []).
hide_names([Name|T0], Skel, Subst, T) :-
	(   sub_atom(Name, 0, _, _, '_'),
	    current_prolog_flag(toplevel_print_anon, false)
	->  true
	;   Subst == [],
	    Skel == '$VAR'(Name)
	), !,
	hide_names(T0, Skel, Subst, T).
hide_names([Name|T0], Skel, Subst, [Name|T]) :-
	hide_names(T0, Skel, Subst, T).

self_bounded(binding([Name], Value, [])) :-
	Value == '$VAR'(Name).

%%	get_respons(-Action)
%
%	Read the continuation entered by the user.

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
	memberchk(Char, ";nrNR \t"), !,
	print_message(query, if_tty(';')).
answer_respons(Char, redo) :-
	memberchk(Char, "tT"), !,
	trace,
	save_debug,
	print_message(query, if_tty('; [trace]')).
answer_respons(Char, continue) :-
	memberchk(Char, "ca\n\ryY."), !,
	print_message(query, if_tty('.')).
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

print_predicate(0'w, [write], [ quoted(true),
				spacing(next_argument)
			      ]).
print_predicate(0'p, [print], [ quoted(true),
				portray(true),
				max_depth(10),
				spacing(next_argument)
			      ]).


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
