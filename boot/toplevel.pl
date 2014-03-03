/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2013, University of Amsterdam
			      VU University Amsterdam

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

:- module('$toplevel',
	  [ '$initialise'/0,		% start Prolog
	    '$toplevel'/0,		% Prolog top-level (re-entrant)
	    '$compile'/0,		% `-c' toplevel
	    version/0,			% Write initial banner
	    version/1,			% Add message to the banner
	    prolog/0,			% user toplevel predicate
	    '$query_loop'/0,		% toplevel predicate
	    (initialization)/1,		% initialization goal (directive)
	    '$thread_init'/0,		% initialise thread
	    (thread_initialization)/1	% thread initialization goal
	    ]).


		 /*******************************
		 *	 FILE_SEARCH_PATH	*
		 *******************************/

:- multifile user:file_search_path/2.

user:file_search_path(user_profile, app_preferences('.')).
:- if(current_prolog_flag(windows, true)).
user:file_search_path(app_preferences, app_data('.')).
user:file_search_path(app_data, PrologAppData) :-
	current_prolog_flag(windows, true),
	catch(win_folder(appdata, AppData), _, fail),
	atom_concat(AppData, '/SWI-Prolog', PrologAppData),
	(   exists_directory(PrologAppData)
	->  true
	;   catch(make_directory(PrologAppData), _, fail)
	).
:- else.
user:file_search_path(app_data, UserLibDir) :-
	catch(expand_file_name('~/lib/swipl', [UserLibDir]), _, fail).
:- endif.
user:file_search_path(app_preferences, UserHome) :-
	catch(expand_file_name(~, [UserHome]), _, fail).


		 /*******************************
		 *	   VERSION BANNER	*
		 *******************************/

:- dynamic
	prolog:version_msg/1.

%%	version is det.
%
%	Print the Prolog banner message and messages registered using
%	version/1.

version :-
	print_message(banner, welcome).

%%	version(+Message) is det.
%
%	Add message to version/0

:- multifile
	system:term_expansion/2.

system:term_expansion((:- version(Message)),
		      prolog:version_msg(Message)).

version(Message) :-
	(   prolog:version_msg(Message)
	->  true
	;   assertz(prolog:version_msg(Message))
	).


		/********************************
		*         INITIALISATION        *
		*********************************/

%	note: loaded_init_file/2 is used by prolog_load_context/2 to
%	confirm we are loading a script.

:- dynamic
	loaded_init_file/2.		% already loaded init files

'$load_init_file'(none) :- !.
'$load_init_file'(Base) :-
	loaded_init_file(Base, _), !.
'$load_init_file'(InitFile) :-
	exists_file(InitFile), !,
	ensure_loaded(user:InitFile).
'$load_init_file'(Base) :-
	absolute_file_name(user_profile(Base),
			   [ access(read),
			     file_errors(fail)
			   ], InitFile),
	asserta(loaded_init_file(Base, InitFile)),
	load_files(user:InitFile,
		   [ scope_settings(false)
		   ]).
'$load_init_file'(_).

'$load_system_init_file' :-
	loaded_init_file(system, _), !.
'$load_system_init_file' :-
	'$option'(system_init_file, Base),
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
	load_files(user:Path,
		   [ silent(true),
		     scope_settings(false)
		   ]), !.
'$load_system_init_file'.

'$load_script_file' :-
	loaded_init_file(script, _), !.
'$load_script_file' :-
	'$option'(script_file, OsFiles),
	load_script_files(OsFiles).

load_script_files([]).
load_script_files([OsFile|More]) :-
	prolog_to_os_filename(File, OsFile),
	(   absolute_file_name(File, Path,
			       [ file_type(prolog),
				 access(read),
				 file_errors(fail)
			       ])
	->  asserta(loaded_init_file(script, Path)),
	    load_files(user:Path, []),
	    load_files(More)
	;   throw(error(existence_error(script_file, File), _))
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
	prolog:initialize_now/2,
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

%%	'$set_file_search_paths' is det.
%
%	Process -p PathSpec options.

'$set_file_search_paths' :-
	'$option'(search_paths, Paths),
	(   '$member'(Path, Paths),
	    atom_chars(Path, Chars),
	    (	phrase('$search_path'(Name, Aliases), Chars)
	    ->	'$reverse'(Aliases, Aliases1),
	        forall('$member'(Alias, Aliases1),
		       asserta(user:file_search_path(Name, Alias)))
	    ;   print_message(error, commandline_arg_type(p, Path))
	    ),
	    fail ; true
	).

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

%%	argv_files(-Files) is det.
%
%	Updated the prolog flag =argv=, extracting the leading directory
%	and files.

argv_files(Files) :-
	current_prolog_flag(argv, Argv),
	no_option_files(Argv, Argv1, Files),
	(   Argv1 \== Argv
	->  set_prolog_flag(argv, Argv1)
	;   true
	).

no_option_files([--|Argv], Argv, []) :- !.
no_option_files([OsScript|Argv], Argv, [Script]) :-
	prolog_to_os_filename(Script, OsScript),
	access_file(Script, read),
	catch(setup_call_cleanup(
		  open(Script, read, In),
		  ( get_char(In, '#'),
		    get_char(In, '!')
		  ),
		  close(In)),
	      _, fail), !.
no_option_files([OsFile|Argv0], Argv, [File|T]) :-
	file_name_extension(_, Ext, OsFile),
	user:prolog_file_type(Ext, prolog), !,
	prolog_to_os_filename(File, OsFile),
	no_option_files(Argv0, Argv, T).
no_option_files(Argv, Argv, []).

clean_argv :-
	(   current_prolog_flag(argv, [--|Argv])
	->  set_prolog_flag(argv, Argv)
	;   true
	).

%%	associated_files(-Files)
%
%	If SWI-Prolog is started as <exe> <file>.<ext>, where <ext> is
%	the extension registered for associated files, set the Prolog
%	flag associated_file, switch to the directory holding the file
%	and -if possible- adjust the window title.

associated_files([]) :-
	current_prolog_flag(saved_program_class, runtime), !,
	clean_argv.
associated_files(Files) :-
	'$set_prolog_file_extension',
	argv_files(Files),
	(   Files = [File|_]
	->  absolute_file_name(File, AbsFile),
	    set_prolog_flag(associated_file, AbsFile),
	    set_working_directory(File),
	    set_window_title(Files)
	;   true
	).

%%	set_working_directory(+File)
%
%	When opening as a GUI application, e.g.,  by opening a file from
%	the Finder/Explorer/..., we typically  want   to  change working
%	directory to the location of  the   primary  file.  We currently
%	detect that we are a GUI app  by the Prolog flag =console_menu=,
%	which is set by swipl-win[.exe].

set_working_directory(File) :-
	current_prolog_flag(console_menu, true),
	access_file(File, read), !,
	file_directory_name(File, Dir),
	working_directory(_, Dir).
set_working_directory(_).

set_window_title([File|More]) :-
	current_predicate(system:window_title/2), !,
	(   More == []
	->  Extra = []
	;   Extra = ['...']
	),
	atomic_list_concat(['SWI-Prolog --', File | Extra], ' ', Title),
	system:window_title(_, Title).
set_window_title(_).


%%	start_pldoc
%
%	If the option  =|--pldoc[=port]|=  is   given,  load  the  PlDoc
%	system.

start_pldoc :-
	'$option'(pldoc_server, Server),
	(   Server == ''
	->  call((doc_server(_), doc_browser))
	;   catch(atom_number(Server, Port), _, fail)
	->  call(doc_server(Port))
	;   print_message(error, option_usage(pldoc)),
	    halt(1)
	).
start_pldoc.


%%	load_associated_files(+Files)
%
%	Load Prolog files specified from the commandline.

load_associated_files(Files) :-
	(   '$member'(File, Files),
	    load_files(user:File, [expand(false)]),
	    fail
	;   true
	).

:- if(current_predicate(system:win_registry_get_value/3)).
hkey('HKEY_CURRENT_USER/Software/SWI/Prolog').
hkey('HKEY_LOCAL_MACHINE/Software/SWI/Prolog').

'$set_prolog_file_extension' :-
	hkey(Key),
	catch(win_registry_get_value(Key, fileExtension, Ext0),
	      _, fail), !,
	(   atom_concat('.', Ext, Ext0)
	->  true
	;   Ext = Ext0
	),
	(   user:prolog_file_type(Ext, prolog)
	->  true
	;   asserta(user:prolog_file_type(Ext, prolog))
	).
:- endif.
'$set_prolog_file_extension'.


		/********************************
		*        TOPLEVEL GOALS         *
		*********************************/

%%	'$initialise' is semidet.
%
%	Called from PL_initialise()  to  do  the   Prolog  part  of  the
%	initialization. If an exception  occurs,   this  is  printed and
%	'$initialise' fails.

'$initialise' :-
	catch(initialise_prolog, E, initialise_error(E)).

initialise_error('$aborted') :- !.
initialise_error(E) :-
	print_message(error, initialization_exception(E)),
	fail.

initialise_prolog :-
	'$clean_history',
	associated_files(Files),
	'$set_file_search_paths',
	init_debug_flags,
	'$run_initialization',
	'$load_system_init_file',
	start_pldoc,
	attach_packs,
	'$option'(init_file, OsFile),
	prolog_to_os_filename(File, OsFile),
	'$load_init_file'(File),
	'$load_script_file',
	load_associated_files(Files),
	'$option'(goal, GoalAtom),
	term_to_atom(Goal, GoalAtom),
	ignore(user:Goal).

init_debug_flags :-
	once(print_predicate(_, [print], PrintOptions)),
	create_prolog_flag(toplevel_print_options, PrintOptions, []),
	create_prolog_flag(prompt_alternatives_on, determinism, []),
	create_prolog_flag(toplevel_extra_white_line, true, []),
	create_prolog_flag(toplevel_print_factorized, false, []),
	'$set_debugger_print_options'(print).

%%	setup_colors is det.
%
%	Setup  interactive  usage  by  enabling    colored   output.

setup_colors :-
	(   stream_property(user_input, tty(true)),
	    stream_property(user_error, tty(true)),
	    stream_property(user_output, tty(true)),
	    \+ current_prolog_flag(color_term, false)
	->  catch(load_files(user:library(ansi_term),
			     [silent(true), if(not_loaded)]),
		  _, true)
	;   true
	).

setup_history :-
	(   stream_property(user_input, tty(true)),
	    current_predicate(rl_add_history/1),
	    \+ current_prolog_flag(save_history, false),
	    catch(load_files(library(prolog_history), [silent(true)]), _, fail)
	->  prolog_history(enable)
	;   true
	).


:- '$hide'('$toplevel'/0).		% avoid in the GUI stacktrace

%%	'$toplevel'/0
%
%	Called from PL_toplevel()

'$toplevel' :-
	'$runtoplevel',
	print_message(informational, halt).

%%	'$runtoplevel'
%
%	Actually run the toplevel. If there  is   a  syntax error in the
%	goal there is no reason to   persue.  Something like that should
%	happen to repetitive exceptions in the toplevel as well, but how
%	do we distinguish between  interactive   usage  that  frequently
%	raises and error and a program crashing in a loop?
%
%	@see prolog/0 is the default interactive toplevel

'$runtoplevel' :-
	'$option'(toplevel, TopLevelAtom),
	catch(term_to_atom(TopLevel0, TopLevelAtom), E,
	      (print_message(error, E),
	       halt(1))),
	toplevel_goal(TopLevel0, TopLevel),
	user:TopLevel.

toplevel_goal(prolog, '$query_loop') :- !,
	catch(setup_colors, E, print_message(warning, E)),
	catch(setup_history, E, print_message(warning, E)).
toplevel_goal(Goal, Goal).


%%	'$compile'
%
%	Toplevel called when invoked with -c option.

'$compile' :-
	'$set_file_search_paths',
	init_debug_flags,
	'$run_initialization',
	catch('$compile_wic', E, (print_message(error, E), halt(1))).


		/********************************
		*    USER INTERACTIVE LOOP      *
		*********************************/

%%	prolog
%
%	Run the Prolog toplevel. This is now  the same as break/0, which
%	pretends  to  be  in  a  break-level    if  there  is  a  parent
%	environment.

prolog :-
	break.

%%	'$query_loop'
%
%	Run the normal Prolog query loop.  Note   that  the query is not
%	protected by catch/3. Dealing with  unhandled exceptions is done
%	by the C-function query_loop().  This   ensures  that  unhandled
%	exceptions are really unhandled (in Prolog).

'$query_loop' :-
	(   current_prolog_flag(break_level, BreakLev)
	->  true
	;   BreakLev = -1
	),
	repeat,
	    (   '$module'(TypeIn, TypeIn),
		(   stream_property(user_input, tty(true))
		->  '$system_prompt'(TypeIn, BreakLev, Prompt),
		    prompt(Old, '|    ')
		;   Prompt = '',
		    prompt(Old, '')
		),
		trim_stacks,
		read_query(Prompt, Query, Bindings),
		prompt(_, Old),
		call_expand_query(Query, ExpandedQuery,
				  Bindings, ExpandedBindings)
	    ->  expand_goal(ExpandedQuery, Goal),
	        '$execute'(Goal, ExpandedBindings)
	    ), !.


read_query(Prompt, Goal, Bindings) :-
	current_prolog_flag(history, N),
	integer(N), N > 0, !,
	read_history(h, '!h',
		     [trace, end_of_file],
		     Prompt, Goal, Bindings).
read_query(Prompt, Goal, Bindings) :-
	remove_history_prompt(Prompt, Prompt1),
	repeat,				% over syntax errors
	prompt1(Prompt1),
	Catch = error(syntax_error(_), _),
	catch('$raw_read'(user_input, Line), Catch,
	      ( print_message(error, Catch),
		fail
	      )),
	save_debug_after_read,
	(   current_predicate(_, user:rl_add_history(_))
	->  format(atom(CompleteLine), '~W~W',
		   [ Line, [partial(true)],
		     '.', [partial(true)]
		   ]),
	    call(user:rl_add_history(CompleteLine))
	;   true
	),
	'$module'(TypeIn, TypeIn),
	catch(read_term_from_atom(Line, Goal,
				  [ variable_names(Bindings),
				    module(TypeIn)
				  ]), E,
	      (   print_message(error, E),
		  fail
	      )), !,
	'$save_history'(Line).


remove_history_prompt('', '') :- !.
remove_history_prompt(Prompt0, Prompt) :-
	atom_chars(Prompt0, Chars0),
	clean_history_prompt_chars(Chars0, Chars1),
	delete_leading_blanks(Chars1, Chars),
	atom_chars(Prompt, Chars).

clean_history_prompt_chars([], []).
clean_history_prompt_chars(['~', !|T], T) :- !.
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

%%	save_debug_after_read
%
%	Called right after the toplevel read to save the debug status if
%	it was modified from the GUI thread using e.g.
%
%	  ==
%	  thread_signal(main, gdebug)
%	  ==
%
%	@bug Ideally, the prompt would change if debug mode is enabled.
%	     That is hard to realise with all the different console
%	     interfaces supported by SWI-Prolog.

save_debug_after_read :-
	current_prolog_flag(debug, true), !,
	save_debug.
save_debug_after_read.

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

'$system_prompt'(Module, BrekLev, Prompt) :-
	current_prolog_flag(toplevel_prompt, PAtom),
	atom_codes(PAtom, P0),
	(    Module \== user
	->   '$substitute'("~m", [Module, ": "], P0, P1)
	;    '$substitute'("~m", [], P0, P1)
	),
	(    BrekLev > 0
	->   '$substitute'("~l", ["[", BrekLev, "] "], P1, P2)
	;    '$substitute'("~l", [], P1, P2)
	),
	current_prolog_flag(query_debug_settings, debug(Debugging, Tracing)),
	(    Tracing == true
	->   '$substitute'("~d", ["[trace] "], P2, P3)
	;    Debugging == true
	->   '$substitute'("~d", ["[debug] "], P2, P3)
	;    '$substitute'("~d", [], P2, P3)
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
join_same_bindings([Name=V0|T0], [[Name|Names]=V|T]) :-
	take_same_bindings(T0, V0, V, Names, T1),
	join_same_bindings(T1, T).

take_same_bindings([], Val, Val, [], []).
take_same_bindings([Name=V1|T0], V0, V, [Name|Names], T) :-
	V0 == V1, !,
	take_same_bindings(T0, V1, V, Names, T).
take_same_bindings([Pair|T0], V0, V, Names, [Pair|T]) :-
	take_same_bindings(T0, V0, V, Names, T).


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
	'$factorize_term'(Value, Skel, Subst0),
	(   current_prolog_flag(toplevel_print_factorized, true)
	->  Subst = Subst0
	;   only_cycles(Subst0, Subst)
	),
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
	print_message(query, if_tty([ansi(bold, ';', [])])).
answer_respons(Char, redo) :-
	memberchk(Char, "tT"), !,
	trace,
	save_debug,
	print_message(query, if_tty([ansi(bold, '; [trace]', [])])).
answer_respons(Char, continue) :-
	memberchk(Char, "ca\n\ryY."), !,
	print_message(query, if_tty([ansi(bold, '.', [])])).
answer_respons(0'b, show_again) :- !,
	break.
answer_respons(Char, show_again) :-
	print_predicate(Char, Pred, Options), !,
	print_message(query, if_tty(['~w'-[Pred]])),
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
