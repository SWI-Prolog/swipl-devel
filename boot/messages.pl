/*  $Id$

    Part of SWI-Prolog
    Designed and implemented by Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1997 University of Amsterdam. All rights reserved.
*/

:- module($messages,
	  [ print_message/2,		% +Kind, +Term
	    print_message_lines/3,	% +Stream, +Prefix, +Lines
	    message_to_string/2		% +Term, -String
	  ]).

:- multifile
	prolog:message/3.
:- discontiguous
	prolog_message/3.

translate_message(Term) -->
	{var(Term)}, !,
	[ 'Unknown message: ~p'-[Term] ].
translate_message(Term) -->
	prolog:message(Term).
translate_message(Term) -->
	prolog_message(Term).
translate_message(error(resource_error(stack), Name)) -->
	[ 'Out of ~w stack'-[Name] ].
translate_message(error(ISO, SWI)) -->
	swi_context(SWI),
	term_message(ISO),
	swi_extra(SWI).
translate_message($aborted) -->
	[ 'Execution Aborted' ].
translate_message(message_lines(Lines), L, T) :- % deal with old C-warning()
	make_message_lines(Lines, L, T).
translate_message(format(Fmt, Args)) -->
	[ Fmt-Args ].
translate_message(Term) -->
	{ Term = error(_, _) },
	[ 'Unknown exception: ~p'-[Term] ].
translate_message(Term) -->
	[ 'Unknown message: ~p'-[Term] ].

make_message_lines([], T, T) :- !.
make_message_lines([Last],  ['~w'-[Last]|T], T) :- !.
make_message_lines([L0|LT], ['~w'-[L0],nl|T0], T) :-
	make_message_lines(LT, T0, T).

term_message(Term) -->
	{var(Term)}, !,
	[ 'Unknown error term: ~p'-[Term] ].
term_message(Term) -->
	iso_message(Term).
term_message(Term) -->
	swi_message(Term).
term_message(Term) -->
	[ 'Unknown error term: ~p'-[Term] ].

iso_message(type_error(evaluable, Actual)) -->
	[ 'Arithmetic: `~p'' is not a function'-[Actual] ].
iso_message(type_error(Expected, Actual)) -->
	[ 'Type error: `~w'' expected, found `~p'''-[Expected, Actual] ].
iso_message(domain_error(Domain, Actual)) -->
	[ 'Domain error: `~w'' expected, found `~p'''-[Domain, Actual] ].
iso_message(instantiation_error) -->
	[ 'Arguments are not sufficiently instantiated' ].
iso_message(representation_error(What)) -->
	[ 'Cannot represent due to `~w'''-[What] ].
iso_message(permission_error(Action, built_in_procedure, Pred)) -->
	{ user_predicate_indicator(Pred, PI)
	},
	[ 'No permission to ~w built-in predicate `~p'''-[Action, PI] ],
	(   {Action \== export}
	->  [ nl,
	      'Use :- redefine_system_predicate(+Head) if redefinition is intended'
	    ]
	;   []
	).
iso_message(permission_error(Action, Type, Object)) -->
	[ 'No permission to ~w ~w `~p'''-[Action, Type, Object] ].
iso_message(evaluation_error(Which)) -->
	[ 'Arithmetic: evaluation error: `~p'''-[Which] ].
iso_message(existence_error(procedure, Proc)) -->
	[ 'Undefined procedure: ~p'-[Proc] ],
	{ dwim_predicates(Proc, Dwims) },
	(   {Dwims \== []}
	->  [nl, '    However, there are definitions for:', nl],
	    dwim_message(Dwims)
	;   []
	).
iso_message(existence_error(Type, Object)) -->
	[ '~w `~p'' does not exist'-[Type, Object] ].
iso_message(busy(Type, Object)) -->
	[ '~w `~p'' is busy'-[Type, Object] ].
iso_message(syntax_error(Id)) -->
	[ 'Syntax error: ' ],
	syntax_error(Id).

syntax_error(end_of_clause) -->
	[ 'Unexpected end of clause' ].
syntax_error(end_of_clause_expected) -->
	[ 'End of clause expected' ].
syntax_error(end_of_file) -->
	[ 'Unexpected end of file' ].
syntax_error(end_of_file_in_atom) -->
	[ 'End of file in quoted atom' ].
syntax_error(end_of_file_in_block_comment) -->
	[ 'End of file in /* ... */ comment' ].
syntax_error(end_of_file_in_string) -->
	[ 'End of file in quoted string' ].
syntax_error(illegal_number) -->
	[ 'Illegal number' ].
syntax_error(long_atom) -->
	[ 'Atom too long (see style_check/1)' ].
syntax_error(long_string) -->
	[ 'String too long (see style_check/1)' ].
syntax_error(operator_clash) -->
	[ 'Operator priority clash' ].
syntax_error(operator_expected) -->
	[ 'Operator expected' ].
syntax_error(operator_balance) -->
	[ 'Unbalanced operator' ].
syntax_error(cannot_start_term) -->
	[ 'Illegal start of term' ].
syntax_error(punct(Punct, End)) -->
	[ 'Unexpected `~w\' before `~w\''-[Punct, End] ].


dwim_predicates(Module:Name/_Arity, Dwims) :- !,
	findall(Dwim, dwim_predicate(Module:Name, Dwim), Dwims).
dwim_predicates(Name/_Arity, Dwims) :-
	findall(Dwim, dwim_predicate(user:Name, Dwim), Dwims).

dwim_message([]) --> [].
dwim_message([user:Head|T]) --> !,
	{functor(Head, Name, Arity)},
	[ '        ~w/~d'-[Name, Arity], nl ],
	dwim_message(T).
dwim_message([Module:Head|T]) --> !,
	{functor(Head, Name, Arity)},
	[ '        ~w:~w/~d'-[Module, Name, Arity], nl],
	dwim_message(T).
dwim_message([Head|T]) -->
	{functor(Head, Name, Arity)},
	[ '        ~w/~d'-[Name, Arity], nl],
	dwim_message(T).


swi_message(io_error(Op, Stream)) -->
	[ 'I/O error in ~w on stream ~w'-[Op, Stream] ].
swi_message(shell(execute, Cmd)) -->
	[ 'Could not execute `~w'''-[Cmd] ].
swi_message(shell(signal(Sig), Cmd)) -->
	[ 'Caught signal ~d on `~w'''-[Sig, Cmd] ].
swi_message(format(Fmt, Args)) -->
	[ Fmt-Args ].
swi_message(signal(Name, Num)) -->
	[ 'Caught signal ~d (~w)'-[Num, Name] ].
swi_message(limit_exceeded(Limit, MaxVal)) -->
	[ 'Exceeded ~w limit (~w)'-[Limit, MaxVal] ].
swi_message(goal_failed(Goal)) -->
	[ 'goal (~p) failed'-[Goal] ].
swi_message(shared_object(_Action, Message)) --> % Message = dlerror() 
	[ '~w'-[Message] ].
swi_message(system_error) -->
	[ 'error in system call'
	].

swi_context(X) -->
	{ var(X)
	}, !,
	[].
swi_context(context(Name/Arity, _Msg)) -->
	{ nonvar(Name)
	}, !,
	[ '~q/~w: '-[Name, Arity] ].
swi_context(file(Path, Line, _CharNo)) -->
	[ '~w:~d: '-[Path, Line] ].
swi_context(stream(Stream, Line, _CharNo)) -->
	[ 'Stream ~w:~d: '-[Stream, Line] ].
swi_context(_) -->
	[].

swi_extra(X) -->
	{ var(X)
	}, !,
	[].
swi_extra(context(_, Msg)) -->
	{ atomic(Msg),
	  Msg \== ''
	}, !,
	[ ' (~w)'-[Msg] ].
swi_extra(string(String, CharPos)) -->
	{ sub_string(String, 0, CharPos, _, Before),
	  sub_string(String, CharPos, _, 0, After)
	},
	[ nl, '~w'-[Before], nl, '** here **', nl, '~w'-[After] ].
swi_extra(_) -->
	[].

		 /*******************************
		 *	  NORMAL MESSAGES	*
		 *******************************/

prolog_message(initialization_exception(Goal, E)) -->
	[ 'Initialization goal ~p raised exception:'-[Goal], nl ],
	translate_message(E).
prolog_message(initialization_exception(E)) -->
	[ 'Prolog initialisation failed:', nl ],
	translate_message(E).
prolog_message(unhandled_exception(E)) -->
	[ 'Unhandled exception: ~p~n'-[E] ].
prolog_message(no_predicates_for(Spec)) -->
	[ 'No predicates for `~w'''-[Spec] ].
prolog_message(goal_failed(Context, Goal)) -->
	[ 'Goal (~w) failed: ~p'-[Context, Goal] ].
prolog_message(no_current_module(Module)) -->
	[ '~w is not a current module (created)'-[Module] ].
prolog_message(commandline_arg_type(Flag, Arg)) -->
	[ 'Bad argument to commandline option -~w: ~w'-[Flag, Arg] ].
prolog_message(missing_feature(Name)) -->
	[ 'This version of SWI-Prolog does not support ~w'-[Name] ].
prolog_message(singletons(List)) -->
	[ 'Singleton variables: ~w'-[List] ].


		 /*******************************
		 *	   LOADING FILES	*
		 *******************************/

prolog_message(modify_active_procedure(Who, What)) -->
	[ '~p: modified active procedure ~p'-[Who, What] ].
prolog_message(load_file(failed(user:File))) -->
	[ 'Failed to load ~p'-[File] ].
prolog_message(load_file(failed(Module:File))) -->
	[ 'Failed to load ~p into module ~p'-[File, Module] ].
prolog_message(mixed_directive(Goal)) -->
	[ 'Cannot pre-compile mixed load/call directive: ~p'-[Goal] ].
prolog_message(cannot_redefine_comma) -->
	[ 'Full stop in clause-body?  Cannot redefine ,/2' ].
prolog_message(illegal_autoload_index(Dir, Term)) -->
	[ 'Illegal term in INDEX file of directory ~w: ~w'-[Dir, Term] ].
prolog_message(redefined_procedure(Type, Proc)) -->
	[ 'Redefined ~w procedure ~p'-[Type, Proc] ].
prolog_message(discontiguous(Proc)) -->
	[ 'Clauses of ~p are not together in the source-file'-[Proc] ].
prolog_message(load_file(start(Level, File))) -->
	[ '~|~t~*+Loading '-[Level] ],
	load_file(File),
	[ ' ...' ].
prolog_message(load_file(done(Level, File, Action, Module, Time, Heap))) -->
	[ '~|~t~*+'-[Level] ],
	load_file(File),
	[ ' ~w'-[Action] ],
	load_module(Module),
	[ ' ~2f sec, ~D bytes'-[Time, Heap] ].
prolog_message(dwim_undefined(Goal, Alternatives)) -->
	{ goal_to_predicate_indicator(Goal, Pred)
	},
	[ 'Undefined procedure: ~p'-[Pred], nl, 
	  '    However, there are definitions for:', nl
	],
	dwim_message(Alternatives).
prolog_message(dwim_correct(Into)) -->
	[ 'Correct to: ~w? '-[Into], flush ].
prolog_message(error(loop_error(Spec), file_search(Used))) -->
	[ 'File search: too many levels of indirections on: ~p'-[Spec], nl,
	  '    Used alias expansions:', nl
	],
	used_search(Used).

used_search([]) -->
	[].
used_search([Alias=Expanded|T]) -->
	[ '        file_search_path(~p, ~p)'-[Alias, Expanded], nl ],
	used_search(T).
	
load_file(file(Spec, _Path)) -->
	(   {atomic(Spec)}
	->  [ '~w'-[Spec] ]
	;   [ '~p'-[Spec] ]
	).
%load_file(file(_, Path)) -->
%	[ '~w'-[Path] ].

load_module(user) --> !.
load_module(system) --> !.
load_module(Module) -->
	[ ' into ~w'-[Module] ].

goal_to_predicate_indicator(Goal, PI) :-
	$strip_module(Goal, Module, Head),
	functor(Head, Name, Arity),
	user_predicate_indicator(Module:Name/Arity, PI).

user_predicate_indicator(Module:PI, PI) :-
	hidden_module(Module), !.
user_predicate_indicator(PI, PI).

hidden_module(user) :- !.
hidden_module(system) :- !.
hidden_module(M) :-
	sub_atom(M, 0, _, _, $).

prolog_message(file_search(cache(Spec, _Cond), Path)) -->
	[ 'File search: ~p --> ~p (cache)'-[Spec, Path] ].
prolog_message(file_search(found(Spec, Cond), Path)) -->
	[ 'File search: ~p --> ~p OK ~p'-[Spec, Path, Cond] ].
prolog_message(file_search(tried(Spec, Cond), Path)) -->
	[ 'File search: ~p --> ~p NO ~p'-[Spec, Path, Cond] ].

		 /*******************************
		 *	        GC		*
		 *******************************/

prolog_message(gc(start)) -->
	[ 'GC: ', flush ].
prolog_message(gc(done(G, T, Time, UG, UT, RG, RT))) -->
	[ at_same_line,
	  'gained ~D+~D in ~2f sec; used ~D+~D; free ~D+~D'-
	  [G, T, Time, UG, UT, RG, RT]
	].
prolog_message(agc(start)) -->
	[ 'AGC: ', flush ].
prolog_message(agc(done(Collected, Remaining, Time))) -->
	[ at_same_line,
	  'reclaimed ~D atoms in ~2f sec. (remaining: ~D)'-
	  [Collected, Time, Remaining]
	].



		 /*******************************
		 *	  MAKE/AUTOLOAD		*
		 *******************************/

prolog_message(make(reload(Files))) -->
	{ length(Files, N)
	},
	[ 'Make: reloading ~D files'-[N] ].
prolog_message(make(done)) -->
	[ 'Make: finished' ].
prolog_message(make(library_index(Dir))) -->
	[ 'Updating index for library ~w'-[Dir] ].
prolog_message(autoload(Pred, File)) -->
	[ 'Autoloading ~p from ~w'-[Pred, File] ].
prolog_message(autoload(read_index(Dir))) -->
	[ 'Loading autoload index for ~w'-[Dir] ].


		 /*******************************
		 *	TOPLEVEL MESSAGES	*
		 *******************************/

prolog_message(version) -->
	{ current_prolog_flag(version, Version),
	  Major is Version // 10000,
	  Minor is (Version // 100) mod 100,
	  Patch is Version mod 100
	},
	[ '~w.~w.~w'-[Major, Minor, Patch] ].
prolog_message(copyright) -->
	[ 'Copyright (c) 1990-2002 University of Amsterdam. ', nl,
	  'Copy policy: LGPL (see www.gnu.org)'
	].
prolog_message(author) -->
	[ 'Jan Wielemaker (jan@swi-prolog.org)' ].
prolog_message(welcome) -->
	[ 'Welcome to SWI-Prolog (Version ' ],
	prolog_message(version),
	[ ')', nl ],
	prolog_message(copyright),
	[ nl, nl,
	  'For help, use ?- help(Topic). or ?- apropos(Word).',
	  nl, nl
	].
prolog_message(about) -->
	[ 'SWI-Prolog version ' ],
	prolog_message(version),
	[ ' by ' ],
	prolog_message(author),
	[ nl ],
	prolog_message(copyright).
prolog_message(halt) -->
	[ 'halt' ].
prolog_message(break(enter(Level))) -->
	[ 'Break level ~d'-[Level] ].
prolog_message(break(exit(Level))) -->
	[ nl, 'Exit break level ~d'-[Level] ].
prolog_message(var_query(_)) -->
	[ '... 1,000,000 ............ 10,000,000 years later', nl, nl,
	  '~t~8|>> 42 << (last release gives the question)'
	].
prolog_message(close_on_abort(Stream)) -->
	[ 'Abort: closed stream ~p'-[Stream] ].

prolog_message(query(no)) -->
	[ nl, 'No' ].
prolog_message(query(yes)) -->
	[ nl, 'Yes' ].
prolog_message(query(yes, Bindings)) -->
	bindings(Bindings),
	[ ' ', flush ].
prolog_message(query(eof)) -->
	[ nl ].

bindings([]) -->
	[].
bindings([Name = Value|T]) -->
	{ current_prolog_flag(toplevel_print_options, Options)
	},
	[ nl, '~w = ~W'-[Name, Value, Options] ],
	bindings(T).

prolog_message(query(help)) -->
	[ nl, 'Actions:', nl, nl,
	  '; (n, r):     redo    t:                 trace & redo', nl,
	  'b:            break   c (a, RET, space): continue', nl,
	  'w:            write   p                  print', nl,
	  'h (?):        help', nl, nl
	].
prolog_message(query(action)) -->
	[ 'Action? ', flush ].
prolog_message(if_tty(Text)) -->
	(   {current_prolog_flag(tty_control, true)}
	->  [ at_same_line, '~w'-[Text] ]
	;   []
	).
prolog_message(halt(Reason)) -->
	[ '~w: halt'-[Reason] ].
prolog_message(no_action(Char)) -->
	[ 'Unknown action: ~c (h for help)'-[Char], nl ].
prolog_message(query(confirm)) -->
	[ 'Please answer \'y\' or \'n\'? ', flush ].

prolog_message(history(help(Show, Help))) -->
	[ 'History Commands:', nl, 
	  '    !!.              Repeat last query', nl, 
	  '    !nr.             Repeat query numbered <nr>', nl, 
	  '    !str.            Repeat last query starting with <str>', nl, 
	  '    !?str.           Repeat last query holding <str>', nl, 
	  '    ^old^new.        Substitute <old> into <new> of last query', nl, 
	  '    !nr^old^new.     Substitute in query numbered <nr>', nl, 
	  '    !str^old^new.    Substitute in query starting with <str>', nl, 
	  '    !?str^old^new.   Substitute in query holding <str>', nl, 
	  '    ~w.~21|Show history list'-[Show], nl, 
	  '    ~w.~21|Show this list'-[Help], nl, nl
	].
prolog_message(history(no_event)) -->
	[ '! No such event' ].
prolog_message(history(bad_substitution)) -->
	[ '! Bad substitution' ].
prolog_message(history(expanded(Event))) -->
	[ '~w.'-[Event] ].
prolog_message(history(history(Events))) -->
	history_events(Events).

history_events([]) -->
	[].
history_events([Nr/Event|T]) -->
	[ '~t~w   ~8|~w.'-[Nr, Event], nl ],
	history_events(T).


		 /*******************************
		 *       DEBUGGER MESSAGES	*
		 *******************************/

prolog_message(spy(Head)) -->
	{ goal_to_predicate_indicator(Head, Pred)
	},
	[ 'Spy point on ~p'-[Pred] ].
prolog_message(nospy(Head)) -->
	{ goal_to_predicate_indicator(Head, Pred)
	},
	[ 'Spy point removed from ~p'-[Pred] ].
prolog_message(trace_mode(Bool)) -->
	[ 'Trace mode switched to ~w'-[Bool] ].
prolog_message(debug_mode(Bool)) -->
	[ 'Debug mode switched to ~w'-[Bool] ].
prolog_message(debugging(Bool)) -->
	[ 'Debug mode is ~w'-[Bool] ].
prolog_message(spying([])) --> !,
	[ 'No spy points' ].
prolog_message(spying(Heads)) -->
	[ 'Spy points (see spy/1) on:', nl ],
	predicate_list(Heads).
prolog_message(trace(Head, [])) --> !,
	{ goal_to_predicate_indicator(Head, Pred)
	},
	[ '        ~p: Not tracing'-[Pred], nl].
prolog_message(trace(Head, Ports)) -->
	{ goal_to_predicate_indicator(Head, Pred)
	},
	[ '        ~p: ~w'-[Pred, Ports], nl].
prolog_message(tracing([])) --> !,
	[ 'No traced predicates (see trace/1)' ].
prolog_message(tracing(Heads)) -->
	[ 'Trace points (see trace/1) on:', nl ],
	tracing_list(Heads).

predicate_list([]) -->			% TBD: Share with dwim, etc.
	[].
predicate_list([H|T]) -->
	{ goal_to_predicate_indicator(H, Pred)
	},
	[ '        ~p'-[Pred], nl],
	predicate_list(T).

tracing_list([]) -->
	[].
tracing_list([trace(Head, Ports)|T]) -->
	translate_message(trace(Head, Ports)),
	tracing_list(T).

prolog_message(frame(Frame, backtrace, _PC)) --> !,
	{ prolog_frame_attribute(Frame, level, Level)
	},
	[ '~t[~D] ~10|'-[Level] ],
	frame_context(Frame),
	frame_goal(Frame).
prolog_message(frame(Frame, choice, PC)) --> !,
	prolog_message(frame(Frame, backtrace, PC)).
prolog_message(frame(Frame, trace(Port), _PC)) --> !,
	[ ' T ' ],
	port(Port),
	frame_level(Frame),
	frame_context(Frame),
	frame_goal(Frame).
prolog_message(frame(Frame, Port, _PC)) -->
	frame_flags(Frame),
	port(Port),
	frame_level(Frame),
	frame_context(Frame),
	frame_goal(Frame),
	[ flush ].

frame_goal(Frame) -->
	{ prolog_frame_attribute(Frame, goal, Goal0),
	  clean_goal(Goal0, Goal),
	  current_prolog_flag(debugger_print_options, Options)
	},
	[ '~W'-[Goal, Options] ].

frame_level(Frame) -->
	{ prolog_frame_attribute(Frame, level, Level)
	},
	[ '(~D) '-[Level] ].

frame_context(Frame) -->
	(   { current_prolog_flag(debugger_show_context, true),
	      prolog_frame_attribute(Frame, context_module, Context)
	    }
	->  [ '[~w] '-[Context] ]
	;   []
	).

frame_flags(Frame) -->
	{ prolog_frame_attribute(Frame, goal, Goal),
	  (   predicate_property(Goal, transparent)
	  ->  T = '^'
	  ;   T = ' '
	  ),
	  (   predicate_property(Goal, spying)
	  ->  S = '*'
	  ;   S = ' '
	  )
	},
	[ '~w~w '-[T, S] ].
	  
port(Port) -->
	{ port_name(Port, Name)
	}, !,
	[ '~w: '-[Name] ].

port_name(call,	     'Call').
port_name(exit,	     'Exit').
port_name(fail,	     'Fail').
port_name(redo,	     'Redo').
port_name(unify,     'Unify').
port_name(exception, 'Exception').

clean_goal(M:Goal, Goal) :-
	hidden_module(M), !.
clean_goal(Goal, Goal).


		 /*******************************
		 *	PRINTING MESSAGES	*
		 *******************************/

%	print_message(+Kind, +Term)
%
%	Print an error message using a term as generated by the exception
%	system.

print_message(Level, Term) :-
	translate_message(Term, Lines, []), !,
	(   $c_current_predicate(_, user:message_hook(_,_,_)),
	    notrace(user:message_hook(Term, Level, Lines))
	->  true
	;   print_system_message(Term, Level, Lines)
	).

%	print_system_message(+Term, +Level, +Lines)
%
%	Print the message if the user did not intecept the message.
%	The first is used for errors and warnings that can be related
%	to source-location.  Note that syntax errors have their own
%	source-location and should therefore not be handled this way.

print_system_message(_, silent, _) :- !.
print_system_message(_, informational, _) :-
	current_prolog_flag(verbose, silent), !.
print_system_message(_, banner, _) :-
	current_prolog_flag(verbose, silent), !.
print_system_message(Term, Level, Lines) :-
	source_location(File, Line),
	Term \= error(syntax_error(_), _),
	prefix(Level, Prefix, LinePrefix, PostFix, Wait, Stream), !,
	format(Stream, Prefix, [File, Line]),
	print_message_lines(Stream, LinePrefix, Lines),
	format(Stream, PostFix, []),
	(   Wait > 0
	->  sleep(Wait)
	;   true
	).
print_system_message(_, Level, Lines) :-
	prefix(Level, LinePrefix, Stream), !,
	print_message_lines(Stream, LinePrefix, Lines).
	
prefix(error,	      'ERROR: (~w:~d):~n',   '\t', '', 0.5, user_error).
prefix(warning,	      'Warning: (~w:~d):~n', '\t', '', 0,   user_error).

prefix(help,	      '',          user_error).
prefix(query,	      '',          user_error).
prefix(debug,	      '',          user_output).
prefix(warning,	      'Warning: ', user_error).
prefix(error,	      'ERROR: ',   user_error).
prefix(banner,	      '',	   user_error).
prefix(informational, '% ',        user_error).

%	print_message_lines(+Stream, +Prefix, +Lines)
%
%	Quintus compatibility predicate to print message lines using
%	a prefix.

print_message_lines(_, _, []) :- !.
print_message_lines(S, P, [at_same_line|Lines]) :- !,
	print_message_line(S, Lines, Rest),
	print_message_lines(S, P, Rest).
print_message_lines(S, P, Lines) :-
	atom_concat('~N', P, Prefix),
	format(S, Prefix, []),
	print_message_line(S, Lines, Rest),
	print_message_lines(S, P, Rest).

print_message_line(S, [flush], []) :- !,
	flush_output(S).
print_message_line(S, [], []) :- !,
	nl(S).
print_message_line(S, [nl|T], T) :- !,
	nl(S).
print_message_line(S, [Fmt-Args|T0], T) :- !,
	format(S, Fmt, Args),
	print_message_line(S, T0, T).
print_message_line(S, [Fmt|T0], T) :-
	format(S, Fmt, []),
	print_message_line(S, T0, T).

%	message_to_string(+Term, -String)
%
%	Translate an error term into a string

message_to_string(Term, Str) :-
        translate_message(Term, Actions, []), !,
        actions_to_format(Actions, Fmt, Args),
        sformat(Str, Fmt, Args).

actions_to_format([], '', []) :- !.
actions_to_format([nl], '', []) :- !.
actions_to_format([Term, nl], Fmt, Args) :- !,
	actions_to_format([Term], Fmt, Args).
actions_to_format([nl|T], Fmt, Args) :- !,
	actions_to_format(T, Fmt0, Args),
	atom_concat('~n', Fmt0, Fmt).
actions_to_format([Fmt0-Args0|Tail], Fmt, Args) :- !,
        actions_to_format(Tail, Fmt1, Args1),
        atom_concat(Fmt0, Fmt1, Fmt),
        append(Args0, Args1, Args).
actions_to_format([Term|Tail], Fmt, Args) :-
	atomic(Term), !,
        actions_to_format(Tail, Fmt1, Args),
	atom_concat(Term, Fmt1, Fmt).
actions_to_format([Term|Tail], Fmt, Args) :-
        actions_to_format(Tail, Fmt1, Args1),
        atom_concat('~w', Fmt1, Fmt),
        append([Term], Args1, Args).

	
