/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@ca.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2012, University of Amsterdam
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

:- module('$messages',
	  [ print_message/2,		% +Kind, +Term
	    print_message_lines/3,	% +Stream, +Prefix, +Lines
	    message_to_string/2		% +Term, -String
	  ]).

:- multifile
	prolog:message/3,		% entire message
	prolog:error_message/3,		% 1-st argument of error term
	prolog:message_context/3,	% Context of error messages
	prolog:message_location/3,	% (File) location of error messages
	prolog:message_line_element/2.	% Extend printing
:- discontiguous
	prolog_message/3.

%%	translate_message(+Term)// is det.
%
%	Translate a message Term into message lines. The produced lines
%	is a list of
%
%	    * nl
%	    Emit a newline
%	    * Fmt-Args
%	    Emit the result of format(Fmt, Args)
%	    * Fmt
%	    Emit the result of format(Fmt)
%	    * flush
%	    Used only as last element of the list.   Simply flush the
%	    output instead of producing a final newline.
%	    * at_same_line
%	    Start the messages at the same line (instead of using ~N)

translate_message(Term) -->
	translate_message2(Term), !.
translate_message(Term) -->
	{ Term = error(_, _) },
	[ 'Unknown exception: ~p'-[Term] ].
translate_message(Term) -->
	[ 'Unknown message: ~p'-[Term] ].

translate_message2(Term) -->
	{var(Term)}, !,
	[ 'Unknown message: ~p'-[Term] ].
translate_message2(Term) -->
	prolog:message(Term).
translate_message2(Term) -->
	prolog_message(Term).
translate_message2(error(resource_error(stack), Name)) -->
	[ 'Out of ~w stack'-[Name] ].
translate_message2(error(resource_error(Missing), _)) -->
	[ 'Not enough resources: ~w'-[Missing] ].
translate_message2(error(ISO, SWI)) -->
	swi_location(SWI),
	term_message(ISO),
	swi_extra(SWI).
translate_message2('$aborted') -->
	[ 'Execution Aborted' ].
translate_message2(message_lines(Lines), L, T) :- % deal with old C-warning()
	make_message_lines(Lines, L, T).
translate_message2(format(Fmt, Args)) -->
	[ Fmt-Args ].

make_message_lines([], T, T) :- !.
make_message_lines([Last],  ['~w'-[Last]|T], T) :- !.
make_message_lines([L0|LT], ['~w'-[L0],nl|T0], T) :-
	make_message_lines(LT, T0, T).

term_message(Term) -->
	{var(Term)}, !,
	[ 'Unknown error term: ~p'-[Term] ].
term_message(Term) -->
	prolog:error_message(Term).
term_message(Term) -->
	iso_message(Term).
term_message(Term) -->
	swi_message(Term).
term_message(Term) -->
	[ 'Unknown error term: ~p'-[Term] ].

iso_message(type_error(evaluable, Actual)) -->
	[ 'Arithmetic: `~p'' is not a function'-[Actual] ].
iso_message(type_error(free_of_attvar, Actual)) -->
	[ 'Type error: `~W'' contains attributed variables'-
	  [Actual,[portray(true), attributes(portray)]] ].
iso_message(type_error(Expected, Actual)) -->
	[ 'Type error: `~w'' expected, found `~p'''-[Expected, Actual] ].
iso_message(domain_error(Domain, Actual)) -->
	[ 'Domain error: `~w'' expected, found `~p'''-[Domain, Actual] ].
iso_message(instantiation_error) -->
	[ 'Arguments are not sufficiently instantiated' ].
iso_message(uninstantiation_error(Var)) -->
	[ 'Uninstantiated argument expected, found ~p'-[Var] ].
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
iso_message(permission_error(import_into(Dest), procedure, Pred)) -->
	[ 'No permission to import ~p into ~w'-[Pred, Dest] ].
iso_message(permission_error(Action, static_procedure, Proc)) -->
	[ 'No permission to ~w static procedure `~p'''-[Action, Proc] ],
	defined_definition('Defined', Proc).
iso_message(permission_error(Action, Type, Object)) -->
	[ 'No permission to ~w ~w `~p'''-[Action, Type, Object] ].
iso_message(evaluation_error(Which)) -->
	[ 'Arithmetic: evaluation error: `~p'''-[Which] ].
iso_message(existence_error(procedure, Proc)) -->
	[ 'Undefined procedure: ~q'-[Proc] ],
	undefined_proc_msg(Proc).
iso_message(existence_error(Type, Object)) -->
	[ '~w `~p'' does not exist'-[Type, Object] ].
iso_message(busy(Type, Object)) -->
	[ '~w `~p'' is busy'-[Type, Object] ].
iso_message(syntax_error(swi_backslash_newline)) -->
	[ 'Deprecated ... \\<newline><white>*.  Use \\c' ].
iso_message(syntax_error(Id)) -->
	[ 'Syntax error: ' ],
	syntax_error(Id).
iso_message(occurs_check(Var, In)) -->
	[ 'Cannot unify ~p with ~p: would create an infinite tree'-[Var, In] ].

undefined_proc_msg(_:(^)/2) --> !,
	undefined_proc_msg((^)/2).
undefined_proc_msg((^)/2) --> !,
	[nl, '  ^/2 can only appear as the 2nd argument of setof/3 and bagof/3'].
undefined_proc_msg((:-)/2) --> !,
	[nl, '  Rules must be loaded from a file'],
	faq('ToplevelMode').
undefined_proc_msg((:-)/1) --> !,
	[nl, '  Directives must be loaded from a file'],
	faq('ToplevelMode').
undefined_proc_msg((?-)/1) --> !,
	[nl, '  ?- is the Prolog prompt'],
	faq('ToplevelMode').
undefined_proc_msg(Proc) -->
	{ dwim_predicates(Proc, Dwims) },
	(   {Dwims \== []}
	->  [nl, '  However, there are definitions for:', nl],
	    dwim_message(Dwims)
	;   []
	).

faq(Page) -->
	[nl, '  See FAQ at http://www.swi-prolog.org/FAQ/', Page, '.txt' ].


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
syntax_error(quoted_punctuation) -->
	[ 'Operand expected, unquoted comma or bar found' ].
syntax_error(list_rest) -->
	[ 'Unexpected comma or bar in rest of list' ].
syntax_error(cannot_start_term) -->
	[ 'Illegal start of term' ].
syntax_error(punct(Punct, End)) -->
	[ 'Unexpected `~w\' before `~w\''-[Punct, End] ].
syntax_error(undefined_char_escape(C)) -->
	[ 'Undefined character escape in quoted atom or string: `\\~w\''-[C] ].
syntax_error(Message) -->
	[ '~w'-[Message] ].

dwim_predicates(Module:Name/_Arity, Dwims) :- !,
	findall(Dwim, dwim_predicate(Module:Name, Dwim), Dwims).
dwim_predicates(Name/_Arity, Dwims) :-
	findall(Dwim, dwim_predicate(user:Name, Dwim), Dwims).

dwim_message([]) --> [].
dwim_message([M:Head|T]) -->
	{ hidden_module(M), !,
	  functor(Head, Name, Arity)
	},
	[ '        ~q'-[Name/Arity], nl ],
	dwim_message(T).
dwim_message([Module:Head|T]) --> !,
	{ functor(Head, Name, Arity)
	},
	[ '        ~q'-[Module:Name/Arity], nl],
	dwim_message(T).
dwim_message([Head|T]) -->
	{functor(Head, Name, Arity)},
	[ '        ~q'-[Name/Arity], nl],
	dwim_message(T).


swi_message(io_error(Op, Stream)) -->
	[ 'I/O error in ~w on stream ~p'-[Op, Stream] ].
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
	[ 'goal unexpectedly failed: ~p'-[Goal] ].
swi_message(shared_object(_Action, Message)) --> % Message = dlerror()
	[ '~w'-[Message] ].
swi_message(system_error(Error)) -->
	[ 'error in system call: ~w'-[Error]
	].
swi_message(system_error) -->
	[ 'error in system call'
	].
swi_message(failure_error(Goal)) -->
	[ 'Goal failed: ~p'-[Goal] ].
swi_message(timeout_error(Op, Stream)) -->
	[ 'Timeout in ~w from ~p'-[Op, Stream] ].
swi_message(not_implemented(Type, What)) -->
	[ '~w `~p\' is not implemented in this version'-[Type, What] ].
swi_message(context_error(nodirective, Goal)) -->
	{ goal_to_predicate_indicator(Goal, PI) },
	[ 'Wrong context: ~p can only be used in a directive'-[PI] ].
swi_message(context_error(edit, no_default_file)) -->
	(   { current_prolog_flag(windows, true) }
	->  [ 'Edit/0 can only be used after opening a \c
	       Prolog file by double-clicking it' ]
	;   [ 'Edit/0 can only be used with the "-s file" commandline option'
	    ]
	).
swi_message(format_argument_type(Fmt, Arg)) -->
	[ 'Illegal argument to format sequence ~~~w: ~p'-[Fmt, Arg] ].
swi_message(format(Msg)) -->
	[ 'Format error: ~w'-[Msg] ].
swi_message(conditional_compilation_error(unterminated, Where)) -->
	[ 'Unterminated conditional compilation from '-[] ],
	cond_location(Where).
swi_message(conditional_compilation_error(no_if, What)) -->
	[ ':- ~w without :- if'-[What] ].

cond_location(File:Line) -->
	{ file_base_name(File, Base) },
	[ '~w:~d'-[Base, Line] ].

swi_location(X) -->
	{ var(X)
	}, !,
	[].
swi_location(Context) -->
	prolog:message_location(Context), !.
swi_location(context(Caller, _Msg)) -->
	{ ground(Caller)
	}, !,
	caller(Caller).
swi_location(file(Path, Line, -1, _CharNo)) --> !,
	[ '~w:~d: '-[Path, Line] ].
swi_location(file(Path, Line, LinePos, _CharNo)) -->
	[ '~w:~d:~d: '-[Path, Line, LinePos] ].
swi_location(stream(Stream, Line, LinePos, CharNo)) -->
	(   { stream_property(Stream, file_name(File)) }
	->  swi_location(file(File, Line, LinePos, CharNo))
        ;   [ 'Stream ~w:~d:~d '-[Stream, LinePos, Line] ]
	).
swi_location(_) -->
	[].

caller(system:'$record_clause'/3) --> !,
	[].
caller(Module:Name/Arity) --> !,
	(   { \+ hidden_module(Module) }
	->  [ '~q:~q/~w: '-[Module, Name, Arity] ]
	;   [ '~q/~w: '-[Name, Arity] ]
	).
caller(Name/Arity) -->
	[ '~q/~w: '-[Name, Arity] ].
caller(Caller) -->
	[ '~p: '-[Caller] ].


swi_extra(X) -->
	{ var(X)
	}, !,
	[].
swi_extra(Context) -->
	prolog:message_context(Context).
swi_extra(context(_, Msg)) -->
	{ nonvar(Msg),
	  Msg \== ''
	}, !,
	swi_comment(Msg).
swi_extra(string(String, CharPos)) -->
	{ sub_string(String, 0, CharPos, _, Before),
	  sub_string(String, CharPos, _, 0, After)
	},
	[ nl, '~w'-[Before], nl, '** here **', nl, '~w'-[After] ].
swi_extra(_) -->
	[].

swi_comment(already_from(Module)) --> !,
	[ ' (already imported from ~q)'-[Module] ].
swi_comment(Msg) -->
	[ ' (~w)'-[Msg] ].


thread_context -->
	{ thread_self(Me), Me \== main }, !,
	['[Thread ~w] '-[Me]].
thread_context -->
	[].

		 /*******************************
		 *	  NORMAL MESSAGES	*
		 *******************************/

prolog_message(initialization_error(_, E, File:Line)) --> !,
	[ '~w:~d: '-[File, Line],
	  'Initialization goal raised exception:', nl
	],
	translate_message(E).
prolog_message(initialization_error(Goal, E, _)) -->
	[ 'Initialization goal ~p raised exception:'-[Goal], nl ],
	translate_message(E).
prolog_message(initialization_failure(_Goal, File:Line)) --> !,
	[ '~w:~d: '-[File, Line],
	  'Initialization goal failed'-[]
	].
prolog_message(initialization_failure(Goal, _)) -->
	[ 'Initialization goal failed: ~p'-[Goal]
	].
prolog_message(initialization_exception(E)) -->
	[ 'Prolog initialisation failed:', nl ],
	translate_message(E).
prolog_message(unhandled_exception(E)) -->
	[ 'Unhandled exception: ' ],
	(   translate_message2(E)
	->  []
	;   [ '~p'-[E] ]
	).
prolog_message(goal_failed(Goal, Context)) -->
	[ 'Goal (~w) failed: ~p'-[Goal, Context] ].
prolog_message(no_current_module(Module)) -->
	[ '~w is not a current module (created)'-[Module] ].
prolog_message(commandline_arg_type(Flag, Arg)) -->
	[ 'Bad argument to commandline option -~w: ~w'-[Flag, Arg] ].
prolog_message(missing_feature(Name)) -->
	[ 'This version of SWI-Prolog does not support ~w'-[Name] ].
prolog_message(singletons(List)) -->
	[ 'Singleton variables: ~w'-[List] ].
prolog_message(multitons(List)) -->
	[ 'Singleton-marked variables appearing more than once: ~w'-[List] ].
prolog_message(profile_no_cpu_time) -->
	[ 'No CPU-time info.  Check the SWI-Prolog manual for details' ].
prolog_message(non_ascii(Text, Type)) -->
	[ 'Unquoted ~w with non-portable characters: ~w'-[Type, Text] ].
prolog_message(io_warning(Stream, Message)) -->
	{ stream_property(Stream, position(Position)), !,
	  stream_position_data(line_count, Position, LineNo),
	  stream_position_data(line_position, Position, LinePos),
	  (   stream_property(Stream, file_name(File))
	  ->  Obj = File
	  ;   Obj = Stream
	  )
	},
	[ '~p:~d:~d: ~w'-[Obj, LineNo, LinePos, Message] ].
prolog_message(io_warning(Stream, Message)) -->
	[ 'stream ~p: ~w'-[Stream, Message] ].
prolog_message(option_usage(pldoc)) -->
	[ 'Usage: --pldoc[=port]' ].
prolog_message(interrupt(begin)) -->
	[ 'Action (h for help) ? ', flush ].
prolog_message(interrupt(end)) -->
	[ 'continue' ].
prolog_message(interrupt(trace)) -->
	[ 'continue (trace mode)' ].
prolog_message(unknown_in_module_user) -->
	[ 'Using a non-error value for unknown in the global module', nl,
	  'causes most of the development environment to stop working.', nl,
	  'Please use :- dynamic or limit usage of unknown to a module.', nl,
	  'See http://www.swi-prolog.org/howto/database.html'
	].


		 /*******************************
		 *	   LOADING FILES	*
		 *******************************/

prolog_message(modify_active_procedure(Who, What)) -->
	[ '~p: modified active procedure ~p'-[Who, What] ].
prolog_message(load_file(failed(user:File))) -->
	[ 'Failed to load ~p'-[File] ].
prolog_message(load_file(failed(Module:File))) -->
	[ 'Failed to load ~p into module ~p'-[File, Module] ].
prolog_message(load_file(failed(File))) -->
	[ 'Failed to load ~p'-[File] ].
prolog_message(mixed_directive(Goal)) -->
	[ 'Cannot pre-compile mixed load/call directive: ~p'-[Goal] ].
prolog_message(cannot_redefine_comma) -->
	[ 'Full stop in clause-body?  Cannot redefine ,/2' ].
prolog_message(illegal_autoload_index(Dir, Term)) -->
	[ 'Illegal term in INDEX file of directory ~w: ~w'-[Dir, Term] ].
prolog_message(redefined_procedure(Type, Proc)) -->
	[ 'Redefined ~w procedure ~p'-[Type, Proc] ],
	defined_definition('Previously defined', Proc).
prolog_message(declare_module(Module, abolish(Predicates))) -->
	[ 'Loading module ~w abolished: ~p'-[Module, Predicates] ].
prolog_message(import_private(Module, Private)) -->
	[ 'import/1: ~p is not exported (still imported into ~q)'-
	  [Private, Module]
	].
prolog_message(ignored_weak_import(Into, From:PI)) -->
	[ 'Local definition of ~p overrides weak import from ~q'-
	  [Into:PI, From]
	].
prolog_message(undefined_export(Module, PI)) -->
	[ 'Exported procedure ~q:~q is not defined'-[Module, PI] ].
prolog_message(no_exported_op(Module, Op)) -->
	[ 'Operator ~q:~q is not exported (still defined)'-[Module, Op] ].
prolog_message(discontiguous((-)/2)) -->
	prolog_message(minus_in_identifier).
prolog_message(discontiguous(Proc)) -->
	[ 'Clauses of ~p are not together in the source-file'-[Proc] ].
prolog_message(load_file(start(Level, File))) -->
	[ '~|~t~*+Loading '-[Level] ],
	load_file(File),
	[ ' ...' ].
prolog_message(include_file(start(Level, File))) -->
	[ '~|~t~*+include '-[Level] ],
	load_file(File),
	[ ' ...' ].
prolog_message(include_file(done(Level, File))) -->
	[ '~|~t~*+included '-[Level] ],
	load_file(File).
prolog_message(load_file(done(Level, File, Action, Module, Time, Clauses))) -->
	[ '~|~t~*+'-[Level] ],
	load_file(File),
	[ ' ~w'-[Action] ],
	load_module(Module),
	[ ' ~2f sec, ~D clauses'-[Time, Clauses] ].
prolog_message(dwim_undefined(Goal, Alternatives)) -->
	{ goal_to_predicate_indicator(Goal, Pred)
	},
	[ 'Undefined procedure: ~q'-[Pred], nl,
	  '    However, there are definitions for:', nl
	],
	dwim_message(Alternatives).
prolog_message(dwim_correct(Into)) -->
	[ 'Correct to: ~q? '-[Into], flush ].
prolog_message(error(loop_error(Spec), file_search(Used))) -->
	[ 'File search: too many levels of indirections on: ~p'-[Spec], nl,
	  '    Used alias expansions:', nl
	],
	used_search(Used).
prolog_message(minus_in_identifier) -->
	[ 'The "-" character should not be used to seperate words in an', nl,
	  'identifier.  Check the SWI-Prolog FAQ for details.'
	].
prolog_message(qlf(removed_after_error(File))) -->
	[ 'Removed incomplete QLF file ~w'-[File] ].
prolog_message(redefine_module(Module, OldFile, File)) -->
	[ 'Module "~q" already loaded from ~w.'-[Module, OldFile], nl,
	  'Wipe and reload from ~w? '-[File], flush
	].
prolog_message(redefine_module_reply) -->
	[ 'Please answer y(es), n(o) or a(bort)' ].
prolog_message(reloaded_in_module(Absolute, OldContext, LM)) -->
	[ '~w was previously loaded in module ~w'-[Absolute, OldContext], nl,
	  '\tnow it is reloaded into module ~w'-[LM] ].

defined_definition(Message, Spec) -->
	{ strip_module(user:Spec, M, Name/Arity),
	  functor(Head, Name, Arity),
	  predicate_property(M:Head, file(File)),
	  predicate_property(M:Head, line_count(Line))
	}, !,
	[ nl, '~w at ~w:~d'-[Message, File,Line] ].
defined_definition(_, _) --> [].

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
	strip_module(Goal, Module, Head),
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
	thread_context,
	[ 'GC: ', flush ].
prolog_message(gc(done(G, T, Time, UG, UT, RG, RT))) -->
	[ at_same_line,
	  'gained ~D+~D in ~2f sec; used ~D+~D; free ~D+~D'-
	  [G, T, Time, UG, UT, RG, RT]
	].
prolog_message(shift_stacks(start(_L,_G,_T))) -->
	thread_context,
	[ 'Stack-shift: ', flush ].
prolog_message(shift_stacks(done(Time, L, G, T))) -->
	{ LKB is L//1024,
	  GKB is G//1024,
	  TKB is T//1024
	},
	[ at_same_line,
	  'local: ~DKB, global: ~DKB, trail: ~DKB bytes (~2f sec)'-
	  [LKB, GKB, TKB, Time]
	].
prolog_message(agc(start)) -->
	thread_context,
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
prolog_message(make(done(_Files))) -->
	[ 'Make: finished' ].
prolog_message(make(library_index(Dir))) -->
	[ 'Updating index for library ~w'-[Dir] ].
prolog_message(autoload(Pred, File)) -->
	thread_context,
	[ 'autoloading ~p from ~w'-[Pred, File] ].
prolog_message(autoload(read_index(Dir))) -->
	[ 'Loading autoload index for ~w'-[Dir] ].


		 /*******************************
		 *	TOPLEVEL MESSAGES	*
		 *******************************/

prolog_message(version) -->
	{ current_prolog_flag(version_git, Version) }, !,
	[ '~w'-[Version] ].
prolog_message(version) -->
	{ current_prolog_flag(version, Version),
	  Major is Version // 10000,
	  Minor is (Version // 100) mod 100,
	  Patch is Version mod 100
	},
	[ '~w.~w.~w'-[Major, Minor, Patch] ].
prolog_message(address_bits) -->
	{ current_prolog_flag(address_bits, Bits)
	}, !,
	[ '~d bits, '-[Bits] ].
prolog_message(threads) -->
	{ current_prolog_flag(threads, true)
	}, !,
	[ 'Multi-threaded, ' ].
prolog_message(threads) -->
	[].
prolog_message(copyright) -->
	[ 'Copyright (c) 1990-2012 University of Amsterdam, VU Amsterdam', nl,
	  'SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software,', nl,
	  'and you are welcome to redistribute it under certain conditions.', nl,
	  'Please visit http://www.swi-prolog.org for details.'
	].
prolog_message(author) -->
	[ 'Jan Wielemaker (jan@swi-prolog.org)' ].
prolog_message(welcome) -->
	[ 'Welcome to SWI-Prolog (' ],
	prolog_message(threads),
	prolog_message(address_bits),
	['Version ' ],
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
prolog_message(break(begin, Level)) -->
	[ 'Break level ~d'-[Level] ].
prolog_message(break(end, Level)) -->
	[ 'Exit break level ~d'-[Level] ].
prolog_message(var_query(_)) -->
	[ '... 1,000,000 ............ 10,000,000 years later', nl, nl,
	  '~t~8|>> 42 << (last release gives the question)'
	].
prolog_message(close_on_abort(Stream)) -->
	[ 'Abort: closed stream ~p'-[Stream] ].

prolog_message(query(QueryResult)) -->
	query_result(QueryResult).

query_result(no) -->		% failure
	[ ansi([bold,fg(red)], 'false.', []) ],
	extra_line.
query_result(yes([])) --> !,	% prompt_alternatives_on: groundness
	[ ansi(bold, 'true.', []) ],
	extra_line.
query_result(yes(Residuals)) -->
	result([], Residuals),
	extra_line.
query_result(done) -->		% user typed <CR>
	extra_line.
query_result(yes(Bindings, Residuals)) -->
	result(Bindings, Residuals),
	prompt(yes, Bindings, Residuals).
query_result(more(Bindings, Residuals)) -->
	result(Bindings, Residuals),
	prompt(more, Bindings, Residuals).
query_result(help) -->
	[ nl, 'Actions:'-[], nl, nl,
	  '; (n, r, space, TAB): redo    t:          trace & redo'-[], nl,
	  'b:                    break   c (a, RET): exit'-[], nl,
	  'w:                    write   p           print'-[], nl,
	  'h (?):                help'-[],
	  nl, nl
	].
query_result(action) -->
	[ 'Action? '-[], flush ].
query_result(confirm) -->
	[ 'Please answer \'y\' or \'n\'? '-[], flush ].
query_result(eof) -->
	[ nl ].
query_result(toplevel_open_line) -->
	[].

prompt(Answer, [], []) --> !,
	prompt(Answer, empty).
prompt(Answer, _, _) --> !,
	prompt(Answer, non_empty).

prompt(yes, empty) --> !,
	[ ansi(bold, 'true.', []) ],
	extra_line.
prompt(yes, _) --> !,
	[ full_stop ],
	extra_line.
prompt(more, empty) --> !,
	[ ansi(bold, 'true ', []), flush ].
prompt(more, _) --> !,
	[ ' '-[], flush ].

result(Bindings, Residuals) -->
	{ current_prolog_flag(toplevel_print_options, Options0),
	  Options = [partial(true)|Options0]
	},
	bindings(Bindings, [priority(699)|Options]),
	bind_res_sep(Bindings, Residuals),
	residuals(Residuals, [priority(999)|Options]).

bindings([], _) -->
	[].
bindings([binding(Names,Skel,Subst)|T], Options) -->
	{ '$last'(Names, Name) },
	var_names(Names), value(Name, Skel, Subst, Options),
	(   { T \== [] }
	->  [ ','-[], nl ],
	    bindings(T, Options)
	;   []
	).

var_names([Name]) --> !,
	[ '~w = '-[Name] ].
var_names([Name1,Name2|T]) --> !,
	[ '~w = ~w, '-[Name1, Name2] ],
	var_names([Name2|T]).


value(Name, Skel, Subst, Options) -->
	(   { var(Skel), Subst = [Skel=S] }
	->  { Skel = '$VAR'(Name) },
	    [ '~W'-[S, Options] ]
	;   [ '~W'-[Skel, Options] ],
	    substitution(Subst, Options)
	).

substitution([], _) --> !.
substitution([N=V|T], Options) -->
	[ ', ', ansi(fg(green), '% where', []), nl,
	  '    ~w = ~W'-[N,V,Options] ],
	substitutions(T, Options).

substitutions([], _) --> [].
substitutions([N=V|T], Options) -->
	[ ','-[], nl, '    ~w = ~W'-[N,V,Options] ],
	substitutions(T, Options).


residuals([], _) -->
	[].
residuals([G|Gs], Options) -->
	(   { Gs \== [] }
	->  [ '~W,'-[G, Options], nl ],
	    residuals(Gs, Options)
	;   [ '~W'-[G, Options] ]
	).

bind_res_sep(_, []) --> !,
	[].
bind_res_sep([], _) --> !,
	[].
bind_res_sep(_, _) -->
	[','-[], nl].

extra_line -->
	{ current_prolog_flag(toplevel_extra_white_line, true) }, !,
	['~N'-[]].
extra_line -->
	[].

prolog_message(if_tty(Message)) -->
	(   {current_prolog_flag(tty_control, true)}
	->  [ at_same_line | Message ]
	;   []
	).
prolog_message(halt(Reason)) -->
	[ '~w: halt'-[Reason] ].
prolog_message(no_action(Char)) -->
	[ 'Unknown action: ~c (h for help)'-[Char], nl ].

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
	[ '~t~w   ~8|~W~W'-[ Nr,
			     Event, [partial(true)],
			     '.', [partial(true)]
			   ],
	  nl
	],
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
	[ ansi(bold, '~t[~D] ~10|', [Level]) ],
	frame_context(Frame),
	frame_goal(Frame).
prolog_message(frame(Frame, choice, PC)) --> !,
	prolog_message(frame(Frame, backtrace, PC)).
prolog_message(frame(_, cut_call, _)) --> !, [].
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
	frame_depth_limit(Port, Frame),
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

frame_depth_limit(fail, Frame) -->
	{ prolog_frame_attribute(Frame, depth_limit_exceeded, true)
	}, !,
	[ '[depth-limit exceeded] ' ].
frame_depth_limit(_, _) -->
	[].

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
	{ port_name(Port, Colour, Name)
	}, !,
	[ ansi([bold,fg(Colour)], '~w: ', [Name]) ].

port_name(call,	     green,   'Call').
port_name(exit,	     green,   'Exit').
port_name(fail,	     red,     'Fail').
port_name(redo,	     yellow,  'Redo').
port_name(unify,     blue,    'Unify').
port_name(exception, magenta, 'Exception').

clean_goal(M:Goal, Goal) :-
	hidden_module(M), !.
clean_goal(M:Goal, Goal) :-
	predicate_property(M:Goal, built_in), !.
clean_goal(Goal, Goal).


		 /*******************************
		 *	  COMPATIBILITY		*
		 *******************************/

prolog_message(compatibility(renamed(Old, New))) -->
	[ 'The predicate ~p has been renamed to ~p.'-[Old, New], nl,
	  'Please update your sources for compatibility with future versions.'
	].


		 /*******************************
		 *	      THREADS		*
		 *******************************/

prolog_message(abnormal_thread_completion(Goal, exception(Ex))) --> !,
	[ 'Thread running "~p" died on exception: '-[Goal] ],
	translate_message(Ex).
prolog_message(abnormal_thread_completion(Goal, fail)) -->
	[ 'Thread running "~p" died due to failure'-[Goal] ].
prolog_message(threads_not_died(Running)) -->
	[ 'The following threads wouldn\'t die: ~p'-[Running] ].


		 /*******************************
		 *	       PACKS		*
		 *******************************/

prolog_message(pack(attached(Pack, BaseDir))) -->
	[ 'Attached package ~w at ~q'-[Pack, BaseDir] ].
prolog_message(pack(duplicate(Entry, OldDir, Dir))) -->
	[ 'Package ~w already attached at ~q.'-[Entry,OldDir], nl,
	  '\tIgnoring version from ~q'- [Entry, OldDir, Dir]
	].
prolog_message(pack(no_arch(Entry, Arch))) -->
	[ 'Package ~w: no binary for architecture ~w'-[Entry, Arch] ].


		 /*******************************
		 *	PRINTING MESSAGES	*
		 *******************************/

:- multifile
	user:message_hook/3.
:- dynamic
	user:message_hook/3.
:- thread_local
	user:thread_message_hook/3.

%%	print_message(+Kind, +Term)
%
%	Print an error message using a term as generated by the exception
%	system.

print_message(Level, Term) :-
	(   must_print(Level, Term)
	->  (   translate_message(Term, Lines, [])
	    ->  (   nonvar(Term),
		    (	notrace(user:thread_message_hook(Term, Level, Lines))
		    ->  true
		    ;   notrace(user:message_hook(Term, Level, Lines))
		    )
		->  true
		;   print_system_message(Term, Level, Lines)
		)
	    )
	;   true
	).

%%	print_system_message(+Term, +Kind, +Lines)
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
print_system_message(_, _, []) :- !.
print_system_message(Term, Kind, Lines) :-
	flush_output(user_output),
	source_location(File, Line),
	Term \= error(syntax_error(_), _),
	msg_property(Kind, location_prefix(File:Line, LocPrefix, LinePrefix)), !,
	insert_prefix(Lines, LinePrefix, PrefixLines),
	'$append'([ begin(Kind, Ctx),
		    LocPrefix,
		    nl
		  | PrefixLines
		  ],
		  [ end(Ctx)
		  ],
		  AllLines),
	msg_property(Kind, stream(Stream)),
	print_message_lines(Stream, AllLines),
	(   msg_property(Kind, wait(Wait)),
	    Wait > 0
	->  sleep(Wait)
	;   true
	).
print_system_message(_, Kind, Lines) :-
	msg_property(Kind, stream(Stream)),
	print_message_lines(Stream, kind(Kind), Lines).

:- multifile
	user:message_property/2.

msg_property(Kind, Property) :-
	user:message_property(Kind, Property), !.
msg_property(Kind, prefix(Prefix)) :-
	msg_prefix(Kind, Prefix), !.
msg_property(_, prefix('~N')) :- !.
msg_property(_, stream(user_error)) :- !.
msg_property(error,
	     location_prefix(File:Line,
			     '~NERROR: ~w:~d:'-[File,Line], '~N\t')) :- !.
msg_property(warning,
	     location_prefix(File:Line,
			     '~NWarning: ~w:~d:'-[File,Line], '~N\t')) :- !.
msg_property(error,   wait(0.1)) :- !.

msg_prefix(debug(_),      '~N% ').
msg_prefix(warning,	      Prefix) :-
	thread_self(Id),
	(   Id == main
	->  Prefix = '~NWarning: '
	;   Prefix = '~NWarning: [Thread ~w] '-Id
	).
msg_prefix(error,	      Prefix) :-
	thread_self(Id),
	(   Id == main
	->  Prefix = '~NERROR: '
	;   Prefix = '~NERROR: [Thread ~w] '-Id
	).
msg_prefix(informational, '~N% ').
msg_prefix(information,   '~N% ').

%%	print_message_lines(+Stream, +PrefixOrKind, +Lines)
%
%	Quintus compatibility predicate to print message lines using
%	a prefix.

print_message_lines(Stream, kind(Kind), Lines) :- !,
	msg_property(Kind, prefix(Prefix)),
	insert_prefix(Lines, Prefix, PrefixLines),
	'$append'([ begin(Kind, Ctx)
		  | PrefixLines
		  ],
		  [ end(Ctx)
		  ],
		  AllLines),
	print_message_lines(Stream, AllLines).
print_message_lines(Stream, Prefix, Lines) :-
	insert_prefix(Lines, Prefix, PrefixLines),
	print_message_lines(Stream, PrefixLines).

%%	insert_prefix(+Lines, +Prefix, -PrefixedLines)

insert_prefix([at_same_line|Lines0], Prefix, Lines) :- !,
	prefix_nl(Lines0, Prefix, Lines).
insert_prefix(Lines0, Prefix, [prefix(Prefix)|Lines]) :-
	prefix_nl(Lines0, Prefix, Lines).

prefix_nl([], _, [nl]).
prefix_nl([nl], _, [nl]) :- !.
prefix_nl([flush], _, [flush]) :- !.
prefix_nl([nl|T0], Prefix, [nl, prefix(Prefix)|T]) :- !,
	prefix_nl(T0, Prefix, T).
prefix_nl([H|T0], Prefix, [H|T]) :-
	prefix_nl(T0, Prefix, T).

%%	print_message_lines(+Stream, +Lines)

print_message_lines(_, []) :- !.
print_message_lines(S, [H|T]) :-
	line_element(S, H),
	print_message_lines(S, T).

line_element(S, E) :-
	notrace(prolog:message_line_element(S, E)), !.
line_element(S, full_stop) :- !,
	'$put_token'(S, '.').		% insert space if needed.
line_element(S, nl) :- !,
	nl(S).
line_element(S, prefix(Fmt-Args)) :- !,
	format(S, Fmt, Args).
line_element(S, prefix(Fmt)) :- !,
	format(S, Fmt, []).
line_element(S, flush) :- !,
	flush_output(S).
line_element(S, Fmt-Args) :- !,
	format(S, Fmt, Args).
line_element(S, ansi(_, Fmt, Args)) :- !,
	format(S, Fmt, Args).
line_element(_, begin(_Level, _Ctx)) :- !.
line_element(_, end(_Ctx)) :- !.
line_element(S, Fmt) :-
	format(S, Fmt, []).


%%	message_to_string(+Term, -String)
%
%	Translate an error term into a string

message_to_string(Term, Str) :-
        translate_message(Term, Actions, []), !,
        actions_to_format(Actions, Fmt, Args),
        format(string(Str), Fmt, Args).

actions_to_format([], '', []) :- !.
actions_to_format([nl], '', []) :- !.
actions_to_format([Term, nl], Fmt, Args) :- !,
	actions_to_format([Term], Fmt, Args).
actions_to_format([nl|T], Fmt, Args) :- !,
	actions_to_format(T, Fmt0, Args),
	atom_concat('~n', Fmt0, Fmt).
actions_to_format([Skip|T], Fmt, Args) :-
	action_skip(Skip), !,
	actions_to_format(T, Fmt, Args).
actions_to_format([Fmt0-Args0|Tail], Fmt, Args) :- !,
        actions_to_format(Tail, Fmt1, Args1),
        atom_concat(Fmt0, Fmt1, Fmt),
	append_args(Args0, Args1, Args).
actions_to_format([Term|Tail], Fmt, Args) :-
	atomic(Term), !,
        actions_to_format(Tail, Fmt1, Args),
	atom_concat(Term, Fmt1, Fmt).
actions_to_format([Term|Tail], Fmt, Args) :-
        actions_to_format(Tail, Fmt1, Args1),
        atom_concat('~w', Fmt1, Fmt),
	append_args([Term], Args1, Args).

action_skip(at_same_line).
action_skip(ansi(_Attrs, _Fmt, _Args)).
action_skip(begin(_Level, _Ctx)).
action_skip(end(_Ctx)).

append_args(M:Args0, Args1, M:Args) :- !,
	strip_module(Args1, _, A1),
	'$append'(Args0, A1, Args).
append_args(Args0, Args1, Args) :-
	strip_module(Args1, _, A1),
	'$append'(Args0, A1, Args).


		 /*******************************
		 *    MESSAGES TO PRINT ONCE	*
		 *******************************/

:- dynamic
	printed/2.

%%	print_once(Message, Level)
%
%	True for messages that must be printed only once.

print_once(compatibility(_), _).

%%	must_print(+Level, +Message)
%
%	True if the message must be printed.

must_print(Level, Message) :-
	nonvar(Message),
	print_once(Message, Level), !,
	\+ printed(Message, Level),
	assert(printed(Message, Level)).
must_print(_, _).

