/*  $Id$

    Part of SWI-Prolog
    Designed and implemented by Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1997 University of Amsterdam. All rights reserved.
*/

:- module($messages,
	  [ print_message/2		% +Kind, +Term
	  ]).

translate_message(Term) -->
	{var(Term)}, !,
	[ 'Unknown exception term: ~p'-[Term] ].
translate_message(Term) -->
	prolog:message(Term).
translate_message(error(resource_error(stack), Name)) -->
	[ 'Out of ~w stack'-[Name] ].
translate_message(error(ISO, SWI)) -->
	swi_context(SWI),
	term_message(ISO),
	swi_extra(SWI).
translate_message($aborted) -->
	[ 'Execution Aborted' ].
translate_message(Term) -->
	[ 'Unknown exception term: ~p'-[Term] ].

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


dwim_predicates(Module:Name/_Arity, Dwims) :- !,
	findall(Dwim, dwim_predicate(Module:Name, Dwim), Dwims).
dwim_predicates(Name/_Arity, Dwims) :-
	findall(Dwim, dwim_predicate(user:Name, Dwim), Dwims).

dwim_message([]) --> [].
dwim_message([user:Head|T]) --> !,
	{functor(Head, Name, Arity)},
	[ '~t~8|~w/~d'-[Name, Arity], nl ],
	dwim_message(T).
dwim_message([Module:Head|T]) --> !,
	{functor(Head, Name, Arity)},
	[ '~t~8|~w:~w/~d'-[Module, Name, Arity], nl],
	dwim_message(T).
dwim_message([Head|T]) -->
	{functor(Head, Name, Arity)},
	[ '~t~8|~w/~d'-[Name, Arity], nl],
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


swi_context(X) -->
	{ var(X)
	}, !,
	[].
swi_context(context(Name/Arity, _Msg)) -->
	{ nonvar(Name)
	}, !,
	[ '~q/~w: '-[Name, Arity] ].
swi_context(file(Path, Line)) -->
	[ Path, ':', Line, ': ' ].
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
	{ From is CharPos+1,
	  string_length(String, Len),
	  AfterLen is Len - From,
	  substring(String, 1, CharPos, Before),
	  substring(String, From, AfterLen, After)
	},
	[ nl, Before, nl, '** here **', nl, After ].
swi_extra(_) -->
	[].

		 /*******************************
		 *	  NORMAL MESSAGES	*
		 *******************************/

:- multifile
	prolog:message/3.

prolog:message(initialization_exception(Goal, E)) -->
	[ 'Initialization goal ~p raised exception:'-[Goal], nl ],
	translate_message(E).
prolog:message(initialization_exception(E)) -->
	[ 'Prolog initialisation failed:', nl ],
	translate_message(E).
prolog:message(no_predicates_for(Spec)) -->
	[ 'No predicates for `~w'''-[Spec] ].
prolog:message(directive_failed(Goal)) -->
	[ 'Directive failed: ~p'-[Goal] ].


%	print_message(+Kind, +Term)
%
%	Print an error message using a term as generated by the exception
%	system.

print_message(Level, Term) :-
	translate_message(Term, Lines, []), !,
	(   current_predicate(_, user:message_hook(_,_,_)),
	    user:message_hook(Term, Level, Lines)
	->  true
	;   print_system_message(Level, Lines)
	).

print_system_message(Level, Lines) :-
	source_location(File, Line),
	prefix(Level, Prefix, LinePrefix, PostFix, Stream), !,
	format(Stream, Prefix, [File, Line]),
	print_message_lines(Stream, LinePrefix, Lines),
	format(Stream, PostFix, []).
print_system_message(Level, Lines) :-
	prefix(Level, LinePrefix, Stream), !,
	print_message_lines(Stream, LinePrefix, Lines).
	
prefix(error,	      'ERROR: (~w:~d):~n',   '\t', '', user_error).
prefix(warning,	      'Warning: (~w:~d):~n', '\t', '', user_error).

prefix(silent,	      '',        user_error).
prefix(help,	      '',        user_error).
prefix(error,	      'ERROR: ', user_error).
prefix(informational, '% ',      user_error).

%	print_message_lines(+Stream, +Prefix, +Lines)
%
%	Quintus compatibility predicate to print message lines using
%	a prefix.

print_message_lines(_, _, []) :- !.
print_message_lines(S, P, Lines) :-
	format(S, '~N~w', [P]),
	print_message_line(S, Lines, Rest),
	print_message_lines(S, P, Rest).

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

	
