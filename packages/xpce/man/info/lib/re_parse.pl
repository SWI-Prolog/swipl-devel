/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

:- module(re_parse,
	  [ re_parse_loop/5
	  ]).


:- meta_predicate
	re_parse_loop(+, +, +, :, +),
	parse_line(+, +, +, :).

%	re_parse_loop(+File, +RegExVar, +ActionVar, +Generator, +EndRegEx)

re_parse_loop(File, ReVar, AVar, PatternGenerator, End) :-
	regex(End, EndRe),
	repeat,
	    (	get(File, read_line, L)
	    ->	(   EndRe \== @nil,
		    send(EndRe, match, L)
		->  !
		;   (	parse_line(L, ReVar, AVar, PatternGenerator)
		    ->	fail		% force next line
		    ;   send(File, report, warning,
			     'Failed to handle line ``%s''''', L),
		        fail
		    )
		)
	    ;	!,
		send(File, close)
	    ).


regex(@nil, @nil) :- !.
regex(String, Regex) :-
	get(string(String), value, Expanded),
	new(Regex, regex(Expanded)).


parse_line(Line, ReVar, AVar, PatternGenerator) :-
	PatternGenerator,
	regex(ReVar, Re),
	send(Re, match, Line),
	pattern_action(Re, Line, AVar, Goal),
	Goal, !.


pattern_action(Re, L, Template, Goal) :-
	functor(Template, Name, Arity),
	functor(Goal,     Name, Arity),
	End is Arity + 1,
	pattern_action(1, End, Re, L, Template, Goal).


pattern_action(N, N, _, _, _, _) :- !.
pattern_action(N, Arity, Re, L, Template, Goal) :-
	arg(N, Template, Arg),
	(   integer(Arg)
	->  get(Re, register_value, L, Arg, Value),
	    arg(N, Goal, Value)
	;   nonvar(Arg),
	    Arg = Index:Type
	->  get(Re, register_value, L, Index, RawValue),
	    get(@pce, convert, RawValue, Type, Value),
	    arg(N, Goal, Value)
	;   arg(N, Goal, Arg)
	),
	NN is N + 1,
	pattern_action(NN, Arity, Re, L, Template, Goal).



