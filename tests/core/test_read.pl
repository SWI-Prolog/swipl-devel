/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2011-2024, University of Amsterdam
			      SWI-Prolog Solutions b.v.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(test_read, [test_read/0]).
:- use_module(library(plunit)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(prolog_source), [valid_term_position/2]).

/** <module> Read tests

Test term reading, notably option processing

@author	Jan Wielemaker
*/

test_read :-
    run_tests([ read_term,
                read_op,
		read_numbers
	      ]).

:- begin_tests(read_term).

test(singletons, Names == ['_a','_A','_0','A']) :-
    term_string(_, "a(_,_a,_A,_0,A)",
                [ singletons(Singletons)
                ]),
    maplist(arg(1), Singletons, Names).
test(warn_singletons, Messages =@= [singletons(a(_,_,_,_,_), ['_a','A'])]) :-
    catch_messages(_,
                   term_string(_, "a(_,_a,_A,_0,A)",
                               [ singletons(warning)
                               ]),
                   Messages).
test(position,
     [Query,TermPos,Comments] ==
     [true,7-11,['$stream_position'(0,0,0,0)-"%hello"]]) :-
	QueryString = "%hello\ntrue",
	term_string(Query, QueryString,
                [ subterm_positions(TermPos),
                  comments(Comments)
                ]).

test(valid_position_var) :-
    term_position_check("Var", _Var, 0-3).

test(valid_position_atom) :-
    term_position_check("'Atom'", 'Atom', 0-6).

test(valid_position_number1) :-
    term_position_check(" 123 ", 123, 1-4).
test(valid_position_number2) :-
    term_position_check(" -123.45     ", -123.45, 1-8).

test(valid_position_string_string,
     [setup((current_prolog_flag(double_quotes, QuotesFlag),
             set_prolog_flag(double_quotes, string))),
      cleanup(set_prolog_flag(double_quotes, QuotesFlag))]) :-
    term_position_check('"abc"', "abc", string_position(0,5)).
test(valid_position_string_codes,
     [setup((current_prolog_flag(double_quotes, QuotesFlag),
             set_prolog_flag(double_quotes, codes))),
      cleanup(set_prolog_flag(double_quotes, QuotesFlag))]) :-
    term_position_check('"abc"', [97,98,99], string_position(0,5)).
test(valid_position_string_chars,
     [setup((current_prolog_flag(double_quotes, QuotesFlag),
             set_prolog_flag(double_quotes, chars))),
      cleanup(set_prolog_flag(double_quotes, QuotesFlag))]) :-
    term_position_check('"abc"', [a,b,c], string_position(0,5)).
test(valid_position_string_atom,
     [setup((current_prolog_flag(double_quotes, QuotesFlag),
             set_prolog_flag(double_quotes, atom))),
      cleanup(set_prolog_flag(double_quotes, QuotesFlag))]) :-
    term_position_check('"abc"', 'abc', string_position(0,5)).

test(valid_position_nil) :-
    term_position_check(" [   ] ", [], 1-6).

test(valid_position_braces) :-
    term_position_check(
	" { foo, bar } ", {foo,bar},
	brace_term_position(1,13,term_position(3,11,6,7,[3-6,8-11]))).

test(valid_position_braces1) :-
    term_position_check(
	" [ foo, [bar] ] ", [foo,[bar]],
	list_position(1,15,[3-6,list_position(8,13,[9-12],none)],none)).
test(valid_position_braces2) :-
    term_position_check(" [ foo,123 | bar] ", [foo,123|bar],
                        list_position(1,17,[3-6,7-10],13-16)).
test(valid_position_braces3) :-
    term_position_check("[X|Xs]", [_X|_Xs],
                        list_position(0,6,[1-2],3-5)).

test(valid_position_term1) :-
    term_position_check("a()", a(), term_position(0, 3, 0, 1, [])).
test(valid_position_term2) :-
    term_position_check("a(X,y)", a(_X,y), term_position(0, 6, 0, 1, [2-3, 4-5])).
test(valid_position_term2) :-
    term_position_check("call(X):[adder]", call(_X):[adder],
                        term_position(0, 15, 7, 8,
                                      [term_position(0, 7, 0, 4, [5-6]),
                                       list_position(8, 15, [9-14], none)])).
test(valid_position_dict1) :-
    term_position_check("_{}", _{},
                        dict_position(0,3,0,1,[])).
test(valid_position_dict2) :-
    term_position_check(
	"_{a:[1],b:{}}", _{a:[1],b:{}},
	dict_position(0,13,0,1,
		      [ key_value_position(2,7,3,4,a,2-3,
					   list_position(4,7,[5-6],none)),
			key_value_position(8,12,9,10,b,8-9,10-12)
		      ])).
% key-value order of term is defined (ordered)
test(valid_position_dict3) :-
    term_position_check(
	"tag{bb:1,aa:2}", tag{aa:2,bb:1},
	dict_position(0,14,0,3,
		      [ key_value_position(4,8,6,7,bb,4-6,7-8),
			key_value_position(9,13,11,12,aa,9-11,12-13)
		      ])).

:- end_tests(read_term).

:- begin_tests(read_op).

:- op(600, fy, !).
:- op(600, fy, []).

test(modify, Term = !([])) :-
    context_module(M),
    term_string(Term, '![]', [module(M)]).
test(minus_block, Term = -([x])) :-
    context_module(M),
    term_string(Term, '-[x]', [module(M)]).
test(modify_block, Term = !([x])) :-
    context_module(M),
    term_string(Term, '![x]', [module(M)]).
test(pos_block) :-
    context_module(M),
    term_position_check(
	"![x]", !([x]),
	term_position(0,4,0,1,[list_position(1,4,[2-3],none)]),
        [module(M)]).

:- end_tests(read_op).

:- begin_tests(read_numbers).

test(float_overflow, error(syntax_error(float_overflow))) :-
    term_string(_, '1.797693134862316e+308', []).
test(float_overflow, F =:= inf) :-
    current_prolog_flag(float_overflow, Old),
    setup_call_cleanup(
        set_prolog_flag(float_overflow, infinity),
        term_string(F, '1.797693134862316e+308', []),
        set_prolog_flag(float_overflow, Old)).

:- end_tests(read_numbers).

term_position_check(TermString, ExpectedTerm, ExpectedTermPos) :-
    term_position_check(TermString, ExpectedTerm, ExpectedTermPos, []).

term_position_check(TermString, ExpectedTerm, ExpectedTermPos, Options) :-
    term_string(Term, TermString,
                [ subterm_positions(TermPos)
                | Options
                ]),
    assertion(ground(TermPos)),
    assertion(ground(ExpectedTermPos)),
    assertion(Term =@= ExpectedTerm),
    assertion(TermPos == ExpectedTermPos),
    assertion(valid_term_position(Term, TermPos)).

%%	catch_messages(+Kind, :Goal, -Messages) is semidet.

:- thread_local
	message/1.
:- meta_predicate
	catch_messages(?, 0, -).

catch_messages(Kind, Goal, Messages) :-
	setup_call_cleanup(
	    asserta((user:thread_message_hook(Term, Kind, _) :-
		        \+ \+ (prolog_load_context(variable_names, VarNames),
			       bind_variable_names(VarNames),
			       assertz(message(Term)))), Ref),
	    once(Goal),
	    erase(Ref)),
	findall(Msg, retract(message(Msg)), Messages).

bind_variable_names([]).
bind_variable_names([Name='$VAR'(Int)|T]) :- !,
	var_name(Int, Name),
	bind_variable_names(T).
bind_variable_names([_|T]) :-
	bind_variable_names(T).

var_name(N, Name) :-
	atom_codes(Name, [C]),
	between(0'A, 0'Z, C),
	N is C - 0'A.
