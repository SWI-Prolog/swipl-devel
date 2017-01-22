/*  Part of SWI-Prolog

    Author:        Benoit Desouter <Benoit.Desouter@UGent.be>
		   Jan Wielemaker (SWI-Prolog port)
    Copyright (c)  2016, Benoit Desouter
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

:- module(tabling_testlib,
	  [ test_expected_variants_present/0,
	    test_tables_cleaned/0,
	    test_answers_expected_tables/0,
	    test_answers_for_expected_variant/1,
	    compare_real_expected_answers/3,		% :Name, +Arity, :E
	    tabling_testlib/0
	  ]).
:- use_module(library(tabling)).
:- use_module(library(terms)).
:- use_module(library(dialect/hprolog)).
:- use_module(library(lists)).

:- module_transparent
	test_expected_variants_present/0,
	test_tables_cleaned/0,
	test_answers_expected_tables/0.
:- meta_predicate
	test_answers_for_expected_variant(:),
	compare_real_expected_answers(:,+,1).

tabling_testlib.				% called in test framework

% requires a predicate expected_variants(-List) in the example
test_expected_variants_present :-
	context_module(M),
	test_expected_variants_present(M).

test_expected_variants_present(M) :-
	M:expected_variants(Xs0),
	maplist(mqualify(M), Xs0, Xs),
	test_expected_variants_present_(Xs, True),
	True \== false,
	% now all expected variants are present.
	% next, we check whether there aren't any more present.
	length(Xs,NumExpected),
	num_tables(NumActual),
	assert_equal(NumExpected,NumActual,'test_expected_variants_present').

num_tables(Count) :-
	'$tbl_variant_table'(Trie),
	aggregate_all(count, trie_gen(Trie,_,_), Count).

mqualify(M,T,M:T).

% uses "private" predicate from table_datastructure.gpp or table_link_manager.gpp depending on the version.
test_expected_variants_present_([], _).
test_expected_variants_present_([X|Xs], True) :-
	(   current_table(X,_)
	->  true
	;   print_message(error, format('Missing table for variant ~p',[X])),
	    True = false
	),
	test_expected_variants_present_(Xs, True).

% test whether all expected tables have received proper cleanup, that is: having the form complete_table/3
% uses "private" predicate from table_datastructure.gpp or table_link_manager.gpp depending on the version.
test_tables_cleaned :-
	context_module(M),
	M:expected_variants(Xs),
	test_tables_cleaned_(Xs),
	% if we get here, write a note to show that we did the test.
	format('test_tables_cleaned succeeded~n',[]).

test_tables_cleaned_([]).
test_tables_cleaned_([X|Xs]) :-
	(   current_table(X,Trie),
	    '$tbl_table_status'(Trie, complete)
	->  true
	;   format('test_tables_cleaned: table for variant ~p did not \c
	            receive proper cleanup~n',[X]),
	    throw('test_tables_cleaned: a table did not receive proper cleanup')
	),
	test_tables_cleaned_(Xs).

test_answers_expected_tables :-
	context_module(M),
	test_answers_expected_tables(M).

test_answers_expected_tables(M) :-
	M:expected_variants(Xs0),
	maplist(mqualify(M), Xs0, Xs),
	test_answers_expected_tables_(Xs, True),
	True \== false.

test_answers_expected_tables_([], _).
test_answers_expected_tables_([Variant|Rest], True) :-
	(   test_answers_for_expected_variant(Variant)
	->  true
	;   print_message(error, format('Wrong answers for expected variant ~p',
					[Variant])),
	    True = false
	),
	test_answers_expected_tables_(Rest, True).

% ATTENTION: works only for ground answers in the tables (which we currently enforce when adding answers as well). To be on the safe side, an exception will be thrown if one of the expected answers is nonground.
% Requires a predicate expected_answers_for_variant/2 in the example.
% Uses "private" predicate from table_datastructure.gpp or table_link_manager.gpp depending on the version.
test_answers_for_expected_variant(M:Variant) :-
	% We really want a variant check here, not unification...
	M:expected_answers_for_variant(SomeVariant,ExpectedAnswers0),
	maplist(mqualify(M), ExpectedAnswers0, ExpectedAnswers),
	variant(Variant,SomeVariant),
	current_table(M:Variant, Trie),
	test_answers_for_variant_(ExpectedAnswers, Trie, True),
	True \== false,
	% Now check that there are not more answers than expected
	length(ExpectedAnswers,NumExpected),
	get_num_answers(Trie,NumActual),
	assert_equal(NumExpected,NumActual,'test_answers_for_expected_variant').

% Slow, but only used for testing. We don't need to keep the number of answers at runtime,
% so we don't keep track of it (for performance).
get_num_answers(Trie,NumActual) :-
	aggregate_all(count, trie_gen(Trie,_,_), NumActual).

test_answers_for_variant_([],_Trie, _).
test_answers_for_variant_([ExpectedAnswer|Rest], Trie, True) :-
	(   ground(ExpectedAnswer)
	->  true
	;   print_message(error, format('Got nonground expected answer ~p, \c
					 which it cannot handle correctly',
					[ ExpectedAnswer ])),
	    True = false
	),
	% get_answer => uses unification, so this won't work properly
	% for nonground answers.
	(   trie_lookup(Trie,ExpectedAnswer,_)
	->  true
	;   print_message(error, format('Missing expected answer ~p',
					[ExpectedAnswer])),
	    True = false
	),
	test_answers_for_variant_(Rest,Trie,True).

assert_equal(NumExpected,NumActual,ContextualInfo) :-
	(   NumExpected == NumActual
	->  true
	;   print_message(error,
			  format('assert_equal failed in context of ~w: \c
			  expected ~w but was ~w~n',
				 [ContextualInfo,NumExpected,NumActual])),
	    fail
	).


% G = Name of goal to obtain real answers. Should take A free variables.
% A = Arity of goal to obtain real answers.
% E = Name of goal to obtain list of expected answers. Should take one argument.
compare_real_expected_answers(M:G,A,E) :-
  call(E,E2),
  length(FreeVarsList,A),
  G2 =.. [G|FreeVarsList],
  list_to_tuple(FreeVarsList,FreeVarsTuple),
  findall(FreeVarsTuple,M:G2,R),
  expect_same_size(E2,R,Ok1),
  expect_lists_equal_sets(E2,R,Ok2),
  Ok1 == true, Ok2 == true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% E = list that has the expected number of elements
% A = list that has the actual number of elements
expect_same_size(E,A,Result) :-
  length(E,Es),
  length(A,As),
  (   Es == As
  ->  Result = true
  ;   print_message(error,
		    format('expected list to have ~d elements but it had ~d~n',
			   [Es,As])),
      Result = false
  ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Both lists are turned into sets first!
% E = list having expected elements
% A = list having actual elements
expect_lists_equal_sets(E,A,True) :-
  list_to_set(E,Es),
  list_to_set(A,As),
  (   (   list_difference_eq(As,Es,[]),
	  list_difference_eq(Es,As,[])
      )
  ->  True = true
  ;   print_message(error,
		    format('lists do not represent equal sets. \c
		            Expected list was ~p, actual list was ~p',[E,A]))
  ).

% [X,Y,Z] -> X-Y-Z
% Empty list. No sensible behaviour. Throw exception.
list_to_tuple([],_) :- !,
  type_error(non_empty_list, []).
% List with at least one element.
list_to_tuple([First|Rest],Tuple) :-
  foldl(to_tuple,Rest,First,Tuple).

% E = 'element'
% Ai = 'accumulator in'
to_tuple(E,Ai,Ai-E).
