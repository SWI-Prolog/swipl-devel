:- module(test_statistics,
          [ test_statistics/0
          ]).
:- use_module(library(plunit)).
:- use_module(library(statistics)).

test_statistics :-
    run_tests([statistics]).


% Running some tests of call_time/2 and call_time/3.
% We collect the "number of inferences" determined by call_time/N
% and possibly the reified outcome and compare what has been obtained
% against what should have been obtained. If the goal about whcih we
% collect statistics is nondet, all its solutions are collected and
% the respective inference counts and reified outcomes are collected
% as "pairs in a bag".
%
% The inference count may differ slight between SWI-Prolog version,
% between the first and subsequent runs on the the same SWI-Prolog
% version (sometimes heavily, must be JIT compilation). Additionally,
% the build test seems to run each test three times, with slightly
% differing count among tests.
%
% Assertions are accordingly lenient.
%
% The predicate report/2 prints out what has been found.
%
% In SWI-Prolog 8.3.22, inference counts are as follows:
%
% STAT_01: 1 but 0 on second execution
% STAT_04: 1
% STAT_05: 1
% STAT_06: 0
% STAT_07: 0
% STAT_09: 0
% STAT_10: 1
% STAT_11: -1  (!FIXME!)
% STAT_12: 1
% STAT_13: [[104,true],[7,false]] but [[0,true],[6,false]] on second execution
% STAT_14: [[3,true]]
% STAT_15: [[0,true],[3,true],[4,true]]
% STAT_16: [[4,false]]
% STAT_17: [[0,true],[3,true],[4,true]]
% STAT_19: 7709
% STAT_20: 9992
% STAT_21: [[1,true],[4,false]]
% STAT_22: [[7,true],[4,false]]
% STAT_23: [[3,true]]
% STAT_24: [[2,true],[4,false]]
%
% In SWI-Prolog 8.3.23, inference counts are the same except for:
%
% STAT_13: [[24299,true],[7,false]] but [[0,true],[6,false]] afterwards
%
% In SWI-Prolog 8.3.23 with modified library(statistics), there is some "less by 1" going on:
%
% STAT_01: 1 but 0 on second execution
% STAT_04: 1
% STAT_05: 1
% STAT_06: 0
% STAT_07: 0
% STAT_09: 0
% STAT_10: 1
% STAT_11: -1 (!FIXME!)
% STAT_12: 1
% STAT_13: [[24299,true],[6,false]] but [[0,true],[5,false]] on second execution
% STAT_14: [[3,true]]
% STAT_15: [[0,true],[2,true],[3,true]]
% STAT_16: [[4,false]]
% STAT_17: [[0,true],[2,true],[3,true]]
% STAT_19: 7709
% STAT_20: 9992
% STAT_21: [[1,true],[3,false]]
% STAT_22: [[7,true],[3,false]]
% STAT_23: [[3,true]]
% STAT_24: [[2,true],[3,false]]


:- begin_tests(statistics).

report(X,Where) :-
   format(user_error,"~s: ~q~n",[X,Where]).

test("STAT_01: calling true, reified result already instantiated to true") :-
   call_time(true,Usage,true),
   report("STAT_01",Usage.inferences),
   assertion(
      (Usage.inferences == 0 ;
       Usage.inferences == 1 ;
       Usage.inferences == 2)).

% This erroneously succeeds in SWI-Prolog 8.3.23
test("STAT_02: calling true, reified result already (wrongly) instantiated to false",fail) :-
   call_time(true,_Usage,false).

test("STAT_03: calling false, reified result already (wrongly) instantiated to true",fail) :-
   call_time(false,_Usage,true).

test("STAT_04: calling false, reified result already instantiated to false") :-
   call_time(false,Usage,false),
   report("STAT_04",Usage.inferences),
   assertion(Usage.inferences == 1).

test("STAT_05: calling false, reified result is captured") :-
   call_time(false,Usage,Result),
   report("STAT_05",Usage.inferences),
   assertion(
      (Usage.inferences == 0 ;
       Usage.inferences == 1)),
   assertion(Result == false).

test("STAT_06: calling true, reified result is captured") :-
   call_time(true,Usage,Result),
   report("STAT_06",Usage.inferences),
   assertion(
      (Usage.inferences == 0 ;
       Usage.inferences == 1)),
   assertion(Result == true).

test("STAT_07: calling true") :-
   call_time(true,Usage),
   report("STAT_07",Usage.inferences),
   assertion(
      (Usage.inferences == 0 ;
       Usage.inferences == 1)).

test("STAT_08: calling false", fail) :-
   call_time(false,_Usage).

test("STAT_09: calling true, result reified") :-
   call_time(true,Usage,true),
   report("STAT_09",Usage.inferences),
   assertion(
      (Usage.inferences == 0 ;
       Usage.inferences == 1)).

test("STAT_10: calling false, result reified") :-
   call_time(false,Usage,false),
   report("STAT_10",Usage.inferences),
   assertion(Usage.inferences == 1).

test("STAT_11: calling a conjunction of true, result reified") :-
   call_time((true,true,true,true,true,true),Usage,true),
   report("STAT_11",Usage.inferences),
   assertion(
      (Usage.inferences == -1 ;  % BAD
       Usage.inferences ==  0)).

test("STAT_12: calling a conjunction of true with one false, result reified") :-
   call_time((true,true,true,false,true,true),Usage,false),
   report("STAT_12",Usage.inferences),
   assertion(Usage.inferences == 1).

test("STAT_13: calling member(1,[1,2,3])") :-
   bagof([Inferences,Result],Usage^(call_time(member(1,[1,2,3]),Usage,Result),Inferences = Usage.inferences),Bag),
   report("STAT_13",Bag),
   assertion(
     (Bag == [[0, true], [6, false]]  ;
      Bag == [[1, true], [6, false]]  ;
      Bag == [[24241,true],[7,false]] ;
      Bag == [[24299,true],[6,false]] ;
      Bag == [[24299,true],[7,false]] ;
      Bag == [[0, true], [5, false]]  ;
      Bag == [[1, true], [5, false]] )).

test("STAT_14: calling member(3,[1,2,3])") :-
   bagof([Inferences,Result],Usage^(call_time(member(3,[1,2,3]),Usage,Result),Inferences = Usage.inferences),Bag),
   report("STAT_14",Bag),
   assertion(
      (Bag == [[3, true]] ;
       Bag == [[4, true]])).

test("STAT_15: calling member(3,[3,3,3])") :-
   bagof([Inferences,Result],Usage^(call_time(member(3,[3,3,3]),Usage,Result),Inferences = Usage.inferences),Bag),
   report("STAT_15",Bag),
   assertion(
      ( Bag == [[0, true], [2, true], [3, true]] ;
        Bag == [[0, true], [3, true], [4, true]] ;
        Bag == [[1, true], [3, true], [4, true]] )).

test("STAT_16: calling member(4,[1,2,3])") :-
   bagof([Inferences,Result],Usage^(call_time(member(4,[1,2,3]),Usage,Result),Inferences = Usage.inferences),Bag),
   report("STAT_16",Bag),
   assertion(Bag == [[4, false]]).

test("STAT_17: calling member(X,[1,2,3])") :-
   bagof([Inferences,Result],X^Usage^(call_time(member(X,[1,2,3]),Usage,Result),Inferences = Usage.inferences),Bag),
   report("STAT_17",Bag),
   assertion(
      ( Bag == [[0, true], [2, true], [3, true]] ;
        Bag == [[0, true], [3, true], [4, true]] ;
        Bag == [[1, true], [3, true], [4, true]] )).

% "throwing" means a report will be printed by the exception will be rethrown in any case
% the test below will emit the text
% % 12 inferences, 0.000 CPU in 0.000 seconds (100% CPU, 87774 Lips)
% on stderr

test("STAT_18: called goal throws",[error(type_error(x,y))]) :-
   call_time(type_error(x,y),_Usage,_Result).

% test for collatz property

collatz(1)     :- !.
collatz(N) :- N mod 2 =:= 0, !, NN is N // 2, collatz(NN).
collatz(N) :- N mod 2 =:= 1, !, NN is 3 * N + 1, collatz(NN).

test("STAT_19: collatz(931386509544713451)") :-
   call_time(collatz(931386509544713451),Usage),
   report("STAT_19",Usage.inferences),
   assertion(
      (Usage.inferences == 7709 ;  % SWI-Prolog 8.3.23
       Usage.inferences == 7710)). % SWI-Prolog 8.3.23 build tests

% test for collatz property with non-tail recursive step counting

collatz(1,0)     :- !.
collatz(N,Steps) :- N mod 2 =:= 0, !, NN is N // 2, collatz(NN,StepsN), Steps is StepsN+1.
collatz(N,Steps) :- N mod 2 =:= 1, !, NN is 3 * N + 1, collatz(NN,StepsN), Steps is StepsN+1.

test("STAT_20: collatz_sc(931386509544713451)") :-
   call_time(collatz(931386509544713451,Steps),Usage),
   report("STAT_20",Usage.inferences),
   assertion(
      (Usage.inferences == 9993 ;
       Usage.inferences == 9992)),
   assertion(Steps == 2283).

% playing around with a weird predicate; not sure about the resulting inference counts

foo([_-x|_])  :- !.
foo([_|More]) :- foo(More).
foo([x-_|_])  :- !.

test("STAT_21: foo 1") :-
   bagof(
      [Inf,Res],
      U^(call_time(foo([a-a,a-a,a-x]),U,Res),Inf = U.inferences),
      Bag),
   report("STAT_21",Bag),
   assertion(
      (Bag == [[1, true], [3, false]] ;
       Bag == [[2, true], [3, false]] ;
       Bag == [[1, true], [4, false]])).

test("STAT_22: foo 2") :-
   bagof(
      [Inf,Res],
      U^(call_time(foo([a-a,a-a,a-a,a-a,a-a,a-a,a-a,a-a,a-x]),U,Res),Inf = U.inferences),
      Bag),
   report("STAT_22",Bag),
   assertion(
      (Bag == [[7, true], [3, false]] ;
       Bag == [[8, true], [3, false]] ;
       Bag == [[7, true], [4, false]])).

test("STAT_23: foo 3") :-
   bagof(
      [Inf,Res],
      U^(call_time(foo([x-a,a-a,a-a]),U,Res),Inf = U.inferences),
      Bag),
   report("STAT_23",Bag),
   assertion(
      (Bag == [[3, true]] ;
       Bag == [[4, true]])).

test("STAT_24: foo 4") :-
   bagof(
      [Inf,Res],
      U^(call_time(foo([a-a,x-a,a-a]),U,Res),Inf = U.inferences),
      Bag),
   report("STAT_24",Bag),
   assertion(
      (Bag == [[2, true], [3, false]] ;
       Bag == [[2, true], [4, false]] ;
       Bag == [[3, true], [3, false]])).

:- end_tests(statistics).

