/*  Author: Joachim Schimpf
    Year: 2013

    This code is given to the public domain "as is". Use at your own risk.
*/

:- module(test_util_iso,
	  [ test/1,				% +File
	    test/2				% +File, +ReportFile
	  ]).

/** <module> ECLiPSe test automation

Use this library as follows: Write a  file with test patterns, using the
primitives should_fail/1, should_give/2, and should_throw/2, e.g.

    ==
    3.0 > 3       should_fail.
    X is 3.0+4    should_give  X==7.0.
    throw(ball)   should_throw ball.
    arg(0,atom,A) should_throw error(type_error(compound,atom),_).
    ==

The file name should have a .tst   extension, e.g. mytests.tst. Then run
all  the  test  in  that  file  by  loading  this  library  and  calling
test('mytests.tst'). This will print a  message   for  every test, and a
summary at the end. To write the  results   to  a  file, use test/2 (see
below).

To temporarily disable a test in a test file, use the fixme prefix, e.g.

    ==
    fixme X is 0/0 should_throw error(evaluation_error(undefined),_).
    ==

The test procedure will skip those and print a count a the end.

The primitives should_fail/1, should_give/2,  should_throw/2 and fixme/1
are also predicates that can be called directly.

    $ Goal should_fail :
    Run the goal Goal and print a message if it doesn't fail.

    $ Goal should_give +CheckGoal :
    Run the goal Goal and print a message if Goal does not succeed, or
    if the result doesn't satisfy CheckGoal.

    CheckGoal can be an arbitrary user-defined goal.  In this case, the
    first solution of Goal is committed to, and CheckGoal executed with
    the variable instantiations of this solution.

    To allow verification of goals with multiple solutions, one special
    form of CheckGoal is recognised:

      ==
      multiple_solutions(SolCountVar, FinalCheck, SolutionCheck)
      ==

    where SolCountVar should be a fresh variable.  With such a
    CheckGoal, ALL solutions to Goal will be generated.  For each
    solution, SolutionCheck will be executed with the variable
    instantiations of this solution, and with SolCountVar
    instantiated to the number of this solution (starting from 1).
    After all solutions have been found, FinalCheck will be executed,
    with SolCountVar instantiated to the total number of solutions.

      ==
      member(X,[a,b,c])  should_give
          multiple_solutions(K, K==3,
              ( K==1 -> X==a
              ; K==2 -> X==b
              ; K==3 -> X==c
          )).
      ==

    $ Goal should_throw +Exception :
    Run the goal Goal and print a message if it doesn't throw Exception.
    The exception term thrown must be an instance (see subsumes_term/2)
    of Exception>

    $ fixme +SkippedTest :
    Skip a test that is known to fail.
    fixme/1 is a low-precedence prefix operator, and can thus be
    textually prefixed to any other test.  Its effect is that the test
    is skipped (not executed).  When multiple tests are done, the number
    of skipped tests gets reported at the end.  Skipped tests count as
    neither succeeded or failed.",
*/

report(brief).

:- op(1200,  fy, fixme).
:- op(1110, xf,  should_fail).
:- op(1110, xfx, should_give).
:- op(1110, xfx, should_throw).
:- op(1110, xfx, should_raise).

%%	test(+TestFile) is det.
%
%	Runs all the test patterns in TestFile.

test(FileIn) :-
        setup_call_cleanup(
	    open(FileIn, read, In),
	    test_stream(In, user_error),
	    close(In)).


%%	test(+TestFile, +ResultFile) is det.
%
%	Runs all the test patterns  in   TestFile,  and  logs results in
%	ResultFile.

test(FileIn, FileOut) :-
        setup_call_cleanup(
	    open(FileIn, read, In),
	    setup_call_cleanup(
		open(FileOut, write, Out),
		test_stream(In, Out),
		close(Out)),
	    close(In)).


    test_stream(In, Out) :-
        stream_property(In, file_name(File)),
        format(Out, '~N% Running ECLiPSe tests from file ~w~n', [File]),
        counter_set(test_count, 0),
        counter_set(non_test_count, 0),
        counter_set(succeeded_test_count, 0),
        counter_set(failed_test_count, 0),
        counter_set(skipped_test_count, 0),
        repeat,
%	    line_count(In, Line),
            catch(catch(read_term(In, Test,
				  [ module(test_util_iso)
				  ]), SyntaxError,
                        unexpected(Out, 0, valid_syntax, throw(SyntaxError))),
                        continue, fail),
	    source_location(_File, Line),
            ( Test \== end_of_file ->
                counter_inc(test_count),
                counter_get(test_count, N),
%               writeq(Out, Test), nl,
                catch(interpret_test(Test, N/Line, Out), continue, true),
                fail
            ;
                counter_get(test_count, N),
                counter_get(succeeded_test_count, TN),
                counter_get(failed_test_count, FN),
                counter_get(skipped_test_count, SN),
                counter_get(non_test_count, NN),
                format(Out, '~N% Finished tests from file ~w~n', [File]),
                format(Out, '% ~D tests found.~n', [N]),
                ( NN==0 -> true ; format(Out, '% ~D ignored as malformed.~n', [NN]) ),
                format(Out, '% ~D tests succeeded.~n', [TN]),
                ( FN==0 -> true ; format(Out, '% ~D tests failed.~n', [FN]) ),
                ( SN==0 -> true ; format(Out, '% ~D tests skipped.~n', [SN]) )
            ),
        !,
	FN =:= 0.


interpret_test((fixme Test), Name, Stream) :- !,
        fixme(Test, Name, Stream).
interpret_test((Goal should_fail), Name, Stream) :-  !,
        should_fail(Goal, Name, Stream).
interpret_test((Goal should_give Check), Name, Stream) :-  !,
        should_give(Goal, Check, Name, Stream).
interpret_test((Goal should_throw Ball), Name, Stream) :-  !,
        should_throw(Goal, Ball, Name, Stream).
interpret_test((Goal should_raise Exception), Name, Stream) :-  !,
        ( Exception==4 -> Ball = error(instantiation_error,_)
        ; Exception==5 -> Ball = error(type_error(_,_),_)
        ; Exception==24 -> Ball = error(type_error(_,_),_)
        ; Exception==6 -> Ball = error(domain_error(_,_),_)
        ; Ball = error(_,_)
        ),
        should_throw(Goal, Ball, Name, Stream).
interpret_test(_Goal, Name, Stream) :-
        write(Stream, 'Non-test goal '), write(Stream, Name),
        write(Stream, ': ignored'), nl(Stream),
        counter_inc(non_test_count).



fixme(Test) :-
        current_output(Stream),
        catch(fixme(Test, Test, Stream), continue, true).

    fixme(_Test, Name, Stream) :-
        write(Stream, 'Test '), write(Stream, Name),
        write(Stream, ': skipped'), nl(Stream),
        counter_inc(skipped_test_count),
        throw(continue).



Goal should_fail :-
        current_output(Stream),
        catch(should_fail(Goal, Goal, Stream), continue, true).

    should_fail(Goal, Name, Stream) :-
        ( catch(Goal, Ball, unexpected(Stream,Name,failure,throw(Ball))) ->
            unexpected(Stream, Name, failure, success)
        ;
            expected_outcome(Stream, Name)
        ).



Goal should_give Check :-
        current_output(Stream),
        catch(should_give(Goal, Check, Goal, Stream), continue, true).


    should_give(_Goal, Check, Name, Stream) :- \+ callable(Check), !,
        unexpected(Stream, Name, success, illegal_check(Check)).

    should_give(Goal, multiple_solutions(K,TotalCheck,SolutionCheck), Name, Stream) :- !,
        counter_set(solutions, 0),
        (
            catch(Goal, Ball, unexpected(Stream,Name,'success or failure',throw(Ball))),
            counter_inc(solutions),
            ( counter_get(solutions, K), catch(SolutionCheck, _, fail) ->
                fail    % next solution
            ;
                unexpected(Stream, Name, success, failed_check(SolutionCheck))
            )
        ;
            ( counter_get(solutions, K), catch(TotalCheck, _, fail) ->
                expected_outcome(Stream, Name)
            ;
                unexpected(Stream,Name,success,failed_check(TotalCheck))
            )
        ).

    should_give(Goal, Check, Name, Stream) :-
        ( catch(Goal, Ball, unexpected(Stream,Name,success,throw(Ball))) ->
            ( catch(Check, _, fail) ->
                expected_outcome(Stream, Name)
            ;
                unexpected(Stream, Name, success, failed_check(Check))
            )
        ;
            unexpected(Stream, Name, success, failure)
        ).



Goal should_throw Ball :-
        current_output(Stream),
        catch(should_throw(Goal, Ball, Goal, Stream), continue, true).

    should_throw(Goal, Expected, Name, Stream) :-
        ( catch(Goal, Ball,
                ( subsumes_term(Expected,Ball) ->
                    expected_outcome(Stream, Name)
                ;
                    unexpected(Stream, Name, throw(Expected), throw(Ball))
                )
            )
        ->
            unexpected(Stream, Name, throw(Expected), success)
        ;
            unexpected(Stream, Name, throw(Expected), failure)
        ).



expected_outcome(Stream, Name) :-
	(   report(brief)
	->  put_char(Stream, '.'),
	    flush_output(Stream)
	;   format(Stream, '~NTest ~w: OK~n', [Name])
	),
	counter_inc(succeeded_test_count),
        throw(continue).

unexpected(Stream, Name, Expected, Outcome) :-
	format(Stream, '~NTest ~w: ~n~texpected ~12|~q,~n~tgot ~12|~q~n',
	       [Name, Expected, Outcome]),
        counter_inc(failed_test_count),
        throw(continue).



%
% ISO implementation of non-backtrackable counters
%

:- dynamic(counter/2).

counter_set(Name, Value) :-
        retractall(counter(Name,_)),
        asserta(counter(Name,Value)).

counter_inc(Name) :-
        ( retract(counter(Name,N0)) -> N1 is N0+1 ; N1 = 1 ),
        asserta(counter(Name,N1)).

counter_get(Name, Value) :-
        counter(Name, Value).

