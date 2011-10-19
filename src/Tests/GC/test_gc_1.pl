:- module(test_gc_1,
	  [ test_gc_1/0
	  ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Test submitted by Keri Harris. This is a   bit hard to describe, but the
rough story is related to discardChoicesAfter()  and C_CUT. Both had the
discardFrame() done in a separate loop  not   to  confuse  GC due to the
reset LocalFrame->clause pointer.

This test shows that the Undo() in discardChoicesAfter() if an exception
is pending can destroy a choice-point and   most likely also other vital
structures. The current implementation fixes  the   issue  by moving the
discardFrame() into the primary loop, but set   BFR such that old frames
cannot be reached.

We keep this test as it is  a nice example of trick setup_call_cleanup/3
usage.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

test_gc_1 :-
	test1,
	test2.

loop(0) :- !.

loop(N) :-
       setup_call_catcher_cleanup(
           true,
           catch(setup_call_cleanup(true,
                                    ( between(1, 2, _), foo ),
                                    garbage_collect),
                 _,
                 true),
           Reason,
           Reason == exit),
       N2 is N - 1,
       loop(N2).

test1 :-
        loop(2), !.

test2 :-
        loop(2) -> true.



:-dynamic my_context/4.

foo :-
        retractall(my_context(foo, bar, baz, qux)),
        assert(my_context(foo, bar, baz, qux)),

        Sql = 'UPDATE sc_450 SET sc_450.record_status=? FROM se_counterparty sc_450 WHERE sc_450.cp_code = ?',

        setup_call_cleanup(true,
                           (
                             get_my_context(_, _, _, _),
                             between(1,2,_),
                             setup_call_cleanup(assert(Sql),
                                                true,
                                                retract(Sql))
                           ),
                           true),

        throw(error(application_error)).

get_my_context(TransactionId,
               TrxId,
               AccessToken,
               TransactionTimestamp) :-
        ( my_context(TransactionId_, TrxId_, AccessToken_, TransactionTimestamp_) ->
            TransactionId = TransactionId_,
            TrxId = TrxId_,
            AccessToken = AccessToken_,
            TransactionTimestamp = TransactionTimestamp_

        ; otherwise ->
            throw(no_context)
        ).
