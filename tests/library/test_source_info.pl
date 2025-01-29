/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2022, SWI-Prolog Solutions b.v.
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

:- module(test_source_info,
	  [ test_source_info/0
	  ]).
:- use_module(library(debug)).
:- use_module(library(prolog_clause)).
:- use_module(library(prolog_breakpoints)).
:- use_module(library(plunit)).

/** <module> Test source debugger info

This test module validates that we   can obtain valid source information
from a clause and we can properly enumerate the possible breakpoints. If
this test passes for a clause we  are   sure  we can set breakpoints for
such a clause and the reported source   locations while using the source
level debugger are correct.

Each test consists of

  - A fact bp(Head, ExpectedBreakPoints).  ExpectedBreakPoints is a
    list of lists, one list of expected goals we can break on per
    clause.  If there is only one clause, a single list suffices.
  - A predicate for Head.

@tbd	Add many more tests.  The clauses below come from fixing the
	debugger to deal with => rules and unification that is inlined
	into the head.
*/

test_source_info :-
    run_tests([ source_info
	      ]).

:- begin_tests(source_info, [cleanup(cleanup)]).

:- discontiguous bp/2.
term_expansion(bp(Head, Breakpoints),
	       [ (test(Name) :- ide_test(Head)),
		 bp(Head, Breakpoints)
	       ]) :-
    pi_head(PI, Head),
    term_to_atom(PI, Name).

% Unification moved into the head.  We cannot break on the unification
% location.

bp(hu1(_), [writeln(_)]).

hu1(X) :-
    X = aap(N),
    writeln(N).

bp(hu2(_), [writeln(_)]).

hu2(X) :-
    X = aap(_),
    writeln(X).

bp(hu3(_), [_=aap(_), writeln(_)]).

hu3(f(X)) :-
    X = aap(_),
    writeln(X).

% => rules

bp(ssu1(_), [atom(_), =>, writeln(_)]).
bp(nsu1(_), [atom(_), !, writeln(_)]).

ssu1(X), atom(X) => writeln(X).
nsu1(X) :- atom(X), !, writeln(X).

bp(ssu2(_), [atom(N), =>, writeln(N)]).

ssu2(X), X = aap(N), atom(N) =>
    writeln(N).

% currently compiled as `i_ssu_choice, i_cut`.  Could become
% `i_ssu_commit`, which would remove the `=>`.
bp(ssu3(_), [=>, writeln(_)]).

ssu3(X), X = aap(N) =>
    writeln(N).

bp(ssu4(_), [writeln(_)]).

ssu4(aap(N)) =>
    writeln(N).

bp(ssu5(_), [_=aap-noot, writeln(_)]).

ssu5(X) =>
    X = aap-noot,
    writeln(X).

bp(ssu6(X,Y), [atom(X), atom(Y), =>, writeln(X-Y)]).

ssu6(X,Y), atom(X), atom(Y) =>
    writeln(X-Y).


% optimized statements (doesn't work in test environment?)
:- if(false).

bp(opt1(_), [writeln(_)]).

:- set_prolog_flag(optimise, true).

opt1(X) :-
    debug(x, ftm, []),
    writeln(X).

:- set_prolog_flag(optimise, false).
:- endif.

		 /*******************************
		 *            TEST CODE		*
		 *******************************/

cleanup :-
    context_module(M),
    abolish_module_tables(M).

ide_test(Head) :-
    findall(Head-CRef, clause(Head, _Body, CRef), Tests),
    expected_breakpoints(Head, BPs),
    (   same_length(Tests, BPs)
    ->  maplist(ide_test_clause, Tests, BPs)
    ;   print_message(warning, no_expected_breakpoints(Head, Tests, BPs)),
	maplist(ide_test_clause, Tests, _)
    ).

expected_breakpoints(Head, BPS) :-
    (   bp(Head, BPS0)
    ->  (   BPS0 = [[_|_]|_]
	->  BPS = BPS0
	;   BPS = [BPS0]
	)
    ;   BPS = []
    ).

ide_test_clause(Head-CRef, EBPS) :-
    clause_info(CRef, File, TermPos, _NameOffset,
		[ variable_names(_VarNames)
		]),
    findall(bp(PC,SubPos,String,VMI),
	    clause_bp(CRef,File,TermPos,PC,SubPos,String,VMI),
	    BreakPoints),
    (   nonvar(EBPS)
    ->  check_expected_breakpoints(Head, EBPS, BreakPoints)
    ;   true
    ).

check_expected_breakpoints(Head, EBPS, BPS) :-
    exclude(enter_exit_bp, BPS, BPS1),
    maplist(arg(3), BPS1, Goals),
    (   maplist(=@=, EBPS, Goals)
    ->  true
    ;   print_message(error, breakpoint_mismatch(Head, EBPS, Goals)),
	fail
    ).

enter_exit_bp(bp(_,_,_,i_enter)) => true.
enter_exit_bp(bp(_,_,_,i_exit)) => true.
enter_exit_bp(_) => fail.


clause_bp(CRef,File,TermPos,PC,AZ,Term,VMI) :-
    '$break_pc'(CRef, PC, NextPC),
    $ ( '$fetch_vm'(CRef, PC, _, VMI),
	'$clause_term_position'(CRef, NextPC, List),
	debug(break, 'Location = ~w', [List]),
	prolog_breakpoints:range(List, TermPos, SubPos),
	string_at(File, SubPos, String),
	(   String = "."
	->  Term = '.'
	;   term_string(Term, String)
	),
	pos_az(SubPos, AZ),
	debug(ide, 'Break at PC=~w, Term at ~p: ~s; VMI=~p',
	      [PC, AZ, String, VMI]),
	check_bp(VMI, Term)
      ).

string_at(File, SubPos, String) :-
    pos_az(SubPos, A-Z),
    Len is Z-A,
    file_string(File, WholeFileString),
    sub_string(WholeFileString, A, Len, _, String).

:- table file_string/2.
file_string(File, String) :-
    setup_call_cleanup(
	open(File, read, In),
	read_string(In, _, String),
	close(In)).

pos_az(SubPos, A-Z) :-
    arg(1, SubPos, A),
    arg(2, SubPos, Z).

%!  check_bp(+VMI, +String)

check_bp(i_call(QPI), Goal) =>
    strip_module(QPI, _, PI),
    pi_head(PI, Head),
    Goal = Head.
check_bp(i_depart(QPI), Goal) =>
    strip_module(QPI, _, PI),
    pi_head(PI, Head),
    Goal = Head.
check_bp(b_unify_var(_), (_=_)) => true.
check_bp(i_atom(_), atom(_)) => true.
check_bp(i_cut, =>) => true.           % Head,Guard => Body
check_bp(i_cut, !) => true.            % Normal cut
check_bp(i_exit, '.') => true.
check_bp(i_enter, _) => true.          % TBD: Must be the head

:- multifile prolog:message//1.

prolog:message(no_expected_breakpoints(Head, _Tests, [])) -->
    !,
    [ 'No breakpoint declaration for ~p'-[Head] ].
prolog:message(no_expected_breakpoints(Head, Tests, Decl)) -->
    { length(Tests, N1),
      length(Decl, N2)
    },
    [ 'Wrong number of clauses in bp(~p): expected ~d, found ~d'-
      [Head, N1, N2]
    ].
prolog:message(breakpoint_mismatch(Head, EBPS, Goals)) -->
    { numbervars((Head,EBPS,Goals), 0, _, [singletons(true)]) },
    [ '~p: wrong breakpoints'-[Head], nl,
      '  found    ~p'-[Goals], nl,
      '  expected ~p'-[EBPS]
    ].

:- end_tests(source_info).
