/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2026, SWI-Prolog Solutions b.v.
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

:- module(test_blob,
          [ test_blob/0
          ]).
:- use_module(library(plunit)).

/** <module> Test reading blobs written as <type>(...)

Tests the read_term/2,3 option blob(Mode).  See blob/2 and section
"BLOBS" in the manual.
*/

test_blob :-
    run_tests([ blob_dead,
                blob_released,
                blob_resolve,
                blob_syntax
              ]).

read_dead(Text, Term) :-
    read_term_from_atom(Text, Term, [blob(dead)]).

:- begin_tests(blob_dead).

test(read, T == '<stream>(0x1234)') :-
    read_dead('<stream>(0x1234)', B),
    format(atom(T), '~q', [B]).
test(in_compound, A == '<stream>(0x1234)') :-
    read_dead('f(<stream>(0x1234))', f(B)),
    format(atom(A), '~q', [B]).
test(round_trip, Back == Text) :-
    Text = 'foo(a,<stream>(0x1234),[<clause>(0x9)])',
    read_dead(Text, T),
    format(atom(Back), '~q', [T]).

                                        % A dead blob has the type profile
                                        % of the blob it stands for.
test(is_atomic) :-
    read_dead('<stream>(0x1234)', B),
    atomic(B).
test(is_not_atom) :-
    read_dead('<stream>(0x1234)', B),
    \+ atom(B).
test(is_not_compound) :-
    read_dead('<stream>(0x1234)', B),
    \+ compound(B).
test(is_ground) :-
    read_dead('<stream>(0x1234)', B),
    ground(B).

test(type_survives, Type == stream) :-
    read_dead('<stream>(0x1234)', B),
    blob(B, Type).
test(real_type_is_unavailable, Type == unavailable) :-
    read_dead('<stream>(0x1234)', B),
    current_blob(B, Type).
test(unknown_type, Type == no_such_blob_type) :-
    read_dead('<no_such_blob_type>(0x1)', B),
    blob(B, Type).

                                        % Accessors must reject it.  They
                                        % do so by comparing the blob type,
                                        % so no special guard is involved.
test(no_stream_access, error(_)) :-
    read_dead('<stream>(0x1234)', B),
    stream_property(B, alias(_)).
test(no_clause_access, error(_)) :-
    read_dead('<clause>(0x1234)', B),
    clause(_, _, B).

                                        % Two dead blobs with equal text are
                                        % distinct (an address is not an
                                        % identity), so compare/3 must not
                                        % call them equal either.
test(distinct) :-
    read_dead('<stream>(0x1234)', A),
    read_dead('<stream>(0x1234)', B),
    A \== B.
test(compare_agrees_with_eq) :-
    read_dead('<stream>(0x1234)', A),
    read_dead('<stream>(0x1234)', B),
    compare(Order, A, B),
    Order \== (=).
test(sort_keeps_both, Len == 2) :-
    read_dead('<stream>(0x1234)', A),
    read_dead('<stream>(0x1234)', B),
    sort([A,B], Sorted),
    length(Sorted, Len).

                                        % The arguments are read as ordinary
                                        % Prolog, so quotes, comments and
                                        % nesting are handled by the reader.
test(quoted_arg, Back == Text) :-
    Text = '<t>(0x1, \'a b\')',
    read_dead(Text, T),
    format(atom(Back), '~q', [T]).
test(string_arg) :-
    read_dead('<t>("str")', B),
    blob(B, t).
test(nested_arg) :-
    read_dead('<t>(f(g(x)))', B),
    blob(B, t).
test(paren_in_quotes) :-
    read_dead('<t>(\')\')', B),
    blob(B, t).
test(bad_syntax, error(syntax_error(_))) :-
    read_dead('<t>(a b c)', _).

                                        % A blob that never had an object is
                                        % not the same as one whose object
                                        % was released with PL_free_blob().
test(not_released) :-
    read_dead('<stream>(0x1234)', B),
    \+ blob_released(B).
test(released_form_reads_back, Type == stream) :-
    read_dead('<stream>(freed)', B),
    blob(B, Type).

:- end_tests(blob_dead).

:- begin_tests(blob_released).

test(text_atom_is_not_released) :-
    \+ blob_released(foo).
test(live_blob_is_not_released) :-
    setup_call_cleanup(
        open_null_stream(S),
        \+ blob_released(S),
        close(S)).
test(not_a_blob) :-
    \+ blob_released(1).

:- end_tests(blob_released).

:- begin_tests(blob_resolve).

read_resolve(Text, Term) :-
    read_term_from_atom(Text, Term, [blob(resolve)]).

written(Blob, Text) :-
    format(atom(Text), '~q', [Blob]).

test(stream, S == S2) :-
    setup_call_cleanup(
        open_null_stream(S),
        ( written(S, Text),
          read_resolve(Text, S2)
        ),
        close(S)).
test(clause_ref, R == R2) :-              % a PL_BLOB_UNIQUE type
    setup_call_cleanup(
        assertz(unit_test_fact(1), R),
        ( written(R, Text),
          read_resolve(Text, R2)
        ),
        erase(R)).
test(stale_falls_back, Type == stream) :-
    read_resolve('<stream>(0xdeadbeef)', B),
    blob(B, Type),
    \+ is_stream(B).
test(dead_does_not_resolve) :-
    setup_call_cleanup(
        open_null_stream(S),
        ( written(S, Text),
          read_dead(Text, S2),
          S \== S2
        ),
        close(S)).

:- end_tests(blob_resolve).

:- begin_tests(blob_syntax).

test(default_is_error, error(syntax_error(_))) :-
    term_to_atom(_, '<stream>(0x1234)').
test(explicit_error, error(syntax_error(_))) :-
    read_term_from_atom('<stream>(0x1234)', _, [blob(error)]).
test(bad_mode, error(domain_error(blob, bogus))) :-
    read_term_from_atom('<stream>(0x1234)', _, [blob(bogus)]).

                                        % Forms that look similar but are
                                        % not blobs keep their meaning.
test(canonical_lt, T == <(a,b)) :-
    read_dead('<(a,b)', T).
test(diamond, T == <>(a)) :-
    read_dead('<>(a)', T).
test(comparison, T == f(<(a,b), >(c,d))) :-
    read_dead('f(a<b, c>d)', T).
test(infix_clash, error(syntax_error(operator_clash))) :-
    read_dead('a<b>(c)', _).
test(no_space_before_paren, error(syntax_error(_))) :-
    read_dead('<stream> (0x1234)', _).

                                        % Declaring `<' as a prefix operator
                                        % makes <a>(f) the legal term
                                        % >(<(a),f).  Claiming the notation
                                        % must not change that.
test(prefix_op_wins, Dead == Error) :-
    setup_call_cleanup(
        op(200, fy, <),
        ( read_term_from_atom('<a>(f)', Error, [blob(error)]),
          read_term_from_atom('<a>(f)', Dead, [blob(dead)])
        ),
        op(0, fy, <)).
test(prefix_op_not_a_blob) :-
    setup_call_cleanup(
        op(200, fy, <),
        ( read_dead('<a>(f)', T),
          \+ blob(T, _)
        ),
        op(0, fy, <)).

:- end_tests(blob_syntax).
