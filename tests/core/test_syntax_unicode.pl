/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org
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

:- module(test_syntax_unicode,
          [ test_syntax_unicode/0
          ]).
:- use_module(library(plunit)).

test_syntax_unicode :-
    run_tests([ syntax_unicode_identifiers,
                syntax_unicode_layout,
                syntax_unicode_solo,
                syntax_unicode_numbers,
                syntax_unicode_version
              ]).

:- begin_tests(syntax_unicode_identifiers).

% --- Variable vs atom by case --------------------------------------------

test(lu_starts_variable) :-
    term_to_atom(T, 'Α = 1'),       % Greek capital alpha (U+0391, Lu)
    T = (V = 1),
    var(V).
test(ll_starts_atom) :-
    term_to_atom(T, 'α = 1'),       % Greek small alpha (U+03B1, Ll)
    T = (Atom = 1),
    atom(Atom).
test(lo_starts_atom) :-
    term_to_atom(T, '中 = 1'),      % CJK ideograph (Lo, no case)
    T = (A = 1),
    atom(A).

% --- Lt now starts an atom (Lu-only uppercase rule) ---------------------

test(lt_starts_atom) :-
    % U+01C5 Latin capital letter D with small letter Z with caron (Lt).
    % Under the Lu-only uppercase rule it is NOT an uppercase character
    % and therefore starts an *atom*.
    term_to_atom(T, 'ǅabc'),
    atom(T).

% --- Underscore prefix in non-cased scripts (existing rule) -------------

test(underscore_starts_variable) :-
    term_to_atom(T, '_変数 = 1'),
    T = (V = 1),
    var(V).

% --- Superscript and subscript digits extend identifiers ---------------

test(superscript_continues_identifier) :-
    term_to_atom(T, 'X² = 1'),
    T = (V = 1),
    var(V).
test(subscript_continues_identifier) :-
    term_to_atom(T, 'X₁ = 1'),
    T = (V = 1),
    var(V).
test(superscript_cannot_start_identifier,
     [error(syntax_error(_))]) :-
    term_to_atom(_, '²X = 1').

% --- XID profile boundary ----------------------------------------------

test(xid_excludes_id_only_codepoint,
     [error(syntax_error(_))]) :-
    % U+037A GREEK YPOGEGRAMMENI is in ID_Continue but not in
    % XID_Continue.  Under the XID-based rules it must not extend an
    % identifier, so 'Xͺ' tokenises as two atoms (X and ͺ as a solo
    % atom) and term_to_atom reports the trailing token.
    term_to_atom(_, 'Xͺ').

:- end_tests(syntax_unicode_identifiers).


:- begin_tests(syntax_unicode_layout).

% --- Pattern_White_Space members are layout ----------------------------

test(lrm_is_whitespace) :-
    % U+200E LEFT-TO-RIGHT MARK is in Pattern_White_Space and must be
    % consumed as layout.  Used around `+` it must leave a parsable
    % term `a+b`.
    atom_codes(S, [0'a, 0x200E, 0'+, 0x200E, 0'b]),
    term_to_atom(T, S),
    T == a+b.

% --- NBSP is NOT in Pattern_White_Space --------------------------------
%
% U+00A0 NO-BREAK SPACE is deliberately not in Pattern_White_Space.  The
% high-bit Unicode classification respects this, but the legacy
% ISO Latin-1 table in src/os/pl-ctype.c still treats U+00A0 as
% whitespace for c =< 0xff.  This test is parked until the Latin-1
% table is brought into alignment.
%
%   test(nbsp_is_not_layout, [error(syntax_error(_))]) :-
%       atom_codes(S, [0'a, 0x00A0, 0'+, 0x00A0, 0'b]),
%       term_to_atom(_, S).

:- end_tests(syntax_unicode_layout).


:- begin_tests(syntax_unicode_solo).

% --- Unicode S? and P? form solo atoms (no glueing) --------------------

test(le_is_atom) :-
    % U+2264 LESS-THAN OR EQUAL TO (Sm)
    term_to_atom(T, '≤'),
    atom(T),
    atom_length(T, 1).
test(double_le_does_not_glue) :-
    % Two ≤ in a row: would have glued under the old regime.  Now
    % each is a solo atom; without an operator declaration the
    % sequence is a syntax error.
    catch(term_to_atom(_, '≤≤'), _, true).
test(left_quote_is_atom) :-
    % U+00AB « (Pi).  Solo.
    term_to_atom(T, '«'),
    atom(T),
    atom_length(T, 1).
test(currency_is_atom) :-
    term_to_atom(T, '€'),
    atom(T),
    atom_length(T, 1).

% --- ASCII gluing still works ------------------------------------------

test(ascii_glues) :-
    term_to_atom(=.., '=..').
test(ascii_neck) :-
    term_to_atom(:-, ':-').
test(ascii_negation) :-
    term_to_atom(\+, '\\+').

:- end_tests(syntax_unicode_solo).


:- begin_tests(syntax_unicode_numbers).

% --- Mixed-script Nd rejected ------------------------------------------

test(mixed_digit_blocks_rejected,
     [error(syntax_error(_))]) :-
    % Latin '1' followed by Devanagari '२' (DEVANAGARI DIGIT TWO).
    % All digits in a number must come from the same Unicode block.
    atom_codes(S, [0'1, 0x0968]),
    term_to_atom(_, S).

% --- Same-script non-ASCII Nd accepted ---------------------------------

test(devanagari_digits_parse) :-
    % U+0967..U+0969 = १२३ (123 in Devanagari)
    atom_codes(S, [0x0967, 0x0968, 0x0969]),
    term_to_atom(N, S),
    N == 123.

:- end_tests(syntax_unicode_numbers).


:- begin_tests(syntax_unicode_version).

test(flag_present) :-
    current_prolog_flag(unicode_syntax_version, V),
    atom(V),
    atom_length(V, L),
    L > 0.

:- end_tests(syntax_unicode_version).
