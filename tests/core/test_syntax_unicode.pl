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
:- encoding(utf8).
:- use_module(library(plunit)).
:- use_module(library(debug)).

test_syntax_unicode :-
    run_tests([ syntax_unicode_identifiers,
                syntax_unicode_layout,
                syntax_unicode_eol,
                syntax_unicode_solo,
                syntax_unicode_stray,
                syntax_unicode_numbers,
                syntax_unicode_version,
                syntax_unicode_atoms
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

test(nbsp_is_not_layout, [error(syntax_error(_))]) :-
    % U+00A0 NO-BREAK SPACE is deliberately not in Pattern_White_Space;
    % outside quoted material it raises a stray-character error.
    atom_codes(S, [0'a, 0x00A0, 0'+, 0x00A0, 0'b]),
    term_to_atom(_, S).

:- end_tests(syntax_unicode_layout).


:- begin_tests(syntax_unicode_eol).

% --- code_type/2 end_of_line covers the seven line-terminator-like
%     Pattern_White_Space code points: LF, VT, FF, CR, NEL (U+0085),
%     LS (U+2028), PS (U+2029).

test(end_of_line_lf)  :- code_type(0x000A, end_of_line).
test(end_of_line_vt)  :- code_type(0x000B, end_of_line).
test(end_of_line_ff)  :- code_type(0x000C, end_of_line).
test(end_of_line_cr)  :- code_type(0x000D, end_of_line).
test(end_of_line_nel) :- code_type(0x0085, end_of_line).
test(end_of_line_ls)  :- code_type(0x2028, end_of_line).
test(end_of_line_ps)  :- code_type(0x2029, end_of_line).

test(end_of_line_space_no, fail)  :- code_type(0x0020, end_of_line).
test(end_of_line_nbsp_no, fail)   :- code_type(0x00A0, end_of_line).

% --- %-comments terminate on the same set ------------------------------

test(comment_terminated_by_nel) :-
    atom_codes(S, [0'%, 0' , 0'c, 0'o, 0'm, 0'm, 0'e, 0'n, 0't,
                   0x0085, 0'r, 0'e, 0'a, 0'l, 0'.]),
    term_to_atom(T, S),
    T == real.
test(comment_terminated_by_ls) :-
    atom_codes(S, [0'%, 0' , 0'x, 0x2028, 0'y, 0'.]),
    term_to_atom(T, S),
    T == y.
test(comment_terminated_by_ps) :-
    atom_codes(S, [0'%, 0' , 0'x, 0x2029, 0'z, 0'.]),
    term_to_atom(T, S),
    T == z.
test(comment_terminated_by_cr) :-
    atom_codes(S, [0'%, 0' , 0'x, 0'\r, 0'q, 0'.]),
    term_to_atom(T, S),
    T == q.

:- end_tests(syntax_unicode_eol).


:- begin_tests(syntax_unicode_stray).

% Code points that fall outside the recognised syntax classes
% (layout, decimal, identifier, solo, bracket, quote) raise
% syntax_error(illegal_character) when they appear in source-text
% token-start position. Inside quoted atoms / strings / comments
% the same code points are accepted verbatim — only bidi-override
% code points are rejected there (CVE-2021-42574 mitigation).

% --- Helpers -----------------------------------------------------------

src_alone(Code, Src) :-                  % "<X>."
    atom_codes(Src, [Code, 0'.]).
src_after_layout(Code, Src) :-           % "a <X>."
    atom_codes(Src, [0'a, 0' , Code, 0'.]).
src_after_id(Code, Src) :-               % "a<X>."
    atom_codes(Src, [0'a, Code, 0'.]).

stray_alone(Code) :-
    src_alone(Code, S),
    catch(read_term_from_atom(S, _, []),
          error(syntax_error(illegal_character), _),
          true).
stray_after_layout(Code) :-
    src_after_layout(Code, S),
    catch(read_term_from_atom(S, _, []),
          error(syntax_error(illegal_character), _),
          true).
stray_after_id(Code) :-
    src_after_id(Code, S),
    catch(read_term_from_atom(S, _, []),
          error(syntax_error(illegal_character), _),
          true).

% Round-trip via writeq+term_string: non-syntax characters survive
% inside a single-quoted atom.
quoted_round_trip(Code) :-
    atom_codes(A0, [0'x, Code, 0'y]),
    with_output_to(string(S), writeq(A0)),
    term_string(A1, S),
    A0 == A1.

% --- Cf format outside Pattern_White_Space -----------------------------

test(stray_shy_alone)         :- stray_alone(0x00AD).         % SOFT HYPHEN
test(stray_shy_after_layout)  :- stray_after_layout(0x00AD).
test(stray_shy_after_id)      :- stray_after_id(0x00AD).
test(stray_zwsp_alone)        :- stray_alone(0x200B).         % ZERO WIDTH SPACE
test(stray_zwsp_after_layout) :- stray_after_layout(0x200B).

% ZWNJ and ZWJ are in XID_Continue per UAX #31 (Other_ID_Continue).
% They are stray at token-start but are absorbed when following an
% id_start.
test(zwnj_token_start_rejected)        :- stray_alone(0x200C).
test(zwnj_after_layout_rejected)       :- stray_after_layout(0x200C).
test(zwnj_absorbed_into_identifier) :-
    src_after_id(0x200C, S),
    read_term_from_atom(S, T, []),
    atom(T),
    atom_codes(T, [0'a, 0x200C]).

% --- Zs / Zl / Zp outside Pattern_White_Space --------------------------

test(stray_nbsp_alone)         :- stray_alone(0x00A0).         % NBSP
test(stray_nbsp_after_layout)  :- stray_after_layout(0x00A0).
test(stray_nbsp_after_id)      :- stray_after_id(0x00A0).
test(stray_ogham_space)        :- stray_alone(0x1680).
test(stray_nnbsp)              :- stray_alone(0x202F).
test(stray_mmsp)               :- stray_alone(0x205F).
test(stray_ideographic_space)  :- stray_alone(0x3000).

% --- Cn unassigned and noncharacters -----------------------------------

test(stray_unassigned)         :- stray_alone(0x0378).         % unassigned
test(stray_noncharacter)       :- stray_alone(0xFFFE).
test(stray_noncharacter_high)  :- stray_alone(0x10FFFE).

% --- No (numbers, not Nd) ----------------------------------------------

test(stray_vulgar_quarter)     :- stray_alone(0x00BC).         % ¼
test(stray_vulgar_half)        :- stray_alone(0x00BD).
test(stray_vulgar_three_quarters) :- stray_alone(0x00BE).

% --- Me enclosing combining marks --------------------------------------

test(stray_enclosing_mark)     :- stray_alone(0x0488).         % Me

% --- Mn non-spacing combining: stray at token start -------------------

test(stray_mn_alone)           :- stray_alone(0x0300).
test(stray_mn_after_layout)    :- stray_after_layout(0x0300).
test(mn_absorbed_into_identifier) :-
    src_after_id(0x0300, S),
    read_term_from_atom(S, T, []),
    atom(T).

% --- Inside quoted material: anything goes (modulo bidi overrides) -----

test(quoted_round_trip_nbsp)        :- quoted_round_trip(0x00A0).
test(quoted_round_trip_shy)         :- quoted_round_trip(0x00AD).
test(quoted_round_trip_zwsp)        :- quoted_round_trip(0x200B).
test(quoted_round_trip_unassigned)  :- quoted_round_trip(0x0378).
test(quoted_round_trip_noncharacter):- quoted_round_trip(0xFFFE).
test(quoted_round_trip_vulgar_half) :- quoted_round_trip(0x00BD).
test(quoted_round_trip_combining)   :- quoted_round_trip(0x0300).
test(quoted_round_trip_ideographic_space) :- quoted_round_trip(0x3000).

:- end_tests(syntax_unicode_stray).


:- begin_tests(syntax_unicode_solo).

% --- Unicode S? and P? form solo atoms (no glueing) --------------------

test(le_is_atom) :-
    % U+2264 LESS-THAN OR EQUAL TO (Sm)
    term_to_atom(T, '≤'),
    atom(T),
    atom_length(T, 1).
test(double_le_does_not_glue, error(syntax_error(_))) :-
    % Two ≤ in a row: would have glued under the old regime.  Now
    % each is a solo atom; without an operator declaration the
    % sequence is a syntax error.
    term_to_atom(_, '≤≤').
test(unmatched_quote_open_is_error, error(syntax_error(_))) :-
    % U+00AB « (Pi) is a paired delimiter; standalone is a syntax
    % error (no matching close).
    term_to_atom(_, '«').
test(quote_pair_is_compound) :-
    % U+00AB..U+00BB pair: «X» reads as '«»'(String), where String
    % is what "..." produces under the current double_quotes flag.
    % The contained text is literal, not parsed as a Prolog term.
    term_to_atom(T, '«hello world»'),
    T =.. [Functor, Arg],
    Functor == '«»',
    text_to_string(Arg, "hello world").
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


% Prolog syntax only accepts ASCII digits
test(devanagari_digits_parse, error(syntax_error(_))) :-
    % U+0967..U+0969 = १२३ (123 in Devanagari)
    term_string(_, "\u0967\u0968\u0969").

% Same-script non-ASCII Nd accepted
test(devanagari_digits_convert, N == 123) :-
    % U+0967..U+0969 = १२३ (123 in Devanagari)
    number_string(N, "\u0967\u0968\u0969").

% Mixed-script Nd rejected
test(mixed_digit_blocks_rejected, fail) :-
    % Latin '1' followed by Devanagari '२' (DEVANAGARI DIGIT TWO).
    % All digits in a number must come from the same Unicode block.
    number_string(_, "1\u0968").

% --- Sign is ASCII only ------------------------------------------------

test(ascii_sign_with_devanagari, N == 123) :-
    number_string(N, "+\u0967\u0968\u0969").

test(unicode_minus_with_devanagari, fail) :-
    % U+2212 MINUS SIGN is not the ASCII '-'; rejected.
    number_string(_, "\u2212\u0967\u0968\u0969").

% --- Float: same-block mantissa and fraction ---------------------------

test(devanagari_float, F =:= 123.45) :-
    number_string(F, "\u0967\u0968\u0969.\u096a\u096b").

test(mixed_int_and_frac_blocks, fail) :-
    % Devanagari int, ASCII frac.
    number_string(_, "\u0967\u0968\u0969.45").

% --- Float exponent: same-block mantissa and exponent ------------------

test(devanagari_exponent, F =:= 123e5) :-
    % U+096B = DEVANAGARI DIGIT FIVE.
    number_string(F, "\u0967\u0968\u0969e\u096b").

test(mixed_mantissa_and_exponent, fail) :-
    number_string(_, "\u0967\u0968\u0969e5").

% --- Rational: same-block numerator and denominator --------------------

test(devanagari_rational) :-
    number_string(R, "\u0967\u0968\u0969r\u096a\u096b"),
    R =:= 123 rdiv 45.

test(mixed_rational_blocks, fail) :-
    number_string(_, "12r\u096a\u096b").

% --- Superscript / subscript digits are id_continue, not Nd ------------

test(superscript_one_not_a_digit, fail) :-
    % U+00B9 SUPERSCRIPT ONE is general category No, not Nd.
    number_string(_, "1\u00b9").

% --- Hex prefix: digits stay ASCII -------------------------------------

test(hex_prefix_ascii, N == 255) :-
    number_string(N, "0xff").

test(hex_prefix_with_unicode_digit, fail) :-
    number_string(_, "0x\u0967").

% --- 0'<char> char-code form: any single Unicode scalar ----------------

test(char_code_ascii_letter, N == 0'a) :-
    number_string(N, "0'a").

test(char_code_unicode_literal, N == 0x0600) :-
    % Literal non-ASCII codepoint after 0' yields its integer value.
    number_string(N, "0'؀").

test(char_code_unicode_escape_via_reader, N == 0x0600) :-
    % \u escape inside 0' is interpreted by the term reader (it has
    % the character_escapes flag set); number_string and friends
    % operate on the literal text and do not interpret \-escapes.
    term_string(N, "0'\\u0600").

:- end_tests(syntax_unicode_numbers).


:- begin_tests(syntax_unicode_version).

test(flag_present) :-
    current_prolog_flag(unicode_syntax_version, V),
    atom(V),
    atom_length(V, L),
    L > 0.

:- end_tests(syntax_unicode_version).


:- begin_tests(syntax_unicode_atoms,
               [ setup(unload_file(library(unicode))),
                 cleanup(unload_file(library(unicode)))
               ]).

% U+0E2A U+0E27 U+0E31 U+0E2A U+0E14 U+0E35
% U+0E0A U+0E32 U+0E27 U+0E42 U+0E25 U+0E01
% — "Hello, World" in Thai.  Already in NFC: the Thai vowel signs
% (ั ี) have wcwidth==0 so the wcwidth fallback flags them as
% combining, but utf8proc confirms the string is unchanged by NFC.
thai_hello_world("สวัสดี\c
		  ชาวโลก").

% Exercises the unicode_atoms policy.  The first block runs WITHOUT
% library(unicode): modes accept/error/reject must work standalone, and
% error/reject use the wcwidth-based conservative fallback.  The second
% block runs AFTER nfc auto-loads library(unicode) and so the precise
% utf8proc-backed check is in effect.

test(default_flag_is_accept) :-
    current_prolog_flag(unicode_atoms, accept).

% --- without library(unicode): wcwidth fallback ------------------------

test(error_rejects_simple_nfd_without_hook,
     error(syntax_error(non_nfc_atom))) :-
    term_string(_, "cafe\u0301", [unicode_atoms(error)]).

test(error_thai_false_positive_without_hook,
     [ error(syntax_error(non_nfc_atom)),
       condition(\+ current_prolog_flag(atom_normalize_hook, true))
     ]) :-
    % The Thai greeting is already in NFC, but the wcwidth fast path
    % conservatively flags any combining mark as non-NFC and so
    % rejects it.  The precise check (next block) accepts it.
    thai_hello_world(S),
    term_string(_, S, [unicode_atoms(error)]).

test(error_accepts_pure_ascii) :-
    term_string(foo, "foo", [unicode_atoms(error)]).

test(reject_rejects_non_ascii,
     error(syntax_error(non_ascii_atom))) :-
    term_string(_, "caf\u00e9", [unicode_atoms(reject)]).

test(reject_passes_quoted_non_ascii) :-
    term_string(T, "'caf\u00e9'", [unicode_atoms(reject)]),
    atom_codes(T, `caf\u00e9`).

% --- nfc auto-loads library(unicode) -----------------------------------

:- if(exists_source(library(unicode))).
% --- with library(unicode): precise utf8proc-backed NFC check ----------
%
% These tests run after nfc_auto_loads_unicode_library above and so
% see the kernel normalisation hook registered.  Same Thai greeting as
% the without_hook test now passes; cafe+combining is still rejected
% as it is genuinely not NFC.

test(nfc_auto_loads_unicode_library) :-
    assertion(\+ current_prolog_flag(atom_normalize_hook, true)),
    term_string(T1, "cafe\u0301", [unicode_atoms(nfc)]),
    term_string(T2, "caf\u00e9",  [unicode_atoms(nfc)]),
    T1 == T2,
    assertion(current_prolog_flag(atom_normalize_hook, true)).

test(error_accepts_thai_with_hook) :-
    thai_hello_world(S),
    term_string(T, S, [unicode_atoms(error)]),
    atom_string(T, S).
:- endif.

test(error_rejects_simple_nfd_with_hook,
     error(syntax_error(non_nfc_atom))) :-
    term_string(_, "cafe\u0301", [unicode_atoms(error)]).

% --- bidi-override always rejected -------------------------------------

test(bidi_in_unquoted_atom_is_error,
     error(syntax_error(bidi_override(0x202E)))) :-
    term_string(_, "a\u202eb", []).

test(bidi_via_escape_is_allowed) :-
    term_string(T, "'a\\u202Eb'", []),
    atom_codes(T, `a\u202Eb`).

:- end_tests(syntax_unicode_atoms).
