/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org
    Copyright (c)  2006-2026, University of Amsterdam
                              VU University Amsterdam
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

:- module(prolog_syntax_map,
          [ main/0,
            write_syntax_map/2          % +File, +Options
          ]).
:- use_module(library(main)).
:- use_module(library(option)).
:- use_module(library(debug), [assertion/1]).
:- use_module(library(lists), [member/2, flatten/2, numlist/3]).
:- use_module(library(option), [option/3]).
:- use_module(library('unicode/unicode_data'), [unicode_property/2]).
:- use_module(derived_core_properties,
              [unicode_derived_core_property/2,
               id_superscript/1,
               id_subscript/1,
               white_space/1,
               east_asian_width/2,
               bidi_mirror/2]).
:- use_module(library(apply), [maplist/3]).
:- use_module(library(readutil), [read_line_to_codes/2]).

/** <module> Generate Prolog Unicode map

Create a C structure and access functions for classifying the
characters needed by the Prolog source-text syntax. The mapping is
grounded in [UAX #31](https://www.unicode.org/reports/tr31/) and
backed by the Unicode Character Database files
DerivedCoreProperties.txt, UnicodeData.txt, and EastAsianWidth.txt.

Each code point gets exactly one byte in the generated table. The
byte is structured as:

    bits 0..3  category enum (see category_index/2)
    bits 4..5  wcwidth+1 (0=invalid -1, 1=zero, 2=normal 1, 3=wide 2)
    bits 6..7  reserved

Categories are mutually exclusive primary classes; legacy U_*
flag-bit semantics are preserved through a 16-entry cat_to_flags[]
lookup, so existing macros like `uflagsW(c) & U_LAYOUT` continue to
work.

Source mapping for the categories:

    layout            ⇐ Pattern_White_Space (UAX #31 R3a)
    decimal           ⇐ general category Nd
    id_start_variable ⇐ general category Lu
    id_start_atom     ⇐ XID_Start \ Lu (UAX #31 R1) +
                          predef ASCII id_start for the JS path
    id_continue       ⇐ XID_Continue \ XID_Start (UAX #31 R1) +
                          superscript/subscript-digit profile addition
    id_continue_solo  ⇐ XID_Continue ∩ solo_cat
                          (e.g. '_' U+005F, '·' U+00B7)
    bracket           ⇐ general categories Ps ∪ Pe
                          (paired delimiters; partner via pl_pair_table)
    quote             ⇐ general categories Pi ∪ Pf
                          (paired delimiters; partner via pl_pair_table)
    solo              ⇐ Sm/Sc/Sk/So/Pc/Pd/Po
                          (broader than UAX #31 Pattern_Syntax; PIP §4.2)
    symbol            ⇐ ASCII operator characters (JS path only)
    other / unassigned ⇒ category 0 (treated as stray by the parser)

Width data in bits 4..5 is sourced from EastAsianWidth.txt (UAX #11)
and the general_category property; PL_wcwidth() reads these bits at
runtime.

Pair tables (Ps↔Pe and Pi↔Pf) come from BidiMirroring.txt with the
standard curly quote pairs U+2018/U+2019 and U+201C/U+201D added
explicitly (those have Bidi_Mirrored=No and are absent from
BidiMirroring.txt).  The reader uses pl_pair_table to recognise
'<open><close>'/1 paired-delimiter terms — see read_paired_term in
pl-read.c.

Usage:

  1. Get DerivedCoreProperties.txt, UnicodeData.txt,
     EastAsianWidth.txt, and BidiMirroring.txt from the Unicode
     consortium and copy or link them into this directory.
  2. Run `swipl prolog_syntax_map.pl` in this directory, which updates
     `../pl-umap.c`.

This module can also create a JavaScript file, which is used for SWISH.
The command for this is

    swipl prolog_syntax_map.pl --out=prolog-ctype.js --lang=javascript
*/

:- multifile
    user:file_search_path/2.

user:file_search_path(unicode, '.').


:- initialization(main, main).

main(Argv) :-
    argv_options(Argv, R, Options),
    assertion(R == []),
    option(out(File), Options, '../pl-umap.c'),
    write_syntax_map(File, Options),
    (   option(lang(javascript), Options)
    ->  true
    ;   option(version_out(VFile), Options, '../pl-umap-version.h'),
        write_version_header(VFile)
    ).

%!  write_version_header(+File)
%
%   Emit a tiny C header that defines UNICODE_SYNTAX_VERSION as a
%   string literal carrying the Unicode version of the UCD files used
%   to generate ../pl-umap.c.  Read from the first comment line of
%   DerivedCoreProperties.txt, e.g.
%
%       # DerivedCoreProperties-17.0.0.txt
%
%   yields "17.0.0".

write_version_header(File) :-
    unicode_data_version(Version),
    setup_call_cleanup(
        open(File, write, Out),
        ( generated_file(Out),
          format(Out, '#ifndef UNICODE_SYNTAX_VERSION~n', []),
          format(Out, '#define UNICODE_SYNTAX_VERSION "~w"~n', [Version]),
          format(Out, '#endif~n', [])
        ),
        close(Out)).

unicode_data_version(Version) :-
    absolute_file_name(unicode('DerivedCoreProperties.txt'),
                       Path, [access(read)]),
    setup_call_cleanup(
        open(Path, read, In),
        read_line_to_codes(In, Line),
        close(In)),
    string_codes(LineS, Line),
    string_concat("# DerivedCoreProperties-", Rest, LineS),
    string_concat(VersionS, ".txt", Rest),
    atom_string(Version, VersionS).

last_unicode_page(LastPage) :-
    LastPage is (0x10ffff + 1) // 0x100.

                 /*******************************
                 *           C TABLES           *
                 *******************************/

%!  write_syntax_map(+File, +Options)
%
%   Options supported are:
%
%           # first_codepage [0]
%           Code page to start
%
%           # last_codepage [last_unicode_page/1]
%           Code page to end.

write_syntax_map(File, Options) :-
    setup_call_cleanup(
        open(File, write, Out),
        write_sort_map(Out, Options),
        close(Out)).

write_sort_map(Out, Options) :-
    gen_tables(Tables, Options),
    write_header(Out, Options),
    forall((member(table(CP, Map), Tables),
            is_list(Map)),
           write_codepage(Out, CP, Map, Options)),
    write_map(Out, Tables, Options),
    write_footer(Out, Options),
    write_decimal_bases(Out, Options),
    write_pair_table(Out, Options).

write_codepage(Out, CP, Map, Options) :-
    option(lang(javascript), Options),
    !,
    assertion(length(Map, 256)),
    cp_name(CP, CPN),
    format(Out, 'var ~w = "', [CPN]),
    map_chars(Map, Out),
    format(Out, '";~n', []).
write_codepage(Out, CP, Map, _Options) :-
    assertion(length(Map, 256)),
    cp_name(CP, CPN),
    format(Out, 'static const unsigned char ~w[256] =~n', [CPN]),
    format(Out, '{ ', []),
    map_entries(Map, CP, 0, Out),
    From is CP*256+(256-8),
    To   is From + 7,
    format(Out, '  /* U~|~`0t~16R~4+..U~|~`0t~16R~4+ */~n};~n~n', [From,To]).

cp_name(CP, CPN) :-
    format(atom(CPN), 'ucp0x~|~`0t~16r~2+', [CP]).

map_entries([], _, _, _).
map_entries([H|T], CP, I, Out) :-
    (   I == 0
    ->  true
    ;   0 =:= I mod 8
    ->  From is CP*256+(I-8),
        To   is From + 7,
        format(Out, ', /* U~|~`0t~16R~4+..U~|~`0t~16R~4+ */~n  ', [From,To])
    ;   format(Out, ', ', [])
    ),
    format(Out, '0x~|~`0t~16r~2+', [H]),
    I2 is I + 1,
    map_entries(T, CP, I2, Out).

map_chars([], _).
map_chars([H|T], Out) :-
    format(Out, '\\x~|~`0t~16r~2+', [H]),
    map_chars(T, Out).


write_map(Out, Tables, Options) :-
    option(lang(javascript), Options),
    !,
    last_unicode_page(DefLast),
    option(last_codepage(Last), Options, DefLast),
    format(Out, 'var uflags_map = [', []),
    js_map_tables(0, Last, Tables, Out),
    format(Out, '];~n~n', []).
write_map(Out, Tables, Options) :-
    last_unicode_page(DefLast),
    option(last_codepage(Last), Options, DefLast),
    format(Out,
           'static const unsigned char* const uflags_map[UNICODE_MAP_SIZE] =~n',
           []),
    format(Out, '{ ', []),
    map_tables(0, Last, Tables, Out),
    format(Out, '~N};~n~n', []).

map_tables(CP, Last, _, _) :-
    CP > Last,
    !.
map_tables(CP, Last, Tables, Out) :-
    (   CP == 0
    ->  true
    ;   0 =:= CP mod 8
    ->  format(Out, ',~n  ', [])
    ;   format(Out, ', ', [])
    ),
    memberchk(table(CP, Map), Tables),
    (   is_list(Map)
    ->  cp_name(CP, CPN),
        format(Out, '~w', [CPN])
    ;   format(Out, '~|~tF(0x~16r)~7+', [Map])
    ),
    CP2 is CP + 1,
    map_tables(CP2, Last, Tables, Out).


js_map_tables(CP, Last, _, _) :-
    CP > Last,
    !.
js_map_tables(CP, Last, Tables, Out) :-
    (   CP == 0
    ->  true
    ;   0 =:= CP mod 8
    ->  format(Out, ',~n  ', [])
    ;   format(Out, ', ', [])
    ),
    memberchk(table(CP, Map), Tables),
    (   is_list(Map)
    ->  cp_name(CP, CPN),
        format(Out, '~w', [CPN])
    ;   format(Out, '0x~|~`0t~16r~2+', [Map])
    ),
    CP2 is CP + 1,
    js_map_tables(CP2, Last, Tables, Out).


write_header(Out, Options) :-
    option(lang(javascript), Options),
    !,
    map_size(Size, Options),
    generated_file(Out),
    format(Out, 'define([], function() {~n', []),
    format(Out, 'var UNICODE_MAP_SIZE~t= ~d;~32|~n', [Size]),
    forall(flag_name(Name, Hex),
           ( upcase_atom(Name, Up),
             format(Out, 'var U_~w~t= 0x~16r;~32|~n', [Up, Hex])
           )),
    format(Out, '~nvar cat_to_flags = [', []),
    write_cat_to_flags_js(Out),
    format(Out, '];~n~n', []).
write_header(Out, Options) :-
    generated_file(Out),
    map_size(Size, Options),
    format(Out, '#define UNICODE_MAP_SIZE ~d~n', [Size]),
    format(Out, '#define F(c) (const unsigned char*)(c)~n~n', []),
    forall(flag_name(Name, Hex),
           ( upcase_atom(Name, Up),
             format(Out, '#define U_~w~t0x~16r~32|~n', [Up, Hex])
           )),
    format(Out, '~n', []),
    format(Out,
'/* Each entry in the per-page tables below holds:~n\c
       bits 0..3  category enum (see prolog_syntax_map.pl)~n\c
       bits 4..5  wcwidth+1 (0=invalid, 1=zero, 2=normal, 3=wide)~n\c
       bits 6..7  reserved~n\c
   cat_to_flags[] maps the category enum back to the legacy U_*~n\c
   bit pattern, so existing macros like uflagsW(c) & U_LAYOUT still~n\c
   work.~n\c
 */~n', []),
    format(Out, 'static const unsigned char cat_to_flags[16] =~n{ ', []),
    write_cat_to_flags_c(Out),
    format(Out, '~N};~n~n', []).

write_cat_to_flags_c(Out) :-
    numlist(0, 15, Indices),
    write_cat_entries_c(Indices, Out).

write_cat_entries_c([], _).
write_cat_entries_c([I|T], Out) :-
    cat_to_flags(I, F),
    (   I == 0
    ->  true
    ;   0 =:= I mod 8
    ->  format(Out, ',~n  ', [])
    ;   format(Out, ', ', [])
    ),
    format(Out, '0x~|~`0t~16r~2+', [F]),
    write_cat_entries_c(T, Out).

write_cat_to_flags_js(Out) :-
    numlist(0, 15, Indices),
    write_cat_entries_js(Indices, Out).

write_cat_entries_js([], _).
write_cat_entries_js([I|T], Out) :-
    cat_to_flags(I, F),
    (   I == 0
    ->  true
    ;   format(Out, ', ', [])
    ),
    format(Out, '0x~|~`0t~16r~2+', [F]),
    write_cat_entries_js(T, Out).

map_size(Size, Options) :-
    last_unicode_page(DefLast),
    option(last_codepage(Last), Options, DefLast),
    Size is Last+1.

generated_file(Out) :-
    format(Out, '/*  Generated file.  Do not edit!\n    \c
                         Generated by Unicode/prolog_syntax_map.pl\n\c
                     */~n~n', []).

write_footer(Out, Options) :-
    option(lang(javascript), Options),
    !,
    format(Out,
'\c
function uflagsRaw(chr) {
  var code = chr.charCodeAt(0);
  var cp = Math.floor(code/0x100);
  if ( cp < UNICODE_MAP_SIZE ) {
    var map = uflags_map[cp];

    if ( typeof(map) == "number" ) {
      return map;
    } else {
      return map.charCodeAt(code&0xff);
    }
  }
  return 0;
}

function uflagsW(chr) {
  return cat_to_flags[uflagsRaw(chr) & 0xF];
}

return {
  flags:       uflagsW,
  id_start:    function(chr) { return (uflagsW(chr) & U_ID_START)    != 0 },
  id_continue: function(chr) { return (uflagsW(chr) & U_ID_CONTINUE) != 0 },
  uppercase:   function(chr) { return (uflagsW(chr) & U_UPPERCASE)   != 0 },
  symbol:      function(chr) { return (uflagsW(chr) & U_SYMBOL)      != 0 },
  solo:        function(chr) { return (uflagsW(chr) & U_SOLO)        != 0 },
  layout:      function(chr) { return (uflagsW(chr) & U_LAYOUT)      != 0 },
  other:       function(chr) { return (uflagsW(chr) & U_OTHER)       != 0 },
  decimal:     function(chr) { return (uflagsW(chr) & U_DECIMAL)     != 0 },
  // Backward compatibility types
  separator:   function(chr) { return (uflagsW(chr) & U_LAYOUT)      != 0 },
  control:     function(chr) { return (uflagsW(chr) & U_OTHER)       != 0 }
}
});~n', []).
write_footer(Out, _Options) :-
    format(Out,
           'static unsigned char\n\c
                uflagsRaw(int code)\n\c
                { int cp = (unsigned)code / 256;\n\c
                \n  \c
                  if ( cp < UNICODE_MAP_SIZE )\n  \c
                  { const unsigned char *s = uflags_map[cp];\n    \c
                    if ( s < (const unsigned char *)256 )\n      \c
                      return (unsigned char)(uintptr_t)s;\n    \c
                    return s[code&0xff];\n  \c
                  }\n  \c
                  return 0;\n\c
                }\n\n\c
                static int\n\c
                uflagsW(int code)\n\c
                { return cat_to_flags[uflagsRaw(code) & 0xF];\n\c
                }\n\n', []).


                 /*******************************
                 *             TABLES           *
                 *******************************/

%!  gen_tables(-Tables, +Options)
%
%   Table is of  the  format  below,   where  CodePage  is  the page
%   (0..255) for 16-bit Unicode and  ValueList   are  the values for
%   each character.
%
%           table(CodePage, ValueList)

gen_tables(Tables, Options) :-
    findall(table(CP,Map), table(CP, Map, Options), Tables).

table(CP, Map, Options) :-
    code_page(CP, Options),
    option(lang(Lang), Options, 'C'),
    findall(M, char(CP, M, Lang), Map0),
    flat_map(Map0, Map).

code_page(CP, Options) :-
    last_unicode_page(DefPage),
    option(first_codepage(First), Options, 0),
    option(last_codepage(Last), Options, DefPage),
    between(First, Last, CP).

char(CP, Value, Lang) :-
    between(0, 255, I),
    Code is 256*CP+I,
    code_byte(Lang, Code, Value).

%!  code_byte(+Lang, +Code, -Byte) is det.
%
%   Byte stored in the per-page uflags table. Bit layout:
%
%     bits 0..3  category enum (see category_index/2)
%     bits 4..5  wcwidth+1 (0=invalid -1, 1=zero, 2=normal, 3=wide)
%     bits 6..7  reserved
%
%   Width data is sourced from the East_Asian_Width property
%   (UAX #11) and the general_category property at table-build
%   time. The runtime PL_wcwidth() reads bits 4..5 directly.

code_byte(Lang, Code, Byte) :-
    code_class(Lang, Code, Class),
    category_index(Class, Cat),
    code_width(Code, Width),
    encode_width(Width, WBits),
    Byte is Cat \/ (WBits << 4).

%!  code_width(+Code, -Width) is det.
%
%   wcwidth-style display width of Code:
%
%     -1  non-printable (control / DEL / C1 control)
%      0  combining mark, format / zero-width invisible char
%      1  normal printable
%      2  wide (East Asian W or F, or default-W in CJK ranges)
%
%   The classification matches Markus Kuhn's mk_wcwidth() behavior
%   updated to current Unicode data: combining is general category
%   Mn/Me + Cf (with U+00AD SOFT HYPHEN as the documented exception
%   that stays width 1); Hangul Jamo medial/final consonants
%   (U+1160..U+11FF) and U+200B ZERO WIDTH SPACE are zero; East Asian
%   Wide / Fullwidth → 2.

code_width(0,      0) :- !.                           % NUL
code_width(Code,  -1) :-                              % C0 / C1 controls
    ( Code < 32
    ; Code >= 0x7F, Code < 0xA0
    ),
    !.
code_width(0x00AD, 1) :- !.                           % SOFT HYPHEN exception
code_width(Code,   0) :-                              % Hangul Jamo medial/final
    Code >= 0x1160, Code =< 0x11FF, !.
code_width(0x200B, 0) :- !.                           % ZERO WIDTH SPACE
code_width(Code,   0) :-                              % combining + format
    unicode_property(Code, general_category(Cat)),
    zero_width_cat(Cat),
    !.
code_width(Code,   2) :-                              % East Asian Wide / Fullwidth
    east_asian_width(Code, EAW),
    wide_eaw(EAW),
    !.
code_width(_,      1).                                % default

zero_width_cat('Mn').
zero_width_cat('Me').
zero_width_cat('Cf').

wide_eaw(w).
wide_eaw(f).

%!  encode_width(+Width, -Bits) is det.
%
%   Pack a wcwidth value into 2 bits: 0=invalid, 1=zero, 2=normal,
%   3=wide. PL_wcwidth() reverses this with `(bits - 1)`.

encode_width(-1, 0).
encode_width( 0, 1).
encode_width( 1, 2).
encode_width( 2, 3).

%!  code_class(+Lang, +Code, -Class) is det.
%
%   Class is the unique syntax category of Code. First-match priority
%   over the priority_class/3 clauses; falls through to `unassigned`
%   for code points that aren't in the Unicode database.

code_class(Lang, Code, Class) :-
    priority_class(Lang, Code, Class), !.
code_class(_, _, unassigned).

priority_class(_, Code, layout) :-
    white_space(Code).
priority_class(_, Code, decimal) :-
    unicode_property(Code, general_category('Nd')).
priority_class(_, Code, id_start_variable) :-
    unicode_property(Code, general_category(Cat)),
    upper_cat(Cat).
priority_class(_, Code, id_start_atom) :-
    unicode_derived_core_property(Code, xid_start).
priority_class(_, Code, id_continue_solo) :-
    is_id_continue(Code),
    is_solo_cat(Code).
priority_class(_, Code, id_continue) :-
    is_id_continue(Code).
priority_class(javascript, Code, symbol) :-
    Code < 256,
    code_type(Code, prolog_symbol).
priority_class(_, Code, bracket) :-
    is_bracket_cat(Code).
priority_class(_, Code, quote) :-
    is_quote_cat(Code).
priority_class(_, Code, solo) :-
    is_solo_cat(Code).
priority_class(_, Code, other) :-
    unicode_property(Code, general_category(_)).

is_id_continue(Code) :-
    unicode_derived_core_property(Code, xid_continue).
is_id_continue(Code) :-
    id_superscript(Code).
is_id_continue(Code) :-
    id_subscript(Code).

is_solo_cat(Code) :-
    unicode_property(Code, general_category(Cat)),
    solo_cat(Cat).

is_bracket_cat(Code) :-
    unicode_property(Code, general_category(Cat)),
    ( Cat == 'Ps' ; Cat == 'Pe' ).

is_quote_cat(Code) :-
    unicode_property(Code, general_category(Cat)),
    ( Cat == 'Pi' ; Cat == 'Pf' ).

%!  bracket_pair(?Open, ?Close) is nondet.
%
%   Open and Close form a Ps↔Pe bracket pair, derived from
%   Unicode BidiMirroring.txt and filtered to general_category
%   Ps (open) / Pe (close).

bracket_pair(Open, Close) :-
    bidi_mirror(Open, Close),
    unicode_property(Open, general_category('Ps')),
    unicode_property(Close, general_category('Pe')).

%!  quote_pair(?Open, ?Close) is nondet.
%
%   Open and Close form a Pi↔Pf quote pair. Pi/Pf pairing is
%   script-conventional rather than algorithmic; the Bidi
%   mirroring data covers the angled quotation marks but not the
%   asymmetric curly forms, so the standard curly pairs are
%   curated below.

quote_pair(Open, Close) :-
    bidi_mirror(Open, Close),
    unicode_property(Open, general_category('Pi')),
    unicode_property(Close, general_category('Pf')).
quote_pair(Open, Close) :-
    quote_pair_curated(Open, Close).

quote_pair_curated(0x2018, 0x2019).        % LEFT/RIGHT SINGLE QUOTATION MARK ' '
quote_pair_curated(0x201C, 0x201D).        % LEFT/RIGHT DOUBLE QUOTATION MARK " "


                 /*******************************
                 *         PAIR TABLE           *
                 *******************************/

%!  pair_entries(-Entries) is det.
%
%   Entries is a sorted list of pair_entry/3 terms — one per code
%   point that participates in a bracket or quote pair, both as
%   open and close. Used to emit the C pair_table[] in pl-umap.c
%   for binary-search lookup of the matching delimiter.

pair_entries(Entries) :-
    findall(pair_entry(Code, Mate, IsOpen),
            ( pair(Open, Close),
              ( Code = Open,  Mate = Close, IsOpen = 1
              ; Code = Close, Mate = Open,  IsOpen = 0
              )
            ),
            Es0),
    sort(Es0, Entries).

pair(Open, Close) :- bracket_pair(Open, Close).
pair(Open, Close) :- quote_pair(Open, Close).

write_pair_table(Out, Options) :-
    option(lang(javascript), Options),
    !,
    %% JS path: nothing for now (Stage 6 reader is C-only).
    true.
write_pair_table(Out, _Options) :-
    pair_entries(Entries),
    length(Entries, N),
    format(Out, '#define PL_PAIR_TABLE_SIZE ~d~n', [N]),
    format(Out,
           'typedef struct {~n  \c
              int code;~n  \c
              int mate;~n  \c
              int is_open;~n\c
            } pl_pair_entry;~n~n', []),
    format(Out,
           'static const pl_pair_entry pl_pair_table[PL_PAIR_TABLE_SIZE] =~n\c
            { ', []),
    write_pair_entries(Entries, Out, 0),
    format(Out, '~N};~n~n', []),
    format(Out,
           '/* Binary-search the pair table for `code`.  Returns the matching\n\c
                delimiter (open ↔ close) or 0 if `code` is not a paired\n\c
                bracket / quote.  `*is_open` (if non-NULL) gets 1 when `code`\n\c
                is the open side, 0 when it is the close.\n\c
             */~n\c
            static int~n\c
            pl_pair_lookup(int code, int *is_open)~n\c
            { int lo = 0, hi = PL_PAIR_TABLE_SIZE - 1;~n\c
              while ( lo <= hi )~n\c
              { int mid = (lo + hi) / 2;~n\c
                int c = pl_pair_table[mid].code;~n\c
                if ( code == c )~n\c
                { if ( is_open ) *is_open = pl_pair_table[mid].is_open;~n  \c
                    return pl_pair_table[mid].mate;~n\c
                }~n\c
                if ( code < c ) hi = mid - 1; else lo = mid + 1;~n\c
              }~n\c
              return 0;~n\c
            }~n~n', []).

write_pair_entries([], _, _).
write_pair_entries([pair_entry(Code, Mate, IsOpen)|T], Out, I) :-
    (   I == 0
    ->  true
    ;   0 =:= I mod 4
    ->  format(Out, ',~n  ', [])
    ;   format(Out, ', ', [])
    ),
    format(Out, '{ 0x~|~`0t~16r~6+, 0x~|~`0t~16r~6+, ~w }',
           [Code, Mate, IsOpen]),
    I2 is I + 1,
    write_pair_entries(T, Out, I2).

%!  category_index(?Class, ?Index) is det.
%
%   The 4-bit category enum values stored in bits 0..3 of each
%   uflags_map byte. Indices 3 and 4 are reserved for Stage 6
%   (bracket and quote pair semantics); Stage 4 leaves them empty.

category_index(unassigned,        0).
category_index(other,             0).
category_index(layout,            1).
category_index(solo,              2).
category_index(bracket,           3).
category_index(quote,             4).
category_index(id_continue,       5).
category_index(id_start_atom,     6).
category_index(id_start_variable, 7).
category_index(decimal,           8).
category_index(symbol,            9).
category_index(id_continue_solo, 10).

%!  cat_to_flags(?Index, ?Flags) is det.
%
%   Maps each category index back to the legacy U_* flag-bit pattern.
%   This preserves the semantics of the existing macros (PlBlankW,
%   PlSoloW, PlIdContW, ...) while the underlying storage uses the
%   compact category enum. Generated as a 16-entry C lookup table.

cat_to_flags(0,  0).
cat_to_flags(1,  0x10).        % U_LAYOUT
cat_to_flags(2,  0x20).        % U_SOLO
cat_to_flags(3,  0x20).        % bracket  → solo (Stage 4 placeholder)
cat_to_flags(4,  0x20).        % quote    → solo (Stage 4 placeholder)
cat_to_flags(5,  0x02).        % U_ID_CONTINUE
cat_to_flags(6,  0x03).        % U_ID_START | U_ID_CONTINUE
cat_to_flags(7,  0x07).        % U_ID_START | U_ID_CONTINUE | U_UPPERCASE
cat_to_flags(8,  0x82).        % U_ID_CONTINUE | U_DECIMAL
cat_to_flags(9,  0x08).        % U_SYMBOL
cat_to_flags(10, 0x22).        % U_SOLO | U_ID_CONTINUE
cat_to_flags(11, 0).
cat_to_flags(12, 0).
cat_to_flags(13, 0).
cat_to_flags(14, 0).
cat_to_flags(15, 0).

%!  flag_name(?Name, ?Bit) is multi.
%
%   Legacy U_* flag bits, kept as #define output for backward compat
%   with C code that uses `uflagsW(c) & U_LAYOUT` etc.

flag_name(id_start,    0x01).
flag_name(id_continue, 0x02).
flag_name(uppercase,   0x04).
flag_name(symbol,      0x08).
flag_name(layout,      0x10).
flag_name(solo,        0x20).
flag_name(other,       0x40).
flag_name(decimal,     0x80).

% See http://www.unicode.org/reports/tr44/#Property_Values

upper_cat('Lu').

solo_cat('Sm').       % a symbol of primarily mathematical use
solo_cat('Sc').       % a currency sign
solo_cat('Sk').       % a non-letterlike modifier symbol
solo_cat('So').       % a symbol of other type
solo_cat('Pc').       % a connecting punctuation mark, like a tie
solo_cat('Pd').       % a dash or hyphen punctuation mark
solo_cat('Ps').       % an opening punctuation mark (of a pair)
solo_cat('Pe').       % a closing punctuation mark (of a pair)
solo_cat('Pi').       % an initial quotation mark
solo_cat('Pf').       % a final quotation mark
solo_cat('Po').       % a punctuation mark of other type

flat_map(Map0, Value) :-
    sort(Map0, [Value]),
    !.
flat_map(Map, Map).


                 /*******************************
                 *            DECIMALS          *
                 *******************************/

write_decimal_bases(Out, Options) :-
    decimal_bases(Bases, Options),
    format(Out, 'static const int decimal_bases[] =~n{ ', []),
    write_bases(Out, Bases, 0).

write_bases(Out, [], _) :-
    !,
    format(Out, '~N};~n~n', []).
write_bases(Out, [H|T], I) :-
    (   I == 0
    ->  true
    ;   0 =:= I mod 8
    ->  format(Out, ',~n  ', [])
    ;   format(Out, ', ', [])
    ),
    format(Out, '0x~|~`0t~16r~2+', [H]),
    I2 is I + 1,
    write_bases(Out, T, I2).


%!  decimal_bases(-Bases, +Options) is det.
%
%   Basis is a list of base codepoints for a decimal block of length
%   10.

decimal_bases(Bases, Options) :-
    findall(Digit, digit(Digit, Options), Digits),
    digit_blocks(Digits, Blocks),
    maplist(digit_base, Blocks, Bases0),
    flatten(Bases0, Bases).

digit(Digit, Options) :-
    code_page(CP, Options),
    Start is CP*256,
    End is Start+255,
    between(Start, End, Digit),
    unicode_property(Digit, general_category('Nd')).

digit_blocks(Digits, [Block|BT]) :-
    block(Digits, T, Block),
    !,
    digit_blocks(T, BT).
digit_blocks(_, []).

block([H|T0], T, [H|Block]) :-
    sequence(H, T0, T, Block),
    Block \== [],
    !.
block([_|T0], T, Block) :-
    block(T0, T, Block).

sequence(I0, [H|T0], T, [H|BT]) :-
    H =:= I0+1,
    !,
    sequence(H, T0, T, BT).
sequence(_, T, T, []).

digit_base(Block, Base) :-
    length(Block, 10),
    !,
    Block = [Base|_].
digit_base(Block, [Base0|Bases]) :-
    length(Block, Len),
    Len mod 10 =:= 0,
    Block = [Base0|_],
    End is Len/10-1,
    numlist(1, End, N0),
    maplist(mul(10), N0, N1),
    maplist(plus(Base0), N1, Bases).

mul(Times, N0, N) :-
    N is N0*Times.
