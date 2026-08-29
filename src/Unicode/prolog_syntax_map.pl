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
:- use_module(library(lists), [member/2, flatten/2, numlist/3,
                               sum_list/2, nth0/3]).
:- use_module(library(option), [option/3]).
:- use_module(library('unicode/unicode_data'), [unicode_property/2]).
:- use_module(derived_core_properties,
              [unicode_derived_core_property/2,
               unicode_property/3,
               id_superscript/1,
               id_subscript/1,
               white_space/1,
               east_asian_width/2,
               bidi_mirror/2]).
:- use_module(library(apply), [maplist/2, maplist/3]).
:- use_module(library(readutil), [read_line_to_codes/2,
                                  read_line_to_string/2]).

/** <module> Generate Prolog Unicode map

Create a C structure and access functions for classifying the
characters needed by the Prolog source-text syntax. The mapping is
grounded in [UAX #31](https://www.unicode.org/reports/tr31/) and
backed by the Unicode Character Database files
DerivedCoreProperties.txt, UnicodeData.txt, EastAsianWidth.txt and
PropList.txt.

Two tables are generated. The first, uflags_map, drives the reader
and gives each code point exactly one byte, structured as:

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
    pattern_syntax    ⇐ Sm/Sc/Sk/So/Pc/Pd/Po ∩ Pattern_Syntax
                          (UAX #31 R3, immutable subset; PropList.txt)
    solo              ⇐ Sm/Sc/Sk/So/Pc/Pd/Po \ Pattern_Syntax
                          (the post-3.1 additions; not stable across
                           Unicode versions, hence quoted under
                           write_canonical/1)
    symbol            ⇐ ASCII operator characters (JS path only)
    other / unassigned ⇒ category 0 (treated as stray by the parser)

Width data in bits 4..5 is sourced from EastAsianWidth.txt (UAX #11)
and the general_category property; PL_wcwidth() reads these bits at
runtime.

A case mapping table (see case_runs/1) provides the Unicode simple
uppercase, lowercase and titlecase mappings, again independent of the
locale, where towupper() and towlower() are not.

The second table, uctype_map, holds the POSIX/C character classes
(alpha, alnum, cntrl, digit, graph, lower, print, punct, space,
upper) as a locale independent replacement for <wctype.h>. See
ctype_index/2 for the source mapping. It is a separate table rather
than extra bits in uflags_map because the ctype classification is
much coarser, so its code pages collapse to a single fill value far
more often.

Pair tables (Ps↔Pe and Pi↔Pf) come from BidiMirroring.txt with the
standard curly quote pairs U+2018/U+2019 and U+201C/U+201D added
explicitly (those have Bidi_Mirrored=No and are absent from
BidiMirroring.txt).  The reader uses pl_pair_table to recognise
'<open><close>'/1 paired-delimiter terms — see read_paired_term in
pl-read.c.

Usage:

  1. Get DerivedCoreProperties.txt, UnicodeData.txt,
     EastAsianWidth.txt, BidiMirroring.txt, and PropList.txt from
     the Unicode consortium and copy or link them into this
     directory.
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
           write_codepage(Out, ucp, CP, Map, Options)),
    write_map(Out, Tables, Options),
    write_footer(Out, Options),
    write_ctype_map(Out, Options),
    write_case_table(Out, Options),
    write_decimal_bases(Out, Options),
    write_pair_table(Out, Options).

write_codepage(Out, Prefix, CP, Map, Options) :-
    option(lang(javascript), Options),
    !,
    assertion(length(Map, 256)),
    cp_name(Prefix, CP, CPN),
    format(Out, 'var ~w = "', [CPN]),
    map_chars(Map, Out),
    format(Out, '";~n', []).
write_codepage(Out, Prefix, CP, Map, _Options) :-
    assertion(length(Map, 256)),
    cp_name(Prefix, CP, CPN),
    format(Out, 'static const unsigned char ~w[256] =~n', [CPN]),
    format(Out, '{ ', []),
    map_entries(Map, CP, 0, Out),
    From is CP*256+(256-8),
    To   is From + 7,
    format(Out, '  /* U~|~`0t~16R~4+..U~|~`0t~16R~4+ */~n};~n~n', [From,To]).

%!  cp_name(+Prefix, +CP, -Name) is det.
%
%   Name of the C table holding code page CP. Prefix is `ucp` for the
%   syntax table and `uct` for the ctype table.

cp_name(Prefix, CP, CPN) :-
    format(atom(CPN), '~w0x~|~`0t~16r~2+', [Prefix, CP]).

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
    map_tables(ucp, 0, Last, Tables, Out),
    format(Out, '~N};~n~n', []).

map_tables(_, CP, Last, _, _) :-
    CP > Last,
    !.
map_tables(Prefix, CP, Last, Tables, Out) :-
    (   CP == 0
    ->  true
    ;   0 =:= CP mod 8
    ->  format(Out, ',~n  ', [])
    ;   format(Out, ', ', [])
    ),
    memberchk(table(CP, Map), Tables),
    (   is_list(Map)
    ->  cp_name(Prefix, CP, CPN),
        format(Out, '~w', [CPN])
    ;   format(Out, '~|~tF(0x~16r)~7+', [Map])
    ),
    CP2 is CP + 1,
    map_tables(Prefix, CP2, Last, Tables, Out).


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
    ->  cp_name(ucp, CP, CPN),
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
    format(Out, '/* Each entry in the per-page tables below holds:~n', []),
    format(Out, ' *   bits 0..3  category enum u_category (see below)~n', []),
    format(Out, ' *   bits 4..5  wcwidth+1 (0=invalid, 1=zero, 2=normal, 3=wide)~n', []),
    format(Out, ' *   bits 6..7  reserved~n', []),
    format(Out, ' */~n~n', []),
    format(Out, 'typedef enum~n', []),
    format(Out, '{ ', []),
    write_cat_enum_entries(Out),
    format(Out, '~N} u_category;~n~n', []),
    format(Out, '#define U_CAT_OF(raw) ((u_category)((raw) & 0xF))~n~n', []).

%!  write_cat_enum_entries(+Out) is det.
%
%   Emit the typedef enum body for u_category, one entry per
%   distinct category index. We pass through category_index/2 in
%   index order, dropping duplicates so `U_CAT_OTHER == U_CAT_UNASSIGNED`
%   stays a single enum constant.

write_cat_enum_entries(Out) :-
    findall(Idx-Class,
            category_index(Class, Idx),
            Pairs0),
    sort(0, @=<, Pairs0, Pairs),
    enum_pairs_unique(Pairs, [], Uniq),
    write_enum_entries(Uniq, Out, 0).

%!  enum_pairs_unique(+Pairs, +Acc, -Unique) is det.
%
%   Drops pairs whose Idx already appeared in Acc (keeps first).

enum_pairs_unique([], _, []).
enum_pairs_unique([Idx-Class|T], Seen, Out) :-
    (   memberchk(Idx, Seen)
    ->  enum_pairs_unique(T, Seen, Out)
    ;   Out = [Idx-Class|Out1],
        enum_pairs_unique(T, [Idx|Seen], Out1)
    ).

write_enum_entries([], _, _).
write_enum_entries([Idx-Class|T], Out, I) :-
    upcase_atom(Class, Up),
    (   I == 0
    ->  true
    ;   format(Out, ',~n  ', [])
    ),
    format(Out, 'U_CAT_~w = ~d', [Up, Idx]),
    I2 is I + 1,
    write_enum_entries(T, Out, I2).

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
    format(Out, 'static unsigned char~n', []),
    format(Out, 'uflagsRaw(int code)~n', []),
    format(Out, '{ int cp = (unsigned)code / 256;~n~n', []),
    format(Out, '  if ( cp < UNICODE_MAP_SIZE )~n', []),
    format(Out, '  { const unsigned char *s = uflags_map[cp];~n', []),
    format(Out, '    if ( s < (const unsigned char *)256 )~n', []),
    format(Out, '      return (unsigned char)(uintptr_t)s;~n', []),
    format(Out, '    return s[code&0xff];~n', []),
    format(Out, '  }~n', []),
    format(Out, '  return 0;~n', []),
    format(Out, '}~n~n', []).


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
%   The classification follows the conventions used by POSIX
%   wcwidth() and Markus Kuhn's reference implementation, evaluated
%   against current Unicode data: combining is general category
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
priority_class(_, Code, pattern_syntax) :-
    is_solo_cat(Code),
    is_pattern_syntax(Code).
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

%!  is_pattern_syntax(+Code) is semidet.
%
%   True when Code has the Pattern_Syntax property (UAX #31 R3).
%   This is the immutable subset of syntax-like code points whose
%   classification is guaranteed not to change across Unicode
%   versions. Sourced from PropList.txt.

is_pattern_syntax(Code) :-
    absolute_file_name(unicode('PropList.txt'),
                       File, [access(read)]),
    unicode_property(File, Code, pattern_syntax).

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
              ( Code = Open,  Mate = Close, IsOpen = true
              ; Code = Close, Mate = Open,  IsOpen = false
              )
            ),
            Es0),
    sort(Es0, Entries).

pair(Open, Close) :- bracket_pair(Open, Close).
pair(Open, Close) :- quote_pair(Open, Close).

write_pair_table(_Out, Options) :-
    option(lang(javascript), Options),
    !,
    %% JS path: nothing for now (Stage 6 reader is C-only).
    true.
write_pair_table(Out, _Options) :-
    pair_entries(Entries),
    length(Entries, N),
    format(Out, '#define PL_PAIR_TABLE_SIZE ~d~n~n', [N]),
    format(Out, 'typedef struct~n', []),
    format(Out, '{ int  code;~n', []),
    format(Out, '  int  mate;~n', []),
    format(Out, '  bool is_open;~n', []),
    format(Out, '} pl_pair_entry;~n~n', []),
    format(Out, 'static const pl_pair_entry pl_pair_table[PL_PAIR_TABLE_SIZE] =~n', []),
    format(Out, '{ ', []),
    write_pair_entries(Entries, Out, 0),
    format(Out, '~N};~n~n', []),
    format(Out, '/* Binary-search the pair table for `code`.  Returns the matching~n', []),
    format(Out, ' * delimiter (open <-> close) or 0 if `code` is not a paired~n', []),
    format(Out, ' * bracket / quote.  `*is_open` (if non-NULL) gets true when~n', []),
    format(Out, ' * `code` is the open side, false when it is the close.~n', []),
    format(Out, ' */~n~n', []),
    format(Out, 'static int~n', []),
    format(Out, 'pl_pair_lookup(int code, bool *is_open)~n', []),
    format(Out, '{ int lo = 0, hi = PL_PAIR_TABLE_SIZE - 1;~n~n', []),
    format(Out, '  while ( lo <= hi )~n', []),
    format(Out, '  { int mid = (lo + hi) / 2;~n', []),
    format(Out, '    int c = pl_pair_table[mid].code;~n~n', []),
    format(Out, '    if ( code == c )~n', []),
    format(Out, '    { if ( is_open )~n', []),
    format(Out, '	*is_open = pl_pair_table[mid].is_open;~n', []),
    format(Out, '      return pl_pair_table[mid].mate;~n', []),
    format(Out, '    }~n', []),
    format(Out, '    if ( code < c ) hi = mid - 1; else lo = mid + 1;~n', []),
    format(Out, '  }~n~n', []),
    format(Out, '  return 0;~n', []),
    format(Out, '}~n~n', []).

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
category_index(pattern_syntax,   11).

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
cat_to_flags(11, 0x20).        % pattern_syntax → U_SOLO
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


                 /*******************************
                 *         CTYPE CLASSES        *
                 *******************************/

%!  ctype_index(?Class, ?Index) is nondet.
%
%   The 4-bit POSIX/C character class stored in each uctype_map
%   entry.  Unlike the POSIX classes themselves these classes are a
%   *partition*: every code point has exactly one.  The familiar
%   POSIX classes (alpha, alnum, graph, print, punct, ...) are unions
%   over this partition and are expanded at table-build time into the
%   generated ctype_to_flags[] lookup, so the runtime cost is one
%   table lookup plus one mask.
%
%   Source mapping:
%
%       cntrl             ⇐ general category Cc ∪ Cf, minus White_Space
%       cntrl_blank       ⇐ Cc ∩ White_Space, horizontal: U+0009 only
%       cntrl_space       ⇐ Cc ∩ White_Space, vertical: U+000A..U+000D
%                           and U+0085
%       blank             ⇐ general category Zs; with cntrl_blank this
%                           is the white space that stays *within* a
%                           line, i.e. White_Space minus the line
%                           terminators
%       line_separator    ⇐ general categories Zl and Zp; with
%                           cntrl_space this is the set of Unicode line
%                           terminators, i.e. `space` minus `blank`,
%                           which backs \term{end_of_line}{}
%       digit             ⇐ U+0030..U+0039 only; POSIX requires that the
%                           `digit` class hold exactly the ten ASCII
%                           digits in every locale.  Other decimal
%                           digits (Nd) land in `number`, so they are
%                           `alnum` but not `digit`, which is what glibc
%                           and Darwin do too.
%       upper             ⇐ derived property Uppercase
%                           (Lu + Other_Uppercase)
%       lower             ⇐ derived property Lowercase
%                           (Ll + Other_Lowercase)
%       alpha             ⇐ derived property Alphabetic, minus
%                           upper/lower
%       number            ⇐ Nd/Nl/No that are not Alphabetic
%       mark              ⇐ Mn/Mc/Me that are not Alphabetic
%       sentence_terminal ⇐ P*/S* ∩ Sentence_Terminal (PropList.txt);
%                           the property is Po-only, so these are all
%                           punctuation.  Backs \term{period}{}.
%       punct             ⇐ the remaining P*/S* that are not Alphabetic
%       private           ⇐ Co (private use)
%       other             ⇐ Cn (unassigned) and Cs (surrogate)
%
%   Note that `space` is *not* `graph`, so U+00A0 is white space here
%   while POSIX' requirement that the `space` and `graph` classes be
%   disjoint is still met.  glibc resolves the same conflict the other
%   way around: it calls U+00A0 a graphic character and consequently
%   must deny that it is space.  Darwin agrees with us on U+00A0 but
%   not on U+0085, whose White_Space property it ignores.  Neither
%   libc implements the Unicode White_Space property; this table does.
%
%   `other` is deliberately neither graph nor print: POSIX requires
%   unassigned code points to belong to no class at all.  Private use
%   is printable though — the embedding application supplies the glyph,
%   and both glibc and Darwin agree — and since POSIX defines `punct`
%   as everything printable that is neither alnum nor space, that is
%   the class it ends up in.

ctype_index(other,              0).
ctype_index(cntrl,              1).
ctype_index(cntrl_blank,        2).
ctype_index(cntrl_space,        3).
ctype_index(blank,              4).
ctype_index(line_separator,     5).
ctype_index(digit,              6).
ctype_index(upper,              7).
ctype_index(lower,              8).
ctype_index(alpha,              9).
ctype_index(number,            10).
ctype_index(mark,              11).
ctype_index(sentence_terminal, 12).
ctype_index(punct,             13).
ctype_index(private,           14).

%!  code_ctype(+Code, -Class) is det.
%
%   Class is the unique ctype class of Code.  First-match priority
%   over the priority_ctype/2 clauses; falls through to `other` for
%   unassigned code points and for the C-other categories that carry
%   no classification (Cs, Co).

code_ctype(Code, Class) :-
    priority_ctype(Code, Class),
    !.
code_ctype(_, other).

priority_ctype(Code, cntrl_blank) :-
    general_category(Code, 'Cc'),
    unicode_white_space(Code),
    \+ unicode_line_terminator(Code).
priority_ctype(Code, cntrl_space) :-
    general_category(Code, 'Cc'),
    unicode_white_space(Code).
priority_ctype(Code, cntrl) :-
    general_category(Code, Cat),
    control_cat(Cat).
priority_ctype(Code, blank) :-
    general_category(Code, 'Zs').
priority_ctype(Code, line_separator) :-
    general_category(Code, Cat),
    line_separator_cat(Cat).
priority_ctype(Code, digit) :-
    Code >= 0'0, Code =< 0'9.
priority_ctype(Code, upper) :-
    core_property(Code, uppercase).
priority_ctype(Code, lower) :-
    core_property(Code, lowercase).
priority_ctype(Code, alpha) :-
    core_property(Code, alphabetic).
priority_ctype(Code, number) :-
    general_category(Code, Cat),
    number_cat(Cat).
priority_ctype(Code, mark) :-
    general_category(Code, Cat),
    mark_cat(Cat).
priority_ctype(Code, sentence_terminal) :-
    general_category(Code, Cat),
    solo_cat(Cat),
    unicode_sentence_terminal(Code).
priority_ctype(Code, punct) :-
    general_category(Code, Cat),
    solo_cat(Cat).
priority_ctype(Code, private) :-
    general_category(Code, 'Co').

%!  general_category(+Code, -Cat) is semidet.
%
%   Cat is the Unicode general category of Code.  Unlike plain
%   unicode_property/2 this also answers inside the blocks that
%   UnicodeData.txt abbreviates to a `<..., First>` / `<..., Last>`
%   marker pair: the CJK and Tangut ideographs, the Hangul syllables,
%   the surrogates and the private use areas.  unicode_property/2
%   reports the two end points and nothing in between, which would
%   leave 137468 private use code points unclassified.
%
%   The syntax classification does not need this: the ideograph and
%   syllable blocks reach it through DerivedCoreProperties.txt, which
%   spells its ranges out, and surrogates and private use are `other`
%   there either way.

general_category(Code, Cat) :-
    unicode_property(Code, general_category(Cat0)),
    !,
    Cat = Cat0.
general_category(Code, Cat) :-
    ensure_ucd_ranges,
    ucd_range(Lo, Hi, Cat0),
    Code >= Lo,
    Code =< Hi,
    !,
    Cat = Cat0.

:- dynamic
    ucd_range/3,
    ucd_ranges_loaded/0.

ensure_ucd_ranges :-
    ucd_ranges_loaded,
    !.
ensure_ucd_ranges :-
    absolute_file_name(unicode('UnicodeData.txt'), File, [access(read)]),
    setup_call_cleanup(
        open(File, read, In),
        read_ucd_ranges(In, -),
        close(In)),
    assertz(ucd_ranges_loaded).

read_ucd_ranges(In, Open0) :-
    read_line_to_string(In, Line),
    (   Line == end_of_file
    ->  true
    ;   ucd_range_line(Line, Open0, Open),
        read_ucd_ranges(In, Open)
    ).

ucd_range_line(Line, Open0, Open) :-
    split_string(Line, ";", "", [CodeS,NameS,CatS|_]),
    !,
    (   sub_string(NameS, _, _, 0, ", First>")
    ->  ucd_code(CodeS, Code),
        atom_string(Cat, CatS),
        Open = Cat-Code
    ;   sub_string(NameS, _, _, 0, ", Last>"),
        Open0 = Cat-Lo
    ->  ucd_code(CodeS, Hi),
        assertz(ucd_range(Lo, Hi, Cat)),
        Open = -
    ;   Open = Open0
    ).
ucd_range_line(_, Open, Open).

ucd_code(String, Code) :-
    string_concat("0x", String, HexString),
    number_string(Code, HexString).

control_cat('Cc').      % a C0 or C1 control code
control_cat('Cf').      % a format control character

number_cat('Nd').       % a decimal digit
number_cat('Nl').       % a letterlike numeric character
number_cat('No').       % a numeric character of other type

mark_cat('Mn').         % a nonspacing combining mark
mark_cat('Mc').         % a spacing combining mark
mark_cat('Me').         % an enclosing combining mark

line_separator_cat('Zl').       % a line separator
line_separator_cat('Zp').       % a paragraph separator

%!  unicode_line_terminator(?Code) is nondet.
%
%   The seven code points the Unicode standard defines as terminating a
%   line (see UAX #14, line break class BK, and the definition of
%   `line terminator` in section 5.8 of the standard).  White_Space
%   minus these is the white space that stays within a line, which is
%   what \term{white}{} reports and POSIX calls `blank`.

unicode_line_terminator(0x000A).        % LINE FEED
unicode_line_terminator(0x000B).        % LINE TABULATION
unicode_line_terminator(0x000C).        % FORM FEED
unicode_line_terminator(0x000D).        % CARRIAGE RETURN
unicode_line_terminator(0x0085).        % NEXT LINE
unicode_line_terminator(0x2028).        % LINE SEPARATOR
unicode_line_terminator(0x2029).        % PARAGRAPH SEPARATOR

%!  unicode_white_space(?Code) is nondet.
%!  unicode_sentence_terminal(?Code) is nondet.
%
%   Properties read from PropList.txt.  White_Space is the full property
%   (25 code points), *not* the much smaller Pattern_White_Space used by
%   white_space/1 to drive the `layout` syntax category: the reader must
%   not treat U+00A0 as layout, while the ctype classes must report it
%   as white space.  Sentence_Terminal (170 code points) is Unicode's
%   answer to "ends a sentence": besides \chr{.}, \chr{!} and \chr{?}
%   it holds the Arabic, Armenian, N'Ko, Devanagari, ideographic and
%   fullwidth sentence enders.

unicode_white_space(Code) :-
    prop_list(white_space, Code).

unicode_sentence_terminal(Code) :-
    prop_list(sentence_terminal, Code).

:- dynamic
    prop_list_cache/2,                  % Property, Code
    prop_list_loaded/1.                 % Property

prop_list(Prop, Code) :-
    (   prop_list_loaded(Prop)
    ->  true
    ;   absolute_file_name(unicode('PropList.txt'), File, [access(read)]),
        forall(unicode_property(File, C, Prop),
               assertz(prop_list_cache(Prop, C))),
        assertz(prop_list_loaded(Prop))
    ),
    prop_list_cache(Prop, Code).

%!  core_property(+Code, +Prop) is semidet.
%
%   As unicode_derived_core_property/2, but resolves the file name
%   only once.  The ctype classification asks for up to three derived
%   properties per code point, over 1.1M code points.

:- dynamic
    derived_core_file/1.

core_property(Code, Prop) :-
    (   derived_core_file(File)
    ->  true
    ;   absolute_file_name(unicode('DerivedCoreProperties.txt'),
                           File, [access(read)]),
        assertz(derived_core_file(File))
    ),
    unicode_property(File, Code, Prop).

%!  ctype_flag_bit(?Name, ?Bit) is nondet.
%
%   The POSIX class bits returned by uctypeFlagsW().  Emitted as
%   UC_* #defines and computed per ctype class into ctype_to_flags[].

ctype_flag_bit(alnum, 0x0001).
ctype_flag_bit(alpha, 0x0002).
ctype_flag_bit(blank, 0x0004).
ctype_flag_bit(cntrl, 0x0008).
ctype_flag_bit(digit, 0x0010).
ctype_flag_bit(eol,   0x0020).
ctype_flag_bit(graph, 0x0040).
ctype_flag_bit(lower, 0x0080).
ctype_flag_bit(print, 0x0100).
ctype_flag_bit(punct, 0x0200).
ctype_flag_bit(space, 0x0400).
ctype_flag_bit(sterm, 0x0800).
ctype_flag_bit(upper, 0x1000).

%!  ctype_class_flag(?Class, ?Flag) is nondet.
%
%   Flag is one of the POSIX classes Class belongs to.  The closure
%   rules below are the POSIX definitions: alnum is alpha ∪ the
%   numeric classes, graph is alnum ∪ punct, and print is graph plus
%   the (non-control) white space.

ctype_class_flag(cntrl,             cntrl).
ctype_class_flag(cntrl_blank,       cntrl).
ctype_class_flag(cntrl_space,       cntrl).
ctype_class_flag(sentence_terminal, sterm).
ctype_class_flag(digit,             digit).
ctype_class_flag(upper,             upper).
ctype_class_flag(lower,             lower).
ctype_class_flag(Class,             blank) :- blank_class(Class).
ctype_class_flag(Class,             eol)   :- eol_class(Class).
ctype_class_flag(Class,             space) :- space_class(Class).
ctype_class_flag(Class,             alpha) :- alpha_class(Class).
ctype_class_flag(Class,             alnum) :- alnum_class(Class).
ctype_class_flag(Class,             punct) :- punct_class(Class).
ctype_class_flag(Class,             graph) :- graph_class(Class).
ctype_class_flag(Class,             print) :- print_class(Class).

blank_class(cntrl_blank).
blank_class(blank).

%!  eol_class(?Class) is nondet.
%
%   The classes holding the seven code points Unicode defines as
%   terminating a line.  `space` is the disjoint union of `blank` and
%   `eol`: white space either stays within a line or ends it.

eol_class(cntrl_space).
eol_class(line_separator).

space_class(Class) :- blank_class(Class).
space_class(Class) :- eol_class(Class).

alpha_class(upper).
alpha_class(lower).
alpha_class(alpha).

alnum_class(Class) :- alpha_class(Class).
alnum_class(digit).
alnum_class(number).

punct_class(punct).
punct_class(sentence_terminal).
punct_class(mark).
punct_class(private).

graph_class(Class) :- alnum_class(Class).
graph_class(Class) :- punct_class(Class).

print_class(Class) :- graph_class(Class).
print_class(blank).
print_class(line_separator).

%!  ctype_index_flags(+Index, -Flags) is det.
%
%   Flags is the UC_* bit set for the ctype class stored as Index.
%   Unused enum slots map to 0.

ctype_index_flags(Index, Flags) :-
    (   once(ctype_index(Class, Index))
    ->  findall(Bit,
                ( ctype_class_flag(Class, Name),
                  ctype_flag_bit(Name, Bit)
                ),
                Bits0),
        sort(Bits0, Bits),
        sum_list(Bits, Flags)
    ;   Flags = 0
    ).


                 /*******************************
                 *         CTYPE TABLES         *
                 *******************************/

%!  gen_ctype_tables(-Tables, +Options) is det.
%
%   As gen_tables/2, but each entry holds a ctype class index rather
%   than a syntax category.  The two tables are kept separate rather
%   than widening the uflags entry: the ctype classification is much
%   coarser, so its pages collapse far better under flat_map/2.

gen_ctype_tables(Tables, Options) :-
    findall(table(CP,Map), ctype_table(CP, Map, Options), Tables).

ctype_table(CP, Map, Options) :-
    code_page(CP, Options),
    findall(M, ctype_char(CP, M), Map0),
    flat_map(Map0, Map).

ctype_char(CP, Value) :-
    between(0, 255, I),
    Code is 256*CP+I,
    code_ctype(Code, Class),
    ctype_index(Class, Value).

%!  write_ctype_map(+Out, +Options) is det.
%
%   Emit the u_ctype enum, the UC_* class bits, ctype_to_flags[], the
%   per-page class tables and the uctypeRaw()/uctypeFlagsW() readers.
%   The JavaScript path (SWISH syntax highlighting) has no use for
%   these and is skipped.

write_ctype_map(_Out, Options) :-
    option(lang(javascript), Options),
    !.
write_ctype_map(Out, Options) :-
    gen_ctype_tables(Tables, Options),
    write_ctype_header(Out),
    forall(( member(table(CP, Map), Tables),
             is_list(Map)
           ),
           write_codepage(Out, uct, CP, Map, Options)),
    write_ctype_table_map(Out, Tables, Options),
    write_ctype_footer(Out).

write_ctype_header(Out) :-
    format(Out, '/* POSIX/C character classes.  Each entry in the uctype_map~n', []),
    format(Out, ' * pages below holds a u_ctype class in bits 0..3; bits 4..7~n', []),
    format(Out, ' * are reserved.  The classes partition the code space; the~n', []),
    format(Out, ' * overlapping POSIX classes are recovered by masking the~n', []),
    format(Out, ' * ctype_to_flags[] entry with the UC_* bits.  The data is~n', []),
    format(Out, ' * derived from UnicodeData.txt, DerivedCoreProperties.txt and~n', []),
    format(Out, ' * the White_Space property in PropList.txt, and is therefore~n', []),
    format(Out, ' * locale independent, unlike <wctype.h>.~n', []),
    format(Out, ' */~n~n', []),
    format(Out, 'typedef enum~n', []),
    format(Out, '{ ', []),
    write_ctype_enum_entries(Out),
    format(Out, '~N} u_ctype;~n~n', []),
    forall(ctype_flag_bit(Name, Bit),
           ( upcase_atom(Name, Up),
             format(Out, '#define UC_~w~t~20|0x~|~`0t~16r~3+~n', [Up, Bit])
           )),
    format(Out, '~n#define U_CTYPE_OF(raw) ((u_ctype)((raw) & 0xF))~n~n', []),
    format(Out, 'static const unsigned short ctype_to_flags[16] =~n', []),
    format(Out, '{ ', []),
    write_ctype_to_flags(Out),
    format(Out, '~N};~n~n', []).

write_ctype_enum_entries(Out) :-
    findall(Idx-Class, ctype_index(Class, Idx), Pairs0),
    sort(0, @=<, Pairs0, Pairs),
    enum_pairs_unique(Pairs, [], Uniq),
    write_ctype_enum(Uniq, Out, 0).

write_ctype_enum([], _, _).
write_ctype_enum([Idx-Class|T], Out, I) :-
    upcase_atom(Class, Up),
    (   I == 0
    ->  true
    ;   format(Out, ',~n  ', [])
    ),
    format(Out, 'U_CTYPE_~w = ~d', [Up, Idx]),
    I2 is I + 1,
    write_ctype_enum(T, Out, I2).

write_ctype_to_flags(Out) :-
    numlist(0, 15, Indices),
    write_ctype_flag_entries(Indices, Out).

write_ctype_flag_entries([], _).
write_ctype_flag_entries([I|T], Out) :-
    ctype_index_flags(I, F),
    (   I == 0
    ->  true
    ;   0 =:= I mod 8
    ->  format(Out, ',~n  ', [])
    ;   format(Out, ', ', [])
    ),
    format(Out, '0x~|~`0t~16r~3+', [F]),
    write_ctype_flag_entries(T, Out).

write_ctype_table_map(Out, Tables, Options) :-
    last_unicode_page(DefLast),
    option(last_codepage(Last), Options, DefLast),
    format(Out,
           'static const unsigned char* const uctype_map[UNICODE_MAP_SIZE] =~n',
           []),
    format(Out, '{ ', []),
    map_tables(uct, 0, Last, Tables, Out),
    format(Out, '~N};~n~n', []).

write_ctype_footer(Out) :-
    format(Out, 'static inline unsigned char~n', []),
    format(Out, 'uctypeRaw(int code)~n', []),
    format(Out, '{ int cp = (unsigned)code / 256;~n~n', []),
    format(Out, '  if ( cp < UNICODE_MAP_SIZE )~n', []),
    format(Out, '  { const unsigned char *s = uctype_map[cp];~n', []),
    format(Out, '    if ( s < (const unsigned char *)256 )~n', []),
    format(Out, '      return (unsigned char)(uintptr_t)s;~n', []),
    format(Out, '    return s[code&0xff];~n', []),
    format(Out, '  }~n', []),
    format(Out, '  return 0;~n', []),
    format(Out, '}~n~n', []),
    format(Out, '/* Locale independent replacement for the <wctype.h> classes.~n', []),
    format(Out, ' * Returns the UC_* bit set of `code`, e.g.~n', []),
    format(Out, ' *~n', []),
    format(Out, ' *   if ( uctypeFlagsW(c) & UC_ALPHA ) ...~n', []),
    format(Out, ' */~n~n', []),
    format(Out, 'static inline unsigned short~n', []),
    format(Out, 'uctypeFlagsW(int code)~n', []),
    format(Out, '{ return ctype_to_flags[uctypeRaw(code) & 0xF];~n', []),
    format(Out, '}~n~n', []).


                 /*******************************
                 *         CASE MAPPING         *
                 *******************************/

%!  case_pair(?Code, ?UpperDelta, ?LowerDelta) is nondet.
%!  title_exception(?Code, ?Title) is nondet.
%
%   The Unicode *simple* case mappings, i.e. fields 12..14 of
%   UnicodeData.txt.  Deltas rather than targets, because they repeat:
%   1505 upper and 1488 lower mappings use only 179 distinct deltas,
%   and they come in long runs.
%
%   Simple mappings are one code point to one code point.  The
%   multi-character mappings of SpecialCasing.txt (U+00DF LATIN SMALL
%   LETTER SHARP S to "SS", the ligatures, the Lithuanian and Turkish
%   tailorings) are deliberately not covered: upcase_atom/2 and friends
%   are length preserving.  Titlecase equals uppercase for all but 58
%   code points, so it is stored as an exception list.

:- dynamic
    case_pair/3,
    title_exception/2,
    case_map_loaded/0.

ensure_case_map :-
    case_map_loaded,
    !.
ensure_case_map :-
    findall(C, case_mapped(C), Codes0),
    sort(Codes0, Codes),
    maplist(add_case_map, Codes),
    assertz(case_map_loaded).

case_mapped(C) :-
    unicode_property(C, simple_uppercase_mapping(_)).
case_mapped(C) :-
    unicode_property(C, simple_lowercase_mapping(_)).
case_mapped(C) :-
    unicode_property(C, simple_titlecase_mapping(_)).

add_case_map(Code) :-
    (   unicode_property(Code, simple_uppercase_mapping(U))
    ->  true
    ;   U = Code
    ),
    (   unicode_property(Code, simple_lowercase_mapping(L))
    ->  true
    ;   L = Code
    ),
    UD is U-Code,
    LD is L-Code,
    assertz(case_pair(Code, UD, LD)),
    (   unicode_property(Code, simple_titlecase_mapping(T)),
        T =\= U
    ->  assertz(title_exception(Code, T))
    ;   true
    ).

%!  case_runs(-Runs) is det.
%
%   Runs is a list of run(Start, Count, UpA-LoA, UpB-LoB) covering all
%   code points that have a case mapping.  A run covers the *contiguous*
%   span Start..Start+Count-1; even offsets use the first delta pair and
%   odd offsets the second.  The two pairs make the alternating blocks
%   cheap: Latin Extended-A and friends are laid out as Aa Bb Cc, where
%   every even code point maps down by one and every odd code point maps
%   up by one.  Encoding those as one run rather than two interleaved
%   ones matters twice: it is smaller (290 runs rather than 1370
%   contiguous ones), and the runs stay disjoint, so the table can be
%   searched by bisection.

case_runs(Runs) :-
    ensure_case_map,
    findall(C, case_pair(C, _, _), Codes0),
    msort(Codes0, Codes),
    build_case_runs(Codes, Runs).

build_case_runs([], []).
build_case_runs([C|T0], [run(C, Count, A, B)|T]) :-
    case_deltas(C, A),
    alternating_length(C, A, B0, NAlt),
    uniform_length(C, A, NUni),
    (   NAlt > NUni
    ->  Count = NAlt, B = B0
    ;   Count = NUni, B = A
    ),
    Skip is Count-1,
    drop(Skip, T0, Rest),
    build_case_runs(Rest, T).

case_deltas(C, U-L) :-
    case_pair(C, U, L).

%!  uniform_length(+Start, +Deltas, -Count) is det.
%
%   Count is the number of consecutive code points from Start that all
%   share Deltas.

uniform_length(C, A, N) :-
    C1 is C+1,
    (   case_deltas(C1, A)
    ->  uniform_length(C1, A, N0),
        N is N0+1
    ;   N = 1
    ).

%!  alternating_length(+Start, +DeltasA, -DeltasB, -Count) is det.
%
%   Count is the number of consecutive code points from Start that
%   alternate between DeltasA (even offset) and DeltasB (odd offset).
%   Count is 1 (and DeltasB is DeltasA) when Start+1 does not start an
%   alternation.

alternating_length(C, A, B, N) :-
    C1 is C+1,
    (   case_deltas(C1, B0),
        B0 \== A
    ->  B = B0,
        alt_length(C, A, B, N0),
        N is N0*2
    ;   B = A,
        N = 1
    ).

alt_length(C, A, B, N) :-
    C2 is C+2,
    C3 is C+3,
    (   case_deltas(C2, A),
        case_deltas(C3, B)
    ->  alt_length(C2, A, B, N0),
        N is N0+1
    ;   N = 1
    ).

drop(0, L, L) :- !.
drop(N, [_|T], L) :-
    N1 is N-1,
    drop(N1, T, L).

%!  case_delta_table(-Deltas) is det.
%
%   Deltas is the sorted list of distinct deltas; the runs store indices
%   into it, which keeps a run entry down to 12 bytes.

case_delta_table(Deltas) :-
    ensure_case_map,
    findall(D, (case_pair(_,U,L), (D=U ; D=L)), Ds),
    sort(Ds, Deltas).

write_case_table(_Out, Options) :-
    option(lang(javascript), Options),
    !.
write_case_table(Out, _Options) :-
    case_runs(Runs),
    case_delta_table(Deltas),
    findall(C-T, title_exception(C,T), Titles0),
    msort(Titles0, Titles),
    length(Runs, NRuns),
    length(Deltas, NDeltas),
    length(Titles, NTitles),
    format(Out, '/* Unicode simple case mapping.  See case_runs/1 in~n', []),
    format(Out, ' * Unicode/prolog_syntax_map.pl for the encoding: each run~n', []),
    format(Out, ' * covers a contiguous span of code points and holds two~n', []),
    format(Out, ' * delta pairs, selected by the low bit of the offset into~n', []),
    format(Out, ' * the run.  Runs are disjoint and sorted, so the table is~n', []),
    format(Out, ' * searched by bisection.  Only the *simple* mappings are~n', []),
    format(Out, ' * covered; case conversion here is length preserving.~n', []),
    format(Out, ' */~n~n', []),
    format(Out, '#define PL_CASE_RUNS ~d~n', [NRuns]),
    format(Out, '#define PL_CASE_DELTAS ~d~n', [NDeltas]),
    format(Out, '#define PL_CASE_TITLES ~d~n~n', [NTitles]),
    format(Out, 'typedef struct~n', []),
    format(Out, '{ int	       start;		/* first code point */~n', []),
    format(Out, '  unsigned short count;		/* code points covered */~n', []),
    format(Out, '  unsigned char  upper[2];	/* case_deltas[] index */~n', []),
    format(Out, '  unsigned char  lower[2];~n', []),
    format(Out, '} pl_case_run;~n~n', []),
    format(Out, 'static const int case_deltas[PL_CASE_DELTAS] =~n{ ', []),
    write_case_deltas(Deltas, Out, 0),
    format(Out, '~N};~n~n', []),
    format(Out, 'static const pl_case_run pl_case_runs[PL_CASE_RUNS] =~n{ ', []),
    write_case_runs(Runs, Deltas, Out, 0),
    format(Out, '~N};~n~n', []),
    format(Out, '/* The 58 code points whose titlecase differs from their~n', []),
    format(Out, ' * uppercase; for all others pl_totitle() == pl_toupper().~n', []),
    format(Out, ' */~n~n', []),
    format(Out, 'static const int pl_case_titles[PL_CASE_TITLES][2] =~n{ ', []),
    write_case_titles(Titles, Out, 0),
    format(Out, '~N};~n~n', []),
    write_case_functions(Out).

write_case_deltas([], _, _).
write_case_deltas([H|T], Out, I) :-
    (   I == 0
    ->  true
    ;   0 =:= I mod 8
    ->  format(Out, ',~n  ', [])
    ;   format(Out, ', ', [])
    ),
    format(Out, '~|~t~d~6+', [H]),
    I2 is I+1,
    write_case_deltas(T, Out, I2).

write_case_runs([], _, _, _).
write_case_runs([run(Start,Count,UA-LA,UB-LB)|T], Deltas, Out, I) :-
    (   I == 0
    ->  true
    ;   format(Out, ',~n  ', [])
    ),
    delta_index(Deltas, UA, IUA),
    delta_index(Deltas, LA, ILA),
    delta_index(Deltas, UB, IUB),
    delta_index(Deltas, LB, ILB),
    format(Out, '{ 0x~|~`0t~16r~6+, ~|~t~d~4+, {~|~t~d~4+,~|~t~d~4+}, {~|~t~d~4+,~|~t~d~4+} }',
           [Start, Count, IUA, IUB, ILA, ILB]),
    I2 is I+1,
    write_case_runs(T, Deltas, Out, I2).

delta_index(Deltas, D, I) :-
    nth0(I, Deltas, D),
    !.

write_case_titles([], _, _).
write_case_titles([C-T0|T], Out, I) :-
    (   I == 0
    ->  true
    ;   0 =:= I mod 4
    ->  format(Out, ',~n  ', [])
    ;   format(Out, ', ', [])
    ),
    format(Out, '{0x~|~`0t~16r~4+,0x~|~`0t~16r~4+}', [C, T0]),
    I2 is I+1,
    write_case_titles(T, Out, I2).

write_case_functions(Out) :-
    format(Out,
'\c
static const pl_case_run *
pl_case_run_of(int code)
{ int lo = 0, hi = PL_CASE_RUNS-1;

  while ( lo <= hi )
  { int mid = (lo+hi)/2;
    const pl_case_run *r = &pl_case_runs[mid];

    if ( code < r->start )
      hi = mid-1;
    else if ( code >= r->start + r->count )
      lo = mid+1;
    else
      return r;
  }

  return NULL;
}


/* Unicode simple case conversion.  Code points without a mapping, and
 * anything outside Unicode, are returned unchanged.
 */

static inline int
pl_toupper(int code)
{ const pl_case_run *r = pl_case_run_of(code);

  return r ? code + case_deltas[r->upper[(code - r->start)&1]] : code;
}


static inline int
pl_tolower(int code)
{ const pl_case_run *r = pl_case_run_of(code);

  return r ? code + case_deltas[r->lower[(code - r->start)&1]] : code;
}


static inline int
pl_totitle(int code)
{ int lo = 0, hi = PL_CASE_TITLES-1;

  while ( lo <= hi )
  { int mid = (lo+hi)/2;
    int c = pl_case_titles[mid][0];

    if ( code == c )
      return pl_case_titles[mid][1];
    if ( code < c ) hi = mid-1; else lo = mid+1;
  }

  return pl_toupper(code);
}

', []).
