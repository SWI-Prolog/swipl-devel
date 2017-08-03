/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2006-2017, University of Amsterdam
			      VU University Amsterdam
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
	    write_syntax_map/2		% +File, +Options
	  ]).
:- use_module(library(main)).
:- use_module(library(option)).
:- use_module(library(debug), [assertion/1]).
:- use_module(library(lists), [member/2]).
:- use_module(library(option), [option/3]).
:- use_module(library('unicode/unicode_data'), [unicode_property/2]).
:- use_module(derived_core_properties, [unicode_derived_core_property/2]).

/** <module> Generate Prolog Unicode map

Create a C structure and  access   functions  for  classification of the
characters we need  for  realising  the   Prolog  syntax.  We  keep  the
definition of the first 128  ASCII   characters.  Characters  above that
needs to be classified as

	* id_start (csymf)
	May start an identifier.

	* id_continue (csym)
	May be used anywhere in identifier

	* uppercase
	We need this to be able to distinquish variables from non-variables.

	* Separators
	We need this for classifying blank space

	* Symbols
	Characters that glue together to form symbols.  These extend the
	default Prolog symbol set: #$&*+-./:<=>?@\^`~

	* lowercase
	<not needed by Prolog>

Usage:

  1. Get DerivedCoreProperties.txt and UnicodeData.txt from the Unicode
     consortium and copy or link them into this directory.
  2. Run `swipl prolog_syntax_map.pl` in this directory, which updates
     `../pl-umap.c`

This module can also create a JavaScript  file, which is used for SWISH.
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
	write_syntax_map(File, Options).

last_unicode_page(LastPage) :-
	LastPage is (0x10ffff + 1) // 0x100.

		 /*******************************
		 *	     C TABLES		*
		 *******************************/

%%	write_syntax_map(+File, +Options)
%
%	Options supported are:
%
%		# first_codepage [0]
%		Code page to start
%
%		# last_codepage [last_unicode_page/1]
%		Code page to end.

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
	write_footer(Out, Options).

write_codepage(Out, CP, Map, Options) :-
	option(lang(javascript), Options), !,
	assertion(length(Map, 256)),
	cp_name(CP, CPN),
	format(Out, 'var ~w = "', [CPN]),
	map_chars(Map, Out),
	format(Out, '";~n', []).
write_codepage(Out, CP, Map, _Options) :-
	assertion(length(Map, 256)),
	cp_name(CP, CPN),
	format(Out, 'static const char ~w[256] =~n', [CPN]),
	format(Out, '{ ', []),
	map_entries(Map, 0, Out),
	format(Out, '~N};~n~n', []).

cp_name(CP, CPN) :-
	format(atom(CPN), 'ucp0x~|~`0t~16r~2+', [CP]).

map_entries([], _, _).
map_entries([H|T], I, Out) :-
	(   I == 0
	->  true
	;   0 =:= I mod 8
	->  format(Out, ',~n  ', [])
	;   format(Out, ', ', [])
	),
	format(Out, '~|0x~`0t~16r~2+', [H]),
	I2 is I + 1,
	map_entries(T, I2, Out).

map_chars([], _).
map_chars([H|T], Out) :-
	format(Out, '\\x~|~`0t~16r~2+', [H]),
	map_chars(T, Out).


write_map(Out, Tables, Options) :-
	option(lang(javascript), Options), !,
	last_unicode_page(DefLast),
	option(last_codepage(Last), Options, DefLast),
	format(Out, 'var uflags_map = [', []),
	js_map_tables(0, Last, Tables, Out),
	format(Out, '];~n~n', []).
write_map(Out, Tables, Options) :-
	last_unicode_page(DefLast),
	option(last_codepage(Last), Options, DefLast),
	format(Out,
	       'static const char* const uflags_map[UNICODE_MAP_SIZE] =~n',
	       []),
	format(Out, '{ ', []),
	map_tables(0, Last, Tables, Out),
	format(Out, '~N};~n~n', []).

map_tables(CP, Last, _, _) :-
	CP > Last, !.
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
	CP > Last, !.
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
	;   format(Out, '0x~16r', [Map])
	),
	CP2 is CP + 1,
	js_map_tables(CP2, Last, Tables, Out).


write_header(Out, Options) :-
	option(lang(javascript), Options), !,
	map_size(Size, Options),
	generated_file(Out),
	format(Out, 'define([], function() {~n', []),
	format(Out, 'var UNICODE_MAP_SIZE~t= ~d;~32|~n', [Size]),
	forall(flag_name(Name, Hex),
	       ( upcase_atom(Name, Up),
		 format(Out, 'var U_~w~t= 0x~16r;~32|~n', [Up, Hex])
	       )),
	format(Out, '~n~n', []).
write_header(Out, Options) :-
	generated_file(Out),
	map_size(Size, Options),
	format(Out, '#define UNICODE_MAP_SIZE ~d~n', [Size]),
	format(Out, '#define F(c) (const char*)(c)~n~n', [Size]),
	forall(flag_name(Name, Hex),
	       ( upcase_atom(Name, Up),
		 format(Out, '#define U_~w~t0x~16r~32|~n', [Up, Hex])
	       )),
	format(Out, '~n~n', []).

map_size(Size, Options) :-
	last_unicode_page(DefLast),
	option(last_codepage(Last), Options, DefLast),
	Size is Last+1.

generated_file(Out) :-
	format(Out, '/*  Generated file.  Do not edit!\n    \c
		         Generated by Unicode/prolog_syntax_map.pl\n\c
		     */~n~n', []).

write_footer(Out, Options) :-
	option(lang(javascript), Options), !,
	format(Out,
'\c
function uflagsW(chr) {
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

return {
  flags:       uflagsW,
  id_start:    function(chr) { return (uflagsW(chr) & U_ID_START)    != 0 },
  id_continue: function(chr) { return (uflagsW(chr) & U_ID_CONTINUE) != 0 },
  uppercase:   function(chr) { return (uflagsW(chr) & U_UPPERCASE)   != 0 },
  separator:   function(chr) { return (uflagsW(chr) & U_SEPARATOR)   != 0 },
  symbol:      function(chr) { return (uflagsW(chr) & U_SYMBOL)      != 0 },
  other:       function(chr) { return (uflagsW(chr) & U_OTHER)       != 0 },
  control:     function(chr) { return (uflagsW(chr) & U_CONTROL)     != 0 }
}
});~n', []).
write_footer(Out, _Options) :-
	format(Out,
	       'static int\n\c
		uflagsW(int code)\n\c
		{ int cp = (unsigned)code / 256;\n\c
		\n  \c
		  if ( cp < UNICODE_MAP_SIZE )\n  \c
		  { const char *s = uflags_map[cp];\n    \c
		    if ( s < (const char *)256 )\n      \c
		      return (int)(intptr_t)s;\n    \c
		    return s[code&0xff];\n  \c
		  }\n  \c
		  return 0;\n\c
		}\n\n', []).


		 /*******************************
		 *	       TABLES		*
		 *******************************/

%%	gen_tables(-Tables, +Options)
%
%	Table is of  the  format  below,   where  CodePage  is  the page
%	(0..255) for 16-bit Unicode and  ValueList   are  the values for
%	each character.
%
%		table(CodePage, ValueList)

gen_tables(Tables, Options) :-
	findall(table(CP,Map), table(CP, Map, Options), Tables).

table(CP, Map, Options) :-
	last_unicode_page(DefPage),
	option(first_codepage(First), Options, 0),
	option(last_codepage(Last), Options, DefPage),
	between(First, Last, CP),
	findall(M, char(CP, M, Options), Map0),
	flat_map(Map0, Map).

char(CP, Value, _Options) :-
	between(0, 255, I),
	Code is 256*CP+I,
	code_flags(Code, Value).

code_flags(Code, Value) :-
	findall(F, flag(Code, F), Fs),
	or(Fs, Value).

or([], 0).
or([H|T], F) :-
	or(T, F0),
	F is F0 \/ H.

flag(Code, Flag) :-
	flag_name(Name, Flag),
	(   Code < 256
	->  predef_code_flag(Code, Name)
	;   code_flag(Code, Name)
	).

flag_name(id_start,    0x01).
flag_name(id_continue, 0x02).
flag_name(uppercase,   0x04).
flag_name(separator,   0x08).
flag_name(symbol,      0x10).
flag_name(other,       0x20).
flag_name(control,     0x40).

%!	predef_code_flag(+C, ?Class) is nondet.
%
%	Fill code page 0 (0..255) using   predefined categories. This is
%	used for consistency of the JavaScript classifier.

predef_code_flag(C, id_start) :-
	(   code_type(C, prolog_atom_start)
	;   code_type(C, prolog_var_start)
	).
predef_code_flag(C, id_continue) :-
	code_type(C, prolog_identifier_continue).
predef_code_flag(C, symbol) :-
	code_type(C, prolog_symbol).
predef_code_flag(C, uppercase) :- unicode_derived_core_property(C, uppercase).
predef_code_flag(C, other) :-
	unicode_property(C, general_category(Cat)),
	other_cat(Cat).
predef_code_flag(C, control) :-
	unicode_property(C, general_category(Cat)),
	control_cat(Cat).
predef_code_flag(C, unassigned) :-
	\+ unicode_property(C, general_category(_)).


code_flag(C, id_start) :-    unicode_derived_core_property(C, id_start).
code_flag(C, id_continue) :- unicode_derived_core_property(C, id_continue).
code_flag(C, uppercase) :-   unicode_derived_core_property(C, uppercase).
code_flag(C, separator) :-
	unicode_property(C, general_category(Cat)),
	sep_cat(Cat).
code_flag(C, symbol) :-
	unicode_property(C, general_category(Cat)),
	symbol_cat(Cat).
code_flag(C, other) :-
	unicode_property(C, general_category(Cat)),
	other_cat(Cat).
code_flag(C, control) :-
	unicode_property(C, general_category(Cat)),
	control_cat(Cat).
code_flag(C, unassigned) :-
	\+ unicode_property(C, general_category(_)).

% See http://www.unicode.org/reports/tr44/#Property_Values

sep_cat('Zs').		% a space character (of various non-zero widths)
sep_cat('Zl').		% U+2028 LINE SEPARATOR only
sep_cat('Zp').		% U+2029 PARAGRAPH SEPARATOR only

symbol_cat('Sm').	% a symbol of primarily mathematical use
symbol_cat('Sc').	% a currency sign
symbol_cat('Sk').	% a non-letterlike modifier symbol
symbol_cat('So').	% a symbol of other type
symbol_cat('Pc').	% a connecting punctuation mark, like a tie
symbol_cat('Pd').	% a dash or hyphen punctuation mark
symbol_cat('Ps').	% an opening punctuation mark (of a pair)
symbol_cat('Pe').	% a closing punctuation mark (of a pair)
symbol_cat('Pi').	% an initial quotation mark
symbol_cat('Pf').	% a final quotation mark
symbol_cat('Po').	% a punctuation mark of other type

other_cat('No').	% a numeric character of other type
other_cat('Me').	% an enclosing combining mark

control_cat('Cc').	% a C0 or C1 control code
control_cat('Cf').	% a format control character
control_cat('Cs').	% a surrogate code point
control_cat('Co').	% a private-use character
control_cat('Cn').	% a reserved unassigned code point or a noncharacter


flat_map(Map0, Value) :-
	sort(Map0, [Value]), !.
flat_map(Map, Map).
