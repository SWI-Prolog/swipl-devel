:- module(unicode_collate_info,
	  [ write_unicode_collate_map/2	% +File, +Options
	  ]).
:- use_module(library('unicode/unicode_data')).
:- use_module(library('http/dcg_basics')).
:- use_module(library(debug)).
:- use_module(library(option)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module creates a simple map   for  removing diacritics from Unicode
characters and mapping them  to  lowercase.   It  defines  tables  and a
function "int sort_point(int code)". The  sort   points  are  defined as
follows:

	* The high 24-bit contains the character code after mapping to
	  lowercase and removing possible diacritics.
	* Bit 8 is 0 for characters that are mapped from upper to
	  lowercase and 1 for all other characters
	* The low 7 bits encode the removed diacritics.  All removed
	  diacritics are ordered by their Unicode codepoint
	  and numbered.sequentially.

As a result, text will be  ordered   by  its basic character, upper case
before lowercase and text with diacritics after text without.


RUNNING

To   run   thhis   module,    first     check    the   instructions   in
library('unicode/unicode_data') for installing  the   Unicode  datafiles
that are not included in this package.


MOTIVATION

This module is a simple-minded replacement for true Unicode support such
as provided by UCI (http://uci.sourceforge.net/).   The advantage of the
do-it-yourself approach adopted here however is  that it greatly reduces
the footprint and  eliminates  installation   and  maintenance  problems
involved in adopting large `can-do-everything' external libraries.

We believe it  deals  correctly  with   the  Western  languages,  Greek,
Cyrillic and other languages with similar handling of diacritics. 


UPPERCASE

For Prolog it makes more sense  to   map  to uppercase as the cannonical
case. However, we wish to order first on  uppercase and to be able to do
prefix matches we need to search on the  lowest value. Hence, we use the
uppercase version for sorting.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- dynamic
	diacritic/1,			% known diacritics.
	diacritic_code/2.		% +Diacritic, -Code (1..N)

		 /*******************************
		 *	     C TABLES		*
		 *******************************/

%	write_unicode_collate_map(+File, +Options)
%	
%	Options supported are:
%	
%		# first_codepage [0]
%		Code page to start
%		
%		# last_codepage [127]
%		Code page to end.
%		
%		# case(UpperOrLower)
%		Canonise to upper (default) or lower case.

write_unicode_collate_map(File, Options) :-
	open(File, write, Out),
	call_cleanup(write_sort_map(Out, Options),
		     close(Out)).

write_sort_map(Out, Options) :-
	gen_tables(Tables, Options),
	write_header(Out, Options),
	forall(member(table(CP, Map), Tables),
	       write_codepage(Out, CP, Map)),
	write_map(Out, Tables, Options),
	write_footer(Out, Options).

write_codepage(Out, CP, Map) :-
	assertion(length(Map, 256)),
	cp_name(CP, CPN),
	format(Out, 'static const int32_t ~w[256] =~n', [CPN]),
	format(Out, '{ ', []),
	map_entries(Map, 0, Out),
	format(Out, '~N};~n~n', []).

cp_name(CP, CPN) :-
	sformat(CPN, 'ucp0x~|~`0t~16r~2+', [CP]).
	       
map_entries([], _, _).
map_entries([H|T], I, Out) :-
	(   I == 0
	->  true
	;   0 =:= I mod 8
	->  format(Out, ',~n  ', [])
	;   format(Out, ', ', [])
	),
	format(Out, '~|0x~`0t~16r~8+', [H]),
	I2 is I + 1,
	map_entries(T, I2, Out).

write_map(Out, Tables, Options) :-
	option(last_codepage(Last), Options, 127),
	format(Out,
	       'static const int32_t* const ucoll_map[UNICODE_MAP_SIZE] =~n',
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
	(   memberchk(table(CP, _), Tables)
	->  cp_name(CP, CPN),
	    format(Out, '~w', [CPN])
	;   format(Out, '~|~tNULL~7+', [])
	),
	CP2 is CP + 1,
	map_tables(CP2, Last, Tables, Out).


write_header(Out, Options) :-
	option(last_codepage(Last), Options, 127),
	Size is Last+1,
	format(Out,
	       '#ifdef WIN32\n\
		typedef int in32_t;\n\
		#else\n\
		#include <inttypes.h>\n\
		#endif\n\n', []),
	format(Out,
	       '#ifndef NULL\n\
		#define NULL ((void*)0)\n\
		#endif\n\n', []),
	format(Out,
	       '#define UNICODE_MAP_SIZE ~d~n~n', [Size]).

write_footer(Out, Options) :-
	(   memberchk(case(lower), Options)
	->  Add = '+0x80'
	;   Add = ''
	),
	format(Out,
	       'static int\n\
		sort_point(int code)\n\
		{ int cp = code / 256;\n\
		\n  \
		  if ( cp < UNICODE_MAP_SIZE && ucoll_map[cp] )\n    \
		    return ucoll_map[cp][code&0xff];\n\
		\n  \
		  return (code<<8)~w;\n\
		}\n\n', [Add]),
	format(Out,
	       'static int\n\
		sort_pointA(int code)\n\
		{ return ucp0x00[code&0xff];\n\
		}\n\n', []).
	

		 /*******************************
		 *	       TABLES		*
		 *******************************/

gen_tables(Tables, Options) :-
	forall(rm_diacritics(_, _, _), true),
	assign_diacritic_codes,
	findall(table(CP,Map), table(CP, Map, Options), Tables).

table(CP, Map, Options) :-
	option(first_codepage(First), Options, 0),
	option(last_codepage(Last), Options, 127),
	between(First, Last, CP),	
	findall(M, char(CP, M, Options), Map),	% now
	non_empty_map(CP, Map, Options).

char(CP, Value, Options) :-
	between(0, 255, I),
	Code is 256*CP+I,
	(   char_to_code(Code, Value, Options)
	->  true
	;   format('Failed on ~d~n', [Code]),
	    Value is Code<<8
	).
	
char_to_code(Code, Value, Options) :-
	memberchk(case(lower), Options), !,
	(   utolower(Code, Lower),
	    Lower \== Code
	->  Cc = Lower,
	    CFlags = 0x00
	;   Cc = Code,
	    CFlags = 0x80
	),
	assertion(integer(Cc)),
	(   rm_diacritics(Cc, Base, Dia),
	    assertion(integer(Base))
	->  diacritic_code(Dia, DiaV),
	    Value is Base << 8 \/ CFlags \/ DiaV
	;   Value is Cc << 8 \/ CFlags
	).
char_to_code(Code, Value, _Options) :-
	(   utoupper(Code, Upper),
	    Upper \== Code
	->  Cc = Upper,
	    CFlags = 0x80
	;   Cc = Code,
	    CFlags = 0x00
	),
	assertion(integer(Cc)),
	(   rm_diacritics(Cc, Base, Dia),
	    assertion(integer(Base))
	->  diacritic_code(Dia, DiaV),
	    Value is Base << 8 \/ CFlags \/ DiaV
	;   Value is Cc << 8 \/ CFlags
	).

non_empty_map(CP, Map, Options) :-
	(   memberchk(case(lower), Options)
	->  Add is 0x80
	;   Add = 0
	),
	\+ empty_map(Map, 0, CP, Add).

empty_map([], _, _, _).
empty_map([H|T], I, CP, Add) :-
	H =:= ((CP*256+I)<<8) + Add, 
	I2 is I + 1,
	empty_map(T, I2, CP, Add).


		 /*******************************
		 *	 CASE CONVERSION	*
		 *******************************/

utolower(Code, Lower) :-
	unicode_property(Code, simple_lowercase_mapping(Lower)).

utoupper(Code, Upper) :-
	unicode_property(Code, simple_uppercase_mapping(Upper)).


		 /*******************************
		 *	     DIACRITICS		*
		 *******************************/

rm_diacritics(Code, Plain, Dia) :-
	unicode_property(Code, decomposition_type(List)),
	List \== '',
	concat_atom(AtomList, ' ', List),
	to_plain(AtomList, Code, Plain, Dia).

to_plain([Special, PlainA], _, Plain, 0) :-
	special(Special), !,
	atom_hex(PlainA, Plain).
to_plain([PlainA], _Code, Plain, 1) :- !,
	atom_hex(PlainA, Plain).
to_plain(List, Code, Plain, Dia) :-
	maplist(atom_hex, List, Numbers),
	Numbers = [Plain, Dia],
	diacritic(Dia, Code), !.

diacritic(Code, For) :- !,
	unicode_property(Code, canonical_combining_class(Cc)),
	(  Cc > 0
	-> assert_diacritic(Code)
	;  debug(diacritic, '~16r: ~16r: Cc = ~d~n', [For, Code, Cc]),
	   fail
	).

assign_diacritic_codes :-
	retractall(diacritic_code(_,_)),
	findall(D, diacritic(D), Ds),
	sort([0,1|Ds], Sorted),		% 0 and 1 are specials
	assign_codes(Sorted, 1).

assign_codes([], _).
assign_codes([H|T], I) :-
	assert(diacritic_code(H, I)),
	I2 is I + 1,
	assign_codes(T, I2).

assert_diacritic(Code) :-
	diacritic(Code), !.
assert_diacritic(Code) :-
	assert(diacritic(Code)).

atom_hex(Atom, Hex) :-
	atom_codes(Atom, Codes),
	phrase(xinteger(Hex), Codes).

special('<font>').
special('<noBreak>').
special('<initial>').
special('<medial>').
special('<final>').
special('<isolated>').
special('<circle>').
special('<super>').
special('<sub>').
special('<vertical>').
special('<wide>').
special('<narrow>').
special('<small>').
special('<square>').
special('<fraction>').
special('<compat>  ').
