/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    jan@swi.psy.uva.nl

    Purpose: Character classification
*/

:- module( ctypes,[
	is_alnum/1,
	is_alpha/1,
	is_ascii/1,
	is_cntrl/1,
	is_csym/1,
	is_csymf/1,
	is_digit/1,
	is_digit/3,
	is_endfile/1,
	is_endline/1,
	is_graph/1,
	is_lower/1,
	is_newline/1,
	is_newpage/1,
	is_paren/2,
	is_period/1,
	is_print/1,
	is_quote/1,
	is_space/1,
	is_upper/1,
	is_white/1,
	to_lower/2,
	to_upper/2] ).

:- style_check( +dollar ).

%	This file is a modified version  of  the  corresponding  Quintus
%	Prolog  library  file.   Due  to the fact that between/3 is very
%	fast in SWI-Prolog this version is based on  this  predicate  as
%	musc as possible.  Meta calls are avoided to gain performance.

%	title:   Character classification
%	library: ctypes
%	Ctypes is a Quintus compatible  library  package  for  character
%	classification.   All  predicates  of  this  library package are
%	logical: they can both be used  to  verify  a  character  is  if
%	specified  type as for generating all successive elements of the
%	category.

%	is_alnum( ?C )
%	Succeeds if `C' is a letter or digit.

is_alnum( C ) :- between( 0'a,0'z,C ).
is_alnum( C ) :- between( 0'A,0'Z,C ).
is_alnum( C ) :- between( 0'0,0'9,C ).

%	is_alpha( ?C )
%	Succeeds if `C' is an upper- or lower case character

is_alpha( C ) :- between( 0'a,0'z,C ).
is_alpha( C ) :- between( 0'A,0'Z,C ).

%	is_ascii( ?C )
%	Succeeds if `C' is an ascii character (e.g. 0 <= `C' <= 127)

is_ascii( C ) :- between( 0,127,C ).

%	is_cntrl( ?C )
%	Succeeds is `C' is the delete character or a control character.
%	Fails on `C' is end of file.

is_cntrl( C ) :- between( 0,31,C ).
is_cntrl( 127 ).

%	is_csym( ?C )
%	Succeeds if `C' is a letter, underscore or digit.

is_csym( C ) :- between( 0'a,0'z,C ).
is_csym( 0'_ ).
is_csym( C ) :- between( 0'A,0'Z,C ).
is_csym( C ) :- between( 0'0,0'9,C ).

%	is_csymf( ?C )
%	Succeeds if `C' is a letter, underscore.

is_csymf( C ) :- between( 0'a,0'z,C ).
is_csymf( 0'_ ).
is_csymf( C ) :- between( 0'A,0'Z,C ).

%	is_digit( ?C )
%	Succeeds if `C' is a digit.

is_digit( C ) :- between( 0'0,0'9,C ).

%	is_digit( ?C,?Base,?Weight )
%	Succeeds if `C' is a digit using `Base'  as  base  and  `Weight'
%	represents its value.

is_digit( C,Base,Weight ) :-
	between( 2,36,Base ),
	succ( X,Base ),
	between( 0,X,Weight ),
	$is_digit( C,Weight ).

$is_digit( C,Weight ) :-
	Weight < 10, !,
	plus( Weight,0'0,C ).
$is_digit( C,Weight ) :-
	plus( Weight,87,C ), !.		/* `a`-10 */
$is_digit( C,Weight ) :-
	plus( Weight,55,C ).		/* `A`-10 */

%	is_endfile( ?C )
%	Succeeds if `C' is the end of file character as returned by get0/1.

is_endfile( -1 ).

%	is_endline( ?C )
%	Succeeds if `C' ends a line.  All control characters except  for
%	the  delete character and tab characater end a line.  The end of
%	file character ends a line as well.

is_endline( C ) :-
	C < 32,
	C \== 9.			/* ^I */

%	is_graph( ?C )
%	Succeeds is `C' is visible when you print it.

is_graph( C ) :- between( 33,126,C ).

%	is_lower( ?C )
%	Succeeds if `C' is a lower case character.

is_lower( C ) :- between( 0'a,0'z,C ).

%	is_newline( ?C )
%	Succeeds if `C' is the newline character.

is_newline( 10 /* ^J */ ).

%	is_newpage( ?C )
%	Succeeds if `C' starts a new page.

is_newpage( 12 /* ^L */ ).

%	is_paren( ?C1,?C2 )
%	Succeeds if `C1' is an openings parenthesis (`(', `[' or `{') and
%	`C2' is the corresponding closing paranthesis.

is_paren( 0'(,0') ).
is_paren( 0'[,0'] ).
is_paren( 0'{,0'} ).

%	is_period( ?C )
%	Succeeds if `C' ends a sentence (`.', `?' or `!').

is_period( 0'. ).
is_period( 0'? ).
is_period( 0'! ).

%	is_print( ?C )
%	Succeeds if `C' is a printable character. See also is_graph/1.

is_print( C ) :-
	between( 32,126,C ).

%	is_punct( ?C )
%	Succeeds if `C' is a printable character, but not an `alnum' 
%	(see is_alnum/1).

is_punct( C ) :- between( 32,  47, C).	% between space and digits
is_punct( C ) :- between( 58,  64, C).	% between digits and uppers
is_punct( C ) :- between( 91,  96, C).	% between uppers and lowers
is_punct( C ) :- between(123, 126, C).	% between lowers and delete

%	is_quote( ?C )
%	Succeeds if `C' is a quote character ( `'" ).

is_quote(39 /* ' */).
is_quote(34 /* " */).
is_quote(96 /* ` */).

%	is_space( ?C )
%	Succeeds if `C' is a layout character

is_space(32).			% ` `
is_space(10).			% `\n`
is_space( 9).			% `\t`
is_space(11).			% `\v`
is_space(12).			% `\f`
is_space(13).			% `\r`

%	is_upper( ?C )
%	Succeeds if `C' is an upper case letter.

is_upper( C ) :- between( 0'A,0'Z,C ).

%	is_white( ?C )
%	Succeeds if `C' is a layout character that does not start a newline.

is_white( 32 ).			% space
is_white(  9 ).			% tab

%	to_lower( ?U,?L )
%	Succeeds  if  `U'  is  upper  case  character  and  `L'  is  the
%	corresponding lower case character or `U' is an ascii character,
%	but not an upper case letter and `L' is equal to `U'.

to_lower( U,L ) :-
	between( 0,127,U ),
	$to_lower( U,L ).

$to_lower( U,L ) :-
	between( 0'A,0'Z,U ), !,
	plus( U,32,L ).
$to_lower( C,C ).

%	to_upper( ?L,?U )
%	Succeeds  if  `L'  is  lower  case  character  and  `U'  is  the
%	corresponding upper case character or `L' is an ascii character,
%	but not an lower case letter and `L' is equal to `U'.

to_upper( L,U ) :-
	between( 0,127,L ),
	$to_upper( L,U ).

$to_upper( L,U ) :-
	between( 0'a,0'z,L ), !,
	plus( U,32,L ).
$to_upper( C,C ).
