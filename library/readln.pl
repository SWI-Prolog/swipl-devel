/*  $Id$

    Read a sentence from the current input stream and convert it
    into a list of atoms and numbers.
    Letters(A-Z, a-z) are converted to atoms
    Digits (0-9) (and a '.' if a real number) are converted to numbers
	Some obscure 'rounding' is done, so you have most of the times
	only 6 significant digits with an exponent part. (This is caused
	by the system predicate 'name'. If you want looonnnggg numbers
	then define digits as parts of words).
	(N.B. reals work only if '.' is not defined as 'stop-char' but
		'escape' will work in this case)

    The reader is >>flexible<<, you can define yourself:
	- the character on which reading will stop
		(this character is escapable with \
		 to read a \ type this character twice!!)
	- the character(s) that make up a word (execpt the
	  characters A-Z, a-z that always make up words!!
	  and (real)-numbers that always are grouped together!!)
	- whether you want conversion of uppercase letters to
	  lowercase letters.

    readln/1
	The default setting for readln/1 is
		- read up till newline
		- see underscore('_') and numbers 0-9 as part of words
		- make lowercase

        - If nothing is read readln/1 succeeds with []
        - If an end_of_file is read readln/1 succeeds with [..|end_of_file]


    readln/5
	This predicate gives you the flexibility.
	It succeeds with arg1 = list of word&atoms
			 arg2 = Ascii code of last character
				(but 'end_of_file' in case of ^D).
	To change one or more of the defaults you have to
	instantiate argument3 and/or argument4 and/or argument5.
	 !! Uninstantiated arguments are defaulted !!
	- stop character(s):
		instantiate argument 3 with the list of ASCII code's
		of the desired stop characters (Note: you can also
		say: ".!?", what is equivalent to [46,33,63] ).
	- word character(s):
		instantiate argument 4 with the list of ASCII code's
		of the desired word-part characters (Note: wou can also
		say: "", what is equivalent to [] ; i.e. no extra
		characters).
	- lowercase conversion:
		instantiate argument 5 with lowercase


Main predicates provided:

    readln(P)		- Read a sentence up till NewLine and
			  unify <P> with the list of atoms/numbers
			  (identical to:
				 readln(P, [10],"_01213456789",uppercase).)
    readln(P, LastCh)   - idem as above but the second argument is unified
			  with the last character read (the ascii-code for
			  the stop-character or 'end_of_file')
    readln(P, LastCh, Arg1, Arg2, Arg3)
			- idem as above but the default setting is changed
			  for the instantiated args:
			  Arg1: List of stop characters
			  Arg2: List of word_part characters
			  Arg3: uppercase/lowercase conversion

Examples:
	read_sentence( P,Case ) :-
		readln( P,_,".!?","_0123456789",Case ).

	read_in( P ) :-				% with numbers as separate
		readln( P,Eof,_,"", _ ).	% entities.

	read_atom( A ) :-			% stop on newline,
		readln( A,_,_," ",_).		% space is part of word


   Author: Wouter Jansweijer
   Date: 26 april 1985

******************************************************************************/

:- module( readln,[readln/1,readln/2,readln/5] ).

:- style_check( +dollar ).

readln( Read ) :-			% the default is read up to EOL
	$readln( Line,LastCh,[10],"_0123456789",uppercase ),
	(   LastCh \== end_of_file,
		Read = Line
	|   append( Line,[end_of_file],Read )
	), !.

readln(Read, LastCh):-
	$readln(Read, LastCh, [10], "_0123456789", uppercase).

readln( P,EOF,StopChars,WordChars,Case ) :-
	(   var( StopChars ),
		Arg1 = [10]
	|   Arg1 = StopChars
	),
	(   var( WordChars ),
		Arg2 = "01234567890_"
	|   Arg2 = WordChars
	),
	(   var( Case ),
		Arg3 = lowercase
	|   Arg3 = Case
	),  !,
	$readln( P,EOF,Arg1,Arg2,Arg3 ).

$readln( P,EOF,StopChars,WordChars,Case ) :-
	$initread( L,EOF,StopChars ),
	$blanks( L,LL ), !,
	$words( P,LL,[],options(WordChars,Case) ), !.

$initread( S,EOF,StopChars ) :- 
	get0(K),
	$readrest( K,S,EOF,StopChars ).

$readrest( -1,[],end_of_file,_ ) :- !.
$readrest( 0'\,[K1|R],EOF,StopChars ) :-
	get0( K1 ),					% skip it, take next char
	get0( K2 ),
	$readrest( K2,R,EOF,StopChars ).
$readrest( K,[K],K,StopChars ) :-			% the stop char(s)
	member( K,StopChars ), !.
$readrest( K,[K|R],EOF,StopChars ) :-			% the normal case
	get0( K1 ),
	$readrest( K1,R,EOF,StopChars ).

$words( [W|Ws],S1,S4,Options ) :-
	$word( W,S1,S2,Options ), !,
	$blanks( S2,S3 ),
	$words( Ws,S3,S4,Options ).
$words( [],S1,S2,_ ) :-
	$blanks( S1,S2), !.
$words( [],S,S,_ ).

$word( N,[46|S1],S3,_ ) :-		% the dot can be in the beginning of
	$basic_num( N1,S1,S2 ),	!,	% a real number.
	$basic_nums( Rest,S2,S3,dot ),	% N.B. only ONE dot IN a number !!
	name( N,[48,46,N1|Rest] ).	% i.e '0.<number>'
$word( N,S0,S2,_ ) :-
	$basic_num( N1,S0,S1 ), !,
	$basic_nums( Rest,S1,S2,_ ),
	name( N,[N1|Rest] ).
$word( W,S0,S2,Options ) :-
	$basic_char( C1,S0,S1,Options ), !,
	$basic_chars( Rest,S1,S2,Options ),
	name( W,[C1|Rest] ).
$word( P,[C|R],R,_ ) :-
	name( P,[C] ), !.

$basic_chars( [A|As],S0,S2,Options ) :-
   $basic_char( A,S0,S1,Options ), !,
   $basic_chars( As,S1,S2,Options ).
$basic_chars( [],S,S,_ ).

$basic_nums( [46,N|As],[46|S1],S3,Dot ) :- % a dot followed by at least one digit
   var( Dot ),				   % but not found a dot already
   $basic_num( N,S1,S2 ), !,
   $basic_nums( As,S2,S3,dot ).
$basic_nums( [A|As],S0,S2,Dot ) :-
   $basic_num( A,S0,S1 ), !,
   $basic_nums( As,S1,S2,Dot ).
$basic_nums( [],S,S,_ ).

$blanks( [C|S0],S1 ) :-
   $blank( C ), !,
   $blanks( S0,S1 ).
$blanks( S,S ).

/* Basic Character types that form $words together */

$basic_char( A,[C|S],S,options(WordChars,Case) ) :-
	$lc( C,A,WordChars,Case ).

$basic_num( N,[N|R],R ) :-
	between( 0'0,0'9,N ).

$blank(X) :-
	X =< 32.

$lc( X,X1,_,Case ) :-	
	between( 0'A,0'Z,X ), !,
	$fix_case( Case,X,X1 ).
$lc( X,X,_,_ ) :-
	between( 0'a,0'z,X ), !.
$lc( X,X,WordChars,_ ) :-
	memberchk( X,WordChars ).

$fix_case( lowercase,U,L ) :- !,
	plus( U,32,L ).
$fix_case( _,C,C ).
