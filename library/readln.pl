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
				(but '-1' in case of ^D).
	To change one or more of the defaults you have to
	instantiate argument3 and/or argument4 and/or argument5.
	 !! Uninstantiated arguments are defaulted !!
	- stop character(s):
		instantiate argument 3 with the list of ASCII code's
		of the desired stop characters (Note: you can also
		say: ".!?", what is equivalent to [46,33,63]).
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
			  the stop-character or -1)
    readln(P, LastCh, Arg1, Arg2, Arg3)
			- idem as above but the default setting is changed
			  for the instantiated args:
			  Arg1: List of stop characters
			  Arg2: List of word_part characters
			  Arg3: uppercase/lowercase conversion

Examples:
	read_sentence(P,Case) :-
		readln(P,_,".!?","_0123456789",Case).

	read_in(P) :-				% with numbers as separate
		readln(P,Eof,_,"", _).	% entities.

	read_atom(A) :-			% stop on newline,
		readln(A,_,_," ",_).		% space is part of word


   Author: Wouter Jansweijer
   Date: 26 april 1985

   Modified: Jan Wielemaker
   Date: 19 feb 2001

   Modernised a bit and fixed some end_of_file/-1 issues.  As we have modules
   since a while I removed the ugly $ stuff :-)
******************************************************************************/

:- module(readln,
	  [ readln/1,			% -Line
	    readln/2,			% -Line, +EOL
	    readln/5			% See above
	  ]).

readln(Read) :-			% the default is read up to EOL
	rl_readln(Line, LastCh, [10], "_0123456789", uppercase),
	(   LastCh == -1
	->  append(Line,[end_of_file], Read)
	;   Read = Line
	).

readln(Read, LastCh):-
	rl_readln(Read, LastCh, [10], "_0123456789", uppercase).

readln(P, EOF, StopChars, WordChars, Case) :-
	(   var(StopChars)
	->  Arg1 = [10]
	;   Arg1 = StopChars
	),
	(   var(WordChars)
	->  Arg2 = "01234567890_"
	;   Arg2 = WordChars
	),
	(   var(Case)
	->  Arg3 = lowercase
	;   Arg3 = Case
	),
	rl_readln(P, EOF, Arg1, Arg2, Arg3).

rl_readln(P, EOF, StopChars, WordChars, Case) :-
	rl_initread(L, EOF, StopChars),
	rl_blanks(L, LL), !,
	rl_words(P, LL,[], options(WordChars, Case)), !.

rl_initread(S, EOF, StopChars) :- 
	get0(K),
	rl_readrest(K, S, EOF, StopChars).

rl_readrest(-1, [], end_of_file, _) :- !.
rl_readrest(0'\, [K1|R], EOF, StopChars) :-
	get0(K1),			% skip it, take next char
	get0(K2),
	rl_readrest(K2, R, EOF, StopChars).
rl_readrest(K, [K], K, StopChars) :-	% the stop char(s)
	member(K, StopChars), !.
rl_readrest(K, [K|R], EOF, StopChars) :-	% the normal case
	get0(K1),
	rl_readrest(K1, R, EOF, StopChars).

rl_words([W|Ws], S1, S4, Options) :-
	rl_word(W, S1, S2, Options), !,
	rl_blanks(S2, S3),
	rl_words(Ws, S3, S4, Options).
rl_words([], S1, S2, _) :-
	rl_blanks(S1, S2), !.
rl_words([], S, S, _).

rl_word(N, [46|S1], S3, _) :-		% the dot can be in the beginning of
	rl_basic_num(N1, S1, S2),	!,	% a real number.
	rl_basic_nums(Rest, S2, S3, dot),	% only ONE dot IN a number !!
	name(N,[48, 46, N1|Rest]).	% i.e '0.<number>'
rl_word(N, S0, S2, _) :-
	rl_basic_num(N1, S0, S1), !,
	rl_basic_nums(Rest, S1, S2, _),
	name(N,[N1|Rest]).
rl_word(W, S0, S2, Options) :-
	rl_basic_char(C1, S0, S1, Options), !,
	rl_basic_chars(Rest, S1, S2, Options),
	name(W, [C1|Rest]).
rl_word(P,[C|R], R, _) :-
	name(P, [C]), !.

rl_basic_chars([A|As], S0, S2, Options) :-
	rl_basic_char(A, S0, S1, Options), !,
	rl_basic_chars(As, S1, S2, Options).
rl_basic_chars([], S, S, _).

rl_basic_nums([46,N|As], [46|S1], S3, Dot) :- % a dot followed by >= one digit
	var(Dot),			% but not found a dot already
	rl_basic_num(N, S1, S2), !,
	rl_basic_nums(As, S2, S3, dot).
rl_basic_nums([A|As], S0, S2, Dot) :-
	rl_basic_num(A, S0, S1), !,
	rl_basic_nums(As, S1, S2, Dot).
rl_basic_nums([], S, S, _).

rl_blanks([C|S0], S1) :-
	rl_blank(C), !,
	rl_blanks(S0, S1).
rl_blanks(S, S).

/* Basic Character types that form rl_words together */

rl_basic_char(A, [C|S], S, options(WordChars, Case)) :-
	rl_lc(C, A, WordChars, Case).

rl_basic_num(N, [N|R], R) :-
	code_type(N, digit).

rl_blank(X) :-
	code_type(X, space).

rl_lc(X, X1, _, Case) :-	
	code_type(X, upper), !,
	rl_fix_case(Case, X, X1).
rl_lc(X, X, _, _) :-
	code_type(X, lower).
rl_lc(X, X, WordChars, _) :-
	memberchk(X, WordChars).

rl_fix_case(lowercase, U, L) :- !,
	code_type(L, lower(U)).
rl_fix_case(_, C, C).
