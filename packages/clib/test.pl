:- module(test_nlp,
	  [ run_tests/0,
            run_tests/1
	  ]).

:- asserta(user:file_search_path(foreign, '.')).
:- asserta(user:file_search_path(library, '.')).
:- asserta(user:file_search_path(library, '../plunit')).

:- use_module(library(plunit)).
:- use_module(library(crypt)).
:- use_module(library(sha)).

:- begin_tests(crypt).

test(default, []) :-
	Passwd = "My password",
	crypt(Passwd, E),
	ground(E),
	crypt(Passwd, E).
test(md5, []) :-
	Passwd = "My password",
	append("$1$", _, E),
	crypt(Passwd, E),
	ground(E),
	crypt(Passwd, E).

:- end_tests(crypt).


:- begin_tests(sha).

test(sha1, [true(Hash=[136, 67, 215, 249, 36, 22, 33, 29,
		      233, 235, 185, 99, 255, 76, 226,
		      129, 37, 147, 40, 120])]) :-
	sha_hash(foobar, Hash, []).
test(sha1, [true(Hash=[136, 67, 215, 249, 36, 22, 33, 29,
		      233, 235, 185, 99, 255, 76, 226, 
		      129, 37, 147, 40, 120])]) :-
	sha_hash(foobar, Hash, [algorithm(sha1)]).
test(sha256, [true(Hash=[195, 171, 143, 241, 55, 32, 232, 173,
			144, 71, 221, 57, 70, 107, 60, 137, 
			116, 229, 146, 194, 250, 56, 61, 74,
			57, 96, 113, 76, 174, 240, 196, 242])]) :-
	sha_hash(foobar, Hash, [algorithm(sha256)]).
test(hmac, [true(Hash=[80, 49, 254, 61, 152, 156, 109, 21,
		      55, 160, 19, 250, 110, 115, 157, 162, 
		      52, 99, 253, 174])]) :-
	hmac_sha(key, data, Hash, [algorithm(sha1)]).
test(hmac, [true(Hash=[80, 49, 254, 61, 152, 156, 109, 21,
		      55, 160, 19, 250, 110, 115, 157, 162,
		      52, 99, 253, 174, 195, 183, 1, 55, 216,
		      40, 227, 106, 206, 34, 27, 208])]) :-
	hmac_sha(key, data, Hash, [algorithm(sha256)]).

:- end_tests(sha).


:- begin_tests(wiki_sha).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Test-cases from http://en.wikipedia.org/wiki/SHA-1
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%%	hash_to_atom(+Hash, -Atom)
%
%	Translates a hash into representation used  in Wikipedia page on
%	SHA-1 hashes.

hash_to_atom(Hash, Atom) :-
	phrase(hash_to_ascii(Hash), Codes),
	atom_codes(Atom, Codes).

hash_to_ascii([]) -->
	[].
hash_to_ascii([B1,B2,B3,B4|T]) -->
	hex_byte(B1),
	hex_byte(B2),
	hex_byte(B3),
	hex_byte(B4),
	(   { T == [] }
	->  []
	;   " ",
	    hash_to_ascii(T)
	).
	      
hex_byte(Byte) -->
	{ High is (Byte>>4) /\ 0xf,
	  Low  is Byte /\ 0xf
	},
	hex(High),
	hex(Low).

hex(Digit) -->
	{ code_type(Code, xdigit(Digit))
	},
	[Code].
	  
test(sha1, [true(Atom='2fd4e1c6 7a2d28fc ed849ee1 bb76e739 1b93eb12')]) :-
	sha_hash("The quick brown fox jumps over the lazy dog", Hash, [algorithm(sha1)]),
	hash_to_atom(Hash, Atom).
test(sha1, [true(Atom='da39a3ee 5e6b4b0d 3255bfef 95601890 afd80709')]) :-
	sha_hash('', Hash, [algorithm(sha1)]),
	hash_to_atom(Hash, Atom).
test(sha256, [true(Atom='d7a8fbb3 07d78094 69ca9abc b0082e4f 8d5651e4 6d3cdb76 2d02d0bf 37c9e592')]) :-
	sha_hash("The quick brown fox jumps over the lazy dog", Hash, [algorithm(sha256)]),
	hash_to_atom(Hash, Atom).
test(sha256, [true(Atom='e3b0c442 98fc1c14 9afbf4c8 996fb924 27ae41e4 649b934c a495991b 7852b855')]) :-
	sha_hash('', Hash, [algorithm(sha256)]),
	hash_to_atom(Hash, Atom).
test(sha512, [true(Atom='07e547d9 586f6a73 f73fbac0 435ed769 51218fb7 d0c8d788 a309d785 436bbb64 \
			 2e93a252 a954f239 12547d1e 8a3b5ed6 e1bfd709 7821233f a0538f3d b854fee6')]) :-
	sha_hash("The quick brown fox jumps over the lazy dog", Hash, [algorithm(sha512)]),
	hash_to_atom(Hash, Atom).
test(sha512, [true(Atom='cf83e135 7eefb8bd f1542850 d66d8007 d620e405 0b5715dc 83f4a921 d36ce9ce \
			 47d0d13c 5d85f2b0 ff8318d2 877eec2f 63b931bd 47417a81 a538327a f927da3e')]) :-
	sha_hash('', Hash, [algorithm(sha512)]),
	hash_to_atom(Hash, Atom).

:- end_tests(wiki_sha).



