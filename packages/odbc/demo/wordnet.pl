:- use_module(odbc).

open :-
	odbc_connect('WordNet', _,
		     [ user(jan),
		       password(geheim),
		       alias(wordnet),
		       open(once)
		     ]).


:- dynamic
	qid/2.

word(Id, Word) :-
	nonvar(Id),
	qid(word(+, -), Qid), !,
	odbc_execute(Qid, [Id], row(Word)).
word(Id, Word) :-
	nonvar(Word),
	qid(word(-, +), Qid), !,
	odbc_execute(Qid, [Word], row(Id)).
word(Id, Word) :-
	nonvar(Id),
	open,
	odbc_prepare(wordnet,
		     'SELECT (lemma) FROM word WHERE wordno=?',
		     [ integer
		     ],
		     Qid),
	assert(qid(word(+, -), Qid)),
	word(Id, Word).
word(Id, Word) :-
	nonvar(Word),
	open,
	odbc_prepare(wordnet,
		     'SELECT (wordno) FROM word WHERE lemma=?',
		     [ default
		     ],
		     Qid,
		     [ types([integer])
		     ]),
	assert(qid(word(-, +), Qid)),
	word(Id, Word).


		 /*******************************
		 *	      PROFILE		*
		 *******************************/

prof_word(N) :-
	(   between(0, N, _),
	    Id is random(140000),
%	    format('wordno = ~w~n', [Id]),
	    word(Id, _),
	    fail
	;   true
	).

prof_word2(N) :-
	(   between(0, N, _),
	    Id is random(140000),
	    sformat(SQL, 'SELECT * FROM word WHERE wordno=~w', [Id]),
	    db_select('WordNet', SQL, tuple(_, _Word)),
%	    format('~w --> ~w~n', [Id, Word]),
	    fail
	;   true
	).
