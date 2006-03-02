:- module(rdf_litindex,
	  [ rdf_literal_index/1,	% +Options
	    rdf_find_literals/2		% +Spec, -ListOfLiterals
	  ]).
:- use_module(rdf_db).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(porter_stem)).
:- use_module(library(double_metaphone)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module finds literals of the RDF database based on stemming and
being flexible to ordering of tokens.  
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- dynamic
	literal_map/2,			% Type, -Map
	new_token/1,			% Hook
	setting/1.
:- volatile
	literal_map/2.

setting(index_tokens(true)).
setting(index_stems(true)).
setting(index_metaphone(true)).
setting(verbose(true)).

rdf_literal_index([]).
rdf_literal_index([H|T]) :-
	set_option(H),
	rdf_literal_index(T).

set_option(Term) :-
	functor(Term, Name, Arity),
	functor(General, Name, Arity),
	(   retract(setting(General))
	->  assert(Term)
	;   throw(error(domain_error(Term, rdf_index_option), _))
	).


		 /*******************************
		 *	      QUERY		*
		 *******************************/

%	rdf_find_literals(+Spec, -Literals)
%	
%	Spec ::= and(Spec,Spec)
%	Spec ::= or(Spec,Spec)
%	Spec ::= not(Spec)
%	Spec ::= sounds(Like)
%	Spec ::= stem(Like)
%	Spec ::= Token
%	
%	sounds(Like) and stem(Like) both map to  a disjunction. First we
%	compile the spec to normal form:   a disjunction of conjunctions
%	on elementary tokens. Then we execute   all the conjunctions and
%	generate the union using ordered-set union.
%	
%	TBD: numbers

rdf_find_literals(Spec, Literals) :-
	compile_spec(Spec, Conjunctions),
	token_index(Map),
	lookup(Conjunctions, Map, Sets),
	ord_union(Sets, Literals).

lookup([], _, []).
lookup([H0|T0], Map, [H|T]) :-
	rdf_find_literal_map(Map, H0, H1),
	sort(H1, H),			% TBD: Semweb order is different!
	lookup(T0, Map, T).
	
compile_spec(sounds(Like), Disjunction) :- !,
	metaphone_index(Map),
	double_metaphone(Like, Key),
	rdf_find_literal_map(Map, Key, Tokens),
	list_to_or(Tokens, Disjunction).
compile_spec(stem(Like), Disjunction) :- !,
	porter_index(Map),
	porter_stem(Like, Key),
	rdf_find_literal_map(Map, Key, Tokens),
	list_to_or(Tokens, Disjunction).
compile_spec(or(A0, B0), or(A,B)) :- !,
	compile_spec(A0, A),
	compile_spec(B0, B).
compile_spec(and(A0, B0), and(A,B)) :- !,
	compile_spec(A0, A),
	compile_spec(B0, B).
compile_spec(Token, Token) :-
	atomic(Token).

list_to_or([], false) :- !.
list_to_or([X], X) :- !.
list_to_or([H|T0], or(H, T)) :-
	list_to_or(T0, T).


%	ord_union(+ListOfSets, -Union)
%	
%	Should eventually use a  comparison   predicate  from the rdf_db
%	library, so we do not need to go from semweb literal ordering to
%	Prolog standard order of terms first.

ord_union([], []).
ord_union([X], [X]).
ord_union([H1,H2|T], Union) :-
	ord_union(H1, H2, H),
	ord_union([H|T], Union).


		 /*******************************
		 *	    TOKEN INDEX		*
		 *******************************/

%	token_index(-Map)
%	
%	Get the index of tokens. If  not   present,  create one from the
%	current database. Once created, the map is kept up-to-date using
%	a monitor hook.

token_index(Map) :-
	literal_map(tokens, Map), !.
token_index(Map) :-
	rdf_new_literal_map(Map),
	assert(literal_map(tokens, Map)),
	(   rdf(_,_,literal(Literal)),
	    register_literal(Literal),
	    fail
	;   true
	),
	verbose('~N', []),
	rdf_monitor(monitor_literal,
		    [ reset,
		      new_literal,
		      old_literal
		    ]).


%	clean_token_index
%	
%	Clean after a reset.

clean_token_index :-
	forall(literal_map(_, Map),
	       rdf_reset_literal_map(Map)).

monitor_literal(new_literal(Literal)) :-
	register_literal(Literal).
monitor_literal(old_literal(Literal)) :-
	unregister_literal(Literal).
monitor_literal(reset) :-
	clean_token_index.


%	register_literal(+Literal)
%	
%	Associate the tokens of a literal with the literal itself.

register_literal(Literal) :-
	(   text_of(Literal, Text)
	->  tokenize_atom(Text, Tokens),
	    literal_map(tokens, Map),
	    add_tokens(Tokens, Text, Map)
	;   true
	).

add_tokens([], _, _).
add_tokens([H|T], Literal, Map) :-
	(   no_index_token(H)
	->  true
	;   (   rdf_keys_in_literal_map(Map, H, _)
	    ->  true
	    ;   forall(new_token(H), true)
	    ),
	    rdf_insert_literal_map(Map, H, Literal),
	    rdf_statistics_literal_map(Map, size(Keys, Values)),
	    (   Keys mod 1000 =:= 0
	    ->  verbose('\rKeys: ~t~D~15|~tValues: ~D~35|', [Keys, Values])
	    ;   true
	    )
	),
	add_tokens(T, Literal, Map).


%	unregister_literal(+Literal)
%	
%	Literal is removed from the database.   As we abstract from lang
%	and type qualifiers we first have to  check this is the last one
%	that is destroyed.

unregister_literal(Literal) :-
	text_of(Literal, Text),
	(   rdf(_,_,literal(Text))
	->  true			% still something left
	;   tokenize_atom(Text, Tokens),
	    literal_map(tokens, Map),
	    del_tokens(Tokens, Text, Map)
	).

del_tokens([], _, _).
del_tokens([H|T], Literal, Map) :-
	rdf_delete_literal_map(Map, H, Literal),
	del_tokens(T, Literal, Map).


%	no_index_token/1
%	
%	Tokens we do not wish to index,   as  they creat huge amounts of
%	data with little or no value.  Is   there  a more general way to
%	describe this? Experience shows that simply  word count is not a
%	good criterium as it often rules out popular domain terms.

no_index_token(X) :-
	atom_length(X, 1), !.
no_index_token(X) :-
	float(X), !.
no_index_token(and).
no_index_token(an).
no_index_token(or).
no_index_token(of).
no_index_token(on).
no_index_token(in).
no_index_token(this).
no_index_token(the).


%	text_of(+LiteralArg, -Text)
%	
%	Get the textual  or  (integer)   numerical  information  from  a
%	literal value.

text_of(type(_, Text), Text) :- !.
text_of(lang(_, Text), Text) :- !.
text_of(Text, Text) :- atom(Text), !.
text_of(Text, Text) :- integer(Text).


		 /*******************************
		 *	   PORTER INDEX		*
		 *******************************/


porter_index(Map) :-
	literal_map(porter, Map), !.
porter_index(Map) :-
	rdf_new_literal_map(Map),
	assert(literal_map(porter, Map)),
	fill_porter_index(Map),
	assert((new_token(Token) :- add_stem(Token, Map))).

fill_porter_index(PorterMap) :-
	token_index(TokenMap),
	rdf_keys_in_literal_map(TokenMap, all, Tokens),
	stem(Tokens, PorterMap).

stem([], _).
stem([Token|T], Map) :-
	porter_stem(Token, Stem),
	rdf_insert_literal_map(Map, Stem, Token),
	stem(T, Map).
	       

add_stem(Token, Map) :-
	porter_stem(Token, Stem),
	rdf_insert_literal_map(Map, Stem, Token).


		 /*******************************
		 *	  METAPHONE INDEX	*
		 *******************************/


metaphone_index(Map) :-
	literal_map(metaphone, Map), !.
metaphone_index(Map) :-
	rdf_new_literal_map(Map),
	assert(literal_map(metaphone, Map)),
	fill_metaphone_index(Map),
	assert((new_token(Token) :- add_metaphone(Token, Map))).

fill_metaphone_index(PorterMap) :-
	token_index(TokenMap),
	rdf_keys_in_literal_map(TokenMap, all, Tokens),
	stem(Tokens, PorterMap).

stem([], _).
stem([Token|T], Map) :-
	double_metaphone(Token, SoundEx),
	rdf_insert_literal_map(Map, SoundEx, Token),
	stem(T, Map).
	       

add_metaphone(Token, Map) :-
	double_metaphone(Token, SoundEx),
	rdf_insert_literal_map(Map, SoundEx, Token).


		 /*******************************
		 *	       UTIL		*
		 *******************************/

verbose(Fmt, Args) :-
	setting(verbose(true)), !,
	format(user_error, Fmt, Args).
verbose(_, _).

