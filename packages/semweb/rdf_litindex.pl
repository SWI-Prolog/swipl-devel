/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2006, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/


:- module(rdf_litindex,
	  [ rdf_set_literal_index_option/1,	% +Options
	    rdf_tokenize_literal/2,		% +Literal, -Tokens
	    rdf_find_literals/2,		% +Spec, -ListOfLiterals
	    rdf_token_expansions/2		% +Spec, -Expansions
	  ]).
:- use_module(rdf_db).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(porter_stem)).
:- use_module(library(double_metaphone)).

/** <module> Search literals
This module finds literals of the RDF database based on stemming and
being flexible to ordering of tokens.  
*/

:- dynamic
	literal_map/2,			% Type, -Map
	new_token/1,			% Hook
	setting/1.
:- volatile
	literal_map/2.
:- multifile
	tokenization/2,			% +Literal, -Tokens
	exclude_from_index/2.		% +Which, +Token


setting(verbose(true)).			% print progress messages

%%	rdf_set_literal_index_option(+Options:list)
%
%	Set options for the literal package.  Currently defined options
%	
%		* verbose(Bool)
%		If =true=, print progress messages while building the
%		index tables.

rdf_set_literal_index_option([]).
rdf_set_literal_index_option([H|T]) :-
	set_option(H),
	rdf_set_literal_index_option(T).

set_option(Term) :-
	functor(Term, Name, Arity),
	functor(General, Name, Arity),
	(   retract(setting(General))
	->  assert(setting(Term))
	;   throw(error(domain_error(Term, rdf_index_option), _))
	).


		 /*******************************
		 *	      QUERY		*
		 *******************************/

%%	rdf_find_literals(+Spec, -Literals)
%
%	Find literals in the RDF database matching Spec.  Spec is defined
%	as:
%	
%	==
%	Spec ::= and(Spec,Spec)
%	Spec ::= or(Spec,Spec)
%	Spec ::= not(Spec)
%	Spec ::= sounds(Like)
%	Spec ::= stem(Like)
%	Spec ::= prefix(Prefix)
%	Spec ::= between(Low, High)	% Numerical between
%	Spec ::= ge(High)		% Numerical greater-equal
%	Spec ::= le(Low)		% Numerical less-equal
%	Spec ::= Token
%	==
%	
%	sounds(Like) and stem(Like) both map to  a disjunction. First we
%	compile the spec to normal form:   a disjunction of conjunctions
%	on elementary tokens. Then we execute   all the conjunctions and
%	generate the union using ordered-set algorithms.
%	
%	@tbd Exploit ordering of numbers and allow for > N, < N, etc.

rdf_find_literals(Spec, Literals) :-
	compile_spec(Spec, DNF),
	token_index(Map),
	lookup(DNF, Map, _, SuperSet),
	flatten(SuperSet, Set0),
	sort(Set0, Literals).

%%	rdf_token_expansions(+Spec, -Extensions)
%	
%	Determine which extensions of  a   token  contribute  to finding
%	literals.

rdf_token_expansions(prefix(Prefix), [prefix(Prefix, Tokens)]) :-
	token_index(Map),
	rdf_keys_in_literal_map(Map, prefix(Prefix), Tokens).
rdf_token_expansions(sounds(Like), [sounds(Like, Tokens)]) :-
	metaphone_index(Map),
	rdf_find_literal_map(Map, [Like], Tokens).
rdf_token_expansions(stem(Like), [stem(Like, Tokens)]) :-
	porter_index(Map),
	rdf_find_literal_map(Map, [Like], Tokens).
rdf_token_expansions(Spec, Expansions) :-
	compile_spec(Spec, DNF),
	token_index(Map),
	lookup(DNF, Map, SCS, _),
	flatten(SCS, CS),
	sort(CS, Expansions0),
	join_expansions(Expansions0, Expansions).

join_expansions([], []).
join_expansions([H0|T0], [H|T]) :-
	untag(H0, Tag, V0),
	Tag =.. L0,
	append(L0, [[V0|Values]], L1),
	H =.. L1,
	join_expansions_by_tag(T0, Tag, T1, Values),
	join_expansions(T1, T).

join_expansions_by_tag([H|T0], Tag, T, [V0|VT]) :-
	untag(H, Tag, V0), !,
	join_expansions_by_tag(T0, Tag, T, VT).
join_expansions_by_tag(L, _, L, []).
	
lookup(false, _, [], []) :- !.
lookup(or(H0,T0), Map, [CH|CT], [H|T]) :- !,
	lookup(H0, Map, CH, H),
	lookup(T0, Map, CT, T).
lookup(H0, Map, [C], [H]) :-
	lookup1(H0, Map, C, H).
	
lookup1(Conj, Map, Cond, Literals) :-
	phrase(conj_to_list(Conj), List),
	rdf_find_literal_map(Map, List, Literals),
	(   Literals \== []
	->  phrase(conj_to_cond(Conj), Cond)
	;   Cond = []
	).

conj_to_list(and(A,B)) --> !,
	conj_to_list(A),
	conj_to_list(B).
conj_to_list(Tagged) -->
	{ untag(Tagged, L) }, !,
	[L].
conj_to_list(L) -->
	[L].


conj_to_cond(and(A,B)) --> !,
	conj_to_cond(A),
	conj_to_cond(B).
conj_to_cond(Tagged) -->
	{ untag(Tagged, _) }, !,
	[ Tagged ].
conj_to_cond(_) -->
	[].


%%	compile_spec(+Spec, -Compiled)
%	
%	Compile a specification as above into disjunctive normal form

compile_spec(Spec, DNF) :-
	expand_fuzzy(Spec, Spec2),
	nnf(Spec2, NNF),
	dnf(NNF, DNF).


expand_fuzzy(Var, _) :-
	var(Var), !,
	throw(error(instantiation_error, _)).
expand_fuzzy(sounds(Like), Or) :- !,
	metaphone_index(Map),
	double_metaphone(Like, Key),
	rdf_find_literal_map(Map, [Key], Tokens),
	list_to_or(Tokens, sounds(Like), Or).
expand_fuzzy(stem(Like), Or) :- !,
	porter_index(Map),
	porter_stem(Like, Key),
	rdf_find_literal_map(Map, [Key], Tokens),
	list_to_or(Tokens, stem(Like), Or).
expand_fuzzy(prefix(Prefix), Or) :- !,
	token_index(Map),
	rdf_keys_in_literal_map(Map, prefix(Prefix), Tokens),
	list_to_or(Tokens, prefix(Prefix), Or).
expand_fuzzy(case(String), Or) :- !,
	token_index(Map),
	rdf_keys_in_literal_map(Map, case(String), Tokens),
	list_to_or(Tokens, case(String), Or).
expand_fuzzy(or(A0, B0), or(A,B)) :- !,
	expand_fuzzy(A0, A),
	expand_fuzzy(B0, B).
expand_fuzzy(and(A0, B0), and(A,B)) :- !,
	expand_fuzzy(A0, A),
	expand_fuzzy(B0, B).
expand_fuzzy(not(A0), not(A)) :- !,
	expand_fuzzy(A0, A).
expand_fuzzy(between(Low, High), Or) :- !,
	token_index(Map),
	rdf_keys_in_literal_map(Map, between(Low, High), Tokens),
	list_to_or(Tokens, between(Low, High), Or).
expand_fuzzy(le(High), Or) :- !,
	token_index(Map),
	rdf_keys_in_literal_map(Map, le(High), Tokens),
	list_to_or(Tokens, le(High), Or).
expand_fuzzy(ge(Low), Or) :- !,
	token_index(Map),
	rdf_keys_in_literal_map(Map, ge(Low), Tokens),
	list_to_or(Tokens, ge(Low), Or).
expand_fuzzy(Token, Token) :-
	atomic(Token), !.
expand_fuzzy(Token, _) :-
	throw(error(type_error(Token, boolean_expression), _)).


list_to_or([], _, false) :- !.
list_to_or([X], How, One) :- !,
	tag(How, X, One).
list_to_or([H0|T0], How, or(H, T)) :-
	tag(How, H0, H),
	list_to_or(T0, How, T).

tag(sounds(X),	  Y, sounds(X,Y)).
tag(stem(X),	  Y, stem(X,Y)).
tag(prefix(X),	  Y, prefix(X,Y)).
tag(case(X),	  Y, case(X,Y)).
tag(between(L,H), Y, between(L,H,Y)).
tag(ge(L),	  Y, ge(L,Y)).
tag(le(H),	  Y, le(H,Y)).

untag(sounds(_,Y),    Y).
untag(stem(_,Y),      Y).
untag(prefix(_,Y),    Y).
untag(case(_,Y),      Y).
untag(between(_,_,Y), Y).
untag(le(_,Y),	      Y).
untag(ge(_,Y),	      Y).

untag(sounds(X,Y),    sounds(X),    Y).
untag(stem(X,Y),      stem(X),	    Y).
untag(prefix(X,Y),    prefix(X),    Y).
untag(case(X,Y),      case(X),	    Y).
untag(between(L,H,Y), between(L,H), Y).
untag(ge(L,Y),	      ge(L),	    Y).
untag(le(H,Y),	      le(H),	    Y).


%%	nnf(+Formula, -NNF)
%	
%	Rewrite to Negative Normal Form, meaning negations only appear
%	around literals.

nnf(not(not(A0)), A) :- !,
	nnf(A0, A).
nnf(not(and(A0,B0)), or(A,B)) :- !,
	nnf(not(A0), A),
	nnf(not(B0), B).
nnf(not(or(A0,B0)), and(A,B)) :- !,
	nnf(not(A0), A),
	nnf(not(B0), B).
nnf(A, A).


%%	dnf(+NNF, -DNF)
%	
%	Convert a formula in NNF to Disjunctive Normal Form (DNF)

dnf(or(A0,B0), or(A, B)) :- !,
	dnf(A0, A),
	dnf(B0, B).
dnf(and(A0,B0), DNF):- !,
	dnf(A0, A1),
	dnf(B0, B1),
	dnf1(and(A1,B1), DNF).
dnf(DNF, DNF).

dnf1(and(A0, or(B,C)), or(P,Q)) :- !,
	dnf1(and(A0,B), P),
	dnf1(and(A0,C), Q).
dnf1(and(or(B,C), A0), or(P,Q)) :- !,
	dnf1(and(A0,B), P),
	dnf1(and(A0,C), Q).
dnf1(DNF, DNF).


		 /*******************************
		 *	    TOKEN INDEX		*
		 *******************************/

%%	token_index(-Map)
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
monitor_literal(transaction(begin, reset)) :-
	rdf_monitor(monitor_literal, [-old_literal]),
	clean_token_index.
monitor_literal(transaction(end, reset)) :-
	rdf_monitor(monitor_literal, [+old_literal]).


%%	register_literal(+Literal)
%	
%	Associate the tokens of a literal with the literal itself.

register_literal(Literal) :-
	(   rdf_tokenize_literal(Literal, Tokens)
	->  text_of(Literal, Text),
	    literal_map(tokens, Map),
	    add_tokens(Tokens, Text, Map)
	;   true
	).

add_tokens([], _, _).
add_tokens([H|T], Literal, Map) :-
	(   rdf_keys_in_literal_map(Map, key(H), _)
	->  true
	;   forall(new_token(H), true)
	),
	rdf_insert_literal_map(Map, H, Literal),
	progress(Map, 'Tokens'),
	add_tokens(T, Literal, Map).


%%	unregister_literal(+Literal)
%	
%	Literal is removed from the database.   As we abstract from lang
%	and type qualifiers we first have to  check this is the last one
%	that is destroyed.

unregister_literal(Literal) :-
	text_of(Literal, Text),
	(   rdf(_,_,literal(Text))
	->  true			% still something left
	;   rdf_tokenize_literal(Literal, Tokens),
	    literal_map(tokens, Map),
	    del_tokens(Tokens, Text, Map)
	).

del_tokens([], _, _).
del_tokens([H|T], Literal, Map) :-
	rdf_delete_literal_map(Map, H, Literal),
	del_tokens(T, Literal, Map).


%%	rdf_tokenize_literal(+Literal, -Tokens) is semidet.
%	
%	Tokenize a literal. We make  this   hookable  as tokenization is
%	generally domain dependent.

rdf_tokenize_literal(Literal, Tokens) :-
	tokenization(Literal, Tokens), !. 		% Hook
rdf_tokenize_literal(Literal, Tokens) :-
	text_of(Literal, Text),
	atom(Text),
	tokenize_atom(Text, Tokens0),
	select_tokens(Tokens0, Tokens).

select_tokens([], []).
select_tokens([H|T0], T) :-
	no_index_token(H), !,
	select_tokens(T0, T).
select_tokens([H|T0], [H|T]) :-
	select_tokens(T0, T).


%	no_index_token/1
%	
%	Tokens we do not wish to index,   as  they creat huge amounts of
%	data with little or no value.  Is   there  a more general way to
%	describe this? Experience shows that simply  word count is not a
%	good criterium as it often rules out popular domain terms.

no_index_token(X) :-
	exclude_from_index(token, X), !.
no_index_token(X) :-			% TBD: only small integers can
	integer(X),			% be indexed
	\+ between(-1073741824, 1073741823, X), !.
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


%%	text_of(+LiteralArg, -Text)
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
	(   atom(Token)
	->  porter_stem(Token, Stem),
	    rdf_insert_literal_map(Map, Stem, Token),
	    progress(Map, 'Porter')
	;   true
	),
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
	metaphone(Tokens, PorterMap).

metaphone([], _).
metaphone([Token|T], Map) :-
	(   atom(Token)
	->  double_metaphone(Token, SoundEx),
	    rdf_insert_literal_map(Map, SoundEx, Token),
	    progress(Map, 'Metaphone')
	;   true
	),
	metaphone(T, Map).
	       

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

progress(Map, Which) :-
	setting(verbose(true)), !,
	rdf_statistics_literal_map(Map, size(Keys, Values)),
	(   Keys mod 1000 =:= 0
	->  format(user_error,
		   '\r~t~w: ~12|Keys: ~t~D~15+; Values: ~t~D~20+',
		   [Which, Keys, Values])
	;   true
	).
progress(_,_).
