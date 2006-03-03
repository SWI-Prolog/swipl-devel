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
	    rdf_find_literals/2			% +Spec, -ListOfLiterals
	  ]).
:- use_module(rdf_db).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
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

rdf_set_literal_index_option([]).
rdf_set_literal_index_option([H|T]) :-
	set_option(H),
	rdf_set_literal_index_option(T).

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
	compile_spec(Spec, DNF),
	token_index(Map),
	lookup(DNF, Map, Sets),
	ord_union(Sets, Literals).

lookup(false, _, []) :- !.
lookup(or(H0,T0), Map, [H|T]) :- !,
	lookup1(H0, Map, H),
	lookup(T0, Map, T).
lookup(H0, Map, [H]) :-
	lookup1(H0, Map, H).
	
lookup1(Conj, Map, Literals) :-
	phrase(conj_to_list(Conj), List),
	rdf_find_literal_map(Map, List, Literals0),
	sort(Literals0, Literals).	% TBD: Semweb order is different!

conj_to_list(and(A,B)) --> !,
	conj_to_list(A),
	conj_to_list(B).
conj_to_list(L) -->
	[L].


%	compile_spec(+Spec, -Compiled)
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
	list_to_or(Tokens, Or).
expand_fuzzy(stem(Like), Or) :- !,
	porter_index(Map),
	porter_stem(Like, Key),
	rdf_find_literal_map(Map, [Key], Tokens),
	list_to_or(Tokens, Or).
expand_fuzzy(or(A0, B0), or(A,B)) :- !,
	expand_fuzzy(A0, A),
	expand_fuzzy(B0, B).
expand_fuzzy(and(A0, B0), and(A,B)) :- !,
	expand_fuzzy(A0, A),
	expand_fuzzy(B0, B).
expand_fuzzy(not(A0), not(A)) :- !,
	expand_fuzzy(A0, A).
expand_fuzzy(Token, Token) :-
	atomic(Token), !.
expand_fuzzy(Token, _) :-
	throw(error(type_error(Token, boolean_expression), _)).


list_to_or([], false) :- !.
list_to_or([X], X) :- !.
list_to_or([H|T0], or(H, T)) :-
	list_to_or(T0, T).


%	nnf(+Formula, -NNF)
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


%	dnf(+NNF, -DNF)
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


%	ord_union(+ListOfSets, -Union)
%	
%	Should eventually use a  comparison   predicate  from the rdf_db
%	library, so we do not need to go from semweb literal ordering to
%	Prolog standard order of terms first.

ord_union([], []).
ord_union([X], [X]) :- !.
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
	;   (   rdf_keys_in_literal_map(Map, key(H), _)
	    ->  true
	    ;   forall(new_token(H), true)
	    ),
	    rdf_insert_literal_map(Map, H, Literal),
	    progress(Map, 'Tokens')
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
