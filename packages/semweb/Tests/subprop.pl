:- module(subprop,
	  [ subprop/0
	  ]).
:- use_module('../rdf_db').

rdf_db:ns(test, 'http://www.test.org/').

% :- rdf_debug(10).

t1 :-
	rdf_assert(test:a, rdfs:subPropertyOf, test:r1),
	rdf_assert(test:jan, test:a, literal(jan)).

t2 :-
	rdf_assert(test:a, rdfs:subPropertyOf, test:r1),
	rdf_assert(test:a, rdfs:subPropertyOf, test:r2),
	rdf_assert(test:jan, test:a, literal(jan)).
	
t3 :-
	rdf_assert(test:a, rdfs:subPropertyOf, test:r1),
	rdf_assert(test:a, rdfs:subPropertyOf, test:r2),
	rdf_assert(test:b, rdfs:subPropertyOf, test:r3),
	rdf_assert(test:b, rdfs:subPropertyOf, test:r4),
	rdf_assert(test:c, rdfs:subPropertyOf, test:a),
	rdf_assert(test:c, rdfs:subPropertyOf, test:b),
	rdf_assert(test:jan, test:a, literal(jan)).
	
subprop :-
	rdf_reset_db,
	t3,
	rdf_has(test:jan, test:r1, Name),
	Name == literal(jan).
