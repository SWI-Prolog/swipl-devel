/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2002, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(rdfs,
	  [ rdfs_subproperty_of/2,	% ?SubProperties, ?Property
	    rdfs_subclass_of/2,		% ?SubClass, ?Class
	    rdfs_class_property/2,	% +Class, ?Property
	    rdfs_individual_of/2,	% ?Resource, ?Class

	    rdfs_label/2,		% ?Resource, ?Label
	    rdfs_ns_label/2,		% +Resource, -Label

	    rdfs_member/2,		% ?Object, +Set
	    rdfs_list_to_prolog_list/2,	% +Set, -List

	    rdfs_find/5			% +String, +Dom, +Props, +Method, -Subj
	  ]).
:- use_module(library(debug)).
:- use_module(library(rdf)).
:- use_module(library(lists)).
:- use_module(rdf_db).


		 /*******************************
		 *	    EXPANSION		*
		 *******************************/

%	user:goal_expansion(+NSGoal, -Goal)
%	
%	This predicate allows for writing down rdf queries in a friendly
%	name-space fashion.  

:- multifile
	user:goal_expansion/2.

user:goal_expansion(rdfs_subproperty_of(Sub0, Prop0),
		    rdfs_subproperty_of(Sub, Prop)) :-
	rdf_global_id(Sub0, Sub),
	rdf_global_id(Prop0, Prop).
user:goal_expansion(rdfs_subclass_of(Sub0, Class0),
		    rdfs_subclass_of(Sub, Class)) :-
	rdf_global_id(Sub0, Sub),
	rdf_global_id(Class0, Class).
user:goal_expansion(rdfs_class_property(Class0, Prop0),
		    rdfs_class_property(Class, Prop)) :-
	rdf_global_id(Class0, Class),
	rdf_global_id(Prop0, Prop).
user:goal_expansion(rdfs_individual_of(Resource0, Class0),
		    rdfs_individual_of(Resource, Class)) :-
	rdf_global_id(Resource0, Resource),
	rdf_global_id(Class0, Class).
user:goal_expansion(rdfs_label(Resource0, Label),
		    rdfs_label(Resource, Label)) :-
	rdf_global_id(Resource0, Resource).


		 /*******************************
		 *	PROPERTY HIERARCHY	*
		 *******************************/

%	rdfs_subproperty_of(+SubProperty, ?Property)
%	rdfs_subproperty_of(?SubProperty, +Property)
%	
%	Query the property hierarchy.

rdfs_subproperty_of(SubProperty, Property) :-
	rdf_reachable(SubProperty, rdfs:subPropertyOf, Property).


		 /*******************************
		 *	  CLASS HIERARCHY	*
		 *******************************/

%	rdfs_subclass_of(+Class, ?Super)
%	rdfs_subclass_of(?Class, +Super)
%	
%	Generate sub/super classes.  rdf_reachable/3 considers the
%	rdfs:subPropertyOf relation as well as cycles.

rdfs_subclass_of(Class, Super) :-
	rdf_reachable(Class, rdfs:subClassOf, Super).


		 /*******************************
		 *	    INDIVIDUALS		*
		 *******************************/

%	rdfs_individual(+Resource, -Class)
%	rdfs_individual(-Resource, +Class)
%	
%	Generate resources belonging to a class or classes a resource
%	belongs to.  We assume everything at the `object' end of a 
%	triple is a class.  A validator should confirm this property.

rdfs_individual_of(Resource, Class) :-
	nonvar(Resource), !,
	rdf_has(Resource, rdf:type, MyClass),
	rdfs_subclass_of(MyClass, Class).
rdfs_individual_of(Resource, Class) :-
	nonvar(Class), !,
	rdfs_subclass_of(SubClass, Class),
	rdf_has(Resource, rdf:type, SubClass).
rdfs_individual_of(_Resource, _Class) :-
	throw(error(instantiation_error, _)).

%	rdfs_label(?Resource, ?Label)
%
%	Convert between class and label.

rdfs_label(Resource, Label) :-
	nonvar(Resource), !,
	take_label(Resource, Label).
rdfs_label(Resource, Label) :-
	rdf_has(Resource, rdfs:label, literal(Label)).

%	rdfs_ns_label(+Resource, -Label)
%	
%	Present label with  namespace  indication.   This  predicate  is
%	indented  to  provide  meaningful  short   names  applicable  to
%	ontology maintainers.  Note that this predicate is non-deterministic
%	if the resource has multiple rdfs:label properties

rdfs_ns_label(Resource, Label) :-
	rdfs_label(Resource, Label0),
	(   rdf_global_id(NS:_, Resource)
	->  concat_atom([NS, Label0], :, Label)
	;   \+ rdf_has(Resource, rdfs:label, _)
	->  Label = Resource
	;   member(Sep, [#,/]),
	    sub_atom(Resource, B, L, A, Sep),
	    sub_atom(Resource, _, A, 0, Frag),
	    \+ sub_atom(Frag, _, _, _, Sep)
	->  Len is B+L,
	    sub_atom(Resource, 0, Len, _, NS),
	    concat_atom([NS, Label0], :, Label)
	;   Label = Label0
	).


%	take_label(+Class, -Label)
%
%	Get the label to use for a class.  

take_label(Class, Label) :-
	(   rdf_has(Class, rdfs:label, literal(Label))
	*-> true
	;   rdf_split_url(_, Label, Class)
	).

%	rdfs_class_property(+Class, ?Property)
%
%	Enumerate the properties in the domain of Class.

rdfs_class_property(Class, Property) :-
	rdfs_individual_of(Property, rdf:'Property'),
	rdf_has(Property, rdfs:domain, Domain),
	rdfs_subclass_of(Class, Domain).


		 /*******************************
		 *	     COLLECTIONS	*
		 *******************************/

%	rdfs_member(?Element, +Set)
%	
%	As Prolog member on sets.  Operates both on attributes parsed as
%	parseType="Collection" as well as on Bag, Set and Alt. 

rdfs_member(Element, Set) :-
	rdfs_individual_of(Set, rdf:'List'), !,
	rdfs_collection_member(Element, Set).
rdfs_member(Element, Set) :-
	rdfs_individual_of(Set, rdf:'Container'), !,
	(   nonvar(Element)
	->  rdf(Set, Predicate, Element),
	    rdf_member_property(Predicate, _N)
	;   between(1, 1000000000, N),
	    rdf_member_property(Prop, N),
	    (	rdf(Set, Prop, Member)
	    ->	Member = Element
	    ;	!, fail
	    )
	).

rdfs_collection_member(Element, Set) :-
	rdf_has(Set, rdf:first, Element),
	assume(rdfs_individual_of(Set, rdf:'List')).
rdfs_collection_member(Element, Set) :-
	rdf_has(Set, rdf:rest, Tail), !,
	rdfs_collection_member(Element, Tail).
rdfs_collection_member(_, Set) :-
	assume(rdf_equal(Set, rdf:nil)),
	fail.

%	rdfs_list_to_prolog_list(+RDFSList, -PrologList)
%	
%	Convert ann RDFS list (result from parseType=Collection) into a
%	Prolog list of elements.

rdfs_list_to_prolog_list(Set, []) :-
	rdf_equal(Set, rdf:nil), !.
rdfs_list_to_prolog_list(Set, [H|T]) :-
	rdf_has(Set, rdf:first, H),
	rdf_has(Set, rdf:rest, Tail), !,
	rdfs_list_to_prolog_list(Tail, T).


		 /*******************************
		 *     SEARCH IN HIERARCHY	*
		 *******************************/

%	rdfs_find(+String, +Domain, +Properties, +Method, -Subject)
%	
%	Search all classes below Domain for a literal property with
%	that matches String.  Method is one of
%	
%		substring
%		word
%		prefix
%		exact
%		
%	domain is defined by owl_satisfy from owl.pl
%		
%	Note that the rdfs:label field is handled by rdfs_label/2,
%	making the URI-ref fragment name the last resort to determine
%	the label.

rdfs_find(String, Domain, Fields, Method, Subject) :-
	globalise_list(Fields, GlobalFields),
	For =.. [Method,String],
	member(Field, GlobalFields),
	(   Field == resource
	->  rdf_subject(Subject),
	    rdf_match_label(Method, String, Subject)
	;   rdf_has(Subject, Field, literal(For, _))
	),
	owl_satisfies(Domain, Subject).

owl_satisfies(Domain, _) :-
	rdf_equal(rdfs:'Resource', Domain), !.
					% Descriptions
owl_satisfies(class(Domain), Resource) :- !,
	(   rdf_equal(Domain, rdfs:'Resource')
	->  true
	;   rdfs_subclass_of(Resource, Domain)
	).
owl_satisfies(union_of(Domains), Resource) :- !,
	member(Domain, Domains),
	owl_satisfies(Domain, Resource), !.
owl_satisfies(intersection_of(Domains), Resource) :- !,
	in_all_domains(Domains, Resource).
owl_satisfies(complement_of(Domain), Resource) :- !,
	\+ owl_satisfies(Domain, Resource).
owl_satisfies(one_of(List), Resource) :- !,
	memberchk(Resource, List).
					% Restrictions
owl_satisfies(all_values_from(Domain), Resource) :-
	(   rdf_equal(Domain, rdfs:'Resource')
	->  true
	;   rdfs_individual_of(Resource, Domain)
	), !.
owl_satisfies(some_values_from(_Domain), _Resource) :- !.
owl_satisfies(has_value(Value), Resource) :-
	rdf_equal(Value, Resource).


in_all_domains([], _).
in_all_domains([H|T], Resource) :-
	owl_satisfies(H, Resource),
	in_all_domains(T, Resource).

globalise_list([], []) :- !.
globalise_list([H0|T0], [H|T]) :- !,
	globalise_list(H0, H),
	globalise_list(T0, T).
globalise_list(X, G) :-
	rdf_global_id(X, G).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
TOP-DOWN


rdfs_find(String, Domain, Fields, Method, Subject) :-
	globalise_list(Fields, GlobalFields),
	generate_domain(Domain, Subject),
	member(Field, GlobalFields),
	(   rdf_equal(Field, rdfs:label)
	->  rdfs_label(Subject, Arg)
	;   rdf_has(Subject, Field, literal(Arg))
	),
	rdf_match_label(Method, String, Arg).
	
%	generate_domain(+Domain, -Resource)
%	
%	Generate all resources that satisfy some a domain specification.

generate_domain(All, Subject) :-
	rdf_equal(All, rdfs:'Resource'), !,
	rdf_subject(Subject).
generate_domain(class(Class), Subject) :- !,
	rdfs_subclass_of(Subject, Class).
generate_domain(all_values_from(Class), Individual) :-
	(   rdf_equal(Class, rdfs:'Resource')
	->  rdf_subject(Individual)			% this is OWL-full
	;   rdfs_individual_of(Individual, Class)
	).
generate_domain(some_values_from(Class), Individual) :- % Actually this is
	rdfs_individual_of(Individual, Class). 		% anything
generate_domain(union_of(Domains), Individual) :-
	member(Domain, Domains),
	generate_domain(Domain, Individual).
generate_domain(intersection_of(Domains), Individual) :-
	in_all_domains(Domains, Individual).
generate_domain(one_of(Individuals), Individual) :-
	member(Individual, Individuals).

in_all_domains([], _).
in_all_domains([H|T], Resource) :-
	generate_domain(H, Resource),
	in_all_domains(T, Resource).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

