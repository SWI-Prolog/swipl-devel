:- use_module(parms).

:- use_module(library(typedef)).
:- use_module(library(cpp_interface)).
:- use_module(library(cpp_codegen)).
:- use_module(library(cpp_server)).

		 /*******************************
		 *	       TYPES		*
		 *******************************/

:- type first_name = atom.
:- type last_name  = atom.
:- type age        = integer.

:- type person -> person(first_name, last_name, age).

:- cpp_callable
	add_person(+person),
	find_person_younger_than(+age, -person) = [zero_or_more].

add_person(person(FirstName, LastName, Age)) :-
	assert(person(FirstName, LastName, Age)).

find_person_younger_than(MaxAge, person(FirstName, LastName, Age)) :-
	person(FirstName, LastName, Age),
	Age =< MaxAge.

		 /*******************************
		 *	      GENERATE		*
		 *******************************/

make_client :-
	format('Creating wrapper ...'), flush_output,
	cpp_server_code('person_proxy.h',
			[ server_class('PersonProxy')
			]),
	format('ok.~ncompiling C++ code ...'), flush_output,
	compile('person.cpp'),
	format('ok.~n').


		 /*******************************
		 *	       SERVER		*
		 *******************************/

start_server :-
	port(Port),
	cpp_server([port(Port)]).


		 /*******************************
		 *	       DATA		*
		 *******************************/

:- dynamic
	person/3.

person('John',  'Jonsson', 34).
person('Mary',  'Smith',   28).
person('Bob',   'Smith',   58).
person('Terry', 'Waits',   52).
person('Tim',   'Seldon',  14).
