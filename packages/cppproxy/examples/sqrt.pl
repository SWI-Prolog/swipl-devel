:- use_module(parms).

:- use_module(library(typedef)).
:- use_module(library(cpp_interface)).
:- use_module(library(cpp_codegen)).
:- use_module(library(cpp_server)).

		 /*******************************
		 *	       TYPES		*
		 *******************************/

:- cpp_callable
	between(+integer, +integer, -integer) = [zero_or_more],
	sqrt(+float, -float).

sqrt(In, Out) :- Out is sqrt(In).

		 /*******************************
		 *	      GENERATE		*
		 *******************************/

make_client :-
	format('Creating wrapper ...'), flush_output,
	cpp_server_code('sqrt_proxy.h',
			[ server_class('SqrtProxy')
			]),
	format('ok.~ncompiling C++ code ...'), flush_output,
	compile('sqrt.cpp'),
	format('ok.~n').


		 /*******************************
		 *	       SERVER		*
		 *******************************/

start_server :-
	port(Port),
	cpp_server([port(Port)]).

