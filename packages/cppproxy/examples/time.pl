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
	version(-integer).

version(1).

		 /*******************************
		 *	      GENERATE		*
		 *******************************/

make_client :-
	format('Creating wrapper ...'), flush_output,
	cpp_server_code('time_proxy.h',
			[ server_class('TimeProxy')
			]),
	format('ok.~ncompiling C++ code ...'), flush_output,
	compile('time.cpp'),
	format('ok.~n').


		 /*******************************
		 *	       SERVER		*
		 *******************************/

start_server :-
	port(Port),
	cpp_server([port(Port)]).

