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


:- module(cpp_gencode,
	  [ cpp_server_code/2		% +File, +Options
	  ]).
:- use_module(typedef).
:- use_module(cpp_interface).
:- use_module(library(option)).
:- use_module(library(debug)).
:- use_module(library(lists)).

%	cpp_server_code(+File, +Options)
%	
%	Write source-code for the server to File,  Defined options
%	
%		* server_name
%		C++ classname used for the server.

cpp_server_code(File, Options) :-
	open(File, write, Out),
	protect_macro(File, Macro),
	call_cleanup(gen_code(Out,
			      [ file(File),
				macro(Macro)
			      | Options
			      ]),
		     close(Out)).

gen_code(Out, Options) :-
	gen_header(Out, Options),
	gen_include(Out, Options),
	server_functions(DetFunctions, NonDetFunctions, Options),
	gen_server_class(Out, DetFunctions, NonDetFunctions, Options),
	gen_query_classes(Out, NonDetFunctions, Options),
	gen_footer(Out, Options).

protect_macro(File, Macro) :-
	clean_name(File, FC0),
	upcase_atom(FC0, UFC),
	atom_concat(UFC, '_INCLUDED', Macro).

gen_header(Out, Options) :-
	output_state(Out, State),
	memberchk(macro(Macro), Options),
	utter(State,
	      [ '/*  SWI-Prolog C++ interface proxy~n',
	        nl,
		'    This is generated code.  Do not edit.',
		'*/',
		nl, nl
	      ]),
	(   memberchk(macro(Macro), Options)
	->  utter(State,
		  [ '#ifndef ~w'+Macro,
		    '#define ~w'+Macro
		  ])
	;   true
	).

gen_footer(Out, Options) :-
	(   memberchk(macro(Macro), Options)
	->  output_state(Out, State),
	    utter(State,
		  [ nl, nl,
		    '#endif /*~w*/'+Macro,
		    nl
		  ])
	;   true
	).
	
gen_include(Out, _Options) :-
	output_state(Out, State),
	utter(State, '#include "SWI-proxy.h"~n~n').

gen_server_class(Out, DetFunctions, NonDetFunctions, Options) :-
	output_state(Out, State),
	option(server_class(Name), Options, 'MyProxy'),
	utter(State, 
	      [ 'class ~w : public PlProxy~n'+[Name],
		begin
	      ]),
	gen_friends(NonDetFunctions, State, Options),
	append(DetFunctions, NonDetFunctions, AllFunctions),
	gen_send_functions(AllFunctions, State, Options),
	gen_receive_functions(AllFunctions, State, Options),
	utter(State, 
	      [ label(public),
		'~w(const char *host, int port) : PlProxy(host, port) {}'
			+[Name],
		nl,nl
	      ]),
	gen_server_functions(DetFunctions, State, Options),
	utter(State,
	      [ end_class
	      ]).
	
%	server_functions(-Functions, +Options)
%	
%	Find the callable functions and their attributes. Options may be
%	used to generate only the functions from certain modules.

server_functions(Functions, _Options) :-
	findall(f(Module, Goal, Attributes),
		current_cpp_callable(Module, Goal, Attributes),
		Functions).

server_functions(DetFunctions, NonDetFunctions, Options) :-
	server_functions(AllFunctions, Options),
	split_functions(AllFunctions, DetFunctions, NonDetFunctions).

%	split_functions(+Functions, -DetFunctions, -NonDetFunctions)

split_functions([], [], []).
split_functions([H|T0], Det, [H|NonDet]) :-
	nondet_function(H), !,
	split_functions(T0, Det, NonDet).
split_functions([H|T0], [H|Det], NonDet) :-
	split_functions(T0, Det, NonDet).

nondet_function(f(_, _, Attributes)) :-
	memberchk(zero_or_more, Attributes).

%	gen_friends(+Functions, +State, +Options)
%	
%	For each non-deterministic query we generate a class. This class
%	must be a friend to access the our data conversion functions.

gen_friends([], _, _) :- !.
gen_friends(Functions, State, Options) :-
	utter(State, '/* friend classes for non-deterministic queries */'),
	forall(member(F, Functions),
	       gen_friend(F, State, Options)),
	utter(State, [nl, nl]).

gen_friend(F, State, _Options) :-
	function_name(F, Name),
	utter(State, 'friend class ~w;'+[Name]).


%	gen_server_functions(+Functions, +State, +Options)
%	
%	Generate the proxy functions for all predicates with either one
%	or zero_or_one solutions.

gen_server_functions([], _, _) :- !.
gen_server_functions(Funcs, State, Options) :-
	utter(State, '/* Public proxy functions */'),
	forall(member(F, Funcs),
	       gen_server_function(State, F, Options)).

gen_server_function(State, Function, _Options) :-
	Function = f(Module, Goal, Attributes),
	function_name(Function, FName),
	return_type(Attributes, Return),
	functor(Goal, _, Arity),
	goal_arg_types_and_names(Goal, Module, ArgTNs),
	maplist(to_arg, ArgTNs, CPPArgs),
	concat_atom(CPPArgs, ', ', Args),
	utter(State,
	      [ '~w ~w(~w)'+[Return, FName, Args],
	        begin,
		'openQuery("~w", "~w", ~d);'+[Module, FName, Arity]
	      ]),
	send_args(ArgTNs, '', State),
	(   memberchk(a(_,_,out,_), ArgTNs)
	->  (   Return == void
	    ->  utter(State, 'runVoidQuery();'),
		receive_args(ArgTNs, '', State)
	    ;   utter(State,
		      [ 'if ( runDetQuery() )',
			begin
		      ]),
		receive_args(ArgTNs, '', State),
		utter(State,
		      [ 'return TRUE;',
			end,
			'return FALSE'
		      ])
	    )
	;   (   Return == void
	    ->  utter(State, 'runVoidQuery();')
	    ;   utter(State, 'return runDetQuery();')
	    )
	),
	utter(State,
	      [ end
	      ]).
	
function_name(f(_Module, Goal, Attributes), FName) :-
	functor(Goal, PName, _Arity),
	option(as(FName), Attributes, PName).

return_type(Attributes, void) :-
	memberchk(one, Attributes), !.
return_type(_, int).

goal_arg_types_and_names(Goal, Module, ArgNames) :-
	arg_types_and_names(Goal, Module, error, ArgNames).

arg_types_and_names(Goal, Module, InOut, ArgNames) :-
	functor(Goal, _, Arity),
	arg_types_and_names(0, Arity, Goal, Module, InOut, ArgNames).

arg_types_and_names(Arity, Arity, _, _, _, []) :- !.
arg_types_and_names(I0, Arity, Goal, Module, InOut, [Arg|AT]) :-
	I is I0 + 1,
	arg(I, Goal, GA),
	arg_name_and_type(GA, I, Module, InOut, Arg),
	arg_types_and_names(I, Arity, Goal, Module, InOut, AT).

arg_name_and_type(IOType, I, Module, IO, a(AName, PlType, InOut, CType)) :-
	atom_concat(a, I, AName),	% name them a1, a2, ...
	io_type(IOType, Type, IO, InOut),
	cpp_type(Type, Module, PlType, CType).

io_type(+Type, Type, _, in) :- !.
io_type(-Type, Type, _, out) :- !.
io_type(Type, Type, InOut, InOut) :-
	InOut \== error.

%	cpp_type(+RawType, +Module, -PrologType, -CppType)
%	
%	RawType is the raw argument type as specified with cpp_callable
%	in Module.  PrologType is the primitive Prolog type and CppType
%	is the C++ type to use for the argument.

cpp_type(Type, Module, PrologType, CppType) :-
	current_type(Module:Type, [PrologType]),
	(   primitive_cpp_type(PrologType, CppType)
	->  true
	;   compound_cpp_type(Module, PrologType, CppType)
	).

primitive_cpp_type(integer, long).
primitive_cpp_type(float,   double).
primitive_cpp_type(atom,    string).

compound_cpp_type(Module, PrologType, CppType) :-
	assertion(\+ primitive_cpp_type(PrologType, _)),
	functor(PrologType, Name, _),
	(   current_cpp_type(Module, CppType, Name)
	->  true
	;   CppType = Name
	).

to_arg(a(Name, _, in, Type), Arg) :-
	numeric_type(Type), !,
	concat_atom([Type, ' ', Name], Arg).
to_arg(a(Name, _, in, Type), Arg) :- !,
	concat_atom(['const ', Type, ' &', Name], Arg).
to_arg(a(Name, _, out, Type), Arg) :-
	concat_atom([Type, ' &', Name], Arg).

numeric_type(int).
numeric_type(long).
numeric_type(float).
numeric_type(double).


send_args([], _, _).
send_args([a(Name, PlType, in, _CTtype)|AT], Prefix, State) :- !,
	send_function(PlType, Function),
	utter(State, '~w~w(~w);'+[Prefix, Function, Name]),
	send_args(AT, Prefix, State).
send_args([_|AT], Prefix, State) :- !,
	send_args(AT, Prefix, State).

send_function(integer, send_int) :- !.
send_function(atom,    send_atom) :- !.
send_function(float,   send_float) :- !.
send_function(_,       send).

receive_args([], _, _).
receive_args([a(Name, PlType, out, _CTtype)|AT], Prefix, State) :- !,
	receive_function(PlType, Function),
	utter(State, '~w~w(~w);'+[Prefix, Function, Name]),
	receive_args(AT, Prefix, State).
receive_args([_|AT], Prefix, State) :- !,
	receive_args(AT, Prefix, State).

receive_function(integer, receive_int) :- !.
receive_function(atom,    receive_atom) :- !.
receive_function(float,   receive_float) :- !.
receive_function(_,       receive).


		 /*******************************
		 *  COMPOUND TYPE SEND/RECEIVE	*
		 *******************************/

gen_send_functions(Functions, State, _Options) :-
	compound_types(Functions, in, SendTypes),
	(   SendTypes == []
	->  true
	;   utter(State, '/* Send compound types to Prolog */'),
	    forall(member(Module:Type, SendTypes),
		   gen_send_function(State, Module, Type)),
	    utter(State, [nl, nl])
	).
	
gen_send_function(State, Module, Type) :-
	current_type(Module:Type, [PrologType]),
	functor(PrologType, TermName, Arity),
	compound_cpp_type(Module, PrologType, CppType),
	utter(State,
	      [ 'void send(const ~w &data)'+[CppType],
	        begin,
		'send_begin_term("~w", ~d);'+[TermName, Arity]
	      ]),
	arg_types_and_names(PrologType, Module, in, Args),
	gen_send_fields(Args, 1, PrologType, State),
	utter(State,
	      [ 'send_end_term();',
		end
	      ]).

gen_send_fields([], _, _, _).
gen_send_fields([a(_Name, PlType, _, _)|AT], I, PlT, State) :-
	arg(I, PlT, Name),
	send_function(PlType, Function),
	utter(State,
	      [ '~w(data.get_~w());'+[Function, Name]
	      ]),
	I2 is I + 1,
	gen_send_fields(AT, I2, PlT, State).



gen_receive_functions(Functions, State, _Options) :-
	compound_types(Functions, out, RecTypes),
	(   RecTypes == []
	->  true
	;   utter(State, '/* Receive compound types to Prolog */'),
	    forall(member(Module:Type, RecTypes),
		   gen_receive_function(State, Module, Type)),
	    utter(State, [nl, nl])
	).

gen_receive_function(State, Module, Type) :-
	current_type(Module:Type, [PrologType]),
	functor(PrologType, TermName, Arity),
	compound_cpp_type(Module, PrologType, CppType),
	utter(State,
	      [ 'void receive(~w &data)'+[CppType],
	        begin,
		'receive_begin_term("~w", ~d);'+[TermName, Arity]
	      ]),
	arg_types_and_names(PrologType, Module, out, Args),
	gen_receive_fields(Args, State),
	maplist(to_call_arg, Args, CArgs),
	concat_atom(CArgs, ', ', CallArgs),
	utter(State,
	      [ 'data.initialize(~w);'+[CallArgs],
		'receive_end_term();',
		end
	      ]).

gen_receive_fields([], _).
gen_receive_fields([a(Name, PlType, _, CType)|AT], State) :-
	receive_function(PlType, Function),
	utter(State,
	      [ '~w ~w;'+[CType, Name],
		'~w(~w);'+[Function, Name]
	      ]),
	gen_receive_fields(AT, State).

to_call_arg(a(Name,_,_,_), Name).


%	compound_types(+Goals, +InOut, -Types)
%	
%	Returns the in-  our  out-types   that  require  compound access
%	functions used by all  Goals  for   which  we  will  generate an
%	interface function.

compound_types(Goals, InOut, Types) :-
	io_term(InOut, Term, Type),
	findall(Module:Type,
		(   member(f(Module, Goal, _A), Goals),
		    arg(_, Goal, Term),
		    current_type(Module:Type, [Expanded]),
		    \+ primitive_cpp_type(Expanded, _)
		),
		Types).

io_term(in,  +Type, Type).
io_term(out, -Type, Type).
		    

		 /*******************************
		 *	    QUERY CLASSES	*
		 *******************************/

%	gen_query_classes(+Out, +NonDetFunctions, +Options)

gen_query_classes(_, [], _) :- !.
gen_query_classes(Out, NonDetFunctions, Options) :-
	output_state(Out, State),
	utter(State,
	      [ nl, nl,
		'/* Classes for non-deterministic queries */'
	      ]),
	forall(member(F, NonDetFunctions),
	       gen_query_class(State, F, Options)).

gen_query_class(State, Function, Options) :-
	Function = f(Module, Goal, _Attributes),
	functor(Goal, Pred, Arity),
	function_name(Function, ClassName),
	option(server_class(Proxy), Options, 'MyProxy'),
	goal_arg_types_and_names(Goal, Module, ArgTNs),
	maplist(to_arg, ArgTNs, CPPArgs),
	concat_atom(CPPArgs, ', ', Args),
	utter(State,
	      [ nl, nl,
		'class ~w : public PlQuery'+[ClassName],
		begin,
		'~w *proxy;'+[Proxy], nl, nl,
		label(public),

		'~w(~w *s)'+[ClassName, Proxy],
		begin,
		'proxy = s;',
		'proxy->openQuery("~w", "~w", ~d);'+[Module, Pred, Arity],
		end,

		'~~~w()'+[ClassName],
		begin,
		'proxy->closeQuery();',
		end,

		nl, nl,
		'int next_solution(~w)'+[Args],
		begin
	      ]),
	(    memberchk(a(_,_,in,_), ArgTNs)
	->   utter(State,
		   [ 'if ( proxy->get_status() == QSTAT_OPEN )',
		     begin
		   ]),
	     send_args(ArgTNs, 'proxy->', State),
	     utter(State,
		   [ end
		   ])
	;    true
	),
	utter(State,
	      [ 'if ( proxy->runNonDetQuery() )',
		begin
	      ]),
	receive_args(ArgTNs, 'proxy->', State),
	utter(State,
	      [ 'return TRUE;',
		end,
		'return FALSE;',
		end,

		end_class
	      ]).
		

	
		 /*******************************
		 *	      OUTPUT		*
		 *******************************/

output_state(Out, state(Out, 0, [])).

utter(State, Term) :-
	arg(1, State, Out),
	emit(Term, Out, State).

emit([], _, _) :- !.
emit([H|T], Out, State) :- !,
	emit(H, Out, State),
	emit(T, Out, State).
emit(nl, Out, _State) :- !,
	nl(Out).
emit(indent, Out, State) :- !,
	indent(State, X),
	line_position(Out, OldPos0),
	(   X < OldPos0
	->  nl(Out),
	    OldPos = 0
	;   OldPos = OldPos0
	),
	Tabs is (X-OldPos) // 8,
	Spaces is (X-OldPos) mod 8,
	forall(between(1, Tabs, _), put(Out, 9)),
	forall(between(1, Spaces, _), put(Out, 32)).
emit(push, Out, State) :- !,
	line_position(Out, Indent),
	push(State, Indent).
emit(pop, _, State) :- !,
	pop(State).
emit(begin, Out, State) :- !,
	emit(indent, Out, State),
	format(Out, '{ ', []),
	indent(State, X),
	Y is X + 2,
	push(State, Y).
emit(end, Out, State) :- !,
	pop(State),
	emit(indent, Out, State),
	format(Out, '}', []).
emit(end_class, Out, State) :- !,
	pop(State),
	emit(indent, Out, State),
	format(Out, '};', []).
emit(indent(Inc), Out, State) :- !,
	indent(State, X),
	Y is X + Inc,
	push(State, Y),
	emit(indent, Out, State).
emit(undent, Out, State) :- !,
	pop(State),
	emit(indent, Out, State).
emit(label(Label), Out, State) :- !,
	emit(indent(-2), Out, State),
	format(Out, '~w:~n', [Label]),
	emit(pop, Out, State).
emit(Fmt+Args, Out, State) :- !,
	emit(indent, Out, State),
	format(Out, Fmt, Args).
emit(Fmt, Out, State) :-
	emit(indent, Out, State),
	format(Out, Fmt, []).

indent(state(_, Indent, _), Indent).

push(State, Indent) :-
	arg(2, State, Old),
	arg(3, State, Stack),
	setarg(3, State, [Old|Stack]),
	setarg(2, State, Indent).
pop(State) :-
	arg(3, State, [Indent|Old]),
	setarg(2, State, Indent),
	setarg(3, State, Old).


		 /*******************************
		 *		UTIL		*
		 *******************************/

%	clean_name(+Raw, -Cleaned)
%	
%	Turn Raw into a name  suitable  as   a  C  symbol by turning all
%	invalid characters into _.

clean_name(Raw, Cleaned) :-
	atom_codes(Raw, Codes),
	clean_codes(Codes, CleanCodes),
	atom_codes(Cleaned, CleanCodes).

clean_codes([], []).
clean_codes([H0|T0], [H|T]) :-
	(   code_type(H0, csym)
	->  H = H0
	;   H = 0'_
	),
	clean_codes(T0, T).
