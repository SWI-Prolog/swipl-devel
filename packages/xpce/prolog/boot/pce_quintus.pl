% File:		quintus_pce.pl
% Module:	pce
% Where:	PCE-3/Quintus Saved State
% Purpose:	Interface between Quintus Prolog and PCE-3
% Author:	Anjo Anjewierden, anjo@swivax.uucp
% Notice:	Copyright (c) University of Amsterdam.  All rights reserved.
% Works with:	Quintus Prolog 2.4, PCE-3.6
% History:	28/07/88 (AA; Created)
%		06/08/88 (AA; Documented)
%		31/05/89 (JW; Modules in messages)


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This modules contains the Prolog part of the interface between Quintus
2.4  and  PCE 3.6.   It  is left  here for  reference  only.  It needs
updating to meet the new PCE interface requirements as well as  to use
Quintus callback implemented from version 3.0

	07/06/91, Jan Wielemaker
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- end_of_file.					  % ignore this file!

:- module(pce,
	[ object/1
	, object/2
	, new/2
	, send/2
	, send/3
	, send/4
	, send/5
	, send/6
	, send/7
	, send/8
	, get/3
	, get/4
	, get/5
	, get/6
	, get/7
	, get/8
	, get/9
	, get_object/3
	, get_object/4
	, get_object/5
	, get_object/6
	, get_object/7
	, get_object/8
	, get_object/9
	, get_chain/3
	, get_chain_object/3
	, chain_list/2
	, chain_list_object/2
	, pce_go/0
	, pce_loop/1

	, make_event_obtainers/0	/* exports from event_speak */
	, make_handlers/1
	, make_handlers/2
	, attach_handlers/2

	, new_bitmap/2			/* bitmap.pl */
	, new_icon/2
	, new_cursor/2
	, load_bitmap/2

	, write_pce_banner/0		/* util.pl */
	, pce_environment/2
	, pce_find_file/3

	, tracepce/0
	, notracepce/0
	, debugpce/0
	, nodebugpce/0
	, debugpce/1
	, nodebugpce/1
	, spypce/1
	, nospypce/1
	, manpce/1
	]).

:- meta_predicate
	pce_loop(:).

:- meta_predicate
	new(:, +),
	send(:, +),
	send(:, +, +),
	send(:, +, +, +),
	send(:, +, +, +, +),
	send(:, +, +, +, +, +),
	send(:, +, +, +, +, +, +),
	send(:, +, +, +, +, +, +, +),
	get(:, +, -),
	get(:, +, +, -),
	get(:, +, +, +, -),
	get(:, +, +, +, +, -),
	get(:, +, +, +, +, +, -),
	get(:, +, +, +, +, +, +, -),
	get(:, +, +, +, +, +, +, +, -).

:- ensure_loaded(
	[ library(strings)
	, library(basics)
	]).

:- ensure_loaded(
	[ library(event_speak)
	, library(bitmap)
	, library(util)
	, library(debug)
	]).

%   strip_module(+Complex, -Term)
%   Strip the `Complex', which is of the form Module:Term, into its module
%   part and its term part.  Return the term part via `Term' and give the
%   module to the C part of the interface.

strip_module(Raw, Term) :-
	strip_module(Raw, Term, user).

strip_module(Spec, Term, _) :-
	nonvar(Spec),
	Spec = Module:Rest, !,
	strip_module(Rest, Term, Module).
strip_module(Term, Term, Module) :-
	pce_sys:qp_module(Module, succeed).	% pass to C

%   [PURPOSE].  Together with the C program "itf-quintus.c" this module 
%   implements the interface between Quintus Prolog and PCE-3.

%   [OBJECTIVES].  Some predicates defined in this module are called millions
%   of times.  We have attempted to make them as efficient as possible
%   without sacrificing readability and conceptual clarity.  It is thought 
%   significant performance gains can only be achieved by writing all 
%   predicates in a language close to the hardware (i.e. C or assembler).  
%   Given the non-optimised implementation of PCE's virtual machine, which
%   the predicates defined in this module call, this appears to be a waste of 
%   time.

%   [REQUIRED READING].  The following documents are relevant:
%	- PCE-3 Programmers Manual [C1.3].
%	  Describes the predicates available in PCE/Prolog from the 
%         programmers point of view.  This module implements these
%	  predicates.
%	- Interfacing PCE-3 to a Host Language via C [C1.2].
%	  Describes what PCE provides to make it accessible from the
%	  host language.
%	- Quintus Prolog foreign function interface.
%	  Chapter 5 in the section on System Dependent Features of the
%	  Quintus Prolog documentation.

%   [DESIGN].  The module is structured around "message vectors".  A
%   message vector (vector for short) is an abstraction for messages
%   as vectors.  A message vector has a number of constituent parts:
%
%	object:	A PCE object name, sometimes called the receiver.
%	type:	An atom describing how the receiver interpretes the
%		message vector.  For "new" and the vector returned by
%		"get" it is the name of a class, for "send" and "get"
%		it is the selector invoking method.
%	argc:	The number of arguments in argv.
%	argv:	A vector of arguments.
%
%   This design enables us to talk about message vectors on which operations
%   can be performed rather than talking about "send" takes such and such
%   arguments.  The scheme followed by all predicates in this module is
%   therefore similar:
%
%	1. Create message vector.
%	2. Specify arguments of the message vector.
%	3. Invoke operation on message vector.
%	4. Check status of result.
%	5. Clean up.
%	6. Return results to the caller.

%   [TERMINOLOGY].  Some terms used in this document are peculiar to PCE.
%   The term "assoc" refers to a PCE object name as an atom.  For
%   example, in @hello, "hello" is an assoc with the object @hello 
%   represents.  The term "ref" refers to a PCE object as an integer,
%   e.g. in @123456 "123456" is a ref.  If an object has an assoc it does not 
%   have a reference and vice versa as far as this module is concerned.


%
%	PCE-3 PREDICATES
%


%   object(+@Object)
%
%   Succeeds if Object is the object name for an existing PCE object.

object(@Object) :-
	pce_exists(Object).

pce_exists(Object) :-
	atom(Object), !,
	pce_sys:qp_assoc_exists(Object, succeed).
pce_exists(Object) :-
	integer(Object),
	pce_sys:qp_ref_exists(Object, succeed).


%   object(+@Object, -Class(...-Arguments...))
%
%   Class is the class of which Object is an instance, Arguments make
%   up the object description of Object.
%
%   [NOTE].  object/2 can be written entirely in terms of the PCE virtual
%   machine (using <-class_name, <-arity and <-arg, N).

object(@Object, Description) :-
	pce_object(Object, Vector),
	pce_sys:qp_class(Vector, Class, Arity, succeed),
	pce_object(Vector, Class, Arity, Description).

pce_object(Object, Vector) :-
	atom(Object), !,
	pce_sys:qp_object_assoc(Object, Vector, succeed).
pce_object(Object, Vector) :-
	integer(Object),
	pce_sys:qp_object_ref(  Object, Vector, succeed).

pce_object(Vector, Class, Arity, Description) :-
	functor(Description, Class, Arity),
	pce_args(Arity, Vector, Description),
	pce_sys:qp_free(Vector, _), !.
pce_object(Vector, _, _, _) :-
	pce_sys:qp_free(Vector, _), !, fail.

pce_args(0, _, _) :- !.
pce_args(N, Vector, Description) :-
	arg(N, Description, Arg),
	pce_arg(N, Vector, Arg),
	P is N-1,					% succ/2
	pce_args(P, Vector, Description).

pce_arg(N, Vector, Value) :-
	pce_sys:qp_arg(Vector, N, Type, Int, Atom, Float, succeed),
	pce_unify(Type, Int, Atom, Float, Value).


%   new(?@Object, +Class(...+Arguments...))
%
%   Creates an instance of Class with the initialising Arguments.  The object
%   created is associated with Object.  Object is locked against being
%   freed by the PCE garbage collector.

new(ModObject, Term) :-
	strip_module(ModObject, @Object),
	pce_new(Object, Term, on).

pce_new(Object, Term, Lock) :-
	atom(Object), !,
	pce_sys:qp_assoc_exists(Object, fail),
	pce_create(Term, Result, Lock),
	pce_sys:qp_associate(Result, Object, _).
pce_new(Object, Term, Lock) :-
	var(Object),
	pce_create(Term, Object, Lock).


pce_create(Term, Object, Lock) :-
	functor(Term, Class, Argc),
	pce_sys:qp_vector(Class, Argc, Vector, succeed),
	pce_term_to_vector(Argc, Term, Vector),
	pce_sys:qp_new(Vector, Lock, Object, Status),
	pce_sys:qp_free(Vector, _),
	!,
	Status = succeed.


%   send(+@Object, +Selector, ...+Arguments...)
%
%   Sends a message to Object telling it to invoke the method
%   associated with Selector with the optional Arguments.  Succeeds
%   if the method returns success, fails otherwise.  All arguments
%   must be ground.

send(Arg1, Arg2) :-
	strip_module(Arg1, A1),			% pass module to C
	send2(A1, Arg2).

send2([], _) :- !.
send2(_, []) :- !.
send2([Object|Objects], Selector) :- !,
	send2(Object, Selector),
	send2(Objects, Selector).
send2(Object, [Selector|Selectors]) :- !,
	send2(Object, Selector),
	send2(Object, Selectors).
send2(Object, Selector) :- !,
	pce_send(Object, Selector).

send(Arg1, Arg2, Arg3) :-
	strip_module(Arg1, A1),			% pass module to C
	send3(A1, Arg2, Arg3).

send3([], _, _) :- !.
send3(_, [], _) :- !.
send3(_, _, []) :- !.
send3([Object|Objects], Selector, Arg) :- !,
	send3(Object, Selector, Arg),
	send3(Objects, Selector, Arg).
send3(Object, [Selector|Selectors], Arg) :- !,
	send3(Object, Selector, Arg),
	send3(Object, Selectors, Arg).
send3(Object, Selector, [Arg|Args]) :- !,
	send3(Object, Selector, Arg),
	send3(Object, Selector, Args).
send3(Object, Selector, Arg) :- !,
	pce_send(Object, Selector, Arg).

send(ModObject, Selector, Arg1, Arg2) :- !,
	strip_module(ModObject, Object),
	pce_send(Object, Selector, Arg1, Arg2).

send(ModObject, Selector, Arg1, Arg2, Arg3) :- !,
	strip_module(ModObject, Object),
	pce_send(Object, Selector, Arg1, Arg2, Arg3).

send(ModObject, Selector, Arg1, Arg2, Arg3, Arg4) :- !,
	strip_module(ModObject, Object),
	pce_send(Object, Selector, Arg1, Arg2, Arg3, Arg4).

send(ModObject, Selector, Arg1, Arg2, Arg3, Arg4, Arg5) :- !,
	strip_module(ModObject, Object),
	pce_send(Object, Selector, Arg1, Arg2, Arg3, Arg4, Arg5).

send(ModObject, Selector, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6) :- !,
	strip_module(ModObject, Object),
	pce_send(Object, Selector, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6).


%   pce_send(+Object, +Selector, ...+Arguments...)
%
%   Sends a message to Object telling it to invoke the method
%   object_assocd with Selector with the optional Arguments.  Succeeds
%   if the method returns success, fails otherwise.  All arguments
%   must be ground.
%
%   pce_send/2.. is implemented by setting up a message vector
%   (pce_sys:qp_assoc_vector/5, qp_ref_vector/5), storing the arguments
%   (pce_arg_vector/3) and then sending the message vector (pce_sys:qp_send/2).

pce_send(Object, Selector) :-
	pce_vector(Object, Selector, 0, Vector),
	pce_send(Vector).

pce_send(Object, Selector, Arg) :-
	pce_vector(Object, Selector, 1, Vector),
	pce_arg_vector(Arg, 1, Vector),
	pce_send(Vector).

pce_send(Object, Selector, Arg1, Arg2) :-
	pce_vector(Object, Selector, 2, Vector),
	pce_arg_vector(Arg1, 1, Vector),
	pce_arg_vector(Arg2, 2, Vector),
	pce_send(Vector).

pce_send(Object, Selector, Arg1, Arg2, Arg3) :-
	pce_vector(Object, Selector, 3, Vector),
	pce_arg_vector(Arg1, 1, Vector),
	pce_arg_vector(Arg2, 2, Vector),
	pce_arg_vector(Arg3, 3, Vector),
	pce_send(Vector).

pce_send(Object, Selector, Arg1, Arg2, Arg3, Arg4) :-
	pce_vector(Object, Selector, 4, Vector),
	pce_arg_vector(Arg1, 1, Vector),
	pce_arg_vector(Arg2, 2, Vector),
	pce_arg_vector(Arg3, 3, Vector),
	pce_arg_vector(Arg4, 4, Vector),
	pce_send(Vector).

pce_send(Object, Selector, Arg1, Arg2, Arg3, Arg4, Arg5) :-
	pce_vector(Object, Selector, 5, Vector),
	pce_arg_vector(Arg1, 1, Vector),
	pce_arg_vector(Arg2, 2, Vector),
	pce_arg_vector(Arg3, 3, Vector),
	pce_arg_vector(Arg4, 4, Vector),
	pce_arg_vector(Arg5, 5, Vector),
	pce_send(Vector).

pce_send(Object, Selector, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6) :-
	pce_vector(Object, Selector, 6, Vector),
	pce_arg_vector(Arg1, 1, Vector),
	pce_arg_vector(Arg2, 2, Vector),
	pce_arg_vector(Arg3, 3, Vector),
	pce_arg_vector(Arg4, 4, Vector),
	pce_arg_vector(Arg5, 5, Vector),
	pce_arg_vector(Arg6, 6, Vector),
	pce_send(Vector).


pce_send(Vector) :-
	pce_sys:qp_send(Vector, Status),
	pce_sys:qp_free(Vector, _),
	!,
	Status = succeed.


get(ModObject, Selector, Value) :-
	strip_module(ModObject, Object),
	pce_vector(Object, Selector, 0, Vector),
	pce_get(Vector, Value).

get(ModObject, Selector, Arg, Value) :-
	strip_module(ModObject, Object),
	pce_vector(Object, Selector, 1, Vector),
	pce_arg_vector(Arg, 1, Vector),
	pce_get(Vector, Value).

get(ModObject, Selector, Arg1, Arg2, Value) :-
	strip_module(ModObject, Object),
	pce_vector(Object, Selector, 2, Vector),
	pce_arg_vector(Arg1, 1, Vector),
	pce_arg_vector(Arg2, 2, Vector),
	pce_get(Vector, Value).

get(ModObject, Selector, Arg1, Arg2, Arg3, Value) :-
	strip_module(ModObject, Object),
	pce_vector(Object, Selector, 3, Vector),
	pce_arg_vector(Arg1, 1, Vector),
	pce_arg_vector(Arg2, 2, Vector),
	pce_arg_vector(Arg3, 3, Vector),
	pce_get(Vector, Value).

get(ModObject, Selector, Arg1, Arg2, Arg3, Arg4, Value) :-
	strip_module(ModObject, Object),
	pce_vector(Object, Selector, 4, Vector),
	pce_arg_vector(Arg1, 1, Vector),
	pce_arg_vector(Arg2, 2, Vector),
	pce_arg_vector(Arg3, 3, Vector),
	pce_arg_vector(Arg4, 4, Vector),
	pce_get(Vector, Value).

get(ModObject, Selector, Arg1, Arg2, Arg3, Arg4, Arg5, Value) :-
	strip_module(ModObject, Object),
	pce_vector(Object, Selector, 5, Vector),
	pce_arg_vector(Arg1, 1, Vector),
	pce_arg_vector(Arg2, 2, Vector),
	pce_arg_vector(Arg3, 3, Vector),
	pce_arg_vector(Arg4, 4, Vector),
	pce_arg_vector(Arg5, 5, Vector),
	pce_get(Vector, Value).

get(ModObject, Selector, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Value) :-
	strip_module(ModObject, Object),
	pce_vector(Object, Selector, 6, Vector),
	pce_arg_vector(Arg1, 1, Vector),
	pce_arg_vector(Arg2, 2, Vector),
	pce_arg_vector(Arg3, 3, Vector),
	pce_arg_vector(Arg4, 4, Vector),
	pce_arg_vector(Arg5, 5, Vector),
	pce_arg_vector(Arg6, 6, Vector),
	pce_get(Vector, Value).


pce_get(Vector, Value) :-
	pce_sys:qp_get(Vector, succeed),
	pce_sys:qp_object(Vector, Type, Int, Atom, Float, succeed),
	pce_unify(Type, Int, Atom, Float, Value),
	pce_sys:qp_free(Vector, _), !.
pce_get(Vector, _) :-
	pce_sys:qp_free(Vector, _), !, fail.


%   pce_arg_vector(+Object, +N, +Vector)
%
%   Store Object as the N'th argument in Vector.  (I'm not sure
%   whether there is a general rule for the order of arguments
%   in predicates like this).  Calls the appropriate procedure
%   in the C-part of the interface.

pce_arg_vector(@Object, N, Vector) :- !,
	(   integer(Object) -> pce_sys:qp_ref(  Vector, N, Object, succeed)
	;   atom(Object)    -> pce_sys:qp_assoc(Vector, N, Object, succeed)
	).
pce_arg_vector(Value, N, Vector) :-
	(   integer(Value)  -> pce_sys:qp_int(  Vector, N, Value, succeed)
	;   atom(Value)     -> pce_sys:qp_atom( Vector, N, Value, succeed)
	;   float(Value)    -> pce_sys:qp_float(Vector, N, Value, succeed)
	;   var(Value)      -> pce_sys:qp_free( Vector, _), !, fail
	;   pce_new(Object, Value, nolock),
	    pce_sys:qp_ref(Vector, N, Object, succeed)
	).



pce_term_to_vector(0, _, _).
pce_term_to_vector(N, Term, Vector) :-
	arg(N, Term, Arg),
	pce_arg_vector(Arg, N, Vector),
	M is N-1,				% succ/2
	pce_term_to_vector(M,Term, Vector).


%   pce_unify(+Result, +Value)
%
%   Value is a term given by the user.  pce_unify/2 succeeds iff Value
%   "unifies" with Result, replacing object names in Result by object
%   descriptions as appropriate.
%
%   For example:
%
%	pce_unify(@p, point(X,Y)).
%
%   succeeds when @p is a point object, binding X and Y.
%
%   [IDEA].  We could remove the cut in the first clause such that 
%   alternative descriptions are generated.  Perhaps it is useful to get rid 
%   of the distinction between get/3.. and get_object/3... and the 
%   distinction between PCE strings and PCE names.  In the latter case, if 
%   we also define the object description of a string to be the text it 
%   contains, this program:
%
%	new(@ch, chain(string(hello), world)),
%	get(@ch, head, H),
%	atom(H),
%	write(H), nl.
%
%   would write/1 'hello'.  Given the current definition in PCE-3 the cut is 
%   necessary, to prevent this.
%
%   Those who have seen the source code for the PCE/Prolog interface in C, 
%   may appreciate pce_unify/2.

pce_unify(Result, Result) :- !.
pce_unify(Result, Value) :-
	(   atomic(Value)
	;   atomic(Result)
	;   functor(Value,  @, 1)
	),
	!, fail.
pce_unify(Result, Value) :-
	object(Result, Value).


%   pce_unify(+Type, +Int, +Atom, +Float, -Value)
%
%   Unifies Value with one the other arguments depending on Type.
 
pce_unify(atom,    _, A, _, A).
pce_unify(default, _, _, _, @default).
pce_unify(float,   _, _, F, F).
pce_unify(integer, I, _, _, I).
pce_unify(nil,     _, _, _, @nil).
pce_unify(off,     _, _, _, @off).
pce_unify(on,      _, _, _, @on).
pce_unify(assoc,   _, A, _, Value) :- pce_unify(@A, Value).
pce_unify(ref,     R, _, _, Value) :- pce_unify(@R, Value).


%   pce_unify_object(+Type, +Int, +Atom, +Float, -Value)
%
%   Unifies Value with one the other arguments depending on Type.
%   The difference with pce_unify/6 is that object names are expanded
%   to object descriptions first.
 
pce_unify_object(atom,    _, A, _, A).
pce_unify_object(default, _, _, _, @default).
pce_unify_object(float,   _, _, F, F).
pce_unify_object(integer, I, _, _, I).
pce_unify_object(nil,     _, _, _, @nil).
pce_unify_object(off,     _, _, _, @off).
pce_unify_object(on,      _, _, _, @on).
pce_unify_object(assoc,   _, A, _, Value) :- object(@A, Value).
pce_unify_object(ref,     R, _, _, Value) :- object(@R, Value).


%   chain_list(+@Chain, -List)
%

chain_list(@nil, List) :- !, List = [].
chain_list(@Chain, List) :-
	pce_object(Chain, Vector),
	chain_list2(Vector, List).

chain_list2(Vector, List) :-
	pce_sys:qp_class(Vector, chain, Arity, succeed),
	chain_list_args(0, Arity, Vector, List),
	pce_sys:qp_free(Vector, _), !.
chain_list2(Vector, _) :-
	pce_sys:qp_free(Vector, _), !, fail.

chain_list_args(Arity, Arity, _, []) :- !.
chain_list_args(N, Arity, Vector, [X|Xs]) :-
	Next is N+1,
	pce_sys:qp_arg(Vector, Next, Type, Int, Atom, Float, succeed),
	pce_unify(Type, Int, Atom, Float, X),
	chain_list_args(Next, Arity, Vector, Xs).


%   chain_list_object(+@Chain, -List)
%
%   Now that we have a well-defined interface between PCE and Prolog,
%   we can implement this directly, rather than through get's.

chain_list_object(@nil, List) :- !, List = [].
chain_list_object(@Chain, List) :-
	pce_object(Chain, Vector),
	chain_list_object2(Vector, List).

chain_list_object2(Vector, List) :-
	pce_sys:qp_class(Vector, chain, Arity, succeed),
	chain_list_object_args(0, Arity, Vector, List),
	pce_sys:qp_free(Vector, _), !.
chain_list_object2(Vector, _) :-
	pce_sys:qp_free(Vector, _), !, fail.

chain_list_object_args(Arity, Arity, _, []) :- !.
chain_list_object_args(N, Arity, Vector, [X|Xs]) :-
	Next is N+1,
	pce_sys:qp_arg(Vector, Next, Type, Int, Atom, Float, succeed),
	pce_unify_object(Type, Int, Atom, Float, X),
	chain_list_object_args(Next, Arity, Vector, Xs).


get_chain(Object, Selector, List) :-
	get(Object, Selector, Chain),
	chain_list(Chain, List).
get_chain_object(Object, Selector, List) :-
	get(Object, Selector, Chain),
	chain_list_object(Chain, List).


%   pce_vector(+@Object, +Type, +Argc, -Vector)

pce_vector(@Object, Type, Argc, Vector) :-
	atom(Object), !,
	pce_sys:qp_assoc_vector(Object, Type, Argc, Vector, succeed).
pce_vector(@Object, Type, Argc, Vector) :-
	integer(Object), !,
	pce_sys:qp_ref_vector(Object, Type, Argc, Vector, succeed).
pce_vector(Obtainer, Type, Argc, Vector) :-
	functor(Obtainer, ?, _),
	new(@Object, Obtainer),
	pce_sys:qp_ref_vector(Object, Type, Argc, Vector, succeed).


get_object(Object, Selector, Value) :-
	get(Object, Selector, X),
	pce_get_object(X, Value).
get_object(Object, Selector, Arg, Value) :-
	get(Object, Selector, Arg, X),
	pce_get_object(X, Value).
get_object(Object, Selector, Arg1, Arg2, Value) :-
	get(Object, Selector, Arg1, Arg2, X),
	pce_get_object(X, Value).
get_object(Object, Selector, Arg1, Arg2, Arg3, Value) :-
	get(Object, Selector, Arg1, Arg2, Arg3, X),
	pce_get_object(X, Value).
get_object(Object, Selector, Arg1, Arg2, Arg3, Arg4, Value) :-
	get(Object, Selector, Arg1, Arg2, Arg3, Arg4, X),
	pce_get_object(X, Value).
get_object(Object, Selector, Arg1, Arg2, Arg3, Arg4, Arg5, Value) :-
	get(Object, Selector, Arg1, Arg2, Arg3, Arg4, Arg5, X),
	pce_get_object(X, Value).
get_object(Object, Selector, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Value) :-
	get(Object, Selector, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, X),
	pce_get_object(X, Value).

pce_get_object(@on, Value) :- !, Value = @on.
pce_get_object(@off, Value) :- !, Value = @off.
pce_get_object(@nil, Value) :- !, Value = @nil.
pce_get_object(@default, Value) :- !, Value = @default.
pce_get_object(@Object, Value) :- !, object(@Object, Value).
pce_get_object(Value, Value).


%   pce_go/0
%
%   Read a message from PCE, transform it into a goal and then call
%   that goal.  The first definition read:
%
%	pce_go :-
%		get_object(@prolog, message, Message),
%		functor(Message, message, Argc),
%		Arity is Argc-2,
%		arg(2, Message, Functor),
%		functor(Goal, Functor, Arity),
%		construct_goal(Argc, Message, Goal),
%		Goal, !.
%
%	construct_goal(2, _, _) :- !.
%	construct_goal(N, Message, Goal) :-
%		arg(N, Message, Arg),
%		X is N - 2,
%		arg(X, Goal, Arg),
%		M is N - 1,
%		construct_goal(M, Message, Goal).
%
%   The second definition follows a general rule for optimising Prolog
%   programs: do not create a term if you throw it away shortly (Message
%   in this predicate).  Of course, it also takes advantage of the fact
%   that we are in this module.  Thus:
%
%	pce_go :-
%		pce_vector(@prolog, message, 0, V),
%		pce_get(V, @Message),
%		pce_object(Message, Vector),
%		pce_sys:qp_class(Vector, message, Argc, succeed),
%		Arity is Argc - 2,
%		pce_arg(2, Vector, Functor),
%		functor(Goal, Functor, Arity),
%		construct_goal(Argc, Vector, Goal),
%		pce_sys:qp_free(Vector, _),
%		Goal, !.
%
%	construct_goal(2, _, _) :- !.
%	construct_goal(N, Vector, Goal) :-
%		pce_arg(N, Vector, Arg),
%		X is N - 2,
%		arg(X, Goal, Arg),
%		M is N - 1,
%		construct_goal(M, Vector, Goal).
%
%   The implementation we now use is cheating a little.  As we can not use
%   hostSend() as defined in the interface specification an additional
%   interface predicate called pce_sys:qp_goal is defined.  It returns a message
%   vector containing the message obtained from @prolog, the selector and
%   the total number of arguments.
%
%   The final version can only be written when Quintus makes calling
%   predicates from PCE itself possible.

pce_go :-
	pce_sys:qp_goal(Module, Functor, Arity, Vector),
	Argc is Arity + 2,
	functor(Goal, Functor, Arity),
	construct_goal(Arity, Argc, Vector, Goal),
	pce_sys:qp_free(Vector, _),
	Module:Goal, !.

construct_goal(0, _, _, _) :- !.
construct_goal(N, A, Vector, Goal) :-
	pce_arg(A, Vector, Arg),
	arg(N, Goal, Arg),
	NewN is N-1,
	NewA is A-1,
	construct_goal(NewN, NewA, Vector, Goal).


pce_loop(Condition) :-
	repeat,
	    pce_go,
	    Condition.
