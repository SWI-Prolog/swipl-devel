/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

:- module(pce_principal,
	  [ new/2, free/1,

	    send/2, send/3, send/4, send/5, send/6, send/7,
	    send/8, send/9, send/10, send/11, send/12,

	    get/3, get/4, get/5, get/6, get/7, get/8,
	    get/9, get/10, get/11, get/12, get/13,

	    send_class/3,
	    get_class/4,

	    object/1, object/2,

	    pce_class/6,
	    pce_lazy_send_method/3,
	    pce_lazy_get_method/3,
	    pce_uses_template/2,

	    pce_method_implementation/4
	  ]).

pce_ifhostproperty(prolog(swi), (:- export(pce_open(_,_,_)))).

:- meta_predicate
	send_class(+, +, :),
	send(+, :),
	send(+, :, +),
	send(+, :, +, +),
	send(+, :, +, +, +),
	send(+, :, +, +, +, +),
	send(+, :, +, +, +, +, +),
	send(+, :, +, +, +, +, +, +),
	send(+, :, +, +, +, +, +, +, +),
	send(+, :, +, +, +, +, +, +, +, +),
	send(+, :, +, +, +, +, +, +, +, +, +),
	send(+, :, +, +, +, +, +, +, +, +, +, +),

	get_class(+, +, :, -),
	get(+, :, -),
	get(+, :, +, -),
	get(+, :, +, +, -),
	get(+, :, +, +, +, -),
	get(+, :, +, +, +, +, -),
	get(+, :, +, +, +, +, +, -),
	get(+, :, +, +, +, +, +, +, -),
	get(+, :, +, +, +, +, +, +, +, -),
	get(+, :, +, +, +, +, +, +, +, +, -),
	get(+, :, +, +, +, +, +, +, +, +, +, -),
	get(+, :, +, +, +, +, +, +, +, +, +, +, -),

	new(?, :).

:- op(100, fx, @).
:- op(150, yfx, ?).
:- op(990, xfx, :=).

		/********************************
		*           LOAD C-PART		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The following predicate must be defined before loading this  file.  It
is  normally defined   in the   prolog-dependant   first  file of  the
interface, called pce_<prolog-name>.pl
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- initialization pce_host:'$load_pce'.


		/********************************
		*          PROLOG LAYER		*
		********************************/


%   free(+Ref)
%   Delete object if it exists.

free(Ref) :-
	object(Ref), !,
	send(Ref, free).
free(_).


%   send(+@Object, +Selector, ...+Arguments...)
%
%   Succeeds if sending a message to Object with Selector and the given
%   Arguments succeeds.

send(Receiver, Selector, A1) :-
        functor(Message, Selector, 1),
        arg(1, Message, A1),
        send(Receiver, Message).

send(Receiver, Selector, A1, A2) :-
        functor(Message, Selector, 2),
        arg(1, Message, A1),
        arg(2, Message, A2),
        send(Receiver, Message).

send(Receiver, Selector, A1, A2, A3) :-
        functor(Message, Selector, 3),
        arg(1, Message, A1),
        arg(2, Message, A2),
        arg(3, Message, A3),
        send(Receiver, Message).

send(Receiver, Selector, A1, A2, A3, A4) :-
        functor(Message, Selector, 4),
        arg(1, Message, A1),
        arg(2, Message, A2),
        arg(3, Message, A3),
        arg(4, Message, A4),
        send(Receiver, Message).

send(Receiver, Selector, A1, A2, A3, A4, A5) :-
        functor(Message, Selector, 5),
        arg(1, Message, A1),
        arg(2, Message, A2),
        arg(3, Message, A3),
        arg(4, Message, A4),
        arg(5, Message, A5),
        send(Receiver, Message).

send(Receiver, Selector, A1, A2, A3, A4, A5, A6) :-
        functor(Message, Selector, 6),
        arg(1, Message, A1),
        arg(2, Message, A2),
        arg(3, Message, A3),
        arg(4, Message, A4),
        arg(5, Message, A5),
        arg(6, Message, A6),
        send(Receiver, Message).

send(Receiver, Selector, A1, A2, A3, A4, A5, A6, A7) :-
        functor(Message, Selector, 7),
        arg(1, Message, A1),
        arg(2, Message, A2),
        arg(3, Message, A3),
        arg(4, Message, A4),
        arg(5, Message, A5),
        arg(6, Message, A6),
        arg(7, Message, A7),
        send(Receiver, Message).

send(Receiver, Selector, A1, A2, A3, A4, A5, A6, A7, A8) :-
        functor(Message, Selector, 8),
        arg(1, Message, A1),
        arg(2, Message, A2),
        arg(3, Message, A3),
        arg(4, Message, A4),
        arg(5, Message, A5),
        arg(6, Message, A6),
        arg(7, Message, A7),
        arg(8, Message, A8),
        send(Receiver, Message).

send(Receiver, Selector, A1, A2, A3, A4, A5, A6, A7, A8, A9) :-
        functor(Message, Selector, 9),
        arg(1, Message, A1),
        arg(2, Message, A2),
        arg(3, Message, A3),
        arg(4, Message, A4),
        arg(5, Message, A5),
        arg(6, Message, A6),
        arg(7, Message, A7),
        arg(8, Message, A8),
        arg(9, Message, A9),
        send(Receiver, Message).

send(Receiver, Selector, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) :-
        functor(Message, Selector, 10),
        arg(1, Message, A1),
        arg(2, Message, A2),
        arg(3, Message, A3),
        arg(4, Message, A4),
        arg(5, Message, A5),
        arg(6, Message, A6),
        arg(7, Message, A7),
        arg(8, Message, A8),
        arg(9, Message, A9),
        arg(10, Message, A10),
        send(Receiver, Message).

%   get(+@Object, +Selector, ...+Arguments..., Rval)
%

get(Receiver, Selector, A1, Answer) :-
        functor(Message, Selector, 1),
        arg(1, Message, A1),
        get(Receiver, Message, Answer).

get(Receiver, Selector, A1, A2, Answer) :-
        functor(Message, Selector, 2),
        arg(1, Message, A1),
        arg(2, Message, A2),
        get(Receiver, Message, Answer).

get(Receiver, Selector, A1, A2, A3, Answer) :-
        functor(Message, Selector, 3),
        arg(1, Message, A1),
        arg(2, Message, A2),
        arg(3, Message, A3),
        get(Receiver, Message, Answer).

get(Receiver, Selector, A1, A2, A3, A4, Answer) :-
        functor(Message, Selector, 4),
        arg(1, Message, A1),
        arg(2, Message, A2),
        arg(3, Message, A3),
        arg(4, Message, A4),
        get(Receiver, Message, Answer).

get(Receiver, Selector, A1, A2, A3, A4, A5, Answer) :-
        functor(Message, Selector, 5),
        arg(1, Message, A1),
        arg(2, Message, A2),
        arg(3, Message, A3),
        arg(4, Message, A4),
        arg(5, Message, A5),
        get(Receiver, Message, Answer).

get(Receiver, Selector, A1, A2, A3, A4, A5, A6, Answer) :-
        functor(Message, Selector, 6),
        arg(1, Message, A1),
        arg(2, Message, A2),
        arg(3, Message, A3),
        arg(4, Message, A4),
        arg(5, Message, A5),
        arg(6, Message, A6),
        get(Receiver, Message, Answer).

get(Receiver, Selector, A1, A2, A3, A4, A5, A6, A7, Answer) :-
        functor(Message, Selector, 7),
        arg(1, Message, A1),
        arg(2, Message, A2),
        arg(3, Message, A3),
        arg(4, Message, A4),
        arg(5, Message, A5),
        arg(6, Message, A6),
        arg(7, Message, A7),
        get(Receiver, Message, Answer).

get(Receiver, Selector, A1, A2, A3, A4, A5, A6, A7, A8, Answer) :-
        functor(Message, Selector, 8),
        arg(1, Message, A1),
        arg(2, Message, A2),
        arg(3, Message, A3),
        arg(4, Message, A4),
        arg(5, Message, A5),
        arg(6, Message, A6),
        arg(7, Message, A7),
        arg(8, Message, A8),
        get(Receiver, Message, Answer).

get(Receiver, Selector, A1, A2, A3, A4, A5, A6, A7, A8, A9, Answer) :-
        functor(Message, Selector, 9),
        arg(1, Message, A1),
        arg(2, Message, A2),
        arg(3, Message, A3),
        arg(4, Message, A4),
        arg(5, Message, A5),
        arg(6, Message, A6),
        arg(7, Message, A7),
        arg(8, Message, A8),
        arg(9, Message, A9),
        get(Receiver, Message, Answer).

get(Receiver, Selector, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, Answer) :-
        functor(Message, Selector, 10),
        arg(1, Message, A1),
        arg(2, Message, A2),
        arg(3, Message, A3),
        arg(4, Message, A4),
        arg(5, Message, A5),
        arg(6, Message, A6),
        arg(7, Message, A7),
        arg(8, Message, A8),
        arg(9, Message, A9),
        arg(10, Message, A10),
        get(Receiver, Message, Answer).


		 /*******************************
		 *	     NEW SEND		*
		 *******************************/

:- multifile
	send_implementation/3,
	get_implementation/4.

send_implementation(true, _Args, _Obj).
send_implementation(fail, _Args, _Obj) :- fail.
send_implementation(once(Id), Args, Obj) :-
	send_implementation(Id, Args, Obj), !.
send_implementation(spy(Id), Args, Obj) :-
	(   '$debugging'		% SWi-Prolog
	->  trace,
	    send_implementation(Id, Args, Obj)
	;   send_implementation(Id, Args, Obj)
	).
send_implementation(trace(Id), Args, Obj) :-
	pce_info(trace(enter, send_implementation(Id, Args, Obj))),
	(   send_implementation(Id, Args, Obj)
	->  pce_info(trace(exit, send_implementation(Id, Args, Obj)))
	;   pce_info(trace(fail, send_implementation(Id, Args, Obj)))
	).
get_implementation(true, _Args, _Obj, _Rval).
get_implementation(fail, _Args, _Obj, _Rval) :- fail.

%	SWI-Prolog: make thus a normal user (debug-able) predicate.

:- '$set_predicate_attribute'(send_implementation(_,_,_),  system,      0).
:- '$set_predicate_attribute'(get_implementation(_,_,_,_), system,      0).
:- '$set_predicate_attribute'(send_implementation(_,_,_),  hide_childs, 0).
:- '$set_predicate_attribute'(get_implementation(_,_,_,_), hide_childs, 0).

		 /*******************************
		 *	    DECLARATIONS	*
		 *******************************/

:- multifile
	pce_class/6,
	pce_lazy_send_method/3,
	pce_lazy_get_method/3,
	pce_uses_template/2.


		 /*******************************
		 *	      @PROLOG		*
		 *******************************/

:- initialization
   (object(@prolog) -> true ; send(@host, name_reference, prolog)).
