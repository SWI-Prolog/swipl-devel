/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
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

:- module(pce_principal,
	  [ new/2, free/1,

	    send/2, send/3, send/4, send/5, send/6, send/7,
	    send/8,

	    get/3, get/4, get/5, get/6, get/7, get/8,

	    send_class/3,
	    get_class/4,

	    object/1, object/2,

	    pce_class/6,
	    pce_lazy_send_method/3,
	    pce_lazy_get_method/3,
	    pce_uses_template/2,

	    pce_method_implementation/2,

	    pce_open/3,			% +Object, +Mode, -Stream
	    in_pce_thread/1,		% :Goal
	    set_pce_thread/0,
	    pce_dispatch/0,

	    op(200, fy,  @),
	    op(250, yfx, ?),
	    op(990, xfx, :=)
	  ]).


:- meta_predicate
	send_class(+, +, :),
	send(+, :),
	send(+, :, +),
	send(+, :, +, +),
	send(+, :, +, +, +),
	send(+, :, +, +, +, +),
	send(+, :, +, +, +, +, +),

	get_class(+, +, :, -),
	get(+, :, -),
	get(+, :, +, -),
	get(+, :, +, +, -),
	get(+, :, +, +, +, -),
	get(+, :, +, +, +, +, -),
	get(+, :, +, +, +, +, +, -),

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

:- pce_host:'$load_pce'.
:- initialization pce_host:'$load_pce'.

pce_ifhostproperty(prolog(sicstus), [
(send(Object, Message) :-
	pce_host:send(Object, Message, 1)),
(get(Object, Message, Return) :-
	pce_host:get(Object, Message, Return, 1)),
(send_class(Object, Class, Message) :-
	pce_host:send(Object, Class, Message, 1)),
(get(Object, Class, Message, Return) :-
	pce_host:get(Object, Class, Message, Return, 1)),
(object(Object) :-
	pce_host:object(Object, 1)),
(object(Object, Term) :-
	pce_host:object(Object, Term, 1)),
(new(Object, Term) :-
	pce_host:new(Object, Term, 1))
]).

:- noprofile((send_implementation/3,
	      get_implementation/4,
	      send_class/3,
	      get_class/4,
	      new/2,
	      send/2,
	      get/3)).


		/********************************
		*          PROLOG LAYER		*
		********************************/


%%   	free(+Ref) is det.
%
%	Delete object if it exists.

free(Ref) :-
	object(Ref), !,
	send(Ref, free).
free(_).


%%	send(+Object, +Selector, +Arg...) is semidet.
%
%	Succeeds if sending a message to Object with Selector and the
%	given Arguments succeeds. Normally, goal_expansion/2 expands all
%	these goals into send(Receiver, Method(Args...)).

send(Receiver, M:Selector, A1) :-
        functor(Message, Selector, 1),
        arg(1, Message, A1),
        send(Receiver, M:Message).

send(Receiver, M:Selector, A1, A2) :-
        functor(Message, Selector, 2),
        arg(1, Message, A1),
        arg(2, Message, A2),
        send(Receiver, M:Message).

send(Receiver, M:Selector, A1, A2, A3) :-
        functor(Message, Selector, 3),
        arg(1, Message, A1),
        arg(2, Message, A2),
        arg(3, Message, A3),
        send(Receiver, M:Message).

send(Receiver, M:Selector, A1, A2, A3, A4) :-
        functor(Message, Selector, 4),
        arg(1, Message, A1),
        arg(2, Message, A2),
        arg(3, Message, A3),
        arg(4, Message, A4),
        send(Receiver, M:Message).

send(Receiver, M:Selector, A1, A2, A3, A4, A5) :-
        functor(Message, Selector, 5),
        arg(1, Message, A1),
        arg(2, Message, A2),
        arg(3, Message, A3),
        arg(4, Message, A4),
        arg(5, Message, A5),
        send(Receiver, M:Message).

send(Receiver, M:Selector, A1, A2, A3, A4, A5, A6) :-
        functor(Message, Selector, 6),
        arg(1, Message, A1),
        arg(2, Message, A2),
        arg(3, Message, A3),
        arg(4, Message, A4),
        arg(5, Message, A5),
        arg(6, Message, A6),
        send(Receiver, M:Message).


%%	get(+Object, :Selector, +Arg..., ?Rval) is semidet.
%
%	See the comments with send/[3-12].

get(Receiver, M:Selector, A1, Answer) :-
        functor(Message, Selector, 1),
        arg(1, Message, A1),
        get(Receiver, M:Message, Answer).

get(Receiver, M:Selector, A1, A2, Answer) :-
        functor(Message, Selector, 2),
        arg(1, Message, A1),
        arg(2, Message, A2),
        get(Receiver, M:Message, Answer).

get(Receiver, M:Selector, A1, A2, A3, Answer) :-
        functor(Message, Selector, 3),
        arg(1, Message, A1),
        arg(2, Message, A2),
        arg(3, Message, A3),
        get(Receiver, M:Message, Answer).

get(Receiver, M:Selector, A1, A2, A3, A4, Answer) :-
        functor(Message, Selector, 4),
        arg(1, Message, A1),
        arg(2, Message, A2),
        arg(3, Message, A3),
        arg(4, Message, A4),
        get(Receiver, M:Message, Answer).

get(Receiver, M:Selector, A1, A2, A3, A4, A5, Answer) :-
        functor(Message, Selector, 5),
        arg(1, Message, A1),
        arg(2, Message, A2),
        arg(3, Message, A3),
        arg(4, Message, A4),
        arg(5, Message, A5),
        get(Receiver, M:Message, Answer).


		 /*******************************
		 *	     NEW SEND		*
		 *******************************/

:- multifile
	send_implementation/3,
	get_implementation/4.

%%	send_implementation(+Id, +Message, +Object)
%
%	Method-bodies are compiled into clauses for this predicate. Id
%	is a unique identifier for the implementation, Message is a
%	compound whose functor is the method name and whose arguments
%	are the arguments to the method-call. Object is the receiving
%	object.

send_implementation(true, _Args, _Obj).
send_implementation(fail, _Args, _Obj) :- fail.
send_implementation(once(Id), Args, Obj) :-
	send_implementation(Id, Args, Obj), !.
send_implementation(spy(Id), Args, Obj) :-
	(   current_prolog_flag(debug, true)
	->  trace,
	    send_implementation(Id, Args, Obj)
	;   send_implementation(Id, Args, Obj)
	).
send_implementation(trace(Id), Args, Obj) :-
	pce_info(pce_trace(enter, send_implementation(Id, Args, Obj))),
	(   send_implementation(Id, Args, Obj)
	->  pce_info(pce_trace(exit, send_implementation(Id, Args, Obj)))
	;   pce_info(pce_trace(fail, send_implementation(Id, Args, Obj)))
	).


%%	get_implementation(+Id, +Message, +Object, -Return)
%
%	As send_implementation/3, but for get-methods.

get_implementation(true, _Args, _Obj, _Rval).
get_implementation(fail, _Args, _Obj, _Rval) :- fail.
get_implementation(once(Id), Args, Obj, Rval) :-
	get_implementation(Id, Args, Obj, Rval), !.
get_implementation(spy(Id), Args, Obj, Rval) :-
	(   current_prolog_flag(debug, true)
	->  trace,
	    get_implementation(Id, Args, Obj, Rval)
	;   get_implementation(Id, Args, Obj, Rval)
	).
get_implementation(trace(Id), Args, Obj, Rval) :-
	pce_info(pce_trace(enter, get_implementation(Id, Args, Obj, Rval))),
	(   get_implementation(Id, Args, Obj, Rval)
	->  pce_info(pce_trace(exit, get_implementation(Id, Args, Obj, Rval)))
	;   pce_info(pce_trace(fail, get_implementation(Id, Args, Obj, Rval))),
	    fail
	).

%	SWI-Prolog: make this a normal user (debug-able) predicate.

pce_ifhostproperty(prolog(swi), [
(:- '$set_predicate_attribute'(send_implementation(_,_,_),  system,      0)),
(:- '$set_predicate_attribute'(get_implementation(_,_,_,_), system,      0)),
(:- '$set_predicate_attribute'(send_implementation(_,_,_),  hide_childs, 0)),
(:- '$set_predicate_attribute'(get_implementation(_,_,_,_), hide_childs, 0))
				]).

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
