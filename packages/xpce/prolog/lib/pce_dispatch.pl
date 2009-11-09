/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org/packages/xpce/
    Copyright (C): 2009, University of Amsterdam

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

:- module(pce_dispatch,
	  [ pce_dispatch/1,		% +Options
	    pce_end_dispatch/0,
	    pce_call/1			% :Goal
	  ]).
:- use_module(library(pce)).

:- meta_predicate
	pce_call(0).

%%	pce_dispatch(+Options) is det.
%
%	Create a new thread =pce= that takes   care  of the XPCE message
%	loop.

pce_dispatch(Options) :-
	thread_create(pce_dispatcher, _, [alias(pce)|Options]).

:- dynamic
	end_pce_dispatcher/1.

pce_dispatcher :-
	set_pce_thread,
	thread_self(Me),
	retractall(pce:pce_thread(_)),
	assert(pce:pce_thread(Me)),
	repeat,
	    catch(send(@display, dispatch), E, true),
	    (	var(E)
	    ->	true
	    ;	print_message(error, E)
	    ),
	retract(end_pce_dispatcher(Sender)),
	thread_send_message(Sender, end_pce_dispatcher).

end(Requester) :-
	assert(end_pce_dispatcher(Requester)).

%%	pce_end_dispatch/0
%
%	End the XPCE dispatcher loop

pce_end_dispatch :-
	thread_self(Me),
	in_pce_thread(end(Me)),
	thread_get_message(end_pce_dispatcher),
	set_pce_thread,
	thread_self(Me),
	retractall(pce:pce_thread(_)),
	assert(pce:pce_thread(Me)).

%%	pce_call(:Goal)
%
%	Run Goal in the XPCE thread.
%
%	@deprecated New code should used in_pce_thread/1.

pce_call(Goal) :-
	in_pce_thread(Goal).
