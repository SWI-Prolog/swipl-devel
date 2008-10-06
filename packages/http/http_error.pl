/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2005, University of Amsterdam

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

:- module(http_error,
	  [ 
	  ]).
:- use_module(library(debug)).
:- use_module(library(prolog_stack)).

/** <module> Decorate uncaught exceptions with stack-trace

This module decorates uncaught exceptions of the user code with a full
stack-trace. It is based on a hook introduced in SWI-Prolog 5.6.5.
Please note that although loading this module greatly simplifies
debugging, it also provides clues for hackers on how to compromise your
site. The more information you give them, the easier it is to break into
your server!

To use this file, simply load it.

@author	Jan Wielemaker
*/

:- multifile
	user:prolog_exception_hook/4.
:- dynamic
	user:prolog_exception_hook/4.

guard(httpd_wrapper:call_handler/6).	% old version
guard(httpd_wrapper:wrapper/5).
guard(httpd_wrapper:handler_with_output_to/4).

user:prolog_exception_hook(error(E, context(Ctx0,Msg)),
			   error(E, context(prolog_stack(Stack),Msg)),
			   Fr, Guard) :-
	Guard \== none,
	prolog_frame_attribute(Guard, predicate_indicator, Goal),
	debug(http_error, 'Got exception ~p (Ctx0=~p, Catcher=~p)',
	      [E, Ctx0, Goal]),
	guard(Goal),
	get_prolog_backtrace(Fr, 50, Stack0),
	debug(http_error, 'Stack = ~w', [Stack0]),
	clean_stack(Stack0, Stack).

clean_stack([], []).
clean_stack([H|_], [H]) :-
	guard_frame(H), !.
clean_stack([H|T0], [H|T]) :-
	clean_stack(T0, T).

guard_frame(frame(_,clause(ClauseRef, _))) :-
	nth_clause(M:Head, _, ClauseRef),
	functor(Head, Name, Arity),
	guard(M:Name/Arity).
	

		 /*******************************
		 *	     MESSAGES		*
		 *******************************/

:- multifile
	prolog:message/3.

prolog:message(error(Error, context(Stack, Message))) -->
	{ is_stack(Stack, Frames) }, !,
	'$messages':translate_message(error(Error, context(_, Message))),
	[ nl, 'In:', nl ],
	prolog_stack:message(Frames).

is_stack(Stack, Frames) :-
	nonvar(Stack),
	Stack = prolog_stack(Frames).
