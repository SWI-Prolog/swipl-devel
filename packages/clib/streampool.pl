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

:- module(stream_pool,
	  [ add_stream_to_pool/2,	% +Stream, :Goal
	    delete_stream_from_pool/1,	% +Stream
	    close_stream_pool/0,
	    dispatch_stream_pool/1,	% +TimeOut
	    stream_pool_main_loop/0
	  ]).
:- use_module(library(quintus)).

:- meta_predicate
	add_stream_to_pool(+, :).

:- volatile
	pool/2.				% sockets don't survive a saved-state
:- dynamic
	pool/2.				% Stream, Action

%	add_stream_to_pool(+Stream :Goal)
%
%	Call Goal whenever there is input on Stream.

add_stream_to_pool(Stream, Action) :-
	'$strip_module'(Action, Module, Plain),
	register_stream(Stream, Module:Plain).

register_stream(Stream, Goal) :-
	assert(pool(Stream, Goal)).

%	delete_stream_from_pool(+Stream)
%
%	Retract stream from the pool

delete_stream_from_pool(Stream) :-
	retractall(pool(Stream, _)).

%	close_stream_pool

close_stream_pool :-
	(   retract(pool(Stream, _)),
	    close(Stream, [force(true)]),
	    fail
	;   true
	).
	
%	dispatch_stream_pool(+TimeOut)
%
%	Wait for input on one or more streams and handle that.  Wait for
%	at most TimeOut seconds (0 means infinite).

dispatch_stream_pool(Timeout) :-
	findall(S, pool(S, _), Pool),
	catch(wait_for_input(Pool, Ready, Timeout), E, true),
	(   var(E)
	->  actions(Ready)
	;   E = error(existence_error(stream, Stream), _)
	->  delete_stream_from_pool(Stream)
	).

actions([]).
actions([H|T]) :-
	action(H),
	actions(T).

action(Stream) :-
	pool(Stream, Action),
	(   catch(Action, E, true)
	->  (   var(E)
	    ->	true
	    ;	print_message(error, E)
	    )
	;   print_message(warning,
			  goal_failed(Action, stream_pool))
	).
	
%	stream_pool_main_loop
%
%	Keep handling input from the streams in the pool until they have
%	all died away.

stream_pool_main_loop :-
	pool(_, _), !,
	(   current_prolog_flag(windows, true)
	->  dispatch_stream_pool(1)	% so we can break out easily
	;   dispatch_stream_pool(0)
	),
	stream_pool_main_loop.
stream_pool_main_loop.

