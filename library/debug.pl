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

:- module(prolog_debug,
	  [ debug/3,			% +Topic, +Format, +Args
	    debug/1,			% +Topic
	    nodebug/1,			% +Topic
	    debugging/1,		% ?Topic
	    debugging/2,		% ?Topic, ?Bool
	    list_debug_topics/0,

	    assertion/1			% :Goal
	  ]).

:- meta_predicate(assertion(:)).
:- set_prolog_flag(generate_debug_info, false).

:- dynamic
	debugging/2.

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
See manual for details. With XPCE, you can use the call below to start a
graphical monitorring tool.

	?- prolog_ide(debug_monitor).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%	debugging(?Topic)
%
%	Check whether we are debugging Topic or enumerate the topics we
%	are debugging.

debugging(Topic) :-
	debugging(Topic, true).

%	debug(+Topic)
%	nodebug(+Topic)
%
%	Add/remove a topic from being printed.  nodebug(_) removes all
%	topics.

debug(Topic) :-
	(   (   retract(debugging(Topic, _))
	    *-> assert(debugging(Topic, true)),
		fail
	    ;   assert(debugging(Topic, true))
	    )
	->  true
	;   true
	).
nodebug(Topic) :-
	(   (   retract(debugging(Topic, _))
	    *-> assert(debugging(Topic, false)),
		fail
	    ;   assert(debugging(Topic, false))
	    )
	->  true
	;   true
	).

%	debug_topic(+Topic)
%
%	Declare a topic for debugging.  This can be used to find all
%	topics available for debugging.

debug_topic(Topic) :-
	(   debugging(Registered, _),
	    Registered =@= Topic
	->  true
	;   assert(debugging(Topic, false))
	).

%	list_debug_topics
%	
%	List currently known debug topics and their setting.

list_debug_topics :-
	format(user_error, '~*t~40|~n', "-"),
	format(user_error, '~w~t~30| ~w~n', ['Debug Topic', 'Activated']),
	format(user_error, '~*t~40|~n', "-"),
	(   debugging(Topic, Value),
	    format(user_error, '~w~t~30| ~w~n', [Topic, Value]),
	    fail
	;   true
	).

%	debug(+Topic, +Format, +Args)
%
%	As format/3 to user_error, but only does something if Topic
%	is activated through debug/1.

debug(Topic, Format, Args) :-
	debugging(Topic, true), !,
	print_debug(Topic, Format, Args).
debug(_, _, _).


:- multifile
	prolog:debug_print_hook/3.

print_debug(Topic, Format, Args) :-
	prolog:debug_print_hook(Topic, Format, Args), !.
print_debug(_, Format, Args) :-
	print_message(informational, format(Format, Args)).


		 /*******************************
		 *	     ASSERTION		*
		 *******************************/

%	assertion(:Goal)
%	
%	Acts similar to C assert() macro.  It has no effect of Goal
%	succeeds.  If Goal fails it prints a message, a stack-trace
%	and finally traps the debugger.
%	
%	assume/1 is backward compatibility.  The better name assertion
%	was suggested by Steve Moyle.

assertion(G) :-
	\+ \+ G, !.			% avoid binding variables
assertion(G) :-
	print_message(error, assumption_failed(G)),
	backtrace(10),
	trace,
	assertion_failed.

assertion_failed.


		 /*******************************
		 *	     EXPANSION		*
		 *******************************/

:- multifile
	user:goal_expansion/2.

user:goal_expansion(debug(Topic,_,_), true) :-
	(   current_prolog_flag(optimise, true)
	->  true
	;   debug_topic(Topic),
	    fail
	).
user:goal_expansion((debugging(Topic) -> _ ; true), true) :-
	(   current_prolog_flag(optimise, true)
	->  true
	;   debug_topic(Topic),
	    fail
	).
user:goal_expansion(assertion(G), Goal) :-
	(   current_prolog_flag(optimise, true)
	->  Goal = true
	;   expand_goal(G, G2),
	    Goal = assertion(G2)
	).
user:goal_expansion(assume(G), Goal) :-
	print_message(informational,
		      compatibility(renamed(assume/1, assertion/1))),
	(   current_prolog_flag(optimise, true)
	->  Goal = true
	;   expand_goal(G, G2),
	    Goal = assertion(G2)
	).


		 /*******************************
		 *	      MESSAGES		*
		 *******************************/

:- multifile
	prolog:message/3.

prolog:message(assumption_failed(G)) -->
	[ 'Assertion failed: ~p'-[G] ].
