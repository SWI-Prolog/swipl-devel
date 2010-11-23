/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2009, University of Amsterdam

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
	    debug_message_context/1,	% (+|-)What

	    assertion/1			% :Goal
	  ]).
:- use_module(library(error)).
:- use_module(library(lists)).

:- meta_predicate(assertion(:)).
/*:- use_module(library(prolog_stack)).*/ % We use the autoloader if needed

%:- set_prolog_flag(generate_debug_info, false).

:- dynamic
	debugging/3,			% Topic, Enabled, To
	debug_context/1.

debug_context(thread).

/** <module> Print debug messages

This library is a replacement for  format/3 for printing debug messages.
Messages are assigned a _topic_. By   dynamically  enabling or disabling
topics the user can  select  desired   messages.  Debug  statements  are
removed when the code is compiled for optimization.

See manual for details. With XPCE, you can use the call below to start a
graphical monitorring tool.

==
?- prolog_ide(debug_monitor).
==

Using the predicate assertion/1 you  can   make  assumptions  about your
program explicit, trapping the debugger if the condition does not hold.

@author	Jan Wielemaker
*/

%%	debugging(+Topic) is semidet.
%%	debugging(-Topic) is nondet.
%%	debugging(?Topic, ?Bool) is nondet.
%
%	Check whether we are debugging Topic or enumerate the topics we
%	are debugging.

debugging(Topic) :-
	debugging(Topic, true, _To).

debugging(Topic, Bool) :-
	debugging(Topic, Bool, _To).

%%	debug(+Topic) is det.
%%	nodebug(+Topic) is det.
%
%	Add/remove a topic from being   printed.  nodebug(_) removes all
%	topics. Gives a warning if the topic is not defined unless it is
%	used from a directive. The latter allows placing debug topics at
%	the start a a (load-)file without warnings.
%
%	For debug/1, Topic can be  a  term   Topic  >  Out, where Out is
%	either a stream or  stream-alias  or   a  filename  (atom). This
%	redirects debug information on this topic to the given output.

debug(Topic) :-
	debug(Topic, true).
nodebug(Topic) :-
	debug(Topic, false).

debug(Spec, Val) :-
	debug_target(Spec, Topic, Out),
	(   (   retract(debugging(Topic, Enabled0, To0))
	    *-> update_debug(Enabled0, To0, Val, Out, Enabled, To),
		assert(debugging(Topic, Enabled, To)),
		fail
	    ;   (   prolog_load_context(file, _)
		->  true
		;   print_message(warning, debug_no_topic(Topic))
		),
		update_debug(false, [], Val, Out, Enabled, To),
	        assert(debugging(Topic, Enabled, To))
	    )
	->  true
	;   true
	).

debug_target(Spec, Topic, To) :-
	nonvar(Spec),
	Spec = (Topic > To), !.
debug_target(Topic, Topic, -).

update_debug(_, To0, true, -, true, To) :- !,
	ensure_output(To0, To).
update_debug(true, To0, true, Out, true, Output) :- !,
	append(To0, [Out], Output).
update_debug(false, _, true, Out, true, [Out]) :- !.
update_debug(_, _, false, -, false, []) :- !.
update_debug(true, [Out], false, Out, false, []) :- !.
update_debug(true, To0, false, Out, true, Output) :- !,
	delete(To0, Out, Output).

ensure_output([], [user_error]) :- !.
ensure_output(List, List).

%%	debug_topic(+Topic) is det.
%
%	Declare a topic for debugging.  This can be used to find all
%	topics available for debugging.

debug_topic(Topic) :-
	(   debugging(Registered, _, _),
	    Registered =@= Topic
	->  true
	;   assert(debugging(Topic, false, []))
	).

%%	list_debug_topics is det.
%
%	List currently known debug topics and their setting.

list_debug_topics :-
	format(user_error, '~*t~45|~n', "-"),
	format(user_error, '~w~t ~w~35| ~w~n',
	       ['Debug Topic', 'Activated', 'To']),
	format(user_error, '~*t~45|~n', "-"),
	(   debugging(Topic, Value, To),
	    format(user_error, '~w~t ~w~35| ~w~n', [Topic, Value, To]),
	    fail
	;   true
	).

%%	debug_message_context(What) is det.
%
%	Specify additional context for debug messages.   What  is one of
%	+Context or -Context and Context is   one of =thread=. =time= or
%	time(Format),  where  Format  is    a  format-specification  for
%	format_time/3 (default is =|%T.%3f|=).   Initially, debug/3 show
%	only thread information.

debug_message_context(+Topic) :- !,
	valid_topic(Topic, Del, Add),
	retractall(debug_context(Del)),
	assert(debug_context(Add)).
debug_message_context(-Topic) :- !,
	valid_topic(Topic, Del, _),
	retractall(debug_context(Del)).
debug_message_context(Term) :-
	type_error(debug_message_context, Term).

valid_topic(thread, thread, thread) :- !.
valid_topic(time, time(_), time('%T.%3f')) :- !.
valid_topic(time(Format), time(_), time(Format)) :- !.
valid_topic(X, _, _) :-
	domain_error(debug_message_context, X).


%%	debug(+Topic, +Format, +Args) is det.
%
%	As format/3 to user_error, but only does something if Topic
%	is activated through debug/1.

debug(Topic, Format, Args) :-
	debugging(Topic, true, To), !,
	print_debug(Topic, To, Format, Args).
debug(_, _, _).


:- multifile
	prolog:debug_print_hook/3.

print_debug(Topic, _To, Format, Args) :-
	prolog:debug_print_hook(Topic, Format, Args), !.
print_debug(_, [], _, _) :- !.
print_debug(_, To, Format, Args) :-
	phrase('$messages':translate_message(debug(Format, Args)), Lines),
	(   member(T, To),
	    debug_output(T, Stream),
	    print_message_lines(Stream, '% ', Lines),
	    fail
	;   true
	).


debug_output(user, user_error) :- !.
debug_output(Stream, Stream) :-
	is_stream(Stream), !.
debug_output(File, Stream) :-
	open(File, append, Stream,
	     [ close_on_abort(false),
	       alias(File),
	       buffer(line)
	     ]).


		 /*******************************
		 *	     ASSERTION		*
		 *******************************/

%%	assertion(:Goal) is det.
%
%	Acts similar to C assert() macro.  It has no effect of Goal
%	succeeds.  If Goal fails it prints a message, a stack-trace
%	and finally traps the debugger.

assertion(G) :-
	\+ \+ G, !.			% avoid binding variables
assertion(G) :-
	print_message(error, assumption_failed(G)),
	backtrace(10),
	(   thread_self(main)
	->  trace,
	    assertion_failed
	;   throw(error(assertion_error(G), _))
	).

assertion_failed.

%%	assume(:Goal) is det.
%
%	Acts similar to C assert() macro.  It has no effect of Goal
%	succeeds.  If Goal fails it prints a message, a stack-trace
%	and finally traps the debugger.
%
%	@deprecated	Use assertion/1 in new code.

		 /*******************************
		 *	     EXPANSION		*
		 *******************************/

:- multifile
	system:goal_expansion/2.

system:goal_expansion(debug(Topic,_,_), true) :-
	(   current_prolog_flag(optimise, true)
	->  true
	;   debug_topic(Topic),
	    fail
	).
system:goal_expansion(debugging(Topic), fail) :-
	(   current_prolog_flag(optimise, true)
	->  true
	;   debug_topic(Topic),
	    fail
	).
system:goal_expansion(assertion(G), Goal) :-
	(   current_prolog_flag(optimise, true)
	->  Goal = true
	;   expand_goal(G, G2),
	    Goal = assertion(G2)
	).
system:goal_expansion(assume(G), Goal) :-
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
	[ 'Assertion failed: ~q'-[G] ].
prolog:message(debug(Fmt, Args)) -->
	show_thread_context,
	show_time_context,
	[ Fmt-Args ].
prolog:message(debug_no_topic(Topic)) -->
	[ '~q: no matching debug topic (yet)'-[Topic] ].

show_thread_context -->
	{ debug_context(thread),
	  thread_self(Me) ,
	  Me \== main
	},
	[ '[Thread ~w] '-[Me] ].
show_thread_context -->
	[].

show_time_context -->
	{ debug_context(time(Format)),
	  get_time(Now),
	  format_time(string(S), Format, Now)
	},
	[ '[~w] '-[S] ].
show_time_context -->
	[].

