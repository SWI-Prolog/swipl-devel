/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2012, University of Amsterdam
			      VU University Amsterdam

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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(prolog_debug,
	  [ debug/3,			% +Topic, +Format, :Args
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
:- set_prolog_flag(generate_debug_info, false).

:- meta_predicate
	assertion(0),
	debug(+,+,:).

:- multifile prolog:assertion_failed/2.
:- dynamic   prolog:assertion_failed/2.

/*:- use_module(library(prolog_stack)).*/ % We use the autoloader if needed

%:- set_prolog_flag(generate_debug_info, false).

:- dynamic
	debugging/3,			% Topic, Enabled, To
	debug_context/1.

debug_context(thread).

/** <module> Print debug messages and test assertions

This library is a replacement for  format/3 for printing debug messages.
Messages are assigned a _topic_. By   dynamically  enabling or disabling
topics the user can  select  desired   messages.  Debug  statements  are
removed when the code is compiled for optimization.

See manual for details. With XPCE, you can use the call below to start a
graphical monitoring tool.

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
%	Examine debug topics. The form debugging(+Topic)  may be used to
%	perform more complex debugging tasks.   A typical usage skeleton
%	is:
%
%	  ==
%		(   debugging(mytopic)
%		->  <perform debugging actions>
%		;   true
%		),
%		...
%	  ==
%
%	The other two calls are intended to examine existing and enabled
%	debugging tokens and are typically not used in user programs.

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
%	the start of a (load-)file without warnings.
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

%%	debug_message_context(+What) is det.
%
%	Specify additional context for debug messages.   What  is one of
%	+Context or -Context, and Context is  one of =thread=, =time= or
%	time(Format),  where  Format  is    a  format specification  for
%	format_time/3 (default is =|%T.%3f|=).  Initially, debug/3 shows
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


%%	debug(+Topic, +Format, :Args) is det.
%
%	Format a message if debug topic  is enabled. Similar to format/3
%	to =user_error=, but only prints if   Topic is activated through
%	debug/1. Args is a  meta-argument  to   deal  with  goal for the
%	@-command.   Output   is   first    handed     to    the    hook
%	prolog:debug_print_hook/3.  If  this  fails,    Format+Args   is
%	translated  to  text   using    the   message-translation   (see
%	print_message/2) for the  term  debug(Format,   Args)  and  then
%	printed to every matching destination   (controlled  by debug/1)
%	using print_message_lines/3.
%
%	The message is preceded by '% ' and terminated with a newline.
%
%	@see	format/3.

debug(Topic, Format, Args) :-
	debugging(Topic, true, To), !,
	print_debug(Topic, To, Format, Args).
debug(_, _, _).


%%	prolog:debug_print_hook(+Topic, +Format, +Args) is semidet.
%
%	Hook called by debug/3.  This  hook   is  used  by the graphical
%	frontend that can be activated using prolog_ide/1:
%
%	  ==
%	  ?- prolog_ide(debug_monitor).
%	  ==

:- multifile
	prolog:debug_print_hook/3.

print_debug(Topic, _To, Format, Args) :-
	prolog:debug_print_hook(Topic, Format, Args), !.
print_debug(_, [], _, _) :- !.
print_debug(Topic, To, Format, Args) :-
	phrase('$messages':translate_message(debug(Format, Args)), Lines),
	(   member(T, To),
	    debug_output(T, Stream),
	    with_output_to(
		Stream,
		print_message_lines(current_output, kind(debug(Topic)), Lines)),
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
%	Acts similar to C assert()  macro.  It   has  no  effect if Goal
%	succeeds. If Goal fails or throws    an exception, the following
%	steps are taken:
%
%	  * call prolog:assertion_failed/2.  If prolog:assertion_failed/2
%	    fails, then:
%
%	    - If this is an interactive toplevel thread, print a
%	      message, the stack-trace, and finally trap the debugger.
%	    - Otherwise, throw error(assertion_error(Reason, G),_) where
%	      Reason is one of =fail= or the exception raised.

assertion(G) :-
	\+ \+ catch(G,
		    Error,
		    assertion_failed(Error, G)),
	!.
assertion(G) :-
	assertion_failed(fail, G),
	assertion_failed.		% prevent last call optimization.

assertion_failed(Reason, G) :-
	prolog:assertion_failed(Reason, G), !.
assertion_failed(Reason, G) :-
	print_message(error, assertion_failed(Reason, G)),
	backtrace(10),
	(   current_prolog_flag(break_level, _)	% interactive thread
	->  trace
	;   throw(error(assertion_error(Reason, G), _))
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
system:goal_expansion(assertion(_), Goal) :-
	current_prolog_flag(optimise, true),
	Goal = true.
system:goal_expansion(assume(_), Goal) :-
	print_message(informational,
		      compatibility(renamed(assume/1, assertion/1))),
	current_prolog_flag(optimise, true),
	Goal = true.


		 /*******************************
		 *	      MESSAGES		*
		 *******************************/

:- multifile
	prolog:message/3.

prolog:message(assertion_failed(_, G)) -->
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

		 /*******************************
		 *	       HOOKS		*
		 *******************************/

%%	prolog:assertion_failed(+Reason, +Goal) is semidet.
%
%	This hook is called if the Goal  of assertion/1 fails. Reason is
%	unified with either =fail= if Goal simply failed or an exception
%	call otherwise. If this hook  fails,   the  default behaviour is
%	activated.  If  the  hooks  throws  an   exception  it  will  be
%	propagated into the caller of assertion/1.
