/*  Part of SWI-Prolog

    Author:        Edison Mera
    E-mail:        efmera@gmail.com
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2013, Process Design Center, Breda, The Netherlands.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

%% Migrated from Ciao to SWI-Prolog

:- module(exceptions, [intercept/3, % :Goal, +Signal, :Handler
		       send_signal/1, % +Signal
		       send_silent_signal/1, % +Signal
		       catch/3, % :Goal, +Catcher, :Recover
		       throw/1, % +Exception
		       halt/0,
		       halt/1,	% +Status
		       abort/0
		      ],
	  [assertions, nortchecks, isomodes]).

:- use_module(engine(exceptions_db)).

:- doc(title, "Exception and Signal handling").

:- doc(author, "The CLIP Group").

:- doc(usage, "These predicates are builtin in Ciao, so nothing special
   has to be done to use them.").

:- doc(module, "This module includes predicates related to
   exceptions and signals, which alter the normal flow of Prolog.").

:- primitive_meta_predicate(intercept(goal, ?, goal)).

%------ errors ------%

:- pred intercept(+callable, ?term, +callable).

:- doc(intercept(Goal, Signal, Handler), "Executes @var{Goal}.  If
   a signal is sent during its execution, @var{Signal} is unified with
   the exception, and if the unification succeeds, @var{Handler} is
   executed and then the execution resumes after the point where the
   exception was thrown.  To avoid infinite loops if @var{Handler}
   raises an exception which unifies with @var{Error}, the exception
   handler is deactivated before executing @var{Handler}.  Note the
   difference with builtin @pred{catch/3}, given the code 
@begin{verbatim}
p(X) :- send_signal(signal), display('---').
p(X) :- display(X).
@end{verbatim}
   the execution of ""@tt{intercept(p(0), E, display(E)),
   display(.), fail.}"" results in the output ""@tt{error---.0.}"".").

:- test intercept(G, S, H) : ( G=((A=a;A=b), send_signal(c(A))),
	    S=c(A), H=display(A) ) + (not_fails, non_det)
# "intercept/3 preserves determinism properties of Goal (even when
   @var{H} and @var{G} share variables)".

intercept(Goal, Signal, Handler) :-
	'$metachoice'(Choice),
	asserta_catching(Choice, Signal, Handler),
	current_fact(catching(Choice, S, H)), % avoid unifs with Goal
	'$metachoice'(BeforeChoice),
	catch(Goal, E, (cut_to(Choice), throw(E))), % cleanup
	'$metachoice'(AfterChoice),
	retract_catching(Choice, S, H),
	( BeforeChoice = AfterChoice -> % no more solutions
	    ! % remove the unnecessary exception choice point
	; true
	).

send_signal2(Signal, _) :-
	var(Signal), !,
	throw(error(instantiation_error, signal/1 -1)).
send_signal2(Signal, _) :-
	current_fact(catching(C, E, H), Ref),
	\+ current_fact_nb(disabled(Ref)),
	E = Signal, !,
	throw_action(H, E, C, Ref).
send_signal2(_Signal, false).

:- trust pred send_signal(Term) : nonvar(Term).

:- doc(send_signal(Signal), "Emits a signal, to be intercepted by an
   ancestor @pred{intercept/3}. The closest matching ancestor is
   chosen. If the signal is not intercepted, the following error is
   thrown: @tt{error(unintercepted_signal(Signal),
   send_signal/1-1)}.").


send_signal(Signal):-
	(
	    send_signal2(Signal, true) ->
	    true
	;
	    throw(error(unintercepted_signal(Signal), signal/1 -1))
	).


:- trust pred send_silent_signal(Term) : nonvar(Term).

:- doc(send_silent_signal(Signal), "Emits a signal as
   @pred{send_signal/1}, but do not throws an error if the signal is
   not intercepted (i.e. just suceeds silently)").

send_silent_signal(Signal) :-
	send_signal2(Signal, _).

throw_action(Handler, _, _, Ref) :-
	asserta_disabled(Ref),
	'$metachoice'(BeforeChoice),
	'$meta_call'(Handler),
	'$metachoice'(AfterChoice),
	retract_disabled(Ref),
	(BeforeChoice = AfterChoice -> ! ; true).

cut_to(Choice) :-
	current_fact(catching(C, _, _), Ref),
	erase(Ref),
	retractall_fact(disabled(Ref)),
	C = Choice,
	'$metacut'(Choice).
