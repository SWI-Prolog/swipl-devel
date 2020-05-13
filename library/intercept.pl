/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2020, VU University Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(intercept,
          [ intercept/3,                        % :Goal, ?Ball, :Handler
            intercept/4,                        % :Goal, ?Ball, :Handler, +Arg
            intercept_all/4,                    % +Templ, :Goal, ?Ball, -List
            nb_intercept_all/4,                 % +Templ, :Goal, ?Ball, -List
            send_signal/1,                      % +Ball
            send_silent_signal/1                % +Ball
          ]).
:- autoload(library(error),[must_be/2]).


/** <module> Intercept and signal interface

This library allows for  creating  an   execution  context  (goal) which
defines  how  calls  to  send_signal/1  are  handled.  This  library  is
typically used to fetch  values  from   the  context  or process results
depending on the context.

For example, assume we  parse  a  (large)   file  using  a  grammar (see
phrase_from_file/3) that has  some  sort   of  _record_  structure. What
should we do with the recognised records? We  can return them in a list,
but if the input is large this is a  huge overhead if the records are to
be asserted or written to a file.  Using this interface we can use

```
document -->
    record(Record),
    !,
    { send_signal(record(Record)) },
    document.
document -->
    [].
```

Given the above, we can assert all   records into the database using the
following query:

```
    ...,
    intercept(phrase_from_file(File, document),
              record(Record),
              assertz(Record)).
```

Or, we can collect all records in a list using intercept_all/4:

```
    ...,
    intercept_all(Record,
                  phrase_from_file(File, document), record(Record),
                  Records).
```
*/

:- meta_predicate
    intercept(0,?,0),
    intercept(0,?,1,?),
    intercept_all(?,0,?,-),
    nb_intercept_all(?,0,?,-).

%!  intercept(:Goal, ?Ball, :Handler)
%
%   Run Goal as call/1.  If  somewhere   during  the  execution  of Goal
%   send_signal/1 is called with a _Signal_  that unifies with Ball, run
%   Handler and continue the execution.
%
%   This predicate is related to catch/3,   but rather than aborting the
%   execution of Goal and running Handler  it continues the execution of
%   Goal. This construct is also   related  to _delimited continuations_
%   (see reset/3 and shift/1). It only covers  one (common) use case for
%   delimited continuations, but does so with   a  simpler interface, at
%   lower overhead and without suffering from  poor interaction with the
%   cut.
%
%   Note that Ball and Handler are _copied_ before calling the (copy) of
%   Handler to avoid instantiation of Ball and/or Handler which can make
%   a subsequent signal fail.
%
%   @see intercept/4, reset/3, catch/4, broadcast_request/1.
%   @compat Ciao

intercept(Goal, Ball, Handler) :-
    do_intercept(Goal, Ball, Handler, args).

%!  intercept(:Goal, ?Ball, :Handler, +Arg)
%
%   Similar to intercept/3,  but  the  copy   of  Handler  is  called as
%   call(Copy,Arg), which allows passing  large   context  arguments  or
%   arguments subject to unification or   _destructive  assignment_. For
%   example:
%
%       ?- intercept(send_signal(x), X, Y=X).
%       true.
%
%       ?- intercept(send_signal(x), X, =(X), Y).
%       Y = x.

intercept(Goal, Ball, Handler, Context) :-
    do_intercept(Goal, Ball, Handler, args(Context)).

do_intercept(Goal, Ball, Handler, Context) :-
    Goal,
    no_lco(Ball, Handler, Context).

no_lco(_,_,_).

%!  intercept_all(+Template, :Goal, ?Ball, -List).
%
%   True when List contains all  instances   of  Template that have been
%   sent using send_signal/1 where the argument  unifies with Ball. Note
%   that backtracking in Goal resets the List.  For example, given
%
%   ```
%   enum(I, Max) :- I =< Max, !, send_signal(emit(I)),
%                   I2 is I+1, enum(I2, Max).
%   enum(_, _).
%   ```
%
%   Consider the following queries
%
%       ?- intercept_all(I, enum(1,6), emit(I), List).
%       List = [1, 2, 3, 4, 5, 6].
%
%       ?- intercept_all(I, (between(1,3,Max),enum(1,Max)),
%                        emit(I), List).
%       Max = 1, List = [1] ;
%	Max = 2, List = [1, 2] ;
%	Max = 3, List = [1, 2, 3].
%
%   @see nb_intercept_all/4

intercept_all(Template, Goal, Ball, List) :-
    List0 = [_],
    State = list(List0, List0),
    intercept(Goal, Ball, add_ball(Template), State),
    arg(1, State, [_|List]).

add_ball(Elem, State) :-
    Tail = [Elem],
    arg(2, State, List),
    setarg(2, List, Tail),
    setarg(2, State, Tail).

%!  nb_intercept_all(+Template, :Goal, ?Ball, -List)
%
%   As intercept_all/4, but backtracing inside Goal does not reset List.
%   Consider this program and the subsequent queries
%
%   ```
%   enum_b(F, T) :- forall(between(F, T, I), send_signal(emit(I))).
%   ```
%
%       ?- intercept_all(I, enum_b(1, 6), emit(I), List).
%       List = [].
%
%       ?- nb_intercept_all(I, enum_b(1, 6), emit(I), List).
%       List = [1, 2, 3, 4, 5, 6].

nb_intercept_all(Template, Goal, Ball, List) :-
    List0 = [_],
    State = list(List0, List0),
    intercept(Goal, Ball, nb_add_ball(Template), State),
    arg(1, State, [_|List]).

nb_add_ball(Elem, State) :-
    duplicate_term(Elem, Copy),
    Tail = [Copy],
    arg(2, State, List),
    nb_linkarg(2, List, Tail),
    nb_linkarg(2, State, Tail).

%!  send_signal(+Signal)
%
%   If this predicate is called from a sub-goal of intercept/3, execute
%   the associated _Handler_ of the intercept/3 environment.
%
%   @error  unintercepted_signal(Signal)  if  there    is   no  matching
%   intercept environment.

send_signal(Signal) :-
    must_be(nonvar, Signal),
    prolog_current_frame(Frame),
    (   interceptor(Frame, Signal, Handler, Context)
    ->  call_handler(Context, Handler)
    ;   throw(error(unintercepted_signal(Signal), _))
    ).

%!  send_silent_signal(+Signal)
%
%   As send_signal/1, but succeed  silently  if   there  is  no matching
%   intercept environment.

send_silent_signal(Signal) :-
    must_be(nonvar, Signal),
    prolog_current_frame(Frame),
    (   interceptor(Frame, Signal, Handler, Context)
    ->  call_handler(Context, Handler)
    ;   true
    ).

call_handler(args, Handler) :-
    call(Handler).
call_handler(args(A0), Handler) :-
    call(Handler, A0).

interceptor(Frame, Signal, Handler, Context) :-
    prolog_frame_attribute(Frame, parent_goal(Next),
                           intercept:do_intercept(_Goal, Signal0, Handler0, Context)),
    (   copy_term(Signal0+Handler0, Signal+Handler)
    ->  true
    ;   interceptor(Next, Signal, Handler, Context)
    ).


		 /*******************************
		 *            SANDBOX		*
		 *******************************/

:- multifile
    sandbox:safe_meta_predicate/1,
    sandbox:safe_primitive/1.

sandbox:safe_meta_predicate(intercept:intercept/3).
sandbox:safe_meta_predicate(intercept:intercept/4).
sandbox:safe_meta_predicate(intercept:intercept_all/4).
sandbox:safe_meta_predicate(intercept:nb_intercept_all/4).

sandbox:safe_primitive(intercept:send_signal(_)).
sandbox:safe_primitive(intercept:send_silent_signal(_)).
