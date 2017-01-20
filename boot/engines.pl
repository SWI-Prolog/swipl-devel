/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2016, VU University Amsterdam
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

:- module('$engines',
          [ engine_create/3,       % ?Template, :Goal, -Engine
            engine_create/4,       % ?Template, :Goal, -Engine, +Options
            engine_next_reified/2, % +Engine, -Term
            engine_yield/1,        % +Term
            engine_self/1,         % -Engine
            current_engine/1       % ?Engine
          ]).

:- meta_predicate
    engine_create(?, 0, -),
    engine_create(?, 0, -, +).

/** <module> Engine (interactor) support


*/

%!  engine_create(?Template, :Goal, ?Engine) is det.
%!  engine_create(?Template, :Goal, -Engine, +Options) is det.
%
%   Create a new engine, prepared to run  Goal and return answers as
%   instances of Template. Goal is not started.

engine_create(Template, Goal, Engine) :-
    (   atom(Engine)
    ->  '$engine_create'(Engine, Template+Goal, [alias(Engine)])
    ;   '$engine_create'(Engine, Template+Goal, [])
    ).
engine_create(Template, Goal, Engine, Options) :-
    '$engine_create'(Engine, Template+Goal, Options).

%!  engine_next(+Engine, -Term) is semidet.
%
%   Switch control to Engine and if engine produces a result, switch
%   control  back  and  unify   the    instance   of  Template  from
%   engine_create/3,4 with Term. Repeatedly calling engine_next/2 on
%   Engine retrieves new instances of  Template by backtracking over
%   Goal. Fails of Goal has no  more   solutions.  If Goal raises an
%   exception the exception is re-raised by this predicate.

%!  engine_next_reified(+Engine, -Term) is det.
%
%   Similar to engine_next/2 but returning answers in reified form.
%   Answers  are  returned  using  the  terms  the(Answer), no, and
%   exception(Error).

engine_next_reified(Engine, Answer) :-
    (   catch(engine_next(Engine, Answer0), Error, true)
    ->  (   var(Error)
        ->  Answer = the(Answer0)
        ;   Answer = exception(Error)
        )
    ;   Answer = no
    ).

%!  engine_post(+Engine, +Package) is det.
%
%   Make the term Package available   for engine_fetch/1 from within
%   the engine. At most one term can   be  made available. Posting a
%   package does not cause  the  engine   to  wakeup.  Therefore, an
%   engine_next/2 call must follow a call to this predicate to make
%   the engine fetch the package. The predicate engine_post/3
%   combines engine_post/2 and engine_next/2.
%
%   @error permission_error(post_to, engine, Package) if a package
%   was already posted and has not yet been fetched by the engine.

%!  engine_post(+Engine, +Package, -Reply) is semidet.
%
%   Same as engine_next/2, but transfer Term to Engine if Engine
%   calls engine_fetch/1. Acts as:
%
%     ==
%     engine_post(Engine, Package, Answer) :-
%         engine_post(Engine, Package),
%         engine_next(Engine, Answer).
%     ==

%!  engine_destroy(+Engine) is det.
%
%   Destroy Engine. Eventually, engine  destruction   will  also  be
%   subject to symbol garbage collection.

%!  engine_yield(+Term) is det.
%
%   Make engine_answer/2 return with the given term.

engine_yield(Term) :-
    '$engine_yield'(Term, 256).

%!  engine_self(E) is semidet.
%
%   True if executed inside engine E

engine_self(E) :-
    thread_self(E),
    is_engine(E).

%!  current_engine(?E)
%
%   True if E is a currently know engine.

current_engine(E) :-
    thread_property(E, engine(true)).

%!  '$engine_yield'(?Term, +Code:integer)
%
%   Cause PL_next_solution() to return with   Code, providing access
%   to  Term  through  PL_yielded().   Can    only   be   called  if
%   PL_open_query() is called with PL_Q_ALLOW_YIELD.  Code tells the
%   code that controls PL_next_solution() what to   do and the codes
%   used must be agreed upon for   a specific application. Code must
%   be an integer >= 256. Other  return   codes  are reserved by the
%   system.

'$engine_yield'(_Term, _Code) :-
    '$yield'.
