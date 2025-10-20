/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org
    Copyright (c)  2021-2025, SWI-Prolog Solutions b.v.
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

:- module(prolog_debug_tools,
          [ (spy)/1,                % :Spec (some users tend to define these as
            (nospy)/1,              % :Spec  an operator)
            nospyall/0,
            debugging/0,
            trap/1,                 % +Exception
            notrap/1                % +Exception
          ]).
:- use_module(library(broadcast), [broadcast/1]).
:- autoload(library(edinburgh), [debug/0]).
:- autoload(library(gensym), [gensym/2]).

:- multifile
    trap_alias/2.

:- set_prolog_flag(generate_debug_info, false).

/** <module> User level debugging tools

This  library  provides  tools   to    control   the  Prolog  debuggers.
Traditionally this code was  built-in.  Because   these  tools  are only
required in (interactive) debugging sessions they   have been moved into
the library.
*/

%!  prolog:debug_control_hook(+Action)
%
%   Allow user-hooks in the Prolog debugger interaction.  See the calls
%   below for the provided hooks.  We use a single predicate with action
%   argument to avoid an uncontrolled poliferation of hooks.

:- multifile
    prolog:debug_control_hook/1.    % +Action

:- meta_predicate
    spy(:),
    nospy(:).

%!  spy(:Spec) is det.
%!  nospy(:Spec) is det.
%!  nospyall is det.
%
%   Set/clear spy-points. A successfully  set   or  cleared spy-point is
%   reported using print_message/2, level `informational`,   with one of
%   the following terms, where Spec is of the form M:Head.
%
%       - spy(Spec)
%       - nospy(Spec)
%
%   @see    spy/1 and nospy/1 call the hook prolog:debug_control_hook/1
%           to allow for alternative specifications of the thing to
%           debug.

spy(Spec) :-
    '$notrace'(spy_(Spec)).

spy_(_:X) :-
    var(X),
    throw(error(instantiation_error, _)).
spy_(_:[]) :- !.
spy_(M:[H|T]) :-
    !,
    spy(M:H),
    spy(M:T).
spy_(Spec) :-
    prolog:debug_control_hook(spy(Spec)),
    !.
spy_(Spec) :-
    '$find_predicate'(Spec, Preds),
    '$member'(PI, Preds),
        pi_to_head(PI, Head),
        '$define_predicate'(Head),
        set_spy_point(Head),
    fail.
spy_(_).

set_spy_point(Head) :-
    '$get_predicate_attribute'(Head, spy, 1),
    !,
    print_message(informational, already_spying(Head)).
set_spy_point(Head) :-
    '$spy'(Head).

nospy(Spec) :-
    notrace(nospy_(Spec)).

nospy_(_:X) :-
    var(X),
    throw(error(instantiation_error, _)).
nospy_(_:[]) :- !.
nospy_(M:[H|T]) :-
    !,
    nospy(M:H),
    nospy(M:T).
nospy_(Spec) :-
    prolog:debug_control_hook(nospy(Spec)),
    !.
nospy_(Spec) :-
    '$find_predicate'(Spec, Preds),
    '$member'(PI, Preds),
         pi_to_head(PI, Head),
        '$nospy'(Head),
    fail.
nospy_(_).

nospyall :-
    notrace(nospyall_).

nospyall_ :-
    prolog:debug_control_hook(nospyall),
    fail.
nospyall_ :-
    spy_point(Head),
        '$nospy'(Head),
    fail.
nospyall_.

pi_to_head(M:PI, M:Head) :-
    !,
    pi_to_head(PI, Head).
pi_to_head(Name/Arity, Head) :-
    functor(Head, Name, Arity).

%!  debugging is det.
%
%   Report current status of the debugger.

:- '$hide'(debugging/0).
debugging :-
    current_prolog_flag(debug, DebugMode),
    notrace(debugging_(DebugMode)).

debugging_(DebugMode) :-
    prolog:debug_control_hook(debugging(DebugMode)),
    !.
debugging_(DebugMode) :-
    print_message(informational, debugging(DebugMode)),
    (   DebugMode == true
    ->  findall(H, spy_point(H), SpyPoints),
        print_message(informational, spying(SpyPoints))
    ;   true
    ),
    trapping,
    forall(debugging_hook(DebugMode), true).

spy_point(Module:Head) :-
    current_predicate(_, Module:Head),
    '$get_predicate_attribute'(Module:Head, spy, 1),
    \+ predicate_property(Module:Head, imported_from(_)).

%!  debugging_hook(+DebugMode)
%
%   Multifile hook that is   called as forall(debugging_hook(DebugMode),
%   true) and that may be used to   extend  the information printed from
%   other debugging libraries.

:- multifile debugging_hook/1.


		 /*******************************
		 *           EXCEPTIONS		*
		 *******************************/

%!  trap(+Formal) is det.
%!  notrap(+Formal) is det.
%
%   Install a trap on error(Formal, Context)  exceptions that unify. The
%   tracer  is  started  when  a  matching  exception  is  raised.  This
%   predicate enables _debug mode_ using  debug/0   to  get more context
%   about the exception. Even with debug   mode  disabled exceptions are
%   still trapped and thus one may call  nodebug/0 to run in normal mode
%   after installing a trap. Exceptions are trapped in any thread. Debug
%   mode is only enabled in the calling  thread. To enable debug mode in
%   all threads use tdebug/0.
%
%   Calling debugging/0 lists the enabled  traps. The predicate notrap/1
%   removes matching (unifying) traps.
%
%   In many cases debugging an exception that  is caught is as simple as
%   below (assuming run/0 starts your program).
%
%   ```
%   ?- trap(_).
%   ?- run.
%   ```
%
%   The multifile hook trap_alias/2 allow for   defining short hands for
%   commonly used traps.  Currently this defines
%
%     - det
%       Trap determinism exceptions raised as a result of the det/1
%       directive.
%     - =>
%       Trap rule existence error exceptions.
%
%   @see gtrap/1 to trap using the graphical debugger.
%   @see _Edit exceptions_ menu in PceEmacs and the graphical debugger
%   that provide a graphical frontend to trap exceptions.

:- dynamic
    exception/4,                    % Name, Term, NotCaught, Caught
    installed/1.                    % ClauseRef

trap(Error) :-
    '$notrace'(trap_(Error)).

trap_(Spec) :-
    expand_trap(Spec, Formal),
    gensym(ex, Rule),
    asserta(exception(Rule, error(Formal, _), true, true)),
    print_message(informational, trap(Rule, error(Formal, _), true, true)),
    install_exception_hook,
    debug.

notrap(Error) :-
    '$notrace'(notrap_(Error)).

notrap_(Spec) :-
    expand_trap(Spec, Formal),
    Exception = error(Formal, _),
    findall(exception(Name, Exception, NotCaught, Caught),
            retract(exception(Name, error(Formal, _), Caught, NotCaught)),
            Trapping),
    print_message(informational, notrap(Trapping)).

expand_trap(Var, _Formal), var(Var) =>
    true.
expand_trap(Alias, Formal), trap_alias(Alias, For) =>
    Formal = For.
expand_trap(Explicit, Formal) =>
    Formal = Explicit.

%!  trap_alias(+Alias, -Error)
%
%   Define short hands for commonly used exceptions.

trap_alias(det,                  determinism_error(_Pred, _Declared, _Observed, property)).
trap_alias(=>,			 existence_error(rule, _)).
trap_alias(existence_error,      existence_error(_,_)).
trap_alias(type_error,           type_error(_,_)).
trap_alias(domain_error,         domain_error(_,_)).
trap_alias(permission_error,     permission_error(_,_,_)).
trap_alias(representation_error, representation_error(_)).
trap_alias(resource_error,       resource_error(_)).
trap_alias(syntax_error,         syntax_error(_)).

trapping :-
    findall(exception(Name, Term, NotCaught, Caught),
            exception(Name, Term, NotCaught, Caught),
            Trapping),
    print_message(information, trapping(Trapping)).

:- dynamic   prolog:prolog_exception_hook/5.
:- multifile prolog:prolog_exception_hook/5.

%!  exception_hook(+ExIn, -ExOut, +Frame, +Catcher, +DebugMode) is
%!                 failure.
%
%   Trap exceptions and consider whether or not to start the tracer.

:- public exception_hook/5.

exception_hook(Ex, Ex, Frame, Catcher, _Debug) :-
    thread_self(Me),
    thread_property(Me, debug(true)),
    broadcast(debug(exception(Ex))),
    exception(_, Ex, NotCaught, Caught),
    !,
    (   Caught == true
    ->  true
    ;   Catcher == none,
        NotCaught == true
    ),
    \+ direct_catch(Frame),
    trace, fail.

%!  direct_catch(+Frame) is semidet.
%
%   True if we are dealing with a  catch(SytemPred, _, _), i.e., a catch
%   directly wrapped around a call to  a   built-in.  In that case it is
%   highly unlikely that we want the debugger to step in.

direct_catch(Frame) :-
    prolog_frame_attribute(Frame, parent, Parent),
    prolog_frame_attribute(Parent, predicate_indicator, system:catch/3),
    prolog_frame_attribute(Frame, level, MyLevel),
    prolog_frame_attribute(Parent, level, CatchLevel),
    MyLevel =:= CatchLevel+1.

%!  install_exception_hook
%
%   Make sure our handler is the first of the hook predicate.

install_exception_hook :-
    installed(Ref),
    (   nth_clause(_, I, Ref)
    ->  I == 1, !                   % Ok, we are the first
    ;   retractall(installed(Ref)),
        erase(Ref),                 % Someone before us!
        fail
    ).
install_exception_hook :-
    asserta((prolog:prolog_exception_hook(Ex, Out, Frame, Catcher, Debug) :-
                    exception_hook(Ex, Out, Frame, Catcher, Debug)), Ref),
    assert(installed(Ref)).


		 /*******************************
		 *            MESSAGES		*
		 *******************************/

:- multifile
    prolog:message//1.

prolog:message(trapping([])) -->
    [ 'No exception traps'-[] ].
prolog:message(trapping(Trapping)) -->
    [ 'Exception traps on'-[], nl ],
    trapping(Trapping).
prolog:message(trap(_Rule, Error, _Caught, _NotCaught)) -->
    [ 'Installed trap for exception '-[] ],
    exception(Error),
    [ nl ].
prolog:message(notrap([])) -->
    [ 'No matching traps'-[] ].
prolog:message(notrap(Trapping)) -->
    [ 'Removed traps from exceptions'-[], nl ],
    trapping(Trapping).

trapping([]) --> [].
trapping([exception(_Rule, Error, _Caught, _NotCaught)|T]) -->
    [ '  '-[] ],
    exception(Error),
    [ nl ],
    trapping(T).

exception(Term) -->
    { copy_term(Term, T2),
      numbervars(T2, 0, _, [singletons(true)])
    },
    [ '~p'-[T2] ].
