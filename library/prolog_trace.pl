/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2019-2024, CWI, Amsterdam
                              SWI-Prolog Solutions b.v.
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

:- module(prolog_trace,
          [ trace/1,                            % :Spec
            trace/2,                            % :Spec, +Ports
            tracing/2,                          % :Spec, -Ports
            list_tracing/0,
            notraceall/0
          ]).
:- autoload(library(apply),[maplist/2]).
:- autoload(library(error),[instantiation_error/1]).
:- autoload(library(prolog_wrap),[wrap_predicate/4]).
:- autoload(library(prolog_code), [pi_head/2]).


/** <module> Print access to predicates

This library prints accesses to  specified   predicates  by wrapping the
predicate.

@see  library(debug)  for  adding  conditional  print  statements  to  a
program.
*/

:- meta_predicate
    trace(:),
    trace(:, +),
    tracing(:, -).

:- dynamic tracing_mask/2 as volatile.          % :Head, Bitmask
:- dynamic trace_condition/3 as volatile.       % :Head, Port, Cond

%!  trace(:Pred) is det.
%!  trace(:Pred, +PortSpec) is det.
%
%   Print passes through _ports_ of  specified   predicates.  Pred is a,
%   possible partial, specification of a predicate as it is also used be
%   spy/1 and similar predicates. Where   a full predicate specification
%   is of the shape `Module:Name/Arity` (or `//Arity for non-terminals),
%   both the module and arity may be   omitted in which case Pred refers
%   to all matching  predicates.  PortSpec  is   either  a  single  port
%   (`call`, `exit`, `fail` or `redo`), preceded with   `+`  or `-` or a
%   list  of  these.  The   predicate    modifies   the   current  trace
%   specification and then installs a suitable wrapper for the predicate
%   using wrap_predicate/4.  For example:
%
%   ```
%   ?- trace(append).
%   %     lists:append/2: [all]
%   %     lists:append/3: [all]
%   %     append/1: [all]
%   true.
%
%   ?- append([a,b], [c], L).
%    T [10] Call: lists:append([a, b], [c], _18032)
%    T [19] Call: lists:append([b], [c], _19410)
%    T [28] Call: lists:append([], [c], _20400)
%    T [28 +0.1ms] Exit: lists:append([], [c], [c])
%    T [19 +0.2ms] Exit: lists:append([b], [c], [b, c])
%    T [10 +0.5ms] Exit: lists:append([a, b], [c], [a, b, c])
%   L = [a, b, c].
%
%   ?- trace(append, -all).
%   %     lists:append/2: Not tracing
%   %     lists:append/3: Not tracing
%   %     append/1: Not tracing
%   ```
%
%   The text between [] indicates the call  depth (first number) and for
%   all ports except the `call` port  the   _wall_  time since the start
%   (call port) in milliseconds. Note that the instrumentation and print
%   time is included in the time. In   the example above the actual time
%   is about 0.00001ms on todays hardware.
%
%   In addition, __conditions__ may be specified.   In this case the the
%   specification takes the shape ``trace(:Head, Port(Condition))``. For
%   example:
%
%       ?- trace(current_prolog_flag(Flag, Value), call(var(Flag))).
%       ?- list_tracing.
%       % Trace points (see trace/1,2) on:
%       %     system:current_prolog_flag(A,_): [call(var(A))]
%
%   This specification will only  print  the   goal  if  the  registered
%   condition succeeds. Note that we can use  the condition for its side
%   effect and then fail to avoid printing the event. Clearing the trace
%   event  on  all  relevant  ports  removes  the  condition.  There  is
%   currently no way to modify the  condition without clearing the trace
%   point first.

trace(Pred) :-
    trace(Pred, +all).

trace(Pred, Spec) :-
    Pred = Ctx:_,
    '$find_predicate'(Pred, Preds),
    Preds \== [],
    maplist(set_trace_pi(Spec, Pred, Ctx), Preds).

set_trace_pi(Spec, PredSpec, Ctx, Pred) :-
    pi_head(Pred, Head0),
    resolve_predicate(Head0, Head),
    bind_head(PredSpec, Head),
    set_trace(Spec, Head, Ctx).

bind_head(Head, Head) :- !.
bind_head(_:Head, _:Head) :- !.
bind_head(_, _).

set_trace(Spec, Head, Ctx) :-
    (   tracing_mask(Head, Spec0)
    ->  true
    ;   Spec0 = 0
    ),
    modify(Spec, Head, Spec0, Spec1, Ctx),
    retractall(tracing_mask(Head, _)),
    (   Spec1 == [] ; Spec1 == 0
    ->  true
    ;   asserta(tracing_mask(Head, Spec1))
    ),
    mask_ports(Spec1, Ports),
    (   Spec1 == 0
    ->  unwrap_predicate(Head, trace),
        print_message(informational, trace(Head, Ports))
    ;   wrapper(Spec1, Head, Wrapped, Wrapper),
        wrap_predicate(Head, trace, Wrapped, Wrapper),
        print_message(informational, trace(Head, Ports))
    ).

resolve_predicate(Head0, Head) :-
    (   predicate_property(Head0, imported_from(M))
    ->  requalify(Head0, M, Head)
    ;   Head = Head0
    ).

requalify(Term, M, M:Plain) :-
    strip_module(Term, _, Plain).

modify(Var, _, _, _, _) :-
    var(Var),
    !,
    instantiation_error(Var).
modify([], _, Spec, Spec, _) :-
    !.
modify([H|T], Head, Spec0, Spec, Ctx) :-
    !,
    modify(H, Head, Spec0, Spec1, Ctx),
    modify(T, Head, Spec1, Spec, Ctx).
modify(+PortSpec, Head, Spec0, Spec, Ctx) :-
    !,
    port_spec(PortSpec, Head, Port, Ctx),
    port_mask(Port, Mask),
    Spec is Spec0 \/ Mask.
modify(-Port, Head, Spec0, Spec, _) :-
    !,
    port_mask(Port, Mask),
    remove_condition(Head, Mask),
    Spec is Spec0 /\ \Mask.
modify(Port, Head, Spec0, Spec, Ctx) :-
    modify(+Port, Head, Spec0, Spec, Ctx).

port_spec(Spec, _, Port, _), atom(Spec) =>
    Port = Spec.
port_spec(Spec, Head, Port, Ctx),
    compound(Spec),
    compound_name_arguments(Spec, Name, [Cond]) =>
    Port = Name,
    port_mask(Port, Mask),
    strip_module(Ctx:Cond, M, PCond),
    (   predicate_property(M:PCond, iso)
    ->  TheCond = PCond
    ;   TheCond = M:PCond
    ),
    asserta(trace_condition(Head, Mask, TheCond)).

remove_condition(Head, Mask) :-
    (   trace_condition(Head, TraceMask, TheCond),
        Mask /\ TraceMask =:= TraceMask,
        retractall(trace_condition(Head, TraceMask, TheCond)),
        fail
    ;   true
    ).

port_mask(all,  0x0f).
port_mask(call, 0x01).
port_mask(exit, 0x02).
port_mask(redo, 0x04).
port_mask(fail, 0x08).

mask_ports(0, []) :-
    !.
mask_ports(Pattern, [H|T]) :-
    is_masked(Pattern, H, Pattern1),
    mask_ports(Pattern1, T).

%!  wrapper(+Ports:integer, :Head, -Wrapped, -Wrapper) is det.
%!  wrapper(+Ports:integer, :Head, +Id, -Wrapped, -Wrapper) is det.
%
%   Adds calls to
%
%      print_message(debug, frame(Head, trace(Port, Id)))
%
%   @arg Id is a term  `#{frame:Frame, level:Level, start:Start}`, where
%   `Frame` is the (fragile) frame  identifier,   `Level`  is  the stack
%   depth and `Start` is the wall-time when the call was started.

wrapper(Ports, Head, Wrapped, Wrapper) :-
    wrapper(Ports, Head,
            #{frame:Frame, level:Level, start:Start},
            Wrapped, Wrapped1),
    Wrapper = (   prolog_current_frame(Frame),
                  prolog_frame_attribute(Frame, level, Level),
                  get_time(Start),
                  Wrapped1
              ).

wrapper(0, _, _, Wrapped, Wrapped) :-
    !.
wrapper(Pattern, Head, Id, Wrapped, Call) :-
    is_masked(Pattern, call, Pattern1),
    !,
    wrapper(Pattern1, Head, Id, Wrapped, Call0),
    Call = (   prolog_trace:on_port(call, Head, Id),
               Call0
           ).
wrapper(Pattern, Head, Id, Wrapped, Call) :-
    is_masked(Pattern, exit, Pattern1),
    !,
    wrapper(Pattern1, Head, Id, Wrapped, Call0),
    Call = (   Call0,
               prolog_trace:on_port(exit, Head, Id)
           ).
wrapper(Pattern, Head, Id, Wrapped, Call) :-
    is_masked(Pattern, redo, Pattern1),
    !,
    wrapper(Pattern1, Head, Id, Wrapped, Call0),
    Call = (   call_cleanup(Call0, Det = true),
               (   Det == true
               ->  true
               ;   true
               ;   prolog_trace:on_port(redo, Head, Id),
                   fail
               )
           ).
wrapper(Pattern, Head, Id, Wrapped, Call) :-
    is_masked(Pattern, fail, Pattern1),
    !,
    wrapper(Pattern1, Head, Id, Wrapped, Call0),
    Call = call((   call_cleanup(Call0, Det = true),
                    (   Det == true
                    ->  !
                    ;   true
                    )
                ;   prolog_trace:on_port(fail, Head, Id),
                    fail
                )).

is_masked(Pattern0, Port, Pattern) :-
    port_mask(Port, Mask),
    Pattern0 /\ Mask =:= Mask,
    !,
    Pattern is Pattern0 /\ \Mask.

%   on_port(+Port, +Head, +Id)
%
%   Called on the various ports. Succeeds on the `call` and `exit` ports
%   and fails otherwise.

:- public on_port/3.
on_port(Port, Head, Id) :-
    (   do_trace(Port, Head)
    ->  print_message(debug, frame(Head, trace(Port, Id)))
    ;   true
    ),
    success_port(Port).

do_trace(Port, Head) :-
    forall(active_trace_condition(Port, Head, Cond),
           Cond).

active_trace_condition(Port, Head, Cond) :-
    trace_condition(Head, Mask, Cond),
    port_mask(Port, PortMask),
    Mask /\ PortMask =\= 0.

success_port(call).                     % on the other ports we must fail.
success_port(exit).

%!  tracing(:Spec, -Ports)
%
%   True if Spec is traced using Ports.   Spec is a fully qualified head
%   term.

tracing(Spec, Ports) :-
    tracing_mask(Spec, Mask),
    mask_ports(Mask, Ports0),
    maplist(add_condition(Spec), Ports0, Ports).

add_condition(Head, Port, PortCond) :-
    trace_condition(Head, Mask, Cond),
    port_mask(Port, PortMask),
    Mask /\ PortMask =\= 0,
    !,
    PortCond =.. [Port,Cond].
add_condition(_, Port, Port).


%!  list_tracing.
%
%   List predicates we are currently tracing

list_tracing :-
    Head = _:_,
    findall(trace(Head, Ports), tracing(Head, Ports), Tracing),
    print_message(informational, tracing(Tracing)).

:- multifile
    prolog_debug_tools:debugging_hook/1.

prolog_debug_tools:debugging_hook(_DebugMode) :-
    (   tracing(_:_, _)
    ->  list_tracing
    ).


%!  notraceall is det.
%
%   Remove all trace points

notraceall :-
    forall(tracing(M:Spec, _Ports),
           trace(M:Spec, -all)).
