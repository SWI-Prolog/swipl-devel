/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2019-2023, CWI, Amsterdam
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

:- dynamic tracing_mask/2.
:- volatile tracing_mask/2.

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

trace(Pred) :-
    trace(Pred, +all).

trace(Pred, Spec) :-
    '$find_predicate'(Pred, Preds),
    Preds \== [],
    maplist(set_trace(Spec), Preds).

set_trace(Spec, Pred) :-
    (   tracing_mask(Pred, Spec0)
    ->  true
    ;   Spec0 = 0
    ),
    modify(Spec, Spec0, Spec1),
    retractall(tracing_mask(Pred, _)),
    (   Spec1 == [] ; Spec1 == 0
    ->  true
    ;   asserta(tracing_mask(Pred, Spec1))
    ),
    mask_ports(Spec1, Ports),
    pi_head(Pred, Head0),
    (   predicate_property(Head0, imported_from(M))
    ->  requalify(Head0, M, Head)
    ;   Head = Head0
    ),
    (   Spec1 == 0
    ->  unwrap_predicate(Head, trace),
        print_message(informational, trace(Head, Ports))
    ;   wrapper(Spec1, Head, Wrapped, Wrapper),
        wrap_predicate(Head, trace, Wrapped, Wrapper),
        print_message(informational, trace(Head, Ports))
    ).

requalify(Term, M, M:Plain) :-
    strip_module(Term, _, Plain).

modify(Var, _, _) :-
    var(Var),
    !,
    instantiation_error(Var).
modify([], Spec, Spec) :-
    !.
modify([H|T], Spec0, Spec) :-
    !,
    modify(H, Spec0, Spec1),
    modify(T, Spec1, Spec).
modify(+Port, Spec0, Spec) :-
    !,
    port_mask(Port, Mask),
    Spec is Spec0 \/ Mask.
modify(-Port, Spec0, Spec) :-
    !,
    port_mask(Port, Mask),
    Spec is Spec0 /\ \Mask.
modify(Port, Spec0, Spec) :-
    port_mask(Port, Mask),
    Spec is Spec0 \/ Mask.

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
    Call = (   print_message(debug, frame(Head, trace(call, Id))),
               Call0
           ).
wrapper(Pattern, Head, Id, Wrapped, Call) :-
    is_masked(Pattern, exit, Pattern1),
    !,
    wrapper(Pattern1, Head, Id, Wrapped, Call0),
    Call = (   Call0,
               print_message(debug, frame(Head, trace(exit, Id)))
           ).
wrapper(Pattern, Head, Id, Wrapped, Call) :-
    is_masked(Pattern, redo, Pattern1),
    !,
    wrapper(Pattern1, Head, Id, Wrapped, Call0),
    Call = (   call_cleanup(Call0, Det = true),
               (   Det == true
               ->  true
               ;   true
               ;   print_message(debug, frame(Head, trace(redo, Id))),
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
                ;   print_message(debug, frame(Head, trace(fail, Id))),
                    fail
                )).

is_masked(Pattern0, Port, Pattern) :-
    port_mask(Port, Mask),
    Pattern0 /\ Mask =:= Mask,
    !,
    Pattern is Pattern0 /\ \Mask.

%!  tracing(:Spec, -Ports)
%
%   True if Spec is traced using Ports

tracing(Spec, Ports) :-
    tracing_mask(Spec, Mask),
    mask_ports(Mask, Ports).

%!  list_tracing.
%
%   List predicates we are currently tracing

list_tracing :-
    PI = _:_,
    findall(trace(Head, Ports), (tracing(PI, Ports), pi_head(PI, Head)), Tracing),
    print_message(informational, tracing(Tracing)).

:- multifile
    prolog_debug_tools:debugging_hook/0.

prolog_debug_tools:debugging_hook :-
    (   tracing(_:_, _)
    ->  list_tracing
    ).


%!  notraceall is det.
%
%   Remove all trace points

notraceall :-
    forall(tracing(M:Spec, _Ports),
           trace(M:Spec, -all)).
