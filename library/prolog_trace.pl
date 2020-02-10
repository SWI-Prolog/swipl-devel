/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2019, CWI, Amsterdam
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
            tracing/2                           % :Spec, -Ports
          ]).
:- autoload(library(apply),[maplist/2]).
:- autoload(library(error),[instantiation_error/1]).
:- autoload(library(prolog_wrap),[wrap_predicate/4]).


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
%    T Call: lists:append([a, b], [c], _10478)
%    T Call: lists:append([b], [c], _11316)
%    T Call: lists:append([], [c], _11894)
%    T Exit: lists:append([], [c], [c])
%    T Exit: lists:append([b], [c], [b, c])
%    T Exit: lists:append([a, b], [c], [a, b, c])
%   L = [a, b, c].
%
%   ?- trace(append, -all).
%   %     lists:append/2: Not tracing
%   %     lists:append/3: Not tracing
%   %     append/1: Not tracing
%
%   @compat This library replaces prior   built-in functionality. Unlike
%   the built-in version, ports are printed   regardless  of the `debug`
%   flag. The built-in version printed  the   call-stack  depth. That is
%   currently not provided by this replacement.

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
    asserta(tracing_mask(Pred, Spec1)),
    mask_ports(Spec1, Ports),
    pi_head(Pred, Head),
    (   Spec1 == 0
    ->  unwrap_predicate(Head, trace),
        print_message(informational, trace(Head, Ports))
    ;   wrapper(Spec1, Head, Wrapped, Wrapper),
        wrap_predicate(Head, trace, Wrapped, Wrapper),
        print_message(informational, trace(Head, Ports))
    ).

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

pi_head(M:PI, M:Head) :-
    !,
    pi_head(PI, Head).
pi_head(Name/Arity, Head) :-
    functor(Head, Name, Arity).
pi_head(Name//Arity0, Head) :-
    Arity is Arity0+1,
    functor(Head, Name, Arity).

wrapper(0, _, Wrapped, Wrapped) :-
    !.
wrapper(Pattern, Head, Wrapped, Call) :-
    is_masked(Pattern, call, Pattern1),
    !,
    wrapper(Pattern1, Head, Wrapped, Call0),
    Call = (   print_message(debug, frame(Head, trace(call))),
               Call0
           ).
wrapper(Pattern, Head, Wrapped, Call) :-
    is_masked(Pattern, exit, Pattern1),
    !,
    wrapper(Pattern1, Head, Wrapped, Call0),
    Call = (   Call0,
               print_message(debug, frame(Head, trace(exit)))
           ).
wrapper(Pattern, Head, Wrapped, Call) :-
    is_masked(Pattern, redo, Pattern1),
    !,
    wrapper(Pattern1, Head, Wrapped, Call0),
    Call = (   call_cleanup(Call0, Det = true),
               (   Det == true
               ->  true
               ;   true
               ;   print_message(debug, frame(Head, trace(redo))),
                   fail
               )
           ).
wrapper(Pattern, Head, Wrapped, Call) :-
    is_masked(Pattern, fail, Pattern1),
    !,
    wrapper(Pattern1, Head, Wrapped, Call0),
    Call = call((   call_cleanup(Call0, Det = true),
                    (   Det == true
                    ->  !
                    ;   true
                    )
                ;   print_message(debug, frame(Head, trace(fail))),
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
