/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2019-2020, VU University Amsterdam
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

:- module(prolog_wrap,
          [ wrap_predicate/4,                   % :Head, +Name, -Wrapped, +Body
            unwrap_predicate/2,                 % :PI, ?Name
            current_predicate_wrapper/4		% :Head, -Name, -Wrapped, -Body
          ]).
:- autoload(library(lists),[member/2]).
:- autoload(library(pairs),[pairs_keys/2]).


:- meta_predicate
    wrap_predicate(:, +, -, +),
%   unwrap_predicate(:, ?),
    current_predicate_wrapper(:, -, -, -).

/** <module> Wrapping predicates

This library allows adding  _wrappers_  to   predicates.  The  notion of
wrappers is known in various languages  under several names. For example
Logtalk knows these as _before methods_   or  _after methods_ and Python
has _decorators_. A SWI-Prolog wrapper is   a  _body term_ that normally
calls the original wrapped definition somewhere.
*/

%!  wrap_predicate(:Head, +Name, -Wrapped, +Body) is det.
%
%   Wrap the predicate referenced by Head   using Body. Subsequent calls
%   to Head call the  given  Body  term.   Body  may  call  the original
%   definition through Wrapped. Wrapped is a   term  of the shape below,
%   where _Closure_ is an opaque _blob_.
%
%       call(Closure(A1, ...))
%
%   Name names the wrapper for  inspection using predicate_property/2 or
%   deletion using unwrap_predicate/2. If Head has   a wrapper with Name
%   the Body of the existing  wrapper   is  updated without changing the
%   order of the registered wrappers. The  same predicate may be wrapped
%   multiple times. Multiple wrappers  are   executed  starting with the
%   last registered (outermost).
%
%   The predicate referenced by Head does not  need to be defined at the
%   moment the wrapper is installed. If Head is undefined, the predicate
%   is created instead of _searched for_ using e.g., the auto loader.
%
%   Registered  wrappers  __are  not  part    of   saved  states__  (see
%   qsave_program/2) and thus need  to   be  re-registered,  for example
%   using initialization/1.

wrap_predicate(M:Head, WName, Wrapped, Body) :-
    '$wrap_predicate'(M:Head, WName, _Closure, Wrapped, Body).

%!  unwrap_predicate(:PI, ?Name) is semidet.
%
%   Remove the outermost wrapper whose name  unifies with Name. Fails if
%   no matching wrapper exists.

%!  current_predicate_wrapper(:Head, -Name, -Wrapped, -Body) is nondet.
%
%   True if Head is wrapped with Body. The arguments are compatible with
%   wrap_predicate/4 such that the result may   be used to re-create the
%   wrapper.
%
%   Wrappers  are  enumerated  starting  with    the   first  registered
%   (innermost) wrapper.

current_predicate_wrapper(M:Head, Name, Wrapped, Body) :-
    '$wrapped_predicate'(M:Head, Pairs),
    Head =.. [_|Args],
    member(Name-CRef, Pairs),
    clause(M:WHead, Body0, CRef),
    WHead =.. [_|Args],
    body_closure(Body0, Wrapped, Body).

body_closure(call(ClosureTerm), Wrapped, Wrapped) :-
    callable(ClosureTerm),
    functor(ClosureTerm, Closure, _Arity),
    blob(Closure, closure),
    !.
body_closure(Body0, Wrapped, Body) :-
    compound(Body0),
    !,
    compound_name_arity(Body0, Name, Arity),
    compound_name_arity(Body,  Name, Arity),
    body_closure_args(0, Arity, Body0, Wrapped, Body).
body_closure(Body, _, Body).

body_closure_args(I, Arity, Body0, Closure, Body) :-
    I < Arity,
    !,
    I2 is I+1,
    arg(I2, Body0, Arg0),
    arg(I2, Body, Arg),
    body_closure(Arg0, Closure, Arg),
    body_closure_args(I2, Arity, Body0, Closure, Body).
body_closure_args(_, _, _, _, _).


%   Not for public docs!
%   '$syspreds':'$predicate_property'(?Property, +Value) is nondet.
%
%   Extend predicate_property/2 to provide the `wrapped` property

:- multifile
    '$syspreds':'$predicate_property'/2.

'$syspreds':'$predicate_property'(wrapped(List), Pred) :-
    '$wrapped_predicate'(Pred, Pairs),
    pairs_keys(Pairs, List).
