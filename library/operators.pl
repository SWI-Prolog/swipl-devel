/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2004-2013, University of Amsterdam
                              VU University Amsterdam
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

:- module(prolog_operator,
        [ push_operators/1,             % +List
          push_operators/2,             % +List, -Undo
          pop_operators/0,
          pop_operators/1,              % +Undo
          push_op/3                     % Precedence, Type, Name
        ]).


/** <module> Manage operators

Often, one wants to define operators to  improve the readibility of some
very specific code. Operators in Prolog  are global objects and changing
operators changes syntax and possible semantics of existing sources. For
this reason it is desirable  to   reset  operator declarations after the
code that needs them has been read.   This module defines a rather cruel
-but portable- method to do this.

Usage:

==
:- push_operators(
        [ op(900, fx, hello_world)
        , op(600, xf, *)
        ]).

hello_world World :-
        ....

:- pop_operators.
==

While the above are for  source-code,   the  calls  push_operators/2 and
pop_operators/1 can be used  for  local   processing  where  it  is more
comfortable to carry the undo context around.

NOTE: In recent versions of SWI-Prolog operators   are local to a module
and can be exported using the syntax   below.  This is not portable, but
otherwise a more structured approach for operator handling.

==
:- module(mymodule,
          [ mypred/1,
            op(500, fx, myop)
          ]).
==

@compat SWI-Prolog
*/

:- thread_local
    operator_stack/1.

:- meta_predicate
    push_operators(:),
    push_operators(:,-),
    push_op(+,+,:).

%!  push_operators(:New) is det.
%!  push_operators(:New, -Undo) is det.
%
%   Installs the operators from New, where New is a list of op(Prec,
%   Type, :Name). The modifications to the operator table are undone
%   in a matching call to pop_operators/0.

push_operators(New, Undo) :-
    strip_module(New, Module, Ops0),
    tag_ops(Ops0, Module, Ops),
    undo_operators(Ops, Undo),
    set_operators(Ops).

push_operators(New) :-
    push_operators(New, Undo),
    asserta(operator_stack(mark-Undo)).

%!  push_op(+Precedence, +Type, :Name) is det.
%
%   As op/3, but this call must  appear between push_operators/1 and
%   pop_operators/0.  The  change  is   undone    by   the  call  to
%   pop_operators/0

push_op(P, T, A) :-
    undo_operator(op(P,T,A), Undo),
    op(P, T, A),
    asserta(operator_stack(incremental-Undo)).

%!  pop_operators is det.
%
%   Revert all changes to the operator table realised since the last
%   push_operators/1.

pop_operators :-
    retract(operator_stack(Mark-Undo)),
    set_operators(Undo),
    Mark == mark,
    !.

%!  pop_operators(+Undo) is det.
%
%   Reset operators as pushed by push_operators/2.

pop_operators(Undo) :-
    set_operators(Undo).

tag_ops([], _, []).
tag_ops([op(P,Tp,N0)|T0], M, [op(P,Tp,N)|T]) :-
    strip_module(M:N0, M1, N1),
    N = M1:N1,
    tag_ops(T0, M, T).

set_operators([]).
set_operators([H|R]) :-
    set_operators(H),
    set_operators(R).
set_operators(op(P,T,A)) :-
    op(P, T, A).

undo_operators([], []).
undo_operators([O0|T0], [U0|T]) :-
    undo_operator(O0, U0),
    undo_operators(T0, T).

undo_operator(op(_P, T, N), op(OP, OT, N)) :-
    current_op(OP, OT, N),
    same_op_type(T, OT),
    !.
undo_operator(op(P, T, [H|R]), [OH|OT]) :-
    !,
    undo_operator(op(P, T, H), OH),
    undo_operator(op(P, T, R), OT).
undo_operator(op(_, _, []), []) :- !.
undo_operator(op(_P, T, N), op(0, T, N)).

same_op_type(T, OT) :-
    op_type(T, Type),
    op_type(OT, Type).

op_type(fx,  prefix).
op_type(fy,  prefix).
op_type(xfx, infix).
op_type(xfy, infix).
op_type(yfx, infix).
op_type(xf,  postfix).
op_type(yf,  postfix).


