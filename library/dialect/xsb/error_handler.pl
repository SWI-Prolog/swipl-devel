/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2019, VU University Amsterdam
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

:- module(error_handler,
          [ check_acyclic/3,		% ?Term,+Predicate,+Arg
            check_atom/3,		% ?Term,+Predicate,+Arg
            check_callable/3,		% ?Term,+Predicate,+Arg
            check_integer/3,		% ?Term,+Predicate,+Arg
            check_nonvar/3,		% ?Term,+Predicate,+Arg
            check_nonvar_list/3,	% ?Term,+Predicate,+Arg
%           check_one_thread/3,         % +Operation,+Object_Type,+Predicate
            check_stream/3,		% ?Term,+Predicate,+Arg
            check_var/3,		% ?Term,+Predicate,+Arg
            check_ground/3,		% ?Term,+Predicate,+Arg

            domain_error/4,             % +Valid_type,-Culprit,+Predicate,+Arg

            print_backtrace/1,		% +Backtrace

            xsb_error_get_tag/2,        % +ErrorTerm, -Formal
            xsb_error_get_message/2     % +ErrorTerm, -Message
          ]).
:- use_module(library(error)).
:- use_module(library(prolog_stack)).

/** <module> XSB compatible error handling
*/

check_acyclic(Term, _Pred, _Arg)     :- must_be(acyclic, Term).
check_atom(Term, _Pred, _Arg)        :- must_be(atom, Term).
check_callable(Term, _Pred, _Arg)    :- must_be(callable, Term).
check_integer(Term, _Pred, _Arg)     :- must_be(integer, Term).
check_nonvar(Term, _Pred, _Arg)      :- must_be(nonvar, Term).
check_nonvar_list(Term, _Pred, _Arg) :- must_be(list(nonvar), Term).
check_stream(Term, _Pred, _Arg)      :- must_be(stream, Term).
check_var(Term, _Pred, _Arg)         :- must_be(var, Term).
check_ground(Term, _Pred, _Arg)      :- must_be(ground, Term).

%!  xsb_error_get_tag(+Term, -Tag) is semidet.
%
%   Tag is the formal part of an error(Formal,Context) term.

xsb_error_get_tag(error(Tag, _), Tag).

%!  xsb_error_get_message(+Term, -Message) is semidet.
%
%   Message is the additional explanation context for an error term,

xsb_error_get_message(error(_, Context), Message) :-
    error_context_message(Context, Message).

error_context_message(Var, Var) :-
    var(Var),
    !.
error_context_message(context(Message, _Stack), Message).

%!  domain_error(+Valid_type, -Culprit, +Predicate, +Arg)
%
%   Throws a domain error.

domain_error(Type, Culprit, _Pred, _Arg) :-
    domain_error(Type, Culprit).

%!  print_backtrace(+Backtrace)
%
%   This predicate, which is used by XSB’s default error handler, prints
%   a backtrace structure to XSB’s standard error stream.

print_backtrace(Backtrace) :-
    print_prolog_backtrace(user_error, Backtrace).


		 /*******************************
		 *            MESSAGES		*
		 *******************************/

:- multifile prolog:message//1.

prolog:message(aborted(Text)) -->
    [ '(abort) ~w'-[Text] ].
