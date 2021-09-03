/*  Part of SWI-Prolog

    WWW:           http://www.swi-prolog.org
    Copyright (c)  2021, SWI-Prolog Solutions b.v.
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

:- module(types,
	  [ must_be/4,			% +Term, +Type, +Goal, +ArgNo
	    illarg/3,			% +ErrorTerm, +Goal, +ArgNo
	    illarg/4			% +ErrorTerm, +Goal, +ArgNo, +Culprit
	  ]).
:- use_module(library(error)).

/** <module> SICStus 4 library(types).

@see	https://sicstus.sics.se/sicstus/docs/4.6.0/html/sicstus/lib_002dtypes.html
*/

%!	must_be(+Term, +Type, +Goal, +ArgNo) is det.
%
%	Similar to must_be/2. This emulation currently only accepts
%	types that SWI-Prolog must_be/2 understands natively.
%
%	The Goal and ArgNo arguments are currently ignored.

must_be(Term, Type, _Goal, _ArgNo) :- must_be(Type, Term).

%!	illarg(+ErrorTerm, +Goal, +ArgNo) is det.
%
%	Same as illarg/4, with Culprit set to argument number ArgNo
%	of Goal. The Goal and ArgNo arguments are otherwise not included
%	in the thrown error.

illarg(ErrorTerm, Goal, ArgNo) :-
	arg(ArgNo, Goal, Culprit),
	illarg(ErrorTerm, Goal, ArgNo, Culprit).

%!	illarg(+ErrorTerm, +Goal, +ArgNo, +Culprit) is det.
%
%	Throw a SICStus standard error described by ErrorTerm.
%	If possible, errors are thrown in the corresponding SWI-Prolog
%	format using library(error). If a SICStus error has no
%	SWI-Prolog counterpart, it is thrown in the same format that
%	SICStus would use.
%
%	The Goal and ArgNo arguments are currently always ignored.
%	Depending on the requested error type, Culprit and/or parts of
%	ErrorTerm may also be ignored.

illarg(var, _Goal, _ArgNo, Culprit) :-
	instantiation_error(Culprit).
illarg(type(ErrorType), Goal, ArgNo, Culprit) :-
	% Equivalent according to SICStus docs.
	must_be(Culprit, ErrorType, Goal, ArgNo).
illarg(domain(ErrorType, ErrorDomain), Goal, ArgNo, Culprit) :-
	must_be(Culprit, ErrorType, Goal, ArgNo),
	domain_error(ErrorDomain, Culprit).
illarg(force_type(ExpType), _Goal, _ArgNo, Culprit) :-
	type_error(ExpType, Culprit).
illarg(context(ContextType, CommandType), _Goal, _ArgNo, _Culprit) :-
	throw(error(context_error(ContextType, CommandType), _)).
illarg(existence(ObjType, Culprit, _Message), _Goal, _ArgNo, _CulpritOther) :-
	existence_error(ObjType, Culprit).
illarg(permission(Operation, ObjType, _Message), _Goal, _ArgNo, Culprit) :-
	permission_error(Operation, ObjType, Culprit).
illarg(representation(ErrorType), _Goal, _ArgNo, _Culprit) :-
	representation_error(ErrorType).
illarg(evaluation(ErrorType), _Goal, _ArgNo, _Culprit) :-
	throw(error(evaluation_error(ErrorType), _)).
illarg(consistency(Culprit1, Culprit2, Message), _Goal, _ArgNo, _CulpritOther) :-
	throw(error(consistency_error(Culprit1, Culprit2, Message), _)).
illarg(syntax(_Pos, Msg, _Tokens, _AfterError), _Goal, _ArgNo, _Culprit) :-
	syntax_error(Msg).
illarg(resource(Resource), _Goal, _ArgNo, _Culprit) :-
	resource_error(Resource).
illarg(system(_Message), _Goal, _ArgNo, _Culprit) :-
	throw(error(system_error, _)).
