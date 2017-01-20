/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2009-2013, University of Amsterdam
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

:- module(commons,
	  [ feature/1				% +Feature
	  ]).
:- use_module(library(error)).

/** <module> Implement Prolog Commons infrastructure
*/

:- multifile
	feature/1.

%%	new_declaration(+PredicateIndicator)
%
%	Directive that tells the system that PredicateIndicator can be
%	used as a directive.

user:term_expansion(new_declaration(Name/Arity),
	       '$directive'(Head)) :-
	functor(Head, Name, Arity).
user:term_expansion((:- Directive), []) :-
	current_predicate('$directive'/1),
	'$directive'(Directive).
user:term_expansion((:- module(Name, Public, Import)),
		    [ (:- module(Name, Public))
		    | ImportsDecls
		    ]) :-
	maplist(import_decl, Import, ImportsDecls).

import_decl(Name,
	    use_module(library(dialect/Name))).


%%	feature(+Feature) is semidet.
%
%	Provide the condition for :- if(feature(...)).

feature(Var) :-
	var(Var), !,
	instantiation_error(Var).
feature(implementation_defined(PI)) :-
	current_predicate(PI).

