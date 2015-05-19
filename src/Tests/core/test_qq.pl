/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemak@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2015, VU University Amsterdam

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

:- module(test_qq, [test_qq/0]).
:- use_module(library(plunit)).
:- use_module(library(quasi_quotations)).

/** <module> Test Quasi Quotation processing

@author	Jan Wielemaker
*/

test_qq :-
	run_tests([ quasi_quotations
		  ]).

:- begin_tests(quasi_quotations).

:- quasi_quotation_syntax(prolog).

prolog(Content, _Vars, _Dict, Term) :-
	with_quasi_quotation_input(Content, Stream, read(Stream, Term)).

test(simple, Term == a(b)) :-
	context_module(Me),
	term_string(Term, "{|prolog||a(b).|}",
		    [module(Me)]).
test(error, error(syntax_error(operator_expected))) :-
	context_module(Me),
	term_string(_Term, "{|prolog||a b.|}",
		    [module(Me)]).

:- end_tests(quasi_quotations).
