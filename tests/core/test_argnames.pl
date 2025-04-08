/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org
    Copyright (c)  2025, SWI-Prolog Solutions b.v.
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

:- module(test_argnames,
          [ test_argnames/0
          ]).
:- use_module(library(plunit)).
:- use_module(library(argnames)).
:- use_module(library(debug)).

test_argnames :-
    run_tests([ argnames_decl,
                argnames,
                argnames_expand,
                export_argnames,
                import_argnames,
                argnames_dict
              ]).

% The test modules inherit from this module.
:- argnames(planet(name, radius)).

:- begin_tests(argnames_decl).

test(ok) :-
    argnames(point1(x,y)).
test(error, error(type_error(atom, 0))) :-
    argnames(point2(x,0)).
test(error, error(domain_error(argnames, point3(x,x)))) :-
    argnames(point3(x,x)).
test(error, error(domain_error(argnames, point4()))) :-
    argnames(point4()).
test(error, error(type_error(compound, point5))) :-
    argnames(point5).

:- end_tests(argnames_decl).

:- begin_tests(argnames).
:- argnames(point(x,y,z)).

test(pt, A =@= point(_,_,_)) :-
    A = point{}.
test(ptx, A =@= point(1,_,_)) :-
    A = point{x:1}.
test(pty, A =@= point(_,2,_)) :-
    A = point{y:2}.
test(ptz, A =@= point(_,_,3)) :-
    A = point{z:3}.
test(current, Term == point(x,y,z)) :-
    current_argnames(point, Term).
test(current, Name+In == point+point(x,y,z)) :-
    In = point(_,_,_),
    current_argnames(Name, In).
test(prop, Arity == 3) :-
    argnames_property(point, arity(Arity)).
test(prop, Functor == point/3) :-
    argnames_property(point, functor(Functor)).
test(prop, Exported == false) :-
    argnames_property(point, exported(Exported)).
test(prop, fail) :-
    argnames_property(point, imported_from(_)).
test(arg, X == 1) :-
    named_arg(x, point{x:1}, X).
test(arg, error(existence_error(argnames, nopoint{x:1}))) :-
    named_arg(x, nopoint{x:1}, _).
test(arg, error(type_error(compound, 3.14))) :-
    named_arg(x, 3.14, _).
test(arg, error(type_error(compound, hello))) :-
    named_arg(x, hello, _).

:- end_tests(argnames).

:- begin_tests(argnames_expand).
:- argnames(box(x,y,w,h)).

test(arg, X == 1) :-
    arg(x of box, box{x:1}, X).
test(arg, Arity == 4) :-
    Arity = property(arity) of box.
test(arg, Functor == box/4) :-
    Functor = property(functor) of box.

:- end_tests(argnames_expand).


:- begin_tests(export_argnames).
:- export(argnames(book(author, title, year, publisher))).

test(book, A =@= book(_,_,1986,_)) :-
    A = book{year:1986}.
test(prop, Exported == true) :-
    argnames_property(book, exported(Exported)).

:- end_tests(export_argnames).

:- begin_tests(import_argnames).
:- '$import_argnames'(plunit_export_argnames:_All).

test(book, A =@= book(_,'Tom Sawyer',_,_)) :-
    A = book{title:'Tom Sawyer'}.
test(prop, Arity == 4) :-
    argnames_property(book, arity(Arity)).
test(prop, From = plunit_export_argnames) :-
    argnames_property(book, imported_from(From)).
test(inherit, Term =@= planet(earth, _)) :-
    Term = planet{name:earth}.
test(inherit, From == test_argnames) :-
    argnames_property(planet, imported_from(From)).

:- end_tests(import_argnames).

:- begin_tests(argnames_dict).
:- argnames(point(x,y)).

test(argnames_to_dict, Dict == #{x:1,y:2}) :-
    argnames_to_dict(point{x:1,y:2}, Dict, []).
test(argnames_to_dict, Dict == #{x:1}) :-
    argnames_to_dict(point{x:1}, Dict, [nonvar]).
test(argnames_to_dict, Dict == #{x:1}) :-
    argnames_to_dict(point{x:1}, Dict, [nonvar]).
test(dict_to_argnames, Term =@= point(1,_)) :-
    dict_to_argnames(#{x:1}, point, Term).
test(dict_to_argnames, Term == point(1,2)) :-
    dict_to_argnames(#{x:1,y:2}, point, Term).
test(dict_to_argnames, Term == point(1,2)) :-
    dict_to_argnames(#{x:1,y:2,z:3}, point, Term).
test(func, X == 1) :-
    Term = point{x:1},
    assertion(Term =@= point(1,_)),
    X = Term.x.
test(func, error(existence_error(arg_name, w, point(1,_)))) :-
    _ = point{x:1}.w.
test(func, fail) :-
    _ = point{x:1}.get(w).
test(func, X == 42) :-
    X = point{x:1}.get(w, 42).
test(func, X =@= #{x:1,y:_,w:42}) :-
    X = point{x:1}.put(w,42).

:- end_tests(argnames_dict).
