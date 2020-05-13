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

:- module(string,
          [ str_cat/3,
            concat_atom/2                       % +List, ?Atom
          ]).
:- use_module(machine).
:- use_module(library(error)).
:- use_module(library(lists)).

%!  concat_atom(+List, ?Atom)
%
%   True when the concatination of the   atomic elements in List produce
%   the atom Atom.  List may contain variables.

concat_atom(List, Atom) :-
    var(Atom),
    !,
    atomic_list_concat(List, Atom).
concat_atom(List, Atom) :-
    must_be(list, List),
    split_atom(List, Atom).

split_atom([], '').
split_atom([H|T], Atom) :-
    nonvar(H),
    !,
    atom_concat(H, Rest, Atom),
    split_atom(T, Rest).
split_atom(List, Atom) :-
    append(Vars, [H|T], List),
    atomic(H),
    !,
    sub_atom(Atom, Before, _, After, H),
    sub_atom(Atom, 0, Before, _, BAtom),
    gen_split(Vars, BAtom),
    sub_atom(Atom, _, After, 0, AAtom),
    split_atom(T, AAtom).

gen_split([V], V).
gen_split([H|T], Atom) :-
    atom_concat(H, R, Atom),
    gen_split(T, R).

