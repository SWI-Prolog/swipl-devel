/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2018, VU University Amsterdam
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

:- module(obfuscate, []).

:- dynamic
    nomap/1.

prolog:obfuscate_identifiers(_) :-
    flag('$obfuscate_id', _, 1),
    forall(module_property(M, class(user)),
           obfuscate_module(M)).

obfuscate_module(M) :-
    forall(private_predicate(M:P),
           obfuscate_predicate(M:P)).

private_predicate(M:Head) :-
    current_predicate(M:Name/Arity),
    functor(Head, Name, Arity),
    \+ predicate_property(M:Head, imported_from(_)),
    \+ predicate_property(M:Head, exported),
    \+ predicate_property(M:Head, foreign),
    \+ predicate_property(M:Head, public).

obfuscate_predicate(_M:P) :-
    functor(P, Name, _Arity),
    flag('$obfuscate_id', Id, Id+1),
    atom_concat('$P$', Id, Name2),
    (   nomap(Name)
    ->  true
    ;   catch('$map_id'(Name, Name2), _, fail)
    ->  format('~q --> ~q~n', [Name, Name2])
    ;   '$unmap_id'(Name),
        asserta(nomap(Name))
    ).
