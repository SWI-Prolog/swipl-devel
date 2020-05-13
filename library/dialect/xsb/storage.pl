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

:- module(storage,
          [ storage_delete_all/1,
            storage_insert_fact/3,
            storage_find_fact/2
          ]).
:- use_module(library(nb_set)).

/** <module> Emulate XSB syslib/storage.P

This is a very partial  emulation  of   the  XSB  storage  library using
SWI-Prolog  non-backtrackable  data   structures.    SWI-Prolog   global
variables and destructive  operations  are,   like  XSB  storages thread
local.

@bug A storage is a SWI-Prolog global   variable,  sharing the same name
space.
*/

storage_delete_all(Storage) :-
    (   nb_current(Storage, _Set)
    ->  nb_delete(Storage)
    ;   true
    ).

storage_insert_fact(Storage, Fact, Inserted) :-
    (   nb_current(Storage, Set)
    ->  add_nb_set(Fact, Set, Inserted)
    ;   empty_nb_set(Set0),
        nb_setval(Storage, Set0),
        nb_current(Storage, Set1),              % Is a copy
        add_nb_set(Fact, Set1, Inserted)
    ).

storage_find_fact(Storage, Fact) :-
    nb_current(Storage, Set),
    gen_nb_set(Set, Fact).
