/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2013, University of Amsterdam
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

:- module(gensym,
        [ reset_gensym/0,
          reset_gensym/1,               % +Base
          gensym/2                      % +Base, -Symbol
        ]).
:- set_prolog_flag(generate_debug_info, false).

/** <module> Generate unique symbols

The predicate gensym/2 is a  traditional   predicate  to generate unique
symbols.  It should be used with care.
*/

%!  gensym(+Base, -Unique)
%
%   Generate <Base>1, <Base>2, etc atoms   on  each subsequent call.
%   Note that there is nothing  that   prevents  other  parts of the
%   application to `invent'  the  same   identifier.  The  predicate
%   gensym/2 is thread-safe in the sense that two threads generating
%   identifiers from the same Base  will   never  generate  the same
%   identifier.
%
%   @see    uuid/1, term_hash/2, variant_sha1/2 may be used to
%           generate various unique or content-based identifiers
%           safely.

gensym(Base, Atom) :-
    atom_concat('$gs_', Base, Key),
    flag(Key, Old, Old+1),
    record_gensym(Key, Old),
    New is Old+1,
    atom_concat(Base, New, Atom).

record_gensym(Key, 0) :-
    !,
    recordz('$gensym', Key).
record_gensym(_, _).

%!  reset_gensym
%
%   Reset all gensym counters.  Please beware this is dangerous: gensym
%   may be in use by other modules that do not expect their counter to
%   be reset!

reset_gensym :-
    with_mutex('$gensym', do_reset_gensym).

do_reset_gensym :-
    (   recorded('$gensym', Key, Ref),
        erase(Ref),
        set_flag(Key, 0),
        fail
    ;   true
    ).

%!  reset_gensym(+Base)
%
%   Reset a specific gensym counter.  Please beware this still is
%   dangerous as other code may use gensym with the same atom!

reset_gensym(Base) :-
    atom_concat('$gs_', Base, Key),
    set_flag(Key, 0).

:- multifile sandbox:safe_primitive/1.

sandbox:safe_primitive(gensym:gensym(_,_)).
