/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2017, VU University Amsterdam
			 CWI Amsterdam
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

:- module(prolog_jiti,
          [ jiti_list/0,
            jiti_list/1                         % +Spec
          ]).
:- use_module(library(apply)).

:- meta_predicate
    jiti_list(:).

/** <module> Just In Time Indexing (JITI) utilities

This module provides utilities to   examine just-in-time indexes created
by the system and can help diagnosing space and performance issues.

@tbd	Use print_message/2 and dynamically figure out the column width.
*/


%!  jiti_list is det.
%!  jiti_list(:Spec) is det.
%
%   List the JITI (Just In  Time   Indexes)  of selected predicates. The
%   predicate jiti_list/0 list all just-in-time  indexed predicates. The
%   predicate jiti_list/1 takes one of  the   patterns  below. All parts
%   except for Name  can  be  variables.   The  last  pattern  takes  an
%   arbitrary number of arguments.
%
%     - Module:Head
%     - Module:Name/Arity
%     - Module:Name

jiti_list :-
    jiti_list(_:_).

jiti_list(Module:Name/Arity) :-
    atom(Name),
    integer(Arity),
    !,
    functor(Head, Name, Arity),
    jiti_list(Module:Head).
jiti_list(Module:Name/Arity) :-
    atom(Name),
    var(Arity),
    !,
    freeze(Head, functor(Head, Name, _)),
    jiti_list(Module:Head).
jiti_list(Module:Name) :-
    atom(Name),
    !,
    freeze(Head, functor(Head, Name, _)),
    jiti_list(Module:Head).
jiti_list(Head) :-
    findall(Head-Indexed,
            (   predicate_property(Head, indexed(Indexed)),
                \+ predicate_property(Head, imported_from(_))
            ), Pairs),
    format('Predicate~46|~w ~t~8+ ~t~w~6+ ~t~w~6+~n',
           ['Indexed','Buckets','Speedup']),
    format('~`=t~68|~n'),
    maplist(print_indexed, Pairs).

print_indexed((M:Head)-[Args-hash(Buckets,Speedup,_List)|More]) :-
    functor(Head, Name, Arity),
    format('~q ~t~48|~p ~t~8+ ~t~D~6+ ~t~1f~6+~n',
           [M:Name/Arity, Args,Buckets,Speedup]),
    maplist(print_secondary_index, More).

print_secondary_index(Args-hash(Buckets,Speedup,_List)) :-
    format('~t~48|~p ~t~8+ ~t~D~6+ ~t~1f~6+~n',
           [Args,Buckets,Speedup]).
