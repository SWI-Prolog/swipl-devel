/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           www.swi-prolog.org
    Copyright (c)  2013-2015, University of Amsterdam
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

:- module('$dicts',
          [ '.'/3                               % +Left, +Right, -Result
          ]).

%!  .(+R, +L, -Result)
%
%   Evaluate dot expressions. Note that  '$get_dict_ex' fails if the
%   first argument is not a dict or the second is not a valid key or
%   unbound.

.(Data, Func, Value) :-
    (   '$get_dict_ex'(Func, Data, V0)
    *-> Value = V0
    ;   is_dict(Data, Tag)
    ->  eval_dict_function(Func, Tag, Data, Value)
    ;   is_list(Data)
    ->  (   (atomic(Func) ; var(Func))
        ->  dict_create(Dict, _, Data),
            '$get_dict_ex'(Func, Dict, Value)
        ;   '$type_error'(atom, Func)
        )
    ;   '$type_error'(dict, Data)
    ).


%!  eval_dict_function(+Func, +Tag, +Dict, -Value)
%
%   Test for predefined functions on dicts or evaluate a user-defined
%   function.

eval_dict_function(get(Key), _, Dict, Value) :-
    !,
    get_dict(Key, Dict, Value).
eval_dict_function(put(Key, Value), _, Dict, NewDict) :-
    !,
    (   atomic(Key)
    ->  put_dict(Key, Dict, Value, NewDict)
    ;   put_dict_path(Key, Dict, Value, NewDict)
    ).
eval_dict_function(put(New), _, Dict, NewDict) :-
    !,
    put_dict(New, Dict, NewDict).
eval_dict_function(Func, Tag, Dict, Value) :-
    call(Tag:Func, Dict, Value).


%!  put_dict_path(+KeyPath, +Dict, +Value, -NewDict)
%
%   Add/replace  a  value  according  to  a  path  definition.  Path
%   segments are separated using '/'.

put_dict_path(Key, Dict, Value, NewDict) :-
    atom(Key),
    !,
    put_dict(Key, Dict, Value, NewDict).
put_dict_path(Path, Dict, Value, NewDict) :-
    get_dict_path(Path, Dict, _Old, NewDict, Value).

get_dict_path(Path, _, _, _, _) :-
    var(Path),
    !,
    '$instantiation_error'(Path).
get_dict_path(Path/Key, Dict, Old, NewDict, New) :-
    !,
    get_dict_path(Path, Dict, OldD, NewDict, NewD),
    (   get_dict(Key, OldD, Old, NewD, New),
        is_dict(Old)
    ->  true
    ;   Old = _{},
        put_dict(Key, OldD, New, NewD)
    ).
get_dict_path(Key, Dict, Old, NewDict, New) :-
    get_dict(Key, Dict, Old, NewDict, New),
    is_dict(Old),
    !.
get_dict_path(Key, Dict, _{}, NewDict, New) :-
    put_dict(Key, Dict, New, NewDict).


                 /*******************************
                 *             REGISTER         *
                 *******************************/

%!  system:term_expansion(+TermIn, -TermOut)
%
%   Support := syntax for defining new functions.
%
%   @tbd    Modify to term_expansion/4, including position
%           management

% note that we need FHead because using a term there rewrites the
% clauses using function expansion.

expand_dict_function((QFHead := V0 :- Body), (QHead :- Body, Eval)) :-
    fqhead(QFHead, FHead, Head, QHead),
    compound(FHead),
    FHead =.. [.,R,M],
    callable(M),
    !,
    '$expand':replace_functions(V0, Eval, V, _Ctx),
    compound_name_arguments(M, Name, Args0),
    '$append'(Args0, [R,V], Args),
    compound_name_arguments(Head, Name, Args).
expand_dict_function((QFHead := V0), (QHead :- Eval)) :-
    fqhead(QFHead, FHead, Head, QHead),
    compound(FHead),
    FHead =.. [.,R,M],
    callable(M),
    !,
    '$expand':replace_functions(V0, Eval, V, _Ctx),
    compound_name_arguments(M, Name, Args0),
    '$append'(Args0, [R,V], Args),
    compound_name_arguments(Head, Name, Args).

fqhead(M:FHead, FHead, Head, M:Head) :- !.
fqhead(FHead,   FHead, Head,   Head).


system:term_expansion(FDecl, Clause) :-
    expand_dict_function(FDecl, Clause).
system:term_expansion(M:FDecl, QClause) :-
    expand_dict_function(FDecl, Clause),
    !,
    QClause = M:Clause.
