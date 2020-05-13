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

:- module(setof,
          [ excess_vars/4	% +Term, +ExistVarTerm, +AddVarList, -VarList
          ]).
:- use_module(library(lists)).

%!  excess_vars(+Term, +ExistVarTerm, +AddVarList, -VarList)
%
%   Returns in VarList the  list  of   (free)  variables  found  in Term
%   concatenated to the end of AddVarList.   (In normal usage AddVarList
%   is passed in as an empty list.)   ExistVarTerm  is a term containing
%   variables assumed to  be  quantified  in   Term  so  none  of  these
%   variables are returned in the  resulting   list  (unless they are in
%   AddVarList.) Subterms of Term of   the  form `(VarTerm^SubTerm)` are
%   treated specially: all variables  in  VarTerm   are  assumed  to  be
%   quantified in SubTerm, and so no   occurrence  of these variables in
%   SubTerm is collected into the resulting list.

excess_vars(Term, ExistVarTerm, AddVarList, VarList) :-
    '$free_variable_set'(ExistVarTerm^Term, _Goal, Vars0),
    append(AddVarList, Vars0, VarList).
