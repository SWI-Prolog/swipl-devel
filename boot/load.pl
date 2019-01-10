/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2018, University of Amsterdam
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

% Load the rest of the system as modules, so we can write a bit more
% readable code.  First we need to load term-expansion, etc. because
% this gives us DCGs.  Then we need to replace the dummy clauses for
% '$expand_term'/4 and '$expand_goal'/2 with links to the real thing.

:- consult([ gc,
             expand,
             dcg
           ]).

:- abolish('$expand_goal'/2),
   asserta(('$expand_goal'(In, Out) :- expand_goal(In, Out))),
   abolish('$expand_term'/4),
   asserta(('$expand_term'(In, P0, Out, P) :- expand_term(In, P0, Out, P))),
   compile_predicates(['$expand_goal'/2, '$expand_term'/4]),
   '$set_predicate_attribute'(system:'$expand_goal'(_,_), system, true),
   '$set_predicate_attribute'(system:'$expand_term'(_,_,_,_), system, true),
   '$set_predicate_attribute'(system:'$expand_goal'(_,_), hide_childs, true),
   '$set_predicate_attribute'(system:'$expand_term'(_,_,_,_), hide_childs, true).

:- consult([ license,                   % requires DCG
             syspred,
             messages,
             toplevel,
             attvar,
             bags,
             apply,
             history,
             dwim,
             parms,
             autoload,
             iri,
             qlf,
             rc,
             predopts,
             packs,
             dicts,
             engines,
             tabling,
             user:topvars
           ]).
