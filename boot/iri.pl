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

:- module('$iri',
          [ register_iri_scheme/3               % +Scheme, :Handler, +Options
          ]).

:- meta_predicate
    register_iri_scheme(+, 3, +).

:- multifile
    iri_scheme_handler/2.

%!  'iri_hook'(+Scheme, +Value, +Action, -Result) is semidet.

:- public iri_hook/4.
iri_hook(Scheme, IRI, Action, Result) :-
    iri_scheme_handler(Scheme, Handler),
    !,
    call(Handler, Action, IRI, Result).
iri_hook(Scheme, _, _, _) :-
    '$existence_error'(iri_scheme, Scheme).

%!  register_iri_scheme(+Scheme, :Handler, +Options) is det.

register_iri_scheme(Scheme, Handler, _Options) :-
    throw(error(context_error(nodirective,
                              register_iri_scheme(Scheme, Handler)), _)).

system:term_expansion((:- register_iri_scheme(Scheme, Handler, _)),
                      '$iri':iri_scheme_handler(Scheme, Module:Closure)) :-
    prolog_load_context(module, M),
    strip_module(M:Handler, Module, Closure).
