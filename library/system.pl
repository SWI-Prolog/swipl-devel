/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2011, University of Amsterdam
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

:- module(swi_system_utilities,
        [ lock_predicate/1,
          unlock_predicate/1,
          system_mode/1,
          system_module/0
        ]).
:- use_module(library(error)).

/** <module> System utilities

This module provides some tools to deal with system predicates. System
predicates cannot be traced or redefined.

@deprecated     Use :- set_prolog_flag(generate_debug_info, false) to
                hide predicate internals from the tracer.
@tbd            Move this functionality to prolog flags.
*/

%!  system_mode(+Boolean) is det.
%
%   Switch the system into system or user mode.  When in system mode,
%   system predicates loose most of their special properties, so it
%   becomes possible to trace and even redefine them.
%
%   @deprecated  New code should use the prolog flag =access_level=.

system_mode(Val) :-
    must_be(boolean, Val),
    (   Val == true
    ->  set_prolog_flag(access_level, system)
    ;   set_prolog_flag(access_level, user)
    ).

%!  system_module
%
%   Any predicate defined after this declaraction   uptil the end of
%   the file will become a system   predicate. Normally invoked by a
%   directive immediately following the module declaration.

system_module :-
    set_prolog_flag(generate_debug_info, false).

:- meta_predicate
    lock_predicate(:),
    unlock_predicate(:).

%!  lock_predicate(+PredInd)
%
%   Transform a predicate into a system predicate.

lock_predicate(PredInd) :-
    '$set_predicate_attribute'(PredInd, system, true).

%!  unlock_predicate(+PredInd)
%
%   Transform a system predicate into a normal system predicate.

unlock_predicate(PredInd) :-
    '$set_predicate_attribute'(PredInd, system, false).
