/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1999-2015, University of Amsterdam
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

:- module(edinburgh,
          [ display/1,
            display/2,
            unknown/2,
            reconsult/1,
            debug/0,
            nodebug/0,
            fileerrors/2
          ]).

:- meta_predicate
    unknown(:, :),
    reconsult(:).


/** <module> Some traditional Edinburgh predicates

This module defines  predicates  from   `traditional  Edinburgh  Prolog'
(Dec10 and C-Prolog) whose functionality  has   been  replaced  by (ISO)
Standard Prolog.
*/

                 /*******************************
                 *            TERM I/O          *
                 *******************************/

%!  display(+Term) is det.
%!  display(+Stream, +Term) is det.
%
%   Write a term, ignoring operators.
%
%   @deprecated     New code must use write_term/3 using the option
%                   ignore_ops(true).

display(Term) :-
    write_term(Term, [quoted(true), ignore_ops(true)]).
display(Stream, Term) :-
    write_term(Stream, Term, [quoted(true), ignore_ops(true)]).

%!  unknown(-Old, +New) is det.
%
%   Edinburgh Prolog predicate for dealing dealing with undefined
%   procedures

unknown(M:Old, M:New) :-
    current_prolog_flag(M:unknown, O),
    map_unknown(O, Old),
    map_unknown(N, New),
    !,
    set_prolog_flag(M:unknown, N).

map_unknown(error,   trace).
map_unknown(warning, trace).
map_unknown(fail,    fail).

%!  reconsult(+FileOrList) is det.
%
%   Load source file(s), wiping the  old content first. SWI-Prolog's
%   consult/1 and related predicates always do this.
%
%   @deprecated The Edinburgh Prolog consult/reconsult distinction
%   is no longer used throughout most of the Prolog world.

reconsult(File) :-
    consult(File).

%!  debug is det.
%!  nodebug is det.
%
%   Switch on/off debug mode.  Note that nodebug/0 has been defined
%   such that is is not traced itself.

debug   :- set_prolog_flag(debug, true).
nodebug :- notrace, set_prolog_flag(debug, false).

:- '$hide'(nodebug/0).

%!  fileerrors(-Old, +New) is det.
%
%   Query and change the  fileerrors  flag.   Default  it  is set to
%   =true=, causing file operations to   raise an exception. Setting
%   it to =false=  activates  the  old   Edinburgh  mode  of  silent
%   failure.
%
%   @deprecated     New code should use catch/3 to handle file errors
%                   silently

fileerrors(Old, New) :-
    current_prolog_flag(fileerrors, Old),
    (   Old == New
    ->  true
    ;   set_prolog_flag(fileerrors, New)
    ).
