/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2014-2015, VU University Amsterdam
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

:- module(modules,
          [ in_temporary_module/3               % ?Module, :Setup, :Goal
          ]).


/** <module> Module utility predicates
*/

:- meta_predicate
    in_temporary_module(?, 0, 0).

%!  in_temporary_module(?Module, :Setup, :Goal)
%
%   Run Goal on temporary loaded sources  and discard the module and
%   loaded predicates after completion.  This predicate performs the
%   following steps:
%
%     1. If Module is unbound, create a unique identifier for it.
%     2. Turn Module into a _temporary_ module using set_module/1.
%        Note that this requires the module to be non-existent or
%        empty.  If Module is specified, it should typically be set
%        to a unique value as obtained from e.g. uuid/1.
%     3. Run Setup in the context of Module.
%     4. If setup succeeded possible choice points are discarded
%        and Goal is started.
%
%   The  logical  result  of  this   predicate    is   the  same  as
%   `(Setup@Module -> Goal@Module)`, i.e., both   Setup and Goal are
%   resolved relative to the current  module,   but  executed in the
%   context of Module.  If  Goal  must   be  called  in  Module, use
%   `call(Goal)`.
%
%   The module and all  its  predicates   are  destroyed  after Goal
%   terminates, as defined by setup_call_cleanup/3.
%
%   *Discussion* This predicate is intended to   load programs in an
%   isolated   environment   and   reclaim   all   resources.   This
%   unfortunately is incomplete:
%
%     - Running the code may leave side effects such as creating
%       records, flags, changing Prolog flags, etc.  The system
%       has no provisions to track this.
%     - So called _functors_ (name/arity pairs) are not yet subject
%       to garbage collection.  Functors are both used to define
%       predicates and to create compound terms.
%
%   @see    library(sandbox) determines whether unknown goals are safe
%           to call.
%   @see    load_files/2 offers the option sandboxed(true) to load code
%           from unknown sources safely.

in_temporary_module(Module, Setup, Goal) :-
    setup_call_cleanup(
        prepare_temporary_module(Module),
        (   @(Setup, Module)
        ->  @(Goal,  Module)
        ),
        destroy_module(Module)).

prepare_temporary_module(Module) :-
    var(Module),
    !,
    (   repeat,
        I is random(1<<63),
        atom_concat('tmp-', I, Module),
        catch(set_module(Module:class(temporary)),
              error(permission_error(_,_,_),_), fail)
    ->  true
    ).
prepare_temporary_module(Module) :-
    set_module(Module:class(temporary)).

destroy_module(Module) :-
    retractall(system:'$load_context_module'(_File, Module, _Options)),
    '$destroy_module'(Module).

