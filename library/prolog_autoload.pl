/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2011-2018, VU University Amsterdam
                              CWI, Amsterdam
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

:- module(prolog_autoload,
          [ autoload/0,
            autoload/1                          % +Options
          ]).
:- use_module(library(option)).
:- use_module(library(error)).
:- use_module(library(aggregate)).
:- use_module(library(prolog_codewalk)).
:- use_module(library(check), [ list_undefined/0 ]).

:- predicate_options(autoload/1, 1,
                     [ verbose(boolean),
                       undefined(oneof([ignore,error]))
                     ]).

/** <module> Autoload all dependencies

The autoloader is there to smoothen   program  development. It liberates
the programmer from finding the  library   that  defines some particular
predicate and including  the  proper   use_module/1,2  directive  in the
sources. This is even better at the toplevel, where just using maplist/3
is way more comfortable than  first   having  to load library(apply). In
addition, it reduces the startup time   of  applications by only loading
the necessary bits.

Of course, there is also a price. One   is  that it becomes less obvious
from where some predicate is loaded and  thus whether you have the right
definition.  The  second  issue  is  that  it  is  harder  to  create  a
stand-alone executable because this executable,   without  access to the
development system, can no longer rely  on autoloading. Finally, program
analysis becomes harder because the program may be incomplete.

This  library  provides  autoload/0  and   autoload/1  to  autoload  all
predicates that are referenced by the program. Now, this is not possible
in Prolog because the language allows   for constructing arbitrary goals
and runtime and calling them (e.g., read(X), call(X)).

The implementation relies on code analysis of  the bodies of all clauses
and all initialization goals.
*/

:- thread_local
    autoloaded_count/1.

%!  autoload is det.
%!  autoload(+Options) is det.
%
%   Force all necessary autoloading to be done _now_.  Options:
%
%       * verbose(+Boolean)
%       If `true` (default `false`), report on the files loaded.
%       * undefined(+Action)
%       Action defines what happens if the analysis finds a
%       definitely undefined predicate.  One of `ignore` or
%       `error`.  Default is `ignore`.

autoload :-
    autoload([]).

autoload(Options) :-
    must_be(list, Options),
    statistics(cputime, T0),
    aggregate_all(count, source_file(_), OldFileCount),
    call_cleanup(
        autoload(0, Iterations, Options),
        check:collect_undef(Undef)),
    aggregate_all(count, source_file(_), NewFileCount),
    statistics(cputime, T1),
    Time is T1-T0,
    information_level(Level, Options),
    NewFiles is NewFileCount - OldFileCount,
    print_message(Level, autoload(completed(Iterations, Time, NewFiles))),
    report_undefined(Undef).

autoload(Iteration0, Iterations, Options) :-
    statistics(cputime, T0),
    autoload_step(NewFiles, NewPreds, Options),
    statistics(cputime, T1),
    Time is T1-T0,
    succ(Iteration0, Iteration),
    (   NewFiles > 0
    ->  information_level(Level, Options),
        print_message(Level, autoload(reiterate(Iteration,
                                                NewFiles, NewPreds, Time))),
        autoload(Iteration, Iterations, Options)
    ;   Iterations = Iteration
    ).

information_level(Level, Options) :-
    (   option(verbose(true), Options)
    ->  Level = informational
    ;   Level = silent
    ).

%!  autoload_step(-NewFiles, -NewPreds, +Options) is det.
%
%   Scan through the program and   autoload all undefined referenced
%   predicates.
%
%   @param NewFiles is unified to the number of files loaded
%   @param NewPreds is unified to the number of predicates imported
%          using the autoloader.

autoload_step(NewFiles, NewPreds, Options) :-
    option(verbose(Verbose), Options, false),
    walk_options(Options, WalkOptions),
    aggregate_all(count, source_file(_), OldFileCount),
    setup_call_cleanup(
        ( current_prolog_flag(autoload, OldAutoLoad),
          current_prolog_flag(verbose_autoload, OldVerbose),
          set_prolog_flag(autoload, true),
          set_prolog_flag(verbose_autoload, Verbose),
          assert_autoload_hook(Ref),
          asserta(autoloaded_count(0))
        ),
        prolog_walk_code(WalkOptions),
        ( retract(autoloaded_count(Count)),
          erase(Ref),
          set_prolog_flag(autoload, OldAutoLoad),
          set_prolog_flag(verbose_autoload, OldVerbose)
        )),
    aggregate_all(count, source_file(_), NewFileCount),
    NewPreds = Count,
    NewFiles is NewFileCount - OldFileCount.

assert_autoload_hook(Ref) :-
    asserta((user:message_hook(autoload(Module:Name/Arity, Library), _, _) :-
                    autoloaded(Module:Name/Arity, Library)), Ref).

:- public
    autoloaded/2.

autoloaded(_, _) :-
    retract(autoloaded_count(N)),
    succ(N, N2),
    asserta(autoloaded_count(N2)),
    fail.                                   % proceed with other hooks

%!  walk_options(+AutoloadOptions, -WalkOptions) is det.
%
%   Construct the option list  for  the  code   walker.  If  we  see  an
%   undefined predicate, we must collect these rather than printing them
%   or immediately terminating with an exception.  This reuses code from
%   library(check).

walk_options([], []).
walk_options([verbose(V)|T0], [verbose(V)|T]) :-
    !,
    walk_options(T0, T).
walk_options([undefined(error)|T0],
             [ undefined(trace),
               on_trace(check:found_undef)
             | T
             ]) :-
    !,
    walk_options(T0, T).
walk_options([_|T0], T) :-
    walk_options(T0, T).


%!  report_undefined(+Undefined) is det.
%
%

report_undefined([]) :-
    !.
report_undefined(Grouped) :-
    existence_error(procedures, Grouped).


                 /*******************************
                 *            MESSAGES          *
                 *******************************/

:- multifile
    prolog:message//1,
    prolog:error_message//1.

prolog:message(autoload(reiterate(Iteration, NewFiles, NewPreds, Time))) -->
    [ 'Autoloader: iteration ~D resolved ~D predicates \c
          and loaded ~D files in ~3f seconds.  Restarting ...'-
      [Iteration, NewPreds, NewFiles, Time]
    ].
prolog:message(autoload(completed(Iterations, Time, NewFiles))) -->
    [ 'Autoloader: loaded ~D files in ~D iterations in ~3f seconds'-
      [NewFiles, Iterations, Time] ].

prolog:error_message(existence_error(procedures, Grouped)) -->
    prolog:message(check(undefined_procedures, Grouped)).
