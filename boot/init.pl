/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2018, University of Amsterdam
                              VU University Amsterdam
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

/*
Consult, derivates and basic things.   This  module  is  loaded  by  the
C-written  bootstrap  compiler.

The $:- directive  is  executed  by  the  bootstrap  compiler,  but  not
inserted  in  the  intermediate  code  file.   Used  to print diagnostic
messages and start the Prolog defined compiler for  the  remaining  boot
modules.

If you want  to  debug  this  module,  put  a  '$:-'(trace).   directive
somewhere.   The  tracer will work properly under boot compilation as it
will use the C defined write predicate  to  print  goals  and  does  not
attempt to call the Prolog defined trace interceptor.
*/

                /********************************
                *    LOAD INTO MODULE SYSTEM    *
                ********************************/

:- '$set_source_module'(system).

'$boot_message'(_Format, _Args) :-
    current_prolog_flag(verbose, silent),
    !.
'$boot_message'(Format, Args) :-
    format(Format, Args),
    !.

'$:-'('$boot_message'('Loading boot file ...~n', [])).


                /********************************
                *          DIRECTIVES           *
                *********************************/

:- meta_predicate
    dynamic(:),
    multifile(:),
    public(:),
    module_transparent(:),
    discontiguous(:),
    volatile(:),
    thread_local(:),
    noprofile(:),
    non_terminal(:),
    '$clausable'(:),
    '$iso'(:),
    '$hide'(:).

%!  dynamic(+Spec) is det.
%!  multifile(+Spec) is det.
%!  module_transparent(+Spec) is det.
%!  discontiguous(+Spec) is det.
%!  volatile(+Spec) is det.
%!  thread_local(+Spec) is det.
%!  noprofile(+Spec) is det.
%!  public(+Spec) is det.
%!  non_terminal(+Spec) is det.
%
%   Predicate versions of standard  directives   that  set predicate
%   attributes. These predicates bail out with an error on the first
%   failure (typically permission errors).

dynamic(Spec)            :- '$set_pattr'(Spec, pred, (dynamic)).
multifile(Spec)          :- '$set_pattr'(Spec, pred, (multifile)).
module_transparent(Spec) :- '$set_pattr'(Spec, pred, (transparent)).
discontiguous(Spec)      :- '$set_pattr'(Spec, pred, (discontiguous)).
volatile(Spec)           :- '$set_pattr'(Spec, pred, (volatile)).
thread_local(Spec)       :- '$set_pattr'(Spec, pred, (thread_local)).
noprofile(Spec)          :- '$set_pattr'(Spec, pred, (noprofile)).
public(Spec)             :- '$set_pattr'(Spec, pred, (public)).
non_terminal(Spec)       :- '$set_pattr'(Spec, pred, (non_terminal)).
'$iso'(Spec)             :- '$set_pattr'(Spec, pred, (iso)).
'$clausable'(Spec)       :- '$set_pattr'(Spec, pred, (clausable)).

'$set_pattr'(M:Pred, How, Attr) :-
    '$set_pattr'(Pred, M, How, Attr).

'$set_pattr'(X, _, _, _) :-
    var(X),
    throw(error(instantiation_error, _)).
'$set_pattr'([], _, _, _) :- !.
'$set_pattr'([H|T], M, How, Attr) :-           % ISO
    !,
    '$set_pattr'(H, M, How, Attr),
    '$set_pattr'(T, M, How, Attr).
'$set_pattr'((A,B), M, How, Attr) :-           % ISO and traditional
    !,
    '$set_pattr'(A, M, How, Attr),
    '$set_pattr'(B, M, How, Attr).
'$set_pattr'(M:T, _, How, Attr) :-
    !,
    '$set_pattr'(T, M, How, Attr).
'$set_pattr'(A, M, pred, Attr) :-
    !,
    '$set_predicate_attribute'(M:A, Attr, true).
'$set_pattr'(A, M, directive, Attr) :-
    !,
    catch('$set_predicate_attribute'(M:A, Attr, true),
          error(E, _),
          print_message(error, error(E, context((Attr)/1,_)))).

%!  '$pattr_directive'(+Spec, +Module) is det.
%
%   This implements the directive version of dynamic/1, multifile/1,
%   etc. This version catches and prints   errors.  If the directive
%   specifies  multiple  predicates,  processing    after  an  error
%   continues with the remaining predicates.

'$pattr_directive'(dynamic(Spec), M) :-
    '$set_pattr'(Spec, M, directive, (dynamic)).
'$pattr_directive'(multifile(Spec), M) :-
    '$set_pattr'(Spec, M, directive, (multifile)).
'$pattr_directive'(module_transparent(Spec), M) :-
    '$set_pattr'(Spec, M, directive, (transparent)).
'$pattr_directive'(discontiguous(Spec), M) :-
    '$set_pattr'(Spec, M, directive, (discontiguous)).
'$pattr_directive'(volatile(Spec), M) :-
    '$set_pattr'(Spec, M, directive, (volatile)).
'$pattr_directive'(thread_local(Spec), M) :-
    '$set_pattr'(Spec, M, directive, (thread_local)).
'$pattr_directive'(noprofile(Spec), M) :-
    '$set_pattr'(Spec, M, directive, (noprofile)).
'$pattr_directive'(public(Spec), M) :-
    '$set_pattr'(Spec, M, directive, (public)).


%!  '$hide'(:PI)
%
%   Predicates protected this way are never visible in the tracer.

'$hide'(Pred) :-
    '$set_predicate_attribute'(Pred, trace, false).

:- '$iso'(((dynamic)/1, (multifile)/1, (discontiguous)/1)).


                /********************************
                *       CALLING, CONTROL        *
                *********************************/

:- noprofile((call/1,
              catch/3,
              once/1,
              ignore/1,
              call_cleanup/2,
              call_cleanup/3,
              setup_call_cleanup/3,
              setup_call_catcher_cleanup/4)).

:- meta_predicate
    ';'(0,0),
    ','(0,0),
    @(0,+),
    call(0),
    call(1,?),
    call(2,?,?),
    call(3,?,?,?),
    call(4,?,?,?,?),
    call(5,?,?,?,?,?),
    call(6,?,?,?,?,?,?),
    call(7,?,?,?,?,?,?,?),
    not(0),
    \+(0),
    '->'(0,0),
    '*->'(0,0),
    once(0),
    ignore(0),
    catch(0,?,0),
    reset(0,?,-),
    setup_call_cleanup(0,0,0),
    setup_call_catcher_cleanup(0,0,?,0),
    call_cleanup(0,0),
    call_cleanup(0,?,0),
    catch_with_backtrace(0,?,0),
    '$meta_call'(0).

:- '$iso'((call/1, (\+)/1, once/1, (;)/2, (',')/2, (->)/2, catch/3)).

% The control structures are always compiled, both   if they appear in a
% clause body and if they are handed  to   call/1.  The only way to call
% these predicates is by means of  call/2..   In  that case, we call the
% hole control structure again to get it compiled by call/1 and properly
% deal  with  !,  etc.  Another  reason  for  having  these  things   as
% predicates is to be able to define   properties for them, helping code
% analyzers.

(M0:If ; M0:Then) :- !, call(M0:(If ; Then)).
(M1:If ; M2:Then) :-    call(M1:(If ; M2:Then)).
(G1   , G2)       :-    call((G1   , G2)).
(If  -> Then)     :-    call((If  -> Then)).
(If *-> Then)     :-    call((If *-> Then)).
@(Goal,Module)    :-    @(Goal,Module).

%!  '$meta_call'(:Goal)
%
%   Interpreted  meta-call  implementation.  By    default,   call/1
%   compiles its argument into  a   temporary  clause. This realises
%   better  performance  if  the  (complex)  goal   does  a  lot  of
%   backtracking  because  this   interpreted    version   needs  to
%   re-interpret the remainder of the goal after backtracking.
%
%   This implementation is used by  reset/3 because the continuation
%   cannot be captured if it contains   a  such a compiled temporary
%   clause.

'$meta_call'(M:G) :-
    prolog_current_choice(Ch),
    '$meta_call'(G, M, Ch).

'$meta_call'(Var, _, _) :-
    var(Var),
    !,
    '$instantiation_error'(Var).
'$meta_call'((A,B), M, Ch) :-
    !,
    '$meta_call'(A, M, Ch),
    '$meta_call'(B, M, Ch).
'$meta_call'((I->T;E), M, Ch) :-
    !,
    (   prolog_current_choice(Ch2),
        '$meta_call'(I, M, Ch2)
    ->  '$meta_call'(T, M, Ch)
    ;   '$meta_call'(E, M, Ch)
    ).
'$meta_call'((I*->T;E), M, Ch) :-
    !,
    (   prolog_current_choice(Ch2),
        '$meta_call'(I, M, Ch2)
    *-> '$meta_call'(T, M, Ch)
    ;   '$meta_call'(E, M, Ch)
    ).
'$meta_call'((I->T), M, Ch) :-
    !,
    (   prolog_current_choice(Ch2),
        '$meta_call'(I, M, Ch2)
    ->  '$meta_call'(T, M, Ch)
    ).
'$meta_call'((I*->T), M, Ch) :-
    !,
    prolog_current_choice(Ch2),
    '$meta_call'(I, M, Ch2),
    '$meta_call'(T, M, Ch).
'$meta_call'((A;B), M, Ch) :-
    !,
    (   '$meta_call'(A, M, Ch)
    ;   '$meta_call'(B, M, Ch)
    ).
'$meta_call'(\+(G), M, _) :-
    !,
    prolog_current_choice(Ch),
    \+ '$meta_call'(G, M, Ch).
'$meta_call'(call(G), M, _) :-
    !,
    prolog_current_choice(Ch),
    '$meta_call'(G, M, Ch).
'$meta_call'(M:G, _, Ch) :-
    !,
    '$meta_call'(G, M, Ch).
'$meta_call'(!, _, Ch) :-
    prolog_cut_to(Ch).
'$meta_call'(G, M, _Ch) :-
    call(M:G).

%!  call(:Closure, ?A).
%!  call(:Closure, ?A1, ?A2).
%!  call(:Closure, ?A1, ?A2, ?A3).
%!  call(:Closure, ?A1, ?A2, ?A3, ?A4).
%!  call(:Closure, ?A1, ?A2, ?A3, ?A4, ?A5).
%!  call(:Closure, ?A1, ?A2, ?A3, ?A4, ?A5, ?A6).
%!  call(:Closure, ?A1, ?A2, ?A3, ?A4, ?A5, ?A6, ?A7).
%
%   Arity 2..8 is demanded by the   ISO standard. Higher arities are
%   supported, but handled by the compiler.   This  implies they are
%   not backed up by predicates and   analyzers  thus cannot ask for
%   their  properties.  Analyzers  should    hard-code  handling  of
%   call/2..

:- '$iso'((call/2,
           call/3,
           call/4,
           call/5,
           call/6,
           call/7,
           call/8)).

call(Goal) :-                           % make these available as predicates
    Goal.
call(Goal, A) :-
    call(Goal, A).
call(Goal, A, B) :-
    call(Goal, A, B).
call(Goal, A, B, C) :-
    call(Goal, A, B, C).
call(Goal, A, B, C, D) :-
    call(Goal, A, B, C, D).
call(Goal, A, B, C, D, E) :-
    call(Goal, A, B, C, D, E).
call(Goal, A, B, C, D, E, F) :-
    call(Goal, A, B, C, D, E, F).
call(Goal, A, B, C, D, E, F, G) :-
    call(Goal, A, B, C, D, E, F, G).

%!  not(:Goal) is semidet.
%
%   Pre-ISO version of \+/1. Note that  some systems define not/1 as
%   a logically more sound version of \+/1.

not(Goal) :-
    \+ Goal.

%!  \+(:Goal) is semidet.
%
%   Predicate version that allows for meta-calling.

\+ Goal :-
    \+ Goal.

%!  once(:Goal) is semidet.
%
%   ISO predicate, acting as call((Goal, !)).

once(Goal) :-
    Goal,
    !.

%!  ignore(:Goal) is det.
%
%   Call Goal, cut choice-points on success  and succeed on failure.
%   intended for calling side-effects and proceed on failure.

ignore(Goal) :-
    Goal,
    !.
ignore(_Goal).

:- '$iso'((false/0)).

%!  false.
%
%   Synonym for fail/0, providing a declarative reading.

false :-
    fail.

%!  catch(:Goal, +Catcher, :Recover)
%
%   ISO compliant exception handling.

catch(_Goal, _Catcher, _Recover) :-
    '$catch'.                       % Maps to I_CATCH, I_EXITCATCH

%!  prolog_cut_to(+Choice)
%
%   Cut all choice points after Choice

prolog_cut_to(_Choice) :-
    '$cut'.                         % Maps to I_CUTCHP

%!  reset(:Goal, ?Ball, -Continue)
%
%   Delimited continuation support.

reset(_Goal, _Ball, _Cont) :-
    '$reset'.

%!  shift(+Ball)
%
%   Shift control back to the enclosing reset/3

shift(Ball) :-
    '$shift'(Ball).

%!  call_continuation(+Continuation:list)
%
%   Call a continuation as created  by   shift/1.  The continuation is a
%   list of '$cont$'(Clause, PC, EnvironmentArg,   ...)  structures. The
%   predicate  '$call_one_tail_body'/1  creates   a    frame   from  the
%   continuation and calls this.
%
%   Note that we can technically also  push the entire continuation onto
%   the environment and  call  it.  Doing   it  incrementally  as  below
%   exploits last-call optimization  and   therefore  possible quadratic
%   expansion of the continuation.

call_continuation([]).
call_continuation([TB|Rest]) :-
    (   Rest == []
    ->  '$call_continuation'(TB)
    ;   '$call_continuation'(TB),
        call_continuation(Rest)
    ).

%!  catch_with_backtrace(:Goal, ?Ball, :Recover)
%
%   As catch/3, but tell library(prolog_stack) to  record a backtrace in
%   case of an exception.

catch_with_backtrace(Goal, Ball, Recover) :-
    catch(Goal, Ball, Recover),
    '$no_lco'.

'$no_lco'.

%!  '$recover_and_rethrow'(:Goal, +Term)
%
%   This goal is used to wrap  the   catch/3  recover handler if the
%   exception is not supposed to be   `catchable'.  An example of an
%   uncachable exception is '$aborted', used   by abort/0. Note that
%   we cut to ensure  that  the   exception  is  not delayed forever
%   because the recover handler leaves a choicepoint.

:- public '$recover_and_rethrow'/2.

'$recover_and_rethrow'(Goal, Exception) :-
    call_cleanup(Goal, throw(Exception)),
    !.


%!  setup_call_cleanup(:Setup, :Goal, :Cleanup).
%!  setup_call_catcher_cleanup(:Setup, :Goal, +Catcher, :Cleanup).
%!  call_cleanup(:Goal, :Cleanup).
%!  call_cleanup(:Goal, +Catcher, :Cleanup).
%
%   Call Cleanup once after Goal is finished (deterministic success,
%   failure, exception or  cut).  The   call  to  '$call_cleanup' is
%   translated to I_CALLCLEANUP. This  instruction   relies  on  the
%   exact stack layout left   by  setup_call_catcher_cleanup/4. Also
%   the predicate name is used by   the kernel cleanup mechanism and
%   can only be changed together with the kernel.

setup_call_catcher_cleanup(Setup, _Goal, _Catcher, _Cleanup) :-
    '$sig_atomic'(Setup),
    '$call_cleanup'.

setup_call_cleanup(Setup, Goal, Cleanup) :-
    setup_call_catcher_cleanup(Setup, Goal, _Catcher, Cleanup).

call_cleanup(Goal, Cleanup) :-
    setup_call_catcher_cleanup(true, Goal, _Catcher, Cleanup).

call_cleanup(Goal, Catcher, Cleanup) :-
    setup_call_catcher_cleanup(true, Goal, Catcher, Cleanup).

                 /*******************************
                 *       INITIALIZATION         *
                 *******************************/

:- meta_predicate
    initialization(0, +).

:- multifile '$init_goal'/3.
:- dynamic   '$init_goal'/3.

%!  initialization(:Goal, +When)
%
%   Register Goal to be executed if a saved state is restored. In
%   addition, the goal is executed depending on When:
%
%       * now
%       Execute immediately
%       * after_load
%       Execute after loading the file in which it appears.  This
%       is initialization/1.
%       * restore_state
%       Do not execute immediately, but only when restoring the
%       state.  Not allowed in a sandboxed environment.
%       * prepare_state
%       Called before saving a state.  Can be used to clean the
%       environment (see also volatile/1) or eagerly execute
%       goals that are normally executed lazily.
%       * program
%       Works as =|-g goal|= goals.
%       * main
%       Starts the application.  Only last declaration is used.
%
%   Note that all goals are executed when a program is restored.

initialization(Goal, When) :-
    '$must_be'(oneof(atom, initialization_type,
                     [ now,
                       after_load,
                       restore,
                       restore_state,
                       prepare_state,
                       program,
                       main
                     ]), When),
    '$initialization_context'(Source, Ctx),
    '$initialization'(When, Goal, Source, Ctx).

'$initialization'(now, Goal, _Source, Ctx) :-
    '$run_init_goal'(Goal, Ctx),
    '$compile_init_goal'(-, Goal, Ctx).
'$initialization'(after_load, Goal, Source, Ctx) :-
    (   Source \== (-)
    ->  '$compile_init_goal'(Source, Goal, Ctx)
    ;   throw(error(context_error(nodirective,
                                  initialization(Goal, after_load)),
                    _))
    ).
'$initialization'(restore, Goal, Source, Ctx) :- % deprecated
    '$initialization'(restore_state, Goal, Source, Ctx).
'$initialization'(restore_state, Goal, _Source, Ctx) :-
    (   \+ current_prolog_flag(sandboxed_load, true)
    ->  '$compile_init_goal'(-, Goal, Ctx)
    ;   '$permission_error'(register, initialization(restore), Goal)
    ).
'$initialization'(prepare_state, Goal, _Source, Ctx) :-
    (   \+ current_prolog_flag(sandboxed_load, true)
    ->  '$compile_init_goal'(when(prepare_state), Goal, Ctx)
    ;   '$permission_error'(register, initialization(restore), Goal)
    ).
'$initialization'(program, Goal, _Source, Ctx) :-
    (   \+ current_prolog_flag(sandboxed_load, true)
    ->  '$compile_init_goal'(when(program), Goal, Ctx)
    ;   '$permission_error'(register, initialization(restore), Goal)
    ).
'$initialization'(main, Goal, _Source, Ctx) :-
    (   \+ current_prolog_flag(sandboxed_load, true)
    ->  '$compile_init_goal'(when(main), Goal, Ctx)
    ;   '$permission_error'(register, initialization(restore), Goal)
    ).


'$compile_init_goal'(Source, Goal, Ctx) :-
    atom(Source),
    Source \== (-),
    !,
    '$store_admin_clause'(system:'$init_goal'(Source, Goal, Ctx),
                          _Layout, Source, Ctx).
'$compile_init_goal'(Source, Goal, Ctx) :-
    assertz('$init_goal'(Source, Goal, Ctx)).


%!  '$run_initialization'(?File, +Options) is det.
%!  '$run_initialization'(?File, +Action, +Options) is det.
%
%   Run initialization directives for all files  if File is unbound,
%   or for a specified file.   Note  that '$run_initialization'/2 is
%   called from runInitialization() in pl-wic.c  for .qlf files. The
%   '$run_initialization'/3 is called with Action   set  to `loaded`
%   when called for a QLF file.

'$run_initialization'(_, loaded, _) :- !.
'$run_initialization'(File, _Action, Options) :-
    '$run_initialization'(File, Options).

'$run_initialization'(File, Options) :-
    setup_call_cleanup(
        '$start_run_initialization'(Options, Restore),
        '$run_initialization_2'(File),
        '$end_run_initialization'(Restore)).

'$start_run_initialization'(Options, OldSandBoxed) :-
    '$push_input_context'(initialization),
    '$set_sandboxed_load'(Options, OldSandBoxed).
'$end_run_initialization'(OldSandBoxed) :-
    set_prolog_flag(sandboxed_load, OldSandBoxed),
    '$pop_input_context'.

'$run_initialization_2'(File) :-
    (   '$init_goal'(File, Goal, Ctx),
        File \= when(_),
        '$run_init_goal'(Goal, Ctx),
        fail
    ;   true
    ).

'$run_init_goal'(Goal, Ctx) :-
    (   catch_with_backtrace('$run_init_goal'(Goal), E,
                             '$initialization_error'(E, Goal, Ctx))
    ->  true
    ;   '$initialization_failure'(Goal, Ctx)
    ).

:- multifile prolog:sandbox_allowed_goal/1.

'$run_init_goal'(Goal) :-
    current_prolog_flag(sandboxed_load, false),
    !,
    call(Goal).
'$run_init_goal'(Goal) :-
    prolog:sandbox_allowed_goal(Goal),
    call(Goal).

'$initialization_context'(Source, Ctx) :-
    (   source_location(File, Line)
    ->  Ctx = File:Line,
        '$input_context'(Context),
        '$top_file'(Context, File, Source)
    ;   Ctx = (-),
        File = (-)
    ).

'$top_file'([input(include, F1, _, _)|T], _, F) :-
    !,
    '$top_file'(T, F1, F).
'$top_file'(_, F, F).


'$initialization_error'(E, Goal, Ctx) :-
    print_message(error, initialization_error(Goal, E, Ctx)).

'$initialization_failure'(Goal, Ctx) :-
    print_message(warning, initialization_failure(Goal, Ctx)).

%!  '$clear_source_admin'(+File) is det.
%
%   Removes source adminstration related to File
%
%   @see Called from destroySourceFile() in pl-proc.c

:- public '$clear_source_admin'/1.

'$clear_source_admin'(File) :-
    retractall('$init_goal'(_, _, File:_)),
    retractall('$load_context_module'(File, _, _)),
    retractall('$resolved_source_path'(_, File)).


                 /*******************************
                 *            STREAM            *
                 *******************************/

:- '$iso'(stream_property/2).
stream_property(Stream, Property) :-
    nonvar(Stream),
    nonvar(Property),
    !,
    '$stream_property'(Stream, Property).
stream_property(Stream, Property) :-
    nonvar(Stream),
    !,
    '$stream_properties'(Stream, Properties),
    '$member'(Property, Properties).
stream_property(Stream, Property) :-
    nonvar(Property),
    !,
    (   Property = alias(Alias),
        atom(Alias)
    ->  '$alias_stream'(Alias, Stream)
    ;   '$streams_properties'(Property, Pairs),
        '$member'(Stream-Property, Pairs)
    ).
stream_property(Stream, Property) :-
    '$streams_properties'(Property, Pairs),
    '$member'(Stream-Properties, Pairs),
    '$member'(Property, Properties).


                /********************************
                *            MODULES            *
                *********************************/

%       '$prefix_module'(+Module, +Context, +Term, -Prefixed)
%       Tags `Term' with `Module:' if `Module' is not the context module.

'$prefix_module'(Module, Module, Head, Head) :- !.
'$prefix_module'(Module, _, Head, Module:Head).

%!  default_module(+Me, -Super) is multi.
%
%   Is true if `Super' is `Me' or a super (auto import) module of `Me'.

default_module(Me, Super) :-
    (   atom(Me)
    ->  (   var(Super)
        ->  '$default_module'(Me, Super)
        ;   '$default_module'(Me, Super), !
        )
    ;   '$type_error'(module, Me)
    ).

'$default_module'(Me, Me).
'$default_module'(Me, Super) :-
    import_module(Me, S),
    '$default_module'(S, Super).


                /********************************
                *      TRACE AND EXCEPTIONS     *
                *********************************/

:- user:dynamic((exception/3,
                 prolog_event_hook/1)).
:- user:multifile((exception/3,
                   prolog_event_hook/1)).

%!  '$undefined_procedure'(+Module, +Name, +Arity, -Action) is det.
%
%   This predicate is called from C   on undefined predicates. First
%   allows the user to take care of   it using exception/3. Else try
%   to give a DWIM warning. Otherwise fail.   C  will print an error
%   message.

:- public
    '$undefined_procedure'/4.

'$undefined_procedure'(Module, Name, Arity, Action) :-
    '$prefix_module'(Module, user, Name/Arity, Pred),
    user:exception(undefined_predicate, Pred, Action0),
    !,
    Action = Action0.
'$undefined_procedure'(Module, Name, Arity, Action) :-
    current_prolog_flag(autoload, true),
    '$autoload'(Module, Name, Arity),
    !,
    Action = retry.
'$undefined_procedure'(_, _, _, error).

'$autoload'(Module, Name, Arity) :-
    source_location(File, _Line),
    !,
    setup_call_cleanup(
        '$start_aux'(File, Context),
        '$autoload2'(Module, Name, Arity),
        '$end_aux'(File, Context)).
'$autoload'(Module, Name, Arity) :-
    '$autoload2'(Module, Name, Arity).

'$autoload2'(Module, Name, Arity) :-
    '$find_library'(Module, Name, Arity, LoadModule, Library),
    functor(Head, Name, Arity),
    '$update_autoload_level'([autoload(true)], Old),
    (   current_prolog_flag(verbose_autoload, true)
    ->  Level = informational
    ;   Level = silent
    ),
    print_message(Level, autoload(Module:Name/Arity, Library)),
    '$compilation_mode'(OldComp, database),
    (   Module == LoadModule
    ->  ensure_loaded(Module:Library)
    ;   (   '$get_predicate_attribute'(LoadModule:Head, defined, 1),
            \+ '$loading'(Library)
        ->  Module:import(LoadModule:Name/Arity)
        ;   use_module(Module:Library, [Name/Arity])
        )
    ),
    '$set_compilation_mode'(OldComp),
    '$set_autoload_level'(Old),
    '$c_current_predicate'(_, Module:Head).

%!  '$loading'(+Library)
%
%   True if the library  is  being   loaded.  Just  testing that the
%   predicate is defined is not  good  enough   as  the  file may be
%   partly  loaded.  Calling  use_module/2  at   any  time  has  two
%   drawbacks: it queries the filesystem,   causing  slowdown and it
%   stops libraries being autoloaded from a   saved  state where the
%   library is already loaded, but the source may not be accessible.

'$loading'(Library) :-
    current_prolog_flag(threads, true),
    '$loading_file'(FullFile, _Queue, _LoadThread),
    file_name_extension(Library, _, FullFile),
    !.

%        handle debugger 'w', 'p' and <N> depth options.

'$set_debugger_write_options'(write) :-
    !,
    create_prolog_flag(debugger_write_options,
                       [ quoted(true),
                         attributes(dots),
                         spacing(next_argument)
                       ], []).
'$set_debugger_write_options'(print) :-
    !,
    create_prolog_flag(debugger_write_options,
                       [ quoted(true),
                         portray(true),
                         max_depth(10),
                         attributes(portray),
                         spacing(next_argument)
                       ], []).
'$set_debugger_write_options'(Depth) :-
    current_prolog_flag(debugger_write_options, Options0),
    (   '$select'(max_depth(_), Options0, Options)
    ->  true
    ;   Options = Options0
    ),
    create_prolog_flag(debugger_write_options,
                       [max_depth(Depth)|Options], []).


                /********************************
                *        SYSTEM MESSAGES        *
                *********************************/

%!  '$confirm'(Spec)
%
%   Ask the user to confirm a question.  Spec is a term as used for
%   print_message/2.

'$confirm'(Spec) :-
    print_message(query, Spec),
    between(0, 5, _),
        get_single_char(Answer),
        (   '$in_reply'(Answer, 'yYjJ \n')
        ->  !,
            print_message(query, if_tty([yes-[]]))
        ;   '$in_reply'(Answer, 'nN')
        ->  !,
            print_message(query, if_tty([no-[]])),
            fail
        ;   print_message(help, query(confirm)),
            fail
        ).

'$in_reply'(Code, Atom) :-
    char_code(Char, Code),
    sub_atom(Atom, _, _, _, Char),
    !.

:- dynamic
    user:portray/1.
:- multifile
    user:portray/1.


                 /*******************************
                 *       FILE_SEARCH_PATH       *
                 *******************************/

:- dynamic user:file_search_path/2.
:- multifile user:file_search_path/2.

user:(file_search_path(library, Dir) :-
        library_directory(Dir)).
user:file_search_path(swi, Home) :-
    current_prolog_flag(home, Home).
user:file_search_path(foreign, swi(ArchLib)) :-
    current_prolog_flag(arch, Arch),
    atom_concat('lib/', Arch, ArchLib).
user:file_search_path(foreign, swi(SoLib)) :-
    (   current_prolog_flag(windows, true)
    ->  SoLib = bin
    ;   SoLib = lib
    ).
user:file_search_path(path, Dir) :-
    getenv('PATH', Path),
    (   current_prolog_flag(windows, true)
    ->  atomic_list_concat(Dirs, (;), Path)
    ;   atomic_list_concat(Dirs, :, Path)
    ),
    '$member'(Dir, Dirs),
    '$no-null-bytes'(Dir).

'$no-null-bytes'(Dir) :-
    sub_atom(Dir, _, _, _, '\u0000'),
    !,
    print_message(warning, null_byte_in_path(Dir)),
    fail.
'$no-null-bytes'(_).

%!  expand_file_search_path(+Spec, -Expanded) is nondet.
%
%   Expand a search path.  The system uses depth-first search upto a
%   specified depth.  If this depth is exceeded an exception is raised.
%   TBD: bread-first search?

expand_file_search_path(Spec, Expanded) :-
    catch('$expand_file_search_path'(Spec, Expanded, 0, []),
          loop(Used),
          throw(error(loop_error(Spec), file_search(Used)))).

'$expand_file_search_path'(Spec, Expanded, N, Used) :-
    functor(Spec, Alias, 1),
    !,
    user:file_search_path(Alias, Exp0),
    NN is N + 1,
    (   NN > 16
    ->  throw(loop(Used))
    ;   true
    ),
    '$expand_file_search_path'(Exp0, Exp1, NN, [Alias=Exp0|Used]),
    arg(1, Spec, Segments),
    '$segments_to_atom'(Segments, File),
    '$make_path'(Exp1, File, Expanded).
'$expand_file_search_path'(Spec, Path, _, _) :-
    '$segments_to_atom'(Spec, Path).

'$make_path'(Dir, '.', Path) :-
    !,
    Path = Dir.
'$make_path'(Dir, File, Path) :-
    sub_atom(Dir, _, _, 0, /),
    !,
    atom_concat(Dir, File, Path).
'$make_path'(Dir, File, Path) :-
    atomic_list_concat([Dir, /, File], Path).


                /********************************
                *         FILE CHECKING         *
                *********************************/

%!  absolute_file_name(+Term, -AbsoluteFile, +Options) is nondet.
%
%   Translate path-specifier into a full   path-name. This predicate
%   originates from Quintus was introduced  in SWI-Prolog very early
%   and  has  re-appeared  in  SICStus  3.9.0,  where  they  changed
%   argument order and added some options.   We addopted the SICStus
%   argument order, but still accept the original argument order for
%   compatibility reasons.

absolute_file_name(Spec, Options, Path) :-
    '$is_options'(Options),
    \+ '$is_options'(Path),
    !,
    absolute_file_name(Spec, Path, Options).
absolute_file_name(Spec, Path, Options) :-
    '$must_be'(options, Options),
                    % get the valid extensions
    (   '$select_option'(extensions(Exts), Options, Options1)
    ->  '$must_be'(list, Exts)
    ;   '$option'(file_type(Type), Options)
    ->  '$must_be'(atom, Type),
        '$file_type_extensions'(Type, Exts),
        Options1 = Options
    ;   Options1 = Options,
        Exts = ['']
    ),
    '$canonicalise_extensions'(Exts, Extensions),
                    % unless specified otherwise, ask regular file
    (   nonvar(Type)
    ->  Options2 = Options1
    ;   '$merge_options'(_{file_type:regular}, Options1, Options2)
    ),
                    % Det or nondet?
    (   '$select_option'(solutions(Sols), Options2, Options3)
    ->  '$must_be'(oneof(atom, solutions, [first,all]), Sols)
    ;   Sols = first,
        Options3 = Options2
    ),
                    % Errors or not?
    (   '$select_option'(file_errors(FileErrors), Options3, Options4)
    ->  '$must_be'(oneof(atom, file_errors, [error,fail]), FileErrors)
    ;   FileErrors = error,
        Options4 = Options3
    ),
                    % Expand shell patterns?
    (   atomic(Spec),
        '$select_option'(expand(Expand), Options4, Options5),
        '$must_be'(boolean, Expand)
    ->  expand_file_name(Spec, List),
        '$member'(Spec1, List)
    ;   Spec1 = Spec,
        Options5 = Options4
    ),
                    % Search for files
    (   Sols == first
    ->  (   '$chk_file'(Spec1, Extensions, Options5, true, Path)
        ->  !       % also kill choice point of expand_file_name/2
        ;   (   FileErrors == fail
            ->  fail
            ;   '$current_module'('$bags', _File),
                findall(P,
                        '$chk_file'(Spec1, Extensions, [access(exist)],
                                    false, P),
                        Candidates),
                '$abs_file_error'(Spec, Candidates, Options5)
            )
        )
    ;   '$chk_file'(Spec1, Extensions, Options5, false, Path)
    ).

'$abs_file_error'(Spec, Candidates, Conditions) :-
    '$member'(F, Candidates),
    '$member'(C, Conditions),
    '$file_condition'(C),
    '$file_error'(C, Spec, F, E, Comment),
    !,
    throw(error(E, context(_, Comment))).
'$abs_file_error'(Spec, _, _) :-
    '$existence_error'(source_sink, Spec).

'$file_error'(file_type(directory), Spec, File, Error, Comment) :-
    \+ exists_directory(File),
    !,
    Error = existence_error(directory, Spec),
    Comment = not_a_directory(File).
'$file_error'(file_type(_), Spec, File, Error, Comment) :-
    exists_directory(File),
    !,
    Error = existence_error(file, Spec),
    Comment = directory(File).
'$file_error'(access(OneOrList), Spec, File, Error, _) :-
    '$one_or_member'(Access, OneOrList),
    \+ access_file(File, Access),
    Error = permission_error(Access, source_sink, Spec).

'$one_or_member'(Elem, List) :-
    is_list(List),
    !,
    '$member'(Elem, List).
'$one_or_member'(Elem, Elem).


'$file_type_extensions'(source, Exts) :-       % SICStus 3.9 compatibility
    !,
    '$file_type_extensions'(prolog, Exts).
'$file_type_extensions'(Type, Exts) :-
    '$current_module'('$bags', _File),
    !,
    findall(Ext, user:prolog_file_type(Ext, Type), Exts0),
    (   Exts0 == [],
        \+ '$ft_no_ext'(Type)
    ->  '$domain_error'(file_type, Type)
    ;   true
    ),
    '$append'(Exts0, [''], Exts).
'$file_type_extensions'(prolog, [pl, '']). % findall is not yet defined ...

'$ft_no_ext'(txt).
'$ft_no_ext'(executable).
'$ft_no_ext'(directory).

%!  user:prolog_file_type(?Extension, ?Type)
%
%   Define type of file based on the extension.  This is used by
%   absolute_file_name/3 and may be used to extend the list of
%   extensions used for some type.
%
%   Note that =qlf= must be last   when  searching for Prolog files.
%   Otherwise use_module/1 will consider  the   file  as  not-loaded
%   because the .qlf file is not  the   loaded  file.  Must be fixed
%   elsewhere.

:- multifile(user:prolog_file_type/2).
:- dynamic(user:prolog_file_type/2).

user:prolog_file_type(pl,       prolog).
user:prolog_file_type(prolog,   prolog).
user:prolog_file_type(qlf,      prolog).
user:prolog_file_type(qlf,      qlf).
user:prolog_file_type(Ext,      executable) :-
    current_prolog_flag(shared_object_extension, Ext).
user:prolog_file_type(dylib,    executable) :-
    current_prolog_flag(apple,  true).

%!  '$chk_file'(+Spec, +Extensions, +Cond, +UseCache, -FullName)
%
%   File is a specification of a Prolog source file. Return the full
%   path of the file.

'$chk_file'(Spec, _Extensions, _Cond, _Cache, _FullName) :-
    \+ ground(Spec),
    !,
    '$instantiation_error'(Spec).
'$chk_file'(Spec, Extensions, Cond, Cache, FullName) :-
    compound(Spec),
    functor(Spec, _, 1),
    !,
    '$relative_to'(Cond, cwd, CWD),
    '$chk_alias_file'(Spec, Extensions, Cond, Cache, CWD, FullName).
'$chk_file'(Segments, Ext, Cond, Cache, FullName) :-    % allow a/b/...
    \+ atomic(Segments),
    !,
    '$segments_to_atom'(Segments, Atom),
    '$chk_file'(Atom, Ext, Cond, Cache, FullName).
'$chk_file'(File, Exts, Cond, _, FullName) :-
    is_absolute_file_name(File),
    !,
    '$extend_file'(File, Exts, Extended),
    '$file_conditions'(Cond, Extended),
    '$absolute_file_name'(Extended, FullName).
'$chk_file'(File, Exts, Cond, _, FullName) :-
    '$relative_to'(Cond, source, Dir),
    atomic_list_concat([Dir, /, File], AbsFile),
    '$extend_file'(AbsFile, Exts, Extended),
    '$file_conditions'(Cond, Extended),
    !,
    '$absolute_file_name'(Extended, FullName).
'$chk_file'(File, Exts, Cond, _, FullName) :-
    '$extend_file'(File, Exts, Extended),
    '$file_conditions'(Cond, Extended),
    '$absolute_file_name'(Extended, FullName).

'$segments_to_atom'(Atom, Atom) :-
    atomic(Atom),
    !.
'$segments_to_atom'(Segments, Atom) :-
    '$segments_to_list'(Segments, List, []),
    !,
    atomic_list_concat(List, /, Atom).

'$segments_to_list'(A/B, H, T) :-
    '$segments_to_list'(A, H, T0),
    '$segments_to_list'(B, T0, T).
'$segments_to_list'(A, [A|T], T) :-
    atomic(A).


%!  '$relative_to'(+Condition, +Default, -Dir)
%
%   Determine the directory to work from.  This can be specified
%   explicitely using one or more relative_to(FileOrDir) options
%   or implicitely relative to the working directory or current
%   source-file.

'$relative_to'(Conditions, Default, Dir) :-
    (   '$option'(relative_to(FileOrDir), Conditions)
    *-> (   exists_directory(FileOrDir)
        ->  Dir = FileOrDir
        ;   atom_concat(Dir, /, FileOrDir)
        ->  true
        ;   file_directory_name(FileOrDir, Dir)
        )
    ;   Default == cwd
    ->  '$cwd'(Dir)
    ;   Default == source
    ->  source_location(ContextFile, _Line),
        file_directory_name(ContextFile, Dir)
    ).

%!  '$chk_alias_file'(+Spec, +Exts, +Cond, +Cache, +CWD,
%!                    -FullFile) is nondet.

:- dynamic
    '$search_path_file_cache'/3,    % SHA1, Time, Path
    '$search_path_gc_time'/1.       % Time
:- volatile
    '$search_path_file_cache'/3,
    '$search_path_gc_time'/1.

:- create_prolog_flag(file_search_cache_time, 10, []).

'$chk_alias_file'(Spec, Exts, Cond, true, CWD, FullFile) :-
    !,
    findall(Exp, expand_file_search_path(Spec, Exp), Expansions),
    Cache = cache(Exts, Cond, CWD, Expansions),
    variant_sha1(Spec+Cache, SHA1),
    get_time(Now),
    current_prolog_flag(file_search_cache_time, TimeOut),
    (   '$search_path_file_cache'(SHA1, CachedTime, FullFile),
        CachedTime > Now - TimeOut,
        '$file_conditions'(Cond, FullFile)
    ->  '$search_message'(file_search(cache(Spec, Cond), FullFile))
    ;   '$member'(Expanded, Expansions),
        '$extend_file'(Expanded, Exts, LibFile),
        (   '$file_conditions'(Cond, LibFile),
            '$absolute_file_name'(LibFile, FullFile),
            '$cache_file_found'(SHA1, Now, TimeOut, FullFile)
        ->  '$search_message'(file_search(found(Spec, Cond), FullFile))
        ;   '$search_message'(file_search(tried(Spec, Cond), LibFile)),
            fail
        )
    ).
'$chk_alias_file'(Spec, Exts, Cond, false, _CWD, FullFile) :-
    expand_file_search_path(Spec, Expanded),
    '$extend_file'(Expanded, Exts, LibFile),
    '$file_conditions'(Cond, LibFile),
    '$absolute_file_name'(LibFile, FullFile).

'$cache_file_found'(_, _, TimeOut, _) :-
    TimeOut =:= 0,
    !.
'$cache_file_found'(SHA1, Now, TimeOut, FullFile) :-
    '$search_path_file_cache'(SHA1, Saved, FullFile),
    !,
    (   Now - Saved < TimeOut/2
    ->  true
    ;   retractall('$search_path_file_cache'(SHA1, _, _)),
        asserta('$search_path_file_cache'(SHA1, Now, FullFile))
    ).
'$cache_file_found'(SHA1, Now, TimeOut, FullFile) :-
    'gc_file_search_cache'(TimeOut),
    asserta('$search_path_file_cache'(SHA1, Now, FullFile)).

'gc_file_search_cache'(TimeOut) :-
    get_time(Now),
    '$search_path_gc_time'(Last),
    Now-Last < TimeOut/2,
    !.
'gc_file_search_cache'(TimeOut) :-
    get_time(Now),
    retractall('$search_path_gc_time'(_)),
    assertz('$search_path_gc_time'(Now)),
    Before is Now - TimeOut,
    (   '$search_path_file_cache'(SHA1, Cached, FullFile),
        Cached < Before,
        retractall('$search_path_file_cache'(SHA1, Cached, FullFile)),
        fail
    ;   true
    ).


'$search_message'(Term) :-
    current_prolog_flag(verbose_file_search, true),
    !,
    print_message(informational, Term).
'$search_message'(_).


%!  '$file_conditions'(+Condition, +Path)
%
%   Verify Path satisfies Condition.

'$file_conditions'(List, File) :-
    is_list(List),
    !,
    \+ ( '$member'(C, List),
         '$file_condition'(C),
         \+ '$file_condition'(C, File)
       ).
'$file_conditions'(Map, File) :-
    \+ (  get_dict(Key, Map, Value),
          C =.. [Key,Value],
          '$file_condition'(C),
         \+ '$file_condition'(C, File)
       ).

'$file_condition'(file_type(directory), File) :-
    !,
    exists_directory(File).
'$file_condition'(file_type(_), File) :-
    !,
    \+ exists_directory(File).
'$file_condition'(access(Accesses), File) :-
    !,
    \+ (  '$one_or_member'(Access, Accesses),
          \+ access_file(File, Access)
       ).

'$file_condition'(exists).
'$file_condition'(file_type(_)).
'$file_condition'(access(_)).

'$extend_file'(File, Exts, FileEx) :-
    '$ensure_extensions'(Exts, File, Fs),
    '$list_to_set'(Fs, FsSet),
    '$member'(FileEx, FsSet).

'$ensure_extensions'([], _, []).
'$ensure_extensions'([E|E0], F, [FE|E1]) :-
    file_name_extension(F, E, FE),
    '$ensure_extensions'(E0, F, E1).

%!  '$list_to_set'(+List, -Set) is det.
%
%   Turn list into a set, keeping   the  left-most copy of duplicate
%   elements.  Note  that  library(lists)  provides  an  O(N*log(N))
%   version, but sets of file name extensions should be short enough
%   for this not to matter.

'$list_to_set'(List, Set) :-
    '$list_to_set'(List, [], Set).

'$list_to_set'([], _, []).
'$list_to_set'([H|T], Seen, R) :-
    memberchk(H, Seen),
    !,
    '$list_to_set'(T, R).
'$list_to_set'([H|T], Seen, [H|R]) :-
    '$list_to_set'(T, [H|Seen], R).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Canonicalise the extension list. Old SWI-Prolog   require  `.pl', etc, which
the Quintus compatibility  requests  `pl'.   This  layer  canonicalises  all
extensions to .ext
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

'$canonicalise_extensions'([], []) :- !.
'$canonicalise_extensions'([H|T], [CH|CT]) :-
    !,
    '$must_be'(atom, H),
    '$canonicalise_extension'(H, CH),
    '$canonicalise_extensions'(T, CT).
'$canonicalise_extensions'(E, [CE]) :-
    '$canonicalise_extension'(E, CE).

'$canonicalise_extension'('', '') :- !.
'$canonicalise_extension'(DotAtom, DotAtom) :-
    sub_atom(DotAtom, 0, _, _, '.'),
    !.
'$canonicalise_extension'(Atom, DotAtom) :-
    atom_concat('.', Atom, DotAtom).


                /********************************
                *            CONSULT            *
                *********************************/

:- dynamic
    user:library_directory/1,
    user:prolog_load_file/2.
:- multifile
    user:library_directory/1,
    user:prolog_load_file/2.

:- prompt(_, '|: ').

:- thread_local
    '$compilation_mode_store'/1,    % database, wic, qlf
    '$directive_mode_store'/1.      % database, wic, qlf
:- volatile
    '$compilation_mode_store'/1,
    '$directive_mode_store'/1.

'$compilation_mode'(Mode) :-
    (   '$compilation_mode_store'(Val)
    ->  Mode = Val
    ;   Mode = database
    ).

'$set_compilation_mode'(Mode) :-
    retractall('$compilation_mode_store'(_)),
    assertz('$compilation_mode_store'(Mode)).

'$compilation_mode'(Old, New) :-
    '$compilation_mode'(Old),
    (   New == Old
    ->  true
    ;   '$set_compilation_mode'(New)
    ).

'$directive_mode'(Mode) :-
    (   '$directive_mode_store'(Val)
    ->  Mode = Val
    ;   Mode = database
    ).

'$directive_mode'(Old, New) :-
    '$directive_mode'(Old),
    (   New == Old
    ->  true
    ;   '$set_directive_mode'(New)
    ).

'$set_directive_mode'(Mode) :-
    retractall('$directive_mode_store'(_)),
    assertz('$directive_mode_store'(Mode)).


%!  '$compilation_level'(-Level) is det.
%
%   True when Level reflects the nesting   in  files compiling other
%   files. 0 if no files are being loaded.

'$compilation_level'(Level) :-
    '$input_context'(Stack),
    '$compilation_level'(Stack, Level).

'$compilation_level'([], 0).
'$compilation_level'([Input|T], Level) :-
    (   arg(1, Input, see)
    ->  '$compilation_level'(T, Level)
    ;   '$compilation_level'(T, Level0),
        Level is Level0+1
    ).


%!  compiling
%
%   Is true if SWI-Prolog is generating a state or qlf file or
%   executes a `call' directive while doing this.

compiling :-
    \+ (   '$compilation_mode'(database),
           '$directive_mode'(database)
       ).

:- meta_predicate
    '$ifcompiling'(0).

'$ifcompiling'(G) :-
    (   '$compilation_mode'(database)
    ->  true
    ;   call(G)
    ).

                /********************************
                *         READ SOURCE           *
                *********************************/

%!  '$load_msg_level'(+Action, +NestingLevel, -StartVerbose, -EndVerbose)

'$load_msg_level'(Action, Nesting, Start, Done) :-
    '$update_autoload_level'([], 0),
    !,
    current_prolog_flag(verbose_load, Type0),
    '$load_msg_compat'(Type0, Type),
    (   '$load_msg_level'(Action, Nesting, Type, Start, Done)
    ->  true
    ).
'$load_msg_level'(_, _, silent, silent).

'$load_msg_compat'(true, normal) :- !.
'$load_msg_compat'(false, silent) :- !.
'$load_msg_compat'(X, X).

'$load_msg_level'(load_file,    _, full,   informational, informational).
'$load_msg_level'(include_file, _, full,   informational, informational).
'$load_msg_level'(load_file,    _, normal, silent,        informational).
'$load_msg_level'(include_file, _, normal, silent,        silent).
'$load_msg_level'(load_file,    0, brief,  silent,        informational).
'$load_msg_level'(load_file,    _, brief,  silent,        silent).
'$load_msg_level'(include_file, _, brief,  silent,        silent).
'$load_msg_level'(load_file,    _, silent, silent,        silent).
'$load_msg_level'(include_file, _, silent, silent,        silent).

%!  '$source_term'(+From, -Read, -RLayout, -Term, -TLayout,
%!                 -Stream, +Options) is nondet.
%
%   Read Prolog terms from the  input   From.  Terms are returned on
%   backtracking. Associated resources (i.e.,   streams)  are closed
%   due to setup_call_cleanup/3.
%
%   @param From is either a term stream(Id, Stream) or a file
%          specification.
%   @param Read is the raw term as read from the input.
%   @param Term is the term after term-expansion.  If a term is
%          expanded into the empty list, this is returned too.  This
%          is required to be able to return the raw term in Read
%   @param Stream is the stream from which Read is read
%   @param Options provides additional options:
%           * encoding(Enc)
%           Encoding used to open From
%           * syntax_errors(+ErrorMode)
%           * process_comments(+Boolean)
%           * term_position(-Pos)

'$source_term'(From, Read, RLayout, Term, TLayout, Stream, Options) :-
    '$source_term'(From, Read, RLayout, Term, TLayout, Stream, [], Options),
    (   Term == end_of_file
    ->  !, fail
    ;   true
    ).

'$source_term'(Input, _,_,_,_,_,_,_) :-
    \+ ground(Input),
    !,
    '$instantiation_error'(Input).
'$source_term'(stream(Id, In, Opts),
               Read, RLayout, Term, TLayout, Stream, Parents, Options) :-
    !,
    '$record_included'(Parents, Id, Id, 0.0, Message),
    setup_call_cleanup(
        '$open_source'(stream(Id, In, Opts), In, State, Parents, Options),
        '$term_in_file'(In, Read, RLayout, Term, TLayout, Stream,
                        [Id|Parents], Options),
        '$close_source'(State, Message)).
'$source_term'(File,
               Read, RLayout, Term, TLayout, Stream, Parents, Options) :-
    absolute_file_name(File, Path,
                       [ file_type(prolog),
                         access(read)
                       ]),
    time_file(Path, Time),
    '$record_included'(Parents, File, Path, Time, Message),
    setup_call_cleanup(
        '$open_source'(Path, In, State, Parents, Options),
        '$term_in_file'(In, Read, RLayout, Term, TLayout, Stream,
                        [Path|Parents], Options),
        '$close_source'(State, Message)).

:- thread_local
    '$load_input'/2.
:- volatile
    '$load_input'/2.

'$open_source'(stream(Id, In, Opts), In,
               restore(In, StreamState, Id, Ref, Opts), Parents, Options) :-
    !,
    '$context_type'(Parents, ContextType),
    '$push_input_context'(ContextType),
    '$set_encoding'(In, Options),
    '$prepare_load_stream'(In, Id, StreamState),
    asserta('$load_input'(stream(Id), In), Ref).
'$open_source'(Path, In, close(In, Path, Ref), Parents, Options) :-
    '$context_type'(Parents, ContextType),
    '$push_input_context'(ContextType),
    open(Path, read, In),
    '$set_encoding'(In, Options),
    asserta('$load_input'(Path, In), Ref).

'$context_type'([], load_file) :- !.
'$context_type'(_, include).

'$close_source'(close(In, Id, Ref), Message) :-
    erase(Ref),
    '$end_consult'(Id),
    call_cleanup(
        close(In),
        '$pop_input_context'),
    '$close_message'(Message).
'$close_source'(restore(In, StreamState, Id, Ref, Opts), Message) :-
    erase(Ref),
    '$end_consult'(Id),
    call_cleanup(
        '$restore_load_stream'(In, StreamState, Opts),
        '$pop_input_context'),
    '$close_message'(Message).

'$close_message'(message(Level, Msg)) :-
    !,
    '$print_message'(Level, Msg).
'$close_message'(_).


%!  '$term_in_file'(+In, -Read, -RLayout, -Term, -TLayout,
%!                  -Stream, +Parents, +Options) is multi.
%
%   True when Term is an expanded term from   In. Read is a raw term
%   (before term-expansion). Stream is  the   actual  stream,  which
%   starts at In, but may change due to processing included files.
%
%   @see '$source_term'/8 for details.

'$term_in_file'(In, Read, RLayout, Term, TLayout, Stream, Parents, Options) :-
    '$skip_script_line'(In, Options),
    '$read_clause_options'(Options, ReadOptions),
    repeat,
      read_clause(In, Raw,
                  [ variable_names(Bindings),
                    term_position(Pos),
                    subterm_positions(RawLayout)
                  | ReadOptions
                  ]),
      b_setval('$term_position', Pos),
      b_setval('$variable_names', Bindings),
      (   Raw == end_of_file
      ->  !,
          (   Parents = [_,_|_]     % Included file
          ->  fail
          ;   '$expanded_term'(In,
                               Raw, RawLayout, Read, RLayout, Term, TLayout,
                               Stream, Parents, Options)
          )
      ;   '$expanded_term'(In, Raw, RawLayout, Read, RLayout, Term, TLayout,
                           Stream, Parents, Options)
      ).

'$read_clause_options'([], []).
'$read_clause_options'([H|T0], List) :-
    (   '$read_clause_option'(H)
    ->  List = [H|T]
    ;   List = T
    ),
    '$read_clause_options'(T0, T).

'$read_clause_option'(syntax_errors(_)).
'$read_clause_option'(term_position(_)).
'$read_clause_option'(process_comment(_)).

'$expanded_term'(In, Raw, RawLayout, Read, RLayout, Term, TLayout,
                 Stream, Parents, Options) :-
    E = error(_,_),
    catch('$expand_term'(Raw, RawLayout, Expanded, ExpandedLayout), E,
          '$print_message_fail'(E)),
    (   Expanded \== []
    ->  '$expansion_member'(Expanded, ExpandedLayout, Term1, Layout1)
    ;   Term1 = Expanded,
        Layout1 = ExpandedLayout
    ),
    (   nonvar(Term1), Term1 = (:-Directive), nonvar(Directive)
    ->  (   Directive = include(File),
            '$current_source_module'(Module),
            '$valid_directive'(Module:include(File))
        ->  stream_property(In, encoding(Enc)),
            '$add_encoding'(Enc, Options, Options1),
            '$source_term'(File, Read, RLayout, Term, TLayout,
                           Stream, Parents, Options1)
        ;   Directive = encoding(Enc)
        ->  set_stream(In, encoding(Enc)),
            fail
        ;   Term = Term1,
            Stream = In,
            Read = Raw
        )
    ;   Term = Term1,
        TLayout = Layout1,
        Stream = In,
        Read = Raw,
        RLayout = RawLayout
    ).

'$expansion_member'(Var, Layout, Var, Layout) :-
    var(Var),
    !.
'$expansion_member'([], _, _, _) :- !, fail.
'$expansion_member'(List, ListLayout, Term, Layout) :-
    is_list(List),
    !,
    (   var(ListLayout)
    ->  '$member'(Term, List)
    ;   is_list(ListLayout)
    ->  '$member_rep2'(Term, Layout, List, ListLayout)
    ;   Layout = ListLayout,
        '$member'(Term, List)
    ).
'$expansion_member'(X, Layout, X, Layout).

% pairwise member, repeating last element of the second
% list.

'$member_rep2'(H1, H2, [H1|_], [H2|_]).
'$member_rep2'(H1, H2, [_|T1], [T2]) :-
    !,
    '$member_rep2'(H1, H2, T1, [T2]).
'$member_rep2'(H1, H2, [_|T1], [_|T2]) :-
    '$member_rep2'(H1, H2, T1, T2).

%!  '$add_encoding'(+Enc, +Options0, -Options)

'$add_encoding'(Enc, Options0, Options) :-
    (   Options0 = [encoding(Enc)|_]
    ->  Options = Options0
    ;   Options = [encoding(Enc)|Options0]
    ).


:- multifile
    '$included'/4.                  % Into, Line, File, LastModified
:- dynamic
    '$included'/4.

%!  '$record_included'(+Parents, +File, +Path, +Time, -Message) is det.
%
%   Record that we included File into the   head of Parents. This is
%   troublesome when creating a QLF  file   because  this may happen
%   before we opened the QLF file (and  we   do  not yet know how to
%   open the file because we  do  not   yet  know  whether this is a
%   module file or not).
%
%   I think that the only sensible  solution   is  to have a special
%   statement for this, that may appear  both inside and outside QLF
%   `parts'.

'$record_included'([Parent|Parents], File, Path, Time,
                   message(DoneMsgLevel,
                           include_file(done(Level, file(File, Path))))) :-
    source_location(SrcFile, Line),
    !,
    '$compilation_level'(Level),
    '$load_msg_level'(include_file, Level, StartMsgLevel, DoneMsgLevel),
    '$print_message'(StartMsgLevel,
                     include_file(start(Level,
                                        file(File, Path)))),
    '$last'([Parent|Parents], Owner),
    (   (   '$compilation_mode'(database)
        ;   '$qlf_current_source'(Owner)
        )
    ->  '$store_admin_clause'(
            system:'$included'(Parent, Line, Path, Time),
            _, Owner, SrcFile:Line)
    ;   '$qlf_include'(Owner, Parent, Line, Path, Time)
    ).
'$record_included'(_, _, _, _, true).

%!  '$master_file'(+File, -MasterFile)
%
%   Find the primary load file from included files.

'$master_file'(File, MasterFile) :-
    '$included'(MasterFile0, _Line, File, _Time),
    !,
    '$master_file'(MasterFile0, MasterFile).
'$master_file'(File, File).


'$skip_script_line'(_In, Options) :-
    '$option'(check_script(false), Options),
    !.
'$skip_script_line'(In, _Options) :-
    (   peek_char(In, #)
    ->  skip(In, 10)
    ;   true
    ).

'$set_encoding'(Stream, Options) :-
    '$option'(encoding(Enc), Options),
    !,
    Enc \== default,
    set_stream(Stream, encoding(Enc)).
'$set_encoding'(_, _).


'$prepare_load_stream'(In, Id, state(HasName,HasPos)) :-
    (   stream_property(In, file_name(_))
    ->  HasName = true,
        (   stream_property(In, position(_))
        ->  HasPos = true
        ;   HasPos = false,
            set_stream(In, record_position(true))
        )
    ;   HasName = false,
        set_stream(In, file_name(Id)),
        (   stream_property(In, position(_))
        ->  HasPos = true
        ;   HasPos = false,
            set_stream(In, record_position(true))
        )
    ).

'$restore_load_stream'(In, _State, Options) :-
    memberchk(close(true), Options),
    !,
    close(In).
'$restore_load_stream'(In, state(HasName, HasPos), _Options) :-
    (   HasName == false
    ->  set_stream(In, file_name(''))
    ;   true
    ),
    (   HasPos == false
    ->  set_stream(In, record_position(false))
    ;   true
    ).


                 /*******************************
                 *          DERIVED FILES       *
                 *******************************/

:- dynamic
    '$derived_source_db'/3.         % Loaded, DerivedFrom, Time

'$register_derived_source'(_, '-') :- !.
'$register_derived_source'(Loaded, DerivedFrom) :-
    retractall('$derived_source_db'(Loaded, _, _)),
    time_file(DerivedFrom, Time),
    assert('$derived_source_db'(Loaded, DerivedFrom, Time)).

%       Auto-importing dynamic predicates is not very elegant and
%       leads to problems with qsave_program/[1,2]

'$derived_source'(Loaded, DerivedFrom, Time) :-
    '$derived_source_db'(Loaded, DerivedFrom, Time).


                /********************************
                *       LOAD PREDICATES         *
                *********************************/

:- meta_predicate
    ensure_loaded(:),
    [:|+],
    consult(:),
    use_module(:),
    use_module(:, +),
    reexport(:),
    reexport(:, +),
    load_files(:),
    load_files(:, +).

%!  ensure_loaded(+FileOrListOfFiles)
%
%   Load specified files, provided they where not loaded before. If the
%   file is a module file import the public predicates into the context
%   module.

ensure_loaded(Files) :-
    load_files(Files, [if(not_loaded)]).

%!  use_module(+FileOrListOfFiles)
%
%   Very similar to ensure_loaded/1, but insists on the loaded file to
%   be a module file. If the file is already imported, but the public
%   predicates are not yet imported into the context module, then do
%   so.

use_module(Files) :-
    load_files(Files, [ if(not_loaded),
                        must_be_module(true)
                      ]).

%!  use_module(+File, +ImportList)
%
%   As use_module/1, but takes only one file argument and imports only
%   the specified predicates rather than all public predicates.

use_module(File, Import) :-
    load_files(File, [ if(not_loaded),
                       must_be_module(true),
                       imports(Import)
                     ]).

%!  reexport(+Files)
%
%   As use_module/1, exporting all imported predicates.

reexport(Files) :-
    load_files(Files, [ if(not_loaded),
                        must_be_module(true),
                        reexport(true)
                      ]).

%!  reexport(+File, +ImportList)
%
%   As use_module/1, re-exporting all imported predicates.

reexport(File, Import) :-
    load_files(File, [ if(not_loaded),
                       must_be_module(true),
                       imports(Import),
                       reexport(true)
                     ]).


[X] :-
    !,
    consult(X).
[M:F|R] :-
    consult(M:[F|R]).

consult(M:X) :-
    X == user,
    !,
    flag('$user_consult', N, N+1),
    NN is N + 1,
    atom_concat('user://', NN, Id),
    load_files(M:Id, [stream(user_input), check_script(false), silent(false)]).
consult(List) :-
    load_files(List, [expand(true)]).

%!  load_files(:File, +Options)
%
%   Common entry for all the consult derivates.  File is the raw user
%   specified file specification, possibly tagged with the module.

load_files(Files) :-
    load_files(Files, []).
load_files(Module:Files, Options) :-
    '$must_be'(list, Options),
    '$load_files'(Files, Module, Options).

'$load_files'(X, _, _) :-
    var(X),
    !,
    '$instantiation_error'(X).
'$load_files'([], _, _) :- !.
'$load_files'(Id, Module, Options) :-   % load_files(foo, [stream(In)])
    '$option'(stream(_), Options),
    !,
    (   atom(Id)
    ->  '$load_file'(Id, Module, Options)
    ;   throw(error(type_error(atom, Id), _))
    ).
'$load_files'(List, Module, Options) :-
    List = [_|_],
    !,
    '$must_be'(list, List),
    '$load_file_list'(List, Module, Options).
'$load_files'(File, Module, Options) :-
    '$load_one_file'(File, Module, Options).

'$load_file_list'([], _, _).
'$load_file_list'([File|Rest], Module, Options) :-
    E = error(_,_),
    catch('$load_one_file'(File, Module, Options), E,
          '$print_message'(error, E)),
    '$load_file_list'(Rest, Module, Options).


'$load_one_file'(Spec, Module, Options) :-
    atomic(Spec),
    '$option'(expand(Expand), Options, false),
    Expand == true,
    !,
    expand_file_name(Spec, Expanded),
    (   Expanded = [Load]
    ->  true
    ;   Load = Expanded
    ),
    '$load_files'(Load, Module, [expand(false)|Options]).
'$load_one_file'(File, Module, Options) :-
    strip_module(Module:File, Into, PlainFile),
    '$load_file'(PlainFile, Into, Options).


%!  '$noload'(+Condition, +FullFile, +Options) is semidet.
%
%   True of FullFile should _not_ be loaded.

'$noload'(true, _, _) :-
    !,
    fail.
'$noload'(not_loaded, FullFile, _) :-
    source_file(FullFile),
    !.
'$noload'(changed, Derived, _) :-
    '$derived_source'(_FullFile, Derived, LoadTime),
    time_file(Derived, Modified),
    Modified @=< LoadTime,
    !.
'$noload'(changed, FullFile, Options) :-
    '$time_source_file'(FullFile, LoadTime, user),
    '$modified_id'(FullFile, Modified, Options),
    Modified @=< LoadTime,
    !.

%!  '$qlf_file'(+Spec, +PlFile, -LoadFile, -Mode, +Options) is det.
%
%   Determine how to load the source. LoadFile is the file to be loaded,
%   Mode is how to load it. Mode is one of
%
%     - compile
%     Normal source compilation
%     - qcompile
%     Compile from source, creating a QLF file in the process
%     - qload
%     Load from QLF file.
%     - stream
%     Load from a stream.  Content can be a source or QLF file.
%
%   @arg Spec is the original search specification
%   @arg PlFile is the resolved absolute path to the Prolog file.

'$qlf_file'(Spec, _, Spec, stream, Options) :-
    '$option'(stream(_), Options),      % stream: no choice
    !.
'$qlf_file'(Spec, FullFile, FullFile, compile, _) :-
    '$spec_extension'(Spec, Ext),       % user explicitly specified
    user:prolog_file_type(Ext, prolog),
    !.
'$qlf_file'(Spec, FullFile, LoadFile, Mode, Options) :-
    '$compilation_mode'(database),
    file_name_extension(Base, PlExt, FullFile),
    user:prolog_file_type(PlExt, prolog),
    user:prolog_file_type(QlfExt, qlf),
    file_name_extension(Base, QlfExt, QlfFile),
    (   access_file(QlfFile, read),
        (   '$qlf_out_of_date'(FullFile, QlfFile, Why)
        ->  (   access_file(QlfFile, write)
            ->  print_message(informational,
                              qlf(recompile(Spec, FullFile, QlfFile, Why))),
                Mode = qcompile
            ;   print_message(warning,
                              qlf(can_not_recompile(Spec, QlfFile, Why))),
                Mode = compile
            ),
            LoadFile = FullFile
        ;   Mode = qload,
            LoadFile = QlfFile
        )
    ->  !
    ;   '$qlf_auto'(FullFile, QlfFile, Options)
    ->  !, Mode = qcompile,
        LoadFile = FullFile
    ).
'$qlf_file'(_, FullFile, FullFile, compile, _).


%!  '$qlf_out_of_date'(+PlFile, +QlfFile, -Why) is semidet.
%
%   True if the  QlfFile  file  is   out-of-date  because  of  Why. This
%   predicate is the negation such that we can return the reason.

'$qlf_out_of_date'(PlFile, QlfFile, Why) :-
    (   access_file(PlFile, read)
    ->  time_file(PlFile, PlTime),
        time_file(QlfFile, QlfTime),
        (   PlTime > QlfTime
        ->  Why = old                   % PlFile is newer
        ;   Error = error(Formal,_),
            catch('$qlf_sources'(QlfFile, _Files), Error, true),
            nonvar(Formal)              % QlfFile is incompatible
        ->  Why = Error
        ;   fail                        % QlfFile is up-to-date and ok
        )
    ;   fail                            % can not read .pl; try .qlf
    ).

%!  '$qlf_auto'(+PlFile, +QlfFile, +Options) is semidet.
%
%   True if we create QlfFile using   qcompile/2. This is determined
%   by the option qcompile(QlfMode) or, if   this is not present, by
%   the prolog_flag qcompile.

:- create_prolog_flag(qcompile, false, [type(atom)]).

'$qlf_auto'(PlFile, QlfFile, Options) :-
    (   memberchk(qcompile(QlfMode), Options)
    ->  true
    ;   current_prolog_flag(qcompile, QlfMode),
        \+ '$in_system_dir'(PlFile)
    ),
    (   QlfMode == auto
    ->  true
    ;   QlfMode == large,
        size_file(PlFile, Size),
        Size > 100000
    ),
    access_file(QlfFile, write).

'$in_system_dir'(PlFile) :-
    current_prolog_flag(home, Home),
    sub_atom(PlFile, 0, _, _, Home).

'$spec_extension'(File, Ext) :-
    atom(File),
    file_name_extension(_, Ext, File).
'$spec_extension'(Spec, Ext) :-
    compound(Spec),
    arg(1, Spec, Arg),
    '$spec_extension'(Arg, Ext).


%!  '$load_file'(+Spec, +ContextModule, +Options) is det.
%
%   Load the file Spec  into   ContextModule  controlled by Options.
%   This wrapper deals with two cases  before proceeding to the real
%   loader:
%
%       * User hooks based on prolog_load_file/2
%       * The file is already loaded.

:- dynamic
    '$resolved_source_path'/2.                  % ?Spec, ?Path

'$load_file'(File, Module, Options) :-
    \+ memberchk(stream(_), Options),
    user:prolog_load_file(Module:File, Options),
    !.
'$load_file'(File, Module, Options) :-
    memberchk(stream(_), Options),
    !,
    '$assert_load_context_module'(File, Module, Options),
    '$qdo_load_file'(File, File, Module, Action, Options),
    '$run_initialization'(File, Action, Options).
'$load_file'(File, Module, Options) :-
    '$resolved_source_path'(File, FullFile),
    (   '$source_file_property'(FullFile, from_state, true)
    ;   '$source_file_property'(FullFile, resource, true)
    ;   '$option'(if(If), Options, true),
        '$noload'(If, FullFile, Options)
    ),
    !,
    '$already_loaded'(File, FullFile, Module, Options).
'$load_file'(File, Module, Options) :-
    absolute_file_name(File, FullFile,
                       [ file_type(prolog),
                         access(read)
                       ]),
    '$register_resolved_source_path'(File, FullFile),
    '$mt_load_file'(File, FullFile, Module, Options),
    '$register_resource_file'(FullFile).

'$register_resolved_source_path'(File, FullFile) :-
    '$resolved_source_path'(File, FullFile),
    !.
'$register_resolved_source_path'(File, FullFile) :-
    compound(File),
    !,
    asserta('$resolved_source_path'(File, FullFile)).
'$register_resolved_source_path'(_, _).

%!  '$translated_source'(+Old, +New) is det.
%
%   Called from loading a QLF state when source files are being renamed.

:- public '$translated_source'/2.
'$translated_source'(Old, New) :-
    forall(retract('$resolved_source_path'(File, Old)),
           assertz('$resolved_source_path'(File, New))).

%!  '$register_resource_file'(+FullFile) is det.
%
%   If we load a file from a resource we   lock  it, so we never have to
%   check the modification again.

'$register_resource_file'(FullFile) :-
    (   sub_atom(FullFile, 0, _, _, 'res://')
    ->  '$set_source_file'(FullFile, resource, true)
    ;   true
    ).

%!  '$already_loaded'(+File, +FullFile, +Module, +Options) is det.
%
%   Called if File is already loaded. If  this is a module-file, the
%   module must be imported into the context  Module. If it is not a
%   module file, it must be reloaded.
%
%   @bug    A file may be associated with multiple modules.  How
%           do we find the `main export module'?  Currently there
%           is no good way to find out which module is associated
%           to the file as a result of the first :- module/2 term.

'$already_loaded'(_File, FullFile, Module, Options) :-
    '$assert_load_context_module'(FullFile, Module, Options),
    '$current_module'(LoadModules, FullFile),
    !,
    (   atom(LoadModules)
    ->  LoadModule = LoadModules
    ;   LoadModules = [LoadModule|_]
    ),
    '$import_from_loaded_module'(LoadModule, Module, Options).
'$already_loaded'(_, _, user, _) :- !.
'$already_loaded'(File, _, Module, Options) :-
    '$load_file'(File, Module, [if(true)|Options]).

%!  '$mt_load_file'(+File, +FullFile, +Module, +Options) is det.
%
%   Deal with multi-threaded  loading  of   files.  The  thread that
%   wishes to load the thread first will  do so, while other threads
%   will wait until the leader finished and  than act as if the file
%   is already loaded.
%
%   Synchronisation is handled using  a   message  queue that exists
%   while the file is being loaded.   This synchronisation relies on
%   the fact that thread_get_message/1 throws  an existence_error if
%   the message queue  is  destroyed.  This   is  hacky.  Events  or
%   condition variables would have made a cleaner design.

:- dynamic
    '$loading_file'/3.              % File, Queue, Thread
:- volatile
    '$loading_file'/3.

'$mt_load_file'(File, FullFile, Module, Options) :-
    current_prolog_flag(threads, true),
    !,
    setup_call_cleanup(
        with_mutex('$load_file',
                   '$mt_start_load'(FullFile, Loading, Options)),
        '$mt_do_load'(Loading, File, FullFile, Module, Options),
        '$mt_end_load'(Loading)).
'$mt_load_file'(File, FullFile, Module, Options) :-
    '$option'(if(If), Options, true),
    '$noload'(If, FullFile, Options),
    !,
    '$already_loaded'(File, FullFile, Module, Options).
'$mt_load_file'(File, FullFile, Module, Options) :-
    '$qdo_load_file'(File, FullFile, Module, Action, Options),
    '$run_initialization'(FullFile, Action, Options).

'$mt_start_load'(FullFile, queue(Queue), _) :-
    '$loading_file'(FullFile, Queue, LoadThread),
    \+ thread_self(LoadThread),
    !.
'$mt_start_load'(FullFile, already_loaded, Options) :-
    '$option'(if(If), Options, true),
    '$noload'(If, FullFile, Options),
    !.
'$mt_start_load'(FullFile, Ref, _) :-
    thread_self(Me),
    message_queue_create(Queue),
    assertz('$loading_file'(FullFile, Queue, Me), Ref).

'$mt_do_load'(queue(Queue), File, FullFile, Module, Options) :-
    !,
    catch(thread_get_message(Queue, _), error(_,_), true),
    '$already_loaded'(File, FullFile, Module, Options).
'$mt_do_load'(already_loaded, File, FullFile, Module, Options) :-
    !,
    '$already_loaded'(File, FullFile, Module, Options).
'$mt_do_load'(_Ref, File, FullFile, Module, Options) :-
    '$assert_load_context_module'(FullFile, Module, Options),
    '$qdo_load_file'(File, FullFile, Module, Action, Options),
    '$run_initialization'(FullFile, Action, Options).

'$mt_end_load'(queue(_)) :- !.
'$mt_end_load'(already_loaded) :- !.
'$mt_end_load'(Ref) :-
    clause('$loading_file'(_, Queue, _), _, Ref),
    erase(Ref),
    thread_send_message(Queue, done),
    message_queue_destroy(Queue).


%!  '$qdo_load_file'(+Spec, +FullFile, +ContextModule, +Options) is det.
%
%   Switch to qcompile mode if requested by the option '$qlf'(+Out)

'$qdo_load_file'(File, FullFile, Module, Action, Options) :-
    memberchk('$qlf'(QlfOut), Options),
    '$stage_file'(QlfOut, StageQlf),
    !,
    setup_call_catcher_cleanup(
        '$qstart'(StageQlf, Module, State),
        '$do_load_file'(File, FullFile, Module, Action, Options),
        Catcher,
        '$qend'(State, Catcher, StageQlf, QlfOut)).
'$qdo_load_file'(File, FullFile, Module, Action, Options) :-
    '$do_load_file'(File, FullFile, Module, Action, Options).

'$qstart'(Qlf, Module, state(OldMode, OldModule)) :-
    '$qlf_open'(Qlf),
    '$compilation_mode'(OldMode, qlf),
    '$set_source_module'(OldModule, Module).

'$qend'(state(OldMode, OldModule), Catcher, StageQlf, QlfOut) :-
    '$set_source_module'(_, OldModule),
    '$set_compilation_mode'(OldMode),
    '$qlf_close',
    '$install_staged_file'(Catcher, StageQlf, QlfOut, warn).

'$set_source_module'(OldModule, Module) :-
    '$current_source_module'(OldModule),
    '$set_source_module'(Module).

%!  '$do_load_file'(+Spec, +FullFile, +ContextModule,
%!                  -Action, +Options) is det.
%
%   Perform the actual loading.

'$do_load_file'(File, FullFile, Module, Action, Options) :-
    '$option'(derived_from(DerivedFrom), Options, -),
    '$register_derived_source'(FullFile, DerivedFrom),
    '$qlf_file'(File, FullFile, Absolute, Mode, Options),
    (   Mode == qcompile
    ->  qcompile(Module:File, Options)
    ;   '$do_load_file_2'(File, Absolute, Module, Action, Options)
    ).

'$do_load_file_2'(File, Absolute, Module, Action, Options) :-
    '$source_file_property'(Absolute, number_of_clauses, OldClauses),
    statistics(cputime, OldTime),

    '$setup_load'(ScopedFlags, OldSandBoxed, OldVerbose, OldAutoLevel, OldXRef,
                  Options),

    '$compilation_level'(Level),
    '$load_msg_level'(load_file, Level, StartMsgLevel, DoneMsgLevel),
    '$print_message'(StartMsgLevel,
                     load_file(start(Level,
                                     file(File, Absolute)))),

    (   memberchk(stream(FromStream), Options)
    ->  Input = stream
    ;   Input = source
    ),

    (   Input == stream,
        (   '$option'(format(qlf), Options, source)
        ->  set_stream(FromStream, file_name(Absolute)),
            '$qload_stream'(FromStream, Module, Action, LM, Options)
        ;   '$consult_file'(stream(Absolute, FromStream, []),
                            Module, Action, LM, Options)
        )
    ->  true
    ;   Input == source,
        file_name_extension(_, Ext, Absolute),
        (   user:prolog_file_type(Ext, qlf),
            E = error(_,_),
            catch('$qload_file'(Absolute, Module, Action, LM, Options),
                  E,
                  print_message(warning, E))
        ->  true
        ;   '$consult_file'(Absolute, Module, Action, LM, Options)
        )
    ->  true
    ;   '$print_message'(error, load_file(failed(File))),
        fail
    ),

    '$import_from_loaded_module'(LM, Module, Options),

    '$source_file_property'(Absolute, number_of_clauses, NewClauses),
    statistics(cputime, Time),
    ClausesCreated is NewClauses - OldClauses,
    TimeUsed is Time - OldTime,

    '$print_message'(DoneMsgLevel,
                     load_file(done(Level,
                                    file(File, Absolute),
                                    Action,
                                    LM,
                                    TimeUsed,
                                    ClausesCreated))),

    '$restore_load'(ScopedFlags, OldSandBoxed, OldVerbose, OldAutoLevel, OldXRef).

'$setup_load'(ScopedFlags, OldSandBoxed, OldVerbose, OldAutoLevel, OldXRef,
              Options) :-
    '$save_file_scoped_flags'(ScopedFlags),
    '$set_sandboxed_load'(Options, OldSandBoxed),
    '$set_verbose_load'(Options, OldVerbose),
    '$set_optimise_load'(Options),
    '$update_autoload_level'(Options, OldAutoLevel),
    '$set_no_xref'(OldXRef).

'$restore_load'(ScopedFlags, OldSandBoxed, OldVerbose, OldAutoLevel, OldXRef) :-
    '$set_autoload_level'(OldAutoLevel),
    set_prolog_flag(xref, OldXRef),
    set_prolog_flag(verbose_load, OldVerbose),
    set_prolog_flag(sandboxed_load, OldSandBoxed),
    '$restore_file_scoped_flags'(ScopedFlags).


%!  '$save_file_scoped_flags'(-State) is det.
%!  '$restore_file_scoped_flags'(-State) is det.
%
%   Save/restore flags that are scoped to a compilation unit.

'$save_file_scoped_flags'(State) :-
    current_predicate(findall/3),          % Not when doing boot compile
    !,
    findall(SavedFlag, '$save_file_scoped_flag'(SavedFlag), State).
'$save_file_scoped_flags'([]).

'$save_file_scoped_flag'(Flag-Value) :-
    '$file_scoped_flag'(Flag, Default),
    (   current_prolog_flag(Flag, Value)
    ->  true
    ;   Value = Default
    ).

'$file_scoped_flag'(generate_debug_info, true).
'$file_scoped_flag'(optimise,            false).
'$file_scoped_flag'(xref,                false).

'$restore_file_scoped_flags'([]).
'$restore_file_scoped_flags'([Flag-Value|T]) :-
    set_prolog_flag(Flag, Value),
    '$restore_file_scoped_flags'(T).


%!  '$import_from_loaded_module'(LoadedModule, Module, Options) is det.
%
%   Import public predicates from LoadedModule into Module

'$import_from_loaded_module'(LoadedModule, Module, Options) :-
    LoadedModule \== Module,
    atom(LoadedModule),
    !,
    '$option'(imports(Import), Options, all),
    '$option'(reexport(Reexport), Options, false),
    '$import_list'(Module, LoadedModule, Import, Reexport).
'$import_from_loaded_module'(_, _, _).


%!  '$set_verbose_load'(+Options, -Old) is det.
%
%   Set the =verbose_load= flag according to   Options and unify Old
%   with the old value.

'$set_verbose_load'(Options, Old) :-
    current_prolog_flag(verbose_load, Old),
    (   memberchk(silent(Silent), Options)
    ->  (   '$negate'(Silent, Level0)
        ->  '$load_msg_compat'(Level0, Level)
        ;   Level = Silent
        ),
        set_prolog_flag(verbose_load, Level)
    ;   true
    ).

'$negate'(true, false).
'$negate'(false, true).

%!  '$set_sandboxed_load'(+Options, -Old) is det.
%
%   Update the Prolog flag  =sandboxed_load=   from  Options. Old is
%   unified with the old flag.
%
%   @error permission_error(leave, sandbox, -)

'$set_sandboxed_load'(Options, Old) :-
    current_prolog_flag(sandboxed_load, Old),
    (   memberchk(sandboxed(SandBoxed), Options),
        '$enter_sandboxed'(Old, SandBoxed, New),
        New \== Old
    ->  set_prolog_flag(sandboxed_load, New)
    ;   true
    ).

'$enter_sandboxed'(Old, New, SandBoxed) :-
    (   Old == false, New == true
    ->  SandBoxed = true,
        '$ensure_loaded_library_sandbox'
    ;   Old == true, New == false
    ->  throw(error(permission_error(leave, sandbox, -), _))
    ;   SandBoxed = Old
    ).
'$enter_sandboxed'(false, true, true).

'$ensure_loaded_library_sandbox' :-
    source_file_property(library(sandbox), module(sandbox)),
    !.
'$ensure_loaded_library_sandbox' :-
    load_files(library(sandbox), [if(not_loaded), silent(true)]).

'$set_optimise_load'(Options) :-
    (   '$option'(optimise(Optimise), Options)
    ->  set_prolog_flag(optimise, Optimise)
    ;   true
    ).

'$set_no_xref'(OldXRef) :-
    (   current_prolog_flag(xref, OldXRef)
    ->  true
    ;   OldXRef = false
    ),
    set_prolog_flag(xref, false).


%!  '$update_autoload_level'(+Options, -OldLevel)
%
%   Update the '$autoload_nesting' and return the old value.

:- thread_local
    '$autoload_nesting'/1.

'$update_autoload_level'(Options, AutoLevel) :-
    '$option'(autoload(Autoload), Options, false),
    (   '$autoload_nesting'(CurrentLevel)
    ->  AutoLevel = CurrentLevel
    ;   AutoLevel = 0
    ),
    (   Autoload == false
    ->  true
    ;   NewLevel is AutoLevel + 1,
        '$set_autoload_level'(NewLevel)
    ).

'$set_autoload_level'(New) :-
    retractall('$autoload_nesting'(_)),
    asserta('$autoload_nesting'(New)).


%!  '$print_message'(+Level, +Term) is det.
%
%   As print_message/2, but deal with  the   fact  that  the message
%   system might not yet be loaded.

'$print_message'(Level, Term) :-
    current_predicate(system:print_message/2),
    !,
    print_message(Level, Term).
'$print_message'(warning, Term) :-
    source_location(File, Line),
    !,
    format(user_error, 'WARNING: ~w:~w: ~p~n', [File, Line, Term]).
'$print_message'(error, Term) :-
    !,
    source_location(File, Line),
    !,
    format(user_error, 'ERROR: ~w:~w: ~p~n', [File, Line, Term]).
'$print_message'(_Level, _Term).

'$print_message_fail'(E) :-
    '$print_message'(error, E),
    fail.

%!  '$consult_file'(+Path, +Module, -Action, -LoadedIn, +Options)
%
%   Called  from  '$do_load_file'/4  using  the   goal  returned  by
%   '$consult_goal'/2. This means that the  calling conventions must
%   be kept synchronous with '$qload_file'/6.

'$consult_file'(Absolute, Module, What, LM, Options) :-
    '$current_source_module'(Module),   % same module
    !,
    '$consult_file_2'(Absolute, Module, What, LM, Options).
'$consult_file'(Absolute, Module, What, LM, Options) :-
    '$set_source_module'(OldModule, Module),
    '$ifcompiling'('$qlf_start_sub_module'(Module)),
    '$consult_file_2'(Absolute, Module, What, LM, Options),
    '$ifcompiling'('$qlf_end_part'),
    '$set_source_module'(OldModule).

'$consult_file_2'(Absolute, Module, What, LM, Options) :-
    '$set_source_module'(OldModule, Module),
    '$load_id'(Absolute, Id, Modified, Options),
    '$start_consult'(Id, Modified),
    (   '$derived_source'(Absolute, DerivedFrom, _)
    ->  '$modified_id'(DerivedFrom, DerivedModified, Options),
        '$start_consult'(DerivedFrom, DerivedModified)
    ;   true
    ),
    '$compile_type'(What),
    '$save_lex_state'(LexState, Options),
    '$set_dialect'(Options),
    call_cleanup('$load_file'(Absolute, Id, LM, Options),
                 '$end_consult'(LexState, OldModule)).

'$end_consult'(LexState, OldModule) :-
    '$restore_lex_state'(LexState),
    '$set_source_module'(OldModule).


:- create_prolog_flag(emulated_dialect, swi, [type(atom)]).

%!  '$save_lex_state'(-LexState, +Options) is det.

'$save_lex_state'(State, Options) :-
    memberchk(scope_settings(false), Options),
    !,
    State = (-).
'$save_lex_state'(lexstate(Style, Dialect), _) :-
    '$style_check'(Style, Style),
    current_prolog_flag(emulated_dialect, Dialect).

'$restore_lex_state'(-) :- !.
'$restore_lex_state'(lexstate(Style, Dialect)) :-
    '$style_check'(_, Style),
    set_prolog_flag(emulated_dialect, Dialect).

'$set_dialect'(Options) :-
    memberchk(dialect(Dialect), Options),
    !,
    expects_dialect(Dialect).               % Autoloaded from library
'$set_dialect'(_).

'$load_id'(stream(Id, _, _), Id, Modified, Options) :-
    !,
    '$modified_id'(Id, Modified, Options).
'$load_id'(Id, Id, Modified, Options) :-
    '$modified_id'(Id, Modified, Options).

'$modified_id'(_, Modified, Options) :-
    '$option'(modified(Stamp), Options, Def),
    Stamp \== Def,
    !,
    Modified = Stamp.
'$modified_id'(Id, Modified, _) :-
    catch(time_file(Id, Modified),
          error(_, _),
          fail),
    !.
'$modified_id'(_, 0.0, _).


'$compile_type'(What) :-
    '$compilation_mode'(How),
    (   How == database
    ->  What = compiled
    ;   How == qlf
    ->  What = '*qcompiled*'
    ;   What = 'boot compiled'
    ).

%!  '$assert_load_context_module'(+File, -Module, -Options)
%
%   Record the module a file was loaded from (see make/0). The first
%   clause deals with loading from  another   file.  On reload, this
%   clause will be discarded by  $start_consult/1. The second clause
%   deals with reload from the toplevel.   Here  we avoid creating a
%   duplicate dynamic (i.e., not related to a source) clause.

:- dynamic
    '$load_context_module'/3.
:- multifile
    '$load_context_module'/3.

'$assert_load_context_module'(_, _, Options) :-
    memberchk(register(false), Options),
    !.
'$assert_load_context_module'(File, Module, Options) :-
    source_location(FromFile, Line),
    !,
    '$master_file'(FromFile, MasterFile),
    '$check_load_non_module'(File, Module),
    '$add_dialect'(Options, Options1),
    '$load_ctx_options'(Options1, Options2),
    '$store_admin_clause'(
        system:'$load_context_module'(File, Module, Options2),
        _Layout, MasterFile, FromFile:Line).
'$assert_load_context_module'(File, Module, Options) :-
    '$check_load_non_module'(File, Module),
    '$add_dialect'(Options, Options1),
    '$load_ctx_options'(Options1, Options2),
    (   clause('$load_context_module'(File, Module, _), true, Ref),
        \+ clause_property(Ref, file(_)),
        erase(Ref)
    ->  true
    ;   true
    ),
    assertz('$load_context_module'(File, Module, Options2)).

'$add_dialect'(Options0, Options) :-
    current_prolog_flag(emulated_dialect, Dialect), Dialect \== swi,
    !,
    Options = [dialect(Dialect)|Options0].
'$add_dialect'(Options, Options).

%!  '$load_ctx_options'(+Options, -CtxOptions) is det.
%
%   Select the load options that  determine   the  load semantics to
%   perform a proper reload. Delete the others.

'$load_ctx_options'([], []).
'$load_ctx_options'([H|T0], [H|T]) :-
    '$load_ctx_option'(H),
    !,
    '$load_ctx_options'(T0, T).
'$load_ctx_options'([_|T0], T) :-
    '$load_ctx_options'(T0, T).

'$load_ctx_option'(derived_from(_)).
'$load_ctx_option'(dialect(_)).
'$load_ctx_option'(encoding(_)).
'$load_ctx_option'(imports(_)).
'$load_ctx_option'(reexport(_)).


%!  '$check_load_non_module'(+File) is det.
%
%   Test  that  a  non-module  file  is  not  loaded  into  multiple
%   contexts.

'$check_load_non_module'(File, _) :-
    '$current_module'(_, File),
    !.          % File is a module file
'$check_load_non_module'(File, Module) :-
    '$load_context_module'(File, OldModule, _),
    Module \== OldModule,
    !,
    format(atom(Msg),
           'Non-module file already loaded into module ~w; \c
               trying to load into ~w',
           [OldModule, Module]),
    throw(error(permission_error(load, source, File),
                context(load_files/2, Msg))).
'$check_load_non_module'(_, _).

%!  '$load_file'(+Path, +Id, -Module, +Options)
%
%   '$load_file'/4 does the actual loading.
%
%   state(FirstTerm:boolean,
%         Module:atom,
%         AtEnd:atom,
%         Stop:boolean,
%         Id:atom,
%         Dialect:atom)

'$load_file'(Path, Id, Module, Options) :-
    State = state(true, _, true, false, Id, -),
    (   '$source_term'(Path, _Read, _Layout, Term, Layout,
                       _Stream, Options),
        '$valid_term'(Term),
        (   arg(1, State, true)
        ->  '$first_term'(Term, Layout, Id, State, Options),
            nb_setarg(1, State, false)
        ;   '$compile_term'(Term, Layout, Id)
        ),
        arg(4, State, true)
    ;   '$end_load_file'(State)
    ),
    !,
    arg(2, State, Module).

'$valid_term'(Var) :-
    var(Var),
    !,
    print_message(error, error(instantiation_error, _)).
'$valid_term'(Term) :-
    Term \== [].

'$end_load_file'(State) :-
    arg(1, State, true),           % empty file
    !,
    nb_setarg(2, State, Module),
    arg(5, State, Id),
    '$current_source_module'(Module),
    '$ifcompiling'('$qlf_start_file'(Id)),
    '$ifcompiling'('$qlf_end_part').
'$end_load_file'(State) :-
    arg(3, State, End),
    '$end_load_file'(End, State).

'$end_load_file'(true, _).
'$end_load_file'(end_module, State) :-
    arg(2, State, Module),
    '$check_export'(Module),
    '$ifcompiling'('$qlf_end_part').
'$end_load_file'(end_non_module, _State) :-
    '$ifcompiling'('$qlf_end_part').


'$first_term'(?-(Directive), Layout, Id, State, Options) :-
    !,
    '$first_term'(:-(Directive), Layout, Id, State, Options).
'$first_term'(:-(Directive), _Layout, Id, State, Options) :-
    nonvar(Directive),
    (   (   Directive = module(Name, Public)
        ->  Imports = []
        ;   Directive = module(Name, Public, Imports)
        )
    ->  !,
        '$module_name'(Name, Id, Module, Options),
        '$start_module'(Module, Public, State, Options),
        '$module3'(Imports)
    ;   Directive = expects_dialect(Dialect)
    ->  !,
        '$set_dialect'(Dialect, State),
        fail                        % Still consider next term as first
    ).
'$first_term'(Term, Layout, Id, State, Options) :-
    '$start_non_module'(Id, State, Options),
    '$compile_term'(Term, Layout, Id).

'$compile_term'(Term, Layout, Id) :-
    '$compile_term'(Term, Layout, Id, -).

'$compile_term'(Var, _Layout, _Id, _Src) :-
    var(Var),
    !,
    '$instantiation_error'(Var).
'$compile_term'((?-Directive), _Layout, Id, _) :-
    !,
    '$execute_directive'(Directive, Id).
'$compile_term'((:-Directive), _Layout, Id, _) :-
    !,
    '$execute_directive'(Directive, Id).
'$compile_term'('$source_location'(File, Line):Term, Layout, Id, _) :-
    !,
    '$compile_term'(Term, Layout, Id, File:Line).
'$compile_term'(Clause, Layout, Id, SrcLoc) :-
    E = error(_,_),
    catch('$store_clause'(Clause, Layout, Id, SrcLoc), E,
          '$print_message'(error, E)).

'$start_non_module'(Id, _State, Options) :-
    '$option'(must_be_module(true), Options, false),
    !,
    throw(error(domain_error(module_file, Id), _)).
'$start_non_module'(Id, State, _Options) :-
    '$current_source_module'(Module),
    '$ifcompiling'('$qlf_start_file'(Id)),
    '$qset_dialect'(State),
    nb_setarg(2, State, Module),
    nb_setarg(3, State, end_non_module).

%!  '$set_dialect'(+Dialect, +State)
%
%   Sets the expected dialect. This is difficult if we are compiling
%   a .qlf file using qcompile/1 because   the file is already open,
%   while we are looking for the first term to decide wether this is
%   a module or not. We save the   dialect  and set it after opening
%   the file or module.
%
%   Note that expects_dialect/1 itself may   be  autoloaded from the
%   library.

'$set_dialect'(Dialect, State) :-
    '$compilation_mode'(qlf, database),
    !,
    expects_dialect(Dialect),
    '$compilation_mode'(_, qlf),
    nb_setarg(6, State, Dialect).
'$set_dialect'(Dialect, _) :-
    expects_dialect(Dialect).

'$qset_dialect'(State) :-
    '$compilation_mode'(qlf),
    arg(6, State, Dialect), Dialect \== (-),
    !,
    '$add_directive_wic'(expects_dialect(Dialect)).
'$qset_dialect'(_).


                 /*******************************
                 *           MODULES            *
                 *******************************/

'$start_module'(Module, _Public, State, _Options) :-
    '$current_module'(Module, OldFile),
    source_location(File, _Line),
    OldFile \== File, OldFile \== [],
    same_file(OldFile, File),
    !,
    nb_setarg(2, State, Module),
    nb_setarg(4, State, true).      % Stop processing
'$start_module'(Module, Public, State, Options) :-
    arg(5, State, File),
    nb_setarg(2, State, Module),
    source_location(_File, Line),
    '$option'(redefine_module(Action), Options, false),
    '$module_class'(File, Class, Super),
    '$redefine_module'(Module, File, Action),
    '$declare_module'(Module, Class, Super, File, Line, false),
    '$export_list'(Public, Module, Ops),
    '$ifcompiling'('$qlf_start_module'(Module)),
    '$export_ops'(Ops, Module, File),
    '$qset_dialect'(State),
    nb_setarg(3, State, end_module).


%!  '$module3'(+Spec) is det.
%
%   Handle the 3th argument of a module declartion.

'$module3'(Var) :-
    var(Var),
    !,
    '$instantiation_error'(Var).
'$module3'([]) :- !.
'$module3'([H|T]) :-
    !,
    '$module3'(H),
    '$module3'(T).
'$module3'(Id) :-
    use_module(library(dialect/Id)).

%!  '$module_name'(?Name, +Id, -Module, +Options) is semidet.
%
%   Determine the module name.  There are some cases:
%
%     - Option module(Module) is given.  In that case, use this
%       module and if Module is the load context, ignore the module
%       header.
%     - The initial name is unbound.  Use the base name of the
%       source identifier (normally the file name).  Compatibility
%       to Ciao.  This might change; I think it is wiser to use
%       the full unique source identifier.

'$module_name'(_, _, Module, Options) :-
    '$option'(module(Module), Options),
    !,
    '$current_source_module'(Context),
    Context \== Module.                     % cause '$first_term'/5 to fail.
'$module_name'(Var, Id, Module, Options) :-
    var(Var),
    !,
    file_base_name(Id, File),
    file_name_extension(Var, _, File),
    '$module_name'(Var, Id, Module, Options).
'$module_name'(Reserved, _, _, _) :-
    '$reserved_module'(Reserved),
    !,
    throw(error(permission_error(load, module, Reserved), _)).
'$module_name'(Module, _Id, Module, _).


'$reserved_module'(system).
'$reserved_module'(user).


%!  '$redefine_module'(+Module, +File, -Redefine)

'$redefine_module'(_Module, _, false) :- !.
'$redefine_module'(Module, File, true) :-
    !,
    (   module_property(Module, file(OldFile)),
        File \== OldFile
    ->  unload_file(OldFile)
    ;   true
    ).
'$redefine_module'(Module, File, ask) :-
    (   stream_property(user_input, tty(true)),
        module_property(Module, file(OldFile)),
        File \== OldFile,
        '$rdef_response'(Module, OldFile, File, true)
    ->  '$redefine_module'(Module, File, true)
    ;   true
    ).

'$rdef_response'(Module, OldFile, File, Ok) :-
    repeat,
    print_message(query, redefine_module(Module, OldFile, File)),
    get_single_char(Char),
    '$rdef_response'(Char, Ok0),
    !,
    Ok = Ok0.

'$rdef_response'(Char, true) :-
    memberchk(Char, `yY`),
    format(user_error, 'yes~n', []).
'$rdef_response'(Char, false) :-
    memberchk(Char, `nN`),
    format(user_error, 'no~n', []).
'$rdef_response'(Char, _) :-
    memberchk(Char, `a`),
    format(user_error, 'abort~n', []),
    abort.
'$rdef_response'(_, _) :-
    print_message(help, redefine_module_reply),
    fail.


%!  '$module_class'(+File, -Class, -Super) is det.
%
%   Determine the initial module from which   I  inherit. All system
%   and library modules inherit from =system=, while all normal user
%   modules inherit from =user=.

'$module_class'(File, Class, system) :-
    current_prolog_flag(home, Home),
    sub_atom(File, 0, Len, _, Home),
    !,
    (   sub_atom(File, Len, _, _, '/boot/')
    ->  Class = system
    ;   Class = library
    ).
'$module_class'(_, user, user).

'$check_export'(Module) :-
    '$undefined_export'(Module, UndefList),
    (   '$member'(Undef, UndefList),
        strip_module(Undef, _, Local),
        print_message(error,
                      undefined_export(Module, Local)),
        fail
    ;   true
    ).


%!  '$import_list'(+TargetModule, +FromModule, +Import, +Reexport) is det.
%
%   Import from FromModule to TargetModule. Import  is one of =all=,
%   a list of optionally  mapped  predicate   indicators  or  a term
%   except(Import).

'$import_list'(_, _, Var, _) :-
    var(Var),
    !,
    throw(error(instantitation_error, _)).
'$import_list'(Target, Source, all, Reexport) :-
    !,
    '$exported_ops'(Source, Import, Predicates),
    '$module_property'(Source, exports(Predicates)),
    '$import_all'(Import, Target, Source, Reexport, weak).
'$import_list'(Target, Source, except(Spec), Reexport) :-
    !,
    '$exported_ops'(Source, Export, Predicates),
    '$module_property'(Source, exports(Predicates)),
    (   is_list(Spec)
    ->  true
    ;   throw(error(type_error(list, Spec), _))
    ),
    '$import_except'(Spec, Export, Import),
    '$import_all'(Import, Target, Source, Reexport, weak).
'$import_list'(Target, Source, Import, Reexport) :-
    !,
    is_list(Import),
    !,
    '$import_all'(Import, Target, Source, Reexport, strong).
'$import_list'(_, _, Import, _) :-
    throw(error(type_error(import_specifier, Import))).


'$import_except'([], List, List).
'$import_except'([H|T], List0, List) :-
    '$import_except_1'(H, List0, List1),
    '$import_except'(T, List1, List).

'$import_except_1'(Var, _, _) :-
    var(Var),
    !,
    throw(error(instantitation_error, _)).
'$import_except_1'(PI as N, List0, List) :-
    '$pi'(PI), atom(N),
    !,
    '$canonical_pi'(PI, CPI),
    '$import_as'(CPI, N, List0, List).
'$import_except_1'(op(P,A,N), List0, List) :-
    !,
    '$remove_ops'(List0, op(P,A,N), List).
'$import_except_1'(PI, List0, List) :-
    '$pi'(PI),
    !,
    '$canonical_pi'(PI, CPI),
    '$select'(P, List0, List),
    '$canonical_pi'(CPI, P),
    !.
'$import_except_1'(Except, _, _) :-
    throw(error(type_error(import_specifier, Except), _)).

'$import_as'(CPI, N, [PI2|T], [CPI as N|T]) :-
    '$canonical_pi'(PI2, CPI),
    !.
'$import_as'(PI, N, [H|T0], [H|T]) :-
    !,
    '$import_as'(PI, N, T0, T).
'$import_as'(PI, _, _, _) :-
    throw(error(existence_error(export, PI), _)).

'$pi'(N/A) :- atom(N), integer(A), !.
'$pi'(N//A) :- atom(N), integer(A).

'$canonical_pi'(N//A0, N/A) :-
    A is A0 + 2.
'$canonical_pi'(PI, PI).

'$remove_ops'([], _, []).
'$remove_ops'([Op|T0], Pattern, T) :-
    subsumes_term(Pattern, Op),
    !,
    '$remove_ops'(T0, Pattern, T).
'$remove_ops'([H|T0], Pattern, [H|T]) :-
    '$remove_ops'(T0, Pattern, T).


%!  '$import_all'(+Import, +Context, +Source, +Reexport, +Strength)

'$import_all'(Import, Context, Source, Reexport, Strength) :-
    '$import_all2'(Import, Context, Source, Imported, ImpOps, Strength),
    (   Reexport == true,
        (   '$list_to_conj'(Imported, Conj)
        ->  export(Context:Conj),
            '$ifcompiling'('$add_directive_wic'(export(Context:Conj)))
        ;   true
        ),
        source_location(File, _Line),
        '$export_ops'(ImpOps, Context, File)
    ;   true
    ).

%!  '$import_all2'(+Imports, +Context, +Source, -Imported, -ImpOps, +Strength)

'$import_all2'([], _, _, [], [], _).
'$import_all2'([PI as NewName|Rest], Context, Source,
               [NewName/Arity|Imported], ImpOps, Strength) :-
    !,
    '$canonical_pi'(PI, Name/Arity),
    length(Args, Arity),
    Head =.. [Name|Args],
    NewHead =.. [NewName|Args],
    (   '$get_predicate_attribute'(Source:Head, transparent, 1)
    ->  '$set_predicate_attribute'(Context:NewHead, transparent, true)
    ;   true
    ),
    (   source_location(File, Line)
    ->  E = error(_,_),
        catch('$store_admin_clause'((NewHead :- Source:Head),
                                    _Layout, File, File:Line),
              E, '$print_message'(error, E))
    ;   assertz((NewHead :- !, Source:Head)) % ! avoids problems with
    ),                                       % duplicate load
    '$import_all2'(Rest, Context, Source, Imported, ImpOps, Strength).
'$import_all2'([op(P,A,N)|Rest], Context, Source, Imported,
               [op(P,A,N)|ImpOps], Strength) :-
    !,
    '$import_ops'(Context, Source, op(P,A,N)),
    '$import_all2'(Rest, Context, Source, Imported, ImpOps, Strength).
'$import_all2'([Pred|Rest], Context, Source, [Pred|Imported], ImpOps, Strength) :-
    Error = error(_,_),
    catch(Context:'$import'(Source:Pred, Strength), Error,
          print_message(error, Error)),
    '$ifcompiling'('$import_wic'(Source, Pred, Strength)),
    '$import_all2'(Rest, Context, Source, Imported, ImpOps, Strength).


'$list_to_conj'([One], One) :- !.
'$list_to_conj'([H|T], (H,Rest)) :-
    '$list_to_conj'(T, Rest).

%!  '$exported_ops'(+Module, -Ops, ?Tail) is det.
%
%   Ops is a list of op(P,A,N) terms representing the operators
%   exported from Module.

'$exported_ops'(Module, Ops, Tail) :-
    '$c_current_predicate'(_, Module:'$exported_op'(_,_,_)),
    !,
    findall(op(P,A,N), Module:'$exported_op'(P,A,N), Ops, Tail).
'$exported_ops'(_, Ops, Ops).

'$exported_op'(Module, P, A, N) :-
    '$c_current_predicate'(_, Module:'$exported_op'(_,_,_)),
    Module:'$exported_op'(P, A, N).

%!  '$import_ops'(+Target, +Source, +Pattern)
%
%   Import the operators export from Source into the module table of
%   Target.  We only import operators that unify with Pattern.

'$import_ops'(To, From, Pattern) :-
    ground(Pattern),
    !,
    Pattern = op(P,A,N),
    op(P,A,To:N),
    (   '$exported_op'(From, P, A, N)
    ->  true
    ;   print_message(warning, no_exported_op(From, Pattern))
    ).
'$import_ops'(To, From, Pattern) :-
    (   '$exported_op'(From, Pri, Assoc, Name),
        Pattern = op(Pri, Assoc, Name),
        op(Pri, Assoc, To:Name),
        fail
    ;   true
    ).


%!  '$export_list'(+Declarations, +Module, -Ops)
%
%   Handle the export list of the module declaration for Module
%   associated to File.

'$export_list'(Decls, Module, Ops) :-
    is_list(Decls),
    !,
    '$do_export_list'(Decls, Module, Ops).
'$export_list'(Decls, _, _) :-
    var(Decls),
    throw(error(instantiation_error, _)).
'$export_list'(Decls, _, _) :-
    throw(error(type_error(list, Decls), _)).

'$do_export_list'([], _, []) :- !.
'$do_export_list'([H|T], Module, Ops) :-
    !,
    E = error(_,_),
    catch('$export1'(H, Module, Ops, Ops1),
          E, ('$print_message'(error, E), Ops = Ops1)),
    '$do_export_list'(T, Module, Ops1).

'$export1'(Var, _, _, _) :-
    var(Var),
    !,
    throw(error(instantiation_error, _)).
'$export1'(Op, _, [Op|T], T) :-
    Op = op(_,_,_),
    !.
'$export1'(PI0, Module, Ops, Ops) :-
    strip_module(Module:PI0, M, PI),
    (   PI = (_//_)
    ->  non_terminal(M:PI)
    ;   true
    ),
    export(M:PI).

'$export_ops'([op(Pri, Assoc, Name)|T], Module, File) :-
    E = error(_,_),
    catch(( '$execute_directive'(op(Pri, Assoc, Module:Name), File),
            '$export_op'(Pri, Assoc, Name, Module, File)
          ),
          E, '$print_message'(error, E)),
    '$export_ops'(T, Module, File).
'$export_ops'([], _, _).

'$export_op'(Pri, Assoc, Name, Module, File) :-
    (   '$get_predicate_attribute'(Module:'$exported_op'(_,_,_), defined, 1)
    ->  true
    ;   '$execute_directive'(discontiguous(Module:'$exported_op'/3), File)
    ),
    '$store_admin_clause'('$exported_op'(Pri, Assoc, Name), _Layout, File, -).

%!  '$execute_directive'(:Goal, +File) is det.
%
%   Execute the argument of :- or ?- while loading a file.

'$execute_directive'(Goal, F) :-
    '$execute_directive_2'(Goal, F).

'$execute_directive_2'(encoding(Encoding), _F) :-
    !,
    (   '$load_input'(_F, S)
    ->  set_stream(S, encoding(Encoding))
    ).
'$execute_directive_2'(ISO, F) :-
    '$expand_directive'(ISO, Normal),
    !,
    '$execute_directive'(Normal, F).
'$execute_directive_2'(Goal, _) :-
    \+ '$compilation_mode'(database),
    !,
    '$add_directive_wic2'(Goal, Type),
    (   Type == call                % suspend compiling into .qlf file
    ->  '$compilation_mode'(Old, database),
        setup_call_cleanup(
            '$directive_mode'(OldDir, Old),
            '$execute_directive_3'(Goal),
            ( '$set_compilation_mode'(Old),
              '$set_directive_mode'(OldDir)
            ))
    ;   '$execute_directive_3'(Goal)
    ).
'$execute_directive_2'(Goal, _) :-
    '$execute_directive_3'(Goal).

'$execute_directive_3'(Goal) :-
    '$current_source_module'(Module),
    '$valid_directive'(Module:Goal),
    !,
    (   '$pattr_directive'(Goal, Module)
    ->  true
    ;   Term = error(_,_),
        catch(Module:Goal, Term, '$exception_in_directive'(Term))
    ->  true
    ;   '$print_message'(warning, goal_failed(directive, Module:Goal)),
        fail
    ).
'$execute_directive_3'(_).


%!  '$valid_directive'(:Directive) is det.
%
%   If   the   flag   =sandboxed_load=   is   =true=,   this   calls
%   prolog:sandbox_allowed_directive/1. This call can deny execution
%   of the directive by throwing an exception.

:- multifile prolog:sandbox_allowed_directive/1.
:- multifile prolog:sandbox_allowed_clause/1.
:- meta_predicate '$valid_directive'(:).

'$valid_directive'(_) :-
    current_prolog_flag(sandboxed_load, false),
    !.
'$valid_directive'(Goal) :-
    Error = error(Formal, _),
    catch(prolog:sandbox_allowed_directive(Goal), Error, true),
    !,
    (   var(Formal)
    ->  true
    ;   print_message(error, Error),
        fail
    ).
'$valid_directive'(Goal) :-
    print_message(error,
                  error(permission_error(execute,
                                         sandboxed_directive,
                                         Goal), _)),
    fail.

'$exception_in_directive'(Term) :-
    '$print_message'(error, Term),
    fail.

%       This predicate deals with the very odd ISO requirement to allow
%       for :- dynamic(a/2, b/3, c/4) instead of the normally used
%       :- dynamic a/2, b/3, c/4 or, if operators are not desirable,
%       :- dynamic((a/2, b/3, c/4)).

'$expand_directive'(Directive, Expanded) :-
    functor(Directive, Name, Arity),
    Arity > 1,
    '$iso_property_directive'(Name),
    Directive =.. [Name|Args],
    '$mk_normal_args'(Args, Normal),
    Expanded =.. [Name, Normal].

'$iso_property_directive'(dynamic).
'$iso_property_directive'(multifile).
'$iso_property_directive'(discontiguous).

'$mk_normal_args'([One], One).
'$mk_normal_args'([H|T0], (H,T)) :-
    '$mk_normal_args'(T0, T).


%       Note that the list, consult and ensure_loaded directives are already
%       handled at compile time and therefore should not go into the
%       intermediate code file.

'$add_directive_wic2'(Goal, Type) :-
    '$common_goal_type'(Goal, Type),
    !,
    (   Type == load
    ->  true
    ;   '$current_source_module'(Module),
        '$add_directive_wic'(Module:Goal)
    ).
'$add_directive_wic2'(Goal, _) :-
    (   '$compilation_mode'(qlf)    % no problem for qlf files
    ->  true
    ;   print_message(error, mixed_directive(Goal))
    ).

'$common_goal_type'((A,B), Type) :-
    !,
    '$common_goal_type'(A, Type),
    '$common_goal_type'(B, Type).
'$common_goal_type'((A;B), Type) :-
    !,
    '$common_goal_type'(A, Type),
    '$common_goal_type'(B, Type).
'$common_goal_type'((A->B), Type) :-
    !,
    '$common_goal_type'(A, Type),
    '$common_goal_type'(B, Type).
'$common_goal_type'(Goal, Type) :-
    '$goal_type'(Goal, Type).

'$goal_type'(Goal, Type) :-
    (   '$load_goal'(Goal)
    ->  Type = load
    ;   Type = call
    ).

'$load_goal'([_|_]).
'$load_goal'(consult(_)).
'$load_goal'(load_files(_)).
'$load_goal'(load_files(_,Options)) :-
    memberchk(qcompile(QlfMode), Options),
    '$qlf_part_mode'(QlfMode).
'$load_goal'(ensure_loaded(_)) :- '$compilation_mode'(wic).
'$load_goal'(use_module(_))    :- '$compilation_mode'(wic).
'$load_goal'(use_module(_, _)) :- '$compilation_mode'(wic).

'$qlf_part_mode'(part).
'$qlf_part_mode'(true).                 % compatibility


                /********************************
                *        COMPILE A CLAUSE       *
                *********************************/

%!  '$store_admin_clause'(+Clause, ?Layout, +Owner, +SrcLoc) is det.
%
%   Store a clause into the   database  for administrative purposes.
%   This bypasses sanity checking.

'$store_admin_clause'(Clause, Layout, Owner, SrcLoc) :-
    Owner \== (-),
    !,
    setup_call_cleanup(
        '$start_aux'(Owner, Context),
        '$store_admin_clause2'(Clause, Layout, Owner, SrcLoc),
        '$end_aux'(Owner, Context)).
'$store_admin_clause'(Clause, Layout, File, SrcLoc) :-
    '$store_admin_clause2'(Clause, Layout, File, SrcLoc).

'$store_admin_clause2'(Clause, _Layout, File, SrcLoc) :-
    (   '$compilation_mode'(database)
    ->  '$record_clause'(Clause, File, SrcLoc)
    ;   '$record_clause'(Clause, File, SrcLoc, Ref),
        '$qlf_assert_clause'(Ref, development)
    ).

%!  '$store_clause'(+Clause, ?Layout, +Owner, +SrcLoc) is det.
%
%   Store a clause into the database.
%
%   @arg    Owner is the file-id that owns the clause
%   @arg    SrcLoc is the file:line term where the clause
%           originates from.

'$store_clause'((_, _), _, _, _) :-
    !,
    print_message(error, cannot_redefine_comma),
    fail.
'$store_clause'(Clause, _Layout, File, SrcLoc) :-
    '$valid_clause'(Clause),
    !,
    (   '$compilation_mode'(database)
    ->  '$record_clause'(Clause, File, SrcLoc)
    ;   '$record_clause'(Clause, File, SrcLoc, Ref),
        '$qlf_assert_clause'(Ref, development)
    ).

'$valid_clause'(_) :-
    current_prolog_flag(sandboxed_load, false),
    !.
'$valid_clause'(Clause) :-
    \+ '$cross_module_clause'(Clause),
    !.
'$valid_clause'(Clause) :-
    Error = error(Formal, _),
    catch(prolog:sandbox_allowed_clause(Clause), Error, true),
    !,
    (   var(Formal)
    ->  true
    ;   print_message(error, Error),
        fail
    ).
'$valid_clause'(Clause) :-
    print_message(error,
                  error(permission_error(assert,
                                         sandboxed_clause,
                                         Clause), _)),
    fail.

'$cross_module_clause'(Clause) :-
    '$head_module'(Clause, Module),
    \+ '$current_source_module'(Module).

'$head_module'(Var, _) :-
    var(Var), !, fail.
'$head_module'((Head :- _), Module) :-
    '$head_module'(Head, Module).
'$head_module'(Module:_, Module).

'$clause_source'('$source_location'(File,Line):Clause, Clause, File:Line) :- !.
'$clause_source'(Clause, Clause, -).

%!  '$store_clause'(+Term, +Id) is det.
%
%   This interface is used by PlDoc (and who knows).  Kept for to avoid
%   compatibility issues.

:- public
    '$store_clause'/2.

'$store_clause'(Term, Id) :-
    '$clause_source'(Term, Clause, SrcLoc),
    '$store_clause'(Clause, _, Id, SrcLoc).

%!  compile_aux_clauses(+Clauses) is det.
%
%   Compile clauses given the current  source   location  but do not
%   change  the  notion  of   the    current   procedure  such  that
%   discontiguous  warnings  are  not  issued.    The   clauses  are
%   associated with the current file and  therefore wiped out if the
%   file is reloaded.
%
%   If the cross-referencer is active, we should not (re-)assert the
%   clauses.  Actually,  we  should   make    them   known   to  the
%   cross-referencer. How do we do that?   Maybe we need a different
%   API, such as in:
%
%     ==
%     expand_term_aux(Goal, NewGoal, Clauses)
%     ==
%
%   @tbd    Deal with source code layout?

compile_aux_clauses(_Clauses) :-
    current_prolog_flag(xref, true),
    !.
compile_aux_clauses(Clauses) :-
    source_location(File, _Line),
    '$compile_aux_clauses'(Clauses, File).

'$compile_aux_clauses'(Clauses, File) :-
    setup_call_cleanup(
        '$start_aux'(File, Context),
        '$store_aux_clauses'(Clauses, File),
        '$end_aux'(File, Context)).

'$store_aux_clauses'(Clauses, File) :-
    is_list(Clauses),
    !,
    forall('$member'(C,Clauses),
           '$compile_term'(C, _Layout, File)).
'$store_aux_clauses'(Clause, File) :-
    '$compile_term'(Clause, _Layout, File).


		 /*******************************
		 *            STAGING		*
		 *******************************/

%!  '$stage_file'(+Target, -Stage) is det.
%!  '$install_staged_file'(+Catcher, +Staged, +Target, +OnError).
%
%   Create files using _staging_, where we  first write a temporary file
%   and move it to Target if  the   file  was created successfully. This
%   provides an atomic transition, preventing  customers from reading an
%   incomplete file.

'$stage_file'(Target, Stage) :-
    file_directory_name(Target, Dir),
    file_base_name(Target, File),
    current_prolog_flag(pid, Pid),
    format(atom(Stage), '~w/.~w.~d', [Dir,File,Pid]).

'$install_staged_file'(exit, Staged, Target, error) :-
    !,
    rename_file(Staged, Target).
'$install_staged_file'(exit, Staged, Target, OnError) :-
    !,
    InstallError = error(_,_),
    catch(rename_file(Staged, Target),
          InstallError,
          '$install_staged_error'(OnError, InstallError, Staged, Target)).
'$install_staged_file'(_, Staged, _, _OnError) :-
    E = error(_,_),
    catch(delete_file(Staged), E, true).

'$install_staged_error'(OnError, Error, Staged, _Target) :-
    E = error(_,_),
    catch(delete_file(Staged), E, true),
    (   OnError = silent
    ->  true
    ;   OnError = fail
    ->  fail
    ;   print_message(warning, Error)
    ).


                 /*******************************
                 *             READING          *
                 *******************************/

:- multifile
    prolog:comment_hook/3.                  % hook for read_clause/3


                 /*******************************
                 *       FOREIGN INTERFACE      *
                 *******************************/

%       call-back from PL_register_foreign().  First argument is the module
%       into which the foreign predicate is loaded and second is a term
%       describing the arguments.

:- dynamic
    '$foreign_registered'/2.

                 /*******************************
                 *   TEMPORARY TERM EXPANSION   *
                 *******************************/

% Provide temporary definitions for the boot-loader.  These are replaced
% by the real thing in load.pl

:- dynamic
    '$expand_goal'/2,
    '$expand_term'/4.

'$expand_goal'(In, In).
'$expand_term'(In, Layout, In, Layout).


                /********************************
                *     SAVED STATE GENERATION    *
                *********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This entry point is called from pl-main.c  if the -c option (compile) is
given. It compiles all files and finally calls qsave_program to create a
saved state.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- public '$compile_wic'/0.

'$compile_wic' :-
    use_module(user:library(qsave), [qsave_program/2]),
    current_prolog_flag(os_argv, Argv),
    '$qsave_options'(Argv, Files, Options),
    '$cmd_option_val'(compileout, Out),
    user:consult(Files),
    user:qsave_program(Out, Options).

'$qsave_options'([], [], []).
'$qsave_options'([--|_], [], []) :-
    !.
'$qsave_options'(['-c'|T0], Files, Options) :-
    !,
    '$argv_files'(T0, T1, Files, FilesT),
    '$qsave_options'(T1, FilesT, Options).
'$qsave_options'([O|T0], Files, [Option|T]) :-
    string_concat("--", Opt, O),
    split_string(Opt, "=", "", [NameS|Rest]),
    atom_string(Name, NameS),
    '$qsave_option'(Name, OptName, Rest, Value),
    !,
    Option =.. [OptName, Value],
    '$qsave_options'(T0, Files, T).
'$qsave_options'([_|T0], Files, T) :-
    '$qsave_options'(T0, Files, T).

'$argv_files'([], [], Files, Files).
'$argv_files'([H|T], [H|T], Files, Files) :-
    sub_atom(H, 0, _, _, -),
    !.
'$argv_files'([H|T0], T, [H|Files0], Files) :-
    '$argv_files'(T0, T, Files0, Files).

%!  '$qsave_option'(+Name, +ValueStrings, -Value) is semidet.

'$qsave_option'(Name, Name, [], true) :-
    qsave:save_option(Name, boolean, _),
    !.
'$qsave_option'(NoName, Name, [], false) :-
    atom_concat('no-', Name, NoName),
    qsave:save_option(Name, boolean, _),
    !.
'$qsave_option'(Name, Name, ValueStrings, Value) :-
    qsave:save_option(Name, Type, _),
    !,
    atomics_to_string(ValueStrings, "=", ValueString),
    '$convert_option_value'(Type, ValueString, Value).
'$qsave_option'(Name, Name, _Chars, _Value) :-
    '$existence_error'(save_option, Name).

'$convert_option_value'(integer, String, Value) :-
    (   number_string(Value, String)
    ->  true
    ;   sub_string(String, 0, _, 1, SubString),
        sub_string(String, _, 1, 0, Suffix0),
        downcase_atom(Suffix0, Suffix),
        number_string(Number, SubString),
        '$suffix_multiplier'(Suffix, Multiplier)
    ->  Value is Number * Multiplier
    ;   '$domain_error'(integer, String)
    ).
'$convert_option_value'(callable, String, Value) :-
    term_string(Value, String).
'$convert_option_value'(atom, String, Value) :-
    atom_string(Value, String).
'$convert_option_value'(boolean, String, Value) :-
    atom_string(Value, String).
'$convert_option_value'(oneof(_), String, Value) :-
    atom_string(Value, String).
'$convert_option_value'(ground, String, Value) :-
    atom_string(Value, String).

'$suffix_multiplier'(b, 1).
'$suffix_multiplier'(k, 1024).
'$suffix_multiplier'(m, 1024 * 1024).
'$suffix_multiplier'(g, 1024 * 1024 * 1024).


                 /*******************************
                 *         TYPE SUPPORT         *
                 *******************************/

'$type_error'(Type, Value) :-
    (   var(Value)
    ->  throw(error(instantiation_error, _))
    ;   throw(error(type_error(Type, Value), _))
    ).

'$domain_error'(Type, Value) :-
    throw(error(domain_error(Type, Value), _)).

'$existence_error'(Type, Object) :-
    throw(error(existence_error(Type, Object), _)).

'$permission_error'(Action, Type, Term) :-
    throw(error(permission_error(Action, Type, Term), _)).

'$instantiation_error'(_Var) :-
    throw(error(instantiation_error, _)).

'$uninstantiation_error'(NonVar) :-
    throw(error(uninstantiation_error(NonVar), _)).

'$must_be'(list, X) :- !,
    '$skip_list'(_, X, Tail),
    (   Tail == []
    ->  true
    ;   '$type_error'(list, Tail)
    ).
'$must_be'(options, X) :- !,
    (   '$is_options'(X)
    ->  true
    ;   '$type_error'(options, X)
    ).
'$must_be'(atom, X) :- !,
    (   atom(X)
    ->  true
    ;   '$type_error'(atom, X)
    ).
'$must_be'(integer, X) :- !,
    (   integer(X)
    ->  true
    ;   '$type_error'(integer, X)
    ).
'$must_be'(callable, X) :- !,
    (   callable(X)
    ->  true
    ;   '$type_error'(callable, X)
    ).
'$must_be'(oneof(Type, Domain, List), X) :- !,
    '$must_be'(Type, X),
    (   memberchk(X, List)
    ->  true
    ;   '$domain_error'(Domain, X)
    ).
'$must_be'(boolean, X) :- !,
    (   (X == true ; X == false)
    ->  true
    ;   '$type_error'(boolean, X)
    ).
% Use for debugging
%'$must_be'(Type, _X) :- format('Unknown $must_be type: ~q~n', [Type]).


                /********************************
                *       LIST PROCESSING         *
                *********************************/

'$member'(El, [H|T]) :-
    '$member_'(T, El, H).

'$member_'(_, El, El).
'$member_'([H|T], El, _) :-
    '$member_'(T, El, H).


'$append'([], L, L).
'$append'([H|T], L, [H|R]) :-
    '$append'(T, L, R).

'$select'(X, [X|Tail], Tail).
'$select'(Elem, [Head|Tail], [Head|Rest]) :-
    '$select'(Elem, Tail, Rest).

'$reverse'(L1, L2) :-
    '$reverse'(L1, [], L2).

'$reverse'([], List, List).
'$reverse'([Head|List1], List2, List3) :-
    '$reverse'(List1, [Head|List2], List3).

'$delete'([], _, []) :- !.
'$delete'([Elem|Tail], Elem, Result) :-
    !,
    '$delete'(Tail, Elem, Result).
'$delete'([Head|Tail], Elem, [Head|Rest]) :-
    '$delete'(Tail, Elem, Rest).

'$last'([H|T], Last) :-
    '$last'(T, H, Last).

'$last'([], Last, Last).
'$last'([H|T], _, Last) :-
    '$last'(T, H, Last).


%!  length(?List, ?N)
%
%   Is true when N is the length of List.

:- '$iso'((length/2)).

length(List, Length) :-
    var(Length),
    !,
    '$skip_list'(Length0, List, Tail),
    (   Tail == []
    ->  Length = Length0                    % +,-
    ;   var(Tail)
    ->  Tail \== Length,                    % avoid length(L,L)
        '$length3'(Tail, Length, Length0)   % -,-
    ;   throw(error(type_error(list, List),
                    context(length/2, _)))
    ).
length(List, Length) :-
    integer(Length),
    Length >= 0,
    !,
    '$skip_list'(Length0, List, Tail),
    (   Tail == []                          % proper list
    ->  Length = Length0
    ;   var(Tail)
    ->  Extra is Length-Length0,
        '$length'(Tail, Extra)
    ;   throw(error(type_error(list, List),
                    context(length/2, _)))
    ).
length(_, Length) :-
    integer(Length),
    !,
    throw(error(domain_error(not_less_than_zero, Length),
                context(length/2, _))).
length(_, Length) :-
    throw(error(type_error(integer, Length),
                context(length/2, _))).

'$length3'([], N, N).
'$length3'([_|List], N, N0) :-
    N1 is N0+1,
    '$length3'(List, N, N1).


                 /*******************************
                 *       OPTION PROCESSING      *
                 *******************************/

%!  '$is_options'(@Term) is semidet.
%
%   True if Term looks like it provides options.

'$is_options'(Map) :-
    is_dict(Map, _),
    !.
'$is_options'(List) :-
    is_list(List),
    (   List == []
    ->  true
    ;   List = [H|_],
        '$is_option'(H, _, _)
    ).

'$is_option'(Var, _, _) :-
    var(Var), !, fail.
'$is_option'(F, Name, Value) :-
    functor(F, _, 1),
    !,
    F =.. [Name,Value].
'$is_option'(Name=Value, Name, Value).

%!  '$option'(?Opt, +Options) is semidet.

'$option'(Opt, Options) :-
    is_dict(Options),
    !,
    [Opt] :< Options.
'$option'(Opt, Options) :-
    memberchk(Opt, Options).

%!  '$option'(?Opt, +Options, +Default) is det.

'$option'(Term, Options, Default) :-
    arg(1, Term, Value),
    functor(Term, Name, 1),
    (   is_dict(Options)
    ->  (   get_dict(Name, Options, GVal)
        ->  Value = GVal
        ;   Value = Default
        )
    ;   functor(Gen, Name, 1),
        arg(1, Gen, GVal),
        (   memberchk(Gen, Options)
        ->  Value = GVal
        ;   Value = Default
        )
    ).

%!  '$select_option'(?Opt, +Options, -Rest) is semidet.
%
%   Select an option from Options.
%
%   @arg Rest is always a map.

'$select_option'(Opt, Options, Rest) :-
    select_dict([Opt], Options, Rest).

%!  '$merge_options'(+New, +Default, -Merged) is det.
%
%   Add/replace options specified in New.
%
%   @arg Merged is always a map.

'$merge_options'(New, Old, Merged) :-
    put_dict(New, Old, Merged).


                 /*******************************
                 *   HANDLE TRACER 'L'-COMMAND  *
                 *******************************/

:- public '$prolog_list_goal'/1.

:- multifile
    user:prolog_list_goal/1.

'$prolog_list_goal'(Goal) :-
    user:prolog_list_goal(Goal),
    !.
'$prolog_list_goal'(Goal) :-
    user:listing(Goal).


                 /*******************************
                 *             HALT             *
                 *******************************/

:- '$iso'((halt/0)).

halt :-
    halt(0).


%!  at_halt(:Goal)
%
%   Register Goal to be called if the system halts.
%
%   @tbd: get location into the error message

:- meta_predicate at_halt(0).
:- dynamic        system:term_expansion/2, '$at_halt'/2.
:- multifile      system:term_expansion/2, '$at_halt'/2.

system:term_expansion((:- at_halt(Goal)),
                      system:'$at_halt'(Module:Goal, File:Line)) :-
    \+ current_prolog_flag(xref, true),
    source_location(File, Line),
    '$current_source_module'(Module).

at_halt(Goal) :-
    asserta('$at_halt'(Goal, (-):0)).

:- public '$run_at_halt'/0.

'$run_at_halt' :-
    forall(clause('$at_halt'(Goal, Src), true, Ref),
           ( '$call_at_halt'(Goal, Src),
             erase(Ref)
           )).

'$call_at_halt'(Goal, _Src) :-
    catch(Goal, E, true),
    !,
    (   var(E)
    ->  true
    ;   subsumes_term(cancel_halt(_), E)
    ->  '$print_message'(informational, E),
        fail
    ;   '$print_message'(error, E)
    ).
'$call_at_halt'(Goal, _Src) :-
    '$print_message'(warning, goal_failed(at_halt, Goal)).

%!  cancel_halt(+Reason)
%
%   This predicate may be called from   at_halt/1 handlers to cancel
%   halting the program. If  causes  halt/0   to  fail  rather  than
%   terminating the process.

cancel_halt(Reason) :-
    throw(cancel_halt(Reason)).


                /********************************
                *      LOAD OTHER MODULES       *
                *********************************/

:- meta_predicate
    '$load_wic_files'(:).

'$load_wic_files'(Files) :-
    Files = Module:_,
    '$execute_directive'('$set_source_module'(OldM, Module), []),
    '$save_lex_state'(LexState, []),
    '$style_check'(_, 0xC7),                % see style_name/2 in syspred.pl
    '$compilation_mode'(OldC, wic),
    consult(Files),
    '$execute_directive'('$set_source_module'(OldM), []),
    '$execute_directive'('$restore_lex_state'(LexState), []),
    '$set_compilation_mode'(OldC).


%!  '$load_additional_boot_files' is det.
%
%   Called from compileFileList() in pl-wic.c.   Gets the files from
%   "-c file ..." and loads them into the module user.

:- public '$load_additional_boot_files'/0.

'$load_additional_boot_files' :-
    current_prolog_flag(argv, Argv),
    '$get_files_argv'(Argv, Files),
    (   Files \== []
    ->  format('Loading additional boot files~n'),
        '$load_wic_files'(user:Files),
        format('additional boot files loaded~n')
    ;   true
    ).

'$get_files_argv'([], []) :- !.
'$get_files_argv'(['-c'|Files], Files) :- !.
'$get_files_argv'([_|Rest], Files) :-
    '$get_files_argv'(Rest, Files).

'$:-'(('$boot_message'('Loading Prolog startup files~n', []),
       source_location(File, _Line),
       file_directory_name(File, Dir),
       atom_concat(Dir, '/load.pl', LoadFile),
       '$load_wic_files'(system:[LoadFile]),
       (   current_prolog_flag(windows, true)
       ->  atom_concat(Dir, '/menu.pl', MenuFile),
           '$load_wic_files'(system:[MenuFile])
       ;   true
       ),
       '$boot_message'('SWI-Prolog boot files loaded~n', []),
       '$compilation_mode'(OldC, wic),
       '$execute_directive'('$set_source_module'(user), []),
       '$set_compilation_mode'(OldC)
      )).
