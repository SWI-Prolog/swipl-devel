/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2023, University of Amsterdam
                              VU University Amsterdam
                              CWI, Amsterdam
                              SWI-Prolog Solutions b.v.
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

:- module(check,
        [ check/0,                      % run all checks
          list_undefined/0,             % list undefined predicates
          list_undefined/1,             % +Options
          list_autoload/0,              % list predicates that need autoloading
          list_redefined/0,             % list redefinitions
          list_cross_module_calls/0,	% List Module:Goal usage
          list_cross_module_calls/1,    % +Options
          list_void_declarations/0,     % list declarations with no clauses
          list_trivial_fails/0,         % list goals that trivially fail
          list_trivial_fails/1,         % +Options
          list_format_errors/0,         % list calls to format with wrong args
          list_format_errors/1,		% +Options
          list_strings/0,               % list string objects in clauses
          list_strings/1,               % +Options
          list_rationals/0,		% list rational objects in clauses
          list_rationals/1              % +Options
        ]).
:- autoload(library(apply),[maplist/2]).
:- autoload(library(lists),[member/2,append/3]).
:- autoload(library(occurs),[sub_term/2]).
:- autoload(library(option),[merge_options/3,option/3]).
:- autoload(library(pairs),
	    [group_pairs_by_key/2,map_list_to_pairs/3,pairs_values/2]).
:- autoload(library(prolog_clause),
	    [clause_info/4,predicate_name/2,clause_name/2]).
:- autoload(library(prolog_code),[pi_head/2]).
:- autoload(library(prolog_codewalk),
	    [prolog_walk_code/1,prolog_program_clause/2]).
:- autoload(library(prolog_format),[format_types/2]).
:- autoload(library(predicate_options), [check_predicate_options/0]).

:- set_prolog_flag(generate_debug_info, false).

:- multifile
       trivial_fail_goal/1,
       string_predicate/1,
       valid_string_goal/1,
       checker/2.

:- dynamic checker/2.


/** <module> Consistency checking

This library provides some consistency  checks   for  the  loaded Prolog
program. The predicate make/0 runs   list_undefined/0  to find undefined
predicates in `user' modules.

@see    gxref/0 provides a graphical cross referencer
@see    PceEmacs performs real time consistency checks while you edit
@see    library(prolog_xref) implements `offline' cross-referencing
@see    library(prolog_codewalk) implements `online' analysis
*/

:- predicate_options(list_undefined/1, 1,
                     [ module_class(list(oneof([user,library,system])))
                     ]).

%!  check is det.
%
%   Run all consistency checks defined by checker/2. Checks enabled by
%   default are:
%
%     * list_undefined/0 reports undefined predicates
%     * list_trivial_fails/0 reports calls for which there is no
%       matching clause.
%     * list_format_errors/0 reports mismatches in format/2,3
%       templates and the list of arguments.
%     * list_redefined/0 reports predicates that have a local
%       definition and a global definition.  Note that these are
%       __not__ errors.
%     * list_void_declarations/0 reports on predicates with defined
%       properties, but no clauses.
%     * list_autoload/0 lists predicates that will be defined at
%       runtime using the autoloader.
%     * check_predicate_options/0 tests for options passed to
%       predicates such as open/4 that are unknown or are used
%       with an invalid argument.
%
%    The checker can be expanded or  restricted by modifying the dynamic
%    multifile hook checker/2.
%
%    The checker may be used in batch, e.g., for CI workflows by calling
%    SWI-Prolog as below. Note that by using ``-l`` to load the program,
%    the program is not started  if   it  used  initialization/2 of type
%    `main` to start the program.
%
%
%    ```
%    swipl -q --on-warning=status --on-error=status \
%          -g check -t halt -l myprogram.pl
%    ```

check :-
    checker(Checker, Message),
    print_message(informational,check(pass(Message))),
    catch(Checker,E,print_message(error,E)),
    fail.
check.

%!  list_undefined is det.
%!  list_undefined(+Options) is det.
%
%   Report undefined predicates.  This   predicate  finds  undefined
%   predicates by decompiling and analyzing the body of all clauses.
%   Options:
%
%       * module_class(+Classes)
%       Process modules of the given Classes.  The default for
%       classes is =|[user]|=. For example, to include the
%       libraries into the examination, use =|[user,library]|=.
%
%   @see gxref/0 provides a graphical cross-referencer.
%   @see make/0 calls list_undefined/0

:- thread_local
    undef/2.

list_undefined :-
    list_undefined([]).

list_undefined(Options) :-
    merge_options(Options,
                  [ module_class([user])
                  ],
                  WalkOptions),
    call_cleanup(
        prolog_walk_code([ undefined(trace),
                           on_trace(found_undef)
                         | WalkOptions
                         ]),
        collect_undef(Grouped)),
    (   Grouped == []
    ->  true
    ;   print_message(warning, check(undefined_procedures, Grouped))
    ).

% The following predicates are used from library(prolog_autoload).

:- public
    found_undef/3,
    collect_undef/1.

collect_undef(Grouped) :-
    findall(PI-From, retract(undef(PI, From)), Pairs),
    keysort(Pairs, Sorted),
    group_pairs_by_key(Sorted, Grouped).

found_undef(To, _Caller, From) :-
    goal_pi(To, PI),
    (   undef(PI, From)
    ->  true
    ;   compiled(PI)
    ->  true
    ;   not_always_present(PI)
    ->  true
    ;   assertz(undef(PI,From))
    ).

compiled(system:'$call_cleanup'/0).     % compiled to VM instructions
compiled(system:'$catch'/0).
compiled(system:'$cut'/0).
compiled(system:'$reset'/0).
compiled(system:'$call_continuation'/1).
compiled(system:'$shift'/1).
compiled(system:'$shift_for_copy'/1).
compiled('$engines':'$yield'/0).

%!  not_always_present(+PI) is semidet.
%
%   True when some predicate is known to be part of the state but is not
%   available in this version.

not_always_present(_:win_folder/2) :-
    \+ current_prolog_flag(windows, true).
not_always_present(_:win_add_dll_directory/2) :-
    \+ current_prolog_flag(windows, true).
not_always_present(_:opt_help/2).
not_always_present(_:opt_type/3).
not_always_present(_:opt_meta/2).

goal_pi(M:Head, M:Name/Arity) :-
    functor(Head, Name, Arity).

%!  list_autoload is det.
%
%   Report predicates that may be  auto-loaded. These are predicates
%   that  are  not  defined,  but  will   be  loaded  on  demand  if
%   referenced.
%
%   @tbd    This predicate uses an older mechanism for finding
%           undefined predicates.  Should be synchronized with
%           list undefined.
%   @see    autoload/0

list_autoload :-
    setup_call_cleanup(
        ( current_prolog_flag(access_level, OldLevel),
          current_prolog_flag(autoload, OldAutoLoad),
          set_prolog_flag(access_level, system),
          set_prolog_flag(autoload, false)
        ),
        list_autoload_(OldLevel),
        ( set_prolog_flag(access_level, OldLevel),
          set_prolog_flag(autoload, OldAutoLoad)
        )).

list_autoload_(SystemMode) :-
    (   setof(Lib-Pred,
              autoload_predicate(Module, Lib, Pred, SystemMode),
              Pairs),
        print_message(informational,
                      check(autoload(Module, Pairs))),
        fail
    ;   true
    ).

autoload_predicate(Module, Library, Name/Arity, SystemMode) :-
    predicate_property(Module:Head, undefined),
    check_module_enabled(Module, SystemMode),
    (   \+ predicate_property(Module:Head, imported_from(_)),
        functor(Head, Name, Arity),
        '$find_library'(Module, Name, Arity, _LoadModule, Library),
        referenced(Module:Head, Module, _)
    ->  true
    ).

check_module_enabled(_, system) :- !.
check_module_enabled(Module, _) :-
    \+ import_module(Module, system).

%!  referenced(+Predicate, ?Module, -ClauseRef) is nondet.
%
%   True if clause ClauseRef references Predicate.

referenced(Term, Module, Ref) :-
    Goal = Module:_Head,
    current_predicate(_, Goal),
    '$get_predicate_attribute'(Goal, system, 0),
    \+ '$get_predicate_attribute'(Goal, imported, _),
    nth_clause(Goal, _, Ref),
    '$xr_member'(Ref, Term).

%!  list_redefined
%
%   Lists predicates that are defined in the global module =user= as
%   well as in a normal module; that   is,  predicates for which the
%   local definition overrules the global default definition.

list_redefined :-
    setup_call_cleanup(
        ( current_prolog_flag(access_level, OldLevel),
          set_prolog_flag(access_level, system)
        ),
        list_redefined_,
        set_prolog_flag(access_level, OldLevel)).

list_redefined_ :-
    current_module(Module),
    Module \== system,
    current_predicate(_, Module:Head),
    \+ predicate_property(Module:Head, imported_from(_)),
    (   global_module(Super),
        Super \== Module,
        '$c_current_predicate'(_, Super:Head),
        \+ redefined_ok(Head),
        '$syspreds':'$defined_predicate'(Super:Head),
        \+ predicate_property(Super:Head, (dynamic)),
        \+ predicate_property(Super:Head, imported_from(Module)),
        functor(Head, Name, Arity)
    ->  print_message(informational,
                      check(redefined(Module, Super, Name/Arity)))
    ),
    fail.
list_redefined_.

redefined_ok('$mode'(_,_)).
redefined_ok('$pldoc'(_,_,_,_)).
redefined_ok('$pred_option'(_,_,_,_)).
redefined_ok('$table_mode'(_,_,_)).
redefined_ok('$tabled'(_,_)).
redefined_ok('$exported_op'(_,_,_)).
redefined_ok('$autoload'(_,_,_)).

global_module(user).
global_module(system).

%!  list_cross_module_calls is det.
%
%   List calls from one module to   another  using Module:Goal where the
%   callee is not defined exported, public or multifile, i.e., where the
%   callee should be considered _private_.

list_cross_module_calls :-
    list_cross_module_calls([]).

list_cross_module_calls(Options) :-
    call_cleanup(
        list_cross_module_calls_guarded(Options),
        retractall(cross_module_call(_,_,_))).

list_cross_module_calls_guarded(Options) :-
    merge_options(Options,
                  [ module_class([user])
                  ],
                  WalkOptions),
    prolog_walk_code([ trace_reference(_),
                       trace_condition(cross_module_call),
                       on_trace(write_call)
                     | WalkOptions
                     ]).

:- thread_local
    cross_module_call/3.

:- public
    cross_module_call/2,
    write_call/3.

cross_module_call(Callee, Context) :-
    \+ same_module_call(Callee, Context).

same_module_call(Callee, Context) :-
    caller_module(Context, MCaller),
    Callee = (MCallee:_),
    (   (   MCaller = MCallee
        ;   predicate_property(Callee, exported)
        ;   predicate_property(Callee, built_in)
        ;   predicate_property(Callee, public)
        ;   clause_property(Context.get(clause), module(MCallee))
        ;   predicate_property(Callee, multifile)
        )
    ->  true
    ).

caller_module(Context, MCaller) :-
    Caller = Context.caller,
    (   Caller = (MCaller:_)
    ->  true
    ;   Caller == '<initialization>',
        MCaller = Context.module
    ).

write_call(Callee, Caller, Position) :-
    cross_module_call(Callee, Caller, Position),
    !.
write_call(Callee, Caller, Position) :-
    (   cross_module_call(_,_,_)
    ->  true
    ;   print_message(warning, check(cross_module_calls))
    ),
    asserta(cross_module_call(Callee, Caller, Position)),
    print_message(warning,
                  check(cross_module_call(Callee, Caller, Position))).

%!  list_void_declarations is det.
%
%   List predicates that have declared attributes, but no clauses.

list_void_declarations :-
    P = _:_,
    (   predicate_property(P, undefined),
        (   '$get_predicate_attribute'(P, meta_predicate, Pattern),
            (   '$get_predicate_attribute'(P, transparent, 1)
            ->   print_message(warning,
                               check(void_declaration(P, meta_predicate(Pattern))))
            ;    print_message(warning,
                               check(void_declaration(P, mode(Pattern))))
            )
        ;   void_attribute(Attr),
            '$get_predicate_attribute'(P, Attr, 1),
            print_message(warning,
                          check(void_declaration(P, Attr)))
        ),
        fail
    ;   predicate_property(P, discontiguous),
        \+ (predicate_property(P, number_of_clauses(N)), N > 0),
        print_message(warning,
                      check(void_declaration(P, discontiguous))),
        fail
    ;   true
    ).

void_attribute(public).
void_attribute(volatile).
void_attribute(det).

%!  list_trivial_fails is det.
%!  list_trivial_fails(+Options) is det.
%
%   List goals that trivially fail  because   there  is  no matching
%   clause.  Options:
%
%     * module_class(+Classes)
%       Process modules of the given Classes.  The default for
%       classes is =|[user]|=. For example, to include the
%       libraries into the examination, use =|[user,library]|=.

:- thread_local
    trivial_fail/2.

list_trivial_fails :-
    list_trivial_fails([]).

list_trivial_fails(Options) :-
    merge_options(Options,
                  [ module_class([user]),
                    infer_meta_predicates(false),
                    autoload(false),
                    evaluate(false),
                    trace_reference(_),
                    on_trace(check_trivial_fail)
                  ],
                  WalkOptions),

    prolog_walk_code([ source(false)
                     | WalkOptions
                     ]),
    findall(CRef, retract(trivial_fail(clause(CRef), _)), Clauses),
    (   Clauses == []
    ->  true
    ;   print_message(warning, check(trivial_failures)),
        prolog_walk_code([ clauses(Clauses)
                         | WalkOptions
                         ]),
        findall(Goal-From, retract(trivial_fail(From, Goal)), Pairs),
        keysort(Pairs, Sorted),
        group_pairs_by_key(Sorted, Grouped),
        maplist(report_trivial_fail, Grouped)
    ).

%!  trivial_fail_goal(:Goal)
%
%   Multifile hook that tells list_trivial_fails/0 to accept Goal as
%   valid.

trivial_fail_goal(pce_expansion:pce_class(_, _, template, _, _, _)).
trivial_fail_goal(pce_host:property(system_source_prefix(_))).

:- public
    check_trivial_fail/3.

check_trivial_fail(MGoal0, _Caller, From) :-
    (   MGoal0 = M:Goal,
        atom(M),
        callable(Goal),
        predicate_property(MGoal0, interpreted),
        \+ predicate_property(MGoal0, dynamic),
        \+ predicate_property(MGoal0, multifile),
        \+ trivial_fail_goal(MGoal0)
    ->  (   predicate_property(MGoal0, meta_predicate(Meta))
        ->  qualify_meta_goal(MGoal0, Meta, MGoal)
        ;   MGoal = MGoal0
        ),
        (   clause(MGoal, _)
        ->  true
        ;   assertz(trivial_fail(From, MGoal))
        )
    ;   true
    ).

report_trivial_fail(Goal-FromList) :-
    print_message(warning, check(trivial_failure(Goal, FromList))).

%!  qualify_meta_goal(+Module, +MetaSpec, +Goal, -QualifiedGoal)
%
%   Qualify a goal if the goal calls a meta predicate

qualify_meta_goal(M:Goal0, Meta, M:Goal) :-
    functor(Goal0, F, N),
    functor(Goal, F, N),
    qualify_meta_goal(1, M, Meta, Goal0, Goal).

qualify_meta_goal(N, M, Meta, Goal0, Goal) :-
    arg(N, Meta,  ArgM),
    !,
    arg(N, Goal0, Arg0),
    arg(N, Goal,  Arg),
    N1 is N + 1,
    (   module_qualified(ArgM)
    ->  add_module(Arg0, M, Arg)
    ;   Arg = Arg0
    ),
    meta_goal(N1, Meta, Goal0, Goal).
meta_goal(_, _, _, _).

add_module(Arg, M, M:Arg) :-
    var(Arg),
    !.
add_module(M:Arg, _, MArg) :-
    !,
    add_module(Arg, M, MArg).
add_module(Arg, M, M:Arg).

module_qualified(N) :- integer(N), !.
module_qualified(:).
module_qualified(^).


%!  list_strings is det.
%!  list_strings(+Options) is det.
%
%   List strings that appear in clauses.   This predicate is used to
%   find  portability  issues  for   changing    the   Prolog   flag
%   =double_quotes= from =codes= to =string=, creating packed string
%   objects.  Warnings  may  be  suppressed    using  the  following
%   multifile hooks:
%
%     - string_predicate/1 to stop checking certain predicates
%     - valid_string_goal/1 to tell the checker that a goal is
%       safe.
%
%   @see Prolog flag =double_quotes=.

list_strings :-
    list_strings([module_class([user])]).

list_strings(Options) :-
    (   prolog_program_clause(ClauseRef, Options),
        clause(Head, Body, ClauseRef),
        \+ ( predicate_indicator(Head, PI),
             string_predicate(PI)
           ),
        make_clause(Head, Body, Clause),
        findall(T,
                (   sub_term(T, Head),
                    string(T)
                ;   Head = M:_,
                    goal_in_body(Goal, M, Body),
                    (   valid_string_goal(Goal)
                    ->  fail
                    ;   sub_term(T, Goal),
                        string(T)
                    )
                ), Ts0),
        sort(Ts0, Ts),
        member(T, Ts),
        message_context(ClauseRef, T, Clause, Context),
        print_message(warning,
                      check(string_in_clause(T, Context))),
        fail
    ;   true
    ).

make_clause(Head, true, Head) :- !.
make_clause(Head, Body, (Head:-Body)).

%!  list_rationals is det.
%!  list_rationals(+Options) is det.
%
%   List rational numbers that appear in clauses. This predicate is used
%   to  find  portability  issues   for    changing   the   Prolog  flag
%   `rational_syntax`  to  `natural`,  creating  rational  numbers  from
%   <integer>/<nonneg>. Options:
%
%      - module_class(+Classes)
%        Determines the modules classes processed.  By default only
%        user code is processed.  See prolog_program_clause/2.
%      - arithmetic(+Bool)
%        If `true` (default `false`) also warn on rationals appearing
%        in arithmetic expressions.
%
%   @see Prolog flag `rational_syntax` and `prefer_rationals`.

list_rationals :-
    list_rationals([module_class([user])]).

list_rationals(Options) :-
    (   option(arithmetic(DoArith), Options, false),
        prolog_program_clause(ClauseRef, Options),
        clause(Head, Body, ClauseRef),
        make_clause(Head, Body, Clause),
        findall(T,
                (   sub_term(T, Head),
                    rational(T),
                    \+ integer(T)
                ;   Head = M:_,
                    goal_in_body(Goal, M, Body),
                    nonvar(Goal),
                    (   DoArith == false,
                        valid_rational_goal(Goal)
                    ->  fail
                    ;   sub_term(T, Goal),
                        rational(T),
                        \+ integer(T)
                    )
                ), Ts0),
        sort(Ts0, Ts),
        member(T, Ts),
        message_context(ClauseRef, T, Clause, Context),
        print_message(warning,
                      check(rational_in_clause(T, Context))),
        fail
    ;   true
    ).


valid_rational_goal(_ is _).
valid_rational_goal(_ =:= _).
valid_rational_goal(_ < _).
valid_rational_goal(_ > _).
valid_rational_goal(_ =< _).
valid_rational_goal(_ >= _).


%!  list_format_errors is det.
%!  list_format_errors(+Options) is det.
%
%   List argument errors for format/2,3.

list_format_errors :-
    list_format_errors([module_class([user])]).

list_format_errors(Options) :-
    (   prolog_program_clause(ClauseRef, Options),
        clause(Head, Body, ClauseRef),
        make_clause(Head, Body, Clause),
        Head = M:_,
        goal_in_body(Goal, M, Body),
        format_warning(Goal, Msg),
        message_context(ClauseRef, Goal, Clause, Context),
        print_message(warning, check(Msg, Goal, Context)),
        fail
    ;   true
    ).

format_warning(system:format(Format, Args), Msg) :-
    nonvar(Format),
    nonvar(Args),
    \+ is_list(Args),
    Msg = format_argv(Args).
format_warning(system:format(Format, Args), Msg) :-
    ground(Format),
    (   is_list(Args)
    ->  length(Args, ArgC)
    ;   nonvar(Args)
    ->  ArgC = 1
    ),
    E = error(Formal,_),
    catch(format_types(Format, Types), E, true),
    (   var(Formal)
    ->  length(Types, TypeC),
        TypeC =\= ArgC,
        Msg = format_argc(TypeC, ArgC)
    ;   Msg = format_template(Formal)
    ).
format_warning(system:format(_Stream, Format, Args), Msg) :-
    format_warning(system:format(Format, Args), Msg).
format_warning(prolog_debug:debug(_Channel, Format, Args), Msg) :-
    format_warning(system:format(Format, Args), Msg).


%!  goal_in_body(-G, +M, +Body) is nondet.
%
%   True when G is a goal called from Body.

goal_in_body(M:G, M, G) :-
    var(G),
    !.
goal_in_body(G, _, M:G0) :-
    atom(M),
    !,
    goal_in_body(G, M, G0).
goal_in_body(G, M, Control) :-
    nonvar(Control),
    control(Control, Subs),
    !,
    member(Sub, Subs),
    goal_in_body(G, M, Sub).
goal_in_body(G, M, G0) :-
    callable(G0),
    (   atom(M)
    ->  TM = M
    ;   TM = system
    ),
    predicate_property(TM:G0, meta_predicate(Spec)),
    !,
    (   strip_goals(G0, Spec, G1),
        simple_goal_in_body(G, M, G1)
    ;   arg(I, Spec, Meta),
        arg(I, G0, G1),
        extend(Meta, G1, G2),
        goal_in_body(G, M, G2)
    ).
goal_in_body(G, M, G0) :-
    simple_goal_in_body(G, M, G0).

simple_goal_in_body(G, M, G0) :-
    (   atom(M),
        callable(G0),
        predicate_property(M:G0, imported_from(M2))
    ->  G = M2:G0
    ;   G = M:G0
    ).

control((A,B), [A,B]).
control((A;B), [A,B]).
control((A->B), [A,B]).
control((A*->B), [A,B]).
control((\+A), [A]).

strip_goals(G0, Spec, G) :-
    functor(G0, Name, Arity),
    functor(G,  Name, Arity),
    strip_goal_args(1, G0, Spec, G).

strip_goal_args(I, G0, Spec, G) :-
    arg(I, G0, A0),
    !,
    arg(I, Spec, M),
    (   extend(M, A0, _)
    ->  arg(I, G, '<meta-goal>')
    ;   arg(I, G, A0)
    ),
    I2 is I + 1,
    strip_goal_args(I2, G0, Spec, G).
strip_goal_args(_, _, _, _).

extend(I, G0, G) :-
    callable(G0),
    integer(I), I>0,
    !,
    length(L, I),
    extend_list(G0, L, G).
extend(0, G, G).
extend(^, G, G).

extend_list(M:G0, L, M:G) :-
    !,
    callable(G0),
    extend_list(G0, L, G).
extend_list(G0, L, G) :-
    G0 =.. List,
    append(List, L, All),
    G =.. All.


%!  message_context(+ClauseRef, +Term, +Clause, -Pos) is det.
%
%   Find an as accurate as possible location for Term in Clause.

message_context(ClauseRef, Term, Clause, file_term_position(File, TermPos)) :-
    clause_info(ClauseRef, File, Layout, _Vars),
    (   Term = _:Goal,
        prolog_codewalk:subterm_pos(Goal, Clause, ==, Layout, TermPos)
    ;   prolog_codewalk:subterm_pos(Term, Clause, ==, Layout, TermPos)
    ),
    !.
message_context(ClauseRef, _String, _Clause, file(File, Line, -1, _)) :-
    clause_property(ClauseRef, file(File)),
    clause_property(ClauseRef, line_count(Line)),
    !.
message_context(ClauseRef, _String, _Clause, clause(ClauseRef)).


:- meta_predicate
    predicate_indicator(:, -).

predicate_indicator(Module:Head, Module:Name/Arity) :-
    functor(Head, Name, Arity).
predicate_indicator(Module:Head, Module:Name//DCGArity) :-
    functor(Head, Name, Arity),
    DCGArity is Arity-2.

%!  string_predicate(:PredicateIndicator)
%
%   Multifile hook to disable list_strings/0 on the given predicate.
%   This is typically used for facts that store strings.

string_predicate(_:'$pldoc'/4).
string_predicate(pce_principal:send_implementation/3).
string_predicate(pce_principal:pce_lazy_get_method/3).
string_predicate(pce_principal:pce_lazy_send_method/3).
string_predicate(pce_principal:pce_class/6).
string_predicate(prolog_xref:pred_comment/4).
string_predicate(prolog_xref:module_comment/3).
string_predicate(pldoc_process:structured_comment//2).
string_predicate(pldoc_process:structured_command_start/3).
string_predicate(pldoc_process:separator_line//0).
string_predicate(pldoc_register:mydoc/3).
string_predicate(http_header:separators/1).

%!  valid_string_goal(+Goal) is semidet.
%
%   Multifile hook that qualifies Goal  as valid for list_strings/0.
%   For example, format("Hello world~n") is considered proper use of
%   string constants.

% system predicates
valid_string_goal(system:format(S)) :- string(S).
valid_string_goal(system:format(S,_)) :- string(S).
valid_string_goal(system:format(_,S,_)) :- string(S).
valid_string_goal(system:string_codes(S,_)) :- string(S).
valid_string_goal(system:string_code(_,S,_)) :- string(S).
valid_string_goal(system:throw(msg(S,_))) :- string(S).
valid_string_goal('$dcg':phrase(S,_,_)) :- string(S).
valid_string_goal('$dcg':phrase(S,_)) :- string(S).
valid_string_goal(system: is(_,_)).     % arithmetic allows for "x"
valid_string_goal(system: =:=(_,_)).
valid_string_goal(system: >(_,_)).
valid_string_goal(system: <(_,_)).
valid_string_goal(system: >=(_,_)).
valid_string_goal(system: =<(_,_)).
% library stuff
valid_string_goal(dcg_basics:string_without(S,_,_,_)) :- string(S).
valid_string_goal(git:read_url(S,_,_)) :- string(S).
valid_string_goal(tipc:tipc_subscribe(_,_,_,_,S)) :- string(S).
valid_string_goal(charsio:format_to_chars(Format,_,_)) :- string(Format).
valid_string_goal(charsio:format_to_chars(Format,_,_,_)) :- string(Format).
valid_string_goal(codesio:format_to_codes(Format,_,_)) :- string(Format).
valid_string_goal(codesio:format_to_codes(Format,_,_,_)) :- string(Format).


                 /*******************************
                 *        EXTENSION HOOKS       *
                 *******************************/

%!  checker(:Goal, +Message:text) is nondet.
%
%   Register code validation routines. Each clause  defines a Goal which
%   performs a consistency check executed by check/0. Message is a short
%   description of the check.  For   example,  assuming  the `my_checks`
%   module defines a predicate list_format_mistakes/0:
%
%      ```
%      :- multifile check:checker/2.
%      check:checker(my_checks:list_format_mistakes,
%                    "errors with format/2 arguments").
%      ```
%
%   The predicate is dynamic, so you  can disable checks with retract/1.
%   For example, to stop reporting redefined predicates:
%
%      ```
%      retract(check:checker(list_redefined,_)).
%      ```

checker(list_undefined,          'undefined predicates').
checker(list_trivial_fails,      'trivial failures').
checker(list_format_errors,      'format/2,3 and debug/3 templates').
checker(list_redefined,          'redefined system and global predicates').
checker(list_void_declarations,  'predicates with declarations but without clauses').
checker(list_autoload,           'predicates that need autoloading').
checker(check_predicate_options, 'predicate options lists').


                 /*******************************
                 *            MESSAGES          *
                 *******************************/

:- multifile
    prolog:message/3.

prolog:message(check(pass(Comment))) -->
    [ 'Checking ~w ...'-[Comment] ].
prolog:message(check(find_references(Preds))) -->
    { length(Preds, N)
    },
    [ 'Scanning for references to ~D possibly undefined predicates'-[N] ].
prolog:message(check(undefined_procedures, Grouped)) -->
    [ 'The predicates below are not defined. If these are defined', nl,
      'at runtime using assert/1, use :- dynamic Name/Arity.', nl, nl
    ],
    undefined_procedures(Grouped).
prolog:message(check(undefined_unreferenced_predicates)) -->
    [ 'The predicates below are not defined, and are not', nl,
      'referenced.', nl, nl
    ].
prolog:message(check(undefined_unreferenced(Pred))) -->
    predicate(Pred).
prolog:message(check(autoload(Module, Pairs))) -->
    { module_property(Module, file(Path))
    },
    !,
    [ 'Into module ~w ('-[Module] ],
    short_filename(Path),
    [ ')', nl ],
    autoload(Pairs).
prolog:message(check(autoload(Module, Pairs))) -->
    [ 'Into module ~w'-[Module], nl ],
    autoload(Pairs).
prolog:message(check(redefined(In, From, Pred))) -->
    predicate(In:Pred),
    redefined(In, From).
prolog:message(check(cross_module_calls)) -->
    [ 'Qualified calls to private predicates'-[] ].
prolog:message(check(cross_module_call(Callee, _Caller, Location))) -->
    { pi_head(PI, Callee) },
    [ '  '-[] ],
    '$messages':swi_location(Location),
    [ 'Cross-module call to ~p'-[PI] ].
prolog:message(check(trivial_failures)) -->
    [ 'The following goals fail because there are no matching clauses.' ].
prolog:message(check(trivial_failure(Goal, Refs))) -->
    { map_list_to_pairs(sort_reference_key, Refs, Keyed),
      keysort(Keyed, KeySorted),
      pairs_values(KeySorted, SortedRefs)
    },
    goal(Goal),
    [ ', which is called from'-[], nl ],
    referenced_by(SortedRefs).
prolog:message(check(string_in_clause(String, Context))) -->
    '$messages':swi_location(Context),
    [ 'String ~q'-[String] ].
prolog:message(check(rational_in_clause(String, Context))) -->
    '$messages':swi_location(Context),
    [ 'Rational ~q'-[String] ].
prolog:message(check(Msg, Goal, Context)) -->
    '$messages':swi_location(Context),
    { pi_head(PI, Goal) },
    [ nl, '    '-[] ],
    predicate(PI),
    [ ': '-[] ],
    check_message(Msg).
prolog:message(check(void_declaration(P, Decl))) -->
    predicate(P),
    [ ' is declared with ', ansi(code, '~p', [Decl]), ' but has no clauses' ].

undefined_procedures([]) -->
    [].
undefined_procedures([H|T]) -->
    undefined_procedure(H),
    undefined_procedures(T).

undefined_procedure(Pred-Refs) -->
    { map_list_to_pairs(sort_reference_key, Refs, Keyed),
      keysort(Keyed, KeySorted),
      pairs_values(KeySorted, SortedRefs)
    },
    predicate(Pred),
    [ ', which is referenced by', nl ],
    referenced_by(SortedRefs).

redefined(user, system) -->
    [ '~t~30| System predicate redefined globally' ].
redefined(_, system) -->
    [ '~t~30| Redefined system predicate' ].
redefined(_, user) -->
    [ '~t~30| Redefined global predicate' ].

goal(user:Goal) -->
    !,
    [ '~p'-[Goal] ].
goal(Goal) -->
    !,
    [ '~p'-[Goal] ].

predicate(Module:Name/Arity) -->
    { atom(Module),
      atom(Name),
      integer(Arity),
      functor(Head, Name, Arity),
      predicate_name(Module:Head, PName)
    },
    !,
    [ ansi(code, '~w', [PName]) ].
predicate(Module:Head) -->
    { atom(Module),
      callable(Head),
      predicate_name(Module:Head, PName)
    },
    !,
    [ ansi(code, '~w', [PName]) ].
predicate(Name/Arity) -->
    { atom(Name),
      integer(Arity)
    },
    !,
    predicate(user:Name/Arity).

autoload([]) -->
    [].
autoload([Lib-Pred|T]) -->
    [ '    ' ],
    predicate(Pred),
    [ '~t~24| from ' ],
    short_filename(Lib),
    [ nl ],
    autoload(T).

%!  sort_reference_key(+Reference, -Key) is det.
%
%   Create a stable key for sorting references to predicates.

sort_reference_key(Term, key(M:Name/Arity, N, ClausePos)) :-
    clause_ref(Term, ClauseRef, ClausePos),
    !,
    nth_clause(Pred, N, ClauseRef),
    strip_module(Pred, M, Head),
    functor(Head, Name, Arity).
sort_reference_key(Term, Term).

clause_ref(clause_term_position(ClauseRef, TermPos), ClauseRef, ClausePos) :-
    arg(1, TermPos, ClausePos).
clause_ref(clause(ClauseRef), ClauseRef, 0).


referenced_by([]) -->
    [].
referenced_by([Ref|T]) -->
    ['\t'], prolog:message_location(Ref),
            predicate_indicator(Ref),
    [ nl ],
    referenced_by(T).

predicate_indicator(clause_term_position(ClauseRef, _)) -->
    { nonvar(ClauseRef) },
    !,
    predicate_indicator(clause(ClauseRef)).
predicate_indicator(clause(ClauseRef)) -->
    { clause_name(ClauseRef, Name) },
    [ '~w'-[Name] ].
predicate_indicator(file_term_position(_,_)) -->
    [ '(initialization)' ].
predicate_indicator(file(_,_,_,_)) -->
    [ '(initialization)' ].


short_filename(Path) -->
    { short_filename(Path, Spec)
    },
    [ '~q'-[Spec] ].

short_filename(Path, Spec) :-
    absolute_file_name('', Here),
    atom_concat(Here, Local0, Path),
    !,
    remove_leading_slash(Local0, Spec).
short_filename(Path, Spec) :-
    findall(LenAlias, aliased_path(Path, LenAlias), Keyed),
    keysort(Keyed, [_-Spec|_]).
short_filename(Path, Path).

aliased_path(Path, Len-Spec) :-
    setof(Alias, Spec^(user:file_search_path(Alias, Spec)), Aliases),
    member(Alias, Aliases),
    Term =.. [Alias, '.'],
    absolute_file_name(Term,
                       [ file_type(directory),
                         file_errors(fail),
                         solutions(all)
                       ], Prefix),
    atom_concat(Prefix, Local0, Path),
    remove_leading_slash(Local0, Local),
    atom_length(Local, Len),
    Spec =.. [Alias, Local].

remove_leading_slash(Path, Local) :-
    atom_concat(/, Local, Path),
    !.
remove_leading_slash(Path, Path).

check_message(format_argc(Expected, InList)) -->
    [ 'Template requires ~w arguments, got ~w'-[Expected, InList] ].
check_message(format_template(Formal)) -->
    { message_to_string(error(Formal, _), Msg) },
    [ 'Invalid template: ~s'-[Msg] ].
check_message(format_argv(Args)) -->
    [ 'Arguments are not in a list (deprecated): ~p'-[Args] ].
