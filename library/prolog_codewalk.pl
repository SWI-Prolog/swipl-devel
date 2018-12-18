/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2012-2016, VU University Amsterdam
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

:- module(prolog_codewalk,
          [ prolog_walk_code/1,         % +Options
            prolog_program_clause/2     % -ClauseRef, +Options
          ]).
:- use_module(library(option)).
:- use_module(library(record)).
:- use_module(library(debug)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(prolog_metainference)).

/** <module> Prolog code walker

This module walks over  the  loaded   program,  searching  for  callable
predicates. It started as part of  library(prolog_autoload) and has been
turned into a seperate module to  facilitate operations that require the
same reachability analysis, such as finding   references to a predicate,
finding unreachable code, etc.

For example, the following  determins  the   call  graph  of  the loaded
program. By using source(true), The exact location   of  the call in the
source file is passed into _Where.

  ==
  :- dynamic
          calls/2.

  assert_call_graph :-
          retractall(calls(_, _)),
          prolog_walk_code([ trace_reference(_),
                             on_trace(assert_edge),
                             source(false)
                           ]),
          predicate_property(calls(_,_), number_of_clauses(N)),
          format('Got ~D edges~n', [N]).

  assert_edge(Callee, Caller, _Where) :-
          calls(Caller, Callee), !.
  assert_edge(Callee, Caller, _Where) :-
          assertz(calls(Caller, Callee)).
  ==
*/

:- meta_predicate
    prolog_walk_code(:).

:- multifile
    prolog:called_by/4,
    prolog:called_by/2.

:- predicate_options(prolog_walk_code/1, 1,
                     [ undefined(oneof([ignore,error,trace])),
                       autoload(boolean),
                       clauses(list),
                       module(atom),
                       module_class(list(oneof([user,system,library,
                                                test,development]))),
                       source(boolean),
                       trace_reference(any),
                       on_trace(callable),
                       infer_meta_predicates(oneof([false,true,all])),
                       evaluate(boolean),
                       verbose(boolean)
                     ]).

:- record
    walk_option(undefined:oneof([ignore,error,trace])=ignore,
                autoload:boolean=true,
                source:boolean=true,
                module:atom,                % Only analyse given module
                module_class:list(oneof([user,system,library,
                                         test,development]))=[user,library],
                infer_meta_predicates:oneof([false,true,all])=true,
                clauses:list,               % Walk only these clauses
                trace_reference:any=(-),
                on_trace:callable,          % Call-back on trace hits
                                            % private stuff
                clause,                     % Processed clause
                caller,                     % Head of the caller
                initialization,             % Initialization source
                undecided,                  % Error to throw error
                evaluate:boolean,           % Do partial evaluation
                verbose:boolean=false).     % Report progress

:- thread_local
    multifile_predicate/3.          % Name, Arity, Module

%!  prolog_walk_code(+Options) is det.
%
%   Walk over all loaded (user) Prolog code. The following code is
%   processed:
%
%     1. The bodies of all clauses in all user and library modules.
%        This steps collects, but does not scan multifile predicates
%        to avoid duplicate work.
%     2. All multi-file predicates collected.
%     3. All goals registered with initialization/1
%
%   Options processed:
%
%     * undefined(+Action)
%     Action defines what happens if the analysis finds a
%     definitely undefined predicate.  One of `ignore` or
%     `error` (default is `ignore`).
%
%     * autoload(+Boolean)
%     Try to autoload code while walking. This is enabled by default
%     to obtain as much as possible information about goals and find
%     references from autoloaded libraries.
%
%     * clauses(+ListOfClauseReferences)
%     Only process the given clauses.  Can be used to find clauses
%     quickly using source(false) and then process only interesting
%     clauses with source information.
%
%     * module(+Module)
%     Only process the given module
%
%     * module_class(+ModuleClass)
%     Limit processing to modules of this class. See
%     module_property/2 for details on module classes.  Default
%     is to scan the classes =user= and =library=.
%
%     * infer_meta_predicates(+BooleanOrAll)
%     Use infer_meta_predicate/2 on predicates with clauses that
%     call known meta-predicates.  The analysis is restarted until
%     a fixed point is reached.  If =true= (default), analysis is
%     only restarted if the inferred meta-predicate contains a
%     callable argument.  If =all=, it will be restarted until no
%     more new meta-predicates can be found.
%
%     * trace_reference(Callable)
%     Print all calls to goals that subsume Callable. Goals are
%     represented as Module:Callable (i.e., they are always
%     qualified).  See also subsumes_term/2.
%
%     * on_trace(:OnTrace)
%     If a reference to =trace_reference= is found, call
%     call(OnTrace, Callee, Caller, Location), where Location is one
%     of these:
%
%       - clause_term_position(+ClauseRef, +TermPos)
%       - clause(+ClauseRef)
%       - file_term_position(+Path, +TermPos)
%       - file(+File, +Line, -1, _)
%       - a variable (unknown)
%
%     Caller is the qualified head of the calling clause or the
%     atom '<initialization>'.
%
%     * source(+Boolean)
%     If =false= (default =true=), to not try to obtain detailed
%     source information for printed messages.
%
%     * verbose(+Boolean)
%     If `true` (default `false`), report derived meta-predicates
%     and iterations.
%
%     @compat OnTrace was called using Caller-Location in older
%             versions.

prolog_walk_code(Options) :-
    meta_options(is_meta, Options, QOptions),
    prolog_walk_code(1, QOptions).

prolog_walk_code(Iteration, Options) :-
    statistics(cputime, CPU0),
    make_walk_option(Options, OTerm, _),
    (   walk_option_clauses(OTerm, Clauses),
        nonvar(Clauses)
    ->  walk_clauses(Clauses, OTerm)
    ;   forall(( walk_option_module(OTerm, M),
                 current_module(M),
                 scan_module(M, OTerm)
               ),
               find_walk_from_module(M, OTerm)),
        walk_from_multifile(OTerm),
        walk_from_initialization(OTerm)
    ),
    infer_new_meta_predicates(New, OTerm),
    statistics(cputime, CPU1),
    (   New \== []
    ->  CPU is CPU1-CPU0,
        (   walk_option_verbose(OTerm, true)
        ->  Level = informational
        ;   Level = silent
        ),
        print_message(Level,
                      codewalk(reiterate(New, Iteration, CPU))),
        succ(Iteration, Iteration2),
        prolog_walk_code(Iteration2, Options)
    ;   true
    ).

is_meta(on_trace).


%!  walk_clauses(+Clauses, +OTerm) is det.
%
%   Walk the given clauses.

walk_clauses(Clauses, OTerm) :-
    must_be(list, Clauses),
    forall(member(ClauseRef, Clauses),
           ( user:clause(CHead, Body, ClauseRef),
             (   CHead = Module:Head
             ->  true
             ;   Module = user,
                 Head = CHead
             ),
             walk_option_clause(OTerm, ClauseRef),
             walk_option_caller(OTerm, Module:Head),
             walk_called_by_body(Body, Module, OTerm)
           )).

%!  scan_module(+Module, +OTerm) is semidet.
%
%   True if we must scan Module according to OTerm.

scan_module(M, OTerm) :-
    walk_option_module_class(OTerm, Classes),
    module_property(M, class(Class)),
    memberchk(Class, Classes).

%!  walk_from_initialization(+OTerm)
%
%   Find initialization/1,2 directives and  process   what  they are
%   calling.  Skip
%
%   @bug    Relies on private '$init_goal'/3 database.

walk_from_initialization(OTerm) :-
    walk_option_caller(OTerm, '<initialization>'),
    forall('$init_goal'(_File, Goal, SourceLocation),
           ( walk_option_initialization(OTerm, SourceLocation),
             walk_from_initialization(Goal, OTerm))).

walk_from_initialization(M:Goal, OTerm) :-
    scan_module(M, OTerm),
    !,
    walk_called_by_body(Goal, M, OTerm).
walk_from_initialization(_, _).


%!  find_walk_from_module(+Module, +OTerm) is det.
%
%   Find undefined calls from the bodies  of all clauses that belong
%   to Module.

find_walk_from_module(M, OTerm) :-
    debug(autoload, 'Analysing module ~q', [M]),
    forall(predicate_in_module(M, PI),
           walk_called_by_pred(M:PI, OTerm)).

walk_called_by_pred(Module:Name/Arity, _) :-
    multifile_predicate(Name, Arity, Module),
    !.
walk_called_by_pred(Module:Name/Arity, _) :-
    functor(Head, Name, Arity),
    predicate_property(Module:Head, multifile),
    !,
    assertz(multifile_predicate(Name, Arity, Module)).
walk_called_by_pred(Module:Name/Arity, OTerm) :-
    functor(Head, Name, Arity),
    (   no_walk_property(Property),
        predicate_property(Module:Head, Property)
    ->  true
    ;   walk_option_caller(OTerm, Module:Head),
        walk_option_clause(OTerm, ClauseRef),
        forall(catch(clause(Module:Head, Body, ClauseRef), _, fail),
               walk_called_by_body(Body, Module, OTerm))
    ).

no_walk_property(number_of_rules(0)).   % no point walking only facts
no_walk_property(foreign).              % cannot walk foreign code

%!  walk_from_multifile(+OTerm)
%
%   Process registered multifile predicates.

walk_from_multifile(OTerm) :-
    forall(retract(multifile_predicate(Name, Arity, Module)),
           walk_called_by_multifile(Module:Name/Arity, OTerm)).

walk_called_by_multifile(Module:Name/Arity, OTerm) :-
    functor(Head, Name, Arity),
    forall(catch(clause_not_from_development(
                     Module:Head, Body, ClauseRef, OTerm),
                 _, fail),
           ( walk_option_clause(OTerm, ClauseRef),
             walk_option_caller(OTerm, Module:Head),
             walk_called_by_body(Body, Module, OTerm)
           )).


%!  clause_not_from_development(:Head, -Body, ?Ref, +Options) is nondet.
%
%   Enumerate clauses for a multifile predicate, but omit those from
%   a module that is specifically meant to support development.

clause_not_from_development(Module:Head, Body, Ref, OTerm) :-
    clause(Module:Head, Body, Ref),
    \+ ( clause_property(Ref, file(File)),
         module_property(LoadModule, file(File)),
         \+ scan_module(LoadModule, OTerm)
       ).

%!  walk_called_by_body(+Body, +Module, +OTerm) is det.
%
%   Check the Body term when  executed   in  the  context of Module.
%   Options:
%
%     - undefined(+Action)
%     One of =ignore=, =error=

walk_called_by_body(True, _, _) :-
    True == true,
    !.                % quickly deal with facts
walk_called_by_body(Body, Module, OTerm) :-
    set_undecided_of_walk_option(error, OTerm, OTerm1),
    set_evaluate_of_walk_option(false, OTerm1, OTerm2),
    catch(walk_called(Body, Module, _TermPos, OTerm2),
          missing(Missing),
          walk_called_by_body(Missing, Body, Module, OTerm)),
    !.
walk_called_by_body(Body, Module, OTerm) :-
    format(user_error, 'Failed to analyse:~n', []),
    portray_clause(('<head>' :- Body)),
    debug_walk(Body, Module, OTerm).

% recompile this library after `debug(codewalk(trace))` and re-try
% for debugging failures.
:- if(debugging(codewalk(trace))).
debug_walk(Body, Module, OTerm) :-
    gtrace,
    walk_called_by_body(Body, Module, OTerm).
:- else.
debug_walk(_,_,_).
:- endif.

%!  walk_called_by_body(+Missing, +Body, +Module, +OTerm)
%
%   Restart the analysis because  the   previous  analysis  provided
%   insufficient information.

walk_called_by_body(Missing, Body, _, OTerm) :-
    debugging(codewalk),
    format(user_error, 'Retrying due to ~w (~p)~n', [Missing, OTerm]),
    portray_clause(('<head>' :- Body)), fail.
walk_called_by_body(undecided_call, Body, Module, OTerm) :-
    catch(forall(walk_called(Body, Module, _TermPos, OTerm),
                 true),
          missing(Missing),
          walk_called_by_body(Missing, Body, Module, OTerm)).
walk_called_by_body(subterm_positions, Body, Module, OTerm) :-
    (   (   walk_option_clause(OTerm, ClauseRef), nonvar(ClauseRef),
            clause_info(ClauseRef, _, TermPos, _NameOffset),
            TermPos = term_position(_,_,_,_,[_,BodyPos])
        ->  WBody = Body
        ;   walk_option_initialization(OTerm, SrcLoc),
            ground(SrcLoc), SrcLoc = _File:_Line,
            initialization_layout(SrcLoc, Module:Body, WBody, BodyPos)
        )
    ->  catch(forall(walk_called(WBody, Module, BodyPos, OTerm),
                     true),
              missing(subterm_positions),
              walk_called_by_body(no_positions, Body, Module, OTerm))
    ;   set_source_of_walk_option(false, OTerm, OTerm2),
        forall(walk_called(Body, Module, _BodyPos, OTerm2),
               true)
    ).
walk_called_by_body(no_positions, Body, Module, OTerm) :-
    set_source_of_walk_option(false, OTerm, OTerm2),
    forall(walk_called(Body, Module, _NoPos, OTerm2),
           true).


%!  walk_called(+Goal, +Module, +TermPos, +OTerm) is multi.
%
%   Perform abstract interpretation of Goal,  touching all sub-goals
%   that  are  directly  called  or  immediately  reachable  through
%   meta-calls.  The  actual  auto-loading  is    performed  by  the
%   predicate_property/2 call for meta-predicates.
%
%   If  Goal  is  disjunctive,  walk_called   succeeds  with  a
%   choice-point.  Backtracking  analyses  the  alternative  control
%   path(s).
%
%   Options:
%
%     * undecided(+Action)
%     How to deal with insifficiently instantiated terms in the
%     call-tree.  Values are:
%
%       - ignore
%       Silently ignore such goals
%       - error
%       Throw =undecided_call=
%
%     * evaluate(+Boolean)
%     If =true= (default), evaluate some goals.  Notably =/2.
%
%   @tbd    Analyse e.g. assert((Head:-Body))?

walk_called(Term, Module, parentheses_term_position(_,_,Pos), OTerm) :-
    nonvar(Pos),
    !,
    walk_called(Term, Module, Pos, OTerm).
walk_called(Var, _, TermPos, OTerm) :-
    var(Var),                              % Incomplete analysis
    !,
    undecided(Var, TermPos, OTerm).
walk_called(M:G, _, term_position(_,_,_,_,[MPos,Pos]), OTerm) :-
    !,
    (   nonvar(M)
    ->  walk_called(G, M, Pos, OTerm)
    ;   undecided(M, MPos, OTerm)
    ).
walk_called((A,B), M, term_position(_,_,_,_,[PA,PB]), OTerm) :-
    !,
    walk_called(A, M, PA, OTerm),
    walk_called(B, M, PB, OTerm).
walk_called((A->B), M, term_position(_,_,_,_,[PA,PB]), OTerm) :-
    !,
    walk_called(A, M, PA, OTerm),
    walk_called(B, M, PB, OTerm).
walk_called((A*->B), M, term_position(_,_,_,_,[PA,PB]), OTerm) :-
    !,
    walk_called(A, M, PA, OTerm),
    walk_called(B, M, PB, OTerm).
walk_called(\+(A), M, term_position(_,_,_,_,[PA]), OTerm) :-
    !,
    \+ \+ walk_called(A, M, PA, OTerm).
walk_called((A;B), M, term_position(_,_,_,_,[PA,PB]), OTerm) :-
    !,
    (   walk_option_evaluate(OTerm, Eval), Eval == true
    ->  Goal = (A;B),
        setof(Goal,
              (   walk_called(A, M, PA, OTerm)
              ;   walk_called(B, M, PB, OTerm)
              ),
              Alts0),
        variants(Alts0, Alts),
        member(Goal, Alts)
    ;   \+ \+ walk_called(A, M, PA, OTerm), % do not propagate bindings
        \+ \+ walk_called(B, M, PB, OTerm)
    ).
walk_called(Goal, Module, TermPos, OTerm) :-
    walk_option_trace_reference(OTerm, To), To \== (-),
    (   subsumes_term(To, Module:Goal)
    ->  M2 = Module
    ;   predicate_property(Module:Goal, imported_from(M2)),
        subsumes_term(To, M2:Goal)
    ),
    print_reference(M2:Goal, TermPos, trace, OTerm),
    fail.                                   % Continue search
walk_called(Goal, Module, _, OTerm) :-
    evaluate(Goal, Module, OTerm),
    !.
walk_called(Goal, M, TermPos, OTerm) :-
    (   (   predicate_property(M:Goal, imported_from(IM))
        ->  true
        ;   IM = M
        ),
        prolog:called_by(Goal, IM, M, Called)
    ;   prolog:called_by(Goal, Called)
    ),
    Called \== [],
    !,
    walk_called_by(Called, M, Goal, TermPos, OTerm).
walk_called(Meta, M, term_position(_,E,_,_,ArgPosList), OTerm) :-
    (   walk_option_autoload(OTerm, false)
    ->  nonvar(M),
        '$get_predicate_attribute'(M:Meta, defined, 1)
    ;   true
    ),
    (   predicate_property(M:Meta, meta_predicate(Head))
    ;   inferred_meta_predicate(M:Meta, Head)
    ),
    !,
    walk_option_clause(OTerm, ClauseRef),
    register_possible_meta_clause(ClauseRef),
    walk_meta_call(1, Head, Meta, M, ArgPosList, E-E, OTerm).
walk_called(Goal, Module, _, _) :-
    nonvar(Module),
    '$get_predicate_attribute'(Module:Goal, defined, 1),
    !.
walk_called(Goal, Module, TermPos, OTerm) :-
    callable(Goal),
    !,
    undefined(Module:Goal, TermPos, OTerm).
walk_called(Goal, _Module, TermPos, OTerm) :-
    not_callable(Goal, TermPos, OTerm).

%!  undecided(+Variable, +TermPos, +OTerm)

undecided(Var, TermPos, OTerm) :-
    walk_option_undecided(OTerm, Undecided),
    (   var(Undecided)
    ->  Action = ignore
    ;   Action = Undecided
    ),
    undecided(Action, Var, TermPos, OTerm).

undecided(ignore, _, _, _) :- !.
undecided(error,  _, _, _) :-
    throw(missing(undecided_call)).

%!  evaluate(Goal, Module, OTerm) is nondet.

evaluate(Goal, Module, OTerm) :-
    walk_option_evaluate(OTerm, Evaluate),
    Evaluate \== false,
    evaluate(Goal, Module).

evaluate(A=B, _) :-
    unify_with_occurs_check(A, B).

%!  undefined(:Goal, +TermPos, +OTerm)
%
%   The analysis trapped a definitely undefined predicate.

undefined(_, _, OTerm) :-
    walk_option_undefined(OTerm, ignore),
    !.
undefined(Goal, _, _) :-
    predicate_property(Goal, autoload(_)),
    !.
undefined(Goal, TermPos, OTerm) :-
    (   walk_option_undefined(OTerm, trace)
    ->  Why = trace
    ;   Why = undefined
    ),
    print_reference(Goal, TermPos, Why, OTerm).

%!  not_callable(+Goal, +TermPos, +OTerm)
%
%   We found a reference to a non-callable term

not_callable(Goal, TermPos, OTerm) :-
    print_reference(Goal, TermPos, not_callable, OTerm).


%!  print_reference(+Goal, +TermPos, +Why, +OTerm)
%
%   Print a reference to Goal, found at TermPos.
%
%   @param Why is one of =trace= or =undefined=

print_reference(Goal, TermPos, Why, OTerm) :-
    walk_option_clause(OTerm, Clause), nonvar(Clause),
    !,
    (   compound(TermPos),
        arg(1, TermPos, CharCount),
        integer(CharCount)          % test it is valid
    ->  From = clause_term_position(Clause, TermPos)
    ;   walk_option_source(OTerm, false)
    ->  From = clause(Clause)
    ;   From = _,
        throw(missing(subterm_positions))
    ),
    print_reference2(Goal, From, Why, OTerm).
print_reference(Goal, TermPos, Why, OTerm) :-
    walk_option_initialization(OTerm, Init), nonvar(Init),
    Init = File:Line,
    !,
    (   compound(TermPos),
        arg(1, TermPos, CharCount),
        integer(CharCount)          % test it is valid
    ->  From = file_term_position(File, TermPos)
    ;   walk_option_source(OTerm, false)
    ->  From = file(File, Line, -1, _)
    ;   From = _,
        throw(missing(subterm_positions))
    ),
    print_reference2(Goal, From, Why, OTerm).
print_reference(Goal, _, Why, OTerm) :-
    print_reference2(Goal, _, Why, OTerm).

print_reference2(Goal, From, trace, OTerm) :-
    walk_option_on_trace(OTerm, Closure),
    walk_option_caller(OTerm, Caller),
    nonvar(Closure),
    call(Closure, Goal, Caller, From),
    !.
print_reference2(Goal, From, Why, _OTerm) :-
    make_message(Why, Goal, From, Message, Level),
    print_message(Level, Message).


make_message(undefined, Goal, Context,
             error(existence_error(procedure, PI), Context), error) :-
    goal_pi(Goal, PI).
make_message(not_callable, Goal, Context,
             error(type_error(callable, Goal), Context), error).
make_message(trace, Goal, Context,
             trace_call_to(PI, Context), informational) :-
    goal_pi(Goal, PI).


goal_pi(Goal, M:Name/Arity) :-
    strip_module(Goal, M, Head),
    callable(Head),
    !,
    functor(Head, Name, Arity).
goal_pi(Goal, Goal).

:- dynamic
    possible_meta_predicate/2.

%!  register_possible_meta_clause(+ClauseRef) is det.
%
%   ClausesRef contains as call  to   a  meta-predicate. Remember to
%   analyse this predicate. We only analyse   the predicate if it is
%   loaded from a user module. I.e.,  system and library modules are
%   trusted.

register_possible_meta_clause(ClausesRef) :-
    nonvar(ClausesRef),
    clause_property(ClausesRef, predicate(PI)),
    pi_head(PI, Head, Module),
    module_property(Module, class(user)),
    \+ predicate_property(Module:Head, meta_predicate(_)),
    \+ inferred_meta_predicate(Module:Head, _),
    \+ possible_meta_predicate(Head, Module),
    !,
    assertz(possible_meta_predicate(Head, Module)).
register_possible_meta_clause(_).

pi_head(Module:Name/Arity, Head, Module)  :-
    !,
    functor(Head, Name, Arity).
pi_head(_, _, _) :-
    assertion(fail).

%!  infer_new_meta_predicates(-MetaSpecs, +OTerm) is det.

infer_new_meta_predicates([], OTerm) :-
    walk_option_infer_meta_predicates(OTerm, false),
    !.
infer_new_meta_predicates(MetaSpecs, OTerm) :-
    findall(Module:MetaSpec,
            ( retract(possible_meta_predicate(Head, Module)),
              infer_meta_predicate(Module:Head, MetaSpec),
              (   walk_option_infer_meta_predicates(OTerm, all)
              ->  true
              ;   calling_metaspec(MetaSpec)
              )
            ),
            MetaSpecs).

%!  calling_metaspec(+Head) is semidet.
%
%   True if this is a meta-specification  that makes a difference to
%   the code walker.

calling_metaspec(Head) :-
    arg(_, Head, Arg),
    calling_metaarg(Arg),
    !.

calling_metaarg(I) :- integer(I), !.
calling_metaarg(^).
calling_metaarg(//).


%!  walk_meta_call(+Index, +GoalHead, +MetaHead, +Module,
%!                 +ArgPosList, +EndPos, +OTerm)
%
%   Walk a call to a meta-predicate.   This walks all meta-arguments
%   labeled with an integer, ^ or //.
%
%   @arg    EndPos reflects the end of the term.  This is used if the
%           number of arguments in the compiled form exceeds the
%           number of arguments in the term read.

walk_meta_call(I, Head, Meta, M, ArgPosList, EPos, OTerm) :-
    arg(I, Head, AS),
    !,
    (   ArgPosList = [ArgPos|ArgPosTail]
    ->  true
    ;   ArgPos = EPos,
        ArgPosTail = []
    ),
    (   integer(AS)
    ->  arg(I, Meta, MA),
        extend(MA, AS, Goal, ArgPos, ArgPosEx, OTerm),
        walk_called(Goal, M, ArgPosEx, OTerm)
    ;   AS == (^)
    ->  arg(I, Meta, MA),
        remove_quantifier(MA, Goal, ArgPos, ArgPosEx, M, MG, OTerm),
        walk_called(Goal, MG, ArgPosEx, OTerm)
    ;   AS == (//)
    ->  arg(I, Meta, DCG),
        walk_dcg_body(DCG, M, ArgPos, OTerm)
    ;   true
    ),
    succ(I, I2),
    walk_meta_call(I2, Head, Meta, M, ArgPosTail, EPos, OTerm).
walk_meta_call(_, _, _, _, _, _, _).

remove_quantifier(Goal, _, TermPos, TermPos, M, M, OTerm) :-
    var(Goal),
    !,
    undecided(Goal, TermPos, OTerm).
remove_quantifier(_^Goal0, Goal,
                  term_position(_,_,_,_,[_,GPos]),
                  TermPos, M0, M, OTerm) :-
    !,
    remove_quantifier(Goal0, Goal, GPos, TermPos, M0, M, OTerm).
remove_quantifier(M1:Goal0, Goal,
                  term_position(_,_,_,_,[_,GPos]),
                  TermPos, _, M, OTerm) :-
    !,
    remove_quantifier(Goal0, Goal, GPos, TermPos, M1, M, OTerm).
remove_quantifier(Goal, Goal, TermPos, TermPos, M, M, _).


%!  walk_called_by(+Called:list, +Module, +Goal, +TermPos, +OTerm)
%
%   Walk code explicitly mentioned to  be   called  through the hook
%   prolog:called_by/2.

walk_called_by([], _, _, _, _).
walk_called_by([H|T], M, Goal, TermPos, OTerm) :-
    (   H = G0+N
    ->  subterm_pos(G0, M, Goal, TermPos, G, GPos),
        (   extend(G, N, G2, GPos, GPosEx, OTerm)
        ->  walk_called(G2, M, GPosEx, OTerm)
        ;   true
        )
    ;   subterm_pos(H, M, Goal, TermPos, G, GPos),
        walk_called(G, M, GPos, OTerm)
    ),
    walk_called_by(T, M, Goal, TermPos, OTerm).

subterm_pos(Sub, _, Term, TermPos, Sub, SubTermPos) :-
    subterm_pos(Sub, Term, TermPos, SubTermPos),
    !.
subterm_pos(Sub, M, Term, TermPos, G, SubTermPos) :-
    nonvar(Sub),
    Sub = M:H,
    !,
    subterm_pos(H, M, Term, TermPos, G, SubTermPos).
subterm_pos(Sub, _, _, _, Sub, _).

subterm_pos(Sub, Term, TermPos, SubTermPos) :-
    subterm_pos(Sub, Term, same_term, TermPos, SubTermPos),
    !.
subterm_pos(Sub, Term, TermPos, SubTermPos) :-
    subterm_pos(Sub, Term, ==, TermPos, SubTermPos),
    !.
subterm_pos(Sub, Term, TermPos, SubTermPos) :-
    subterm_pos(Sub, Term, =@=, TermPos, SubTermPos),
    !.
subterm_pos(Sub, Term, TermPos, SubTermPos) :-
    subterm_pos(Sub, Term, subsumes_term, TermPos, SubTermPos),
    !.

%!  walk_dcg_body(+Body, +Module, +TermPos, +OTerm)
%
%   Walk a DCG body that is meta-called.

walk_dcg_body(Var, _Module, TermPos, OTerm) :-
    var(Var),
    !,
    undecided(Var, TermPos, OTerm).
walk_dcg_body([], _Module, _, _) :- !.
walk_dcg_body([_|_], _Module, _, _) :- !.
walk_dcg_body(String, _Module, _, _) :-
    string(String),
    !.
walk_dcg_body(!, _Module, _, _) :- !.
walk_dcg_body(M:G, _, term_position(_,_,_,_,[MPos,Pos]), OTerm) :-
    !,
    (   nonvar(M)
    ->  walk_dcg_body(G, M, Pos, OTerm)
    ;   undecided(M, MPos, OTerm)
    ).
walk_dcg_body((A,B), M, term_position(_,_,_,_,[PA,PB]), OTerm) :-
    !,
    walk_dcg_body(A, M, PA, OTerm),
    walk_dcg_body(B, M, PB, OTerm).
walk_dcg_body((A->B), M, term_position(_,_,_,_,[PA,PB]), OTerm) :-
    !,
    walk_dcg_body(A, M, PA, OTerm),
    walk_dcg_body(B, M, PB, OTerm).
walk_dcg_body((A*->B), M, term_position(_,_,_,_,[PA,PB]), OTerm) :-
    !,
    walk_dcg_body(A, M, PA, OTerm),
    walk_dcg_body(B, M, PB, OTerm).
walk_dcg_body((A;B), M, term_position(_,_,_,_,[PA,PB]), OTerm) :-
    !,
    (   walk_dcg_body(A, M, PA, OTerm)
    ;   walk_dcg_body(B, M, PB, OTerm)
    ).
walk_dcg_body((A|B), M, term_position(_,_,_,_,[PA,PB]), OTerm) :-
    !,
    (   walk_dcg_body(A, M, PA, OTerm)
    ;   walk_dcg_body(B, M, PB, OTerm)
    ).
walk_dcg_body({G}, M, brace_term_position(_,_,PG), OTerm) :-
    !,
    walk_called(G, M, PG, OTerm).
walk_dcg_body(G, M, TermPos, OTerm) :-
    extend(G, 2, G2, TermPos, TermPosEx, OTerm),
    walk_called(G2, M, TermPosEx, OTerm).


%!  subterm_pos(+SubTerm, +Term, :Cmp,
%!              +TermPosition, -SubTermPos) is nondet.
%
%   True when SubTerm is a sub  term   of  Term, compared using Cmp,
%   TermPosition describes the term layout   of  Term and SubTermPos
%   describes the term layout of SubTerm.   Cmp  is typically one of
%   =same_term=, =|==|=, =|=@=|= or =|subsumes_term|=

:- meta_predicate
    subterm_pos(+, +, 2, +, -),
    sublist_pos(+, +, +, +, 2, -).

subterm_pos(_, _, _, Pos, _) :-
    var(Pos), !, fail.
subterm_pos(Sub, Term, Cmp, Pos, Pos) :-
    call(Cmp, Sub, Term),
    !.
subterm_pos(Sub, Term, Cmp, term_position(_,_,_,_,ArgPosList), Pos) :-
    is_list(ArgPosList),
    compound(Term),
    nth1(I, ArgPosList, ArgPos),
    arg(I, Term, Arg),
    subterm_pos(Sub, Arg, Cmp, ArgPos, Pos).
subterm_pos(Sub, Term, Cmp, list_position(_,_,ElemPosList,TailPos), Pos) :-
    sublist_pos(ElemPosList, TailPos, Sub, Term, Cmp, Pos).
subterm_pos(Sub, {Arg}, Cmp, brace_term_position(_,_,ArgPos), Pos) :-
    subterm_pos(Sub, Arg, Cmp, ArgPos, Pos).

sublist_pos([EP|TP], TailPos, Sub, [H|T], Cmp, Pos) :-
    (   subterm_pos(Sub, H, Cmp, EP, Pos)
    ;   sublist_pos(TP, TailPos, Sub, T, Cmp, Pos)
    ).
sublist_pos([], TailPos, Sub, Tail, Cmp, Pos) :-
    TailPos \== none,
    subterm_pos(Sub, Tail, Cmp, TailPos, Pos).

%!  extend(+Goal, +ExtraArgs, +TermPosIn, -TermPosOut, +OTerm)
%
%   @bug:

extend(Goal, 0, Goal, TermPos, TermPos, _) :- !.
extend(Goal, _, _, TermPos, TermPos, OTerm) :-
    var(Goal),
    !,
    undecided(Goal, TermPos, OTerm).
extend(M:Goal, N, M:GoalEx,
       term_position(F,T,FT,TT,[MPos,GPosIn]),
       term_position(F,T,FT,TT,[MPos,GPosOut]), OTerm) :-
    !,
    (   var(M)
    ->  undecided(N, MPos, OTerm)
    ;   true
    ),
    extend(Goal, N, GoalEx, GPosIn, GPosOut, OTerm).
extend(Goal, N, GoalEx, TermPosIn, TermPosOut, _) :-
    callable(Goal),
    !,
    Goal =.. List,
    length(Extra, N),
    extend_term_pos(TermPosIn, N, TermPosOut),
    append(List, Extra, ListEx),
    GoalEx =.. ListEx.
extend(Goal, _, _, TermPos, _, OTerm) :-
    print_reference(Goal, TermPos, not_callable, OTerm).

extend_term_pos(Var, _, _) :-
    var(Var),
    !.
extend_term_pos(term_position(F,T,FT,TT,ArgPosIn),
                N,
                term_position(F,T,FT,TT,ArgPosOut)) :-
    !,
    length(Extra, N),
    maplist(=(0-0), Extra),
    append(ArgPosIn, Extra, ArgPosOut).
extend_term_pos(F-T, N, term_position(F,T,F,T,Extra)) :-
    length(Extra, N),
    maplist(=(0-0), Extra).


%!  variants(+SortedList, -Variants) is det.

variants([], []).
variants([H|T], List) :-
    variants(T, H, List).

variants([], H, [H]).
variants([H|T], V, List) :-
    (   H =@= V
    ->  variants(T, V, List)
    ;   List = [V|List2],
        variants(T, H, List2)
    ).

%!  predicate_in_module(+Module, ?PI) is nondet.
%
%   True if PI is a predicate locally defined in Module.

predicate_in_module(Module, PI) :-
    current_predicate(Module:PI),
    PI = Name/Arity,
    functor(Head, Name, Arity),
    \+ predicate_property(Module:Head, imported_from(_)).


                 /*******************************
                 *      ENUMERATE CLAUSES       *
                 *******************************/

%!  prolog_program_clause(-ClauseRef, +Options) is nondet.
%
%   True when ClauseRef is a reference   for  clause in the program.
%   Options   is   a   subset   of    the   options   processed   by
%   prolog_walk_code/1. The logic for deciding   on which clauses to
%   enumerate is shared with prolog_walk_code/1.
%
%     * module(?Module)
%     * module_class(+list(Classes))

prolog_program_clause(ClauseRef, Options) :-
    make_walk_option(Options, OTerm, _),
    setup_call_cleanup(
        true,
        (   current_module(Module),
            scan_module(Module, OTerm),
            module_clause(Module, ClauseRef, OTerm)
        ;   retract(multifile_predicate(Name, Arity, MM)),
            multifile_clause(ClauseRef, MM:Name/Arity, OTerm)
        ;   initialization_clause(ClauseRef, OTerm)
        ),
        retractall(multifile_predicate(_,_,_))).


module_clause(Module, ClauseRef, _OTerm) :-
    predicate_in_module(Module, Name/Arity),
    \+ multifile_predicate(Name, Arity, Module),
    functor(Head, Name, Arity),
    (   predicate_property(Module:Head, multifile)
    ->  assertz(multifile_predicate(Name, Arity, Module)),
        fail
    ;   predicate_property(Module:Head, Property),
        no_enum_property(Property)
    ->  fail
    ;   catch(nth_clause(Module:Head, _, ClauseRef), _, fail)
    ).

no_enum_property(foreign).

multifile_clause(ClauseRef, M:Name/Arity, OTerm) :-
    functor(Head, Name, Arity),
    catch(clauseref_not_from_development(M:Head, ClauseRef, OTerm),
          _, fail).

clauseref_not_from_development(Module:Head, Ref, OTerm) :-
    nth_clause(Module:Head, _N, Ref),
    \+ ( clause_property(Ref, file(File)),
         module_property(LoadModule, file(File)),
         \+ scan_module(LoadModule, OTerm)
       ).

initialization_clause(ClauseRef, OTerm) :-
    catch(clause(system:'$init_goal'(_File, M:_Goal, SourceLocation),
                 true, ClauseRef),
          _, fail),
    walk_option_initialization(OTerm, SourceLocation),
    scan_module(M, OTerm).


                 /*******************************
                 *            MESSAGES          *
                 *******************************/

:- multifile
    prolog:message//1,
    prolog:message_location//1.

prolog:message(trace_call_to(PI, Context)) -->
    [ 'Call to ~q at '-[PI] ],
    prolog:message_location(Context).

prolog:message_location(clause_term_position(ClauseRef, TermPos)) -->
    { clause_property(ClauseRef, file(File)) },
    message_location_file_term_position(File, TermPos).
prolog:message_location(clause(ClauseRef)) -->
    { clause_property(ClauseRef, file(File)),
      clause_property(ClauseRef, line_count(Line))
    },
    !,
    [ '~w:~d: '-[File, Line] ].
prolog:message_location(clause(ClauseRef)) -->
    { clause_name(ClauseRef, Name) },
    [ '~w: '-[Name] ].
prolog:message_location(file_term_position(Path, TermPos)) -->
    message_location_file_term_position(Path, TermPos).
prolog:message(codewalk(reiterate(New, Iteration, CPU))) -->
    [ 'Found new meta-predicates in iteration ~w (~3f sec)'-
      [Iteration, CPU], nl ],
    meta_decls(New),
    [ 'Restarting analysis ...'-[], nl ].

meta_decls([]) --> [].
meta_decls([H|T]) -->
    [ ':- meta_predicate ~q.'-[H], nl ],
    meta_decls(T).

message_location_file_term_position(File, TermPos) -->
    { arg(1, TermPos, CharCount),
      filepos_line(File, CharCount, Line, LinePos)
    },
    [ '~w:~d:~d: '-[File, Line, LinePos] ].

%!  filepos_line(+File, +CharPos, -Line, -Column) is det.
%
%   @param CharPos is 0-based character offset in the file.
%   @param Column is the current column, counting tabs as 8 spaces.

filepos_line(File, CharPos, Line, LinePos) :-
    setup_call_cleanup(
        ( open(File, read, In),
          open_null_stream(Out)
        ),
        ( copy_stream_data(In, Out, CharPos),
          stream_property(In, position(Pos)),
          stream_position_data(line_count, Pos, Line),
          stream_position_data(line_position, Pos, LinePos)
        ),
        ( close(Out),
          close(In)
        )).
