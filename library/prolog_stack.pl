/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2004-2018, University of Amsterdam
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

:- module(prolog_stack,
          [ get_prolog_backtrace/2,     % +MaxDepth, -Stack
            get_prolog_backtrace/3,     % +Frame, +MaxDepth, -Stack
            prolog_stack_frame_property/2, % +Frame, ?Property
            print_prolog_backtrace/2,   % +Stream, +Stack
            print_prolog_backtrace/3,   % +Stream, +Stack, +Options
            backtrace/1                 % +MaxDepth
          ]).
:- use_module(library(prolog_clause)).
:- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(option)).

:- dynamic stack_guard/1.
:- multifile stack_guard/1.

:- predicate_options(print_prolog_backtrace/3, 3,
                     [ subgoal_positions(boolean)
                     ]).

/** <module> Examine the Prolog stack

This module defines  high-level  primitives   for  examining  the Prolog
stack,  primarily  intended  to  support   debugging.  It  provides  the
following functionality:

    * get_prolog_backtrace/2 gets a Prolog representation of the
    Prolog stack.  This can be used for printing, but also to enrich
    exceptions using prolog_exception_hook/4.  Decorating exceptions
    is provided by this library and controlled by the hook
    stack_guard/1.

    * print_prolog_backtrace/2 prints a backtrace as returned by
    get_prolog_backtrace/2

    * The shorthand backtrace/1 fetches and prints a backtrace.

This library may be enabled by default to improve interactive debugging,
for example by adding the lines below   to  your ~/swiplrc (swipl.ini in
Windows) to decorate uncaught exceptions:

  ==
  :- use_module(library(prolog_stack)).
  ==

@bug    Use of this library may negatively impact performance of
        applications that process (error-)exceptions frequently
        as part of their normal processing.
*/

:- create_prolog_flag(backtrace,            true, [type(boolean), keep(true)]).
:- create_prolog_flag(backtrace_depth,      20,   [type(integer), keep(true)]).
:- create_prolog_flag(backtrace_goal_depth, 3,    [type(integer), keep(true)]).
:- create_prolog_flag(backtrace_show_lines, true, [type(boolean), keep(true)]).

%!  get_prolog_backtrace(+MaxDepth, -Backtrace) is det.
%!  get_prolog_backtrace(+MaxDepth, -Backtrace, +Options) is det.
%
%   Obtain a backtrace from the current location. The backtrace is a
%   list of frames. Each  frame  is  an   opaque  term  that  can be
%   inspected using the predicate  prolog_stack_frame_property/2 can
%   be used to extract  information  from   these  frames.  Most use
%   scenarios will pass the stack   to print_prolog_backtrace/2. The
%   following options are provided:
%
%     * frame(+Frame)
%     Start at Frame instead of the current frame.
%     * goal_depth(+Depth)
%     If Depth > 0, include a shallow copy of the goal arguments
%     into the stack.  Default is set by the Prolog flag
%     =backtrace_goal_depth=, set to =2= initially, showing the
%     goal and toplevel of any argument.
%     * guard(+Guard)
%     Do not show stack frames above Guard.  See stack_guard/1.
%
%   @param Frame is the frame to start from. See prolog_current_frame/1.
%   @param MaxDepth defines the maximum number of frames returned.
%   @compat get_prolog_backtrace/3 used to have the parameters
%   +Frame, +MaxDepth, -Backtrace. A call that matches this
%   signature is mapped to get_prolog_backtrace(MaxDepth, Backtrace,
%   [frame(Frame)]).

get_prolog_backtrace(MaxDepth, Stack) :-
    get_prolog_backtrace(MaxDepth, Stack, []).

get_prolog_backtrace(Fr, MaxDepth, Stack) :-
    integer(Fr), integer(MaxDepth), var(Stack),
    !,
    get_prolog_backtrace_lc(MaxDepth, Stack, [frame(Fr)]),
    nlc.
get_prolog_backtrace(MaxDepth, Stack, Options) :-
    get_prolog_backtrace_lc(MaxDepth, Stack, Options),
    nlc.            % avoid last-call-optimization, such that
                        % the top of the stack is always a nice Prolog
                        % frame

nlc.

get_prolog_backtrace_lc(MaxDepth, Stack, Options) :-
    (   option(frame(Fr), Options)
    ->  PC = call
    ;   prolog_current_frame(Fr0),
        prolog_frame_attribute(Fr0, pc, PC),
        prolog_frame_attribute(Fr0, parent, Fr)
    ),
    (   option(goal_term_depth(GoalDepth), Options)
    ->  true
    ;   current_prolog_flag(backtrace_goal_depth, GoalDepth)
    ),
    option(guard(Guard), Options, none),
    must_be(nonneg, GoalDepth),
    backtrace(MaxDepth, Fr, PC, GoalDepth, Guard, Stack).

backtrace(0, _, _, _, _, []) :- !.
backtrace(MaxDepth, Fr, PC, GoalDepth, Guard,
          [frame(Level, Where, Goal)|Stack]) :-
    prolog_frame_attribute(Fr, level, Level),
    (   PC == foreign
    ->  prolog_frame_attribute(Fr, predicate_indicator, Pred),
        Where = foreign(Pred)
    ;   PC == call
    ->  prolog_frame_attribute(Fr, predicate_indicator, Pred),
        Where = call(Pred)
    ;   prolog_frame_attribute(Fr, clause, Clause)
    ->  Where = clause(Clause, PC)
    ;   Where = meta_call
    ),
    (   Where == meta_call
    ->  Goal = 0
    ;   copy_goal(GoalDepth, Fr, Goal)
    ),
    (   prolog_frame_attribute(Fr, pc, PC2)
    ->  true
    ;   PC2 = foreign
    ),
    (   prolog_frame_attribute(Fr, parent, Parent),
        prolog_frame_attribute(Parent, predicate_indicator, PI),
        PI == Guard                             % last frame
    ->  backtrace(1, Parent, PC2, GoalDepth, Guard, Stack)
    ;   prolog_frame_attribute(Fr, parent, Parent),
        more_stack(Parent)
    ->  D2 is MaxDepth - 1,
        backtrace(D2, Parent, PC2, GoalDepth, Guard, Stack)
    ;   Stack = []
    ).

more_stack(Parent) :-
    prolog_frame_attribute(Parent, predicate_indicator, PI),
    \+ (   PI = '$toplevel':G,
           G \== (toplevel_call/1)
       ),
    !.
more_stack(_) :-
    current_prolog_flag(break_level, Break),
    Break >= 1.

%!  copy_goal(+TermDepth, +Frame, -Goal) is det.
%
%   Create a shallow copy of the frame's  goal to help debugging. In
%   addition to shallow copying, high-arity   terms  are represented
%   as below.  Currently the 16 first arguments are hardcoded.
%
%     ==
%     name(A1, ..., A16, <skipped Skipped of Arity>, An)
%     ==

copy_goal(0, _, 0) :- !.                        % 0 is not a valid goal
copy_goal(D, Fr, Goal) :-
    prolog_frame_attribute(Fr, goal, Goal0),
    (   Goal0 = Module:Goal1
    ->  copy_term_limit(D, Goal1, Goal2),
        (   hidden_module(Module)
        ->  Goal = Goal2
        ;   Goal = Module:Goal2
        )
    ;   copy_term_limit(D, Goal0, Goal)
    ).

hidden_module(system).
hidden_module(user).

copy_term_limit(0, In, '...') :-
    compound(In),
    !.
copy_term_limit(N, In, Out) :-
    is_dict(In),
    !,
    dict_pairs(In, Tag, PairsIn),
    N2 is N - 1,
    MaxArity = 16,
    copy_pairs(PairsIn, N2, MaxArity, PairsOut),
    dict_pairs(Out, Tag, PairsOut).
copy_term_limit(N, In, Out) :-
    compound(In),
    !,
    compound_name_arity(In, Functor, Arity),
    N2 is N - 1,
    MaxArity = 16,
    (   Arity =< MaxArity
    ->  compound_name_arity(Out, Functor, Arity),
        copy_term_args(0, Arity, N2, In, Out)
    ;   OutArity is MaxArity+2,
        compound_name_arity(Out, Functor, OutArity),
        copy_term_args(0, MaxArity, N2, In, Out),
        SkipArg is MaxArity+1,
        Skipped is Arity - MaxArity - 1,
        format(atom(Msg), '<skipped ~D of ~D>', [Skipped, Arity]),
        arg(SkipArg, Out, Msg),
        arg(Arity, In, InA),
        arg(OutArity, Out, OutA),
        copy_term_limit(N2, InA, OutA)
    ).
copy_term_limit(_, In, Out) :-
    copy_term_nat(In, Out).

copy_term_args(I, Arity, Depth, In, Out) :-
    I < Arity,
    !,
    I2 is I + 1,
    arg(I2, In, InA),
    arg(I2, Out, OutA),
    copy_term_limit(Depth, InA, OutA),
    copy_term_args(I2, Arity, Depth, In, Out).
copy_term_args(_, _, _, _, _).

copy_pairs([], _, _, []) :- !.
copy_pairs(Pairs, _, 0, ['<skipped>'-Skipped]) :-
    !,
    length(Pairs, Skipped).
copy_pairs([K-V0|T0], N, MaxArity, [K-V|T]) :-
    copy_term_limit(N, V0, V),
    MaxArity1 is MaxArity - 1,
    copy_pairs(T0, N, MaxArity1, T).


%!  prolog_stack_frame_property(+Frame, ?Property) is nondet.
%
%   True when Property is a property of   Frame. Frame is an element
%   of a stack-trace as produced by get_prolog_backtrace/2.  Defined
%   properties are:
%
%     * level(Level)
%     * predicate(PI)
%     * location(File:Line)

prolog_stack_frame_property(frame(Level,_,_), level(Level)).
prolog_stack_frame_property(frame(_,Where,_), predicate(PI)) :-
    frame_predicate(Where, PI).
prolog_stack_frame_property(frame(_,clause(Clause,PC),_), location(File:Line)) :-
    subgoal_position(Clause, PC, File, CharA, _CharZ),
    File \= @(_),                   % XPCE Object reference
    lineno(File, CharA, Line).
prolog_stack_frame_property(frame(_,_,_,Goal), goal(Goal)) :-
    Goal \== 0.


frame_predicate(foreign(PI), PI).
frame_predicate(call(PI), PI).
frame_predicate(clause(Clause, _PC), PI) :-
    clause_property(Clause, PI).

default_backtrace_options(Options) :-
    (   current_prolog_flag(backtrace_show_lines, true)
    ->  Options = []
    ;   Options = [subgoal_positions(false)]
    ).

%!  print_prolog_backtrace(+Stream, +Backtrace) is det.
%!  print_prolog_backtrace(+Stream, +Backtrace, +Options) is det.
%
%   Print a stacktrace in human readable form to Stream.
%   Options is an option list that accepts:
%
%       * subgoal_positions(+Boolean)
%       If =true=, print subgoal line numbers.  The default depends
%       on the Prolog flag =backtrace_show_lines=.
%
%   @arg Backtrace is a list of frame(Depth,Location,Goal) terms.

print_prolog_backtrace(Stream, Backtrace) :-
    print_prolog_backtrace(Stream, Backtrace, []).

print_prolog_backtrace(Stream, Backtrace, Options) :-
    default_backtrace_options(DefOptions),
    merge_options(Options, DefOptions, FinalOptions),
    phrase(message(Backtrace, FinalOptions), Lines),
    print_message_lines(Stream, '', Lines).

:- public                               % Called from some handlers
    message//1.

message(Backtrace) -->
    {default_backtrace_options(Options)},
    message(Backtrace, Options).

message(Backtrace, Options) -->
    message_frames(Backtrace, Options),
    warn_nodebug(Backtrace).

message_frames([], _) -->
    [].
message_frames([H|T], Options) -->
    message_frames(H, Options),
    (   {T == []}
    ->  []
    ;   [nl],
        message_frames(T, Options)
    ).

message_frames(frame(Level, Where, 0), Options) -->
    !,
    level(Level),
    where_no_goal(Where, Options).
message_frames(frame(Level, _Where, '$toplevel':toplevel_call(_)), _) -->
    !,
    level(Level),
    [ '<user>'-[] ].
message_frames(frame(Level, Where, Goal), Options) -->
    level(Level),
    [ '~p'-[Goal] ],
    where_goal(Where, Options).

where_no_goal(foreign(PI), _) -->
    [ '~w <foreign>'-[PI] ].
where_no_goal(call(PI), _) -->
    [ '~w'-[PI] ].
where_no_goal(clause(Clause, PC), Options) -->
    { option(subgoal_positions(true), Options, true),
      subgoal_position(Clause, PC, File, CharA, _CharZ),
      File \= @(_),                 % XPCE Object reference
      lineno(File, CharA, Line),
      clause_predicate_name(Clause, PredName)
    },
    !,
    [ '~w at ~w:~d'-[PredName, File, Line] ].
where_no_goal(clause(Clause, _PC), _) -->
    { clause_property(Clause, file(File)),
      clause_property(Clause, line_count(Line)),
      clause_predicate_name(Clause, PredName)
    },
    !,
    [ '~w at ~w:~d'-[PredName, File, Line] ].
where_no_goal(clause(Clause, _PC), _) -->
    { clause_name(Clause, ClauseName)
    },
    [ '~w <no source>'-[ClauseName] ].
where_no_goal(meta_call, _) -->
    [ '<meta call>' ].

where_goal(foreign(_), _) -->
    [ ' <foreign>'-[] ],
    !.
where_goal(clause(Clause, PC), Options) -->
    { option(subgoal_positions(true), Options, true),
      subgoal_position(Clause, PC, File, CharA, _CharZ),
      File \= @(_),                 % XPCE Object reference
      lineno(File, CharA, Line)
    },
    !,
    [ ' at ~w:~d'-[File, Line] ].
where_goal(clause(Clause, _PC), _) -->
    { clause_property(Clause, file(File)),
      clause_property(Clause, line_count(Line))
    },
    !,
    [ ' at ~w:~d'-[ File, Line] ].
where_goal(clause(Clause, _PC), _) -->
    { clause_name(Clause, ClauseName)
    },
    !,
    [ ' ~w <no source>'-[ClauseName] ].
where_goal(_, _) -->
    [].

level(Level) -->
    [ '~|~t[~D]~6+ '-[Level] ].

warn_nodebug(Backtrace) -->
    { contiguous(Backtrace) },
    !.
warn_nodebug(_Backtrace) -->
    [ nl,nl,
      'Note: some frames are missing due to last-call optimization.'-[], nl,
      'Re-run your program in debug mode (:- debug.) to get more detail.'-[]
    ].

contiguous([frame(D0,_,_)|Frames]) :-
    contiguous(Frames, D0).

contiguous([], _).
contiguous([frame(D1,_,_)|Frames], D0) :-
    D1 =:= D0-1,
    contiguous(Frames, D1).


%!  clause_predicate_name(+ClauseRef, -Predname) is det.
%
%   Produce a name (typically  Functor/Arity)   for  a  predicate to
%   which Clause belongs.

clause_predicate_name(Clause, PredName) :-
    user:prolog_clause_name(Clause, PredName),
    !.
clause_predicate_name(Clause, PredName) :-
    nth_clause(Head, _N, Clause),
    !,
    predicate_name(user:Head, PredName).


%!  backtrace(+MaxDepth)
%
%   Get and print a stacktrace to the user_error stream.

backtrace(MaxDepth) :-
    get_prolog_backtrace_lc(MaxDepth, Stack, []),
    print_prolog_backtrace(user_error, Stack).


subgoal_position(ClauseRef, PC, File, CharA, CharZ) :-
    debug(backtrace, 'Term-position in ~p at PC=~w:', [ClauseRef, PC]),
    clause_info(ClauseRef, File, TPos, _),
    '$clause_term_position'(ClauseRef, PC, List),
    debug(backtrace, '\t~p~n', [List]),
    find_subgoal(List, TPos, PosTerm),
    arg(1, PosTerm, CharA),
    arg(2, PosTerm, CharZ).

find_subgoal([A|T], term_position(_, _, _, _, PosL), SPos) :-
    is_list(PosL),
    nth1(A, PosL, Pos),
    nonvar(Pos),
    !,
    find_subgoal(T, Pos, SPos).
find_subgoal([], Pos, Pos).


%!  lineno(+File, +Char, -Line)
%
%   Translate a character location to a line-number.

lineno(File, Char, Line) :-
    setup_call_cleanup(
        ( open(File, read, Fd),
          set_stream(Fd, newline(detect))
        ),
        lineno_(Fd, Char, Line),
        close(Fd)).

lineno_(Fd, Char, L) :-
    stream_property(Fd, position(Pos)),
    stream_position_data(char_count, Pos, C),
    C > Char,
    !,
    stream_position_data(line_count, Pos, L0),
    L is L0-1.
lineno_(Fd, Char, L) :-
    skip(Fd, 0'\n),
    lineno_(Fd, Char, L).


                 /*******************************
                 *        DECORATE ERRORS       *
                 *******************************/

%!  prolog_stack:stack_guard(+PI) is semidet.
%
%   Dynamic multifile hook that is normally not defined. The hook is
%   called with PI equal to =none= if   the  exception is not caught
%   and with a fully qualified   (e.g., Module:Name/Arity) predicate
%   indicator of the predicate that called  catch/3 if the exception
%   is caught.
%
%   The exception is of the form error(Formal, ImplDef) and this
%   hook succeeds, ImplDef is unified to a term
%   context(prolog_stack(StackData), Message).  This context
%   information is used by the message printing system to print a
%   human readable representation of the stack when the exception
%   was raised.
%
%   For example, using a clause   stack_guard(none)  prints contexts
%   for uncaught exceptions only.  Using   a  clause  stack_guard(_)
%   prints a full  stack-trace  for  any   error  exception  if  the
%   exception   is   given    to     print_message/2.    See    also
%   library(http/http_error), which limits printing of exceptions to
%   exceptions in user-code called from the HTTP server library.
%
%   Details of the exception decoration is  controlled by two Prolog
%   flags:
%
%       * backtrace_depth
%       Integer that controls the maximum number of frames
%       collected.  Default is 20.  If a guard is specified, callers
%       of the guard are removed from the stack-trace.
%
%       * backtrace_show_lines
%       Boolean that indicates whether the library tries to find
%       line numbers for the calls.  Default is =true=.

:- multifile
    user:prolog_exception_hook/4.
:- dynamic
    user:prolog_exception_hook/4.

user:prolog_exception_hook(error(E, context(Ctx0,Msg)),
                           error(E, context(prolog_stack(Stack),Msg)),
                           Fr, GuardSpec) :-
    current_prolog_flag(backtrace, true),
    \+ is_stack(Ctx0, _Frames),
    (   atom(GuardSpec)
    ->  debug(backtrace, 'Got uncaught (guard = ~q) exception ~p (Ctx0=~p)',
              [GuardSpec, E, Ctx0]),
        stack_guard(GuardSpec),
        Guard = GuardSpec
    ;   prolog_frame_attribute(GuardSpec, predicate_indicator, Guard),
        debug(backtrace, 'Got exception ~p (Ctx0=~p, Catcher=~p)',
              [E, Ctx0, Guard]),
        stack_guard(Guard)
    ),
    (   current_prolog_flag(backtrace_depth, Depth)
    ->  Depth > 0
    ;   Depth = 20                  % Thread created before lib was loaded
    ),
    get_prolog_backtrace(Depth, Stack0,
                         [ frame(Fr),
                           guard(Guard)
                         ]),
    debug(backtrace, 'Stack = ~p', [Stack0]),
    clean_stack(Stack0, Stack1),
    join_stacks(Ctx0, Stack1, Stack).

clean_stack(List, List) :-
    stack_guard(X), var(X),
    !.      % Do not stop if we catch all
clean_stack(List, Clean) :-
    clean_stack2(List, Clean).

clean_stack2([], []).
clean_stack2([H|_], [H]) :-
    guard_frame(H),
    !.
clean_stack2([H|T0], [H|T]) :-
    clean_stack2(T0, T).

guard_frame(frame(_,clause(ClauseRef, _, _))) :-
    nth_clause(M:Head, _, ClauseRef),
    functor(Head, Name, Arity),
    stack_guard(M:Name/Arity).

join_stacks(Ctx0, Stack1, Stack) :-
    nonvar(Ctx0),
    Ctx0 = prolog_stack(Stack0),
    is_list(Stack0), !,
    append(Stack0, Stack1, Stack).
join_stacks(_, Stack, Stack).


%!  stack_guard(+Reason) is semidet.
%
%   Dynamic multifile predicate. It is called  with `none`, `'C'` or
%   the predicate indicator of the   _guard_,  the predicate calling
%   catch/3. The exception must be of   _compatible_  with the shape
%   error(Formal, context(Stack, Msg)). The  default   is  to  catch
%   `none`, uncaught exceptions. `'C'`  implies   that  the callback
%   from C will handle the exception.

stack_guard(none).
stack_guard(system:catch_with_backtrace/3).


                 /*******************************
                 *           MESSAGES           *
                 *******************************/

:- multifile
    prolog:message//1.

prolog:message(error(Error, context(Stack, Message))) -->
    { Message \== 'DWIM could not correct goal',
      is_stack(Stack, Frames)
    },
    !,
    '$messages':translate_message(error(Error, context(_, Message))),
    [ nl, 'In:', nl ],
    (   {is_list(Frames)}
    ->  message(Frames)
    ;   ['~w'-[Frames]]
    ).

is_stack(Stack, Frames) :-
    nonvar(Stack),
    Stack = prolog_stack(Frames).
