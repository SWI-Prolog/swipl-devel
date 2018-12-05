/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org/projects/xpce/
    Copyright (c)  2006-2018, University of Amsterdam
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

:- module(prolog_xref,
          [ xref_source/1,              % +Source
            xref_source/2,              % +Source, +Options
            xref_called/3,              % ?Source, ?Callable, ?By
            xref_called/4,              % ?Source, ?Callable, ?By, ?Cond
            xref_defined/3,             % ?Source. ?Callable, -How
            xref_definition_line/2,     % +How, -Line
            xref_exported/2,            % ?Source, ?Callable
            xref_module/2,              % ?Source, ?Module
            xref_uses_file/3,           % ?Source, ?Spec, ?Path
            xref_op/2,                  % ?Source, ?Op
            xref_prolog_flag/4,         % ?Source, ?Flag, ?Value, ?Line
            xref_comment/3,             % ?Source, ?Title, ?Comment
            xref_comment/4,             % ?Source, ?Head, ?Summary, ?Comment
            xref_mode/3,                % ?Source, ?Mode, ?Det
            xref_option/2,              % ?Source, ?Option
            xref_clean/1,               % +Source
            xref_current_source/1,      % ?Source
            xref_done/2,                % +Source, -When
            xref_built_in/1,            % ?Callable
            xref_source_file/3,         % +Spec, -Path, +Source
            xref_source_file/4,         % +Spec, -Path, +Source, +Options
            xref_public_list/3,         % +File, +Src, +Options
            xref_public_list/4,         % +File, -Path, -Export, +Src
            xref_public_list/6,         % +File, -Path, -Module, -Export, -Meta, +Src
            xref_public_list/7,         % +File, -Path, -Module, -Export, -Public, -Meta, +Src
            xref_meta/3,                % +Source, +Goal, -Called
            xref_meta/2,                % +Goal, -Called
            xref_hook/1,                % ?Callable
                                        % XPCE class references
            xref_used_class/2,          % ?Source, ?ClassName
            xref_defined_class/3        % ?Source, ?ClassName, -How
          ]).
:- use_module(library(debug), [debug/3]).
:- use_module(library(lists), [append/3, append/2, member/2, select/3]).
:- use_module(library(operators), [push_op/3]).
:- use_module(library(shlib), [current_foreign_library/2]).
:- use_module(library(ordsets)).
:- use_module(library(prolog_source)).
:- use_module(library(option)).
:- use_module(library(error)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- if(exists_source(library(pldoc))).
:- use_module(library(pldoc), []).      % Must be loaded before doc_process
:- use_module(library(pldoc/doc_process)).
:- endif.
:- use_module(library(solution_sequences)).
:- use_module(library(modules)).

:- predicate_options(xref_source/2, 2,
                     [ silent(boolean),
                       module(atom),
                       register_called(oneof([all,non_iso,non_built_in])),
                       comments(oneof([store,collect,ignore])),
                       process_include(boolean)
                     ]).


:- dynamic
    called/4,                       % Head, Src, From, Cond
    (dynamic)/3,                    % Head, Src, Line
    (thread_local)/3,               % Head, Src, Line
    (multifile)/3,                  % Head, Src, Line
    (public)/3,                     % Head, Src, Line
    defined/3,                      % Head, Src, Line
    meta_goal/3,                    % Head, Called, Src
    foreign/3,                      % Head, Src, Line
    constraint/3,                   % Head, Src, Line
    imported/3,                     % Head, Src, From
    exported/2,                     % Head, Src
    xmodule/2,                      % Module, Src
    uses_file/3,                    % Spec, Src, Path
    xop/2,                          % Src, Op
    source/2,                       % Src, Time
    used_class/2,                   % Name, Src
    defined_class/5,                % Name, Super, Summary, Src, Line
    (mode)/2,                       % Mode, Src
    xoption/2,                      % Src, Option
    xflag/4,                        % Name, Value, Src, Line

    module_comment/3,               % Src, Title, Comment
    pred_comment/4,                 % Head, Src, Summary, Comment
    pred_comment_link/3,            % Head, Src, HeadTo
    pred_mode/3.                    % Head, Src, Det

:- create_prolog_flag(xref, false, [type(boolean)]).

/** <module> Prolog cross-referencer data collection

This module implements to data-collection  part of the cross-referencer.
This code is used in two places:

    * gxref/0 (part of XPCE) provides a graphical front-end for this
    module
    * PceEmacs (also part of XPCE) uses the cross-referencer to color
    goals and predicates depending on their references.

@bug    meta_predicate/1 declarations take the module into consideration.
        Predicates that are both available as meta-predicate and normal
        (in different modules) are handled as meta-predicate in all
        places.
*/

:- predicate_options(xref_source_file/4, 4,
                     [ file_type(oneof([txt,prolog,directory])),
                       silent(boolean)
                     ]).
:- predicate_options(xref_public_list/3, 3,
                     [ path(-atom),
                       module(-atom),
                       exports(-list(any)),
                       public(-list(any)),
                       meta(-list(any)),
                       silent(boolean)
                     ]).


                 /*******************************
                 *            HOOKS             *
                 *******************************/

%!  prolog:called_by(+Goal, +Module, +Context, -Called) is semidet.
%
%   True when Called is a list of callable terms called from Goal,
%   handled by the predicate Module:Goal and executed in the context
%   of the module Context.  Elements of Called may be qualified.  If
%   not, they are called in the context of the module Context.

%!  prolog:called_by(+Goal, -ListOfCalled)
%
%   If this succeeds, the cross-referencer assumes Goal may call any
%   of the goals in  ListOfCalled.  If   this  call  fails,  default
%   meta-goal analysis is used to determine additional called goals.
%
%   @deprecated     New code should use prolog:called_by/4

%!  prolog:meta_goal(+Goal, -Pattern)
%
%   Define meta-predicates. See  the  examples   in  this  file  for
%   details.

%!  prolog:hook(Goal)
%
%   True if Goal is a hook that  is called spontaneously (e.g., from
%   foreign code).

:- multifile
    prolog:called_by/4,             % +Goal, +Module, +Context, -Called
    prolog:called_by/2,             % +Goal, -Called
    prolog:meta_goal/2,             % +Goal, -Pattern
    prolog:hook/1,                  % +Callable
    prolog:generated_predicate/1.   % :PI

:- meta_predicate
    prolog:generated_predicate(:).

:- dynamic
    meta_goal/2.

:- meta_predicate
    process_predicates(2, +, +).

                 /*******************************
                 *           BUILT-INS          *
                 *******************************/

%!  hide_called(:Callable, +Src) is semidet.
%
%   True when the cross-referencer should   not  include Callable as
%   being   called.   This   is    determined     by    the   option
%   =register_called=.

hide_called(Callable, Src) :-
    xoption(Src, register_called(Which)),
    !,
    mode_hide_called(Which, Callable).
hide_called(Callable, _) :-
    mode_hide_called(non_built_in, Callable).

mode_hide_called(all, _) :- !, fail.
mode_hide_called(non_iso, _:Goal) :-
    goal_name_arity(Goal, Name, Arity),
    current_predicate(system:Name/Arity),
    predicate_property(system:Goal, iso).
mode_hide_called(non_built_in, _:Goal) :-
    goal_name_arity(Goal, Name, Arity),
    current_predicate(system:Name/Arity),
    predicate_property(system:Goal, built_in).
mode_hide_called(non_built_in, M:Goal) :-
    goal_name_arity(Goal, Name, Arity),
    current_predicate(M:Name/Arity),
    predicate_property(M:Goal, built_in).

%!  built_in_predicate(+Callable)
%
%   True if Callable is a built-in

system_predicate(Goal) :-
    goal_name_arity(Goal, Name, Arity),
    current_predicate(system:Name/Arity),   % avoid autoloading
    predicate_property(system:Goal, built_in),
    !.


                /********************************
                *            TOPLEVEL           *
                ********************************/

verbose(Src) :-
    \+ xoption(Src, silent(true)).

:- thread_local
    xref_input/2.                   % File, Stream


%!  xref_source(+Source) is det.
%!  xref_source(+Source, +Options) is det.
%
%   Generate the cross-reference data  for   Source  if  not already
%   done and the source is not modified.  Checking for modifications
%   is only done for files.  Options processed:
%
%     * silent(+Boolean)
%     If =true= (default =false=), emit warning messages.
%     * module(+Module)
%     Define the initial context module to work in.
%     * register_called(+Which)
%     Determines which calls are registerd.  Which is one of
%     =all=, =non_iso= or =non_built_in=.
%     * comments(+CommentHandling)
%     How to handle comments.  If =store=, comments are stored into
%     the database as if the file was compiled. If =collect=,
%     comments are entered to the xref database and made available
%     through xref_mode/2 and xref_comment/4.  If =ignore=,
%     comments are simply ignored. Default is to =collect= comments.
%     * process_include(+Boolean)
%     Process the content of included files (default is `true`).
%
%   @param Source   File specification or XPCE buffer

xref_source(Source) :-
    xref_source(Source, []).

xref_source(Source, Options) :-
    prolog_canonical_source(Source, Src),
    (   last_modified(Source, Modified)
    ->  (   source(Src, Modified)
        ->  true
        ;   xref_clean(Src),
            assert(source(Src, Modified)),
            do_xref(Src, Options)
        )
    ;   xref_clean(Src),
        get_time(Now),
        assert(source(Src, Now)),
        do_xref(Src, Options)
    ).

do_xref(Src, Options) :-
    must_be(list, Options),
    setup_call_cleanup(
        xref_setup(Src, In, Options, State),
        collect(Src, Src, In, Options),
        xref_cleanup(State)).

last_modified(Source, Modified) :-
    prolog:xref_source_time(Source, Modified),
    !.
last_modified(Source, Modified) :-
    atom(Source),
    \+ is_global_url(Source),
    exists_file(Source),
    time_file(Source, Modified).

is_global_url(File) :-
    sub_atom(File, B, _, _, '://'),
    !,
    B > 1,
    sub_atom(File, 0, B, _, Scheme),
    atom_codes(Scheme, Codes),
    maplist(between(0'a, 0'z), Codes).

xref_setup(Src, In, Options, state(In, Dialect, Xref, [SRef|HRefs])) :-
    maplist(assert_option(Src), Options),
    assert_default_options(Src),
    current_prolog_flag(emulated_dialect, Dialect),
    prolog_open_source(Src, In),
    set_initial_mode(In, Options),
    asserta(xref_input(Src, In), SRef),
    set_xref(Xref),
    (   verbose(Src)
    ->  HRefs = []
    ;   asserta(user:thread_message_hook(_,_,_), Ref),
        HRefs = [Ref]
    ).

assert_option(_, Var) :-
    var(Var),
    !,
    instantiation_error(Var).
assert_option(Src, silent(Boolean)) :-
    !,
    must_be(boolean, Boolean),
    assert(xoption(Src, silent(Boolean))).
assert_option(Src, register_called(Which)) :-
    !,
    must_be(oneof([all,non_iso,non_built_in]), Which),
    assert(xoption(Src, register_called(Which))).
assert_option(Src, comments(CommentHandling)) :-
    !,
    must_be(oneof([store,collect,ignore]), CommentHandling),
    assert(xoption(Src, comments(CommentHandling))).
assert_option(Src, module(Module)) :-
    !,
    must_be(atom, Module),
    assert(xoption(Src, module(Module))).
assert_option(Src, process_include(Boolean)) :-
    !,
    must_be(boolean, Boolean),
    assert(xoption(Src, process_include(Boolean))).

assert_default_options(Src) :-
    (   xref_option_default(Opt),
        generalise_term(Opt, Gen),
        (   xoption(Src, Gen)
        ->  true
        ;   assertz(xoption(Src, Opt))
        ),
        fail
    ;   true
    ).

xref_option_default(silent(false)).
xref_option_default(register_called(non_built_in)).
xref_option_default(comments(collect)).
xref_option_default(process_include(true)).

%!  xref_cleanup(+State) is det.
%
%   Restore processing state according to the saved State.

xref_cleanup(state(In, Dialect, Xref, Refs)) :-
    prolog_close_source(In),
    set_prolog_flag(emulated_dialect, Dialect),
    set_prolog_flag(xref, Xref),
    maplist(erase, Refs).

set_xref(Xref) :-
    current_prolog_flag(xref, Xref),
    set_prolog_flag(xref, true).

%!  set_initial_mode(+Stream, +Options) is det.
%
%   Set  the  initial  mode  for  processing    this   file  in  the
%   cross-referencer. If the file is loaded, we use information from
%   the previous load context, setting   the  appropriate module and
%   dialect.

set_initial_mode(_Stream, Options) :-
    option(module(Module), Options),
    !,
    '$set_source_module'(Module).
set_initial_mode(Stream, _) :-
    stream_property(Stream, file_name(Path)),
    source_file_property(Path, load_context(M, _, Opts)),
    !,
    '$set_source_module'(M),
    (   option(dialect(Dialect), Opts)
    ->  expects_dialect(Dialect)
    ;   true
    ).
set_initial_mode(_, _) :-
    '$set_source_module'(user).

%!  xref_input_stream(-Stream) is det.
%
%   Current input stream for cross-referencer.

xref_input_stream(Stream) :-
    xref_input(_, Var),
    !,
    Stream = Var.

%!  xref_push_op(Source, +Prec, +Type, :Name)
%
%   Define operators into the default source module and register
%   them to be undone by pop_operators/0.

xref_push_op(Src, P, T, N0) :-
    '$current_source_module'(M0),
    strip_module(M0:N0, M, N),
    (   is_list(N),
        N \== []
    ->  maplist(push_op(Src, P, T, M), N)
    ;   push_op(Src, P, T, M, N)
    ).

push_op(Src, P, T, M0, N0) :-
    strip_module(M0:N0, M, N),
    Name = M:N,
    valid_op(op(P,T,Name)),
    push_op(P, T, Name),
    assert_op(Src, op(P,T,Name)),
    debug(xref(op), ':- ~w.', [op(P,T,Name)]).

valid_op(op(P,T,M:N)) :-
    atom(M),
    valid_op_name(N),
    integer(P),
    between(0, 1200, P),
    atom(T),
    op_type(T).

valid_op_name(N) :-
    atom(N),
    !.
valid_op_name(N) :-
    N == [].

op_type(xf).
op_type(yf).
op_type(fx).
op_type(fy).
op_type(xfx).
op_type(xfy).
op_type(yfx).

%!  xref_set_prolog_flag(+Flag, +Value, +Src, +Line)
%
%   Called when a directive sets a Prolog flag.

xref_set_prolog_flag(Flag, Value, Src, Line) :-
    atom(Flag),
    !,
    assertz(xflag(Flag, Value, Src, Line)).
xref_set_prolog_flag(_, _, _, _).

%!  xref_clean(+Source) is det.
%
%   Reset the database for the given source.

xref_clean(Source) :-
    prolog_canonical_source(Source, Src),
    retractall(called(_, Src, _Origin, _Cond)),
    retractall(dynamic(_, Src, Line)),
    retractall(multifile(_, Src, Line)),
    retractall(public(_, Src, Line)),
    retractall(defined(_, Src, Line)),
    retractall(meta_goal(_, _, Src)),
    retractall(foreign(_, Src, Line)),
    retractall(constraint(_, Src, Line)),
    retractall(imported(_, Src, _From)),
    retractall(exported(_, Src)),
    retractall(uses_file(_, Src, _)),
    retractall(xmodule(_, Src)),
    retractall(xop(Src, _)),
    retractall(xoption(Src, _)),
    retractall(xflag(_Name, _Value, Src, Line)),
    retractall(source(Src, _)),
    retractall(used_class(_, Src)),
    retractall(defined_class(_, _, _, Src, _)),
    retractall(mode(_, Src)),
    retractall(module_comment(Src, _, _)),
    retractall(pred_comment(_, Src, _, _)),
    retractall(pred_comment_link(_, Src, _)),
    retractall(pred_mode(_, Src, _)).


                 /*******************************
                 *          READ RESULTS        *
                 *******************************/

%!  xref_current_source(?Source)
%
%   Check what sources have been analysed.

xref_current_source(Source) :-
    source(Source, _Time).


%!  xref_done(+Source, -Time) is det.
%
%   Cross-reference executed at Time

xref_done(Source, Time) :-
    prolog_canonical_source(Source, Src),
    source(Src, Time).


%!  xref_called(?Source, ?Called, ?By) is nondet.
%!  xref_called(?Source, ?Called, ?By, ?Cond) is nondet.
%
%   Enumerate the predicate-call relations. Predicate called by
%   directives have a By '<directive>'.

xref_called(Source, Called, By) :-
    xref_called(Source, Called, By, _).

xref_called(Source, Called, By, Cond) :-
    canonical_source(Source, Src),
    called(Called, Src, By, Cond).


%!  xref_defined(?Source, +Goal, ?How) is nondet.
%
%   Test if Goal is accessible in Source.   If this is the case, How
%   specifies the reason why the predicate  is accessible. Note that
%   this predicate does not deal with built-in or global predicates,
%   just locally defined and imported ones.  How   is  one of of the
%   terms below. Location is one of Line (an integer) or File:Line
%   if the definition comes from an included (using :-
%   include(File)) directive.
%
%     * dynamic(Location)
%     * thread_local(Location)
%     * multifile(Location)
%     * public(Location)
%     * local(Location)
%     * foreign(Location)
%     * constraint(Location)
%     * imported(From)

xref_defined(Source, Called, How) :-
    nonvar(Source),
    !,
    canonical_source(Source, Src),
    xref_defined2(How, Src, Called).
xref_defined(Source, Called, How) :-
    xref_defined2(How, Src, Called),
    canonical_source(Source, Src).

xref_defined2(dynamic(Line), Src, Called) :-
    dynamic(Called, Src, Line).
xref_defined2(thread_local(Line), Src, Called) :-
    thread_local(Called, Src, Line).
xref_defined2(multifile(Line), Src, Called) :-
    multifile(Called, Src, Line).
xref_defined2(public(Line), Src, Called) :-
    public(Called, Src, Line).
xref_defined2(local(Line), Src, Called) :-
    defined(Called, Src, Line).
xref_defined2(foreign(Line), Src, Called) :-
    foreign(Called, Src, Line).
xref_defined2(constraint(Line), Src, Called) :-
    constraint(Called, Src, Line).
xref_defined2(imported(From), Src, Called) :-
    imported(Called, Src, From).


%!  xref_definition_line(+How, -Line)
%
%   If the 3th argument of xref_defined contains line info, return
%   this in Line.

xref_definition_line(local(Line),        Line).
xref_definition_line(dynamic(Line),      Line).
xref_definition_line(thread_local(Line), Line).
xref_definition_line(multifile(Line),    Line).
xref_definition_line(public(Line),       Line).
xref_definition_line(constraint(Line),   Line).
xref_definition_line(foreign(Line),      Line).


%!  xref_exported(?Source, ?Head) is nondet.
%
%   True when Source exports Head.

xref_exported(Source, Called) :-
    prolog_canonical_source(Source, Src),
    exported(Called, Src).

%!  xref_module(?Source, ?Module) is nondet.
%
%   True if Module is defined in Source.

xref_module(Source, Module) :-
    nonvar(Source),
    !,
    prolog_canonical_source(Source, Src),
    xmodule(Module, Src).
xref_module(Source, Module) :-
    xmodule(Module, Src),
    prolog_canonical_source(Source, Src).

%!  xref_uses_file(?Source, ?Spec, ?Path) is nondet.
%
%   True when Source tries to load a file using Spec.
%
%   @param Spec is a specification for absolute_file_name/3
%   @param Path is either an absolute file name of the target
%          file or the atom =|<not_found>|=.

xref_uses_file(Source, Spec, Path) :-
    prolog_canonical_source(Source, Src),
    uses_file(Spec, Src, Path).

%!  xref_op(?Source, Op) is nondet.
%
%   Give the operators active inside the module. This is intended to
%   setup the environment for incremental parsing of a term from the
%   source-file.
%
%   @param Op       Term of the form op(Priority, Type, Name)

xref_op(Source, Op) :-
    prolog_canonical_source(Source, Src),
    xop(Src, Op).

%!  xref_prolog_flag(?Source, ?Flag, ?Value, ?Line) is nondet.
%
%   True when Flag is set  to  Value   at  Line  in  Source. This is
%   intended to support incremental  parsing  of   a  term  from the
%   source-file.

xref_prolog_flag(Source, Flag, Value, Line) :-
    prolog_canonical_source(Source, Src),
    xflag(Flag, Value, Src, Line).

xref_built_in(Head) :-
    system_predicate(Head).

xref_used_class(Source, Class) :-
    prolog_canonical_source(Source, Src),
    used_class(Class, Src).

xref_defined_class(Source, Class, local(Line, Super, Summary)) :-
    prolog_canonical_source(Source, Src),
    defined_class(Class, Super, Summary, Src, Line),
    integer(Line),
    !.
xref_defined_class(Source, Class, file(File)) :-
    prolog_canonical_source(Source, Src),
    defined_class(Class, _, _, Src, file(File)).

:- thread_local
    current_cond/1,
    source_line/1.

current_source_line(Line) :-
    source_line(Var),
    !,
    Line = Var.

%!  collect(+Source, +File, +Stream, +Options)
%
%   Process data from Source. If File  \== Source, we are processing
%   an included file. Stream is the stream   from  shich we read the
%   program.

collect(Src, File, In, Options) :-
    (   Src == File
    ->  SrcSpec = Line
    ;   SrcSpec = (File:Line)
    ),
    option(comments(CommentHandling), Options, collect),
    (   CommentHandling == ignore
    ->  CommentOptions = [],
        Comments = []
    ;   CommentHandling == store
    ->  CommentOptions = [ process_comment(true) ],
        Comments = []
    ;   CommentOptions = [ comments(Comments) ]
    ),
    repeat,
        catch(prolog_read_source_term(
                  In, Term, Expanded,
                  [ term_position(TermPos)
                  | CommentOptions
                  ]),
              E, report_syntax_error(E, Src, [])),
        update_condition(Term),
        stream_position_data(line_count, TermPos, Line),
        setup_call_cleanup(
            asserta(source_line(SrcSpec), Ref),
            catch(process(Expanded, Comments, TermPos, Src, EOF),
                  E, print_message(error, E)),
            erase(Ref)),
        EOF == true,
    !.

report_syntax_error(E, _, _) :-
    fatal_error(E),
    throw(E).
report_syntax_error(_, _, Options) :-
    option(silent(true), Options),
    !,
    fail.
report_syntax_error(E, Src, _Options) :-
    (   verbose(Src)
    ->  print_message(error, E)
    ;   true
    ),
    fail.

fatal_error(time_limit_exceeded).
fatal_error(error(resource_error(_),_)).

%!  update_condition(+Term) is det.
%
%   Update the condition under which the current code is compiled.

update_condition((:-Directive)) :-
    !,
    update_cond(Directive).
update_condition(_).

update_cond(if(Cond)) :-
    !,
    asserta(current_cond(Cond)).
update_cond(else) :-
    retract(current_cond(C0)),
    !,
    assert(current_cond(\+C0)).
update_cond(elif(Cond)) :-
    retract(current_cond(C0)),
    !,
    assert(current_cond((\+C0,Cond))).
update_cond(endif) :-
    retract(current_cond(_)),
    !.
update_cond(_).

%!  current_condition(-Condition) is det.
%
%   Condition is the current compilation condition as defined by the
%   :- if/1 directive and friends.

current_condition(Condition) :-
    \+ current_cond(_),
    !,
    Condition = true.
current_condition(Condition) :-
    findall(C, current_cond(C), List),
    list_to_conj(List, Condition).

list_to_conj([], true).
list_to_conj([C], C) :- !.
list_to_conj([H|T], (H,C)) :-
    list_to_conj(T, C).


                 /*******************************
                 *           PROCESS            *
                 *******************************/

%!  process(+Expanded, +Comments, +TermPos, +Src, -EOF) is det.
%
%   Process a source term that has  been   subject  to term expansion as
%   well as its optional leading structured comments.
%
%   @arg TermPos is the term position that describes the start of the
%   term.  We need this to find _leading_ comments.
%   @arg EOF is unified with a boolean to indicate whether or not
%   processing was stopped because `end_of_file` was processed.

process(Expanded, Comments, TermPos, Src, EOF) :-
    is_list(Expanded),                          % term_expansion into list.
    !,
    (   member(Term, Expanded),
        process(Term, Src),
        Term == end_of_file
    ->  EOF = true
    ;   EOF = false
    ),
    xref_comments(Comments, TermPos, Src).
process(end_of_file, _, _, _, true) :-
    !.
process(Term, Comments, TermPos, Src, false) :-
    process(Term, Src),
    xref_comments(Comments, TermPos, Src).

%!  process(+Term, +Src) is det.

process(Var, _) :-
    var(Var),
    !.                    % Warn?
process(end_of_file, _) :- !.
process((:- Directive), Src) :-
    !,
    process_directive(Directive, Src),
    !.
process((?- Directive), Src) :-
    !,
    process_directive(Directive, Src),
    !.
process((Head :- Body), Src) :-
    !,
    assert_defined(Src, Head),
    process_body(Body, Head, Src).
process('$source_location'(_File, _Line):Clause, Src) :-
    !,
    process(Clause, Src).
process(Term, Src) :-
    process_chr(Term, Src),
    !.
process(M:(Head :- Body), Src) :-
    !,
    process((M:Head :- M:Body), Src).
process(Head, Src) :-
    assert_defined(Src, Head).


                 /*******************************
                 *            COMMENTS          *
                 *******************************/

%!  xref_comments(+Comments, +FilePos, +Src) is det.

xref_comments([], _Pos, _Src).
:- if(current_predicate(parse_comment/3)).
xref_comments([Pos-Comment|T], TermPos, Src) :-
    (   Pos @> TermPos              % comments inside term
    ->  true
    ;   stream_position_data(line_count, Pos, Line),
        FilePos = Src:Line,
        (   parse_comment(Comment, FilePos, Parsed)
        ->  assert_comments(Parsed, Src)
        ;   true
        ),
        xref_comments(T, TermPos, Src)
    ).

assert_comments([], _).
assert_comments([H|T], Src) :-
    assert_comment(H, Src),
    assert_comments(T, Src).

assert_comment(section(_Id, Title, Comment), Src) :-
    assertz(module_comment(Src, Title, Comment)).
assert_comment(predicate(PI, Summary, Comment), Src) :-
    pi_to_head(PI, Src, Head),
    assertz(pred_comment(Head, Src, Summary, Comment)).
assert_comment(link(PI, PITo), Src) :-
    pi_to_head(PI, Src, Head),
    pi_to_head(PITo, Src, HeadTo),
    assertz(pred_comment_link(Head, Src, HeadTo)).
assert_comment(mode(Head, Det), Src) :-
    assertz(pred_mode(Head, Src, Det)).

pi_to_head(PI, Src, Head) :-
    pi_to_head(PI, Head0),
    (   Head0 = _:_
    ->  strip_module(Head0, M, Plain),
        (   xmodule(M, Src)
        ->  Head = Plain
        ;   Head = M:Plain
        )
    ;   Head = Head0
    ).
:- endif.

%!  xref_comment(?Source, ?Title, ?Comment) is nondet.
%
%   Is true when Source has a section comment with Title and Comment

xref_comment(Source, Title, Comment) :-
    canonical_source(Source, Src),
    module_comment(Src, Title, Comment).

%!  xref_comment(?Source, ?Head, ?Summary, ?Comment) is nondet.
%
%   Is true when Head in Source has the given PlDoc comment.

xref_comment(Source, Head, Summary, Comment) :-
    canonical_source(Source, Src),
    (   pred_comment(Head, Src, Summary, Comment)
    ;   pred_comment_link(Head, Src, HeadTo),
        pred_comment(HeadTo, Src, Summary, Comment)
    ).

%!  xref_mode(?Source, ?Mode, ?Det) is nondet.
%
%   Is  true  when  Source  provides  a   predicate  with  Mode  and
%   determinism.

xref_mode(Source, Mode, Det) :-
    canonical_source(Source, Src),
    pred_mode(Mode, Src, Det).

%!  xref_option(?Source, ?Option) is nondet.
%
%   True when Source was processed using Option. Options are defined
%   with xref_source/2.

xref_option(Source, Option) :-
    canonical_source(Source, Src),
    xoption(Src, Option).


                 /********************************
                 *           DIRECTIVES         *
                 ********************************/

process_directive(Var, _) :-
    var(Var),
    !.                    % error, but that isn't our business
process_directive(Dir, _Src) :-
    debug(xref(directive), 'Processing :- ~q', [Dir]),
    fail.
process_directive((A,B), Src) :-       % TBD: what about other control
    !,
    process_directive(A, Src),      % structures?
    process_directive(B, Src).
process_directive(List, Src) :-
    is_list(List),
    !,
    process_directive(consult(List), Src).
process_directive(use_module(File, Import), Src) :-
    process_use_module2(File, Import, Src, false).
process_directive(expects_dialect(Dialect), Src) :-
    process_directive(use_module(library(dialect/Dialect)), Src),
    expects_dialect(Dialect).
process_directive(reexport(File, Import), Src) :-
    process_use_module2(File, Import, Src, true).
process_directive(reexport(Modules), Src) :-
    process_use_module(Modules, Src, true).
process_directive(use_module(Modules), Src) :-
    process_use_module(Modules, Src, false).
process_directive(consult(Modules), Src) :-
    process_use_module(Modules, Src, false).
process_directive(ensure_loaded(Modules), Src) :-
    process_use_module(Modules, Src, false).
process_directive(load_files(Files, _Options), Src) :-
    process_use_module(Files, Src, false).
process_directive(include(Files), Src) :-
    process_include(Files, Src).
process_directive(dynamic(Dynamic), Src) :-
    process_predicates(assert_dynamic, Dynamic, Src).
process_directive(thread_local(Dynamic), Src) :-
    process_predicates(assert_thread_local, Dynamic, Src).
process_directive(multifile(Dynamic), Src) :-
    process_predicates(assert_multifile, Dynamic, Src).
process_directive(public(Public), Src) :-
    process_predicates(assert_public, Public, Src).
process_directive(export(Export), Src) :-
    process_predicates(assert_export, Export, Src).
process_directive(import(Import), Src) :-
    process_import(Import, Src).
process_directive(module(Module, Export), Src) :-
    assert_module(Src, Module),
    assert_module_export(Src, Export).
process_directive(module(Module, Export, Import), Src) :-
    assert_module(Src, Module),
    assert_module_export(Src, Export),
    assert_module3(Import, Src).
process_directive('$set_source_module'(system), Src) :-
    assert_module(Src, system).     % hack for handling boot/init.pl
process_directive(pce_begin_class_definition(Name, Meta, Super, Doc), Src) :-
    assert_defined_class(Src, Name, Meta, Super, Doc).
process_directive(pce_autoload(Name, From), Src) :-
    assert_defined_class(Src, Name, imported_from(From)).

process_directive(op(P, A, N), Src) :-
    xref_push_op(Src, P, A, N).
process_directive(set_prolog_flag(Flag, Value), Src) :-
    (   Flag == character_escapes
    ->  set_prolog_flag(character_escapes, Value)
    ;   true
    ),
    current_source_line(Line),
    xref_set_prolog_flag(Flag, Value, Src, Line).
process_directive(style_check(X), _) :-
    style_check(X).
process_directive(encoding(Enc), _) :-
    (   xref_input_stream(Stream)
    ->  catch(set_stream(Stream, encoding(Enc)), _, true)
    ;   true                        % can this happen?
    ).
process_directive(pce_expansion:push_compile_operators, _) :-
    '$current_source_module'(SM),
    call(pce_expansion:push_compile_operators(SM)). % call to avoid xref
process_directive(pce_expansion:pop_compile_operators, _) :-
    call(pce_expansion:pop_compile_operators).
process_directive(meta_predicate(Meta), Src) :-
    process_meta_predicate(Meta, Src).
process_directive(arithmetic_function(FSpec), Src) :-
    arith_callable(FSpec, Goal),
    !,
    current_source_line(Line),
    assert_called(Src, '<directive>'(Line), Goal).
process_directive(format_predicate(_, Goal), Src) :-
    !,
    current_source_line(Line),
    assert_called(Src, '<directive>'(Line), Goal).
process_directive(if(Cond), Src) :-
    !,
    current_source_line(Line),
    assert_called(Src, '<directive>'(Line), Cond).
process_directive(elif(Cond), Src) :-
    !,
    current_source_line(Line),
    assert_called(Src, '<directive>'(Line), Cond).
process_directive(else, _) :- !.
process_directive(endif, _) :- !.
process_directive(Goal, Src) :-
    current_source_line(Line),
    process_body(Goal, '<directive>'(Line), Src).

%!  process_meta_predicate(+Decl, +Src)
%
%   Create meta_goal/3 facts from the meta-goal declaration.

process_meta_predicate((A,B), Src) :-
    !,
    process_meta_predicate(A, Src),
    process_meta_predicate(B, Src).
process_meta_predicate(Decl, Src) :-
    process_meta_head(Src, Decl).

process_meta_head(Src, Decl) :-         % swapped arguments for maplist
    compound(Decl),
    compound_name_arity(Decl, Name, Arity),
    compound_name_arity(Head, Name, Arity),
    meta_args(1, Arity, Decl, Head, Meta),
    (   (   prolog:meta_goal(Head, _)
        ;   prolog:called_by(Head, _, _, _)
        ;   prolog:called_by(Head, _)
        ;   meta_goal(Head, _)
        )
    ->  true
    ;   assert(meta_goal(Head, Meta, Src))
    ).

meta_args(I, Arity, _, _, []) :-
    I > Arity,
    !.
meta_args(I, Arity, Decl, Head, [H|T]) :-               % 0
    arg(I, Decl, 0),
    !,
    arg(I, Head, H),
    I2 is I + 1,
    meta_args(I2, Arity, Decl, Head, T).
meta_args(I, Arity, Decl, Head, [H|T]) :-               % ^
    arg(I, Decl, ^),
    !,
    arg(I, Head, EH),
    setof_goal(EH, H),
    I2 is I + 1,
    meta_args(I2, Arity, Decl, Head, T).
meta_args(I, Arity, Decl, Head, [//(H)|T]) :-
    arg(I, Decl, //),
    !,
    arg(I, Head, H),
    I2 is I + 1,
    meta_args(I2, Arity, Decl, Head, T).
meta_args(I, Arity, Decl, Head, [H+A|T]) :-             % I --> H+I
    arg(I, Decl, A),
    integer(A), A > 0,
    !,
    arg(I, Head, H),
    I2 is I + 1,
    meta_args(I2, Arity, Decl, Head, T).
meta_args(I, Arity, Decl, Head, Meta) :-
    I2 is I + 1,
    meta_args(I2, Arity, Decl, Head, Meta).


              /********************************
              *             BODY              *
              ********************************/

%!  xref_meta(+Source, +Head, -Called) is semidet.
%
%   True when Head calls Called in Source.
%
%   @arg    Called is a list of called terms, terms of the form
%           Term+Extra or terms of the form //(Term).

xref_meta(Source, Head, Called) :-
    canonical_source(Source, Src),
    xref_meta_src(Head, Called, Src).

%!  xref_meta(+Head, -Called) is semidet.
%!  xref_meta_src(+Head, -Called, +Src) is semidet.
%
%   True when Called is a  list  of   terms  called  from Head. Each
%   element in Called can be of the  form Term+Int, which means that
%   Term must be extended with Int additional arguments. The variant
%   xref_meta/3 first queries the local context.
%
%   @tbd    Split predifined in several categories.  E.g., the ISO
%           predicates cannot be redefined.
%   @tbd    Rely on the meta_predicate property for many predicates.
%   @deprecated     New code should use xref_meta/3.

xref_meta_src(Head, Called, Src) :-
    meta_goal(Head, Called, Src),
    !.
xref_meta_src(Head, Called, _) :-
    xref_meta(Head, Called),
    !.
xref_meta_src(Head, Called, _) :-
    compound(Head),
    compound_name_arity(Head, Name, Arity),
    apply_pred(Name),
    Arity > 5,
    !,
    Extra is Arity - 1,
    arg(1, Head, G),
    Called = [G+Extra].

apply_pred(call).                               % built-in
apply_pred(maplist).                            % library(apply_macros)

xref_meta((A, B),               [A, B]).
xref_meta((A; B),               [A, B]).
xref_meta((A| B),               [A, B]).
xref_meta((A -> B),             [A, B]).
xref_meta((A *-> B),            [A, B]).
xref_meta(findall(_V,G,_L),     [G]).
xref_meta(findall(_V,G,_L,_T),  [G]).
xref_meta(findnsols(_N,_V,G,_L),    [G]).
xref_meta(findnsols(_N,_V,G,_L,_T), [G]).
xref_meta(setof(_V, EG, _L),    [G]) :-
    setof_goal(EG, G).
xref_meta(bagof(_V, EG, _L),    [G]) :-
    setof_goal(EG, G).
xref_meta(forall(A, B),         [A, B]).
xref_meta(maplist(G,_),         [G+1]).
xref_meta(maplist(G,_,_),       [G+2]).
xref_meta(maplist(G,_,_,_),     [G+3]).
xref_meta(maplist(G,_,_,_,_),   [G+4]).
xref_meta(map_list_to_pairs(G,_,_), [G+2]).
xref_meta(map_assoc(G, _),      [G+1]).
xref_meta(map_assoc(G, _, _),   [G+2]).
xref_meta(checklist(G, _L),     [G+1]).
xref_meta(sublist(G, _, _),     [G+1]).
xref_meta(include(G, _, _),     [G+1]).
xref_meta(exclude(G, _, _),     [G+1]).
xref_meta(partition(G, _, _, _, _),     [G+2]).
xref_meta(partition(G, _, _, _),[G+1]).
xref_meta(call(G),              [G]).
xref_meta(call(G, _),           [G+1]).
xref_meta(call(G, _, _),        [G+2]).
xref_meta(call(G, _, _, _),     [G+3]).
xref_meta(call(G, _, _, _, _),  [G+4]).
xref_meta(not(G),               [G]).
xref_meta(notrace(G),           [G]).
xref_meta(\+(G),                [G]).
xref_meta(ignore(G),            [G]).
xref_meta(once(G),              [G]).
xref_meta(initialization(G),    [G]).
xref_meta(initialization(G,_),  [G]).
xref_meta(retract(Rule),        [G]) :- head_of(Rule, G).
xref_meta(clause(G, _),         [G]).
xref_meta(clause(G, _, _),      [G]).
xref_meta(phrase(G, _A),        [//(G)]).
xref_meta(phrase(G, _A, _R),    [//(G)]).
xref_meta(call_dcg(G, _A, _R),  [//(G)]).
xref_meta(phrase_from_file(G,_),[//(G)]).
xref_meta(catch(A, _, B),       [A, B]).
xref_meta(catch_with_backtrace(A, _, B), [A, B]).
xref_meta(thread_create(A,_,_), [A]).
xref_meta(thread_create(A,_),   [A]).
xref_meta(thread_signal(_,A),   [A]).
xref_meta(thread_at_exit(A),    [A]).
xref_meta(thread_initialization(A), [A]).
xref_meta(engine_create(_,A,_), [A]).
xref_meta(engine_create(_,A,_,_), [A]).
xref_meta(predsort(A,_,_),      [A+3]).
xref_meta(call_cleanup(A, B),   [A, B]).
xref_meta(call_cleanup(A, _, B),[A, B]).
xref_meta(setup_call_cleanup(A, B, C),[A, B, C]).
xref_meta(setup_call_catcher_cleanup(A, B, _, C),[A, B, C]).
xref_meta(call_residue_vars(A,_), [A]).
xref_meta(with_mutex(_,A),      [A]).
xref_meta(assume(G),            [G]).   % library(debug)
xref_meta(assertion(G),         [G]).   % library(debug)
xref_meta(freeze(_, G),         [G]).
xref_meta(when(C, A),           [C, A]).
xref_meta(time(G),              [G]).   % development system
xref_meta(profile(G),           [G]).
xref_meta(at_halt(G),           [G]).
xref_meta(call_with_time_limit(_, G), [G]).
xref_meta(call_with_depth_limit(G, _, _), [G]).
xref_meta(call_with_inference_limit(G, _, _), [G]).
xref_meta(alarm(_, G, _),       [G]).
xref_meta(alarm(_, G, _, _),    [G]).
xref_meta('$add_directive_wic'(G), [G]).
xref_meta(with_output_to(_, G), [G]).
xref_meta(if(G),                [G]).
xref_meta(elif(G),              [G]).
xref_meta(meta_options(G,_,_),  [G+1]).
xref_meta(on_signal(_,_,H),     [H+1]) :- H \== default.
xref_meta(distinct(G),          [G]).   % library(solution_sequences)
xref_meta(distinct(_, G),       [G]).
xref_meta(order_by(_, G),       [G]).
xref_meta(limit(_, G),          [G]).
xref_meta(offset(_, G),         [G]).
xref_meta(reset(G,_,_),         [G]).

                                        % XPCE meta-predicates
xref_meta(pce_global(_, new(_)), _) :- !, fail.
xref_meta(pce_global(_, B),     [B+1]).
xref_meta(ifmaintainer(G),      [G]).   % used in manual
xref_meta(listen(_, G),         [G]).   % library(broadcast)
xref_meta(listen(_, _, G),      [G]).
xref_meta(in_pce_thread(G),     [G]).

xref_meta(G, Meta) :-                   % call user extensions
    prolog:meta_goal(G, Meta).
xref_meta(G, Meta) :-                   % Generated from :- meta_predicate
    meta_goal(G, Meta).

setof_goal(EG, G) :-
    var(EG), !, G = EG.
setof_goal(_^EG, G) :-
    !,
    setof_goal(EG, G).
setof_goal(G, G).


%!  head_of(+Rule, -Head)
%
%   Get the head for a retract call.

head_of(Var, _) :-
    var(Var), !, fail.
head_of((Head :- _), Head).
head_of(Head, Head).

%!  xref_hook(?Callable)
%
%   Definition of known hooks.  Hooks  that   can  be  called in any
%   module are unqualified.  Other  hooks   are  qualified  with the
%   module where they are called.

xref_hook(Hook) :-
    prolog:hook(Hook).
xref_hook(Hook) :-
    hook(Hook).


hook(attr_portray_hook(_,_)).
hook(attr_unify_hook(_,_)).
hook(attribute_goals(_,_,_)).
hook(goal_expansion(_,_)).
hook(term_expansion(_,_)).
hook(resource(_,_,_)).
hook('$pred_option'(_,_,_,_)).

hook(emacs_prolog_colours:goal_classification(_,_)).
hook(emacs_prolog_colours:term_colours(_,_)).
hook(emacs_prolog_colours:goal_colours(_,_)).
hook(emacs_prolog_colours:style(_,_)).
hook(emacs_prolog_colours:identify(_,_)).
hook(pce_principal:pce_class(_,_,_,_,_,_)).
hook(pce_principal:send_implementation(_,_,_)).
hook(pce_principal:get_implementation(_,_,_,_)).
hook(pce_principal:pce_lazy_get_method(_,_,_)).
hook(pce_principal:pce_lazy_send_method(_,_,_)).
hook(pce_principal:pce_uses_template(_,_)).
hook(prolog:locate_clauses(_,_)).
hook(prolog:message(_,_,_)).
hook(prolog:error_message(_,_,_)).
hook(prolog:message_location(_,_,_)).
hook(prolog:message_context(_,_,_)).
hook(prolog:message_line_element(_,_)).
hook(prolog:debug_control_hook(_)).
hook(prolog:help_hook(_)).
hook(prolog:show_profile_hook(_,_)).
hook(prolog:general_exception(_,_)).
hook(prolog:predicate_summary(_,_)).
hook(prolog:residual_goals(_,_)).
hook(prolog_edit:load).
hook(prolog_edit:locate(_,_,_)).
hook(shlib:unload_all_foreign_libraries).
hook(system:'$foreign_registered'(_, _)).
hook(predicate_options:option_decl(_,_,_)).
hook(user:exception(_,_,_)).
hook(user:file_search_path(_,_)).
hook(user:library_directory(_)).
hook(user:message_hook(_,_,_)).
hook(user:portray(_)).
hook(user:prolog_clause_name(_,_)).
hook(user:prolog_list_goal(_)).
hook(user:prolog_predicate_name(_,_)).
hook(user:prolog_trace_interception(_,_,_,_)).
hook(user:prolog_event_hook(_)).
hook(user:prolog_exception_hook(_,_,_,_)).
hook(sandbox:safe_primitive(_)).
hook(sandbox:safe_meta_predicate(_)).
hook(sandbox:safe_meta(_,_)).
hook(sandbox:safe_global_variable(_)).
hook(sandbox:safe_directive(_)).


%!  arith_callable(+Spec, -Callable)
%
%   Translate argument of arithmetic_function/1 into a callable term

arith_callable(Var, _) :-
    var(Var), !, fail.
arith_callable(Module:Spec, Module:Goal) :-
    !,
    arith_callable(Spec, Goal).
arith_callable(Name/Arity, Goal) :-
    PredArity is Arity + 1,
    functor(Goal, Name, PredArity).

%!  process_body(+Body, +Origin, +Src) is det.
%
%   Process a callable body (body of  a clause or directive). Origin
%   describes the origin of the call. Partial evaluation may lead to
%   non-determinism, which is why we backtrack over process_goal/3.
%
%   We limit the number of explored paths   to  100 to avoid getting
%   trapped in this analysis.

process_body(Body, Origin, Src) :-
    forall(limit(100, process_goal(Body, Origin, Src, _Partial)),
           true).

%!  process_goal(+Goal, +Origin, +Src, ?Partial) is multi.
%
%   Xref Goal. The argument Partial is bound   to  `true` if there was a
%   partial evalation inside Goal that has bound variables.

process_goal(Var, _, _, _) :-
    var(Var),
    !.
process_goal(Goal, Origin, Src, P) :-
    Goal = (_,_),                               % problems
    !,
    phrase(conjunction(Goal), Goals),
    process_conjunction(Goals, Origin, Src, P).
process_goal(Goal, Origin, Src, _) :-           % Final disjunction, no
    Goal = (_;_),                               % problems
    !,
    phrase(disjunction(Goal), Goals),
    forall(member(G, Goals),
           process_body(G, Origin, Src)).
process_goal(Goal, Origin, Src, P) :-
    (   (   xmodule(M, Src)
        ->  true
        ;   M = user
        ),
        (   predicate_property(M:Goal, imported_from(IM))
        ->  true
        ;   IM = M
        ),
        prolog:called_by(Goal, IM, M, Called)
    ;   prolog:called_by(Goal, Called)
    ),
    !,
    must_be(list, Called),
    assert_called(Src, Origin, Goal),
    process_called_list(Called, Origin, Src, P).
process_goal(Goal, Origin, Src, _) :-
    process_xpce_goal(Goal, Origin, Src),
    !.
process_goal(load_foreign_library(File), _Origin, Src, _) :-
    process_foreign(File, Src).
process_goal(load_foreign_library(File, _Init), _Origin, Src, _) :-
    process_foreign(File, Src).
process_goal(use_foreign_library(File), _Origin, Src, _) :-
    process_foreign(File, Src).
process_goal(use_foreign_library(File, _Init), _Origin, Src, _) :-
    process_foreign(File, Src).
process_goal(Goal, Origin, Src, P) :-
    xref_meta_src(Goal, Metas, Src),
    !,
    assert_called(Src, Origin, Goal),
    process_called_list(Metas, Origin, Src, P).
process_goal(Goal, Origin, Src, _) :-
    asserting_goal(Goal, Rule),
    !,
    assert_called(Src, Origin, Goal),
    process_assert(Rule, Origin, Src).
process_goal(Goal, Origin, Src, P) :-
    partial_evaluate(Goal, P),
    assert_called(Src, Origin, Goal).

disjunction(Var)   --> {var(Var), !}, [Var].
disjunction((A;B)) --> !, disjunction(A), disjunction(B).
disjunction(G)     --> [G].

conjunction(Var)   --> {var(Var), !}, [Var].
conjunction((A,B)) --> !, conjunction(A), conjunction(B).
conjunction(G)     --> [G].

shares_vars(RVars, T) :-
    term_variables(T, TVars0),
    sort(TVars0, TVars),
    ord_intersect(RVars, TVars).

process_conjunction([], _, _, _).
process_conjunction([Disj|Rest], Origin, Src, P) :-
    nonvar(Disj),
    Disj = (_;_),
    Rest \== [],
    !,
    phrase(disjunction(Disj), Goals),
    term_variables(Rest, RVars0),
    sort(RVars0, RVars),
    partition(shares_vars(RVars), Goals, Sharing, NonSHaring),
    forall(member(G, NonSHaring),
           process_body(G, Origin, Src)),
    (   Sharing == []
    ->  true
    ;   maplist(term_variables, Sharing, GVars0),
        append(GVars0, GVars1),
        sort(GVars1, GVars),
        ord_intersection(GVars, RVars, SVars),
        VT =.. [v|SVars],
        findall(VT,
                (   member(G, Sharing),
                    process_goal(G, Origin, Src, PS),
                    PS == true
                ),
                Alts0),
        (   Alts0 == []
        ->  true
        ;   (   true
            ;   P = true,
                sort(Alts0, Alts1),
                variants(Alts1, 10, Alts),
                member(VT, Alts)
            )
        )
    ),
    process_conjunction(Rest, Origin, Src, P).
process_conjunction([H|T], Origin, Src, P) :-
    process_goal(H, Origin, Src, P),
    process_conjunction(T, Origin, Src, P).


process_called_list([], _, _, _).
process_called_list([H|T], Origin, Src, P) :-
    process_meta(H, Origin, Src, P),
    process_called_list(T, Origin, Src, P).

process_meta(A+N, Origin, Src, P) :-
    !,
    (   extend(A, N, AX)
    ->  process_goal(AX, Origin, Src, P)
    ;   true
    ).
process_meta(//(A), Origin, Src, P) :-
    !,
    process_dcg_goal(A, Origin, Src, P).
process_meta(G, Origin, Src, P) :-
    process_goal(G, Origin, Src, P).

%!  process_dcg_goal(+Grammar, +Origin, +Src, ?Partial) is det.
%
%   Process  meta-arguments  that  are  tagged   with  //,  such  as
%   phrase/3.

process_dcg_goal(Var, _, _, _) :-
    var(Var),
    !.
process_dcg_goal((A,B), Origin, Src, P) :-
    !,
    process_dcg_goal(A, Origin, Src, P),
    process_dcg_goal(B, Origin, Src, P).
process_dcg_goal((A;B), Origin, Src, P) :-
    !,
    process_dcg_goal(A, Origin, Src, P),
    process_dcg_goal(B, Origin, Src, P).
process_dcg_goal((A|B), Origin, Src, P) :-
    !,
    process_dcg_goal(A, Origin, Src, P),
    process_dcg_goal(B, Origin, Src, P).
process_dcg_goal((A->B), Origin, Src, P) :-
    !,
    process_dcg_goal(A, Origin, Src, P),
    process_dcg_goal(B, Origin, Src, P).
process_dcg_goal((A*->B), Origin, Src, P) :-
    !,
    process_dcg_goal(A, Origin, Src, P),
    process_dcg_goal(B, Origin, Src, P).
process_dcg_goal({Goal}, Origin, Src, P) :-
    !,
    process_goal(Goal, Origin, Src, P).
process_dcg_goal(List, _Origin, _Src, _) :-
    is_list(List),
    !.               % terminal
process_dcg_goal(List, _Origin, _Src, _) :-
    string(List),
    !.                % terminal
process_dcg_goal(Callable, Origin, Src, P) :-
    extend(Callable, 2, Goal),
    !,
    process_goal(Goal, Origin, Src, P).
process_dcg_goal(_, _, _, _).


extend(Var, _, _) :-
    var(Var), !, fail.
extend(M:G, N, M:GX) :-
    !,
    callable(G),
    extend(G, N, GX).
extend(G, N, GX) :-
    (   compound(G)
    ->  compound_name_arguments(G, Name, Args),
        length(Rest, N),
        append(Args, Rest, NArgs),
        compound_name_arguments(GX, Name, NArgs)
    ;   atom(G)
    ->  length(NArgs, N),
        compound_name_arguments(GX, G, NArgs)
    ).

asserting_goal(assert(Rule), Rule).
asserting_goal(asserta(Rule), Rule).
asserting_goal(assertz(Rule), Rule).
asserting_goal(assert(Rule,_), Rule).
asserting_goal(asserta(Rule,_), Rule).
asserting_goal(assertz(Rule,_), Rule).

process_assert(0, _, _) :- !.           % catch variables
process_assert((_:-Body), Origin, Src) :-
    !,
    process_body(Body, Origin, Src).
process_assert(_, _, _).

%!  variants(+SortedList, +Max, -Variants) is det.

variants([], _, []).
variants([H|T], Max, List) :-
    variants(T, H, Max, List).

variants([], H, _, [H]).
variants(_, _, 0, []) :- !.
variants([H|T], V, Max, List) :-
    (   H =@= V
    ->  variants(T, V, Max, List)
    ;   List = [V|List2],
        Max1 is Max-1,
        variants(T, H, Max1, List2)
    ).

%!  partial_evaluate(+Goal, ?Parrial) is det.
%
%   Perform partial evaluation on Goal to trap cases such as below.
%
%     ==
%           T = hello(X),
%           findall(T, T, List),
%     ==
%
%   @tbd    Make this user extensible? What about non-deterministic
%           bindings?

partial_evaluate(Goal, P) :-
    eval(Goal),
    !,
    P = true.
partial_evaluate(_, _).

eval(X = Y) :-
    unify_with_occurs_check(X, Y).


                 /*******************************
                 *          XPCE STUFF          *
                 *******************************/

pce_goal(new(_,_), new(-, new)).
pce_goal(send(_,_), send(arg, msg)).
pce_goal(send_class(_,_,_), send_class(arg, arg, msg)).
pce_goal(get(_,_,_), get(arg, msg, -)).
pce_goal(get_class(_,_,_,_), get_class(arg, arg, msg, -)).
pce_goal(get_chain(_,_,_), get_chain(arg, msg, -)).
pce_goal(get_object(_,_,_), get_object(arg, msg, -)).

process_xpce_goal(G, Origin, Src) :-
    pce_goal(G, Process),
    !,
    assert_called(Src, Origin, G),
    (   arg(I, Process, How),
        arg(I, G, Term),
        process_xpce_arg(How, Term, Origin, Src),
        fail
    ;   true
    ).

process_xpce_arg(new, Term, Origin, Src) :-
    callable(Term),
    process_new(Term, Origin, Src).
process_xpce_arg(arg, Term, Origin, Src) :-
    compound(Term),
    process_new(Term, Origin, Src).
process_xpce_arg(msg, Term, Origin, Src) :-
    compound(Term),
    (   arg(_, Term, Arg),
        process_xpce_arg(arg, Arg, Origin, Src),
        fail
    ;   true
    ).

process_new(_M:_Term, _, _) :- !.       % TBD: Calls on other modules!
process_new(Term, Origin, Src) :-
    assert_new(Src, Origin, Term),
    (   compound(Term),
        arg(_, Term, Arg),
        process_xpce_arg(arg, Arg, Origin, Src),
        fail
    ;   true
    ).

assert_new(_, _, Term) :-
    \+ callable(Term),
    !.
assert_new(Src, Origin, Control) :-
    functor_name(Control, Class),
    pce_control_class(Class),
    !,
    forall(arg(_, Control, Arg),
           assert_new(Src, Origin, Arg)).
assert_new(Src, Origin, Term) :-
    compound(Term),
    arg(1, Term, Prolog),
    Prolog == @(prolog),
    (   Term =.. [message, _, Selector | T],
        atom(Selector)
    ->  Called =.. [Selector|T],
        process_body(Called, Origin, Src)
    ;   Term =.. [?, _, Selector | T],
        atom(Selector)
    ->  append(T, [_R], T2),
        Called =.. [Selector|T2],
        process_body(Called, Origin, Src)
    ),
    fail.
assert_new(_, _, @(_)) :- !.
assert_new(Src, _, Term) :-
    functor_name(Term, Name),
    assert_used_class(Src, Name).


pce_control_class(and).
pce_control_class(or).
pce_control_class(if).
pce_control_class(not).


                /********************************
                *       INCLUDED MODULES        *
                ********************************/

%!  process_use_module(+Modules, +Src, +Rexport) is det.

process_use_module(_Module:_Files, _, _) :- !.  % loaded in another module
process_use_module([], _, _) :- !.
process_use_module([H|T], Src, Reexport) :-
    !,
    process_use_module(H, Src, Reexport),
    process_use_module(T, Src, Reexport).
process_use_module(library(pce), Src, Reexport) :-     % bit special
    !,
    xref_public_list(library(pce), Path, Exports, Src),
    forall(member(Import, Exports),
           process_pce_import(Import, Src, Path, Reexport)).
process_use_module(File, Src, Reexport) :-
    (   xoption(Src, silent(Silent))
    ->  Extra = [silent(Silent)]
    ;   Extra = [silent(true)]
    ),
    (   xref_public_list(File, Src,
                         [ path(Path),
                           module(M),
                           exports(Exports),
                           public(Public),
                           meta(Meta)
                         | Extra
                         ])
    ->  assert(uses_file(File, Src, Path)),
        assert_import(Src, Exports, _, Path, Reexport),
        assert_xmodule_callable(Exports, M, Src, Path),
        assert_xmodule_callable(Public, M, Src, Path),
        maplist(process_meta_head(Src), Meta),
        (   File = library(chr)     % hacky
        ->  assert(mode(chr, Src))
        ;   true
        )
    ;   assert(uses_file(File, Src, '<not_found>'))
    ).

process_pce_import(Name/Arity, Src, Path, Reexport) :-
    atom(Name),
    integer(Arity),
    !,
    functor(Term, Name, Arity),
    (   \+ system_predicate(Term),
        \+ Term = pce_error(_)      % hack!?
    ->  assert_import(Src, [Name/Arity], _, Path, Reexport)
    ;   true
    ).
process_pce_import(op(P,T,N), Src, _, _) :-
    xref_push_op(Src, P, T, N).

%!  process_use_module2(+File, +Import, +Src, +Reexport) is det.
%
%   Process use_module/2 and reexport/2.

process_use_module2(File, Import, Src, Reexport) :-
    (   xref_source_file(File, Path, Src)
    ->  assert(uses_file(File, Src, Path)),
        (   catch(public_list(Path, _, Meta, Export, _Public, []), _, fail)
        ->  assert_import(Src, Import, Export, Path, Reexport),
            forall((  member(Head, Meta),
                      imported(Head, _, Path)
                   ),
                   process_meta_head(Src, Head))
        ;   true
        )
    ;   assert(uses_file(File, Src, '<not_found>'))
    ).


%!  xref_public_list(+Spec, +Source, +Options) is semidet.
%
%   Find meta-information about File. This predicate reads all terms
%   upto the first term that is not  a directive. It uses the module
%   and  meta_predicate  directives  to   assemble  the  information
%   in Options.  Options processed:
%
%     * path(-Path)
%     Path is the full path name of the referenced file.
%     * module(-Module)
%     Module is the module defines in Spec.
%     * exports(-Exports)
%     Exports is a list of predicate indicators and operators
%     collected from the module/2 term and reexport declarations.
%     * public(-Public)
%     Public declarations of the file.
%     * meta(-Meta)
%     Meta is a list of heads as they appear in meta_predicate/1
%     declarations.
%     * silent(+Boolean)
%     Do not print any messages or raise exceptions on errors.
%
%   The information collected by this predicate   is  cached. The cached
%   data is considered valid as long  as   the  modification time of the
%   file does not change.
%
%   @param Source is the file from which Spec is referenced.

xref_public_list(File, Src, Options) :-
    option(path(Path), Options, _),
    option(module(Module), Options, _),
    option(exports(Exports), Options, _),
    option(public(Public), Options, _),
    option(meta(Meta), Options, _),
    xref_source_file(File, Path, Src, Options),
    public_list(Path, Module, Meta, Exports, Public, Options).

%!  xref_public_list(+File, -Path, -Export, +Src) is semidet.
%!  xref_public_list(+File, -Path, -Module, -Export, -Meta, +Src) is semidet.
%!  xref_public_list(+File, -Path, -Module, -Export, -Public, -Meta, +Src) is semidet.
%
%   Find meta-information about File. This predicate reads all terms
%   upto the first term that is not  a directive. It uses the module
%   and  meta_predicate  directives  to   assemble  the  information
%   described below.
%
%   These predicates fail if File is not a module-file.
%
%   @param  Path is the canonical path to File
%   @param  Module is the module defined in Path
%   @param  Export is a list of predicate indicators.
%   @param  Meta is a list of heads as they appear in
%           meta_predicate/1 declarations.
%   @param  Src is the place from which File is referenced.
%   @deprecated New code should use xref_public_list/3, which
%           unifies all variations using an option list.

xref_public_list(File, Path, Export, Src) :-
    xref_source_file(File, Path, Src),
    public_list(Path, _, _, Export, _, []).
xref_public_list(File, Path, Module, Export, Meta, Src) :-
    xref_source_file(File, Path, Src),
    public_list(Path, Module, Meta, Export, _, []).
xref_public_list(File, Path, Module, Export, Public, Meta, Src) :-
    xref_source_file(File, Path, Src),
    public_list(Path, Module, Meta, Export, Public, []).

%!  public_list(+Path, -Module, -Meta, -Export, -Public, +Options)
%
%   Read the public information for Path.  Options supported are:
%
%     - silent(+Boolean)
%       If `true`, ignore (syntax) errors.  If not specified the default
%       is inherited from xref_source/2.

:- dynamic  public_list_cache/6.
:- volatile public_list_cache/6.

public_list(Path, Module, Meta, Export, Public, _Options) :-
    public_list_cache(Path, Modified,
                      Module0, Meta0, Export0, Public0),
    time_file(Path, ModifiedNow),
    (   abs(Modified-ModifiedNow) < 0.0001
    ->  !,
        t(Module,Meta,Export,Public) = t(Module0,Meta0,Export0,Public0)
    ;   retractall(public_list_cache(Path, _, _, _, _, _)),
        fail
    ).
public_list(Path, Module, Meta, Export, Public, Options) :-
    public_list_nc(Path, Module0, Meta0, Export0, Public0, Options),
    (   Error = error(_,_),
        catch(time_file(Path, Modified), Error, fail)
    ->  asserta(public_list_cache(Path, Modified,
                                  Module0, Meta0, Export0, Public0))
    ;   true
    ),
    t(Module,Meta,Export,Public) = t(Module0,Meta0,Export0,Public0).

public_list_nc(Path, Module, Meta, Export, Public, Options) :-
    in_temporary_module(
        TempModule,
        true,
        public_list_diff(TempModule, Path, Module,
                         Meta, [], Export, [], Public, [], Options)).


public_list_diff(TempModule,
                 Path, Module, Meta, MT, Export, Rest, Public, PT, Options) :-
    setup_call_cleanup(
        public_list_setup(TempModule, Path, In, State),
        phrase(read_directives(In, Options, [true]), Directives),
        public_list_cleanup(In, State)),
    public_list(Directives, Path, Module, Meta, MT, Export, Rest, Public, PT).

public_list_setup(TempModule, Path, In, state(OldM, OldXref)) :-
    prolog_open_source(Path, In),
    '$set_source_module'(OldM, TempModule),
    set_xref(OldXref).

public_list_cleanup(In, state(OldM, OldXref)) :-
    '$set_source_module'(OldM),
    set_prolog_flag(xref, OldXref),
    prolog_close_source(In).


read_directives(In, Options, State) -->
    {  repeat,
       catch(prolog_read_source_term(In, Term, Expanded,
                                     [ process_comment(true),
                                       syntax_errors(error)
                                     ]),
             E, report_syntax_error(E, -, Options))
    -> nonvar(Term),
       Term = (:-_)
    },
    !,
    terms(Expanded, State, State1),
    read_directives(In, Options, State1).
read_directives(_, _, _) --> [].

terms(Var, State, State) --> { var(Var) }, !.
terms([H|T], State0, State) -->
    !,
    terms(H, State0, State1),
    terms(T, State1, State).
terms((:-if(Cond)), State0, [True|State0]) -->
    !,
    { eval_cond(Cond, True) }.
terms((:-elif(Cond)), [True0|State], [True|State]) -->
    !,
    { eval_cond(Cond, True1),
      elif(True0, True1, True)
    }.
terms((:-else), [True0|State], [True|State]) -->
    !,
    { negate(True0, True) }.
terms((:-endif), [_|State], State) -->  !.
terms(H, State, State) -->
    (   {State = [true|_]}
    ->  [H]
    ;   []
    ).

eval_cond(Cond, true) :-
    catch(Cond, _, fail),
    !.
eval_cond(_, false).

elif(true,  _,    else_false) :- !.
elif(false, true, true) :- !.
elif(True,  _,    True).

negate(true,       false).
negate(false,      true).
negate(else_false, else_false).

public_list([(:- module(Module, Export0))|Decls], Path,
            Module, Meta, MT, Export, Rest, Public, PT) :-
    !,
    append(Export0, Reexport, Export),
    public_list_(Decls, Path, Meta, MT, Reexport, Rest, Public, PT).
public_list([(:- encoding(_))|Decls], Path,
            Module, Meta, MT, Export, Rest, Public, PT) :-
    public_list(Decls, Path, Module, Meta, MT, Export, Rest, Public, PT).

public_list_([], _, Meta, Meta, Export, Export, Public, Public).
public_list_([(:-(Dir))|T], Path, Meta, MT, Export, Rest, Public, PT) :-
    public_list_1(Dir, Path, Meta, MT0, Export, Rest0, Public, PT0),
    !,
    public_list_(T, Path, MT0, MT, Rest0, Rest, PT0, PT).
public_list_([_|T], Path, Meta, MT, Export, Rest, Public, PT) :-
    public_list_(T, Path, Meta, MT, Export, Rest, Public, PT).

public_list_1(reexport(Spec), Path, Meta, MT, Reexport, Rest, Public, PT) :-
    reexport_files(Spec, Path, Meta, MT, Reexport, Rest, Public, PT).
public_list_1(reexport(Spec, Import), Path, Meta, Meta, Reexport, Rest, Public, Public) :-
    public_from_import(Import, Spec, Path, Reexport, Rest).
public_list_1(meta_predicate(Decl), _Path, Meta, MT, Export, Export, Public, Public) :-
    phrase(meta_decls(Decl), Meta, MT).
public_list_1(public(Decl), _Path, Meta, Meta, Export, Export, Public, PT) :-
    phrase(public_decls(Decl), Public, PT).

%!  reexport_files(+Files, +Src,
%!                 -Meta, ?MetaTail, -Exports, ?ExportsTail,
%!                 -Public, ?PublicTail)

reexport_files([], _, Meta, Meta, Export, Export, Public, Public) :- !.
reexport_files([H|T], Src, Meta, MT, Export, ET, Public, PT) :-
    !,
    xref_source_file(H, Path, Src),
    public_list(Path, _Module, Meta0, Export0, Public0, []),
    append(Meta0, MT1, Meta),
    append(Export0, ET1, Export),
    append(Public0, PT1, Public),
    reexport_files(T, Src, MT1, MT, ET1, ET, PT1, PT).
reexport_files(Spec, Src, Meta, MT, Export, ET, Public, PT) :-
    xref_source_file(Spec, Path, Src),
    public_list(Path, _Module, Meta0, Export0, Public0, []),
    append(Meta0, MT, Meta),
    append(Export0, ET, Export),
    append(Public0, PT, Public).

public_from_import(except(Map), Path, Src, Export, Rest) :-
    !,
    xref_public_list(Path, _, AllExports, Src),
    except(Map, AllExports, NewExports),
    append(NewExports, Rest, Export).
public_from_import(Import, _, _, Export, Rest) :-
    import_name_map(Import, Export, Rest).


%!  except(+Remove, +AllExports, -Exports)

except([], Exports, Exports).
except([PI0 as NewName|Map], Exports0, Exports) :-
    !,
    canonical_pi(PI0, PI),
    map_as(Exports0, PI, NewName, Exports1),
    except(Map, Exports1, Exports).
except([PI0|Map], Exports0, Exports) :-
    canonical_pi(PI0, PI),
    select(PI2, Exports0, Exports1),
    same_pi(PI, PI2),
    !,
    except(Map, Exports1, Exports).


map_as([PI|T], Repl, As, [PI2|T])  :-
    same_pi(Repl, PI),
    !,
    pi_as(PI, As, PI2).
map_as([H|T0], Repl, As, [H|T])  :-
    map_as(T0, Repl, As, T).

pi_as(_/Arity, Name, Name/Arity).
pi_as(_//Arity, Name, Name//Arity).

import_name_map([], L, L).
import_name_map([_/Arity as NewName|T0], [NewName/Arity|T], Tail) :-
    !,
    import_name_map(T0, T, Tail).
import_name_map([_//Arity as NewName|T0], [NewName//Arity|T], Tail) :-
    !,
    import_name_map(T0, T, Tail).
import_name_map([H|T0], [H|T], Tail) :-
    import_name_map(T0, T, Tail).

canonical_pi(Name//Arity0, PI) :-
    integer(Arity0),
    !,
    PI = Name/Arity,
    Arity is Arity0 + 2.
canonical_pi(PI, PI).

same_pi(Canonical, PI2) :-
    canonical_pi(PI2, Canonical).

meta_decls(Var) -->
    { var(Var) },
    !.
meta_decls((A,B)) -->
    !,
    meta_decls(A),
    meta_decls(B).
meta_decls(A) -->
    [A].

public_decls(Var) -->
    { var(Var) },
    !.
public_decls((A,B)) -->
    !,
    public_decls(A),
    public_decls(B).
public_decls(A) -->
    [A].

                 /*******************************
                 *             INCLUDE          *
                 *******************************/

process_include([], _) :- !.
process_include([H|T], Src) :-
    !,
    process_include(H, Src),
    process_include(T, Src).
process_include(File, Src) :-
    callable(File),
    !,
    (   once(xref_input(ParentSrc, _)),
        xref_source_file(File, Path, ParentSrc)
    ->  (   (   uses_file(_, Src, Path)
            ;   Path == Src
            )
        ->  true
        ;   assert(uses_file(File, Src, Path)),
            (   xoption(Src, process_include(true))
            ->  findall(O, xoption(Src, O), Options),
                setup_call_cleanup(
                    open_include_file(Path, In, Refs),
                    collect(Src, Path, In, Options),
                    close_include(In, Refs))
            ;   true
            )
        )
    ;   assert(uses_file(File, Src, '<not_found>'))
    ).
process_include(_, _).

%!  open_include_file(+Path, -In, -Refs)
%
%   Opens an :- include(File) referenced file.   Note that we cannot
%   use prolog_open_source/2 because we   should  _not_ safe/restore
%   the lexical context.

open_include_file(Path, In, [Ref]) :-
    once(xref_input(_, Parent)),
    stream_property(Parent, encoding(Enc)),
    '$push_input_context'(xref_include),
    catch((   prolog:xref_open_source(Path, In)
          ->  set_stream(In, encoding(Enc))
          ;   include_encoding(Enc, Options),
              open(Path, read, In, Options)
          ), E,
          ( '$pop_input_context', throw(E))),
    catch((   peek_char(In, #)              % Deal with #! script
          ->  skip(In, 10)
          ;   true
          ), E,
          ( close_include(In, []), throw(E))),
    asserta(xref_input(Path, In), Ref).

include_encoding(wchar_t, []) :- !.
include_encoding(Enc, [encoding(Enc)]).


close_include(In, Refs) :-
    maplist(erase, Refs),
    close(In, [force(true)]),
    '$pop_input_context'.

%!  process_foreign(+Spec, +Src)
%
%   Process a load_foreign_library/1 call.

process_foreign(Spec, Src) :-
    ground(Spec),
    current_foreign_library(Spec, Defined),
    !,
    (   xmodule(Module, Src)
    ->  true
    ;   Module = user
    ),
    process_foreign_defined(Defined, Module, Src).
process_foreign(_, _).

process_foreign_defined([], _, _).
process_foreign_defined([H|T], M, Src) :-
    (   H = M:Head
    ->  assert_foreign(Src, Head)
    ;   assert_foreign(Src, H)
    ),
    process_foreign_defined(T, M, Src).


                 /*******************************
                 *          CHR SUPPORT         *
                 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This part of the file supports CHR. Our choice is between making special
hooks to make CHR expansion work and  then handle the (complex) expanded
code or process the  CHR  source   directly.  The  latter looks simpler,
though I don't like the idea  of   adding  support for libraries to this
module.  A  file  is  supposed  to  be  a    CHR   file  if  it  uses  a
use_module(library(chr) or contains a :-   constraint/1 directive. As an
extra bonus we get the source-locations right :-)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

process_chr(@(_Name, Rule), Src) :-
    mode(chr, Src),
    process_chr(Rule, Src).
process_chr(pragma(Rule, _Pragma), Src) :-
    mode(chr, Src),
    process_chr(Rule, Src).
process_chr(<=>(Head, Body), Src) :-
    mode(chr, Src),
    chr_head(Head, Src, H),
    chr_body(Body, H, Src).
process_chr(==>(Head, Body), Src) :-
    mode(chr, Src),
    chr_head(Head, H, Src),
    chr_body(Body, H, Src).
process_chr((:- chr_constraint(_)), Src) :-
    (   mode(chr, Src)
    ->  true
    ;   assert(mode(chr, Src))
    ).

chr_head(X, _, _) :-
    var(X),
    !.                      % Illegal.  Warn?
chr_head(\(A,B), Src, H) :-
    chr_head(A, Src, H),
    process_body(B, H, Src).
chr_head((H0,B), Src, H) :-
    chr_defined(H0, Src, H),
    process_body(B, H, Src).
chr_head(H0, Src, H) :-
    chr_defined(H0, Src, H).

chr_defined(X, _, _) :-
    var(X),
    !.
chr_defined(#(C,_Id), Src, C) :-
    !,
    assert_constraint(Src, C).
chr_defined(A, Src, A) :-
    assert_constraint(Src, A).

chr_body(X, From, Src) :-
    var(X),
    !,
    process_body(X, From, Src).
chr_body('|'(Guard, Goals), H, Src) :-
    !,
    chr_body(Guard, H, Src),
    chr_body(Goals, H, Src).
chr_body(G, From, Src) :-
    process_body(G, From, Src).

assert_constraint(_, Head) :-
    var(Head),
    !.
assert_constraint(Src, Head) :-
    constraint(Head, Src, _),
    !.
assert_constraint(Src, Head) :-
    generalise_term(Head, Term),
    current_source_line(Line),
    assert(constraint(Term, Src, Line)).


                /********************************
                *       PHASE 1 ASSERTIONS      *
                ********************************/

%!  assert_called(+Src, +From, +Head) is det.
%
%   Assert the fact that Head is called by From in Src. We do not
%   assert called system predicates.

assert_called(_, _, Var) :-
    var(Var),
    !.
assert_called(Src, From, Goal) :-
    var(From),
    !,
    assert_called(Src, '<unknown>', Goal).
assert_called(_, _, Goal) :-
    expand_hide_called(Goal),
    !.
assert_called(Src, Origin, M:G) :-
    !,
    (   atom(M),
        callable(G)
    ->  current_condition(Cond),
        (   xmodule(M, Src)         % explicit call to own module
        ->  assert_called(Src, Origin, G)
        ;   called(M:G, Src, Origin, Cond) % already registered
        ->  true
        ;   hide_called(M:G, Src)           % not interesting (now)
        ->  true
        ;   generalise(Origin, OTerm),
            generalise(G, GTerm)
        ->  assert(called(M:GTerm, Src, OTerm, Cond))
        ;   true
        )
    ;   true                        % call to variable module
    ).
assert_called(Src, _, Goal) :-
    (   xmodule(M, Src)
    ->  M \== system
    ;   M = user
    ),
    hide_called(M:Goal, Src),
    !.
assert_called(Src, Origin, Goal) :-
    current_condition(Cond),
    (   called(Goal, Src, Origin, Cond)
    ->  true
    ;   generalise(Origin, OTerm),
        generalise(Goal, Term)
    ->  assert(called(Term, Src, OTerm, Cond))
    ;   true
    ).


%!  expand_hide_called(:Callable) is semidet.
%
%   Goals that should not turn up as being called. Hack. Eventually
%   we should deal with that using an XPCE plugin.

expand_hide_called(pce_principal:send_implementation(_, _, _)).
expand_hide_called(pce_principal:get_implementation(_, _, _, _)).
expand_hide_called(pce_principal:pce_lazy_get_method(_,_,_)).
expand_hide_called(pce_principal:pce_lazy_send_method(_,_,_)).

assert_defined(Src, Goal) :-
    defined(Goal, Src, _),
    !.
assert_defined(Src, Goal) :-
    generalise(Goal, Term),
    current_source_line(Line),
    assert(defined(Term, Src, Line)).

assert_foreign(Src, Goal) :-
    foreign(Goal, Src, _),
    !.
assert_foreign(Src, Goal) :-
    generalise(Goal, Term),
    current_source_line(Line),
    assert(foreign(Term, Src, Line)).

%!  assert_import(+Src, +Import, +ExportList, +From, +Reexport) is det.
%
%   Asserts imports into Src. Import   is  the import specification,
%   ExportList is the list of known   exported predicates or unbound
%   if this need not be checked and From  is the file from which the
%   public predicates come. If  Reexport   is  =true=, re-export the
%   imported predicates.
%
%   @tbd    Tighter type-checking on Import.

assert_import(_, [], _, _, _) :- !.
assert_import(Src, [H|T], Export, From, Reexport) :-
    !,
    assert_import(Src, H, Export, From, Reexport),
    assert_import(Src, T, Export, From, Reexport).
assert_import(Src, except(Except), Export, From, Reexport) :-
    !,
    is_list(Export),
    !,
    except(Except, Export, Import),
    assert_import(Src, Import, _All, From, Reexport).
assert_import(Src, Import as Name, Export, From, Reexport) :-
    !,
    pi_to_head(Import, Term0),
    rename_goal(Term0, Name, Term),
    (   in_export_list(Term0, Export)
    ->  assert(imported(Term, Src, From)),
        assert_reexport(Reexport, Src, Term)
    ;   current_source_line(Line),
        assert_called(Src, '<directive>'(Line), Term0)
    ).
assert_import(Src, Import, Export, From, Reexport) :-
    pi_to_head(Import, Term),
    !,
    (   in_export_list(Term, Export)
    ->  assert(imported(Term, Src, From)),
        assert_reexport(Reexport, Src, Term)
    ;   current_source_line(Line),
        assert_called(Src, '<directive>'(Line), Term)
    ).
assert_import(Src, op(P,T,N), _, _, _) :-
    xref_push_op(Src, P,T,N).

in_export_list(_Head, Export) :-
    var(Export),
    !.
in_export_list(Head, Export) :-
    member(PI, Export),
    pi_to_head(PI, Head).

assert_reexport(false, _, _) :- !.
assert_reexport(true, Src, Term) :-
    assert(exported(Term, Src)).

%!  process_import(:Import, +Src)
%
%   Process an import/1 directive

process_import(M:PI, Src) :-
    pi_to_head(PI, Head),
    !,
    (   atom(M),
        current_module(M),
        module_property(M, file(From))
    ->  true
    ;   From = '<unknown>'
    ),
    assert(imported(Head, Src, From)).
process_import(_, _).

%!  assert_xmodule_callable(PIs, Module, Src, From)
%
%   We can call all exports  and   public  predicates of an imported
%   module using Module:Goal.
%
%   @tbd    Should we distinguish this from normal imported?

assert_xmodule_callable([], _, _, _).
assert_xmodule_callable([PI|T], M, Src, From) :-
    (   pi_to_head(M:PI, Head)
    ->  assert(imported(Head, Src, From))
    ;   true
    ),
    assert_xmodule_callable(T, M, Src, From).


%!  assert_op(+Src, +Op) is det.
%
%   @param Op       Ground term op(Priority, Type, Name).

assert_op(Src, op(P,T,M:N)) :-
    (   '$current_source_module'(M)
    ->  Name = N
    ;   Name = M:N
    ),
    (   xop(Src, op(P,T,Name))
    ->  true
    ;   assert(xop(Src, op(P,T,Name)))
    ).

%!  assert_module(+Src, +Module)
%
%   Assert we are loading code into Module.  This is also used to
%   exploit local term-expansion and other rules.

assert_module(Src, Module) :-
    xmodule(Module, Src),
    !.
assert_module(Src, Module) :-
    '$set_source_module'(Module),
    assert(xmodule(Module, Src)),
    (   module_property(Module, class(system))
    ->  retractall(xoption(Src, register_called(_))),
        assert(xoption(Src, register_called(all)))
    ;   true
    ).

assert_module_export(_, []) :- !.
assert_module_export(Src, [H|T]) :-
    !,
    assert_module_export(Src, H),
    assert_module_export(Src, T).
assert_module_export(Src, PI) :-
    pi_to_head(PI, Term),
    !,
    assert(exported(Term, Src)).
assert_module_export(Src, op(P, A, N)) :-
    xref_push_op(Src, P, A, N).

%!  assert_module3(+Import, +Src)
%
%   Handle 3th argument of module/3 declaration.

assert_module3([], _) :- !.
assert_module3([H|T], Src) :-
    !,
    assert_module3(H, Src),
    assert_module3(T, Src).
assert_module3(Option, Src) :-
    process_use_module(library(dialect/Option), Src, false).


%!  process_predicates(:Closure, +Predicates, +Src)
%
%   Process areguments of dynamic,  etc.,   using  call(Closure, PI,
%   Src).  Handles  both  lists  of    specifications  and  (PI,...)
%   specifications.

process_predicates(Closure, Preds, Src) :-
    is_list(Preds),
    !,
    process_predicate_list(Preds, Closure, Src).
process_predicates(Closure, Preds, Src) :-
    process_predicate_comma(Preds, Closure, Src).

process_predicate_list([], _, _).
process_predicate_list([H|T], Closure, Src) :-
    (   nonvar(H)
    ->  call(Closure, H, Src)
    ;   true
    ),
    process_predicate_list(T, Closure, Src).

process_predicate_comma(Var, _, _) :-
    var(Var),
    !.
process_predicate_comma(M:(A,B), Closure, Src) :-
    !,
    process_predicate_comma(M:A, Closure, Src),
    process_predicate_comma(M:B, Closure, Src).
process_predicate_comma((A,B), Closure, Src) :-
    !,
    process_predicate_comma(A, Closure, Src),
    process_predicate_comma(B, Closure, Src).
process_predicate_comma(A, Closure, Src) :-
    call(Closure, A, Src).


assert_dynamic(PI, Src) :-
    pi_to_head(PI, Term),
    (   thread_local(Term, Src, _)  % dynamic after thread_local has
    ->  true                        % no effect
    ;   current_source_line(Line),
        assert(dynamic(Term, Src, Line))
    ).

assert_thread_local(PI, Src) :-
    pi_to_head(PI, Term),
    current_source_line(Line),
    assert(thread_local(Term, Src, Line)).

assert_multifile(PI, Src) :-                    % :- multifile(Spec)
    pi_to_head(PI, Term),
    current_source_line(Line),
    assert(multifile(Term, Src, Line)).

assert_public(PI, Src) :-                       % :- public(Spec)
    pi_to_head(PI, Term),
    current_source_line(Line),
    assert_called(Src, '<public>'(Line), Term),
    assert(public(Term, Src, Line)).

assert_export(PI, Src) :-                       % :- export(Spec)
    pi_to_head(PI, Term),
    !,
    assert(exported(Term, Src)).

%!  pi_to_head(+PI, -Head) is semidet.
%
%   Translate Name/Arity or Name//Arity to a callable term. Fails if
%   PI is not a predicate indicator.

pi_to_head(Var, _) :-
    var(Var), !, fail.
pi_to_head(M:PI, M:Term) :-
    !,
    pi_to_head(PI, Term).
pi_to_head(Name/Arity, Term) :-
    functor(Term, Name, Arity).
pi_to_head(Name//DCGArity, Term) :-
    Arity is DCGArity+2,
    functor(Term, Name, Arity).


assert_used_class(Src, Name) :-
    used_class(Name, Src),
    !.
assert_used_class(Src, Name) :-
    assert(used_class(Name, Src)).

assert_defined_class(Src, Name, _Meta, _Super, _) :-
    defined_class(Name, _, _, Src, _),
    !.
assert_defined_class(_, _, _, -, _) :- !.               % :- pce_extend_class
assert_defined_class(Src, Name, Meta, Super, Summary) :-
    current_source_line(Line),
    (   Summary == @(default)
    ->  Atom = ''
    ;   is_list(Summary)
    ->  atom_codes(Atom, Summary)
    ;   string(Summary)
    ->  atom_concat(Summary, '', Atom)
    ),
    assert(defined_class(Name, Super, Atom, Src, Line)),
    (   Meta = @(_)
    ->  true
    ;   assert_used_class(Src, Meta)
    ),
    assert_used_class(Src, Super).

assert_defined_class(Src, Name, imported_from(_File)) :-
    defined_class(Name, _, _, Src, _),
    !.
assert_defined_class(Src, Name, imported_from(File)) :-
    assert(defined_class(Name, _, '', Src, file(File))).


                /********************************
                *            UTILITIES          *
                ********************************/

%!  generalise(+Callable, -General)
%
%   Generalise a callable term.

generalise(Var, Var) :-
    var(Var),
    !.                    % error?
generalise(pce_principal:send_implementation(Id, _, _),
           pce_principal:send_implementation(Id, _, _)) :-
    atom(Id),
    !.
generalise(pce_principal:get_implementation(Id, _, _, _),
           pce_principal:get_implementation(Id, _, _, _)) :-
    atom(Id),
    !.
generalise('<directive>'(Line), '<directive>'(Line)) :- !.
generalise(Module:Goal0, Module:Goal) :-
    atom(Module),
    !,
    generalise(Goal0, Goal).
generalise(Term0, Term) :-
    callable(Term0),
    generalise_term(Term0, Term).


                 /*******************************
                 *      SOURCE MANAGEMENT       *
                 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This section of the file contains   hookable  predicates to reason about
sources. The built-in code here  can  only   deal  with  files. The XPCE
library(pce_prolog_xref) provides hooks to deal with XPCE objects, so we
can do cross-referencing on PceEmacs edit   buffers.  Other examples for
hooking can be databases, (HTTP) URIs, etc.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- multifile
    prolog:xref_source_directory/2, % +Source, -Dir
    prolog:xref_source_file/3.      % +Spec, -Path, +Options


%!  xref_source_file(+Spec, -File, +Src) is semidet.
%!  xref_source_file(+Spec, -File, +Src, +Options) is semidet.
%
%   Find named source file from Spec, relative to Src.

xref_source_file(Plain, File, Source) :-
    xref_source_file(Plain, File, Source, []).

xref_source_file(QSpec, File, Source, Options) :-
    nonvar(QSpec), QSpec = _:Spec,
    !,
    must_be(acyclic, Spec),
    xref_source_file(Spec, File, Source, Options).
xref_source_file(Spec, File, Source, Options) :-
    nonvar(Spec),
    prolog:xref_source_file(Spec, File,
                            [ relative_to(Source)
                            | Options
                            ]),
    !.
xref_source_file(Plain, File, Source, Options) :-
    atom(Plain),
    \+ is_absolute_file_name(Plain),
    (   prolog:xref_source_directory(Source, Dir)
    ->  true
    ;   atom(Source),
        file_directory_name(Source, Dir)
    ),
    atomic_list_concat([Dir, /, Plain], Spec0),
    absolute_file_name(Spec0, Spec),
    do_xref_source_file(Spec, File, Options),
    !.
xref_source_file(Spec, File, Source, Options) :-
    do_xref_source_file(Spec, File,
                        [ relative_to(Source)
                        | Options
                        ]),
    !.
xref_source_file(_, _, _, Options) :-
    option(silent(true), Options),
    !,
    fail.
xref_source_file(Spec, _, Src, _Options) :-
    verbose(Src),
    print_message(warning, error(existence_error(file, Spec), _)),
    fail.

do_xref_source_file(Spec, File, Options) :-
    nonvar(Spec),
    option(file_type(Type), Options, prolog),
    absolute_file_name(Spec, File,
                       [ file_type(Type),
                         access(read),
                         file_errors(fail)
                       ]),
    !.

%!  canonical_source(?Source, ?Src) is det.
%
%   Src is the canonical version of Source if Source is given.

canonical_source(Source, Src) :-
    (   ground(Source)
    ->  prolog_canonical_source(Source, Src)
    ;   Source = Src
    ).

%!  goal_name_arity(+Goal, -Name, -Arity)
%
%   Generalized version of  functor/3  that   can  deal  with name()
%   goals.

goal_name_arity(Goal, Name, Arity) :-
    (   compound(Goal)
    ->  compound_name_arity(Goal, Name, Arity)
    ;   atom(Goal)
    ->  Name = Goal, Arity = 0
    ).

generalise_term(Specific, General) :-
    (   compound(Specific)
    ->  compound_name_arity(Specific, Name, Arity),
        compound_name_arity(General, Name, Arity)
    ;   General = Specific
    ).

functor_name(Term, Name) :-
    (   compound(Term)
    ->  compound_name_arity(Term, Name, _)
    ;   atom(Term)
    ->  Name = Term
    ).

rename_goal(Goal0, Name, Goal) :-
    (   compound(Goal0)
    ->  compound_name_arity(Goal0, _, Arity),
        compound_name_arity(Goal, Name, Arity)
    ;   Goal = Name
    ).
