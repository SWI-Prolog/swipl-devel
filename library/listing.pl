/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2001-2018, University of Amsterdam
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

:- module(prolog_listing,
        [ listing/0,
          listing/1,			% :Spec
          listing/2,                    % :Spec, +Options
          portray_clause/1,             % +Clause
          portray_clause/2,             % +Stream, +Clause
          portray_clause/3              % +Stream, +Clause, +Options
        ]).
:- use_module(library(lists)).
:- use_module(library(settings)).
:- use_module(library(option)).
:- use_module(library(error)).
:- use_module(library(debug)).
:- use_module(library(ansi_term)).
:- use_module(library(prolog_clause)).
:- set_prolog_flag(generate_debug_info, false).

:- module_transparent
    listing/0.
:- meta_predicate
    listing(:),
    listing(:, +),
    portray_clause(+,+,:).

:- predicate_options(portray_clause/3, 3, [pass_to(system:write_term/3, 3)]).

:- multifile
    prolog:locate_clauses/2.        % +Spec, -ClauseRefList

/** <module> List programs and pretty print clauses

This module implements listing code from  the internal representation in
a human readable format.

    * listing/0 lists a module.
    * listing/1 lists a predicate or matching clause
    * listing/2 lists a predicate or matching clause with options
    * portray_clause/2 pretty-prints a clause-term

Layout can be customized using library(settings). The effective settings
can be listed using list_settings/1 as   illustrated below. Settings can
be changed using set_setting/2.

    ==
    ?- list_settings(listing).
    ========================================================================
    Name                      Value (*=modified) Comment
    ========================================================================
    listing:body_indentation  4              Indentation used goals in the body
    listing:tab_distance      0              Distance between tab-stops.
    ...
    ==

@tbd    More settings, support _|Coding Guidelines for Prolog|_ and make
        the suggestions there the default.
@tbd    Provide persistent user customization
*/

:- setting(listing:body_indentation, nonneg, 4,
           'Indentation used goals in the body').
:- setting(listing:tab_distance, nonneg, 0,
           'Distance between tab-stops.  0 uses only spaces').
:- setting(listing:cut_on_same_line, boolean, false,
           'Place cuts (!) on the same line').
:- setting(listing:line_width, nonneg, 78,
           'Width of a line.  0 is infinite').
:- setting(listing:comment_ansi_attributes, list, [fg(green)],
           'ansi_format/3 attributes to print comments').


%!  listing
%
%   Lists all predicates defined  in   the  calling module. Imported
%   predicates are not listed. To  list   the  content of the module
%   `mymodule`, use one of the calls below.
%
%     ```
%     ?- mymodule:listing.
%     ?- listing(mymodule:_).
%     ```

listing :-
    context_module(Context),
    list_module(Context, []).

list_module(Module, Options) :-
    (   current_predicate(_, Module:Pred),
        \+ predicate_property(Module:Pred, imported_from(_)),
        strip_module(Pred, _Module, Head),
        functor(Head, Name, _Arity),
        (   (   predicate_property(Module:Pred, built_in)
            ;   sub_atom(Name, 0, _, _, $)
            )
        ->  current_prolog_flag(access_level, system)
        ;   true
        ),
        nl,
        list_predicate(Module:Head, Module, Options),
        fail
    ;   true
    ).


%!  listing(:What) is det.
%!  listing(:What, +Options) is det.
%
%   List matching clauses. What is either a plain specification or a
%   list of specifications. Plain specifications are:
%
%     * Predicate indicator (Name/Arity or Name//Arity)
%     Lists the indicated predicate.  This also outputs relevant
%     _declarations_, such as multifile/1 or dynamic/1.
%
%     * A _Head_ term.  In this case, only clauses whose head
%     unify with _Head_ are listed.  This is illustrated in the
%     query below that only lists the first clause of append/3.
%
%       ==
%       ?- listing(append([], _, _)).
%       lists:append([], L, L).
%       ==
%
%    The following options are defined:
%
%      - variable_names(+How)
%      One of `source` (default) or `generated`.  If `source`, for each
%      clause that is associated to a source location the system tries
%      to restore the original variable names.  This may fail if macro
%      expansion is not reversible or the term cannot be read due to
%      different operator declarations.  In that case variable names
%      are generated.
%
%      - source(+Bool)
%      If `true` (default `false`), extract the lines from the source
%      files that produced the clauses, i.e., list the original source
%      text rather than the _decompiled_ clauses. Each set of contiguous
%      clauses is preceded by a comment that indicates the file and
%      line of origin.  Clauses that cannot be related to source code
%      are decompiled where the comment indicates the decompiled state.
%      This is notably practical for collecting the state of _multifile_
%      predicates.  For example:
%
%         ```
%         ?- listing(file_search_path, [source(true)]).
%         ```

listing(Spec) :-
    listing(Spec, []).

listing(Spec, Options) :-
    call_cleanup(
        listing_(Spec, Options),
        close_sources).

listing_(M:Spec, Options) :-
    var(Spec),
    !,
    list_module(M, Options).
listing_(M:List, Options) :-
    is_list(List),
    !,
    forall(member(Spec, List),
           listing_(M:Spec, Options)).
listing_(X, Options) :-
    (   prolog:locate_clauses(X, ClauseRefs)
    ->  strip_module(X, Context, _),
        list_clauserefs(ClauseRefs, Context, Options)
    ;   '$find_predicate'(X, Preds),
        list_predicates(Preds, X, Options)
    ).

list_clauserefs([], _, _) :- !.
list_clauserefs([H|T], Context, Options) :-
    !,
    list_clauserefs(H, Context, Options),
    list_clauserefs(T, Context, Options).
list_clauserefs(Ref, Context, Options) :-
    @(clause(Head, Body, Ref), Context),
    list_clause(Head, Body, Ref, Context, Options).

%!  list_predicates(:Preds:list(pi), :Spec, +Options) is det.

list_predicates(PIs, Context:X, Options) :-
    member(PI, PIs),
    pi_to_head(PI, Pred),
    unify_args(Pred, X),
    list_define(Pred, DefPred),
    list_predicate(DefPred, Context, Options),
    nl,
    fail.
list_predicates(_, _, _).

list_define(Head, LoadModule:Head) :-
    compound(Head),
    Head \= (_:_),
    functor(Head, Name, Arity),
    '$find_library'(_, Name, Arity, LoadModule, Library),
    !,
    use_module(Library, []).
list_define(M:Pred, DefM:Pred) :-
    '$define_predicate'(M:Pred),
    (   predicate_property(M:Pred, imported_from(DefM))
    ->  true
    ;   DefM = M
    ).

pi_to_head(PI, _) :-
    var(PI),
    !,
    instantiation_error(PI).
pi_to_head(M:PI, M:Head) :-
    !,
    pi_to_head(PI, Head).
pi_to_head(Name/Arity, Head) :-
    functor(Head, Name, Arity).


%       Unify the arguments of the specification with the given term,
%       so we can partially instantate the head.

unify_args(_, _/_) :- !.                % Name/arity spec
unify_args(X, X) :- !.
unify_args(_:X, X) :- !.
unify_args(_, _).

list_predicate(Pred, Context, _) :-
    predicate_property(Pred, undefined),
    !,
    decl_term(Pred, Context, Decl),
    comment('%   Undefined: ~q~n', [Decl]).
list_predicate(Pred, Context, _) :-
    predicate_property(Pred, foreign),
    !,
    decl_term(Pred, Context, Decl),
    comment('%   Foreign: ~q~n', [Decl]).
list_predicate(Pred, Context, Options) :-
    notify_changed(Pred, Context),
    list_declarations(Pred, Context),
    list_clauses(Pred, Context, Options).

decl_term(Pred, Context, Decl) :-
    strip_module(Pred, Module, Head),
    functor(Head, Name, Arity),
    (   hide_module(Module, Context, Head)
    ->  Decl = Name/Arity
    ;   Decl = Module:Name/Arity
    ).


decl(thread_local, thread_local).
decl(dynamic,      dynamic).
decl(volatile,     volatile).
decl(multifile,    multifile).
decl(public,       public).

declaration(Pred, Source, Decl) :-
    decl(Prop, Declname),
    predicate_property(Pred, Prop),
    decl_term(Pred, Source, Funct),
    Decl =.. [ Declname, Funct ].
declaration(Pred, Source, Decl) :-
    predicate_property(Pred, meta_predicate(Head)),
    strip_module(Pred, Module, _),
    (   (Module == system; Source == Module)
    ->  Decl = meta_predicate(Head)
    ;   Decl = meta_predicate(Module:Head)
    ),
    (   meta_implies_transparent(Head)
    ->  !                                   % hide transparent
    ;   true
    ).
declaration(Pred, Source, Decl) :-
    predicate_property(Pred, transparent),
    decl_term(Pred, Source, PI),
    Decl = module_transparent(PI).

%!  meta_implies_transparent(+Head) is semidet.
%
%   True if the meta-declaration Head implies  that the predicate is
%   transparent.

meta_implies_transparent(Head):-
    compound(Head),
    arg(_, Head, Arg),
    implies_transparent(Arg),
    !.

implies_transparent(Arg) :-
    integer(Arg),
    !.
implies_transparent(:).
implies_transparent(//).
implies_transparent(^).


list_declarations(Pred, Source) :-
    findall(Decl, declaration(Pred, Source, Decl), Decls),
    (   Decls == []
    ->  true
    ;   write_declarations(Decls, Source),
        format('~n', [])
    ).


write_declarations([], _) :- !.
write_declarations([H|T], Module) :-
    format(':- ~q.~n', [H]),
    write_declarations(T, Module).

list_clauses(Pred, Source, Options) :-
    strip_module(Pred, Module, Head),
    forall(clause(Pred, Body, Ref),
           list_clause(Module:Head, Body, Ref, Source, Options)).

list_clause(_Head, _Body, Ref, _Source, Options) :-
    option(source(true), Options),
    (   clause_property(Ref, file(File)),
        clause_property(Ref, line_count(Line)),
        catch(source_clause_string(File, Line, String, Repositioned),
              _, fail),
        debug(listing(source), 'Read ~w:~d: "~s"~n', [File, Line, String])
    ->  !,
        (   Repositioned == true
        ->  comment('% From ~w:~d~n', [ File, Line ])
        ;   true
        ),
        writeln(String)
    ;   decompiled
    ->  fail
    ;   asserta(decompiled),
        comment('% From database (decompiled)~n', []),
        fail                                    % try next clause
    ).
list_clause(Module:Head, Body, Ref, Source, Options) :-
    restore_variable_names(Module, Head, Body, Ref, Options),
    write_module(Module, Source, Head),
    portray_clause((Head:-Body)).

%!  restore_variable_names(+Module, +Head, +Body, +Ref, +Options) is det.
%
%   Try to restore the variable names  from   the  source  if the option
%   variable_names(source) is true.

restore_variable_names(Module, Head, Body, Ref, Options) :-
    option(variable_names(source), Options, source),
    catch(clause_info(Ref, _, _, _,
                      [ head(QHead),
                        body(Body),
                        variable_names(Bindings)
                      ]),
          _, true),
    unify_head(Module, Head, QHead),
    !,
    bind_vars(Bindings),
    name_other_vars((Head:-Body), Bindings).
restore_variable_names(_,_,_,_,_).

unify_head(Module, Head, Module:Head) :-
    !.
unify_head(_, Head, Head) :-
    !.
unify_head(_, _, _).

bind_vars([]) :-
    !.
bind_vars([Name = Var|T]) :-
    Var = '$VAR'(Name),
    bind_vars(T).

%!  name_other_vars(+Term, +Bindings) is det.
%
%   Give a '$VAR'(N) name to all   remaining variables in Term, avoiding
%   clashes with the given variable names.

name_other_vars(Term, Bindings) :-
    term_singletons(Term, Singletons),
    bind_singletons(Singletons),
    term_variables(Term, Vars),
    name_vars(Vars, 0, Bindings).

bind_singletons([]).
bind_singletons(['$VAR'('_')|T]) :-
    bind_singletons(T).

name_vars([], _, _).
name_vars([H|T], N, Bindings) :-
    between(N, infinite, N2),
    var_name(N2, Name),
    \+ memberchk(Name=_, Bindings),
    !,
    H = '$VAR'(N2),
    N3 is N2 + 1,
    name_vars(T, N3, Bindings).

var_name(I, Name) :-               % must be kept in sync with writeNumberVar()
    L is (I mod 26)+0'A,
    N is I // 26,
    (   N == 0
    ->  char_code(Name, L)
    ;   format(atom(Name), '~c~d', [L, N])
    ).

write_module(Module, Context, Head) :-
    hide_module(Module, Context, Head),
    !.
write_module(Module, _, _) :-
    format('~q:', [Module]).

hide_module(system, Module, Head) :-
    predicate_property(Module:Head, imported_from(M)),
    predicate_property(system:Head, imported_from(M)),
    !.
hide_module(Module, Module, _) :- !.

notify_changed(Pred, Context) :-
    strip_module(Pred, user, Head),
    predicate_property(Head, built_in),
    \+ predicate_property(Head, (dynamic)),
    !,
    decl_term(Pred, Context, Decl),
    comment('%   NOTE: system definition has been overruled for ~q~n',
            [Decl]).
notify_changed(_, _).

%!  source_clause_string(+File, +Line, -String, -Repositioned)
%
%   True when String is the source text for a clause starting at Line in
%   File.

source_clause_string(File, Line, String, Repositioned) :-
    open_source(File, Line, Stream, Repositioned),
    stream_property(Stream, position(Start)),
    '$raw_read'(Stream, _TextWithoutComments),
    stream_property(Stream, position(End)),
    stream_position_data(char_count, Start, StartChar),
    stream_position_data(char_count, End, EndChar),
    Length is EndChar - StartChar,
    set_stream_position(Stream, Start),
    read_string(Stream, Length, String),
    skip_blanks_and_comments(Stream, blank).

skip_blanks_and_comments(Stream, _) :-
    at_end_of_stream(Stream),
    !.
skip_blanks_and_comments(Stream, State0) :-
    peek_string(Stream, 80, String),
    string_chars(String, Chars),
    phrase(blanks_and_comments(State0, State), Chars, Rest),
    (   Rest == []
    ->  read_string(Stream, 80, _),
        skip_blanks_and_comments(Stream, State)
    ;   length(Chars, All),
        length(Rest, RLen),
        Skip is All-RLen,
        read_string(Stream, Skip, _)
    ).

blanks_and_comments(State0, State) -->
    [C],
    { transition(C, State0, State1) },
    !,
    blanks_and_comments(State1, State).
blanks_and_comments(State, State) -->
    [].

transition(C, blank, blank) :-
    char_type(C, space).
transition('%', blank, line_comment).
transition('\n', line_comment, blank).
transition(_, line_comment, line_comment).
transition('/', blank, comment_0).
transition('/', comment(N), comment(N,/)).
transition('*', comment(N,/), comment(N1)) :-
    N1 is N + 1.
transition('*', comment_0, comment(1)).
transition('*', comment(N), comment(N,*)).
transition('/', comment(N,*), State) :-
    (   N == 1
    ->  State = blank
    ;   N2 is N - 1,
        State = comment(N2)
    ).


open_source(File, Line, Stream, Repositioned) :-
    source_stream(File, Stream, Pos0, Repositioned),
    line_count(Stream, Line0),
    (   Line >= Line0
    ->  Skip is Line - Line0
    ;   set_stream_position(Stream, Pos0),
        Skip is Line - 1
    ),
    debug(listing(source), '~w: skip ~d to ~d', [File, Line0, Line]),
    (   Skip =\= 0
    ->  Repositioned = true
    ;   true
    ),
    forall(between(1, Skip, _),
           skip(Stream, 0'\n)).

:- thread_local
    opened_source/3,
    decompiled/0.

source_stream(File, Stream, Pos0, _) :-
    opened_source(File, Stream, Pos0),
    !.
source_stream(File, Stream, Pos0, true) :-
    open(File, read, Stream),
    stream_property(Stream, position(Pos0)),
    asserta(opened_source(File, Stream, Pos0)).

close_sources :-
    retractall(decompiled),
    forall(retract(opened_source(_,Stream,_)),
           close(Stream)).


%!  portray_clause(+Clause) is det.
%!  portray_clause(+Out:stream, +Clause) is det.
%!  portray_clause(+Out:stream, +Clause, +Options) is det.
%
%   Portray `Clause' on the current  output   stream.  Layout of the
%   clause is to our best standards.   As  the actual variable names
%   are not available we use A, B, ... Deals with ';', '|', '->' and
%   calls via meta-call predicates as determined using the predicate
%   property   meta_predicate.   If   Clause   contains   attributed
%   variables, these are treated as normal variables.
%
%   If  Options  is  provided,   the    option-list   is  passed  to
%   write_term/3 that does the final writing of arguments.

%       The prolog_list_goal/1 hook is  a  dubious   as  it  may lead to
%       confusion if the heads relates to other   bodies.  For now it is
%       only used for XPCE methods and works just nice.
%
%       Not really ...  It may confuse the source-level debugger.

%portray_clause(Head :- _Body) :-
%       user:prolog_list_goal(Head), !.
portray_clause(Term) :-
    current_output(Out),
    portray_clause(Out, Term).

portray_clause(Stream, Term) :-
    must_be(stream, Stream),
    portray_clause(Stream, Term, []).

portray_clause(Stream, Term, M:Options) :-
    must_be(list, Options),
    meta_options(is_meta, M:Options, QOptions),
    \+ \+ ( copy_term_nat(Term, Copy),
            numbervars(Copy, 0, _,
                       [ singletons(true)
                       ]),
            do_portray_clause(Stream, Copy, QOptions)
          ).

is_meta(portray_goal).

do_portray_clause(Out, Var, Options) :-
    var(Var),
    !,
    pprint(Out, Var, 1200, Options).
do_portray_clause(Out, (Head :- true), Options) :-
    !,
    pprint(Out, Head, 1200, Options),
    full_stop(Out).
do_portray_clause(Out, Term, Options) :-
    clause_term(Term, Head, Neck, Body),
    !,
    inc_indent(0, 1, Indent),
    infix_op(Neck, RightPri, LeftPri),
    pprint(Out, Head, LeftPri, Options),
    format(Out, ' ~w', [Neck]),
    (   nonvar(Body),
        Body = Module:LocalBody,
        \+ primitive(LocalBody)
    ->  nlindent(Out, Indent),
        format(Out, '~q', [Module]),
        '$put_token'(Out, :),
        nlindent(Out, Indent),
        write(Out, '(   '),
        inc_indent(Indent, 1, BodyIndent),
        portray_body(LocalBody, BodyIndent, noindent, 1200, Out, Options),
        nlindent(Out, Indent),
        write(Out, ')')
    ;   setting(listing:body_indentation, BodyIndent),
        portray_body(Body, BodyIndent, indent, RightPri, Out, Options)
    ),
    full_stop(Out).
do_portray_clause(Out, (:-use_module(File, Imports)), Options) :-
    length(Imports, Len),
    Len > 3,
    !,
    format(Out, ':- use_module(~q,', [File]),
    portray_list(Imports, 14, Out, Options),
    write(Out, ').\n').
do_portray_clause(Out, (:-module(Module, Exports)), Options) :-
    !,
    format(Out, ':- module(~q,', [Module]),
    portray_list(Exports, 10, Out, Options),
    write(Out, ').\n').
do_portray_clause(Out, (:-Directive), Options) :-
    !,
    write(Out, ':- '),
    portray_body(Directive, 3, noindent, 1199, Out, Options),
    full_stop(Out).
do_portray_clause(Out, Fact, Options) :-
    portray_body(Fact, 0, noindent, 1200, Out, Options),
    full_stop(Out).

clause_term((Head:-Body), Head, :-, Body).
clause_term((Head-->Body), Head, -->, Body).

full_stop(Out) :-
    '$put_token'(Out, '.'),
    nl(Out).


%!  portray_body(+Term, +Indent, +DoIndent, +Priority, +Out, +Options)
%
%   Write Term at current indentation. If   DoIndent  is 'indent' we
%   must first call nlindent/2 before emitting anything.

portray_body(Var, _, _, Pri, Out, Options) :-
    var(Var),
    !,
    pprint(Out, Var, Pri, Options).
portray_body(!, _, _, _, Out, _) :-
    setting(listing:cut_on_same_line, true),
    !,
    write(Out, ' !').
portray_body((!, Clause), Indent, _, Pri, Out, Options) :-
    setting(listing:cut_on_same_line, true),
    \+ term_needs_braces((_,_), Pri),
    !,
    write(Out, ' !,'),
    portray_body(Clause, Indent, indent, 1000, Out, Options).
portray_body(Term, Indent, indent, Pri, Out, Options) :-
    !,
    nlindent(Out, Indent),
    portray_body(Term, Indent, noindent, Pri, Out, Options).
portray_body(Or, Indent, _, _, Out, Options) :-
    or_layout(Or),
    !,
    write(Out, '(   '),
    portray_or(Or, Indent, 1200, Out, Options),
    nlindent(Out, Indent),
    write(Out, ')').
portray_body(Term, Indent, _, Pri, Out, Options) :-
    term_needs_braces(Term, Pri),
    !,
    write(Out, '( '),
    ArgIndent is Indent + 2,
    portray_body(Term, ArgIndent, noindent, 1200, Out, Options),
    nlindent(Out, Indent),
    write(Out, ')').
portray_body((A,B), Indent, _, _Pri, Out, Options) :-
    !,
    infix_op(',', LeftPri, RightPri),
    portray_body(A, Indent, noindent, LeftPri, Out, Options),
    write(Out, ','),
    portray_body(B, Indent, indent, RightPri, Out, Options).
portray_body(\+(Goal), Indent, _, _Pri, Out, Options) :-
    !,
    write(Out, \+), write(Out, ' '),
    prefix_op(\+, ArgPri),
    ArgIndent is Indent+3,
    portray_body(Goal, ArgIndent, noindent, ArgPri, Out, Options).
portray_body(Call, _, _, _, Out, Options) :- % requires knowledge on the module!
    m_callable(Call),
    option(module(M), Options, user),
    predicate_property(M:Call, meta_predicate(Meta)),
    !,
    portray_meta(Out, Call, Meta, Options).
portray_body(Clause, _, _, Pri, Out, Options) :-
    pprint(Out, Clause, Pri, Options).

m_callable(Term) :-
    strip_module(Term, _, Plain),
    callable(Plain),
    Plain \= (_:_).

term_needs_braces(Term, Pri) :-
    callable(Term),
    functor(Term, Name, _Arity),
    current_op(OpPri, _Type, Name),
    OpPri > Pri,
    !.

%!  portray_or(+Term, +Indent, +Priority, +Out) is det.

portray_or(Term, Indent, Pri, Out, Options) :-
    term_needs_braces(Term, Pri),
    !,
    inc_indent(Indent, 1, NewIndent),
    write(Out, '(   '),
    portray_or(Term, NewIndent, Out, Options),
    nlindent(Out, NewIndent),
    write(Out, ')').
portray_or(Term, Indent, _Pri, Out, Options) :-
    or_layout(Term),
    !,
    portray_or(Term, Indent, Out, Options).
portray_or(Term, Indent, Pri, Out, Options) :-
    inc_indent(Indent, 1, NestIndent),
    portray_body(Term, NestIndent, noindent, Pri, Out, Options).


portray_or((If -> Then ; Else), Indent, Out, Options) :-
    !,
    inc_indent(Indent, 1, NestIndent),
    infix_op((->), LeftPri, RightPri),
    portray_body(If, NestIndent, noindent, LeftPri, Out, Options),
    nlindent(Out, Indent),
    write(Out, '->  '),
    portray_body(Then, NestIndent, noindent, RightPri, Out, Options),
    nlindent(Out, Indent),
    write(Out, ';   '),
    infix_op(;, _LeftPri, RightPri2),
    portray_or(Else, Indent, RightPri2, Out, Options).
portray_or((If *-> Then ; Else), Indent, Out, Options) :-
    !,
    inc_indent(Indent, 1, NestIndent),
    infix_op((*->), LeftPri, RightPri),
    portray_body(If, NestIndent, noindent, LeftPri, Out, Options),
    nlindent(Out, Indent),
    write(Out, '*-> '),
    portray_body(Then, NestIndent, noindent, RightPri, Out, Options),
    nlindent(Out, Indent),
    write(Out, ';   '),
    infix_op(;, _LeftPri, RightPri2),
    portray_or(Else, Indent, RightPri2, Out, Options).
portray_or((If -> Then), Indent, Out, Options) :-
    !,
    inc_indent(Indent, 1, NestIndent),
    infix_op((->), LeftPri, RightPri),
    portray_body(If, NestIndent, noindent, LeftPri, Out, Options),
    nlindent(Out, Indent),
    write(Out, '->  '),
    portray_or(Then, Indent, RightPri, Out, Options).
portray_or((If *-> Then), Indent, Out, Options) :-
    !,
    inc_indent(Indent, 1, NestIndent),
    infix_op((->), LeftPri, RightPri),
    portray_body(If, NestIndent, noindent, LeftPri, Out, Options),
    nlindent(Out, Indent),
    write(Out, '*-> '),
    portray_or(Then, Indent, RightPri, Out, Options).
portray_or((A;B), Indent, Out, Options) :-
    !,
    inc_indent(Indent, 1, NestIndent),
    infix_op(;, LeftPri, RightPri),
    portray_body(A, NestIndent, noindent, LeftPri, Out, Options),
    nlindent(Out, Indent),
    write(Out, ';   '),
    portray_or(B, Indent, RightPri, Out, Options).
portray_or((A|B), Indent, Out, Options) :-
    !,
    inc_indent(Indent, 1, NestIndent),
    infix_op('|', LeftPri, RightPri),
    portray_body(A, NestIndent, noindent, LeftPri, Out, Options),
    nlindent(Out, Indent),
    write(Out, '|   '),
    portray_or(B, Indent, RightPri, Out, Options).


%!  infix_op(+Op, -Left, -Right) is semidet.
%
%   True if Op is an infix operator and Left is the max priority of its
%   left hand and Right is the max priority of its right hand.

infix_op(Op, Left, Right) :-
    current_op(Pri, Assoc, Op),
    infix_assoc(Assoc, LeftMin, RightMin),
    !,
    Left is Pri - LeftMin,
    Right is Pri - RightMin.

infix_assoc(xfx, 1, 1).
infix_assoc(xfy, 1, 0).
infix_assoc(yfx, 0, 1).

prefix_op(Op, ArgPri) :-
    current_op(Pri, Assoc, Op),
    pre_assoc(Assoc, ArgMin),
    !,
    ArgPri is Pri - ArgMin.

pre_assoc(fx, 1).
pre_assoc(fy, 0).

postfix_op(Op, ArgPri) :-
    current_op(Pri, Assoc, Op),
    post_assoc(Assoc, ArgMin),
    !,
    ArgPri is Pri - ArgMin.

post_assoc(xf, 1).
post_assoc(yf, 0).

%!  or_layout(@Term) is semidet.
%
%   True if Term is a control structure for which we want to use clean
%   layout.
%
%   @tbd    Change name.

or_layout(Var) :-
    var(Var), !, fail.
or_layout((_;_)).
or_layout((_->_)).
or_layout((_*->_)).

primitive(G) :-
    or_layout(G), !, fail.
primitive((_,_)) :- !, fail.
primitive(_).


%!  portray_meta(+Out, +Call, +MetaDecl, +Options)
%
%   Portray a meta-call. If Call   contains non-primitive meta-calls
%   we put each argument on a line and layout the body. Otherwise we
%   simply print the goal.

portray_meta(Out, Call, Meta, Options) :-
    contains_non_primitive_meta_arg(Call, Meta),
    !,
    Call =.. [Name|Args],
    Meta =.. [_|Decls],
    format(Out, '~q(', [Name]),
    line_position(Out, Indent),
    portray_meta_args(Decls, Args, Indent, Out, Options),
    format(Out, ')', []).
portray_meta(Out, Call, _, Options) :-
    pprint(Out, Call, 999, Options).

contains_non_primitive_meta_arg(Call, Decl) :-
    arg(I, Call, CA),
    arg(I, Decl, DA),
    integer(DA),
    \+ primitive(CA),
    !.

portray_meta_args([], [], _, _, _).
portray_meta_args([D|DT], [A|AT], Indent, Out, Options) :-
    portray_meta_arg(D, A, Out, Options),
    (   DT == []
    ->  true
    ;   format(Out, ',', []),
        nlindent(Out, Indent),
        portray_meta_args(DT, AT, Indent, Out, Options)
    ).

portray_meta_arg(I, A, Out, Options) :-
    integer(I),
    !,
    line_position(Out, Indent),
    portray_body(A, Indent, noindent, 999, Out, Options).
portray_meta_arg(_, A, Out, Options) :-
    pprint(Out, A, 999, Options).

%!  portray_list(+List, +Indent, +Out)
%
%   Portray a list like this.  Right side for improper lists
%
%           [ element1,             [ element1
%             element2,     OR      | tail
%           ]                       ]

portray_list([], _, Out, _) :-
    !,
    write(Out, []).
portray_list(List, Indent, Out, Options) :-
    nlindent(Out, Indent),
    write(Out, '[ '),
    EIndent is Indent + 2,
    portray_list_elements(List, EIndent, Out, Options),
    nlindent(Out, Indent),
    write(Out, ']').

portray_list_elements([H|T], EIndent, Out, Options) :-
    pprint(Out, H, 999, Options),
    (   T == []
    ->  true
    ;   nonvar(T), T = [_|_]
    ->  write(Out, ','),
        nlindent(Out, EIndent),
        portray_list_elements(T, EIndent, Out, Options)
    ;   Indent is EIndent - 2,
        nlindent(Out, Indent),
        write(Out, '| '),
        pprint(Out, T, 999, Options)
    ).

%!  pprint(+Out, +Term, +Priority, +Options)
%
%   Print  Term  at  Priority.  This  also  takes  care  of  several
%   formatting options, in particular:
%
%     * {}(Arg) terms are printed with aligned arguments, assuming
%     that the term is a body-term.
%     * Terms that do not fit on the line are wrapped using
%     pprint_wrapped/3.
%
%   @tbd    Decide when and how to wrap long terms.

pprint(Out, Term, _, Options) :-
    nonvar(Term),
    Term = {}(Arg),
    line_position(Out, Indent),
    ArgIndent is Indent + 2,
    format(Out, '{ ', []),
    portray_body(Arg, ArgIndent, noident, 1000, Out, Options),
    nlindent(Out, Indent),
    format(Out, '}', []).
pprint(Out, Term, Pri, Options) :-
    (   compound(Term)
    ->  compound_name_arity(Term, _, Arity),
        Arity > 0
    ;   is_dict(Term)
    ),
    \+ nowrap_term(Term),
    setting(listing:line_width, Width),
    Width > 0,
    (   write_length(Term, Len, [max_length(Width)|Options])
    ->  true
    ;   Len = Width
    ),
    line_position(Out, Indent),
    Indent + Len > Width,
    Len > Width/4,                 % ad-hoc rule for deeply nested goals
    !,
    pprint_wrapped(Out, Term, Pri, Options).
pprint(Out, Term, Pri, Options) :-
    listing_write_options(Pri, WrtOptions, Options),
    write_term(Out, Term, WrtOptions).

nowrap_term('$VAR'(_)) :- !.
nowrap_term(_{}) :- !.                  % empty dict
nowrap_term(Term) :-
    functor(Term, Name, Arity),
    current_op(_, _, Name),
    (   Arity == 2
    ->  infix_op(Name, _, _)
    ;   Arity == 1
    ->  (   prefix_op(Name, _)
        ->  true
        ;   postfix_op(Name, _)
        )
    ).


pprint_wrapped(Out, Term, _, Options) :-
    Term = [_|_],
    !,
    line_position(Out, Indent),
    portray_list(Term, Indent, Out, Options).
pprint_wrapped(Out, Dict, _, Options) :-
    is_dict(Dict),
    !,
    dict_pairs(Dict, Tag, Pairs),
    pprint(Out, Tag, 1200, Options),
    format(Out, '{ ', []),
    line_position(Out, Indent),
    pprint_nv(Pairs, Indent, Out, Options),
    nlindent(Out, Indent-2),
    format(Out, '}', []).
pprint_wrapped(Out, Term, _, Options) :-
    Term =.. [Name|Args],
    format(Out, '~q(', Name),
    line_position(Out, Indent),
    pprint_args(Args, Indent, Out, Options),
    format(Out, ')', []).

pprint_args([], _, _, _).
pprint_args([H|T], Indent, Out, Options) :-
    pprint(Out, H, 999, Options),
    (   T == []
    ->  true
    ;   format(Out, ',', []),
        nlindent(Out, Indent),
        pprint_args(T, Indent, Out, Options)
    ).


pprint_nv([], _, _, _).
pprint_nv([Name-Value|T], Indent, Out, Options) :-
    pprint(Out, Name, 999, Options),
    format(Out, ':', []),
    pprint(Out, Value, 999, Options),
    (   T == []
    ->  true
    ;   format(Out, ',', []),
        nlindent(Out, Indent),
        pprint_nv(T, Indent, Out, Options)
    ).


%!  listing_write_options(+Priority, -WriteOptions) is det.
%
%   WriteOptions are write_term/3 options for writing a term at
%   priority Priority.

listing_write_options(Pri,
                      [ quoted(true),
                        numbervars(true),
                        priority(Pri),
                        spacing(next_argument)
                      | Options
                      ],
                      Options).

%!  nlindent(+Out, +Indent)
%
%   Write newline and indent to  column   Indent.  Uses  the setting
%   listing:tab_distance to determine the mapping   between tabs and
%   spaces.

nlindent(Out, N) :-
    nl(Out),
    setting(listing:tab_distance, D),
    (   D =:= 0
    ->  tab(Out, N)
    ;   Tab is N // D,
        Space is N mod D,
        put_tabs(Out, Tab),
        tab(Out, Space)
    ).

put_tabs(Out, N) :-
    N > 0,
    !,
    put(Out, 0'\t),
    NN is N - 1,
    put_tabs(Out, NN).
put_tabs(_, _).


%!  inc_indent(+Indent0, +Inc, -Indent)
%
%   Increment the indent with logical steps.

inc_indent(Indent0, Inc, Indent) :-
    Indent is Indent0 + Inc*4.

:- multifile
    sandbox:safe_meta/2.

sandbox:safe_meta(listing(What), []) :-
    not_qualified(What).

not_qualified(Var) :-
    var(Var),
    !.
not_qualified(_:_) :- !, fail.
not_qualified(_).


%!  comment(+Format, +Args)
%
%   Emit a comment.

comment(Format, Args) :-
    stream_property(current_output, tty(true)),
    setting(listing:comment_ansi_attributes, Attributes),
    Attributes \== [],
    !,
    ansi_format(Attributes, Format, Args).
comment(Format, Args) :-
    format(Format, Args).
