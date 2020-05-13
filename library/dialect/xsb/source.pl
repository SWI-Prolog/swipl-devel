/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2019-2020, VU University Amsterdam
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

:- module(xsb_source, []).
:- autoload(library(apply),  [convlist/3,partition/4]).
:- autoload(library(debug),  [debug/3]).
:- autoload(library(error),  [instantiation_error/1]).
:- autoload(library(occurs), [sub_term/2]).

/** <module> Support XSB source .P files

This module is a lightweight module that  allows loading .P files as XSB
source  files.  This   module   is   intended    to   be   loaded   from
``<config>/init.pl``, providing transparent usage  of   XSB  files  with
neglectable impact impact if no XSB sources are used.
*/

% xsb_max_file_size is used for buffering  the   source  in  memory when
% reading a source file through the   gpp preprocessor. Eventually, this
% should probably create an intermediate file.

:- create_prolog_flag(xsb_max_file_size, 100 000 000,
                      [ keep(true)
                      ]).

:- multifile
    user:prolog_file_type/2,
    user:term_expansion/2.

user:prolog_file_type('P', prolog).

user:term_expansion(begin_of_file, Out) :-
    prolog_load_context(file, File),
    file_name_extension(Path, 'P', File),
    include_options(File, Include),
    compiler_options(COptions),
    '$append'(Include, COptions, Extra),
    xsb_directives(File, Directives),
    directive_exports(Directives, Public, Directives1),
    (   Public == []
    ->  Out = Out1
    ;   file_base_name(Path, Module),
        Out = [ (:- module(Module, Public))
              | Out1
              ]
    ),
    Out1 = [ (:- expects_dialect(xsb)),
             (:- use_module(library(tables)))
           | Out2
           ],
    '$append'(Extra, More, Out2),
    (   nonvar(Module)
    ->  setup_call_cleanup(
            '$set_source_module'(OldM, Module),
            phrase(head_directives(Directives1, File), More),
            '$set_source_module'(OldM))
    ;   phrase(head_directives(Directives1, File), More)
    ),
    debug(xsb(header), '~p: directives: ~p', [File, More]).

include_options(File, Option) :-
    (   xsb_header_file(File, FileH)
    ->  Option = [(:- include(FileH))]
    ;   Option = []
    ).

:- multifile xsb:xsb_compiler_option/1.
:- dynamic   xsb:xsb_compiler_option/1.

compiler_options(Directives) :-
    findall(D, mapped_xsb_option(D), Directives).

mapped_xsb_option((:- D)) :-
    xsb:xsb_compiler_option(O),
    map_compiler_option(O, D).

map_compiler_option(singleton_warnings_off, style_check(-singleton)).
map_compiler_option(optimize,               set_prolog_flag(optimise, true)).

xsb_header_file(File, FileH) :-
    file_name_extension(Base, _, File),
    file_name_extension(Base, 'H', FileH),
    exists_file(FileH).

%!  directive_exports(+AllDirectives, -Public, -OtherDirectives)

directive_exports(AllDirectives, Exports, RestDirectives) :-
    partition(is_export, AllDirectives, ExportDirectives, RestDirectives),
    phrase(exports(ExportDirectives), Exports).

is_export(export(_)).

exports([]) -->
    [].
exports([export(H)|T]) -->
    export_decl(H),
    exports(T).

export_decl(Var) -->
    { var(Var),
      !,
      instantiation_error(Var)
    }.
export_decl((A,B)) -->
    !,
    export_decl(A),
    export_decl(B).
export_decl(PI) -->
    [PI].

%!  head_directives(+Directives, +File)// is det.
%!  head_directives_s(+Directives, +State)// is det.

head_directives(Directives, File) -->
    { current_prolog_flag(max_table_subgoal_size_action, Action),
      (   current_prolog_flag(max_table_subgoal_size, Size)
      ->  true
      ;   Size = -1
      )
    },
    head_directives_s(Directives,
                      #{file: File,
                        max_table_subgoal_size_action: Action,
                        max_table_subgoal_size:Size
                       }).


head_directives_s([], _) --> [].
head_directives_s([H|T], State0) -->
    { update_state(H, State0, State) },
    !,
    head_directives_s(T, State).
head_directives_s([H|T], State) -->
    head_directive(H, State),
    head_directives_s(T, State).

update_state(set_prolog_flag(max_table_subgoal_size_action, Action),
             State0, State) :-
    State = State0.put(max_table_subgoal_size_action, Action).
update_state(set_prolog_flag(max_table_subgoal_size, Size),
             State0, State) :-
    State = State0.put(max_table_subgoal_size, Size).

%!  head_directive(+Directive, +State)// is det.

head_directive(import(from(Preds, From)), State) -->
    !,
    { assertz(xsb:moved_directive(State.file, import(from(Preds, From))))
    },
    [ (:- xsb_import(Preds, From)) ].
head_directive(table(Preds as XSBOptions), State) -->
    !,
    { ignored_table_options(XSBOptions, Options),
      table_clauses(Preds, Options, Clauses, State),
      assertz(xsb:moved_directive(State.file, table(Preds as XSBOptions)))
    },
    seq(Clauses).
head_directive(table(Preds), State) -->
    !,
    { table_clauses(Preds, true, Clauses, State),
      assertz(xsb:moved_directive(State.file, table(Preds)))
    },
    seq(Clauses).
head_directive(_, _) -->
    [].

seq([]) --> [].
seq([H|T]) --> [H], seq(T).

ignored_table_options((A0,B0), Conj) :-
    !,
    ignored_table_options(A0, A),
    ignored_table_options(B0, B),
    mkconj(A, B, Conj).
ignored_table_options(Option, Option) :-
    supported_table_option(Option),
    !.
ignored_table_options(opaque, true) :-
    !.
ignored_table_options(Option, true) :-
    print_message(warning, xsb(table_option_ignored(Option))).

supported_table_option(variant).
supported_table_option(subsumptive).
supported_table_option(incremental).
supported_table_option(shared).
supported_table_option(private).
supported_table_option(max_answers(_)).
supported_table_option(subgoal_abstract(_)).
supported_table_option(answer_abstract(_)).

mkconj(true, X, X) :- !.
mkconj(X, true, X) :- !.
mkconj(X, Y, (X,Y)) :- !.

table_clauses(Preds, Options0, Clauses, State) :-
    add_defaults(Options0, Options, State),
    (   Options == true
    ->  expand_term((:- table(Preds)), Clauses)
    ;   expand_term((:- table(Preds as Options)), Clauses)
    ).

add_defaults(Opts, Opts, _) :-
    sub_term(subgoal_abstract(_), Opts),
    !.
add_defaults(Opts0, Opts, State) :-
    #{max_table_subgoal_size_action:abstract,
      max_table_subgoal_size:Size} :< State,
    Size >= 0,
    !,
    mkconj(Opts0, subgoal_abstract(Size), Opts).
add_defaults(Opts, Opts, _).

%!  xsb_directives(+File, -Directives) is semidet.
%
%   Directives is a list of all directives in File and its header.
%
%   @bug: track :- op/3 declarations to update the syntax.

xsb_directives(File, Directives) :-
    setup_call_cleanup(
        '$push_input_context'(xsb_directives),
        xsb_directives_aux(File, Directives),
        '$pop_input_context').

xsb_directives_aux(File, Directives) :-
    xsb_header_file(File, FileH),
    !,
    setup_call_cleanup(
        open(FileH, read, In),
        findall(D, stream_directive(In, D), Directives, PDirectives),
        close(In)),
    xsb_P_directives(PDirectives).
xsb_directives_aux(_File, Directives) :-
    xsb_P_directives(Directives).

xsb_P_directives(Directives) :-
    prolog_load_context(stream, In),
    stream_property(In, reposition(true)),
    !,
    setup_call_cleanup(
        stream_property(In, position(Pos)),
        findall(PI, stream_directive(In, PI), Directives),
        set_stream_position(In, Pos)).
xsb_P_directives(Directives) :-
    prolog_load_context(stream, In),
    current_prolog_flag(xsb_max_file_size, MaxSize),
    peek_string(In, MaxSize, String),
    setup_call_cleanup(
        open_string(String, In2),
        findall(PI, stream_directive(In2, PI), Directives),
        close(In2)).

stream_directive(In, Directive) :-
    repeat,
        read_term(In, Term,
                  [ syntax_errors(quiet),
                    module(xsb_source)
                  ]),
        (   Term == end_of_file
        ->  !, fail
        ;   Term = (:- Directive),
            nonvar(Directive)
        ;   fail
        ).

% define the typical XSB operators to limit syntax errors while
% scanning for :- export(_).
:- op(1050,  fy, import).
:- op(1100,  fx, export).
:- op(1100,  fx, mode).
:- op(1040, xfx, from).
:- op(1100,  fy, index).
:- op(1100,  fy, ti).
:- op(1045, xfx, as).
:- op(900,   fy, tnot).
