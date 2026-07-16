/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org
    Copyright (c)  2026, SWI-Prolog Solutions b.v.
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

:- module(test_predicate_options,
          [ test_predicate_options/0
          ]).
:- use_module(library(plunit)).
:- use_module(library(predicate_options)).
:- use_module(library(apply), [maplist/3, exclude/3, maplist/2]).
:- use_module(library(lists), [member/2, append/3, append/2, reverse/2]).
:- use_module(library(readutil), [read_line_to_string/2]).
:- use_module(library(filesex), [directory_file_path/3]).
:- use_module(library(edit), []). % public predicate_location/2

/** <module> Test library(predicate_options)

Includes  a  _drift  check_  that    verifies  the  hand-written  option
declarations in library(dialect/swi/syspred_options) still   cover every
option   in   the   corresponding   C   ``PL_option_t[]``   array.   See
predopts_drift/1.
*/

%!  source_dir(-Dir) is semidet.
%
%   True when Dir is the root   of SWI-Prolog source distribution. Fails
%   if there is no source.

source_dir(Dir) :-
    source_file(source_dir(_), File),
    file_directory_name(File, TestLibs),
    file_directory_name(TestLibs, Tests),
    file_directory_name(Tests, Dir),
    directory_file_path(Dir, 'src/pl-prims.c', Prims),
    exists_file(Prims).

%!  drift_testable is semidet.
%
%   True when the drift check can run: the source tree is present and
%   predicate_location/2 resolves a foreign (C) predicate to its source
%   file.  The latter fails on some platforms (e.g. WASM), where every C
%   option array would otherwise be reported as stale.

drift_testable :-
    source_dir(_),
    prolog_edit:predicate_location(open(_,_,_,_), Loc),
    get_dict(file, Loc, _).

test_predicate_options :-
    run_tests([ predicate_options_check,
                predicate_options_lint
              ]),
    (   drift_testable
    ->  run_tests([ predicate_options_drift
                  ])
    ;   true
    ).

                 /*******************************
                 *          CHECK CALLS         *
                 *******************************/

:- begin_tests(predicate_options_check).

% A supported option validates, an unsupported one is reported.

test(good_option) :-
    catch(check_predicate_option(system:open/4, 4, type(text)), E, true),
    var(E).
test(bad_option_value, error(type_error(_, _), _)) :-
    check_predicate_option(system:open/4, 4, type(no_such_type_value)).
test(bad_option_name, error(existence_error(option, _), _)) :-
    check_predicate_option(system:open/4, 4, no_such_option(x)).

% The new mirrors added in this change are queryable.

test(thread_wait_decl, [condition(current_prolog_flag(threads, true))]) :-
    once(current_predicate_option(system:thread_wait/2, 2, retry_every(_))),
    once(current_predicate_option(system:thread_wait/2, 2, timeout(_))).
test(transaction_decl) :-
    once(current_predicate_option('$syspreds':transaction/2, 2, bulk(_))).

:- end_tests(predicate_options_check).

                 /*******************************
                 *            LINTER            *
                 *******************************/

% Target predicates for check_raw_option_access/1.  The linter is run on
% each one individually, so they do not interfere with each other or with
% the rest of this file.

:- use_module(library(option)).
:- use_module(library(lists)).

:- predicate_options(raw_use/2, 2, [ verbose(boolean) ]).
raw_use(_X, Options) :-                         % Category A
    ( memberchk(verbose(true), Options) -> true ; true ).

wrap_open(File, Mode, Stream, Options) :-       % Category B
    open(File, Mode, Stream, [encoding(utf8)|Options]).

safe_prolog(Spec, Path) :-                      % negative: leftmost-wins target
    absolute_file_name(Spec, Path, [file_errors(fail)|_Opts]).

proper_list(File, Stream) :-                    % negative: not a partial list
    open(File, read, Stream, [type(binary),encoding(utf8)]).

not_options(X, L) :-                            % negative: not an option list
    ( memberchk(foo(X), L) -> true ; true ).

:- begin_tests(predicate_options_lint).

lint(PI, Kinds) :-
    predicate_options:raw_option_findings(test_predicate_options:PI, Pairs),
    findall(F, member(F-_, Pairs), Kinds).

test(category_a, Opt == verbose(true)) :-
    lint(raw_use/2, Findings),
    member(raw_option_access(_, Opt), Findings).
test(category_b, Prefix == [encoding(utf8)]) :-
    lint(wrap_open/4, Findings),
    member(prepend_override(_, Prefix), Findings).
test(safe_prolog_target, Findings == []) :-
    lint(safe_prolog/2, Findings).
test(proper_list_ok, Findings == []) :-
    lint(proper_list/2, Findings).
test(not_an_option_list, Findings == []) :-
    lint(not_options/2, Findings).

:- end_tests(predicate_options_lint).

:- if(drift_testable).

                 /*******************************
                 *          DRIFT CHECK         *
                 *******************************/

:- begin_tests(predicate_options_drift).

test(no_drift, Drift == []) :-
    predopts_drift(Drift),
    maplist(report_drift, Drift).

report_drift(Drift) :-
    print_message(warning, predopts_drift(Drift)).

:- end_tests(predicate_options_drift).

%!  predopts_drift(-Drift) is det.
%
%   Drift is a list of discrepancies between the C option arrays under
%   SrcRoot/src and the predicate_options/3 declarations loaded from
%   library(dialect/swi/syspred_options).  Each element is one of:
%
%     - missing(PI, Array, Option)
%       Option appears in the C Array scanned for PI but is not declared.
%     - unmapped_array(File, Array, Options)
%       A `PL_option_t Array[]` exists in the C source but is not listed
%       in c_option_array/2, so the checker does not know which predicate
%       it belongs to (likely a new option predicate).
%     - stale_mapping(Array)
%       c_option_array/2 refers to an Array no longer present in the
%       sources.

predopts_drift(Drift) :-
    core_option_files(Files),
    maplist(file_option_arrays, Files, FoundLists),
    append(FoundLists, Found),
    findall(D, array_drift(Found, D), Missing),
    findall(unmapped_array(F,A,Os),
            ( member(A-found(F,Os), Found),
              \+ c_option_array(A, _)
            ),
            Unmapped),
    findall(stale_mapping(A),
            ( c_option_array(A, _),
              \+ member(A-_, Found)
            ),
            Stale),
    append([Missing, Unmapped, Stale], Drift).

array_drift(Found, missing(PI, Array, Option)) :-
    member(Array-found(_File, Options), Found),
    c_option_array(Array, PI),
    PI \= internal(_),
    member(Option, Options),
    \+ ( current_predicate_option(PI, _, Decl),
         functor(Decl, Option, _)
       ).

                 /*******************************
                 *   C ARRAY -> PREDICATE MAP   *
                 *******************************/

%!  c_option_array(?Array, ?Target) is nondet.
%
%   Maps the name of a `PL_option_t[]` array in the core C sources to the
%   predicate whose option list it scans.  Target is a module-qualified
%   PI, or internal(PI) for arrays scanned by internal ($-prefixed)
%   predicates that are deliberately not mirrored in syspred_options.
%   An array shared by several predicates has several clauses.

c_option_array(open4_options,             system:open/4).
c_option_array(close2_options,            system:close/2).
c_option_array(wildcard_options,          system:wildcard_match/3).
c_option_array(prolog_flag_options,       system:create_prolog_flag/3).
c_option_array(prolog_listen_options,     system:prolog_listen/3).
c_option_array(open_shared_object_options, system:open_shared_object/3).
c_option_array(numbervar_options,         system:numbervars/4).
c_option_array(read_clause_options,       system:read_clause/3).
c_option_array(read_term_options,         system:read_term/3).
c_option_array(transaction_options,       '$syspreds':transaction/2).
c_option_array(write_term_options,        system:write_term/3).
c_option_array(write_length_options,      system:write_size/4).
c_option_array(zip_open_stream_options,   system:zip_open_stream/3).
c_option_array(zip_new_file_options,      system:zipper_open_new_file_in_zip/4).
c_option_array(zipopen3_options,          system:zipper_open_current/3).
% Thread option arrays live in pl-thread.c/pl-mutex.c, which are only
% scanned when a thread predicate can be located.  In a single-threaded
% build these predicates are absent (see the matching :- if/1 guard in
% library(dialect/swi/syspred_options)), so drop the mappings to avoid
% flagging their unreachable arrays as stale.
:- if(current_prolog_flag(threads, true)).
c_option_array(mutex_options,             system:mutex_create/2).
c_option_array(make_thread_options,       system:thread_create/3).
c_option_array(make_engine_options,       '$engines':engine_create/4).
c_option_array(message_queue_options,     system:message_queue_create/2).
c_option_array(thread_wait_options,       system:thread_wait/2).
c_option_array(thread_update_options,     system:thread_update/2).
% Shared timeout_options[] is scanned for these three:
c_option_array(timeout_options,           system:thread_send_message/3).
c_option_array(timeout_options,           system:thread_get_message/3).
c_option_array(timeout_options,           system:thread_wait/2).
:- endif.
% Internal ($-prefixed) predicates: intentionally not mirrored.
c_option_array(put_quoted_options,        internal(system:'$put_quoted'/4)).
c_option_array(open_wic_options,          internal(system:'$open_wic'/2)).
c_option_array(transaction_options,       internal(system:'$transaction'/2)).

core_option_files(Files) :-
    findall(File, distinct(File, file_option_file(File)), Files).

file_option_file(File) :-
    c_option_array(_Name, Spec),
    spec_pi(Spec, PI),
    pi_head(PI, Head),
    prolog_edit:predicate_location(Head, Loc),
    File = Loc.get(file).

spec_pi(internal(PI), PI) :-
    !.
spec_pi(PI, PI).


                 /*******************************
                 *        C ARRAY PARSING       *
                 *******************************/

%!  file_option_arrays(+File, -Pairs) is det.
%
%   Pairs is a list Array-found(File, Options), one per
%   `static const PL_option_t Array[] = { ... }` block in File.  Options
%   are the option _names_ (atoms), resolved from the ATOM_<id> entries
%   using src/ATOMS.

file_option_arrays(File, Pairs) :-
    atom_id_map(Map),
    setup_call_cleanup(
        open(File, read, In),
        read_arrays(In, Map, none, [], Raw),
        close(In)),
    maplist(add_file(File), Raw, Pairs).

add_file(File, Name-Options, Name-found(File, Options)).

read_arrays(In, Map, State, Acc0, Pairs) :-
    read_line_to_string(In, Line),
    (   Line == end_of_file
    ->  Pairs = Acc0
    ;   step_array(Line, Map, State, State1, Acc0, Acc1),
        read_arrays(In, Map, State1, Acc1, Pairs)
    ).

% State is none, or array(Name, OptionsSoFar) while inside a block.

step_array(Line, _Map, none, array(Name, []), Acc, Acc) :-
    array_header(Line, Name),
    !.
step_array(_Line, _Map, none, none, Acc, Acc) :- !.
step_array(Line, _Map, array(Name, Opts), none,
           Acc, [Name-OptsF|Acc]) :-
    sub_string(Line, _, _, _, "};"),
    !,
    reverse(Opts, OptsF).
step_array(Line, Map, array(Name, Opts0), array(Name, Opts), Acc, Acc) :-
    line_option_ids(Line, Ids),
    convlist(resolve_id(Map), Ids, Names),
    append_new(Names, Opts0, Opts).

array_header(Line, Name) :-
    sub_string(Line, _, _, _, "PL_option_t "),
    sub_string(Line, _, _, _, "[]"),
    split_string(Line, " \t*", " \t*", Parts),
    nth_option_name(Parts, Name0),
    atom_string(Name, Name0).

% The token immediately before "[]" is the array name.
nth_option_name(Parts, Name) :-
    append(_, [Tok|_], Parts),
    string_concat(Name, "[]", Tok),
    !.
nth_option_name(Parts, Name) :-             % "name" and "[]" split apart
    append(_, [Name, "[]"|_], Parts),
    Name \== "".

%!  line_option_ids(+Line, -Ids) is det.
%
%   Ids are the C identifiers following "ATOM_" on Line, excluding the
%   NULL sentinel.  OPT_* tokens never match because "ATOM_" requires a
%   trailing "_".

line_option_ids(Line, Ids) :-
    findall(Id, atom_ref(Line, Id), Ids0),
    exclude(==(''), Ids0, Ids).

atom_ref(Line, Id) :-
    sub_string(Line, Before, _, _, "ATOM_"),
    Start is Before+5,
    sub_string(Line, Start, _, 0, Rest),
    csym_prefix(Rest, IdS),
    IdS \== "",
    atom_string(Id, IdS).

csym_prefix(Str, Prefix) :-
    string_chars(Str, Chars),
    take_csym(Chars, PChars),
    string_chars(Prefix, PChars).

take_csym([C|T], [C|R]) :-
    char_type(C, csym),
    !,
    take_csym(T, R).
take_csym(_, []).

resolve_id(Dict, Id, Name) :-
    Id \== 'NULL',
    atom_string(Name, Dict.get(Id)).

append_new([], Opts, Opts).
append_new([N|T], Opts0, Opts) :-
    (   memberchk(N, Opts0)
    ->  Opts1 = Opts0
    ;   Opts1 = [N|Opts0]
    ),
    append_new(T, Opts1, Opts).

                 /*******************************
                 *          ATOMS TABLE         *
                 *******************************/

%!  atom_id_map(-Dict) is det.
%
%   Assoc from the ATOM_<id> C identifier (as an atom) to its text value,
%   read from src/ATOMS.  Cached in a global for the duration of a check.

:- dynamic atom_id_map_cache/1.

atom_id_map(Dict) :-
    (   atom_id_map_cache(Dict)
    ->  true
    ;   source_dir(Dir),
        directory_file_path(Dir, 'src/ATOMS', File),
        read_atoms(File, Dict),
        asserta(atom_id_map_cache(Dict))
    ).

read_atoms(File, Dict) :-
    setup_call_cleanup(
        open(File, read, In),
        read_atom_lines(In, [], Pairs),
        close(In)),
    dict_create(Dict, #, Pairs).

read_atom_lines(In, Acc0, Pairs) :-
    read_line_to_string(In, Line),
    (   Line == end_of_file
    ->  Pairs = Acc0
    ;   (   atom_line(Line, Id, Text)
        ->  Acc1 = [Id-Text|Acc0]
        ;   Acc1 = Acc0
        ),
        read_atom_lines(In, Acc1, Pairs)
    ).

% "A <name> <string>" defines ATOM_<name> as <string>.  Take the last
% quoted field as the text.
atom_line(Line, Id, Text) :-
    split_string(Line, " \t", " \t", Fields0),
    exclude(==(""), Fields0, Fields),
    Fields = ["A", Name|Rest],
    Rest \== [],
    atom_string(Id, Name),
    quoted_text(Rest, Text).

quoted_text(Fields, Text) :-
    atomics_to_string(Fields, " ", Joined),
    ( sub_string(Joined, B, _, _, "\""),
      sub_string(Joined, B2, _, 0, "\""),
      B2 > B
    -> Len is B2-B-1,
       Start is B+1,
       sub_string(Joined, Start, Len, _, Text)
    ; Fields = [F|_],
      Text = F
    ).

:- multifile prolog:message//1.

prolog:message(predopts_drift(missing(PI, Array, Option))) -->
    [ 'predicate_options drift: ~q scans ~w with option ~q, not declared'
      -[PI, Array, Option] ].
prolog:message(predopts_drift(unmapped_array(File, Array, Options))) -->
    [ 'predicate_options drift: unmapped option array ~w in ~w (~q)'
      -[Array, File, Options] ].
prolog:message(predopts_drift(stale_mapping(Array))) -->
    [ 'predicate_options drift: c_option_array/2 names ~w, gone from sources'
      -[Array] ].

:- endif.                            % source_dir(_)
