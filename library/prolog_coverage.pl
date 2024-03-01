/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org
    Copyright (c)  2006-2024, University of Amsterdam
                              VU University Amsterdam
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

:- module(prolog_coverage,
          [ coverage/1,                 % :Goal
            coverage/2,                 % :Goal, +Options
            show_coverage/1,            % :Options
            show_coverage/2,            % :Goal, +Options (deprecated)
            cov_save_data/2,            % +File, +Options
            cov_load_data/2,            % +File, +Options
            cov_reset/0,                %
            cov_property/1              % ?Property
          ]).
:- autoload(library(apply),
            [exclude/3, maplist/2, convlist/3, maplist/3, maplist/4]).
:- autoload(library(ordsets), [ord_intersection/3, ord_subtract/3, ord_union/3]).
:- autoload(library(pairs),
            [ group_pairs_by_key/2,
              pairs_keys_values/3,
              pairs_values/2,
              map_list_to_pairs/3
            ]).
:- autoload(library(ansi_term), [ansi_format/3]).
:- autoload(library(filesex), [directory_file_path/3, make_directory_path/1]).
:- autoload(library(lists),
            [append/3, flatten/2, max_list/2, member/2, append/2, sum_list/2]).
:- autoload(library(option), [option/2, option/3]).
:- autoload(library(readutil), [read_line_to_string/2]).
:- use_module(library(prolog_breakpoints), []).
:- autoload(library(prolog_clause), [clause_info/4]).
:- autoload(library(solution_sequences), [call_nth/2, distinct/2]).
:- use_module(library(debug), [debug/3, assertion/1]).
:- autoload(library(error), [must_be/2]).
:- autoload(library(prolog_code), [pi_head/2]).
:- autoload(library(terms), [mapsubterms/3]).

:- set_prolog_flag(generate_debug_info, false).

/** <module> Coverage analysis tool

The purpose of this module is to find which part of the program has been
used by a certain goal. Usage is defined   in terms of clauses for which
the _head unification_ succeeded. For each clause  we count how often it
succeeded and how often it  failed.  In   addition  we  track  all _call
sites_, creating goal-by-goal annotated clauses.

The result is  represented  as  a   list  of  clause-references.  As the
references to clauses of dynamic predicates  cannot be guaranteed, these
are omitted from the result.

Using coverage/2 with the option annotate(true),  implied by ext(Ext) or
dir(Dir), the analysis creates a line-by-line   copy of the source files
that is annotated with how many times   this  line was executed and with
what logical results. These annotations rely on relating executable code
to source locations which is shared by the source level debugger. Source
level rewrites due to term or goal expansion may harm the results.

The typical usage is to load the program  and run the query below to get
a report by  file  with  percentages   and  a  directory  `cov`  holding
annotated   files   that   provide     line-by-line   annotations.   See
show_coverage/1 for details.

   ?- coverage(Goal, [dir(cov)]).

## Coverage collection and threads {#coverage-threads}

The coverage collect data structure is   shared  by threads created from
the thread that is collecting  coverage   data.  Currently,  this thread
should be _joined_ before we can operate on the coverage data.

## Combining coverage data from multiple runs {#coverage-merge}

The coverage tools allow  both  combining   data  from  running multiple
queries as combining data from multiple Prolog processes.

For multiple queries in the same process, coverage data may be collected
using  coverage/1  which,  unlike  coverage/2,    does  not  change  the
non-deterministic semantics of the  `Goal`  and   adds  to  the  already
collected data. If no current collection   is in progress, the currently
collected data can be displayed using show_coverage/1.

Coverage data may be saved to a   file using cov_save_data/2. Saved data
can be reloaded using cov_load_data/2. Data   from  multiple Prolog runs
can be combined  in  the  same   file  using  cov_save_data/2  with  the
append(true) option. When possible, file locking  is used to ensure that
concurrect processes can safely use the same   data file. The result can
be shown by loading  the  code  that   was  relevant  to  all  runs, use
cov_load_data/2 and show the result using show_coverage/1.

Note that saving  an  loading  the   coverage  data  saves  and restores
references to the clauses as the Nth clause  of a predicate defined in a
specific file. This implies that the program   must be loaded in exactly
the same way, including  optimization   level,  term/goal  expansion and
order of _multifile_ predicates.

## Predicate reference {#coverage-predicates}
*/

:- meta_predicate
    coverage(0),
    coverage(0,+),                      % :Goal, +Options
    show_coverage(:),                   % +Options
    show_coverage(0,+).                 % :Goal, +Options (deprecated)

:- predicate_options(show_coverage/1, 1,
                     [ all(boolean),
                       modules(list(atom)),
                       roots(list),
                       annotate(boolean),
                       ext(atom),
                       dir(atom),
                       line_numbers(boolean),
                       color(boolean)
                     ]).
:- predicate_options(coverage/2, 2,
                     [ show(boolean),
                       pass_to(prolog_coverage:show_coverage/1,1)
                     ]).
:- predicate_options(cov_save_data/2, 2,
                     [ append(boolean)
                     ]).
:- predicate_options(cov_load_data/2, 2,
                     [ load(boolean),
                       silent(boolean)
                     ]).


%!  coverage(:Goal)
%
%   As  call(Goal),  collecting  coverage  information   while  Goal  is
%   running. If Goal succeeds with a   choice point, coverage collection
%   is suspended and  resumed  if  we   backtrack  into  Goal.  Calls to
%   coverage/1 may be nested.

coverage(Goal) :-
    setup_call_cleanup(
        '$cov_start'(Level),
        cov_run(Goal, Level),
        '$cov_stop'(Level)).

cov_run(Goal, Level) :-
    call(Goal),
    deterministic(Det),
    (   Det == true
    ->  true
    ;   (   '$cov_stop'(Level)
        ;   '$cov_start'(Level),
            fail
        )
    ).

%!  coverage(:Goal, +Options) is semidet.
%
%   Collect and optionally report coverage by  Goal. Goal is executed as
%   in once/1. Options processed:
%
%     - show(+Boolean)
%       When `true` (default), call show_coverage/1 passing Options
%       to show the collected coverage data and reset the data.  When
%       `false`, collect the data but do not reset it.  If there is
%       already existing data the new data is added.

coverage(Goal, Options) :-
    clean_output(Options),
    setup_call_cleanup(
        '$cov_start'(Level),
        once(Goal),
        cov_finish(Level, Options)).

show_coverage(Goal, Options) :-
    print_message(warning, coverage(deprecated(show_coverage/2))),
    coverage(Goal, Options).

cov_finish(Level, Options) :-
    option(show(true), Options, true),
    !,
    '$cov_stop'(Level),
    (   Level == 1
    ->  show_coverage(Options),
        cov_reset
    ;   true
    ).
cov_finish(Level, _) :-
    '$cov_stop'(Level).


%!  show_coverage(+Options) is det.
%
%   Show collected coverage data. By default   it reports the percentage
%   of called and  failed  clauses  related   to  covered  files.  Using
%   dir(Dir), detailed line-by-line annotated files   are created in the
%   directory Dir.  Other options control the level of detail.
%
%     - all(+Boolean)
%       When true, report on any file in which some predicate was
%       called.
%     - modules(+Modules)
%       Only report on files that implement one of the given Modules.
%     - roots(+Directories)
%       Only report on files below one of the given roots.  Each
%       directory in Directories can be a specification for
%       absolute_file_name/3.
%     - annotate(+Bool)
%       Create an annotated file for the detailed results.
%       This is implied if the `ext` or `dir` option are
%       specified.
%     - ext(+Ext)
%       Extension to use for the annotated file. Default is
%       `.cov`.
%     - dir(+Dir)
%       Dump the annotations in the given directory.  If not
%       given, the annotated files are created in the same
%       directory as the source file.   Each clause that is
%       related to a physical line in the file is annotated
%       with one of:
%
%         | ###  | Clause was never executed.                       |
%         | ++N  | Clause was entered N times and always succeeded  |
%         | --N  | Clause was entered N times and never succeeded   |
%         | +N-M | Clause has succeeded N times and failed M times  |
%         | +N*M | Clause was entered N times and succeeded M times |
%
%       All _call sites_ are annotated using the same conventions,
%       except that `---` is used to annotate subgoals that were
%       never called.
%     - line_numbers(Boolean)
%       If `true` (default), add line numbers to the annotated file.
%     - color(Boolean)
%       Controls using ANSI escape sequences to color the output
%       in the annotated source.  Default is `true`.
%     - width(+Columns)
%       Presumed width of the output window.  A value of 40 is
%       considered the minimum.  Smaller values are handled as 40.
%
%   For example, run a goal and create   annotated  files in a directory
%   `cov` using:
%
%       ?- show_coverage([dir(cov)]).
%
%   @bug Color annotations are created using   ANSI escape sequences. On
%   most systems these are displayed  if  the   file  is  printed on the
%   terminal. On most systems `less` may be   used with the ``-r`` flag.
%   Alternatively, programs such as `ansi2html` (Linux)   may be used to
%   convert the files to HTML. It would  probably be better to integrate
%   the output generation with library(pldoc/doc_htmlsrc).

show_coverage(_:Options), is_list(Options) =>
    covered(Succeeded, Failed),
    (   report_hook(Succeeded, Failed)
    ->  true
    ;   file_coverage(Succeeded, Failed, Options)
    ).
show_coverage(_:Goal), Goal=_:Call, callable(Call) =>
    print_message(warning, cov_deprecated(show_coverage)),
    coverage(Goal, []).
show_coverage(_:Options) =>
    must_be(list, Options).

%!  covered(-Succeeded, -Failed) is det.
%
%   Collect failed and succeeded clauses.

covered(Succeeded, Failed) :-
    findall(Cl, ('$cov_data'(clause(Cl), Enter, 0), Enter > 0), Failed0),
    findall(Cl, ('$cov_data'(clause(Cl), _, Exit), Exit > 0), Succeeded0),
    sort(Failed0, Failed),
    sort(Succeeded0, Succeeded).


                 /*******************************
                 *           REPORTING          *
                 *******************************/

%!  file_coverage(+Succeeded, +Failed, +Options) is det.
%
%   Write a report on the clauses covered   organised by file to current
%   output. Show detailed information about   the  non-coverered clauses
%   defined in the modules Modules.

file_coverage(Succeeded, Failed, Options) :-
    abolish_module_tables(prolog_coverage),
    findall(File-PrintFile,
            report_file(File, PrintFile, Succeeded, Failed, Options),
            Pairs),
    Pairs \== [],
    !,

    (   option(width(W0), Options)
    ->  W is max(40, W0)
    ;   pairs_values(Pairs, PrintFiles),
        maplist(atom_length, PrintFiles, Lengths),
        max_list(Lengths, Longest),
        IdealWidth is Longest+21,

        tty_width(Width, Options),
        W is min(IdealWidth, Width - 2)
    ),
    CovCol is W - 6,
    ClausesCol is CovCol - 6,

    header('Coverage by File', W),
    ansi_format(bold, '~w~t~w~*|~t~w~*|~t~w~*|~n',
                ['File', 'Clauses', ClausesCol, '%Cov', CovCol, '%Fail', W]),
    hr(W),
    forall(member(File-_, Pairs),
           file_summary(File, Succeeded, Failed,
                        W, CovCol, ClausesCol,
                        Options)),
    hr(W),

    (   annotate_files(Options)
    ->  forall(member(File-_, Pairs),
               file_details(File, Succeeded, Failed, Options)),
        progress_done('done', [])
    ;   true
    ).
file_coverage(_Succeeded, _Failed, _Options) :-
    print_message(warning, coverage(no_files_to_report)).

%!  report_file(?File, -PrintFile, -Succeeded, -Failed, +Options) is semidet

report_file(File, PrintFile, Succeeded, Failed, Options) :-
    (   nonvar(File)
    ->  true
    ;   (   source_file(File)
        ;   distinct(File, source_includes(_, File))
        )
    ),
    cov_report_file(File, PrintFile, Options),
    cov_clause_sets(File, Succeeded, Failed, Sets),
    \+ ( Sets.failed == [],
         Sets.succeeded == []
       ).

%!  source_includes(?Main, ?Included) is nondet.
%
%   True when Included is (recursively) included in the "true" source
%   fine Main.

:- table source_includes/2.

source_includes(Main, Included) :-
    nonvar(Main),
    !,
    source_file_property(Main, includes(File, _Time)),
    (   Included = File
    ;   source_includes(File, Included)
    ).
source_includes(Main, Included) :-
    nonvar(Included),
    !,
    source_file_property(Included, included_in(Parent, _Time)),
    (   no_included_file(Parent)
    ->  Main = Parent
    ;   source_includes(Main, Parent)
    ).
source_includes(Main, Included) :-
    source_file(Main),			% generator
    source_includes(Main, Included).

main_source(File, Main) :-
    no_included_file(File),
    !,
    Main = File.
main_source(File, Main) :-
    source_includes(Main, File).

%!  file_summary(+File, +Succeeded, +Failed,
%!               +Width, +CovCol, +ClausesCol, +Options) is det.
%
%   Write a summary with the file  and   clause  percentages on a single
%   line.

file_summary(File, Succeeded, Failed, W, CovCol, ClausesCol, Options) :-
    cov_report_file(File, PrintFile, Options),
    cov_clause_sets(File, Succeeded, Failed, Sets0),
    \+ ( Sets0.failed == [],
         Sets0.succeeded == []
       ),
    !,
    deduplicate_clauses(File, Sets0, Sets),

    length(Sets.clauses, AC),
    length(Sets.uncovered, UC),
    length(Sets.failed, FC),

    CP is 100-100*UC/AC,
    FCP is 100*FC/AC,
    summary(PrintFile, ClausesCol-8, SFile),
    format('~w ~`.t ~D~*| ~t~1f~*| ~t~1f~*|~n',
           [SFile, AC, ClausesCol, CP, CovCol, FCP, W]).
file_summary(_,_,_,_,_,_,_).

file_details(File, Succeeded, Failed, Options) :-
    cov_report_file(File, _PrintFile, Options),
    cov_clause_sets(File, Succeeded, Failed, Sets0),
    \+ ( Sets0.failed == [],
         Sets0.succeeded == []
       ),
    !,
    deduplicate_clauses(File, Sets0, Sets),
    ord_union(Sets.failed, Sets.succeeded, Covered),
    detailed_report(Sets.uncovered, Covered, File, Options).
file_details(_,_,_,_).

%!  cov_clause_sets(+File, +Succeeded, +Failed, -Sets) is det.

cov_clause_sets(File, Succeeded, Failed,
                #{ clauses: All_wo_system,
                   succeeded: Succeeded_wo_system,
                   failed: Failed_wo_system,
                   uncovered: Uncovered_wo_system
                 }) :-
    file_clauses(File, FileClauses),
    ord_intersection(FileClauses, Failed, FailedInFile),
    ord_intersection(FileClauses, Succeeded, SucceededInFile),
    ord_subtract(FileClauses, SucceededInFile, UnCov1),
    ord_subtract(UnCov1, FailedInFile, Uncovered),

    clean_set(FileClauses, All_wo_system),
    clean_set(SucceededInFile, Succeeded_wo_system),
    clean_set(FailedInFile, Failed_wo_system),
    clean_set(Uncovered, Uncovered_wo_system).

clean_set(Clauses, UserClauses) :-
    exclude(is_pldoc, Clauses, Clauses_wo_pldoc),
    exclude(is_system_clause, Clauses_wo_pldoc, UserClauses).

is_system_clause(Clause) :-
    clause_pi(Clause, Name),
    Name = system:_.

is_pldoc(Clause) :-
    clause_pi(Clause, _Module:Name2/_Arity),
    pldoc_predicate(Name2).

pldoc_predicate('$pldoc').
pldoc_predicate('$mode').
pldoc_predicate('$pred_option').
pldoc_predicate('$exported_op').        % not really PlDoc ...

summary(String, MaxLen, Summary) :-
    string_length(String, Len),
    (   Len < MaxLen
    ->  Summary = String
    ;   SLen is MaxLen - 5,
        sub_string(String, _, SLen, 0, End),
        string_concat('...', End, Summary)
    ).

%!  file_clauses(+File, -Set) is det.
%
%   Set are all clauses in File as an ordered set.

file_clauses(File, Set) :-
    findall(Cl, clause_source(Cl, File, _), Clauses),
    sort(Clauses, Set).

%!  clause_source(+Clause, -File, -Line) is semidet.
%!  clause_source(-Clause, +File, -Line) is semidet.

clause_source(Clause, File, Line) :-
    nonvar(Clause),
    !,
    clause_property(Clause, file(File)),
    clause_property(Clause, line_count(Line)).
clause_source(Clause, File, Line) :-
    clause_in_file(File, File, Clause, Line).
clause_source(Clause, File, Line) :-
    source_includes(Main, File),
    clause_in_file(Main, File, Clause, Line).

clause_in_file(Main, Source, Clause, Line) :-
    Pred = _:_,
    source_file(Pred, Main),
    \+ predicate_property(Pred, multifile),
    nth_clause(Pred, _Index, Clause),
    clause_property(Clause, file(Source)),
    clause_property(Clause, line_count(Line)).
clause_in_file(_Main, Source, Clause, Line) :-
    Pred = _:_,
    predicate_property(Pred, multifile),
    nth_clause(Pred, _Index, Clause),
    clause_property(Clause, file(Source)),
    clause_property(Clause, line_count(Line)).

%!  deduplicate_clauses(+File, +ClauseSetsIn, -ClauseSetsOut) is det.
%
%   @arg ClauseSetsIn is a dict   with  `clauses`, `uncovered`, `failed`
%   and `succeeded`.

deduplicate_clauses(File, Set, Set) :-
    no_included_file(File),
    !.
deduplicate_clauses(_File, SetIn, SetOut) :-
    _{clauses:AC, uncovered:UC, failed:FC, succeeded:FS} :< SetIn,
    clause_duplicates(AC, AC1),
    clause_duplicates(UC, UC1),
    clause_duplicates(FC, FC1),
    clause_duplicates(FS, FS1),
    exclude(covered_in_some_file(AC1, FC, FS), UC1, UC2),
    exclude(succeeded_in_some_file(AC1, FS), FC1, FC2),
    SetOut = SetIn.put(_{clauses:AC1, uncovered:UC2, failed:FC2, succeeded:FS1}).

no_included_file(File) :-
    source_file(File).

%!  clause_duplicates(+Clauses, -Sets) is det.
%
%   Assuming Clauses is a list of clauses   associated  with a file that
%   was included multiple times, get  the   equivalent  clauses as sets.
%   Note that we know all Clauses come from the same file.
%
%   @arg Sets is an ordered set  of   ordered  sets of clause references
%   that form an equivalence group.

clause_duplicates(Clauses, Sets) :-
    maplist(clause_dedup_data, Clauses, Dedups),
    sort(2, @=<, Dedups, ByMain),       % first my line
    sort(1, @=<, ByMain, ByLine),       % then by main
    clause_sets(ByLine, Sets0),
    sort(Sets0, Sets).

clause_dedup_data(Clause, dd(Line, Main, Clause)) :-
    clause_property(Clause, line_count(Line)),
    clause_property(Clause, source(Main)).

clause_sets([], []).
clause_sets([H|T0], Sets) :-
    same_line_clauses(H, SameL, T0, T1),
    same_line_clause_sets([H|SameL], Sets, More),
    clause_sets(T1, More).

same_line_clauses(CRef, [H|TS], [H|T0], T) :-
    arg(1, CRef, Line),
    arg(1, H, Line),
    !,
    same_line_clauses(CRef, TS, T0, T).
same_line_clauses(_, [], L, L).

%!  same_line_clause_sets(+DDClauses, -Sets, ?Tail) is det.
%
%   Given that DDClauses is a list of   dd(Line, File, Clause) each with
%   the same `Line` and ordered on File,  compute the sets of equivalent
%   clauses.
%
%   First we deal with the common case where there is at most one clause
%   per file.  Then we consider them all the same.

same_line_clause_sets([], Sets, Sets) :-
    !.
same_line_clause_sets(SameL, Sets, More) :-
    map_list_to_pairs(arg(2), SameL, Pairs),
    group_pairs_by_key(Pairs, ByFile),
    pairs_values(ByFile, FileSets),
    \+ member([_,_|_], FileSets),
    !,
    maplist(arg(3), SameL, Clauses0),
    sort(Clauses0, Clauses),
    Sets = [Clauses|More].
same_line_clause_sets([H|T0], [Clauses|Sets], More) :-
    same_clauses(H, Same, T0, T),
    maplist(arg(3), [H|Same], Clauses0),
    sort(Clauses0, Clauses),
    same_line_clause_sets(T, Sets, More).

same_clauses(CRef, [Same|TS], L0, L) :-
    select(Same, L0, L1),
    same_clause(CRef, Same),
    !,
    same_clauses(CRef, TS, L1, L).
same_clauses(_, [], L, L).

same_clause(dd(L1, F1, C1), dd(L2, F2, C2)) :-
    assertion(L1 == L2),
    F1 \== F2,
    clause_property(C1, size(Size)),
    clause_property(C2, size(Size)),
    clause(Head0, Body1, C1),
    clause(Head1, Body2, C2),
    mapsubterms(unqualify, (Head0:-Body1), Clause1),
    mapsubterms(unqualify, (Head1:-Body2), Clause2),
    Clause1 =@= Clause2.

unqualify(_:X, X).

covered_in_some_file(AllEQ, Failed, Succeeded, UncoveredSet) :-
    member(Clause, UncoveredSet),
    member(EQSet, AllEQ),
    memberchk(Clause, EQSet),
    !,
    member(Cl2, EQSet),
    (   memberchk(Cl2, Succeeded)
    ;   memberchk(Cl2, Failed)
    ),
    !.
covered_in_some_file(_AllEQ, _Failed, _Succeeded, _UncoveredSet) :-
    assertion(fail).

succeeded_in_some_file(AllEQ, Succeeded, FailedSet) :-
    member(Clause, FailedSet),
    member(EQSet, AllEQ),
    memberchk(Clause, EQSet),
    !,
    member(Cl2, EQSet),
    memberchk(Cl2, Succeeded),
    !.
succeeded_in_some_file(_AllEQ, _Succeeded, _FailedSet) :-
    assertion(fail).

%!  cov_report_file(+File, -PrintFile, +Options) is semidet.
%
%   Whether or not to report on File.   Scenarios:
%
%     - all(true)
%       Report on every file.
%     - modules(List)
%       Report of the file implements one of the modules in List.
%     - roots(+Dirs)
%       Report if the file appears below one of Dirs.
%     - (default)
%       Report if the file implements a `user` or `test` module.

cov_report_file(File, _, _) :-
    source_file(cov_report_file(_,_,_), File),
    !,
    fail.                               % do not report on myself
cov_report_file(File, File, Options) :-
    option(all(true), Options),
    !.
cov_report_file(File, File, Options) :-
    option(modules(Modules), Options),
    file_module(File, M),
    memberchk(M, Modules),
    !.
cov_report_file(File, PrintFile, Options) :-
    option(roots(Roots), Options),
    !,
    must_be(list, Roots),
    member(Root, Roots),
    absolute_file_name(Root, Path,
                       [ file_type(directory),
                         solutions(all),
                         file_errors(fail)
                       ]),
    ensure_slash(Path, Path1),
    atom_concat(Path1, PrintFile, File),
    !.
cov_report_file(File, File, _Options) :-
    (   file_module(File, M),
        module_property(M, class(user))
    ->  true
    ;   forall(source_file_property(File, module(M)),
               module_property(M, class(test)))
    ).

file_module(File, Module) :-
    source_file_property(File, module(Module)).
file_module(File, Module) :-
    source_includes(Main, File),
    file_module(Main, Module).

ensure_slash(Path, Path) :-
    sub_atom(Path, _, _, 0, /),
    !.
ensure_slash(Path, Path1) :-
    atom_concat(Path, /, Path1).

%!  annotate_files(+Options) is semidet.

annotate_files(Options) :-
    (   option(annotate(true), Options)
    ;   option(dir(_), Options)
    ;   option(ext(_), Options)
    ),
    !.

%!  detailed_report(+Uncovered, +Covered, +File:atom, +Options) is det
%
%   Generate a detailed report for  File.   Depending  on  Options, this
%   either creates an annotated version of File   or  it generates a per
%   clause report of non-covered clauses.
%
%   @arg Uncovered is a list of uncovered clauses.  If File is an
%   included file, it is a list of sets of clause references that
%   represent the same clause.
%   @arg Covered is a list of covered clauses.  As with Uncovered,
%   this is a list of sets for an included File

detailed_report(Uncovered, Covered, File, Options):-
    annotate_files(Options),
    !,
    convlist(line_annotation(File, uncovered), Uncovered, Annot1),
    convlist(line_annotation(File, covered),   Covered,   Annot20),
    flatten(Annot20, Annot2),
    append(Annot1, Annot2, AnnotationsLen),
    pairs_keys_values(AnnotationsLen, Annotations, Lens),
    max_list(Lens, MaxLen),
    Margin is MaxLen+1,
    annotate_file(File, Annotations, [margin(Margin)|Options]).
detailed_report(Uncovered, _, File, _Options):-
    convlist(uncovered_clause_line(File), Uncovered, Pairs),
    sort(Pairs, Pairs_sorted),
    group_pairs_by_key(Pairs_sorted, Compact_pairs),
    nl,
    file_base_name(File, Base),
    format('~2|Clauses not covered from file ~p~n', [Base]),
    format('~4|Predicate ~59|Clauses at lines ~n', []),
    maplist(print_clause_line, Compact_pairs),
    nl.

line_annotation(File, uncovered, Clause, Annotation) :-
    !,
    clause_or_set_source_location(Clause, File, Line),
    Annotation = (Line-ansi(error,###))-3.
line_annotation(File, covered, ClauseOrSet, [HeadAllot|CallSites]) :-
    clause_or_set_source_location(ClauseOrSet, File, Line),
    clause_or_set_cov_data(ClauseOrSet, Entered, Exited),
    line_annotation_msg(line_anot(Line, 0, Entered, Exited), HeadAllot),
    flatten([ClauseOrSet], Clauses),
    maplist(clause_call_site_annotations, Clauses, AnnotSets),
    append(AnnotSets, Annots),
    join_annots(Annots, Joined),
    maplist(line_annotation_msg, Joined, CallSites),
    check_correct_offsets(Clauses, AnnotSets).

clause_or_set_source_location([Clause|_], File, Line) =>
    clause_property(Clause, file(File)),
    clause_property(Clause, line_count(Line)).
clause_or_set_source_location(Clause, File, Line) =>
    clause_property(Clause, file(File)),
    clause_property(Clause, line_count(Line)).

clause_or_set_cov_data(Clause, Entered, Exited),
    blob(Clause, clause) =>
    '$cov_data'(clause(Clause), Entered, Exited).
clause_or_set_cov_data(Clauses, Entered, Exited) =>
    maplist(clause_or_set_cov_data, Clauses, LEntered, LExited),
    sum_list(LEntered, Entered),
    sum_list(LExited, Exited).

line_annotation_msg(line_anot(Line, _PC, Entered, Exited), (Line-Annot)-Len) :-
    (   Exited == Entered
    ->  format(string(Text), '++~D', [Entered]),
        Annot = ansi(comment, Text)
    ;   Exited == 0
    ->  format(string(Text), '--~D', [Entered]),
        Annot = ansi(warning, Text)
    ;   Exited < Entered
    ->  Failed is Entered - Exited,
        format(string(Text), '+~D-~D', [Exited, Failed]),
        Annot = ansi(comment, Text)
    ;   format(string(Text), '+~D*~D', [Entered, Exited]),
        Annot = ansi(fg(cyan), Text)
    ),
    string_length(Text, Len).

uncovered_clause_line(File, Code, Name-Line) :-
    clause_or_set_source_location(Clause, File, Line),
    (   Code = [Clause|_]                % included file; omit module
    ->  clause_pi(Clause, _:Name)
    ;   clause_pi(Code, Name)
    ).

%!  clause_pi(+Clause, -Name) is det.
%
%   Return the clause predicate indicator as Module:Name/Arity.

clause_pi(Clause, Name) :-
    clause(Module:Head, _, Clause),
    functor(Head,F,A),
    Name=Module:F/A.

print_clause_line((Module:Name/Arity)-Lines):-
    term_string(Module:Name, Complete_name),
    summary(Complete_name, 54, SName),
    format('~4|~w~t~59|~p~n', [SName/Arity, Lines]).


		 /*******************************
		 *     LINE LEVEL CALL SITES	*
		 *******************************/

join_annots(Annots, Joined) :-
    sort(2, @=<, Annots, ByPC),
    join_annots_(ByPC, Joined0),
    sort(1, @=<, Joined0, Joined).

join_annots_([], []).
join_annots_([H0|T0], [H|T]) :-
    sum_annot_counts(H0, H, T0, T1),
    join_annots_(T1, T).

sum_annot_counts(line_anot(Line, PC, Enter1, Exit1),
                 Final,
                 [line_anot(Line, PC, Enter2, Exit2)|T0],
                 T) :-
    !,
    Enter is Enter1 + Enter2,
    Exit  is Exit1 + Exit2,
    sum_annot_counts(line_anot(Line, PC, Enter, Exit),
                     Final, T0, T).
sum_annot_counts(Sum, Sum, T, T).

%!  clause_call_site_annotations(+Clause, -Annotations) is det.
%
%   @arg Annotations is a list line_anot(Line, PC, Entered, Exited)

clause_call_site_annotations(Clause, Annots) :-
    findall(Annot,
            clause_call_site_annotation(Clause, Annot),
            Annots).

clause_call_site_annotation(ClauseRef,
                            line_anot(Line, NextPC, Entered, Exited)) :-
    clause_call_site(ClauseRef, PC-NextPC, Line:_LPos),
    (   '$cov_data'(call_site(ClauseRef, NextPC), Entered, Exited)
    ->  true
    ;   '$fetch_vm'(ClauseRef, PC, _, VMI),
        \+ no_annotate_call_site(VMI)
    ->  Entered = 0, Exited = 0
    ).

no_annotate_call_site(i_enter).
no_annotate_call_site(i_exit).
no_annotate_call_site(i_cut).

clause_call_site(ClauseRef, PC-NextPC, Pos) :-
    clause_info(ClauseRef, File, TermPos, _NameOffset),
    '$break_pc'(ClauseRef, PC, NextPC),
    '$clause_term_position'(ClauseRef, NextPC, List),
    catch(prolog_breakpoints:range(List, TermPos, SubPos), E, true),
    (   var(E)
    ->  arg(1, SubPos, A),
        file_offset_pos(File, A, Pos)
    ;   print_message(warning, coverage(clause_info(ClauseRef))),
        fail
    ).

file_offset_pos(File, A, Line:LPos) :-
    file_text(File, String),
    State = start(1, 0),
    call_nth(sub_string(String, S, _, _, "\n"), NLine),
    (   S >= A
    ->  !,
        State = start(Line, SLine),
        LPos is A-SLine
    ;   NS is S+1,
        NLine1 is NLine+1,
        nb_setarg(1, State, NLine1),
        nb_setarg(2, State, NS),
        fail
    ).

file_text(File, String) :-
    setup_call_cleanup(
        open(File, read, In),
        read_string(In, _, String),
        close(In)).

%!  check_correct_offsets(+Clauses, +Annotations) is det.
%
%   Verify that all PC's that  were   annotated  have  been generated as
%   possible call sites.

check_correct_offsets([Clause|_], [Annots|_]) :-
    maplist(arg(2), Annots, PCs),
    check_covered_call_sites(Clause, PCs).

check_covered_call_sites(Clause, Reported) :-
    findall(PC, ('$cov_data'(call_site(Clause,PC), Enter, _), Enter > 0), Seen),
    sort(Reported, SReported),
    sort(Seen, SSeen),
    ord_subtract(SSeen, SReported, Missed),
    (   Missed == []
    ->  true
    ;   print_message(warning, coverage(unreported_call_sites(Clause, Missed)))
    ).


		 /*******************************
		 *           ANNOTATE		*
		 *******************************/

clean_output(Options) :-
    option(dir(Dir), Options),
    !,
    option(ext(Ext), Options, cov),
    format(atom(Pattern), '~w/*.~w', [Dir, Ext]),
    expand_file_name(Pattern, Files),
    maplist(delete_file, Files).
clean_output(Options) :-
    forall(source_file(File),
           clean_output(File, Options)).

clean_output(File, Options) :-
    option(ext(Ext), Options, cov),
    file_name_extension(File, Ext, CovFile),
    (   exists_file(CovFile)
    ->  E = error(_,_),
        catch(delete_file(CovFile), E,
              print_message(warning, E))
    ;   true
    ).


%!  annotate_file(+File, +Annotations, +Options) is det.
%
%   Create  an  annotated  copy  of  File.  Annotations  is  a  list  of
%   `LineNo-Annotation`,  where  `Annotation`  is  atomic    or  a  term
%   Format-Args,  optionally  embedded   in    ansi(Code,   Annotation).

annotate_file(Source, Annotations, Options) :-
    option(ext(Ext), Options, cov),
    (   option(dir(Dir), Options)
    ->  file_base_name(Source, Base),
        file_name_extension(Base, Ext, CovFile),
        directory_file_path(Dir, CovFile, CovPath),
        make_directory_path(Dir)
    ;   file_name_extension(Source, Ext, CovPath)
    ),
    summary(Source, 30, SSource),
    progress('Annotating ~w in ~w ... ', [SSource,CovPath]),
    keysort(Annotations, SortedAnnotations),
    setup_call_cleanup(
        open(Source, read, In),
        setup_call_cleanup(
            open(CovPath, write, Out),
            annotate(In, Out, SortedAnnotations, Options),
            close(Out)),
        close(In)).

annotate(In, Out, Annotations, Options) :-
    (   option(color(true), Options, true)
    ->  set_stream(Out, tty(true))
    ;   true
    ),
    annotate(In, Out, Annotations, 0, Options).

annotate(In, Out, Annotations, LineNo0, Options) :-
    read_line_to_string(In, Line),
    (   Line == end_of_file
    ->  true
    ;   succ(LineNo0, LineNo),
        margins(LMargin, CMargin, Options),
        line_no(LineNo, Out, LMargin),
        annotations(LineNo, Out, LMargin, Annotations, Annotations1),
        format(Out, '~t~*|~s~n', [CMargin, Line]),
        annotate(In, Out, Annotations1, LineNo, Options)
    ).

annotations(Line, Out, LMargin, [Line-Annot|T0], T) :-
    !,
    write_annotation(Out, Annot),
    (   T0 = [Line-_|_]
    ->  with_output_to(Out, ansi_format(bold, ' \u2bb0~n~t~*|', [LMargin])),
        annotations(Line, Out, LMargin, T0, T)
    ;   T = T0
    ).
annotations(_, _, _, Annots, Annots).

write_annotation(Out, ansi(Code, Fmt-Args)) =>
    with_output_to(Out, ansi_format(Code, Fmt, Args)).
write_annotation(Out, ansi(Code, Fmt)) =>
    with_output_to(Out, ansi_format(Code, Fmt, [])).
write_annotation(Out, Fmt-Args) =>
    format(Out, Fmt, Args).
write_annotation(Out, Fmt) =>
    format(Out, Fmt, []).

line_no(_, _, 0) :- !.
line_no(Line, Out, LMargin) :-
    with_output_to(Out, ansi_format(fg(127,127,127), '~t~d ~*|',
                                    [Line, LMargin])).

margins(LMargin, Margin, Options) :-
    option(line_numbers(true), Options, true),
    !,
    option(line_number_margin(LMargin), Options, 6),
    option(margin(AMargin), Options, 4),
    Margin is LMargin+AMargin.
margins(0, Margin, Options) :-
    option(margin(Margin), Options, 4).

%!  report_hook(+Succeeded, +Failed) is semidet.
%
%   This hook is called after the data   collection. It is passed a list
%   of objects that have succeeded as  well   as  a list of objects that
%   have failed.  The objects are one of
%
%     - ClauseRef
%       The specified clause
%     - call_site(ClauseRef, PC)
%       A call was make in ClauseRef at the given program counter.

:- multifile
    report_hook/2.

		 /*******************************
		 *          SAVE/RELOAD		*
		 *******************************/

%!  cov_save_data(+File, +Options) is det.
%
%   Save the coverage information to File.  Options:
%
%     - append(true)
%       Append to File rather than truncating the data if the file
%       exists.
%
%   The File is  opened  using   lock(exclusive),  which  implies  that,
%   provided the OS and file system   implements  file locking, multiple
%   processes may save coverage data to the same file.
%
%   The saved data is highly specific to the  setup in which it has been
%   created. It can typically only be  reloaded using cov_load_data/2 in
%   the same Prolog executable  using  the   same  options  and with all
%   relevant source file unmodified at the same location.
%
%   Reproducibility can be improved by  using   `.qlf`  files  or _saved
%   states_.

:- thread_local
    saved_clause/2.                     % Clause, Ref

cov_save_data(File, Options) :-
    (   option(append(true), Options)
    ->  Mode = append
    ;   Mode = write
    ),
    absolute_file_name(File, Path, [ access(write) ]),
    setup_call_cleanup(
        open(Path, Mode, Out,
             [ encoding(utf8),
               lock(exclusive)
             ]),
        cov_save_to_stream(Out),
        ( retractall(saved_clause(_,_)),
          close(Out))).

cov_save_to_stream(Out) :-
    get_time(Now),
    format(Out, 'cov_begin_data(~1f).~n', [Now]),
    forall('$cov_data'(Site, Enter, Exit),
           cov_save_entry(Out, Site, Enter, Exit)),
    format(Out, 'cov_end_data.~n', []).

:- det(cov_save_entry/4).
cov_save_entry(Out, call_site(Clause, PC), Enter, Exit) =>
    save_clause(Out, Clause, Ref),
    (   nonvar(Ref)
    ->  format(Out, '~q.~n', [cs(Ref, PC, Enter, Exit)])
    ;   true
    ).
cov_save_entry(Out, clause(Clause), Enter, Exit) =>
    save_clause(Out, Clause, Ref),
    (   nonvar(Ref)
    ->  format(Out, '~q.~n', [cs(Ref, Enter, Exit)])
    ;   true
    ).

save_clause(_Out, Clause, Ref) :-
    saved_clause(Clause, Ref),
    !.
save_clause(Out, Clause, Ref) :-
    clause_property(Clause, file(File)),
    clause_property(Clause, line_count(Line)),
    clause_property(Clause, size(Bytes)),
    clause_property(Clause, predicate(PI)),
    main_source(File, Main),
    source_file_property(Main, load_context(Module, Location, Options)),
    nth_clause(_, Nth, Clause),
    !,
    (   predicate_property(saved_clause(_,_), number_of_clauses(N))
    ->  Ref is N+1
    ;   Ref = 1
    ),
    format(Out, '~q.~n', [cl(PI, Nth, Bytes, Main, File:Line, Module, Location, Options, Ref)]),
    assertz(saved_clause(Clause, Ref)).
save_clause(_Out, Clause, _Ref) :-
    debug(cov(save), 'Could not save clause ~p', [Clause]).

%!  cov_load_data(+File, +Options) is det.
%
%   Reload coverage data from File.  Options:
%
%     - load(true)
%       If specified and the file in which a clauses is expected to
%       exist, load the file using load_files/2 with the same options
%       as used to initially load the file.
%     - silent(+Boolean)
%       When `true`, do not emit messages on not loaded source files.
%
%   Data is assumed to be reliable if   the Nth-clause of a predicate is
%   loaded from the same file at the same   line number and has the same
%   size. Unreliable data is ignored, silently if silent(true) is used.

:- thread_local
    warned/1.

cov_load_data(File, Options) :-
    absolute_file_name(File, Path, [ access(read) ]),
    setup_call_cleanup(
        open(Path, read, In, [encoding(utf8)]),
        cov_load_data_from_stream(In, Options),
        ( retractall(saved_clause(_,_)),
          retractall(warned(_)),
          close(In))).

cov_load_data_from_stream(In, Options) :-
    read_term(In, Term, []),
    cov_load_data_from_stream(Term, In, Options).

cov_load_data_from_stream(end_of_file, _, _) :-
    !.
cov_load_data_from_stream(Term, In, Options) :-
    cov_restore_data(Term, Options),
    read_term(In, Term2, []),
    cov_load_data_from_stream(Term2, In, Options).

cov_restore_data(cov_begin_data(_), _Options) =>
    true.
cov_restore_data(cl(PI, Nth,
                    Bytes, Main, File:Line, Module, _Location, LoadOptions,
                    Ref), Options) =>
    (   restore_clause(PI, Nth, Bytes, File, Line, Ref)
    ->  true
    ;   source_file(File)
    ->  warn(File, coverage(source_changed(File, PI)))
    ;   option(load(true), Options)
    ->  load_files(Module:Main, [if(not_loaded)|LoadOptions]),
        (   restore_clause(PI, Nth, Bytes, File, Line, Ref)
        ->  true
        ;   warn(File, coverage(source_changed(File, PI)))
        )
    ;   option(silent(true), Options)
    ->  true
    ;   warn(File, coverage(no_source(File)))
    ).
cov_restore_data(cs(Ref, PC, Enter, Exit), _Options) =>
    (   saved_clause(Clause, Ref)
    ->  '$cov_add'(call_site(Clause, PC), Enter, Exit)
    ;   true
    ).
cov_restore_data(cs(Ref, Enter, Exit), _Options) =>
    (   saved_clause(Clause, Ref)
    ->  '$cov_add'(clause(Clause), Enter, Exit)
    ;   true
    ).
cov_restore_data(cov_end_data, _Options) =>
    retractall(saved_clause(_,_)).

restore_clause(PI, _Nth, Bytes, File, Line, Ref) :-
    pi_head(PI, Head),
    predicate_property(Head, multifile),
    !,
    (   nth_clause(Head, _, Clause),
        clause_property(Clause, file(File)),
        clause_property(Clause, line_count(Line)),
        clause_property(Clause, size(Bytes))
    ->  assertz(saved_clause(Clause, Ref))
    ;   warn(File, coverage(no_multifile_source(File:Line, PI)))
    ).
restore_clause(PI, Nth, Bytes, File, Line, Ref) :-
    pi_head(PI, Head),
    (   nth_clause(Head, Nth, Clause)
    ->  (   clause_property(Clause, file(File)),
            clause_property(Clause, line_count(Line)),
            clause_property(Clause, size(Bytes))
        ->  assertz(saved_clause(Clause, Ref))
        ;   warn(File, coverage(source_changed(File:Line, PI, Nth)))
        )
    ).

warn(Term, _Msg) :-
    warned(Term),
    !.
warn(Term, Msg) :-
    assertz(warned(Term)),
    print_message(warning, Msg).


%!  cov_reset is det.
%
%   Discard  all  collected  coverage  data.  This  predicate  raises  a
%   permission error if coverage collection is in progress.

cov_reset :-
    '$cov_reset'.


%!  cov_property(?Property)
%
%   True when coverage analysis satisfies   Property.  Currently defined
%   properties are:
%
%     - active(?Nesting)
%       True when coverage data is   being  collected. Nesting expresses
%       the nesting of coverage/1 calls and is normally 1 (one).

cov_property(active(Level)) :-
    '$cov_active'(Level).


		 /*******************************
		 *             MESSAGES		*
		 *******************************/

:- multifile
    prolog:message//1.

prolog:message(coverage(Msg)) -->
    message(Msg).

message(no_files_to_report) -->
    [ 'No coverage events in selected files'-[] ].
message(clause_info(ClauseRef)) -->
    [ 'Inconsistent clause info for '-[] ],
    clause_msg(ClauseRef).
message(unreported_call_sites(ClauseRef, PCList)) -->
    [ 'Failed to report call sites for '-[] ],
    clause_msg(ClauseRef),
    [ nl, '  Missed at these PC offsets: ~p'-[PCList] ].
message(source_changed(File, PI)) -->
    [ 'Predicate ', ansi(code, '~p', [PI]), ' cannot be found while file ',
      url(File), ' is loaded.'
    ].
message(no_source(File)) -->
    [ 'File ', url(File), ' is not loaded.  Please re-run with ', nl,
      'file loaded or use the ', ansi(code, 'load(true)', []), ' option.'
    ].
message(no_multifile_source(Location, PI)) -->
    [ 'Could not find matching clause for multifile predicate ',
      ansi(code, '~p', [PI]), ' at ', url(Location)
    ].
message(source_changed(File:Line, PI, Nth)) -->
    [ '~D-th clause for '-[Nth], ansi(code, '~p', [PI]),
      ' cannot be found at ', url(File:Line), '.'
    ].
message(deprecated(show_coverage/2)) -->
    [ 'show_coverage/2 is deprecated.  Please use coverage/2', nl,
      'with the same arguments.'
    ].


clause_msg(ClauseRef) -->
    { clause_pi(ClauseRef, PI),
      clause_property(ClauseRef, file(File)),
      clause_property(ClauseRef, line_count(Line))
    },
    [ '~p at'-[PI], nl, '  ', url(File:Line) ].


		 /*******************************
		 *      TTY PRINT SUPPORT	*
		 *******************************/

progress(_, _) :-
    current_prolog_flag(verbose, silent),
    !.
progress(Format, Args) :-
    stream_property(user_output, tty(true)),
    !,
    format(user_output, '\r\e[2K', []),
    ansi_format(comment, Format, Args),
    flush_output(user_output).
progress(Format, Args) :-
    format(Format, Args),
    nl.

progress_done(_,_) :-
    current_prolog_flag(verbose, silent),
    !.
progress_done(Format, Args) :-
    stream_property(user_output, tty(true)),
    !,
    ansi_format(comment, Format, Args),
    nl.
progress_done(_, _).

header(Title, Width) :-
    hr(Width),
    ansi_format([bold], '~t~w~t~*|', [Title,Width]),
    nl.

hr(Width) :-
    format('~N~`\u2015t~*|~n', [Width]).

%!  tty_width(-Width, +Options) is det.

tty_width(W, Options) :-
    option(width(W), Options),
    !.
:- if(current_predicate(tty_size/2)).
tty_width(W, _Options) :-
    catch(tty_size(_, TtyW), _, fail),
    !,
    W is max(60, TtyW).
:- endif.
tty_width(78, _).
