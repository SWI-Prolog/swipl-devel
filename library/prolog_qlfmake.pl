/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2025, SWI-Prolog Solutions b.v.
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

:- module(prolog_qlfmake,
          [ qlf_make/0,
            qlf_make/1                  % +Spec
          ]).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(ansi_term)).
:- use_module(library(apply)).
:- if(exists_source(library(pldoc))).
:- use_module(library(pldoc)).
:- use_module(library(prolog_source)).
:- use_module(library(dcg/high_order)).

:- endif.

/** <module> Compile the library to QLF format

Compilation mode:
   - Handle debug/3
     - Optimise, forcing loading of .pl files in debug mode?
   - library(apply_macros)
     - Load in advance
   - Do not include compiled documentation
     - doc_collect(false).
*/

% :- debug(qlf_make).

%!  qlf_make is det.
%
%   Compile all files from the system libraries  to .QLF format. This is
%   normally called as part  of   building  SWI-Prolog.  The compilation
%   consists of these phases:
%
%     1. Prepare the compilation environment (expansion, optimization)
%     2. Build the _aggregate_ .QLF files specified in aggregate_qlf/1.
%     3. Find all .pl files that need to a .QLF version.
%     4. Find the subset that need rebuilding
%     5. Compile these files
%     6. Report on the sizes

qlf_make :-
    set_prolog_flag(optimise, true),
    set_prolog_flag(optimise_debug, true),
    preload(library(apply_macros), []),
    preload_pldoc,
    qmake_aggregates,
    system_lib_files(Files),
    include(qlf_needs_rebuild, Files, Rebuild),
    report_work(Files, Rebuild),
    qcompile_files(Rebuild),
    size_stats(Files).

%!  qlf_make(+Spec) is det.
%
%   Ensure a .QLF version of Spec. If the   .QLF  file for Spec does not
%   exist, is incompatible or one of its   source files has changed, run
%   qcompile/1 to compile the file.

qlf_make(Spec) :-
    absolute_file_name(Spec, PlFile,
                       [ file_type(prolog),
                         access(read)
                       ]),
    (   qlf_needs_rebuild(PlFile)
    ->  qcompile_(PlFile)
    ;   true
    ).

qcompile_files([]) => true.
qcompile_files([+H|T]) =>
    qcompile_(H),
    qcompile_files(T).
qcompile_files([H|T]) =>
    file_dependencies(H, Deps),
    intersection(Deps, T, Deps1),
    (   Deps1 == []
    ->  qcompile_(H),
        qcompile_files(T)
    ;   subtract(T, Deps1, T1),
        append([Deps1, [+H], T1], Agenda),
        qcompile_files(Agenda)
    ).

qcompile_(PlFile) :-
    progress(PlFile),
    qcompile(PlFile, [imports([])]).

%!  preload_pldoc is det.
%
%   Preload the documentation system and disable it.  We need to do this
%   to avoid embedding the system documentation into the .qlf files.

preload_pldoc :-
    exists_source(library(pldoc)),
    !,
    preload(library(pldoc), [doc_collect/1]),
    doc_collect(false).
preload_pldoc.

%!  preload(+Spec, +Imports) is det.
%
%   Ensure the .QLF file for Spec, load   the file and import predicates
%   from Imports. This  is  used  to   preload  files  that  affect  the
%   compilation such as library(apply_macros) and PlDoc.

preload(Spec, Imports) :-
    absolute_file_name(Spec, File,
                       [ extensions([pl]),
                         access(read),
                         file_errors(fail)
                       ]),
    !,
    qlf_make(File),
    use_module(File, Imports).
preload(_, _).

%!  qlf_needs_rebuild(+PlFile:atom) is semidet.
%
%   True when PlFile  needs  to  be   recompiled.  This  currently  only
%   considers the immediate  source  file,   __not__  included  files or
%   imported files that define operators, goal or term expansion rules.

qlf_needs_rebuild(PlFile) :-
    pl_qlf_file(PlFile, QlfFile),
    (   \+ exists_file(QlfFile)
    ->  true
    ;   '$qlf_versions'(QlfFile, CurrentVersion, _MinLOadVersion, FileVersion,
                        CurrentSignature, FileSignature),
        (   FileVersion \== CurrentVersion
        ;   FileSignature \== CurrentSignature
        )
    ->  true
    ;   time_file(QlfFile, QlfTime),
        '$qlf_sources'(QlfFile, Sources),
        member(S, Sources),
        arg(1, S, File),
        time_file(File, STime),
        STime > QlfTime+1
    ).

pl_qlf_file(PlFile, QlfFile) :-
    file_name_extension(Base, pl, PlFile),
    file_name_extension(Base, qlf, QlfFile).

%!  size_stats(+Files) is det.
%
%   Print (size) statistics on the created .QLF files.

size_stats(Files) :-
    maplist(size_stat, Files, PlSizes, Qlfizes),
    sum_list(PlSizes, PlSize),
    sum_list(Qlfizes, Qlfize),
    length(Files, Count),
    print_message(informational, qlf_make(size(Count, Qlfize, PlSize))).

size_stat(PlFile, PlSize, QlfSize) :-
    pl_qlf_file(PlFile, QlfFile),
    size_file(PlFile, PlSize),
    size_file(QlfFile, QlfSize).

:- dynamic qlf_part_of/2.               % Part, Whole

                /*******************************
                *         DEPENDENCIES         *
                *******************************/

%!  file_dependencies(+File, -Deps:ordset) is det.
%
%   True when Deps is a  list  of   absolute  file  names  that form the
%   dependencies of File. These dependencies are   used to determine the
%   order in which we compile the units.   This  does __not__ state that
%   the compilation process depends  on   these  dependencies.  But, qlf
%   compiling a module does load  these   dependencies,  either from the
%   source or created .qlf file. Only   if the loaded dependency exports
%   macros (term/goal expansion rules) or operators  we actually need to
%   have the depedencies compiled before us.   Still,  qlf compiling the
%   dependencies before speeds up the compilation of this file.
%
%   This predicate examines the file loading  directives. Note that Deps
%   does __not__ contain files  loaded  using   include/1  as  we do not
%   create .qlf files for these.

file_dependencies(File, Deps) :-
    prolog_file_directives(File, Directives, []),
    phrase(file_deps(Directives), Deps0),
    convlist(absolute_path(File), Deps0, Deps1),
    sort(Deps1, Deps).

file_deps([]) ==>
    [].
file_deps([H|T]) ==>
    file_dep(H),
    file_deps(T).

file_dep((:- Dir)) ==>
    (   { directive_file(Dir, Files) }
    ->  file_or_files(Files)
    ;   []
    ).
file_dep(_) ==>
    [].

file_or_files(Files), is_list(Files) ==>
    sequence(file, Files).
file_or_files(File) ==>
    file(File).

file(File) -->
    [File].

directive_file(ensure_loaded(File), File).
directive_file(consult(File), File).
directive_file(load_files(File, _), File).
directive_file(use_module(File), File).
directive_file(use_module(File, _), File).
directive_file(autoload(File), File).
directive_file(autoload(File, _), File).
directive_file(reexport(File), File).
directive_file(reexport(File, _), File).

absolute_path(RelativeTo, _:Spec, File) =>
    absolute_path(RelativeTo, Spec, File).
absolute_path(_RelativeTo, Spec, File),
    compound(Spec), compound_name_arity(Spec, _, 1) =>
    absolute_file_name(Spec, File,
                       [ access(read),
                         file_type(source),
                         file_errors(fail)
                       ]).
absolute_path(RelativeTo, Spec, File) =>
    absolute_file_name(Spec, File,
                       [ relative_to(RelativeTo),
                         access(read),
                         file_type(source),
                         file_errors(fail)
                       ]).


                /*******************************
                *       FIND CANDIDATES        *
                *******************************/

%!  system_lib_files(-LibFiles:list(atom)) is det.
%
%   True when LibFiles is a list of  all   files  for  which a .QLF file
%   needs to be build.  This means, all .pl files __except__:
%
%     - `INDEX.pl`, `MKINDEX.pl` and `CLASSINDEX.pl`
%     - Files that are part of an aggregate .QLF file
%     - Files that are explicitly excluded as specified by exclude/1
%       or exclude_dir/1.
%
%   These rules must be kept   in  sync with `cmake/InstallSource.cmake`
%   that creates CMake install targets for  the   .qlf  files. We need a
%   better solution for this using a  common   set  of rules that can be
%   interpreted by both Prolog and CMake.

system_lib_files(LibFiles) :-
    findall(Dir, system_lib_dir(Dir), Dirs),
    maplist(dir_files, Dirs, FilesL),
    append(FilesL, Files0),
    sort(Files0, Files),
    exclude(excluded, Files, LibFiles).

system_lib_dir(LibDir) :-
    working_directory(PWD, PWD),
    source_alias(Alias),
    absolute_file_name(Alias, LibDir,
                       [ file_type(directory),
                         solutions(all),
                         file_errors(fail),
                         access(read)
                       ]),
    sub_atom(LibDir, 0, _, _, PWD).

source_alias(library(.)).
source_alias(app(.)).
source_alias(pce('prolog/demo')).
source_alias(pce('prolog/contrib')).


%!  dir_files(+Dir, -Files) is det.
%
%   Get all files from Dir recursively.  Skip directories that are
%   excluded by exclude_dir/1.

dir_files(Dir, Files) :-
    dir_files_([Dir|DirT], DirT, Files).

dir_files_([], [], []) :- !.
dir_files_([D|DT], DirT, Files) :-
    \+ excluded_directory(D),
    !,
    dir_files_dirs(D, Files, FileT, DirT, DirT2),
    dir_files_(DT, DirT2, FileT).
dir_files_([_|DT], DirT, Files) :-
    dir_files_(DT, DirT, Files).

dir_files_dirs(Dir, Files, FileT, Dirs, DirT) :-
    directory_files(Dir, Entries),
    dir_files_dirs_(Entries, Dir, Files, FileT, Dirs, DirT).

dir_files_dirs_([], _, Files, Files, Dirs, Dirs).
dir_files_dirs_([H|T], Dir, Files, FileT, Dirs, DirT) :-
    hidden_entry(H),
    !,
    dir_files_dirs_(T, Dir, Files, FileT, Dirs, DirT).
dir_files_dirs_([H|T], Dir, Files, FileT, Dirs, DirT) :-
    atomic_list_concat([Dir, /, H], Path),
    (   exists_file(Path)
    ->  Files = [Path|Files1],
        dir_files_dirs_(T, Dir, Files1, FileT, Dirs, DirT)
    ;   exists_directory(Path)
    ->  Dirs = [Path|Dirs1],
        dir_files_dirs_(T, Dir, Files, FileT, Dirs1, DirT)
    ;   dir_files_dirs_(T, Dir, Files, FileT, Dirs, DirT)
    ).

hidden_entry('.').
hidden_entry('..').

excluded(File) :-
    \+ file_name_extension(_, pl, File),
    !.
excluded(File) :-
    file_base_name(File, 'INDEX.pl'),
    !.
excluded(File) :-
    file_base_name(File, 'MKINDEX.pl'),
    !.
excluded(File) :-
    file_base_name(File, 'CLASSINDEX.pl'),
    !.
excluded(File) :-
    qlf_part_of(File, Main),
    !,
    report_excluded(excluded(part(Main), File)).
excluded(File) :-
    exclude(Spec),
    same_base(Spec, pl, File),
    absolute_file_name(Spec, File1,
                       [ extensions([pl]),
                         access(read),
                         solutions(all)
                       ]),
    File == File1,
    !,
    report_excluded(excluded(rule(Spec), File)).

same_base(Spec, Ext, Path) :-
    spec_base(Spec, Base),
    file_base_name(Path, File),
    file_name_extension(Base, Ext, File).

spec_base(Spec, Base) :-
    compound(Spec),
    Spec =.. [_,Sub],
    last_segment(Sub, Base).

last_segment(_/B, L) =>
    last_segment(B, L).
last_segment(A, L), atomic(A) =>
    L = A.

exclude(library(prolog_qlfmake)).
exclude(library(win_menu)).
exclude(library(threadutil)).
exclude(library(check_installation)).
exclude(library(sty_pldoc)).
exclude(library(sty_xpce)).
exclude(library(tabling)).
exclude(library(theme/dark)).
exclude(library(http/dcg_basics)).
exclude(library(chr/chr_translate_bootstrap1)).
exclude(library(chr/chr_translate_bootstrap2)).
exclude(library(trace/pprint)).
exclude(library(xref/quintus)).
exclude(library(xref/sicstus)).
exclude(library(pldoc/hooks)).

excluded_directory(Dir) :-
    exclude_dir(Spec),
    spec_base(Spec, Base),
    atom_concat(/, Base, SBase),
    once(sub_atom(Dir, _, _, _, SBase)),
    absolute_file_name(Spec, Dir1,
                       [ file_type(directory),
                         access(read),
                         solutions(all)
                       ]),
    sub_atom(Dir, 0, _, _, Dir1),
    !,
    report_excluded(excluded(rule(Spec), Dir)).

exclude_dir(swi(xpce/prolog/lib/compatibility)).


                /*******************************
                *          AGGREGATES          *
                *******************************/

%!  qmake_aggregates is det.
%
%   QLF compile the _aggregates_.  This   also  populates  qlf_part_of/2
%   which is used to avoid compiling these parts.

qmake_aggregates :-
    retractall(qlf_part_of(_,_)),
    forall(aggregate_qlf(Spec),
           qmake_aggregate(Spec)).

qmake_aggregate(Spec) :-
    exists_source(Spec),
    !,
    qlf_make(Spec),
    absolute_file_name(Spec, PlFile,
                       [ file_type(prolog),
                         access(read)
                       ]),
    pl_qlf_file(PlFile, QlfFile),
    '$qlf_sources'(QlfFile, Sources),
    forall(member(source(S), Sources),
           assertz(qlf_part_of(S, PlFile))).
qmake_aggregate(_).

aggregate_qlf(library(pce)).
aggregate_qlf(library(trace/trace)).
aggregate_qlf(library(emacs/emacs)).


                /*******************************
                *       FILE SEARCH PATH       *
                *******************************/

:- multifile
    user:file_search_path/2.

user:file_search_path(chr,   library(chr)).
user:file_search_path(pldoc, library(pldoc)).
user:file_search_path(doc,   swi(xpce/prolog/lib/doc)).


                /*******************************
                *           FEEDBACK           *
                *******************************/

report_work(Files, Rebuild) :-
    length(Files, AllFiles),
    length(Rebuild, NeedsRebuild),
    print_message(informational, qlf_make(planning(AllFiles, NeedsRebuild))).

progress(_PlFile) :-
    current_prolog_flag(verbose, silent),
    !.
progress(PlFile) :-
    stream_property(user_output, tty(true)),
    current_prolog_flag(color_term, true),
    \+ debugging(qlf_make),
    !,
    ansi_format(comment, '\r~w ...', [PlFile]),
    format(user_output, '\e[K', []),
    flush_output(user_output).
progress(PlFile) :-
    format(user_output, '~N~w ...', [PlFile]),
    flush_output(user_output).

report_excluded(Msg) :-
    debugging(qlf_make),
    !,
    print_message(informational, qlf_make(Msg)).
report_excluded(_).

:- multifile prolog:message//1.

prolog:message(qlf_make(Msg)) -->
    message(Msg).

message(planning(_AllFiles, 0)) ==>
    [].
message(planning(AllFiles, AllFiles)) ==>
    [ 'Building ~D qlf files'-[AllFiles] ].
message(planning(AllFiles, NeedsRebuild)) ==>
    [ '~D qlf files.  ~D need to be rebuild'-[AllFiles, NeedsRebuild] ].
message(size(Count, Qlfize, PlSize)) ==>
    [ '~D qlf files take ~D bytes.  Source ~D bytes'-
      [Count, Qlfize, PlSize]
    ].
message(excluded(Reason, File)) ==>
    [ 'Excluded ', url(File) ],
    excl_reason(Reason).

excl_reason(part(_Main)) -->
    [ ' (part of aggregate QLF)' ].
excl_reason(rule(_Spec)) -->
    [ ' (explicit)' ].
