/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2025, University of Amsterdam
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

:- module('$autoload',
          [ '$find_library'/5,
            '$in_library'/3,
            '$define_predicate'/1,
            '$update_library_index'/1,          % +Options
            '$autoload'/1,

            make_library_index/1,
            make_library_index/2,
            reload_library_index/0,
            autoload_path/1,

            autoload/1,                         % +File
            autoload/2,                         % +File, +Imports

            autoload_call/1,                    % :Goal

            require/1                           % +Predicates
          ]).

:- meta_predicate
    '$autoload'(:),
    autoload(:),
    autoload(:, +),
    autoload_call(0),
    require(:).

:- dynamic
    library_index/3,                % Head x Module x Path
    autoload_directories/1,         % List
    index_checked_at/1.             % Time
:- volatile
    library_index/3,
    autoload_directories/1,
    index_checked_at/1.

user:file_search_path(autoload, swi(library)).
user:file_search_path(autoload, pce(prolog/lib)).
user:file_search_path(autoload, app_config(lib)).
user:file_search_path(autoload, Dir) :-
    '$ext_library_directory'(Dir).

:- create_prolog_flag(warn_autoload, false, []).

%!  '$find_library'(+Module, +Name, +Arity, -LoadModule, -Library) is semidet.
%
%   Locate a predicate in the library. Name   and arity are the name
%   and arity of  the  predicate  searched   for.  `Module'  is  the
%   preferred target module. The return  values   are  the full path
%   name (excluding extension) of the library and module declared in
%   that file.

'$find_library'(_Module, :, 2, _LoadModule, _Library) :-
    !, fail.
'$find_library'(Module, Name, Arity, LoadModule, Library) :-
    load_library_index(Name, Arity),
    functor(Head, Name, Arity),
    (   library_index(Head, Module, Library),
        LoadModule = Module
    ;   library_index(Head, LoadModule, Library)
    ),
    !.

%!  '$in_library'(+Name, +Arity, -Path) is semidet.
%!  '$in_library'(-Name, -Arity, -Path) is nondet.
%
%   Is true if Name/Arity is in the autoload libraries.

'$in_library'(Name, Arity, Path) :-
    atom(Name), integer(Arity),
    !,
    Name/Arity \= (:)/2,
    load_library_index(Name, Arity),
    functor(Head, Name, Arity),
    library_index(Head, _, Path).
'$in_library'(Name, Arity, Path) :-
    load_library_index(Name, Arity),
    library_index(Head, _, Path),
    Head \= _:_,
    functor(Head, Name, Arity).

%!  '$define_predicate'(:Head)
%
%   Make sure PredInd can be called. First  test if the predicate is
%   defined. If not, invoke the autoloader.

:- meta_predicate
    '$define_predicate'(:).

'$define_predicate'(Head) :-
    '$defined_predicate'(Head),
    !.
'$define_predicate'(Term) :-
    Term = Module:Head,
    (   compound(Head)
    ->  compound_name_arity(Head, Name, Arity)
    ;   Name = Head, Arity = 0
    ),
    '$undefined_procedure'(Module, Name, Arity, retry).


                /********************************
                *          UPDATE INDEX         *
                ********************************/

:- thread_local
    silent/0.

%!  '$update_library_index'(+Options)
%
%   Called from make/0 to update the index   of the library for each
%   library directory that has a writable   index.  Note that in the
%   Windows  version  access_file/2  is  mostly   bogus.  We  assert
%   silent/0 to suppress error messages.  Options:
%
%       - system(+Boolean)
%         Do (not) include system libraries.   Default `false`.
%       - user(+Boolean)
%         Do (not) include user libraries.  Default `true`.

'$update_library_index'(Options) :-
    setof(Dir, writable_indexed_directory(Dir, Options), Dirs),
    !,
    setup_call_cleanup(
        asserta(silent, Ref),
        guarded_make_library_index(Dirs),
        erase(Ref)),
    (   flag('$modified_index', true, false)
    ->  reload_library_index
    ;   true
    ).
'$update_library_index'(_).

guarded_make_library_index([]).
guarded_make_library_index([Dir|Dirs]) :-
    (   catch(make_library_index(Dir), E,
              print_message(error, E))
    ->  true
    ;   print_message(warning, goal_failed(make_library_index(Dir)))
    ),
    guarded_make_library_index(Dirs).

%!  writable_indexed_directory(-Dir, +Options) is nondet.
%
%   True when Dir is an indexed   library  directory with a writable
%   index, i.e., an index that can be updated.

writable_indexed_directory(Dir, Options) :-
    current_prolog_flag(home, Home),
    writable_indexed_directory(Dir),
    (   sub_atom(Dir, 0, _, _, Home)
    ->  '$option'(system(true), Options, false)
    ;   '$option'(user(true), Options, true)
    ).

writable_indexed_directory(Dir) :-
    index_file_name(IndexFile, autoload('INDEX'), [access([read,write])]),
    file_directory_name(IndexFile, Dir).
writable_indexed_directory(Dir) :-
    absolute_file_name(library('MKINDEX'),
                       [ file_type(prolog),
                         access(read),
                         solutions(all),
                         file_errors(fail)
                       ], MkIndexFile),
    file_directory_name(MkIndexFile, Dir),
    plfile_in_dir(Dir, 'INDEX', _, IndexFile),
    access_file(IndexFile, write).


                /********************************
                *           LOAD INDEX          *
                ********************************/

%!  reload_library_index
%
%   Reload the index on the next call

reload_library_index :-
    context_module(M),
    reload_library_index(M).

reload_library_index(M) :-
    with_mutex('$autoload', clear_library_index(M)).

clear_library_index(M) :-
    retractall(M:library_index(_, _, _)),
    retractall(M:autoload_directories(_)),
    retractall(M:index_checked_at(_)).


%!  load_library_index(?Name, ?Arity) is det.
%!  load_library_index(?Name, ?Arity, :IndexSpec) is det.
%
%   Try to find Name/Arity  in  the   library.  If  the predicate is
%   there, we are happy. If not, we  check whether the set of loaded
%   libraries has changed and if so we reload the index.

:- meta_predicate load_library_index(?, ?, :).
:- public load_library_index/3.

load_library_index(Name, Arity) :-
    load_library_index(Name, Arity, autoload('INDEX')).

load_library_index(Name, Arity, M:_Spec) :-
    atom(Name), integer(Arity),
    functor(Head, Name, Arity),
    M:library_index(Head, _, _),
    !.
load_library_index(_, _, Spec) :-
    notrace(with_mutex('$autoload', load_library_index_p(Spec))).

load_library_index_p(M:_) :-
    M:index_checked_at(Time),
    get_time(Now),
    Now-Time < 60,
    !.
load_library_index_p(M:Spec) :-
    findall(Index, index_file_name(Index, Spec, [access(read)]), List0),
    '$list_to_set'(List0, List),
    retractall(M:index_checked_at(_)),
    get_time(Now),
    assert(M:index_checked_at(Now)),
    (   M:autoload_directories(List)
    ->  true
    ;   retractall(M:library_index(_, _, _)),
        retractall(M:autoload_directories(_)),
        read_index(List, M),
        assert(M:autoload_directories(List))
    ).

%!  index_file_name(-IndexFile, +Spec, +Options) is nondet.
%
%   True if IndexFile is an autoload   index file. Options is passed
%   to  absolute_file_name/3.  This  predicate   searches  the  path
%   =autoload=.
%
%   @see file_search_path/2.

index_file_name(IndexFile, FileSpec, Options) :-
    absolute_file_name(FileSpec,
                       IndexFile,
                       [ file_type(prolog),
                         solutions(all),
                         file_errors(fail)
                       | Options
                       ]).

read_index([], _) :- !.
read_index([H|T], M) :-
    !,
    read_index(H, M),
    read_index(T, M).
read_index(Index, M) :-
    print_message(silent, autoload(read_index(Dir))),
    file_directory_name(Index, Dir),
    setup_call_cleanup(
        '$push_input_context'(autoload_index),
        setup_call_cleanup(
            win_open_index(Index, In),
            read_index_from_stream(Dir, In, M),
            close(In)),
        '$pop_input_context').

%!  win_open_index(+Index, -Stream) is det.
%
%   Open an INDEX.pl file. When concurrently   building the index we may
%   run into sharing violations on Windows.

:- if(current_prolog_flag(windows, true)).
win_open_index(Index, In) :-
    between(1, 10, _),
    catch(open(Index, read, In, [encoding(utf8)]),
          error(permission_error(open, source_sink, _),_), (sleep(0.1),fail)),
    !.
:- endif.
win_open_index(Index, In) :-
    open(Index, read, In, [encoding(utf8)]).

read_index_from_stream(Dir, In, M) :-
    repeat,
        read(In, Term),
        assert_index(Term, Dir, M),
    !.

assert_index(end_of_file, _, _) :- !.
assert_index(index(Term, Module, File), Dir, M) :-
    !,
    atomic_list_concat([Dir, '/', File], Path),
    assertz(M:library_index(Term, Module, Path)),
    fail.
assert_index(index(Name, Arity, Module, File), Dir, M) :-
    !,                                          % Read old index format
    functor(Head, Name, Arity),
    head_meta_any(Head),
    assert_index(index(Head, Module, File), Dir, M).
assert_index(Term, Dir, _) :-
    print_message(error, illegal_autoload_index(Dir, Term)),
    fail.


                /********************************
                *       CREATE INDEX.pl         *
                ********************************/

%!  make_library_index(+Dir) is det.
%
%   Create an index for autoloading  from   the  directory  Dir. The
%   index  file  is  called  INDEX.pl.  In    Dir  contains  a  file
%   MKINDEX.pl, this file is loaded and we  assume that the index is
%   created by directives that appearin   this  file. Otherwise, all
%   source  files  are  scanned  for  their  module-header  and  all
%   exported predicates are added to the autoload index.
%
%   @see make_library_index/2

make_library_index(Dir0) :-
    forall(absolute_file_name(Dir0, Dir,
                              [ expand(true),
                                file_type(directory),
                                file_errors(fail),
                                solutions(all)
                              ]),
           make_library_index2(Dir)).

make_library_index2(Dir) :-
    plfile_in_dir(Dir, 'MKINDEX', _MkIndex, AbsMkIndex),
    access_file(AbsMkIndex, read),
    !,
    load_files(user:AbsMkIndex, [silent(true)]).
make_library_index2(Dir) :-
    findall(Pattern, source_file_pattern(Pattern), PatternList),
    make_library_index2(Dir, PatternList).

%!  make_library_index(+Dir, +Patterns:list(atom)) is det.
%
%   Create an autoload index INDEX.pl for  Dir by scanning all files
%   that match any of the file-patterns in Patterns. Typically, this
%   appears as a directive in MKINDEX.pl.  For example:
%
%   ```
%   :- prolog_load_context(directory, Dir),
%      make_library_index(Dir, ['*.pl']).
%   ```
%
%   @see make_library_index/1.

make_library_index(Dir0, Patterns) :-
    forall(absolute_file_name(Dir0, Dir,
                              [ expand(true),
                                file_type(directory),
                                file_errors(fail),
                                solutions(all)
                              ]),
           make_library_index2(Dir, Patterns)).

make_library_index2(Dir, Patterns) :-
    plfile_in_dir(Dir, 'INDEX', _Index, AbsIndex),
    ensure_slash(Dir, DirS),
    pattern_files(Patterns, DirS, Files),
    (   library_index_out_of_date(Dir, AbsIndex, Files)
    ->  do_make_library_index(AbsIndex, DirS, Files),
        set_flag('$modified_index', true)
    ;   true
    ).

ensure_slash(Dir, DirS) :-
    (   sub_atom(Dir, _, _, 0, /)
    ->  DirS = Dir
    ;   atom_concat(Dir, /, DirS)
    ).

source_file_pattern(Pattern) :-
    user:prolog_file_type(PlExt, prolog),
    PlExt \== qlf,
    atom_concat('*.', PlExt, Pattern).

plfile_in_dir(Dir, Base, PlBase, File) :-
    file_name_extension(Base, pl, PlBase),
    atomic_list_concat([Dir, '/', PlBase], File).

pattern_files([], _, []).
pattern_files([H|T], DirS, Files) :-
    atom_concat(DirS, H, P0),
    expand_file_name(P0, Files0),
    '$append'(Files0, Rest, Files),
    pattern_files(T, DirS, Rest).

library_index_out_of_date(_Dir, Index, _Files) :-
    \+ exists_file(Index),
    !.
library_index_out_of_date(Dir, Index, Files) :-
    time_file(Index, IndexTime),
    (   time_file(Dir, DotTime),
        DotTime - IndexTime > 0.001             % compensate for jitter
    ;   '$member'(File, Files),                 % and rounding
        time_file(File, FileTime),
        FileTime - IndexTime > 0.001
    ),
    !.


do_make_library_index(Index, Dir, Files) :-
    ensure_slash(Dir, DirS),
    '$stage_file'(Index, StagedIndex),
    setup_call_catcher_cleanup(
        open(StagedIndex, write, Out),
        ( print_message(informational, make(library_index(Dir))),
          index_header(Out),
          index_files(Files, DirS, Out)
        ),
        Catcher,
        install_index(Out, Catcher, StagedIndex, Index)).

install_index(Out, Catcher, StagedIndex, Index) :-
    catch(close(Out), Error, true),
    (   silent
    ->  OnError = silent
    ;   OnError = error
    ),
    (   var(Error)
    ->  TheCatcher = Catcher
    ;   TheCatcher = exception(Error)
    ),
    '$install_staged_file'(TheCatcher, StagedIndex, Index, OnError).

%!  index_files(+Files, +Directory, +Out:stream) is det.
%
%   Write index for Files in Directory to the stream Out.

index_files([], _, _).
index_files([File|Files], DirS, Fd) :-
    (   catch(exports(File, Module, Exports, Meta, Public), E,
              print_message(warning, E)),
        nonvar(Module)
    ->  atom_concat(DirS, Local, File),
        file_name_extension(Base, _, Local),
        forall(index_term(Exports, Meta, Public, Term),
               format(Fd, 'index(~k, ~k, ~k).~n',
                      [Term, Module, Base]))
    ;   true
    ),
    index_files(Files, DirS, Fd).

index_term(Exports, Meta, _Public, Term) :-
    '$member'(Export, Exports),
    ground(Export),
    export_term(Export, Meta, Term).
index_term(_Exports, _Meta, Publics, (public):Head) :-
    '$member'(Public, Publics),
    '$pi_head'(Public, Head).

export_term(Op, _Meta, Term) :-
    Op = op(_Pri,_Type,_Name),
    !,
    Term = op:Op.
export_term(PI, Meta, Head) :-
    '$pi_head'(PI, Head),
    (   '$member'(Head, Meta)
    ->  true
    ;   head_meta_any(Head)
    ).

head_meta_any(Head) :-
    (   atom(Head)
    ->  true
    ;   compound_name_arguments(Head, _, Args),
        meta_any(Args)
    ).

meta_any([]).
meta_any([?|T]) :-
    meta_any(T).

index_header(Fd):-
    format(Fd, '/*  Creator: make/0~n~n', []),
    format(Fd, '    Purpose: Provide index for autoload~n', []),
    format(Fd, '*/~n~n', []).

%!  exports(+File, -Module, -Exports) is det.
%!  exports(+File, -Module, -Exports, -Meta) is det.
%
%   Get the exports from a library as  a   list  of PIs. Exports are all
%   exports of the module header (including  op/3 terms) and encountered
%   export/1  directives.  Meta  are  all    heads  in  meta_predicate/1
%   declarations.

:- public exports/3.                            % using by library(prolog_deps).
exports(File, Module, Exports) :-
    exports(File, Module, Exports, _Meta, _Public).

exports(File, Module, Exports, Meta, Public) :-
    (   current_prolog_flag(xref, Old)
    ->  true
    ;   Old = false
    ),
    setup_call_cleanup(
        set_prolog_flag(xref, true),
        snapshot(exports_(File, Module, Exports, Meta, Public)),
        set_prolog_flag(xref, Old)).

exports_(File, Module, Exports, Meta, Public) :-
    State = state(true, _, [], [], []),
    (   '$source_term'(File,
                       _Read,_RLayout,
                       Term,_TermLayout,
                       _Stream,
                       [ syntax_errors(quiet)
                       ]),
        (   Term = (:- module(M,ModuleExports)),
            is_list(ModuleExports),
            arg(1, State, true)
        ->  nb_setarg(1, State, false),
            nb_setarg(2, State, M),
            nb_setarg(3, State, ModuleExports),
            fail
        ;   nb_setarg(1, State, false),
            fail
        ;   Term = (:- export(Export))
        ->  phrase(export_pi(Export), PIs),
            arg(3, State, E0),
            '$append'(E0, PIs, E1),
            nb_setarg(3, State, E1),
            fail
        ;   Term = (:- public(Public))
        ->  phrase(export_pi(Public), PIs),
            arg(5, State, E0),
            '$append'(E0, PIs, E1),
            nb_setarg(5, State, E1),
            fail
        ;   Term = (:- meta_predicate(Heads)),
            phrase(meta(Heads), M1),
            arg(4, State, M0),
            '$append'(M0, M1, M2),
            nb_setarg(4, State, M2)
        ;   Term = (:- use_foreign_library(Lib)),
            nonvar(Lib),
            arg(2, State, M),
            atom(M)
        ->  catch('$syspreds':use_foreign_library_noi(M:Lib), error(_,_), true),
            fail
        ;   Term = (:- Directive),
            nonvar(Directive)
        ->  fail
        ;   Term == []                          % Expansion for conditionals
        ->  fail
        ;   !
        )
    ;   true
    ),
    arg(2, State, Module),
    arg(3, State, Exports),
    arg(4, State, Meta),
    arg(5, State, Public).

export_pi(Var) -->
    { var(Var) },
    !.
export_pi((A,B)) -->
    !,
    export_pi(A),
    export_pi(B).
export_pi(PI) -->
    { ground(PI) },
    [PI].

meta(Var) -->
    { var(Var) },
    !.
meta((A,B)) -->
    !,
    meta(A),
    meta(B).
meta(Head) -->
    { callable(Head) },
    [Head].


                 /*******************************
                 *            EXTENDING         *
                 *******************************/

%!  autoload_path(+Path) is det.
%
%   Add Path to the libraries that are  used by the autoloader. This
%   extends the search  path  =autoload=   and  reloads  the library
%   index.  For example:
%
%     ==
%     :- autoload_path(library(http)).
%     ==
%
%   If this call appears as a directive,  it is term-expanded into a
%   clause  for  user:file_search_path/2  and  a  directive  calling
%   reload_library_index/0. This keeps source information and allows
%   for removing this directive.

autoload_path(Alias) :-
    (   user:file_search_path(autoload, Alias)
    ->  true
    ;   assertz(user:file_search_path(autoload, Alias)),
        reload_library_index
    ).

system:term_expansion((:- autoload_path(Alias)),
                      [ user:file_search_path(autoload, Alias),
                        (:- reload_library_index)
                      ]).


		 /*******************************
		 *      RUNTIME AUTOLOADER	*
		 *******************************/

%!  $autoload'(:PI) is semidet.
%
%   Provide PI by autoloading.  This checks:
%
%     - Explicit autoload/2 declarations
%     - Explicit autoload/1 declarations
%     - The library if current_prolog_flag(autoload, true) holds.

'$autoload'(PI) :-
    source_location(File, _Line),
    !,
    setup_call_cleanup(
        '$start_aux'(File, Context),
        '$autoload2'(PI),
        '$end_aux'(File, Context)).
'$autoload'(PI) :-
    '$autoload2'(PI).

'$autoload2'(PI) :-
    setup_call_cleanup(
        leave_sandbox(Old),
        '$autoload3'(PI),
        restore_sandbox(Old)).

leave_sandbox(Sandboxed) :-
    current_prolog_flag(sandboxed_load, Sandboxed),
    set_prolog_flag(sandboxed_load, false).
restore_sandbox(Sandboxed) :-
    set_prolog_flag(sandboxed_load, Sandboxed).

'$autoload3'(PI) :-
    autoload_from(PI, LoadModule, FullFile),
    do_autoload(FullFile, PI, LoadModule).

%!  autoload_from(+PI, -LoadModule, -PlFile) is semidet.
%
%   True when PI can be defined  by   loading  File which is defined the
%   module LoadModule.

autoload_from(Module:PI, LoadModule, FullFile) :-
    autoload_in(Module, explicit),
    current_autoload(Module:File, Ctx, import(Imports)),
    memberchk(PI, Imports),
    library_info(File, Ctx, FullFile, LoadModule, Exports),
    (   pi_in_exports(PI, Exports)
    ->  !
    ;   autoload_error(Ctx, not_exported(PI, File, FullFile, Exports)),
        fail
    ).
autoload_from(Module:Name/Arity, LoadModule, FullFile) :-
    autoload_in(Module, explicit),
    PI = Name/Arity,
    current_autoload(Module:File, Ctx, all),
    library_info(File, Ctx, FullFile, LoadModule, Exports),
    pi_in_exports(PI, Exports).
autoload_from(Module:Name/Arity, LoadModule, Library) :-
    autoload_in(Module, general),
    '$find_library'(Module, Name, Arity, LoadModule, Library).

:- public autoload_in/2.                        % used in syspred

autoload_in(Module, How) :-
    current_prolog_flag(autoload, AutoLoad),
    autoload_in(AutoLoad, How, Module),
    !.

%!  autoload_in(+AutoloadFlag, +AutoloadMode, +TargetModule) is semidet.

autoload_in(true,             _,        _).
autoload_in(explicit,         explicit, _).
autoload_in(user,             _,        user).
autoload_in(user_or_explicit, explicit, _).
autoload_in(user_or_explicit, _,        user).


%!  do_autoload(Library, :PI, +LoadModule) is det.
%
%   Load File, importing PI into the qualified  module. File is known to
%   define LoadModule. There are three cases:
%
%     - The target is the autoload module itself.  Uncommon.
%     - We already loaded this module. Note that
%       '$get_predicate_attribute'/3 alone is not enough as it will
%       consider auto-import from `user`. '$c_current_predicate'/2
%       verifies the predicate really exists, but doesn't validate
%       that it is defined.
%     - We must load the module and import the target predicate.
%
%   @arg Library is an absolute file   name, either without extension or
%   with the source (.pl) extension.

do_autoload(Library, Module:Name/Arity, LoadModule) :-
    functor(Head, Name, Arity),
    '$update_autoload_level'([autoload(true)], Old),
    verbose_autoload(Module:Name/Arity, Library),
    loadable_file(Library, File),
    '$compilation_mode'(OldComp, database),
    (   Module == LoadModule
    ->  ensure_loaded(Module:File)
    ;   (   '$c_current_predicate'(_, LoadModule:Head),
            '$get_predicate_attribute'(LoadModule:Head, defined, 1),
            \+ '$loading'(Library)
        ->  Module:import(LoadModule:Name/Arity)
        ;   use_module(Module:File, [Name/Arity])
        ),
        warn_autoload(Module, LoadModule:Name/Arity)
    ),
    '$set_compilation_mode'(OldComp),
    '$set_autoload_level'(Old),
    '$c_current_predicate'(_, Module:Head).

loadable_file(PlFile, File) :-
    exists_file(PlFile), !,
    File = PlFile.
loadable_file(PlFile, Base) :-
    file_name_extension(Base, pl, PlFile),
    !.
loadable_file(File, File).

verbose_autoload(PI, Library) :-
    current_prolog_flag(verbose_autoload, true),
    !,
    set_prolog_flag(verbose_autoload, false),
    print_message(informational, autoload(PI, Library)),
    set_prolog_flag(verbose_autoload, true).
verbose_autoload(PI, Library) :-
    print_message(silent, autoload(PI, Library)).

%!  autoload_call(:Goal)
%
%   Call Goal, optionally autoloading it first.

autoload_call(Goal) :-
    '$pi_head'(PI, Goal),
    (   current_predicate(PI)
    ->  true
    ;   '$autoload'(PI)
    ),
    call(Goal).

%!  autoloadable(:Head, -File) is nondet.
%
%   True when Head can be  autoloaded   from  File.  This implements the
%   predicate_property/2 property autoload(File).  The   module  must be
%   instantiated.

:- public                               % used from predicate_property/2
    autoloadable/2.

autoloadable(M:Head, FullFile) :-
    atom(M),
    current_module(M),
    autoload_in(M, explicit),
    (   callable(Head)
    ->  goal_name_arity(Head, Name, Arity),
        autoload_from(M:Name/Arity, _, FullFile)
    ;   findall((M:H)-F, autoloadable_2(M:H, F), Pairs),
        (   '$member'(M:Head-FullFile, Pairs)
        ;   current_autoload(M:File, Ctx, all),
            library_info(File, Ctx, FullFile, _, Exports),
            '$member'(PI, Exports),
            '$pi_head'(PI, Head),
            \+ memberchk(M:Head-_, Pairs)
        )
    ).
autoloadable(M:Head, FullFile) :-
    (   var(M)
    ->  autoload_in(any, general)
    ;   autoload_in(M, general)
    ),
    (   callable(Head)
    ->  goal_name_arity(Head, Name, Arity),
        (   '$find_library'(_, Name, Arity, _, FullFile)
        ->  true
        )
    ;   '$in_library'(Name, Arity, autoload),
        functor(Head, Name, Arity)
    ).


autoloadable_2(M:Head, FullFile) :-
    current_autoload(M:File, Ctx, import(Imports)),
    library_info(File, Ctx, FullFile, _LoadModule, _Exports),
    '$member'(PI, Imports),
    '$pi_head'(PI, Head).

goal_name_arity(Head, Name, Arity) :-
    compound(Head),
    !,
    compound_name_arity(Head, Name, Arity).
goal_name_arity(Head, Head, 0).

%!  library_info(+Spec, +AutoloadContext, -FullFile, -Module, -Exports)
%
%   Find information about a library. Spec  is the file specification as
%   it appears in the  autoload/1,2  call.   AutoloadContext  is  a term
%   File:Line, providing the location of the directive.
%
%   @arg FullFile is the source (.pl) file in canonical (absolute)
%   notation.
%   @arg Module is the module defined in FullFile
%   @arg Exports is a list of predicate indicators.

library_info(Spec, _, FullFile, Module, Exports) :-
    '$resolved_source_path'(Spec, FullFile, []),
    !,
    (   \+ '$loading_file'(FullFile, _Queue, _LoadThread)
    ->  '$current_module'(Module, FullFile),
        '$module_property'(Module, exports(Exports))
    ;   library_info_from_file(FullFile, _, Module, Exports)
    ).
library_info(Spec, Context, FullFile, Module, Exports) :-
    (   Context = (Path:_Line)
    ->  Extra = [relative_to(Path)]
    ;   Extra = []
    ),
    (   absolute_file_name(Spec, AbsFile,
                           [ file_type(prolog),
                             access(read),
                             file_errors(fail)
                           | Extra
                           ])
    ->  library_info_from_file(AbsFile, FullFile, Module, Exports),
        '$register_resolved_source_path'(Spec, FullFile)
    ;   absolute_file_name(Spec, FullFile,
                           [ file_type(prolog),
                             solutions(all),
                             file_errors(fail)
                           | Extra
                           ]),
        source_file(FullFile),
        '$current_module'(Module, FullFile)
    ->  '$module_property'(Module, exports(Exports))
    ;   autoload_error(Context, no_file(Spec)),
        fail
    ).

library_info_from_file(QlfFile, PlFile, Module, Exports) :-
    file_name_extension(_, qlf, QlfFile),
    !,
    '$qlf_module'(QlfFile, Info),
    _{module:Module, exports:Exports, file:PlFile} :< Info.
library_info_from_file(PlFile, PlFile, Module, Exports) :-
    setup_call_cleanup(
        '$set_source_module'(OldModule, system),
        setup_call_cleanup(
            '$open_source'(PlFile, In, State, [], []),
            '$term_in_file'(In, _Read, _RLayout, Term, _TLayout, _Stream,
                            [PlFile], []),
            '$close_source'(State, true)),
        '$set_source_module'(OldModule)),
    (   Term = (:- module(Module, Exports))
    ->  !
    ;   nonvar(Term),
        skip_header(Term)
    ->  fail
    ;   '$domain_error'(module_header, Term)
    ).

skip_header(begin_of_file).


:- dynamic printed/3.
:- volatile printed/3.

autoload_error(Context, Error) :-
    suppress(Context, Error),
    !.
autoload_error(Context, Error) :-
    get_time(Now),
    assertz(printed(Context, Error, Now)),
    print_message(warning, error(autoload(Error), autoload(Context))).

suppress(Context, Error) :-
    printed(Context, Error, Printed),
    get_time(Now),
    (   Now - Printed < 1
    ->  true
    ;   retractall(printed(Context, Error, _)),
        fail
    ).


		 /*******************************
		 *            CALLBACK		*
		 *******************************/

:- public
    set_autoload/1.

%!  set_autoload(+Value) is det.
%
%   Hook called from set_prolog_flag/2 when  autoloading is switched. If
%   the desired value is `false` we   should  materialize all registered
%   requests for autoloading. We must do so before disabling autoloading
%   as loading the files may require autoloading.

set_autoload(FlagValue) :-
    current_prolog_flag(autoload, FlagValue),
    !.
set_autoload(FlagValue) :-
    \+ autoload_in(FlagValue, explicit, any),
    !,
    setup_call_cleanup(
        nb_setval('$autoload_disabling', true),
        materialize_autoload(Count),
        nb_delete('$autoload_disabling')),
    print_message(informational, autoload(disabled(Count))).
set_autoload(_).

materialize_autoload(Count) :-
    State = state(0),
    forall(current_predicate(M:'$autoload'/3),
           materialize_autoload(M, State)),
    arg(1, State, Count).

materialize_autoload(M, State) :-
    (   current_autoload(M:File, Context, Import),
        library_info(File, Context, PlFile, _LoadModule, _Exports),
        arg(1, State, N0),
        N is N0+1,
        nb_setarg(1, State, N),
        loadable_file(PlFile, LoadFile),
        (   Import == all
        ->  verbose_autoload(M:all, PlFile),
            use_module(M:LoadFile)
        ;   Import = import(Preds)
        ->  verbose_autoload(M:Preds, PlFile),
            use_module(M:LoadFile, Preds)
        ),
        fail
    ;   true
    ),
    abolish(M:'$autoload'/3).


		 /*******************************
		 *          AUTOLOAD/2		*
		 *******************************/

autoload(M:File) :-
    (   \+ autoload_in(M, explicit)
    ;   nb_current('$autoload_disabling', true)
    ),
    !,
    use_module(M:File).
autoload(M:File) :-
    '$must_be'(filespec, File),
    source_context(Context),
    assert_autoload(M, File, Context, all).

autoload(M:File, Imports) :-
    (   \+ autoload_in(M, explicit)
    ;   nb_current('$autoload_disabling', true)
    ),
    !,
    use_module(M:File, Imports).
autoload(M:File, Imports0) :-
    '$must_be'(filespec, File),
    valid_imports(Imports0, Imports),
    source_context(Context),
    register_autoloads(Imports, M, File, Context),
    assert_autoload(M, File, Context, import(Imports)).

source_context(Path:Line) :-
    source_location(Path, Line),
    !.
source_context(-).

%!  assert_autoload(+Module, +File, +Context, +Imports) is det.
%
%   Assert that Module can autoload  predicates   defines  in Imports by
%   loading File. Context provides the file owner of the declaration.
%
%   @arg Imports is either `all`  or   imports(List).  That latter comes
%   from an autoload/2 directive.

assert_autoload(Module, File, _, Imports) :-
    current_autoload(Module:File, _, Imports),
    !.
assert_autoload(Module, File, Context, Imports) :-
    set_admin_properties(Module),
    Clause = Module:'$autoload'(File, Context, Imports),
    '$initialization_context'(Source, Ctx),
    '$store_admin_clause2'(Clause, _Layout, Source, Ctx).

set_admin_properties(Module) :-
    predicate_property(Module:'$autoload'(_,_,_), discontiguous),
    !.
set_admin_properties(Module) :-
    discontiguous(Module:'$autoload'/3).

valid_imports(Imports0, Imports) :-
    '$must_be'(list, Imports0),
    valid_import_list(Imports0, Imports).

valid_import_list([], []).
valid_import_list([H0|T0], [H|T]) :-
    '$pi_head'(H0, Head),
    '$pi_head'(H, Head),
    valid_import_list(T0, T).

%!  register_autoloads(+ListOfPI, +Module, +File, +Context)
%
%   Put an `autoload` flag on all   predicates declared using autoload/2
%   to prevent duplicates or the user defining the same predicate.
%
%   @arg File is the first argument of autoload/1,2

register_autoloads([], _, _, _).
register_autoloads([PI|T], Module, File, Context) :-
    PI = Name/Arity,
    functor(Head, Name, Arity),
    (   '$get_predicate_attribute'(Module:Head, autoload, 1)
    ->  (   current_autoload(Module:_File0, _Ctx0, import(Imports)),
            memberchk(PI, Imports)
        ->  '$permission_error'(redefine, imported_procedure, PI),
            fail
        ;   Done = true
        )
    ;   '$c_current_predicate'(_, Module:Head), % no auto-import
        '$get_predicate_attribute'(Module:Head, imported, From)
    ->  (   (   '$resolved_source_path'(File, FullFile)
            ->  true
            ;   '$resolve_source_path'(File, FullFile, [])
            ),
            module_property(From, file(FullFile))
        ->  Done = true
        ;   print_message(warning,
                          autoload(already_defined(Module:PI, From))),
            Done = true
        )
    ;   true
    ),
    (   Done == true
    ->  true
    ;   '$set_predicate_attribute'(Module:Head, autoload, 1)
    ),
    register_autoloads(T, Module, File, Context).

pi_in_exports(PI, Exports) :-
    '$member'(E, Exports),
    canonical_pi(E, PI),
    !.

canonical_pi(Var, _) :-
    var(Var), !, fail.
canonical_pi(Name/Arity, Name/Arity).
canonical_pi(Name//A0,   Name/Arity) :-
    Arity is A0 + 2.

current_autoload(M:File, Context, Term) :-
    '$get_predicate_attribute'(M:'$autoload'(_,_,_), defined, 1),
    M:'$autoload'(File, Context, Term).

		 /*******************************
		 *            CHECK		*
		 *******************************/

%!  warn_autoload(+TargetModule, :PI) is det.
%
%   @arg PI is of the shape LoadModule:Name/Arity.

warn_autoload(TargetModule, PI) :-
    current_prolog_flag(warn_autoload, true),
    \+ current_prolog_flag(xref, true),
    \+ nb_current('$autoload_warning', true),
    \+ nowarn_autoload(TargetModule, PI),
    '$pi_head'(PI, Head),
    source_file(Head, File),
    '$source_defines_expansion'(File),
    setup_call_cleanup(
        b_setval('$autoload_warning', true),
        print_message(warning,
                      deprecated(autoload(TargetModule, File, PI, expansion))),
        nb_delete('$autoload_warning')).
warn_autoload(_, _).

%!  nowarn_autoload(+TargetModule, +LoadModulePI) is semidet.
%
%   True when LoadModule:'$nowarn_autoload'(PI,TargetModule)  is defined
%   and true.
%
%   @tbd As is, these  facts  must  be   defined  by  the  library being
%   autoloaded. Possibly we want a specific autoload declaration. As all
%   this only affects the Prolog libraries,   we can always change this.
%   One option might be this, where `How`   is one of `true`, `false` or
%   `warning`.
%
%      :- autoloadable(PI, How)

nowarn_autoload(TargetModule, LoadModule:PI) :-
    NoWarn = LoadModule:'$nowarn_autoload'(PI,TargetModule),
    '$c_current_predicate'(_, NoWarn),
    \+ '$get_predicate_attribute'(NoWarn, imported, _From),
    call(NoWarn).


                 /*******************************
                 *             REQUIRE          *
                 *******************************/

%!  require(:ListOfPredIndicators) is det.
%
%   Register the predicates  in   ListOfPredIndicators  for  autoloading
%   using autoload/2 if they are not system predicates.

require(M:Spec) :-
    (   is_list(Spec)
    ->  List = Spec
    ;   phrase(comma_list(Spec), List)
    ), !,
    require(List, M, FromLib),
    keysort(FromLib, Sorted),
    by_file(Sorted, Autoload),
    forall('$member'(File-Import, Autoload),
           autoload(M:File, Import)).
require(_:Spec) :-
    '$type_error'(list, Spec).

require([],_, []).
require([H|T], M, Needed) :-
   '$pi_head'(H, Head),
   (   '$get_predicate_attribute'(system:Head, defined, 1)
   ->  require(T, M, Needed)
   ;   '$pi_head'(Module:Name/Arity, M:Head),
       (   '$find_library'(Module, Name, Arity, LoadModule, Library)
       ->  (   current_predicate(LoadModule:Name/Arity)
           ->  Module:import(LoadModule:Name/Arity),
               require(T, M, Needed)
           ;   Needed = [Library-H|More],
               require(T, M, More)
           )
       ;   print_message(error, error(existence_error(procedure, Name/Arity), _)),
           require(T, M, Needed)
       )
   ).

by_file([], []).
by_file([File-PI|T0], [Spec-[PI|PIs]|T]) :-
    on_path(File, Spec),
    same_file(T0, File, PIs, T1),
    by_file(T1, T).

on_path(Library, library(Base)) :-
    file_base_name(Library, Base),
    findall(Path, plain_source(library(Base), Path), [Library]),
    !.
on_path(Library, Library).

plain_source(Spec, Path) :-
    absolute_file_name(Spec, PathExt,
                       [ file_type(prolog),
                         access(read),
                         file_errors(fail),
                         solutions(all)
                       ]),
    file_name_extension(Path, _, PathExt).

same_file([File-PI|T0], File, [PI|PIs], T) :-
    !,
    same_file(T0, File, PIs, T).
same_file(List, _, [], List).

comma_list(Var) -->
    { var(Var),
      !,
      '$instantiation_error'(Var)
    }.
comma_list((A,B)) -->
    !,
    comma_list(A),
    comma_list(B).
comma_list(A) -->
    [A].
