/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2020, University of Amsterdam
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

:- module('$autoload',
          [ '$find_library'/5,
            '$in_library'/3,
            '$define_predicate'/1,
            '$update_library_index'/0,
            '$autoload'/1,

            make_library_index/1,
            make_library_index/2,
            reload_library_index/0,
            autoload_path/1,

            autoload/1,                         % +File
            autoload/2,                         % +File, +Imports

            require/1				% +Predicates
          ]).

:- meta_predicate
    '$autoload'(:),
    autoload(:),
    autoload(:, +),
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


%!  '$find_library'(+Module, +Name, +Arity, -LoadModule, -Library) is semidet.
%
%   Locate a predicate in the library. Name   and arity are the name
%   and arity of  the  predicate  searched   for.  `Module'  is  the
%   preferred target module. The return  values   are  the full path
%   name (excluding extension) of the library and module declared in
%   that file.

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
    load_library_index(Name, Arity),
    functor(Head, Name, Arity),
    library_index(Head, _, Path).
'$in_library'(Name, Arity, Path) :-
    load_library_index(Name, Arity),
    library_index(Head, _, Path),
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

%!  '$update_library_index'
%
%   Called from make/0 to update the index   of the library for each
%   library directory that has a writable   index.  Note that in the
%   Windows  version  access_file/2  is  mostly   bogus.  We  assert
%   silent/0 to suppress error messages.

'$update_library_index' :-
    setof(Dir, writable_indexed_directory(Dir), Dirs),
    !,
    setup_call_cleanup(
        asserta(silent, Ref),
        guarded_make_library_index(Dirs),
        erase(Ref)),
    (   flag('$modified_index', true, false)
    ->  reload_library_index
    ;   true
    ).
'$update_library_index'.

guarded_make_library_index([]).
guarded_make_library_index([Dir|Dirs]) :-
    (   catch(make_library_index(Dir), E,
              print_message(error, E))
    ->  true
    ;   print_message(warning, goal_failed(make_library_index(Dir)))
    ),
    guarded_make_library_index(Dirs).

%!  writable_indexed_directory(-Dir) is nondet.
%
%   True when Dir is an indexed   library  directory with a writable
%   index, i.e., an index that can be updated.

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
            open(Index, read, In),
            read_index_from_stream(Dir, In, M),
            close(In)),
        '$pop_input_context').

read_index_from_stream(Dir, In, M) :-
    repeat,
        read(In, Term),
        assert_index(Term, Dir, M),
    !.

assert_index(end_of_file, _, _) :- !.
assert_index(index(Name, Arity, Module, File), Dir, M) :-
    !,
    functor(Head, Name, Arity),
    atomic_list_concat([Dir, '/', File], Path),
    assertz(M:library_index(Head, Module, Path)),
    fail.
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
        flag('$modified_index', _, true)
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
        DotTime > IndexTime
    ;   '$member'(File, Files),
        time_file(File, FileTime),
        FileTime > IndexTime
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
    catch(setup_call_cleanup(
              open(File, read, In),
              read(In, Term),
              close(In)),
          E, print_message(warning, E)),
    (   Term = (:- module(Module, Public)),
        is_list(Public)
    ->  atom_concat(DirS, Local, File),
        file_name_extension(Base, _, Local),
        forall(public_predicate(Public, Name/Arity),
               format(Fd, 'index((~k), ~k, ~k, ~k).~n',
                      [Name, Arity, Module, Base]))
    ;   true
    ),
    index_files(Files, DirS, Fd).

public_predicate(Public, PI) :-
    '$member'(PI0, Public),
    canonical_pi(PI0, PI).

canonical_pi(Var, _) :-
    var(Var), !, fail.
canonical_pi(Name/Arity, Name/Arity).
canonical_pi(Name//A0,   Name/Arity) :-
    Arity is A0 + 2.


index_header(Fd):-
    format(Fd, '/*  Creator: make/0~n~n', []),
    format(Fd, '    Purpose: Provide index for autoload~n', []),
    format(Fd, '*/~n~n', []).


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

%!  autoload_from(+PI, -LoadModule, -File) is semidet.
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
autoload_in(explicit_or_user, explicit, _).
autoload_in(user,             explicit, user).
autoload_in(explicit_or_user, explicit, _).
autoload_in(user,             _,        user).
autoload_in(explicit_or_user, general,  user).


%!  do_autoload(+File, :PI, +LoadModule) is det.
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

do_autoload(Library, Module:Name/Arity, LoadModule) :-
    functor(Head, Name, Arity),
    '$update_autoload_level'([autoload(true)], Old),
    verbose_autoload(Module:Name/Arity, Library),
    '$compilation_mode'(OldComp, database),
    (   Module == LoadModule
    ->  ensure_loaded(Module:Library)
    ;   (   '$c_current_predicate'(_, LoadModule:Head),
            '$get_predicate_attribute'(LoadModule:Head, defined, 1),
            \+ '$loading'(Library)
        ->  Module:import(LoadModule:Name/Arity)
        ;   use_module(Module:Library, [Name/Arity])
        )
    ),
    '$set_compilation_mode'(OldComp),
    '$set_autoload_level'(Old),
    '$c_current_predicate'(_, Module:Head).

verbose_autoload(PI, Library) :-
    current_prolog_flag(verbose_autoload, true),
    !,
    set_prolog_flag(verbose_autoload, false),
    print_message(informational, autoload(PI, Library)),
    set_prolog_flag(verbose_autoload, true).
verbose_autoload(PI, Library) :-
    print_message(silent, autoload(PI, Library)).


%!  autoloadable(:Head, -File) is nondet.
%
%   True when Head can be  autoloaded   from  File.  This implements the
%   predicate_property/2 property autoload(File).  The   module  muse be
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
%   Find information about a library.

library_info(Spec, _, FullFile, Module, Exports) :-
    '$resolved_source_path'(Spec, FullFile, []),
    !,
    (   \+ '$loading_file'(FullFile, _Queue, _LoadThread)
    ->  '$current_module'(Module, FullFile),
        '$module_property'(Module, exports(Exports))
    ;   library_info_from_file(FullFile, Module, Exports)
    ).
library_info(Spec, Context, FullFile, Module, Exports) :-
    (   Context = (Path:_Line)
    ->  Extra = [relative_to(Path)]
    ;   Extra = []
    ),
    (   absolute_file_name(Spec, FullFile,
                           [ file_type(prolog),
                             access(read),
                             file_errors(fail)
                           | Extra
                           ])
    ->  '$register_resolved_source_path'(Spec, FullFile),
        library_info_from_file(FullFile, Module, Exports)
    ;   autoload_error(Context, no_file(Spec)),
        fail
    ).


library_info_from_file(FullFile, Module, Exports) :-
    setup_call_cleanup(
        '$open_source'(FullFile, In, State, [], []),
        '$term_in_file'(In, _Read, _RLayout, Term, _TLayout, _Stream,
                        [FullFile], []),
        '$close_source'(State, true)),
    (   Term = (:- module(Module, Exports))
    ->  !
    ;   nonvar(Term),
        skip_header(Term)
    ->  fail
    ;   throw(error(domain_error(module_file, FullFile), _))
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
        library_info(File, Context, FullFile, _LoadModule, _Exports),
        arg(1, State, N0),
        N is N0+1,
        nb_setarg(1, State, N),
        (   Import == all
        ->  verbose_autoload(M:all, FullFile),
            use_module(M:FullFile)
        ;   Import = import(Preds)
        ->  verbose_autoload(M:Preds, FullFile),
            use_module(M:FullFile, Preds)
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
    retractall(M:'$autoload'(File, _, _)),
    assert_autoload(M:'$autoload'(File, Context, all)).

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
    (   current_autoload(M:File, _, import(Imports))
    ->  true
    ;   assert_autoload(M:'$autoload'(File, Context, import(Imports)))
    ).

source_context(Path:Line) :-
    source_location(Path, Line),
    !.
source_context(-).

assert_autoload(Clause) :-
    '$initialization_context'(Source, Ctx),
    '$store_admin_clause2'(Clause, _Layout, Source, Ctx).

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

current_autoload(M:File, Context, Term) :-
    '$get_predicate_attribute'(M:'$autoload'(_,_,_), defined, 1),
    M:'$autoload'(File, Context, Term).

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
       (   '$find_library'(Module, Name, Arity, _LoadModule, Library)
       ->  Needed = [Library-H|More],
           require(T, M, More)
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
