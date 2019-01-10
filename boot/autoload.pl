/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2012, University of Amsterdam
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

:- module('$autoload',
          [ '$find_library'/5,
            '$in_library'/3,
            '$define_predicate'/1,
            '$update_library_index'/0,
            make_library_index/1,
            make_library_index/2,
            reload_library_index/0,
            autoload_path/1
          ]).

:- dynamic
    library_index/3,                % Head x Module x Path
    autoload_directories/1,         % List
    index_checked_at/1.             % Time
:- volatile
    library_index/3,
    autoload_directories/1,
    index_checked_at/1.

user:file_search_path(autoload, library(.)).


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
    index_file_name(IndexFile, [access([read,write])]),
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
    with_mutex('$autoload', clear_library_index).

clear_library_index :-
    retractall(library_index(_, _, _)),
    retractall(autoload_directories(_)),
    retractall(index_checked_at(_)).


%!  load_library_index(?Name, ?Arity) is det.
%
%   Try to find Name/Arity  in  the   library.  If  the predicate is
%   there, we are happy. If not, we  check whether the set of loaded
%   libraries has changed and if so we reload the index.

load_library_index(Name, Arity) :-
    atom(Name), integer(Arity),
    functor(Head, Name, Arity),
    library_index(Head, _, _),
    !.
load_library_index(_, _) :-
    notrace(with_mutex('$autoload', load_library_index_p)).

load_library_index_p :-
    index_checked_at(Time),
    get_time(Now),
    Now-Time < 60,
    !.
load_library_index_p :-
    findall(Index, index_file_name(Index, [access(read)]), List0),
    list_set(List0, List),
    retractall(index_checked_at(_)),
    get_time(Now),
    assert(index_checked_at(Now)),
    (   autoload_directories(List)
    ->  true
    ;   retractall(library_index(_, _, _)),
        retractall(autoload_directories(_)),
        read_index(List),
        assert(autoload_directories(List))
    ).

list_set([], R) :-                      % == list_to_set/2 from library(lists)
    closel(R).
list_set([H|T], R) :-
    memberchk(H, R),
    !,
    list_set(T, R).

closel([]) :- !.
closel([_|T]) :-
    closel(T).


%!  index_file_name(-IndexFile, +Options) is nondet.
%
%   True if IndexFile is an autoload   index file. Options is passed
%   to  absolute_file_name/3.  This  predicate   searches  the  path
%   =autoload=.
%
%   @see file_search_path/2.

index_file_name(IndexFile, Options) :-
    absolute_file_name(autoload('INDEX'),
                       IndexFile,
                       [ file_type(prolog),
                         solutions(all),
                         file_errors(fail)
                       | Options
                       ]).

read_index([]) :- !.
read_index([H|T]) :-
    !,
    read_index(H),
    read_index(T).
read_index(Index) :-
    print_message(silent, autoload(read_index(Dir))),
    file_directory_name(Index, Dir),
    setup_call_cleanup(
        '$push_input_context'(autoload_index),
        setup_call_cleanup(
            open(Index, read, In),
            read_index_from_stream(Dir, In),
            close(In)),
        '$pop_input_context').

read_index_from_stream(Dir, In) :-
    repeat,
        read(In, Term),
        assert_index(Term, Dir),
    !.

assert_index(end_of_file, _) :- !.
assert_index(index(Name, Arity, Module, File), Dir) :-
    !,
    functor(Head, Name, Arity),
    atomic_list_concat([Dir, '/', File], Path),
    assertz(library_index(Head, Module, Path)),
    fail.
assert_index(Term, Dir) :-
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
    plfile_in_dir(Dir, 'MKINDEX', MkIndex, AbsMkIndex),
    access_file(AbsMkIndex, read),
    !,
    setup_call_cleanup(
        working_directory(OldDir, Dir),
        load_files(user:MkIndex, [silent(true)]),
        working_directory(_, OldDir)).
make_library_index2(Dir) :-
    findall(Pattern, source_file_pattern(Pattern), PatternList),
    make_library_index2(Dir, PatternList).

%!  make_library_index(+Dir, +Patterns:list(atom)) is det.
%
%   Create an autoload index INDEX.pl for  Dir by scanning all files
%   that match any of the file-patterns in Patterns. Typically, this
%   appears as a directive in MKINDEX.pl.  For example:
%
%     ==
%     :- make_library_index(., ['*.pl']).
%     ==
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
    (   library_index_out_of_date(AbsIndex, Files)
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

library_index_out_of_date(Index, _Files) :-
    \+ exists_file(Index),
    !.
library_index_out_of_date(Index, Files) :-
    time_file(Index, IndexTime),
    (   time_file('.', DotTime),
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
