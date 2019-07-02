/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2019, VU University Amsterdam
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

:- module(xsb,
          [ add_lib_dir/1,			% +Directories
	    add_lib_dir/2,			% +Root, +Directories

            compile/2,                          % +File, +Options
            load_dyn/1,                         % +File
            load_dyn/2,                         % +File, +Direction
            load_dync/1,                        % +File
            load_dync/2,                        % +File, +Direction

            set_global_compiler_options/1,	% +Options
            compiler_options/1,			% +Options

            xsb_import/2,                       % +Preds, From
            xsb_dynamic/1,                      % +Preds

            fail_if/1,				% :Goal

            not_exists/1,			% :Goal
            sk_not/1,				% :Goal
            gc_tables/1,                        % -Remaining

            is_most_general_term/1,		% @Term

            cputime/1,				% -Seconds
            walltime/1,				% -Seconds

            path_sysop/2,                       % +Op, ?Value
            path_sysop/3,                       % +Op, ?Value1, ?Value2

            op(1050,  fy, import),
            op(1050,  fx, export),
            op(1040, xfx, from),
            op(1100,  fy, index),               % ignored
            op(1100,  fy, ti),                  % transformational indexing?
            op(1100,  fx, mode),                % ignored
            op(1045, xfx, as),
            op(900,   fy, tnot),
            op(900,   fy, not)                  % defined as op in XSB
          ]).
:- use_module(library(error)).
:- use_module(library(debug)).
:- use_module(library(dialect/xsb/source)).
:- use_module(library(dialect/xsb/tables)).
:- use_module(library(aggregate)).
:- use_module(library(option)).

/** <module> XSB Prolog compatibility layer

This  module  provides  partial  compatibility   with  the  [XSB  Prolog
system](http://xsb.sourceforge.net/)
*/

:- meta_predicate
    xsb_import(:, +),                   % Module interaction
    xsb_dynamic(:),

    compile(:, +),                      % Loading files
    load_dyn(:),
    load_dyn(:, +),
    load_dync(:),
    load_dync(:, +),

    fail_if(0),                         % Meta predicates
    not_exists(0),
    sk_not(0).



		 /*******************************
		 *	    LIBRARY SETUP	*
		 *******************************/

%%	push_xsb_library
%
%	Pushes searching for  dialect/xsb  in   front  of  every library
%	directory that contains such as sub-directory.

push_xsb_library :-
    (   absolute_file_name(library(dialect/xsb), Dir,
			   [ file_type(directory),
			     access(read),
			     solutions(all),
			     file_errors(fail)
			   ]),
	asserta((user:file_search_path(library, Dir) :-
		prolog_load_context(dialect, xsb))),
	fail
    ;   true
    ).

:- push_xsb_library.

%!  setup_dialect
%
%   Further dialect initialization.  Called from expects_dialect/1.

:- public setup_dialect/0.

setup_dialect :-
    style_check(-discontiguous).

:- multifile
    user:term_expansion/2,
    user:goal_expansion/2.

:- dynamic
    moved_directive/2.

% Register XSB specific term-expansion to rename conflicting directives.

user:term_expansion(In, Out) :-
    prolog_load_context(dialect, xsb),
    xsb_term_expansion(In, Out).

xsb_term_expansion((:- Directive), []) :-
    prolog_load_context(file, File),
    retract(moved_directive(File, Directive)),
    debug(xsb(header), 'Moved to head: ~p', [Directive]),
    !.
xsb_term_expansion((:- import Preds from From),
                   (:- xsb_import(Preds, From))).
xsb_term_expansion((:- index(_PI, _, _)), []).  % what is tbis?
xsb_term_expansion((:- index(_PI, _How)), []).
xsb_term_expansion((:- index(_PI)), []).
xsb_term_expansion((:- ti(_PI)), []).
xsb_term_expansion((:- mode(_Modes)), []).
xsb_term_expansion((:- dynamic(Preds)), (:- xsb_dynamic(Preds))).

user:goal_expansion(In, Out) :-
    prolog_load_context(dialect, xsb),
    (   xsb_mapped_predicate(In, Out)
    ->  true
    ;   xsb_inlined_goal(In, Out)
    ).

xsb_mapped_predicate(expand_file_name(File, Expanded),
                     xsb_expand_file_name(File, Expanded)).
xsb_mapped_predicate(abolish_module_tables(UserMod),
                     abolish_module_tables(user)) :-
    UserMod == usermod.
xsb_mapped_predicate(dynamic(Spec), xsb_dynamic(Spec)) :-
    Spec = as(_,_).

xsb_inlined_goal(fail_if(P), \+(P)).

%!  xsb_import(:Predicates, +From)
%
%   Make Predicates visible in From. As the XSB library structructure is
%   rather different from SWI-Prolog's, this is a heuristic process.

:- dynamic
    mapped__module/2.                           % XSB name -> Our name

xsb_import(Into:Preds, From) :-
    mapped__module(From, Mapped),
    !,
    xsb_import(Preds, Into, Mapped).
xsb_import(Into:Preds, From) :-
    xsb_import(Preds, Into, From).

xsb_import(Var, _Into, _From) :-
    var(Var),
    !,
    instantiation_error(Var).
xsb_import((A,B), Into, From) :-
    !,
    xsb_import(A, Into, From),
    xsb_import(B, Into, From).
xsb_import(Name/Arity, Into, From) :-
    functor(Head, Name, Arity),
    xsb_mapped_predicate(Head, NewHead),
    functor(NewHead, NewName, Arity),
    !,
    xsb_import(NewName/Arity, Into, From).
xsb_import(PI, Into, usermod) :-
    !,
    export(user:PI),
    @(import(user:PI), Into).
xsb_import(Name/Arity, Into, _From) :-
    functor(Head, Name, Arity),
    predicate_property(Into:Head, iso),
    !,
    debug(xsb(import), '~p: already visible (ISO)', [Into:Name/Arity]).
xsb_import(PI, Into, From) :-
    import_from_module(clean, PI, Into, From),
    !.
xsb_import(PI, Into, From) :-
    prolog_load_context(file, Here),
    absolute_file_name(From, Path,
                       [ extensions(['P', pl, prolog]),
                         access(read),
                         relative_to(Here),
                         file_errors(fail)
                       ]),
    !,
    debug(xsb(import), '~p: importing from ~p', [Into:PI, Path]),
    load_module(Into:Path, PI).
xsb_import(PI, Into, From) :-
    absolute_file_name(library(From), Path,
                       [ extensions(['P', pl, prolog]),
                         access(read),
                         file_errors(fail)
                       ]),
    !,
    debug(xsb(import), '~p: importing from ~p', [Into:PI, Path]),
    load_module(Into:Path, PI).
xsb_import(Name/Arity, Into, _From) :-
    functor(Head, Name, Arity),
    predicate_property(Into:Head, visible),
    !,
    debug(xsb(import), '~p: already visible', [Into:Name/Arity]).
xsb_import(PI, Into, From) :-
    import_from_module(dirty, PI, Into, From),
    !.
xsb_import(_Name/_Arity, _Into, From) :-
    existence_error(xsb_module, From).

%!  import_from_module(?Clean, +PI, +Into, +From) is semidet.
%
%   Try to import PI into  module  Into   from  Module  From.  The clean
%   version only deals  with  cleanly   exported  predicates.  The dirty
%   version is more aggressive.

import_from_module(clean, PI, Into, From) :-
    module_property(From, exports(List)),
    memberchk(PI, List),
    !,
    debug(xsb(import), '~p: importing from module ~p', [Into:PI, From]),
    @(import(From:PI), Into).
import_from_module(dirty, PI, Into, From) :-
    current_predicate(From:PI),
    !,
    debug(xsb(import), '~p: importing from module ~p', [Into:PI, From]),
    (   check_exported(From, PI)
    ->  @(import(From:PI), Into)
    ;   true
    ).
import_from_module(dirty, PI, _Into, From) :-
    module_property(From, file(File)),
    !,
    print_message(error, xsb(not_in_module(File, From, PI))).

check_exported(Module, PI) :-
    module_property(Module, exports(List)),
    memberchk(PI, List),
    !.
check_exported(Module, PI) :-
    module_property(Module, file(File)),
    print_message(error, xsb(not_in_module(File, Module, PI))).

load_module(Into:Path, PI) :-
    use_module(Into:Path, []),
    (   module_property(Module, file(Path))
    ->  file_base_name(Path, File),
        file_name_extension(Base, _, File),
        (   Base == Module
        ->  true
        ;   atom_concat(xsb_, Base, Module)
        ->  map_module(Base, Module)
        ;   print_message(warning,
                          xsb(file_loaded_into_mismatched_module(Path, Module))),
            map_module(Base, Module)
        )
    ;   print_message(warning, xsb(loaded_unknown_module(Path)))
    ),
    import_from_module(_, PI, Into, Module).

map_module(XSB, Module) :-
    mapped__module(XSB, Module),
    !.
map_module(XSB, Module) :-
    assertz(mapped__module(XSB, Module)).


		 /*******************************
		 *      BUILT-IN PREDICATES	*
		 *******************************/

%!  add_lib_dir(+Directories) is det.
%!  add_lib_dir(+Root, +Directories) is det.
%
%   Add    members    of    the    comma      list     Directories    to
%   user:library_directory/1.  If  Root  is  given,    all   members  of
%   Directories are interpreted relative to Root.

add_lib_dir(Directories) :-
    add_lib_dir('.', Directories).

add_lib_dir(_, Var) :-
    var(Var),
    !,
    instantiation_error(Var).
add_lib_dir(Root, (A,B)) :-
    !,
    add_lib_dir(Root, A),
    add_lib_dir(Root, B).
add_lib_dir(Root, a(Dir)) :-
    !,
    add_to_library_directory(Root, Dir, asserta).
add_lib_dir(Root, Dir) :-
    add_to_library_directory(Root, Dir, assertz).

add_to_library_directory(Root, Dir, How) :-
    (   expand_file_name(Dir, [Dir1])
    ->  true
    ;   Dir1 = Dir
    ),
    relative_file_name(TheDir, Root, Dir1),
    exists_directory(TheDir),
    !,
    (   user:library_directory(TheDir)
    ->  true
    ;   call(How, user:library_directory(TheDir))
    ).
add_to_library_directory(_, _, _).

%!  compile(File, Options)
%
%   The XSB version compiles a file into .xwam without loading it. We do
%   not have that. Calling qcompile/1 seems the best start.

compile(File, _Options) :-
    qcompile(File).

%!  load_dyn(+FileName) is det.
%!  load_dyn(+FileName, +Direction) is det.
%!  load_dync(+FileName) is det.
%!  load_dync(+FileName, +Direction) is det.
%
%   Proper implementation requires  the   Quintus  `all_dynamic` option.
%   SWI-Prolog never had that as  clause/2   is  allowed on static code,
%   which is the main reason to want this.
%
%   The _dync_ versions demand source in canonical format. In SWI-Prolog
%   there is little reason to demand this.

load_dyn(File)       :-
    '$style_check'(Style, Style),
    setup_call_cleanup(
        style_check(-singleton),
        load_files(File),
        '$style_check'(_, Style)).
        load_dyn(File, Dir)  :- must_be(oneof([z]), Dir), load_dyn(File).
load_dync(File)      :- load_dyn(File).
load_dync(File, Dir) :- load_dyn(File, Dir).

%!  set_global_compiler_options(+List) is det.
%
%   Set the XSB global compiler options.

:- multifile xsb_compiler_option/1.
:- dynamic   xsb_compiler_option/1.

set_global_compiler_options(List) :-
    must_be(list, List),
    maplist(set_global_compiler_option, List).

set_global_compiler_option(+Option) :-
    !,
    valid_compiler_option(Option),
    (   xsb_compiler_option(Option)
    ->  true
    ;   assertz(xsb_compiler_option(Option))
    ).
set_global_compiler_option(-Option) :-
    !,
    valid_compiler_option(Option),
    retractall(xsb_compiler_option(Option)).
set_global_compiler_option(-Option) :-
    valid_compiler_option(Option),
    (   xsb_compiler_option(Option)
    ->  true
    ;   assertz(xsb_compiler_option(Option))
    ).

valid_compiler_option(Option) :-
    must_be(oneof([ singleton_warnings_off,
                    optimize,
                    allow_redefinition,
                    xpp_on,
                    spec_off
                  ]), Option).

%!  compiler_options(+Options) is det.
%
%   Locally switch the compiler options

compiler_options(Options) :-
    must_be(list, Options),
    maplist(compiler_option, Options).

compiler_option(+Option) :-
    !,
    valid_compiler_option(Option),
    set_compiler_option(Option).
compiler_option(-Option) :-
    !,
    valid_compiler_option(Option),
    clear_compiler_option(Option).
compiler_option(Option) :-
    valid_compiler_option(Option),
    set_compiler_option(Option).

set_compiler_option(singleton_warnings_off) :-
    style_check(-singleton).
set_compiler_option(optimize) :-
    set_prolog_flag(optimise, true).
set_compiler_option(allow_redefinition).
set_compiler_option(xpp_on).
set_compiler_option(spec_off).

clear_compiler_option(singleton_warnings_off) :-
    style_check(+singleton).
clear_compiler_option(optimize) :-
    set_prolog_flag(optimise, false).
clear_compiler_option(allow_redefinition).
clear_compiler_option(xpp_on).

%!  xsb_dynamic(Preds)
%
%   Apply dynamic to the original predicate.  This deals with a sequence
%   that seems common in XSB:
%
%       :- import p/1 from x.
%       :- dynamic p/1.
%
%   This also deals with ``:- dynamic Spec as Flags``.

xsb_dynamic(M:Preds) :-
    xsb_dynamic_(Preds, M, []).

xsb_dynamic_(Preds, _M, _) :-
    var(Preds),
    !,
    instantiation_error(Preds).
xsb_dynamic_(Spec as Flags, M, Options0) :-
    !,
    dyn_flags(Flags, Options0, Options),
    xsb_dynamic_(Spec, M, Options).
xsb_dynamic_((A,B), M, Options) :-
    !,
    xsb_dynamic_(A, M, Options),
    xsb_dynamic_(B, M, Options).
xsb_dynamic_(Name/Arity, M, Options) :-
    functor(Head, Name, Arity),
    '$get_predicate_attribute'(M:Head, imported, M2), % predicate_property/2 requires
    !,                                                % P to be defined.
    dynamic([M2:Name/Arity], Options).
xsb_dynamic_(PI, M, Options) :-
    dynamic([M:PI], Options).

dyn_flags(Var, _, _) :-
    var(Var),
    !,
    instantiation_error(Var).
dyn_flags((A,B), Options0, Options) :-
    !,
    dyn_flags(A, Options0, Options1),
    dyn_flags(B, Options1, Options).
dyn_flags(incremental, Options0, Options) :-
    !,
    merge_options([incremental(true)], Options0, Options).
dyn_flags(opaque, Options0, Options) :-
    !,
    merge_options([incremental(false)], Options0, Options).
dyn_flags(shared, Options0, Options) :-
    !,
    merge_options([thread(shared)], Options0, Options).
dyn_flags(private, Options0, Options) :-
    !,
    merge_options([thread(local)], Options0, Options).
dyn_flags(abstract(Level), Options0, Options) :-
    !,
    merge_options([abstract(Level)], Options0, Options).
dyn_flags(Flag, _, _) :-
    domain_error(xsb_dynamic_flag, Flag).


		 /*******************************
		 *            BUILT-INS		*
		 *******************************/

%!  fail_if(:P)
%
%   Same as \+ (support XSB legacy code).  As the XSB manual claims this
%   is optimized we normally do goal expansion to \+/1.

fail_if(P) :-
    \+ P.

		 /*******************************
		 *      TABLING BUILT-INS	*
		 *******************************/

%!  not_exists(:P).
%!  sk_not(:P).
%
%   XSB tabled negation. According to the XSB manual, sk_not/1 is an old
%   name for not_exists/1. The predicates   tnot/1  and not_exists/1 are
%   not precisely the same. We ignore that for now.

not_exists(P) :-
    tnot(P).

sk_not(P) :-
    not_exists(P).

%!  gc_tables(-Remaining) is det.
%
%   The table abolish predicates leave  the   actual  destruction of the
%   tables to the atom  garbage  collector   to  avoid  deleting  active
%   tables. This predicate runs garbage_collect_atoms/0   and counts the
%   remaining erased tables.
%
%   @compat Due to the heuristic nature of garbage_collect_atoms/0, not
%   all tables may be reclaimed immediately.

gc_tables(Remaining) :-
    garbage_collect_atoms,
    aggregate_all(count, remaining_table(_), Remaining).

remaining_table(Trie) :-
    current_blob(Trie, trie),
    '$is_answer_trie'(Trie),
    '$atom_references'(Trie, 0).


%!  is_most_general_term(@X) is semidet.
%
%   Succeeds if X is  compound  term   with  all  distinct  variables as
%   arguments, or if X is an atom. (It fails if X is a cons node.)
%
%      ```
%      ?- is_most_general_term(f(_,_,_,_)).
%      true.
%      ?- is_most_general_term(abc).
%      true.
%      ?- is_most_general_term(f(X,Y,Z,X)).
%      false.
%      ?- is_most_general_term(f(X,Y,Z,a)).
%      false.
%      ?- is_most_general_term([_|_]).
%      false.
%      ```

is_most_general_term(List) :-
    is_list(List),
    !,
    length(List, Len),
    sort(List, L2),
    length(L2, Len).
is_most_general_term(Term) :-
    compound(Term),
    !,
    compound_name_arity(Term, _, Arity),
    (   Arity == 0
    ->  true
    ;   Term \= [_|_],
        term_variables(Term, Vars),
        sort(Vars, Sorted),
        length(Sorted, Arity)
    ).
is_most_general_term(Term) :-
    atom(Term).


%!  cputime(-Seconds) is det.
%
%   True when Seconds is the used CPU time.

cputime(Seconds) :-
    statistics(cputime, Seconds).

%!  walltime(-Seconds) is det.
%
%   True when Seconds is the wall time sice Prolog was started

walltime(Seconds) :-
    get_time(Now),
    statistics(epoch, Epoch),
    Seconds is Now - Epoch.


%!  path_sysop(+Op, ?Value) is semidet.
%!  path_sysop(+Op, ?Arg1, ?Arg2) is semidet.
%
%   Unified interface to the  operations  on   files.  All  these  calls
%   succeed iff the corresponding system call succeeds.
%
%   @compat The below implementation covers all operations from XSB 3.8.
%   SWI file name operations are always on   POSIX style file names. The
%   implementation may have semantic differences.

path_sysop(isplain, File) :-
    exists_file(File).
path_sysop(isdir, Dir) :-
    exists_directory(Dir).
path_sysop(rm, File) :-
    delete_file(File).
path_sysop(rmdir, Dir) :-
    delete_directory(Dir).
path_sysop(rmdir_rec, Dir) :-
    delete_directory_and_contents(Dir).
path_sysop(cwd, CWD) :-
    working_directory(CWD, CWD).
path_sysop(chdir, CWD) :-
    working_directory(_, CWD).
path_sysop(mkdir, Dir) :-
    make_directory(Dir).
path_sysop(exists, Entry) :-
    access_file(Entry, exist).
path_sysop(readable, Entry) :-
    access_file(Entry, read).
path_sysop(writable, Entry) :-
    access_file(Entry, write).
path_sysop(executable, Entry) :-
    access_file(Entry, execute).
path_sysop(tmpfilename, Name) :-
    tmp_file(swi, Name).
path_sysop(isabsolute, Name) :-
    is_absolute_file_name(Name).


path_sysop(rename, Old, New) :-
    rename_file(Old, New).
path_sysop(copy, From, To) :-
    copy_file(From, To).
path_sysop(link, From, To) :-
    link_file(From, To, symbolic).
path_sysop(modtime, Path, Time) :-
    time_file(Path, Time).
path_sysop(newerthan, Path1, Path2) :-
    time_file(Path1, Time1),
    (   catch(time_file(Path2, Time2), error(existence_error(_,_),_), fail)
    ->  Time1 > Time2
    ;   true
    ).
path_sysop(size, Path, Size) :-
    size_file(Path, Size).
path_sysop(extension, Path, Ext) :-
    file_name_extension(_, Ext, Path).
path_sysop(basename, Path, Base) :-
    file_base_name(Path, File),
    file_name_extension(Base, _, File).
path_sysop(dirname, Path, Dir) :-
    file_directory_name(Path, Dir0),
    (   sub_atom(Dir0, _, _, 0, /)
    ->  Dir = Dir0
    ;   atom_concat(Dir0, /, Dir)
    ).
path_sysop(expand, Name, Path) :-
    absolute_file_name(Name, Path).


		 /*******************************
		 *           MESSAGES		*
		 *******************************/

:- multifile
    prolog:message//1.

prolog:message(xsb(not_in_module(File, Module, PI))) -->
    [ '~p, implementing ~p does not export ~p'-[File, Module, PI] ].
prolog:message(xsb(file_loaded_into_mismatched_module(File, Module))) -->
    [ 'File ~p defines module ~p'-[File, Module] ].
