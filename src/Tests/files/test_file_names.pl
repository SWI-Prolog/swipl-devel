/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2022, SWI-Prolog Solutions b.v.
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

:- module(test_file_names,
          [ test_file_names/0,
            segment_limit/2             % +Type,-Limit
          ]).
:- encoding(utf8).

:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(filesex)).
:- use_module(library(lists)).
:- use_module(library(gensym)).
:- use_module(library(terms)).
:- use_module(library(plunit)).

/** <module> File system name tests

This test module tests creating and accessing  files with names that may
cause problems such as  long  path   names  or  names containing Unicode
characters.

*/

:- op(400,fx,/).

% :- debug(files).

:- dynamic
    dir/1,
    file/1,
    name_policy/1,
    init_nesting/1.

:- meta_predicate
    run_with_name(+, +, -, 0),
    with_name_policy(+, 0),
    bin_search(1, +, +, -).

test_file_names :-
    run_tests([ file_names
              ]).

%!  can_represent(+Code) is semidet.
%
%   True when the default  encoding  can   represet  Code.  Note that on
%   Windows we use the Unicode functions  to   access  files,  so we can
%   represent all characters regardless of OS locale settings.

can_represent(_Code) :-
    current_prolog_flag(windows, true),
    !.
can_represent(Code) :-
    catch(setup_call_cleanup(
              open_null_stream(S),
              ( set_stream(S, encoding(text)),
                put_code(S, Code)
              ),
              close(S)),
          error(_,_),
          fail).

:- begin_tests(file_names).

test(1) :-
    Dir = mydir,
    File = myfile,
    test_steps(safe,
        [ make_directory(/Dir),
          make_file(/Dir/File),
          remove(/Dir/File)
        ]).
test(2) :-
    test_steps(safe,
        [ make_directory(/Dir),
          make_file(/Dir/'~')
        ]).
:- if(can_represent(0'α)).              % Greek
test(3) :-
    test_steps(safe,
        [ make_directory(/(greek=Dir)),
          make_directory(/Dir2),
          make_directory(/Dir2/greek),
          make_file(/Dir/(long(200)=File)),
          remove(/Dir/File)
        ]).
test(5) :-
    test_steps(safe,
        [ make_directory(/(long(100)=Dir)),
          make_directory(/Dir/(long(100)=Dir2)),
          make_directory(/Dir3),
          make_directory(/Dir3/greek),
          make_file(/Dir/Dir2/(long(100)=File)),
          remove(/Dir/Dir2/File)
        ]).
:- endif.
:- if(can_represent(0x1f600)).
test(4) :-
    test_steps(safe,
        [ make_directory(/(emoji(10)=Dir)),
          make_directory(/Dir2),
          make_directory(/Dir2/hindi),
          make_file(/Dir/(long(100)=File)),
          remove(/Dir/File)
        ]).
:- endif.
test(6) :-
    test_steps(safe,
        [ make_directory(/(long(200)=Dir)),
          make_directory(/Dir/(long(200)=Dir2)),
          make_directory(/Dir3),
          make_directory(/Dir3/greek),
          make_file(/Dir/Dir2/(long(100)=File)),
          remove(/Dir/Dir2/File)
        ]).

:- end_tests(file_names).

%!  test_steps(+Policy, +Actions)
%
%   Execute a number of modifications  to   the  filesystem  in the test
%   directory and verify all  files  and   directories  can  be accessed
%   correctly between each modification step.

test_steps(Policy, Actions) :-
    setup_call_cleanup(
        init,
        with_name_policy(Policy,
                         maplist(test_step, Actions)),
        cleanup).

test_step(Action) :-
    settle_names(Action, Action1),
    debug(files, 'Action: ~p', [Action1]),
    step(Action1),
    test_fs.

%!  test_fs
%
%   Verify that we can access all   files  and directories from anywhere
%   and get results that are consistent with our shadow admin.

test_fs :-
    forall(dir(Dir), test_step(dir, '.', ls(Dir))),
    forall(dir(Dir), test_step(dir, '/', ls(Dir))),
    forall(dir(Dir), forall(dir(Dir2), test_step(dir, Dir2, ls(Dir)))),

    forall(file(File), test_step(file, '.', verify(File))),
    forall(file(File), test_step(file, '/', verify(File))),
    forall(file(File), forall(dir(Dir2),
                              test_step(file, Dir2, verify(File)))).

test_step(Type, From, Action) :-
    debug(test(Type), 'Test from ~q: ~p', [From, Action]),
    step(From, Action).

%!  step(+Action).
%!  step(+From, +Action).
%
%   Execute an action on the  file  system.   For  step/2,  From  is the
%   directory from which to execute Action. This   can be `/`, `.` or an
%   existing directory.

step(Action) :-
    step('.', Action).

% Create/modify operations
step(From, make_directory(Dir)) =>
    run_with_name(From, Dir, Name,
                  make_directory(Name)),
    assert(dir(Dir)).
step(From, make_file(File)) =>
    run_with_name(From, File, Name,
                  make_file(File, Name)),
    assert(file(File)).
step(From, remove(Node)) =>
    run_with_name(From, Node, Name,
                  (   dir(Node)
                  ->  delete_directory(Name),
                      retractall(dir(Node))
                  ;   delete_file(Name),
                      retractall(file(Node))
                  )).
step(_From, rename(Old, New)) =>        % TBD: Deal with From
    rename_file(Old, New),
    (   retract(file(Old))
    ->  assert(file(New))
    ;   assertion(fail)                 % TBD: Moved directory
    ).

% Read/verify operations
step(From, ls(Dir)) =>
    run_with_name(From, Dir, Name,
                  ls(Dir, Name)).
step(From, verify(File)) =>
    run_with_name(From, File, Name,
                  (   exists_file(Name),
                      file_properties(File, Name),
                      read_file(File, Name)
                  )).
step(From, exists(Node)) =>
    run_with_name(From, Node, Name,
                  (   dir(Node)
                  ->  exists_directory(Name)
                  ;   exists_file(Name)
                  )).
step(From, properties(File)) =>
    run_with_name(From, File, Name,
                  file_properties(File, Name)).
step(From, read(File)) =>
    run_with_name(From, File, Name,
                  read_file(File, Name)).

file_args(make_directory(Dir),  [Dir]).
file_args(make_file(File),      [File]).
file_args(remove(Node),         [Node]).
file_args(rename(Node1, Node2), [Node1, Node2]).
file_args(ls(Dir),              [Dir]).
file_args(exists(Node),         [Node]).
file_args(read(File),           [File]).
file_args(properties(File),     [File]).
file_args(verify(File),	        [File]).

:- det(make_name/2).
make_name(Spec, Name) :-
    phrase(make_segments(Spec), Segments),
    atomic_list_concat(Segments, /, Name).

make_segments(Var) -->
    { var(Var),
      !,
      make_name(Var)
    },
    [Var].
make_segments(A/B) -->
    !,
    make_segments(A),
    make_segments(B).
make_segments(/Name) -->
    !,
    { test_dir(Root) },
    [Root],
    make_segments(Name).
make_segments(Policy=Name) -->
    !,
    { make_name_in_policy(Policy, Name)
    },
    [Name].
make_segments(Name) -->
    { atom(Name)
    },
    [Name].

make_name(Var) :-
    (   name_policy(Policy)
    ->  true
    ;   Policy = safe
    ),
    make_name_in_policy(Policy, Var).

make_name_in_policy(empty, Name) =>
    Name = ''.
make_name_in_policy(safe, Name) =>
    gensym(f, Name).
make_name_in_policy(greek, Name) =>
    Name = χαίρετε.                     % hello
make_name_in_policy(hindi, Name) =>
    Name = नमस्कार.                     % hello
make_name_in_policy(emoji(N), Name) =>
    Start = 0x1f600,
    End is Start+N-1,
    numlist(Start, End, Codes),
    atom_codes(Name, Codes).
make_name_in_policy(long(max), Name) =>
    segment_limit(file, Limit),
    long_segment(Limit, Name).
make_name_in_policy(long(Length), Name) =>
    long_segment(Length, Name).
make_name_in_policy(Literal, Name), string(Literal) =>
    atom_string(Name, Literal).
make_name_in_policy(A+B, Name) =>
    make_name_in_policy(A, AN),
    make_name_in_policy(B, BN),
    atom_concat(AN, BN, Name).

with_name_policy(Policy, Goal) :-
    setup_call_cleanup(
        asserta(name_policy(Policy), Ref),
        Goal,
        erase(Ref)).

settle_names(Action0, Action), file_args(Action0, Args) =>
    mapsubterms_var(settle_name(Args), Action0, Action).

settle_name(Replace, Spec, Name) :-
    member(R, Replace),
    R == Spec,
    !,
    make_name(Spec, Name).

%!  run_with_name(+From, +LocalName, -UseName, :Goal)

run_with_name(/, File, Name, Goal) =>
    use_name(/, File, Name),
    call(Goal).
run_with_name(., File, Name, Goal) =>
    use_name(., File, Name),
    call(Goal).
run_with_name(Dir, File, Name, Goal), safe_dir(Dir) =>
    use_name(Dir, File, Name),
    setup_call_cleanup(
        working_directory(Here, Dir),
        call(Goal),
        working_directory(_, Here)).
run_with_name(_, _, _, _) =>
    true.

%!  safe_dir(+Dir) is semidet.
%
%   True  when Dir  is  a safe  directory which  to  make the  current
%   directory.

:- if(current_prolog_flag(windows, true)).
safe_dir(Dir) :-
    absolute_file_name(Dir, Path),
    atom_length(Path, Len),
    Len < 200.
:- else.
safe_dir(_).
:- endif.



%!  use_name(+From, +LocalName, -UseName) is det.

use_name(/, File, Name) =>
    working_directory(Here, Here),
    directory_file_path(Here, File, Name).
use_name(., File, Name) =>
    Name = File.
use_name(Dir, File, Name) =>
    ensure_slash(Dir, DirS),
    relative_file_name(File, DirS, Name).

ensure_slash(Dir, DirS), sub_atom(Dir, _, _, 0, /) => DirS = Dir.
ensure_slash(Dir, DirS) => atom_concat(Dir, /, DirS).

%!  make_file(+File, +Name).
%!  read_file(+File, +Name).
%!  file_properties(+File, +Name).
%!  ls(+Dir, +Name).
%
%   Basic file operations. The first argument   is the logical file name
%   (`test-dir/...`) while the second is the name  we must use to access
%   the object, which may be the same,   an  absolute path or a relative
%   path from another node in the test tree.

make_file(File, Name) :-
    get_time(Now),
    setup_call_cleanup(
        open(Name, write, Out, [encoding(utf8)]),
        format(Out, '~q.~n', [#{name:File, created:Now}]),
        close(Out)).

read_file(File, Name) :-
    read_file_to_term(Name, Term),
    assertion(Term.name == File).

file_properties(_File, Name) :-
    read_file_to_term(Name, Term),
    time_file(Name, Time),
    assertion(abs(Term.created - Time) < 1),
    read_file_to_bytes(Name, Bytes),
    length(Bytes, Size0),
    size_file(Name, Size1),
    assertion(Size0 == Size1).

ls(Dir, Name) :-
    directory_files(Name, Files),
    sort(Files, Sorted),
    shadow_entries(Dir, Expected),
    (   Sorted == Expected
    ->  true
    ;   throw(ls(Sorted, Expected))
    ).

read_file_to_term(Name, Term) :-
    setup_call_cleanup(
        open(Name, read, In, [encoding(utf8)]),
        read(In, Term),
        close(In)).

read_file_to_bytes(Name, Bytes) :-
    setup_call_cleanup(        open(Name, read, In, [type(binary)]),
        (   get_byte(In, B0),
            read_bytes(B0, In, Bytes)
        ),
        close(In)).

read_bytes(-1, _, Bytes) => Bytes = [].
read_bytes(B, In, Bytes) => Bytes = [B|T], get_byte(In, C), read_bytes(C, In, T).


%!  init is det.
%!  cleanup is det.
%
%   Initialise the test environment

init :-
    (   retract(init_nesting(Level))
    ->  true
    ;   Level = 0
    ),
    (   Level == 0
    ->  test_dir(TestDir),
        (   exists_directory(TestDir)
        ->  delete_directory_contents(TestDir)
        ;   make_directory(TestDir)
        ),
        assert(dir(TestDir))
    ;   true
    ),
    Level1 is Level+1,
    assert(init_nesting(Level1)).

cleanup :-
    retract(init_nesting(Level)),
    Level0 is Level - 1,
    assert(init_nesting(Level0)),
    (   Level0 == 0
    ->  test_dir(TestDir),
        (   exists_directory(TestDir)
        ->  delete_directory_and_contents(TestDir)
        ;   true
        ),
        retractall(dir(_)),
        retractall(file(_)),
        retractall(name_policy(_))
    ;   true
    ).

test_dir(TestDir) :-
    current_prolog_flag(pid, PID),
    format(atom(TestDir), 'test-dir-~d', [PID]).

%!  shadow_entries(+Dir, -Entries) is det.
%
%   True when our shadow admin says Entries  appear in Dir. Entries is a
%   sorted list of plain file  or   directory  names,  including `.` and
%   `..`.

shadow_entries(In, Entries) :-
    findall(File, file_in_dir(In, File), Files),
    findall(Dir,  dir_in_dir(In, Dir), Dirs),
    append(Files, Dirs, Entries0),
    sort(['.', '..'|Entries0], Entries).

file_in_dir(In, Entry) :-
    file(File),
    file_directory_name(File, In),
    file_base_name(File, Entry).
dir_in_dir(In, Entry) :-
    dir(Dir),
    file_directory_name(Dir, In),
    file_base_name(Dir, Entry).


		 /*******************************
		 *        GENERATE NAMES	*
		 *******************************/

% Some stuff we may want for future versions

win_reserved(0'<).
win_reserved(0'>).
win_reserved(0':).
win_reserved(0'").
win_reserved(0'/).
win_reserved(0'\\).
win_reserved(0'|).
win_reserved(0'?).
win_reserved(0'*).

surrogate_lead(C)  :- between(0xd800, 0xdbff, C).
surrogate_trail(C) :- between(0xdc00, 0xdfff, C).
surrogate(C)       :- between(0xd800, 0xdfff, C).

%!  segment_limit(+Type, -Limit)
%
%   Find the maximum length of a plain file name. Type is one of `file`
%   or `directory`.

:- table segment_limit/2.

segment_limit(Type, Limit) :-
    setup_call_cleanup(
        init,
        (   current_prolog_flag(path_max, High),
            Max is High*2,
            bin_search(test_long_segment(Type), 2, Max, Limit)
        ),
        cleanup).

test_long_segment(Type, Length) :-
    long_segment(Length, Name),
    test_segment(Type, Name).

test_segment(Type, File) :-
    test_dir(TestDir),
    directory_file_path(TestDir, File, Full),
    (   Type == file
    ->  step(make_file(Full))
    ;   step(make_directory(Full))
    ),
    step(exists(Full)),
    step(remove(Full)).

%!  long_segment(+Length, -Segment) is det.
%
%   Create a long simple ASCII file name.

long_segment(Length, Segment) :-
    length(Codes, Length),
    foldl(next_long, Codes, 0, _),
    atom_codes(Segment, Codes).

next_long(Code, I0, I) :-
    I is I0+1,
    (   I0 mod 10 =:= 0
    ->  Code is 0'a + (I0//10) mod 26
    ;   Code is 0'0 + I0 mod 10
    ).

%!  bin_search(:Goal, +Low, +High, -Value) is det.
%
%   Find the highest value between  Low   and  High for which call(Goal,
%   Value) is true.

bin_search(Goal, Low, High, Value) :-
    Mid0 is (Low+High)//2,
    (   Mid0 == Low
    ->  Mid is Mid0+1
    ;   Mid is Mid0
    ),
    (   catch(call(Goal, Mid), _, fail)
    ->  (   Mid == High
        ->  Value = Mid
        ;   bin_search(Goal, Mid, High, Value)
        )
    ;   (   Mid == High
        ->  Value = Low
        ;   bin_search(Goal, Low, Mid, Value)
        )
    ).
