/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c) 2010-2013, University of Amsterdam,
                             VU University
    Amsterdam All rights reserved.

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

:- module(git,
          [ git/2,                      % +Argv, +Options
            git_process_output/3,       % +Argv, :OnOutput, +Options
            git_open_file/4,            % +Dir, +File, +Branch, -Stream
            is_git_directory/1,         % +Dir
            git_describe/2,             % -Version, +Options
            git_hash/2,                 % -Hash, +Options
            git_ls_tree/2,              % -Content, +Options
            git_remote_url/3,           % +Remote, -URL, +Options
            git_ls_remote/3,            % +GitURL, -Refs, +Options
            git_branches/2,             % -Branches, +Options
            git_remote_branches/2,      % +GitURL, -Branches
            git_default_branch/2,       % -DefaultBranch, +Options
            git_tags_on_branch/3,       % +Dir, +Branch, -Tags
            git_shortlog/3,             % +Dir, -Shortlog, +Options
            git_log_data/3,             % +Field, +Record, -Value
            git_show/4,                 % +Dir, +Hash, -Commit, +Options
            git_commit_data/3           % +Field, +Record, -Value
          ]).
:- use_module(library(process)).
:- use_module(library(readutil)).
:- use_module(library(option)).
:- use_module(library(dcg/basics)).
:- use_module(library(record)).
:- use_module(library(lists)).
:- use_module(library(error)).

:- meta_predicate
    git_process_output(+, 1, +).

/** <module> Run GIT commands

This module performs common GIT tasks by calling git as a remote process
through process_create/3. It requires that the =git= executable is in the
current PATH.

This module started life in ClioPatria and   has been used by the Prolog
web-server to provide information on git   repositories. It is now moved
into the core Prolog library to support the Prolog package manager.
*/

:- predicate_options(git/2, 2,
                     [ directory(atom),
                       error(-codes),
                       output(-codes),
                       status(-any),
                       askpass(any)
                     ]).
:- predicate_options(git_default_branch/2, 2,
                     [ pass_to(git_process_output/3, 3)
                     ] ).
:- predicate_options(git_describe/2, 2,
                     [ commit(atom),
                       directory(atom),
                       match(atom)
                     ]).
:- predicate_options(git_hash/2, 2,
                     [ commit(atom),
                       directory(atom)
                     ]).
:- predicate_options(git_ls_tree/2, 2,
                     [ commit(atom),
                       directory(atom)
                     ]).
:- predicate_options(git_process_output/3, 3,
                     [ directory(atom),
                       askpass(any),
                       error(-codes)
                     ]).
:- predicate_options(git_remote_url/3, 3,
                     [ pass_to(git_process_output/3, 3)
                     ]).
:- predicate_options(git_shortlog/3, 3,
                     [ revisions(atom),
                       limit(nonneg),
                       path(atom)
                     ]).
:- predicate_options(git_show/4, 4,
                     [ diff(oneof([patch,stat]))
                     ]).


%!  git(+Argv, +Options) is det.
%
%   Run a GIT command.  Defined options:
%
%     * directory(+Dir)
%     Execute in the given directory
%     * output(-Out)
%     Unify Out with a list of codes representing stdout of the
%     command.  Otherwise the output is handed to print_message/2
%     with level =informational=.
%     * error(-Error)
%     As output(Out), but messages are printed at level =error=.
%     * askpass(+Program)
%     Export GIT_ASKPASS=Program

git(Argv, Options) :-
    git_cwd_options(Argv, Argv1, Options),
    env_options(Extra, Options),
    setup_call_cleanup(
        process_create(path(git), Argv1,
                       [ stdout(pipe(Out)),
                         stderr(pipe(Error)),
                         process(PID)
                       | Extra
                       ]),
        call_cleanup(
            ( read_stream_to_codes(Out, OutCodes, []),
              read_stream_to_codes(Error, ErrorCodes, [])
            ),
            process_wait(PID, Status)),
        close_streams([Out,Error])),
    print_error(ErrorCodes, Options),
    print_output(OutCodes, Options),
    (   option(status(Status0), Options)
    ->  Status = Status0
    ;   Status == exit(0)
    ->  true
    ;   throw(error(process_error(git(Argv), Status), _))
    ).

git_cwd_options(Argv0, Argv, Options) :-
    option(directory(Dir), Options),
    !,
    Argv = ['-C', file(Dir) | Argv0 ].
git_cwd_options(Argv, Argv, _).

env_options([env(['GIT_ASKPASS'=Program])], Options) :-
    option(askpass(Exe), Options),
    !,
    exe_options(ExeOptions),
    absolute_file_name(Exe, PlProg, ExeOptions),
    prolog_to_os_filename(PlProg, Program).
env_options([], _).

exe_options(Options) :-
    current_prolog_flag(windows, true),
    !,
    Options = [ extensions(['',exe,com]), access(read) ].
exe_options(Options) :-
    Options = [ access(execute) ].

print_output(OutCodes, Options) :-
    option(output(Codes), Options),
    !,
    Codes = OutCodes.
print_output([], _) :- !.
print_output(OutCodes, _) :-
    print_message(informational, git(output(OutCodes))).

print_error(OutCodes, Options) :-
    option(error(Codes), Options),
    !,
    Codes = OutCodes.
print_error([], _) :- !.
print_error(OutCodes, _) :-
    phrase(classify_message(Level), OutCodes, _),
    print_message(Level, git(output(OutCodes))).

classify_message(error) -->
    string(_), "fatal:",
    !.
classify_message(error) -->
    string(_), "error:",
    !.
classify_message(warning) -->
    string(_), "warning:",
    !.
classify_message(informational) -->
    [].

%!  close_streams(+Streams:list) is det.
%
%   Close a list of streams, throwing the first error if some stream
%   failed to close.

close_streams(List) :-
    phrase(close_streams(List), Errors),
    (   Errors = [Error|_]
    ->  throw(Error)
    ;   true
    ).

close_streams([H|T]) -->
    { catch(close(H), E, true) },
    (   { var(E) }
    ->  []
    ;   [E]
    ),
    close_streams(T).


%!  git_process_output(+Argv, :OnOutput, +Options) is det.
%
%   Run a git-command and process the output with OnOutput, which is
%   called as call(OnOutput, Stream).

git_process_output(Argv, OnOutput, Options) :-
    git_cwd_options(Argv, Argv1, Options),
    env_options(Extra, Options),
    setup_call_cleanup(
        process_create(path(git), Argv1,
                       [ stdout(pipe(Out)),
                         stderr(pipe(Error)),
                         process(PID)
                       | Extra
                       ]),
        call_cleanup(
            ( call(OnOutput, Out),
              read_stream_to_codes(Error, ErrorCodes, [])
            ),
            git_wait(PID, Out, Status)),
        close_streams([Out,Error])),
    print_error(ErrorCodes, Options),
    (   Status = exit(0)
    ->  true
    ;   throw(error(process_error(git, Status)))
    ).

git_wait(PID, Out, Status) :-
    at_end_of_stream(Out),
    !,
    process_wait(PID, Status).
git_wait(PID, Out, Status) :-
    setup_call_cleanup(
        open_null_stream(Null),
        copy_stream_data(Out, Null),
        close(Null)),
    process_wait(PID, Status).


%!  git_open_file(+GitRepoDir, +File, +Branch, -Stream) is det.
%
%   Open the file File in the given bare GIT repository on the given
%   branch (treeisch).
%
%   @bug    We cannot tell whether opening failed for some reason.

git_open_file(Dir, File, Branch, In) :-
    atomic_list_concat([Branch, :, File], Ref),
    process_create(path(git),
                   [ '-C', file(Dir), show, Ref ],
                   [ stdout(pipe(In))
                   ]),
    set_stream(In, file_name(File)).


%!  is_git_directory(+Directory) is semidet.
%
%   True if Directory is a  git   directory  (Either  checked out or
%   bare).

is_git_directory(Directory) :-
    directory_file_path(Directory, '.git', GitDir),
    exists_directory(GitDir),
    !.
is_git_directory(Directory) :-
    exists_directory(Directory),
    git(['rev-parse', '--git-dir'],
        [ output(Codes),
          error(_),
          status(Status),
          directory(Directory)
        ]),
    Status == exit(0),
    string_codes(".\n", Codes).

%!  git_describe(-Version, +Options) is semidet.
%
%   Describe the running version  based  on   GIT  tags  and hashes.
%   Options:
%
%       * match(+Pattern)
%       Only use tags that match Pattern (a Unix glob-pattern; e.g.
%       =|V*|=)
%       * directory(Dir)
%       Provide the version-info for a directory that is part of
%       a GIT-repository.
%       * commit(+Commit)
%       Describe Commit rather than =HEAD=
%
%   @see git describe

git_describe(Version, Options) :-
    (   option(match(Pattern), Options)
    ->  true
    ;   git_version_pattern(Pattern)
    ),
    (   option(commit(Commit), Options)
    ->  Extra = [Commit]
    ;   Extra = []
    ),
    option(directory(Dir), Options, .),
    setup_call_cleanup(
        process_create(path(git),
                       [ 'describe',
                         '--match', Pattern
                       | Extra
                       ],
                       [ stdout(pipe(Out)),
                         stderr(null),
                         process(PID),
                         cwd(Dir)
                       ]),
        call_cleanup(
            read_stream_to_codes(Out, V0, []),
            git_wait(PID, Out, Status)),
        close(Out)),
    Status = exit(0),
    !,
    atom_codes(V1, V0),
    normalize_space(atom(Plain), V1),
    (   git_is_clean(Dir)
    ->  Version = Plain
    ;   atom_concat(Plain, '-DIRTY', Version)
    ).
git_describe(Version, Options) :-
    option(directory(Dir), Options, .),
    option(commit(Commit), Options, 'HEAD'),
    setup_call_cleanup(
        process_create(path(git),
                       [ 'rev-parse', '--short',
                         Commit
                       ],
                       [ stdout(pipe(Out)),
                         stderr(null),
                         process(PID),
                         cwd(Dir)
                       ]),
        call_cleanup(
            read_stream_to_codes(Out, V0, []),
            git_wait(PID, Out, Status)),
        close(Out)),
    Status = exit(0),
    atom_codes(V1, V0),
    normalize_space(atom(Plain), V1),
    (   git_is_clean(Dir)
    ->  Version = Plain
    ;   atom_concat(Plain, '-DIRTY', Version)
    ).


:- multifile
    git_version_pattern/1.

git_version_pattern('V*').
git_version_pattern('*').


%!  git_is_clean(+Dir) is semidet.
%
%   True if the given directory is in   a git module and this module
%   is clean. To us, clean only   implies that =|git diff|= produces
%   no output.

git_is_clean(Dir) :-
    setup_call_cleanup(process_create(path(git), ['diff', '--stat'],
                                      [ stdout(pipe(Out)),
                                        stderr(null),
                                        cwd(Dir)
                                      ]),
                       stream_char_count(Out, Count),
                       close(Out)),
    Count == 0.

stream_char_count(Out, Count) :-
    setup_call_cleanup(open_null_stream(Null),
                       (   copy_stream_data(Out, Null),
                           character_count(Null, Count)
                       ),
                       close(Null)).


%!  git_hash(-Hash, +Options) is det.
%
%   Return the hash of the indicated object.

git_hash(Hash, Options) :-
    option(commit(Commit), Options, 'HEAD'),
    git_process_output(['rev-parse', '--verify', Commit],
                       read_hash(Hash),
                       Options).

read_hash(Hash, Stream) :-
    read_line_to_codes(Stream, Line),
    atom_codes(Hash, Line).


%!  git_ls_tree(-Entries, +Options) is det.
%
%   True  when  Entries  is  a  list  of  entries  in  the  the  GIT
%   repository, Each entry is a term:
%
%     ==
%     object(Mode, Type, Hash, Size, Name)
%     ==

git_ls_tree(Entries, Options) :-
    option(commit(Commit), Options, 'HEAD'),
    git_process_output(['ls-tree', '-z', '-r', '-l', Commit],
                       read_tree(Entries),
                       Options).

read_tree(Entries, Stream) :-
    read_stream_to_codes(Stream, Codes),
    phrase(ls_tree(Entries), Codes).

ls_tree([H|T]) -->
    ls_entry(H),
    !,
    ls_tree(T).
ls_tree([]) --> [].

ls_entry(object(Mode, Type, Hash, Size, Name)) -->
    string(MS), " ",
    string(TS), " ",
    string(HS), " ",
    string(SS), "\t",
    string(NS), [0],
    !,
    { number_codes(Mode, [0'0,0'o|MS]),
      atom_codes(Type, TS),
      atom_codes(Hash, HS),
      (   Type == blob
      ->  number_codes(Size, SS)
      ;   Size = 0          % actually '-', but 0 sums easier
      ),
      atom_codes(Name, NS)
    }.


%!  git_remote_url(+Remote, -URL, +Options) is det.
%
%   URL is the remote (fetch) URL for the given Remote.

git_remote_url(Remote, URL, Options) :-
    git_process_output([remote, show, Remote],
                       read_url("Fetch URL:", URL),
                       Options).

read_url(Tag, URL, In) :-
    repeat,
        read_line_to_codes(In, Line),
        (   Line == end_of_file
        ->  !, fail
        ;   phrase(url_codes(Tag, Codes), Line)
        ->  !, atom_codes(URL, Codes)
        ).

url_codes(Tag, Rest) -->
    { string_codes(Tag, TagCodes) },
    whites, string(TagCodes), whites, string(Rest).


%!  git_ls_remote(+GitURL, -Refs, +Options) is det.
%
%   Execute =|git ls-remote|= against the remote repository to fetch
%   references from the remote.  Options processed:
%
%     * heads(Boolean)
%     * tags(Boolean)
%     * refs(List)
%
%   For example, to find the hash of the remote =HEAD=, one can use
%
%     ==
%     ?- git_ls_remote('git://www.swi-prolog.org/home/pl/git/pl-devel.git',
%                      Refs, [refs(['HEAD'])]).
%     Refs = ['5d596c52aa969d88e7959f86327f5c7ff23695f3'-'HEAD'].
%     ==
%
%   @param Refs is a list of pairs hash-name.

git_ls_remote(GitURL, Refs, Options) :-
    findall(O, ls_remote_option(Options, O), RemoteOptions),
    option(refs(LimitRefs), Options, []),
    must_be(list(atom), LimitRefs),
    append([ 'ls-remote' | RemoteOptions], [GitURL|LimitRefs], Argv),
    git_process_output(Argv, remote_refs(Refs), []).

ls_remote_option(Options, '--heads') :-
    option(heads(true), Options).
ls_remote_option(Options, '--tags') :-
    option(tags(true), Options).

remote_refs(Refs, Out) :-
    read_line_to_codes(Out, Line0),
    remote_refs(Line0, Out, Refs).

remote_refs(end_of_file, _, []) :- !.
remote_refs(Line, Out, [Hash-Ref|Tail]) :-
    phrase(remote_ref(Hash,Ref), Line),
    read_line_to_codes(Out, Line1),
    remote_refs(Line1, Out, Tail).

remote_ref(Hash, Ref) -->
    string_without("\t ", HashCodes),
    whites,
    string_without("\t ", RefCodes),
    { atom_codes(Hash, HashCodes),
      atom_codes(Ref, RefCodes)
    }.


%!  git_remote_branches(+GitURL, -Branches) is det.
%
%   Exploit git_ls_remote/3 to fetch  the   branches  from  a remote
%   repository without downloading it.

git_remote_branches(GitURL, Branches) :-
    git_ls_remote(GitURL, Refs, [heads(true)]),
    findall(B, (member(_-Head, Refs),
                atom_concat('refs/heads/', B, Head)),
            Branches).


%!  git_default_branch(-BranchName, +Options) is det.
%
%   True when BranchName is the default branch of a repository.

git_default_branch(BranchName, Options) :-
    git_process_output([branch],
                       read_default_branch(BranchName),
                       Options).

read_default_branch(BranchName, In) :-
    repeat,
        read_line_to_codes(In, Line),
        (   Line == end_of_file
        ->  !, fail
        ;   phrase(default_branch(Codes), Line)
        ->  !, atom_codes(BranchName, Codes)
        ).

default_branch(Rest) -->
    "*", whites, string(Rest).

%!  git_branches(-Branches, +Options) is det.
%
%   True when Branches is the list of branches in the repository.
%   In addition to the usual options, this processes:
%
%     - contains(Commit)
%     Return only branches that contain Commit.

git_branches(Branches, Options) :-
    (   select_option(commit(Commit), Options, GitOptions)
    ->  Extra = ['--contains', Commit]
    ;   Extra = [],
        GitOptions = Options
    ),
    git_process_output([branch|Extra],
                       read_branches(Branches),
                       GitOptions).

read_branches(Branches, In) :-
    read_line_to_codes(In, Line),
    (   Line == end_of_file
    ->  Branches = []
    ;   Line = [_,_|Codes],
        atom_codes(H, Codes),
        Branches = [H|T],
        read_branches(T, In)
    ).


%!  git_tags_on_branch(+Dir, +Branch, -Tags) is det.
%
%   Tags is a list of tags in Branch on the GIT repository Dir, most
%   recent tag first.
%
%   @see Git tricks at http://mislav.uniqpath.com/2010/07/git-tips/

git_tags_on_branch(Dir, Branch, Tags) :-
    git_process_output([ log, '--oneline', '--decorate', Branch ],
                       log_to_tags(Tags),
                       [ directory(Dir) ]).

log_to_tags(Tags, Out) :-
    read_line_to_codes(Out, Line0),
    log_to_tags(Line0, Out, Tags, []).

log_to_tags(end_of_file, _, Tags, Tags) :- !.
log_to_tags(Line, Out, Tags, Tail) :-
    phrase(tags_on_line(Tags, Tail1), Line),
    read_line_to_codes(Out, Line1),
    log_to_tags(Line1, Out, Tail1, Tail).

tags_on_line(Tags, Tail) -->
    string_without(" ", _Hash),
    tags(Tags, Tail),
    skip_rest.

tags(Tags, Tail) -->
    whites,
    "(",
    tag_list(Tags, Rest),
    !,
    tags(Rest, Tail).
tags(Tags, Tags) -->
    skip_rest.

tag_list([H|T], Rest) -->
    "tag:", !, whites,
    string(Codes),
    (   ")"
    ->  { atom_codes(H, Codes),
          T = Rest
        }
    ;   ","
    ->  { atom_codes(H, Codes)
        },
        whites,
        tag_list(T, Rest)
    ).
tag_list(List, Rest) -->
    string(_),
    (   ")"
    ->  { List = Rest }
    ;   ","
    ->  whites,
        tag_list(List, Rest)
    ).

skip_rest(_,_).


                 /*******************************
                 *        READ GIT HISTORY      *
                 *******************************/

%!  git_shortlog(+Dir, -ShortLog, +Options) is det.
%
%   Fetch information like the  GitWeb   change  overview. Processed
%   options:
%
%       * limit(+Count)
%       Maximum number of commits to show (default is 10)
%       * revisions(+Revisions)
%       Git revision specification
%       * path(+Path)
%       Only show commits that affect Path.  Path is the path of
%       a checked out file.
%       * git_path(+Path)
%       Similar to =path=, but Path is relative to the repository.
%
%   @param ShortLog is a list of =git_log= records.

:- record
    git_log(commit_hash:atom,
            author_name:atom,
            author_date_relative:atom,
            committer_name:atom,
            committer_date_relative:atom,
            committer_date_unix:integer,
            subject:atom,
            ref_names:list).

git_shortlog(Dir, ShortLog, Options) :-
    (   option(revisions(Range), Options)
    ->  RangeSpec = [Range]
    ;   option(limit(Limit), Options, 10),
        RangeSpec = ['-n', Limit]
    ),
    (   option(git_path(Path), Options)
    ->  Extra = ['--', Path]
    ;   option(path(Path), Options)
    ->  relative_file_name(Path, Dir, RelPath),
        Extra = ['--', RelPath]
    ;   Extra = []
    ),
    git_format_string(git_log, Fields, Format),
    append([[log, Format], RangeSpec, Extra], GitArgv),
    git_process_output(GitArgv,
                       read_git_formatted(git_log, Fields, ShortLog),
                       [directory(Dir)]).


read_git_formatted(Record, Fields, ShortLog, In) :-
    read_line_to_codes(In, Line0),
    read_git_formatted(Line0, In, Record, Fields, ShortLog).

read_git_formatted(end_of_file, _, _, _, []) :- !.
read_git_formatted(Line, In, Record, Fields, [H|T]) :-
    record_from_line(Record, Fields, Line, H),
    read_line_to_codes(In, Line1),
    read_git_formatted(Line1, In, Record, Fields, T).

record_from_line(RecordName, Fields, Line, Record) :-
    phrase(fields_from_line(Fields, Values), Line),
    Record =.. [RecordName|Values].

fields_from_line([], []) --> [].
fields_from_line([F|FT], [V|VT]) -->
    to_nul_s(Codes),
    { field_to_prolog(F, Codes, V) },
    fields_from_line(FT, VT).

to_nul_s([]) --> [0], !.
to_nul_s([H|T]) --> [H], to_nul_s(T).

field_to_prolog(ref_names, Line, List) :-
    phrase(ref_names(List), Line),
    !.
field_to_prolog(committer_date_unix, Line, Stamp) :-
    !,
    number_codes(Stamp, Line).
field_to_prolog(_, Line, Atom) :-
    atom_codes(Atom, Line).

ref_names([]) --> [].
ref_names(List) -->
    blanks, "(", ref_name_list(List), ")".

ref_name_list([H|T]) -->
    string_without(",)", Codes),
    { atom_codes(H, Codes) },
    (   ",", blanks
    ->  ref_name_list(T)
    ;   {T=[]}
    ).


%!  git_show(+Dir, +Hash, -Commit, +Options) is det.
%
%   Fetch info from a GIT commit.  Options processed:
%
%     * diff(Diff)
%     GIT option on how to format diffs.  E.g. =stat=
%     * max_lines(Count)
%     Truncate the body at Count lines.
%
%   @param  Commit is a term git_commit(...)-Body.  Body is currently
%           a list of lines, each line represented as a list of
%           codes.

:- record
    git_commit(tree_hash:atom,
               parent_hashes:list,
               author_name:atom,
               author_date:atom,
               committer_name:atom,
               committer_date:atom,
               subject:atom).

git_show(Dir, Hash, Commit, Options) :-
    git_format_string(git_commit, Fields, Format),
    option(diff(Diff), Options, patch),
    diff_arg(Diff, DiffArg),
    git_process_output([ show, DiffArg, Hash, Format ],
                       read_commit(Fields, Commit, Options),
                       [directory(Dir)]).

diff_arg(patch, '-p').
diff_arg(stat, '--stat').

read_commit(Fields, Data-Body, Options, In) :-
    read_line_to_codes(In, Line1),
    record_from_line(git_commit, Fields, Line1, Data),
    read_line_to_codes(In, Line2),
    (   Line2 == []
    ->  option(max_lines(Max), Options, -1),
        read_n_lines(In, Max, Body)
    ;   Line2 == end_of_file
    ->  Body = []
    ).

read_n_lines(In, Max, Lines) :-
    read_line_to_codes(In, Line1),
    read_n_lines(Line1, Max, In, Lines).

read_n_lines(end_of_file, _, _, []) :- !.
read_n_lines(_, 0, In, []) :-
    !,
    setup_call_cleanup(open_null_stream(Out),
                       copy_stream_data(In, Out),
                       close(Out)).
read_n_lines(Line, Max0, In, [Line|More]) :-
    read_line_to_codes(In, Line2),
    Max is Max0-1,
    read_n_lines(Line2, Max, In, More).


%!  git_format_string(:Record, -FieldNames, -Format)
%
%   If Record is a record with  fields   whose  names  match the GIT
%   format field-names, Format is a  git =|--format=|= argument with
%   the appropriate format-specifiers,  terminated   by  %x00, which
%   causes the actual field to be 0-terminated.

:- meta_predicate
    git_format_string(:, -, -).

git_format_string(M:RecordName, Fields, Format) :-
    current_record(RecordName, M:Term),
    findall(F, record_field(Term, F), Fields),
    maplist(git_field_format, Fields, Formats),
    atomic_list_concat(['--format='|Formats], Format).

record_field(Term, Name) :-
    arg(_, Term, Field),
    field_name(Field, Name).

field_name(Name:_Type=_Default, Name) :- !.
field_name(Name:_Type, Name) :- !.
field_name(Name=_Default, Name) :- !.
field_name(Name, Name).

git_field_format(Field, Fmt) :-
    (   git_format(NoPercent, Field)
    ->  atomic_list_concat(['%', NoPercent, '%x00'], Fmt)
    ;   existence_error(git_format, Field)
    ).

git_format('H', commit_hash).
git_format('h', abbreviated_commit_hash).
git_format('T', tree_hash).
git_format('t', abbreviated_tree_hash).
git_format('P', parent_hashes).
git_format('p', abbreviated_parent_hashes).

git_format('an', author_name).
git_format('aN', author_name_mailcap).
git_format('ae', author_email).
git_format('aE', author_email_mailcap).
git_format('ad', author_date).
git_format('aD', author_date_rfc2822).
git_format('ar', author_date_relative).
git_format('at', author_date_unix).
git_format('ai', author_date_iso8601).

git_format('cn', committer_name).
git_format('cN', committer_name_mailcap).
git_format('ce', committer_email).
git_format('cE', committer_email_mailcap).
git_format('cd', committer_date).
git_format('cD', committer_date_rfc2822).
git_format('cr', committer_date_relative).
git_format('ct', committer_date_unix).
git_format('ci', committer_date_iso8601).

git_format('d', ref_names).             % git log?
git_format('e', encoding).              % git log?

git_format('s', subject).
git_format('f', subject_sanitized).
git_format('b', body).
git_format('N', notes).

git_format('gD', reflog_selector).
git_format('gd', shortened_reflog_selector).
git_format('gs', reflog_subject).


                 /*******************************
                 *            MESSAGES          *
                 *******************************/

:- multifile
    prolog:message//1.

prolog:message(git(output(Codes))) -->
    { split_lines(Codes, Lines) },
    git_lines(Lines).

git_lines([]) --> [].
git_lines([H|T]) -->
    [ '~s'-[H] ],
    (   {T==[]}
    ->  []
    ;   [nl], git_lines(T)
    ).

split_lines([], []) :- !.
split_lines(All, [Line1|More]) :-
    append(Line1, [0'\n|Rest], All),
    !,
    split_lines(Rest, More).
split_lines(Line, [Line]).
