/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1998-2025, University of Amsterdam
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

:- module(prolog_edit,
          [ edit/1,                     % +Spec
            edit/0
          ]).
:- autoload(library(lists), [member/2, append/3, select/3]).
:- autoload(library(make), [make/0]).
:- autoload(library(prolog_breakpoints), [breakpoint_property/2]).
:- autoload(library(apply), [foldl/5, maplist/3, maplist/2]).
:- use_module(library(dcg/high_order), [sequence/5]).
:- autoload(library(readutil), [read_line_to_string/2]).


% :- set_prolog_flag(generate_debug_info, false).

/** <module> Editor interface

This module implements the generic editor  interface. It consists of two
extensible parts with little  in  between.   The  first  part deals with
translating the input into source-location, and the second with starting
an editor.
*/

:- multifile
    locate/3,                       % +Partial, -FullSpec, -Location
    locate/2,                       % +FullSpec, -Location
    select_location/3,              % +Pairs, +Spec, -Location
    exists_location/1,              % +Location
    user_select/2,                  % +Max, -I
    edit_source/1,                  % +Location
    edit_command/2,                 % +Editor, -Command
    load/0.                         % provides load-hooks

%!  edit(+Spec)
%
%   Edit indicated object.

edit(Spec) :-
    notrace(edit_no_trace(Spec)).

edit_no_trace(Spec) :-
    var(Spec),
    !,
    throw(error(instantiation_error, _)).
edit_no_trace(Spec) :-
    load_extensions,
    findall(Location-FullSpec,
            locate(Spec, FullSpec, Location),
            Pairs0),
    sort(Pairs0, Pairs1),
    merge_locations(Pairs1, Pairs),
    do_select_location(Pairs, Spec, Location),
    do_edit_source(Location).

%!  edit
%
%   Edit associated or script file.  This is the Prolog file opened
%   by double-clicking or the file loaded using
%
%     ==
%     % swipl [-s] file.pl
%     ==

edit :-
    current_prolog_flag(associated_file, File),
    !,
    edit(file(File)).
edit :-
    '$cmd_option_val'(script_file, OsFiles),
    OsFiles = [OsFile],
    !,
    prolog_to_os_filename(File, OsFile),
    edit(file(File)).
edit :-
    throw(error(context_error(edit, no_default_file), _)).


                 /*******************************
                 *            LOCATE            *
                 *******************************/

%!  locate(+Spec, -FullSpec, -Location:dict)

locate(FileSpec:Line, file(Path, line(Line)), #{file:Path, line:Line}) :-
    integer(Line), Line >= 1,
    ground(FileSpec),                      % so specific; do not try alts
    !,
    locate(FileSpec, _, #{file:Path}).
locate(FileSpec:Line:LinePos,
       file(Path, line(Line), linepos(LinePos)),
       #{file:Path, line:Line, linepos:LinePos}) :-
    integer(Line), Line >= 1,
    integer(LinePos), LinePos >= 1,
    ground(FileSpec),                      % so specific; do not try alts
    !,
    locate(FileSpec, _, #{file:Path}).
locate(Path, file(Path), #{file:Path}) :-
    atom(Path),
    exists_file(Path).
locate(Pattern, file(Path), #{file:Path}) :-
    atom(Pattern),
    catch(expand_file_name(Pattern, Files), error(_,_), fail),
    member(Path, Files),
    exists_file(Path).
locate(FileBase, file(File), #{file:File}) :-
    atom(FileBase),
    find_source(FileBase, File).
locate(FileSpec, file(File), #{file:File}) :-
    is_file_search_spec(FileSpec),
    find_source(FileSpec, File).
locate(FileBase, source_file(Path),  #{file:Path}) :-
    atom(FileBase),
    source_file(Path),
    file_base_name(Path, File),
    (   File == FileBase
    ->  true
    ;   file_name_extension(FileBase, _, File)
    ).
locate(FileBase, include_file(Path),  #{file:Path}) :-
    atom(FileBase),
    setof(Path, include_file(Path), Paths),
    member(Path, Paths),
    file_base_name(Path, File),
    (   File == FileBase
    ->  true
    ;   file_name_extension(FileBase, _, File)
    ).
locate(Name, FullSpec, Location) :-
    atom(Name),
    locate(Name/_, FullSpec, Location).
locate(Name/Arity, Module:Name/Arity, Location) :-
    locate(Module:Name/Arity, Location).
locate(Name//DCGArity, FullSpec, Location) :-
    (   integer(DCGArity)
    ->  Arity is DCGArity+2,
        locate(Name/Arity, FullSpec, Location)
    ;   locate(Name/_, FullSpec, Location) % demand arity >= 2
    ).
locate(Name/Arity, library(File),  #{file:PlPath}) :-
    atom(Name),
    '$in_library'(Name, Arity, Path),
    (   absolute_file_name(library(.), Dir,
                           [ file_type(directory),
                             solutions(all)
                           ]),
        atom_concat(Dir, File0, Path),
        atom_concat(/, File, File0)
    ->  find_source(Path, PlPath)
    ;   fail
    ).
locate(Module:Name, Module:Name/Arity, Location) :-
    locate(Module:Name/Arity, Location).
locate(Module:Head, Module:Name/Arity, Location) :-
    callable(Head),
    \+ ( Head = (PName/_),
         atom(PName)
       ),
    functor(Head, Name, Arity),
    locate(Module:Name/Arity, Location).
locate(Spec, module(Spec), Location) :-
    locate(module(Spec), Location).
locate(Spec, Spec, Location) :-
    locate(Spec, Location).

include_file(Path) :-
    source_file_property(Path, included_in(_,_)).

%!  is_file_search_spec(@Spec) is semidet.
%
%   True if Spec is valid pattern for absolute_file_name/3.

is_file_search_spec(Spec) :-
    compound(Spec),
    compound_name_arguments(Spec, Alias, [Arg]),
    is_file_spec(Arg),
    user:file_search_path(Alias, _),
    !.

is_file_spec(Name), atom(Name) => true.
is_file_spec(Name), string(Name) => true.
is_file_spec(Term), cyclic_term(Term) => fail.
is_file_spec(A/B) => is_file_spec(A), is_file_spec(B).
is_file_spec(_) => fail.

%!  find_source(++FileSpec, =File) is semidet.
%
%   Find a source file from FileSpec.  If FileSpec resolves to a .qlf
%   file, File is the embedded `.pl` file (which may not exist).

find_source(FileSpec, File) :-
    catch(absolute_file_name(FileSpec, File0,
                             [ file_type(prolog),
                               access(read),
                               file_errors(fail)
                             ]),
          error(_,_), fail),
    prolog_source(File0, File).

prolog_source(File0, File) :-
    file_name_extension(_, Ext, File0),
    user:prolog_file_type(Ext, qlf),
    !,
    '$qlf_module'(File0, Info),
    File = Info.get(file).
prolog_source(File, File).


%!  locate(+Spec, -Location)
%
%   Locate object from the specified location.

locate(file(File, line(Line)), #{file:File, line:Line}).
locate(file(File), #{file:File}).
locate(Module:Name/Arity, #{file:File, line:Line}) :-
    (   atom(Name), integer(Arity)
    ->  functor(Head, Name, Arity)
    ;   Head = _                    % leave unbound
    ),
    (   (   var(Module)
        ;   var(Name)
        )
    ->  NonImport = true
    ;   NonImport = false
    ),
    current_predicate(Name, Module:Head),
    \+ (   NonImport == true,
           Module \== system,
           predicate_property(Module:Head, imported_from(_))
       ),
    functor(Head, Name, Arity),     % bind arity
    predicate_property(Module:Head, file(File)),
    predicate_property(Module:Head, line_count(Line)).
locate(module(Module), Location) :-
    atom(Module),
    module_property(Module, file(Path)),
    (   module_property(Module, line_count(Line))
    ->  Location = #{file:Path, line:Line}
    ;   Location = #{file:Path}
    ).
locate(breakpoint(Id), Location) :-
    integer(Id),
    breakpoint_property(Id, clause(Ref)),
    (   breakpoint_property(Id, file(File)),
        breakpoint_property(Id, line_count(Line))
    ->  Location =  #{file:File, line:Line}
    ;   locate(clause(Ref), Location)
    ).
locate(clause(Ref), #{file:File, line:Line}) :-
    clause_property(Ref, file(File)),
    clause_property(Ref, line_count(Line)).
locate(clause(Ref, _PC), #{file:File, line:Line}) :- % TBD: use clause
    clause_property(Ref, file(File)),
    clause_property(Ref, line_count(Line)).


                 /*******************************
                 *             EDIT             *
                 *******************************/

%!  do_edit_source(+Location)
%
%   Actually call the editor to edit Location, a list of Name(Value)
%   that contains file(File) and may contain line(Line). First the
%   multifile hook edit_source/1 is called. If this fails the system
%   checks for XPCE and the prolog-flag editor. If the latter is
%   built_in or pce_emacs, it will start PceEmacs.
%
%   Finally, it will get the editor to use from the prolog-flag
%   editor and use edit_command/2 to determine how this editor
%   should be called.

do_edit_source(Location) :-             % hook
    edit_source(Location),
    !.
do_edit_source(Location) :-             % PceEmacs
    current_prolog_flag(editor, Editor),
    is_pceemacs(Editor),
    current_prolog_flag(gui, true),
    !,
    location_url(Location, URL),        % File[:Line[:LinePos]]
    run_pce_emacs(URL).
do_edit_source(Location) :-             % External editor
    external_edit_command(Location, Command),
    print_message(informational, edit(waiting_for_editor)),
    (   catch(shell(Command), E,
              (print_message(warning, E),
               fail))
    ->  print_message(informational, edit(make)),
        make
    ;   print_message(informational, edit(canceled))
    ).

external_edit_command(Location, Command) :-
    #{file:File, line:Line} :< Location,
    editor(Editor),
    file_base_name(Editor, EditorFile),
    file_name_extension(Base, _, EditorFile),
    edit_command(Base, Cmd),
    prolog_to_os_filename(File, OsFile),
    atom_codes(Cmd, S0),
    substitute('%e', Editor, S0, S1),
    substitute('%f', OsFile, S1, S2),
    substitute('%d', Line,   S2, S),
    !,
    atom_codes(Command, S).
external_edit_command(Location, Command) :-
    #{file:File} :< Location,
    editor(Editor),
    file_base_name(Editor, EditorFile),
    file_name_extension(Base, _, EditorFile),
    edit_command(Base, Cmd),
    prolog_to_os_filename(File, OsFile),
    atom_codes(Cmd, S0),
    substitute('%e', Editor, S0, S1),
    substitute('%f', OsFile, S1, S),
    \+ substitute('%d', 1, S, _),
    !,
    atom_codes(Command, S).
external_edit_command(Location, Command) :-
    #{file:File} :< Location,
    editor(Editor),
    format(string(Command), '"~w" "~w"', [Editor, File]).

is_pceemacs(pce_emacs).
is_pceemacs(built_in).

%!  run_pce_emacs(+URL) is semidet.
%
%   Dynamically load and run emacs/1.

run_pce_emacs(URL) :-
    autoload_call(in_pce_thread(autoload_call(emacs(URL)))).

%!  editor(-Editor)
%
%   Determine the external editor to run.

editor(Editor) :-                       % $EDITOR
    current_prolog_flag(editor, Editor),
    (   sub_atom(Editor, 0, _, _, $)
    ->  sub_atom(Editor, 1, _, 0, Var),
        catch(getenv(Var, Editor), _, fail), !
    ;   Editor == default
    ->  catch(getenv('EDITOR', Editor), _, fail), !
    ;   \+ is_pceemacs(Editor)
    ->  !
    ).
editor(Editor) :-                       % User defaults
    getenv('EDITOR', Editor),
    !.
editor(vi) :-                           % Platform defaults
    current_prolog_flag(unix, true),
    !.
editor(notepad) :-
    current_prolog_flag(windows, true),
    !.
editor(_) :-                            % No luck
    throw(error(existence_error(editor), _)).

%!  edit_command(+Editor, -Command)
%
%   This predicate should specify the shell-command called to invoke
%   the user's editor. The following substitutions will be made:
%
%           | %e | Path name of the editor            |
%           | %f | Path name of the file to be edited |
%           | %d | Line number of the target          |


edit_command(vi,          '%e +%d \'%f\'').
edit_command(vi,          '%e \'%f\'').
edit_command(emacs,       '%e +%d \'%f\'').
edit_command(emacs,       '%e \'%f\'').
edit_command(notepad,     '"%e" "%f"').
edit_command(wordpad,     '"%e" "%f"').
edit_command(uedit32,     '%e "%f/%d/0"').      % ultraedit (www.ultraedit.com)
edit_command(jedit,       '%e -wait \'%f\' +line:%d').
edit_command(jedit,       '%e -wait \'%f\'').
edit_command(edit,        '%e %f:%d').          % PceEmacs client script
edit_command(edit,        '%e %f').

edit_command(emacsclient, Command) :- edit_command(emacs, Command).
edit_command(vim,         Command) :- edit_command(vi,    Command).
edit_command(nvim,        Command) :- edit_command(vi,    Command).

substitute(FromAtom, ToAtom, Old, New) :-
    atom_codes(FromAtom, From),
    (   atom(ToAtom)
    ->  atom_codes(ToAtom, To)
    ;   number_codes(ToAtom, To)
    ),
    append(Pre, S0, Old),
    append(From, Post, S0) ->
    append(Pre, To, S1),
    append(S1, Post, New),
    !.
substitute(_, _, Old, Old).


                 /*******************************
                 *            SELECT            *
                 *******************************/

merge_locations([L1|T1], Locations) :-
    L1 = Loc1-Spec1,
    select(L2, T1, T2),
    L2 = Loc2-Spec2,
    same_location(Loc1, Loc2, Loc),
    merge_specs(Spec1, Spec2, Spec),
    !,
    merge_locations([Loc-Spec|T2], Locations).
merge_locations(Locations, Locations).

same_location(L, L, L).
same_location(#{file:F1}, #{file:F2}, #{file:F}) :-
    best_same_file(F1, F2, F).
same_location(#{file:F1, line:Line}, #{file:F2}, #{file:F, line:Line}) :-
    best_same_file(F1, F2, F).
same_location(#{file:F1}, #{file:F2, line:Line}, #{file:F, line:Line}) :-
    best_same_file(F1, F2, F).

best_same_file(F1, F2, F) :-
    catch(same_file(F1, F2), _, fail),
    !,
    atom_length(F1, L1),
    atom_length(F2, L2),
    (   L1 < L2
    ->  F = F1
    ;   F = F2
    ).

merge_specs(Spec, Spec, Spec) :-
    !.
merge_specs(file(F1), file(F2), file(F)) :-
    best_same_file(F1, F2, F),
    !.
merge_specs(Spec1, Spec2, Spec) :-
    merge_specs_(Spec1, Spec2, Spec),
    !.
merge_specs(Spec1, Spec2, Spec) :-
    merge_specs_(Spec2, Spec1, Spec),
    !.

merge_specs_(FileSpec, Spec, Spec) :-
    is_filespec(FileSpec).

is_filespec(source_file(_)) => true.
is_filespec(Term),
    compound(Term),
    compound_name_arguments(Term, Alias, [_Arg]),
    user:file_search_path(Alias, _) => true.
is_filespec(_) =>
    fail.

%!  select_location(+Pairs, +UserSpec, -Location) is semidet.
%
%   @arg Pairs is a list of `Location-Spec` pairs
%   @arg Location is a list of properties

do_select_location(Pairs, Spec, Location) :-
    select_location(Pairs, Spec, Location),                % HOOK
    !,
    Location \== [].
do_select_location([], Spec, _) :-
    !,
    print_message(warning, edit(not_found(Spec))),
    fail.
do_select_location([#{file:File}-file(File)], _, Location) :-
    !,
    Location = #{file:File}.
do_select_location([Location-_Spec], _, Location) :-
    existing_location(Location),
    !.
do_select_location(Pairs, _, Location) :-
    foldl(number_location, Pairs, NPairs, 1, End),
    print_message(help, edit(select(NPairs))),
    (   End == 1
    ->  fail
    ;   Max is End - 1,
        user_selection(Max, I),
        memberchk(I-(Location-_Spec), NPairs)
    ).

%!  existing_location(+Location) is semidet.
%
%   True when Location can be edited.  By   default  that means that the
%   file exists. This facility is hooked   to allow for alternative ways
%   to reach the source, e.g., by lazily downloading it.

existing_location(Location) :-
    exists_location(Location),
    !.
existing_location(Location) :-
    #{file:File} :< Location,
    access_file(File, read).

number_location(Pair, N-Pair, N, N1) :-
    Pair = Location-_Spec,
    existing_location(Location),
    !,
    N1 is N+1.
number_location(Pair, 0-Pair, N, N).

user_selection(Max, I) :-
    user_select(Max, I),
    !.
user_selection(Max, I) :-
    print_message(help, edit(choose(Max))),
    read_number(Max, I).

%!  read_number(+Max, -X) is semidet.
%
%   Read a number between 1 and Max. If Max < 10, use get_single_char/1.

read_number(Max, X) :-
    Max < 10,
    !,
    get_single_char(C),
    put_code(user_error, C),
    between(0'0, 0'9, C),
    X is C - 0'0.
read_number(_, X) :-
    read_line_to_string(user_input, String),
    number_string(X, String).


                 /*******************************
                 *             MESSAGES         *
                 *******************************/

:- multifile
    prolog:message/3.

prolog:message(edit(Msg)) -->
    message(Msg).

message(not_found(Spec)) -->
    [ 'Cannot find anything to edit from "~p"'-[Spec] ],
    (   { atom(Spec) }
    ->  [ nl, '    Use edit(file(~q)) to create a new file'-[Spec] ]
    ;   []
    ).
message(select(NPairs)) -->
    { \+ (member(N-_, NPairs), N > 0) },
    !,
    [ 'Found the following locations:', nl ],
    sequence(target, [nl], NPairs).
message(select(NPairs)) -->
    [ 'Please select item to edit:', nl ],
    sequence(target, [nl], NPairs).
message(choose(_Max)) -->
    [ nl, 'Your choice? ', flush ].
message(waiting_for_editor) -->
    [ 'Waiting for editor ... ', flush ].
message(make) -->
    [ 'Running make to reload modified files' ].
message(canceled) -->
    [ 'Editor returned failure; skipped make/0 to reload files' ].

target(0-(Location-Spec)) ==>
    [ ansi(warning, '~t*~3| ', [])],
    edit_specifier(Spec),
    [ '~t~32|' ],
    edit_location(Location, false),
    [ ansi(warning, ' (no source available)', [])].
target(N-(Location-Spec)) ==>
    [ ansi(bold, '~t~d~3| ', [N])],
    edit_specifier(Spec),
    [ '~t~32|' ],
    edit_location(Location, true).

edit_specifier(Module:Name/Arity) ==>
    [ '~w:'-[Module],
      ansi(code, '~w/~w', [Name, Arity]) ].
edit_specifier(file(_Path)) ==>
    [ '<file>' ].
edit_specifier(source_file(_Path)) ==>
    [ '<loaded file>' ].
edit_specifier(include_file(_Path)) ==>
    [ '<included file>' ].
edit_specifier(Term) ==>
    [ '~p'-[Term] ].

edit_location(Location, false) ==>
    { location_label(Location, Label) },
    [ ansi(warning, '~s', [Label]) ].
edit_location(Location, true) ==>
    { location_label(Location, Label),
      location_url(Location, URL)
    },
    [ url(URL, Label) ].

location_label(Location, Label) :-
    #{file:File, line:Line} :< Location,
    !,
    short_filename(File, ShortFile),
    format(string(Label), '~w:~d', [ShortFile, Line]).
location_label(Location, Label) :-
    #{file:File} :< Location,
    !,
    short_filename(File, ShortFile),
    format(string(Label), '~w', [ShortFile]).

location_url(Location, File:Line:LinePos) :-
    #{file:File, line:Line, linepos:LinePos} :< Location,
    !.
location_url(Location, File:Line) :-
    #{file:File, line:Line} :< Location,
    !.
location_url(Location, File) :-
    #{file:File} :< Location.

%!  short_filename(+Path, -Spec) is det.
%
%   Spec is a way to refer to the file Path that is shorter. The path is
%   shortened by either taking  it  relative   to  the  current  working
%   directory or use one of the Prolog path aliases.

short_filename(Path, Spec) :-
    working_directory(Here, Here),
    atom_concat(Here, Local0, Path),
    !,
    remove_leading_slash(Local0, Spec).
short_filename(Path, Spec) :-
    findall(LenAlias, aliased_path(Path, LenAlias), Keyed),
    keysort(Keyed, [_-Spec|_]).
short_filename(Path, Path).

aliased_path(Path, Len-Spec) :-
    setof(Alias, file_alias_path(Alias), Aliases),
    member(Alias, Aliases),
    Alias \== autoload,             % confusing and covered by something else
    Term =.. [Alias, '.'],
    absolute_file_name(Term, Prefix,
                       [ file_type(directory),
                         file_errors(fail),
                         solutions(all)
                       ]),
    atom_concat(Prefix, Local0, Path),
    remove_leading_slash(Local0, Local1),
    remove_extension(Local1, Local2),
    unquote_segments(Local2, Local),
    atom_length(Local2, Len),
    Spec =.. [Alias, Local].

file_alias_path(Alias) :-
    user:file_search_path(Alias, _).

remove_leading_slash(Path, Local) :-
    atom_concat(/, Local, Path),
    !.
remove_leading_slash(Path, Path).

remove_extension(File0, File) :-
    file_name_extension(File, Ext, File0),
    user:prolog_file_type(Ext, source),
    !.
remove_extension(File, File).

unquote_segments(File, Segments) :-
    split_string(File, "/", "/", SegmentStrings),
    maplist(atom_string, SegmentList, SegmentStrings),
    maplist(no_quote_needed, SegmentList),
    !,
    segments(SegmentList, Segments).
unquote_segments(File, File).


no_quote_needed(A) :-
    format(atom(Q), '~q', [A]),
    Q == A.

segments([Segment], Segment) :-
    !.
segments(List, A/Segment) :-
    append(L1, [Segment], List),
    !,
    segments(L1, A).


                 /*******************************
                 *        LOAD EXTENSIONS       *
                 *******************************/

load_extensions :-
    load,
    fail.
load_extensions.

:- load_extensions.
