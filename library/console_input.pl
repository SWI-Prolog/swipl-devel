/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org
    Copyright (c)  2013-2026, VU University Amsterdam
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

:- module(prolog_console_input,
          [
          ]).
:- autoload(library(lists), [reverse/2, append/2]).
:- autoload(library(dcg/basics), [remainder/3]).
:- autoload(library(apply), [maplist/3, convlist/3]).

/** <module> Support entering toplevel queries

This module implements prolog:complete_input/4, which is used notably by
the command line editor  library(edit)  to   perform  context  aware TAB
completion. It is a seperate library such that other input scenarios can
reuse this code.
*/


:- multifile
    prolog:complete_input/4.

%!  prolog:complete_input(+BeforeCursor:string, +AfterCursor:string,
%!                        -Delete:atom, -Completions:list) is semidet.
%
%   Compute    auto    completions    for      the     input    line
%   BeforeCursor+AfterCursor.  Implemented completions
%
%     - Files<br>
%       - Text starting ``'<prefix>`` (single quotes) is completed as a
%         file. If there is a single matching file a terminating `'` is
%         added.
%       - If the start of the line is ``[<prefix>`` and text can be
%         a non-quoted atom, look for Prolog files with this name.
%         Directories can be inserted as `/` (unquoted), as long as
%         each _segment_ is a valid atom.  If a single ``.pl`` file
%         is found it is closed with ``]``, _without_ the extension.
%       - Text starting ``library(<prefix>`` is completed as a
%         library file using the same rules as ``[<prefix>``, but
%         looking in all library directories instead of the current
%         directory.
%     - Atoms<br>
%       ``<prefix>`` is used to complete atoms that can be written
%       without quotes (excluding symbol atoms such as ``-->``) and
%       are _permanent_, i.e., have references from clauses records,
%       flags, foreign code, etc.
%     - Variables<br>
%       ``<Prefix>`` is completed as an atom if it is a valid variable
%       name.  It is completed with other atoms from the query being
%       edited using the same search as Emacs _dabbrev_ (_Dynamic
%       Abbreviation_). ``$<Prefix>`` expands to saved toplevel
%       variables.
%
%   @arg    Delete is an atom or string representing the text that is
%           replaced by the completion
%   @arg    Completions is a list of elements of this shape:
%
%             - Atom
%             Used for a plain completion without comment
%             - Atom-Comment
%             Used for a completion with comment.  This will be
%             used for predicates.

prolog:complete_input(Before, After, Delete, Completions) :-
    string_codes(Before, Chars),
    reverse(Chars, BeforeRev),
    complete(BeforeRev, After, Delete, Completions).

complete(BeforeRev, _After, Prefix, Files) :-   % complete files
    phrase(file_prefix(Prefix, Type), BeforeRev),
    !,
    (   Type = library(Close)
    ->  complete_library(Prefix, Close, Files)
    ;   atom_concat(Prefix, '*', Pattern),
        expand_file_name(Pattern, Files0),
        finish_file_name(Files0, Type, Files)
    ).
complete(BeforeRev, After, Prefix, Completions) :-   % complete atoms
    phrase(identifier_prefix_r(Prefix, Type), BeforeRev, BeforeRev1),
    !,
    identifier_completions(Type, Prefix, BeforeRev1, After, Completions).

identifier_completions(atom, Prefix, _, _, Atoms) :-
    '$atom_completions'(Prefix, Atoms).
identifier_completions(var, Prefix, BeforeRev, After, Vars) :-
    findall(Var, var_starts(Prefix, BeforeRev, After, Var), Vars0),
    sort(Vars0, Vars).

var_starts(Prefix, [0'$|_], _After, Var) :-        % $Var
    !,
    recorded('$topvar', Var = _),
    sub_atom(Var, 0, _, _, Prefix).
var_starts(Prefix, BeforeRev, _After, Var) :-        % Backard search
    phrase((..., nonid_char, identifier_prefix_r(Var, var)),
           BeforeRev, _),
    sub_string(Var, 0, _, _, Prefix).
var_starts(Prefix, _BeforeRev, AfterString, Var) :-  % Forward search
    string_codes(AfterString, After),
    phrase((..., nonid_char, identifier_prefix(Var, var)),
           After, _),
    sub_string(Var, 0, _, _, Prefix).

... --> [] ; [_], ... .

nonid_char -->
    [C],
    { \+ code_type(C, prolog_identifier_continue) }.

%!  identifier_prefix(-Prefix, -Type)   is semidet.
%!  identifier_prefix_r(-Prefix, -Type) is semidet.

identifier_prefix(Prefix, Type) -->
    atom_chars(String),
    { String = [H|_],
      (   code_type(H, prolog_var_start)
      ->  Type = var
      ;   Type = atom
      ),
      string_codes(Prefix, String) % do not create an atom
    }.

identifier_prefix_r(Prefix, Type) -->
    atom_chars(RevString),
    { reverse(RevString, String),
      String = [H|_],
      (   code_type(H, prolog_var_start)
      ->  Type = var
      ;   Type = atom
      ),
      string_codes(Prefix, String) % do not create an atom
    }.

atom_chars([H|T]) --> atom_char(H), !, atom_chars(T).
atom_chars([]) --> [].

atom_char(C) --> [C], { atom_char(C) }.

atom_char(C) :- code_type(C, prolog_identifier_continue).

%!  file_prefix(-Prefix, -Type)// is semidet.
%
%   True when the part before the cursor looks like a file name.

file_prefix(Prefix, file) -->
    file_chars(RevString, quoted('\'')), "'",
    !,
    remainder(_),
    { reverse(RevString, String),
      atom_codes(Prefix, String)
    }.
file_prefix(Prefix, consult(']')) -->
    file_chars(RevString, unquoted), "[",
    !,
    { reverse(RevString, String),
      atom_codes(Prefix, String)
    }.
file_prefix(Prefix, library(')')) -->
    file_chars(RevString, unquoted), "(yrarbil",
    !,
    remainder(_),
    { reverse(RevString, String),
      atom_codes(Prefix, String)
    }.

file_chars([H|T], Style) --> file_char(H, Style), !, file_chars(T, Style).
file_chars([], _) --> [].

file_char(C, Style) --> [C], { file_char(C, Style) }.

file_char(C, _) :- code_type(C, csym).
file_char(0'/, _).
file_char(C, quoted(_)) :-
    file_char(C).

file_char(0'.).
file_char(0'-).
file_char(0'~).
:- if(current_prolog_flag(windows,true)).
file_char(0':).
file_char(0'\s).
:- endif.

%!  finish_file_name(+Matches, -Completions) is det.
%
%   Add closing quote for a unique file name.

finish_file_name([Dir0], _, [Dir]) :-
    exists_directory(Dir0),
    !,
    atom_concat(Dir0, '/', Dir).
finish_file_name([File0], Close, [File]) :-
    exists_file(File0),
    close_file_name(File0, Close, File),
    !.
finish_file_name(Files0, _, Files) :-
    maplist(tag_dir, Files0, Files).

tag_dir(Dir, DirS) :-
    exists_directory(Dir),
    !,
    atom_concat(Dir, /, DirS).
tag_dir(File, File).

close_file_name(File0, consult(Close), File) :-
    file_name_extension(Base, Ext, File0),
    user:prolog_file_type(Ext, prolog),
    atom_concat(Base, Close, File).
close_file_name(File0, quoted(Close), File) :-
    atom_concat(File0, Close, File).

%!  complete_library(+Prefix:atom, +Close:atom, -Completions:list) is
%!                   semidet.
%
%   Complete to a library entry on "library(Prefix".

complete_library(Prefix, Close, Libraries) :-
    findall(Pairs, complete_one_libdir(Prefix, Pairs), DirPairs),
    (   DirPairs = [LibDir-[f(File)]]
    ->  atom_concat(LibDir, Local, File),
        atom_concat(Local, Close, Completion),
        Libraries = [Completion]
    ;   DirPairs = [LibDir-[d(Dir)]]
    ->  atom_concat(LibDir, Local, Dir),
        atom_concat(Local, '/', Completion),
        Libraries = [Completion]
    ;   maplist(local_libs, DirPairs, FilesLists),
        append(FilesLists, Libraries0),
        sort(Libraries0, Libraries)
    ).

complete_one_libdir(Prefix, LibdirS-Files) :-
    absolute_file_name(library(.), LibDir,
                       [ file_type(directory),
                         solutions(all)
                       ]),
    atom_concat(LibDir, /, LibdirS),
    atomic_list_concat([LibdirS, Prefix, '*'], Pattern),
    expand_file_name(Pattern, Entries),
    convlist(dir_or_source, Entries, Files0),
    sort(Files0, Files),
    Files \== [].

local_libs(LibDir-Members, Locals) :-
    maplist(local_file_name(LibDir), Members, Locals).

local_file_name(LibDir, f(File), Local) :-
    atom_concat(LibDir, Local, File).
local_file_name(LibDir, d(Dir), Local) :-
    atom_concat(LibDir, Local0, Dir),
    atom_concat(Local0, /, Local).

dir_or_source(Entry, f(Plain)) :-
    file_name_extension(Plain, Ext, Entry),
    user:prolog_file_type(Ext, prolog),
    !.
dir_or_source(Entry, d(Entry)) :-
    exists_directory(Entry).
