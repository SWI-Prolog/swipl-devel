/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2003-2017, University of Amsterdam
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

:- use_module(library(dcg/basics)).
:- prolog_load_context(directory, Dir),
   working_directory(_, Dir).

%       daily
%
%       True if this is a daily build

daily :-
    getenv('DAILY', true).

name :-
    daily,
    !,
    version(Major, Minor, Patch, Tag),
    vi_product_version(version(Major, Minor, Patch, Tag), VIProductVersion),
    format('!define _VERSION "~w"~n', [VIProductVersion]),
    get_time(X),
    format_time(string(Date), '%F', X),
    format('Name "SWI-Prolog ~w"~n', [Date]).
name :-
    version(Major, Minor, Patch, Tag),
    version_string(version(Major, Minor, Patch, Tag), Version),
    vi_product_version(version(Major, Minor, Patch, Tag), VIProductVersion),
    format('!define _VERSION "~w"~n', [VIProductVersion]),
    get_time(X),
    format_time(string(Date), '%F', X),
    format('Name "SWI-Prolog ~w (~s)"~n', [Version, Date]).

vi_product_version(version(Major,Minor,Patch,''), Version) :-
    !,
    format(string(Version), '~w.~w.~w.0', [Major, Minor, Patch]).
vi_product_version(version(Major,Minor,Patch,Tag), Version) :-
    tag_revision(Tag, Rev),
    format(string(Version), '~w.~w.~w.~w', [Major, Minor, Patch, Rev]).

tag_revision(Tag, Rev) :-
    atom_number(Tag, Rev),
    !.
tag_revision(Tag, Rev) :-
    atom_concat(rc, RevA, Tag),
    atom_number(RevA, Rev),
    !.
tag_revision(alpha, 1).
tag_revision(beta, 2).

version_string(version(Major,Minor,Patch,''), Version) :-
    !,
    format(string(Version), '~w.~w.~w', [Major, Minor, Patch]).
version_string(version(Major,Minor,Patch,Tag), Version) :-
    format(string(Version), '~w.~w.~w-~w', [Major, Minor, Patch, Tag]).


%!  outfile(-File:atom) is det.
%
%   Compute the name of the output file.

outfile(File) :-
    daily,
    !,
    outarch(Arch),
    get_time(X),
    format_time(string(Date), '%F', X),
    format(atom(File), 'swipl-~w-~w.exe',
            [Arch, Date]).
outfile(File) :-
    outarch(Arch),
    version(Major, Minor, Patch, Tag),
    (   Tag == ''
    ->  format(atom(File), 'swipl-~w-~w~w~w.exe',
               [Arch, Major, Minor, Patch])
    ;   format(atom(File), 'swipl-~w-~w~w~w-~w.exe',
               [Arch, Major, Minor, Patch, Tag])
    ).

outarch(w64) :-
    current_prolog_flag(arch, 'x64-win64'),
    !.
outarch(w32).

outfile :-
    outfile(File),
    format('!define _OUTFILE "~w"~n', [File]),
    format('OutFile "~w"~n', [File]).

version(Major, Minor, Patch, Tag) :-
    current_prolog_flag(version_git, V),
    atom_codes(V, Codes),
    phrase(version(Major, Minor, Patch, Tag), Codes, _),
    !.
version(Major, Minor, Patch, Tag) :-
    current_prolog_flag(version_data, swi(Major,Minor,Patch,Extra)),
    (   memberchk(tag(Tag), Extra)
    ->  true
    ;   Tag = ''
    ).

version(Major, Minor, Patch, Tag) -->
    number(Major), ".",
    number(Minor), ".",
    number(Patch),
    tag(Tag).

tag(Tag) -->
    "-", !,
    alnums(Codes),
    { atom_codes(Tag, Codes) }.
tag('') -->
    "".

alnums([H|T]) -->
    [H], {code_type(H, alnum)},
    !,
    alnums(T).
alnums([]) -->
    "".

run :-
    tell('version.nsi'),
    name,
    outfile,
    told.


                 /*******************************
                 *             DEFINES          *
                 *******************************/

:- dynamic
    def/2.

%!  get_defines is det.
%
%   Process /DName and /DName=Value options. Asserts facts to
%   def(Name, Value)

get_defines :-
    current_prolog_flag(argv, Argv),
    maplist(assert_defines, Argv).

assert_defines(Def) :-
    atom_codes(Def, Codes),
    phrase(def(Name, Value), Codes),
    assert(def(Name, Value)).

def(Name, Value) -->
    "/D", string(NameCodes), "=", string(ValueCodes), eos,
    !,
    { atom_codes(Name, NameCodes),
      atom_codes(Value, ValueCodes)
    }.
def(Name, '1') -->
    "/D", string(NameCodes), eos,
    !,
    { atom_codes(Name, NameCodes)
    }.


%!  expand_defs(+Codes, -Expanded) is det.
%
%   Expand ${Name} in Codes using defs

expand_defs([], []) :- !.
expand_defs([0'$, 0'{|T0], Expanded) :-
    append(NameCodes, [0'}|Rest], T0),
    !,
    atom_codes(Name, NameCodes),
    (   def(Name, Value)
    ->  true
    ;   throw(error(existence_error(def, Name), _))
    ),
    atom_codes(Value, ValueCodes),
    append(ValueCodes, RestOut, Expanded),
    !,
    expand_defs(Rest, RestOut).
expand_defs([H|T0], [H|T]) :-
    expand_defs(T0, T).



                 /*******************************
                 *             CHECK            *
                 *******************************/

:- dynamic
    install_file/1,
    install_dir/1,
    in_skip/2.

parse_script(Script) :-
    retractall(install_file(_)),
    retractall(install_dir(_)),
    open(Script, read, In),
    read_line_to_codes(In, Line0),
    process_file_decls(Line0, In),
    close(In).

process_file_decls(end_of_file, _) :- !.
process_file_decls(Line, In) :-
    (   phrase(process_file_decl, Line)
    ->  true
    ;   format('ERROR: Failed to process ~s~n', [Line]),
        portray_text(true),
        gtrace,
        phrase(process_file_decl, Line)
    ),
    read_line_to_codes(In, Line1),
    process_file_decls(Line1, In).

process_file_decl -->
    "!ifdef", ws, identifier(Id), ws,
    !,
    {   def(Id, _)
    ->  asserta(in_skip(Id, false))
    ;   asserta(in_skip(Id, true))
    }.
process_file_decl -->
    "!ifndef", ws, identifier(Id), ws,
    !,
    {   def(Id, _)
    ->  asserta(in_skip(Id, true))
    ;   asserta(in_skip(Id, false))
    }.
process_file_decl -->
    "!else", ws,
    !,
    {   retract(in_skip(Id, Skip))
    ->  negate(Skip, NewSkip),
        asserta(in_skip(Id, NewSkip))
    }.
process_file_decl -->
    "!endif", ws,
    !,
    { retract(in_skip(_,_)) -> true }.
process_file_decl -->
    {  in_skip(_, Skip)
    -> Skip == true
    },
    !,
    string(_),
    eos.
process_file_decl -->
    ws, "File", blank, ws,
    !,
    (   "/r", ws
    ->  path(Dir),
        { assert(install_dir(Dir))
        }
    ;   "/oname="
    ->  path(_Oname),
        blank, ws,
        install_file
    ;   "/nonfatal"
    ->  blank, ws,
        install_file
    ;   install_file
    ),
    ws.
process_file_decl -->
    string(_),
    eos.

install_file -->
    path(File),
    { assert(install_file(File)) }.

path(Path) -->
    token(Token),
    { prolog_to_os_filename(Path, Token)
    }.

token(Value) -->
    (   "\""
    ->  string(Codes),
        "\""
    ;   "'"
    ->  string(Codes),
        "'"
    ;   string(Codes),
        sep
    ),
    !,
    { expand_defs(Codes, Expanded),
      name(Value, Expanded)
    }.

identifier(Id) -->
    idcharf(C0),
    idchars(T),
    { atom_codes(Id, [C0|T]) }.

idchars([H|T]) --> idchar(H), idchars(T).
idchars([]) --> [].

idchar(C)  --> [C], { code_type(C, csym) }.
idcharf(C) --> [C], { code_type(C, csymf) }.

sep -->
    peek_blank,
    !.
sep -->
    eos.

ws -->
    blank,
    !,
    ws.
ws -->
    [].

peek_blank -->
    peek(C),
    { code_type(C, space)
    }.

peek(C, X, X) :-
    X = [C|_].

negate(true, false).
negate(false, true).

%       check_covered(+Dir)
%
%       See whether all files in Dir are covered by some install
%       instruction.

check_covered([]) :- !.
check_covered([H|T]) :-
    !,
    check_covered(H),
    check_covered(T).
check_covered(Dir) :-
    exists_directory(Dir),
    !,
    (   install_dir(D),
        same_file(Dir, D)
    ->  already_covered(D)
    ;   atom_concat(Dir, '/*', Pattern),
        expand_file_name(Pattern, Entries),
        check_covered(Entries)
    ).
check_covered(File) :-
    install_file(F),
    same_file(F, File),
    !.
check_covered(Path) :-
    ignore_file(File),
    file_base_name(Path, File),
    !.
check_covered(Path) :-
    file_directory_name(Path, Dir),
    ignore_dir(Dir),
    !.
check_covered(File) :-
    flag(errors, E, E+1),
    print_message(error, format('File ~w is not covered by installer',
                                [File])).

already_covered(Dir) :-
    (   install_file(File),
        atom_concat(Dir, X, File),
        sub_atom(X, 0, _, _, /),
        flag(errors, E, E+1),
        print_message(error, format('File ~w already covered by ~w',
                                    [File, Dir])),
        fail
    ;   true
    ).

check_files :-
    parse_script('swipl.nsi'),
    flag(errors, Old, 0),
    def('SWIPL', SWIPL),
    check_covered(SWIPL),
    flag(errors, New, Old),
    New == 0.

ignore_file('INDEX.pl').
ignore_file('Makefile').
ignore_file('swipl.home').
ignore_file('swipl.ico').
ignore_file('plwin.opt').
ignore_file('pl2xpce.pdb').
ignore_file('dlltest.pdb').
ignore_file('double_metaphone.pdb').
ignore_file('porter_stem.pdb').
ignore_file('Support SWI-Prolog development.url').
ignore_file('SWI-Prolog website.url').
ignore_file('uninstall.exe').

% ignore the space-package
ignore_file('space.pdb').
ignore_file('space.dll').
ignore_file('geos.dll').
ignore_file('spatialindex1.dll').
ignore_file('space.html').

ignore_dir('pl/library/space').


                 /*******************************
                 *             RUN IT           *
                 *******************************/

main :-
    (   get_defines,
        run,
        check_files,
        halt
    ;   halt(1)
    ).

% :- main.

