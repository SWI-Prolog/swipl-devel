/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2018-2020, VU University Amsterdam
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

:- module(doc2tex,
          [ doc2tex/2
          ]).
:- use_module(library(dcg/basics)).
:- use_module(library(pure_input)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(main)).
:- use_module(library(uri)).
:- autoload(library(gui_tracer), [gtrace/0]).

/** <module> .doc to .tex translator

This program is part of the toolchain to generate the documentation. The
LaTeX sources for the documentation is written as .doc files on which we
do some preprocessing to generate the .tex. These steps include:

  - Change predicate references as Name/Arity into \predref{Name}{Arity}
  - Change PL_*() and S*() into \cfuncref{Func} references
  - Do tab expansion for code and verbatim blocks
  - Change hard-to-type sequences such as {=\=} into LaTeX url
    commands.

This translation was originally written in  Perl. It was translated into
Prolog to give more control over the process and make it more readable.
*/

:- initialization(main, main).

main([InFile, OutFile]) :-
    doc2tex(InFile, OutFile).

%!  doc2tex(+InFile, +OutFile)
%
%   Translate the .doc InFile into a .tex OutFile

doc2tex(InFile, OutFile) :-
    phrase_from_file(doc2tex(Result), InFile),
    setup_call_cleanup(
        open(OutFile, write, Out),
        emit(Result, Out),
        close(Out)).

doc2tex(Result) -->
    tr(One),
    !,
    { add_result(One, Result, Tail) },
    doc2tex(Tail).
doc2tex([]) -->
    [].

add_result(List, Result, Tail) :-
    is_list(List),
    !,
    append(List, Tail, Result).
add_result(One, [One|Tail], Tail).


		 /*******************************
		 *            RECOGNISERS	*
		 *******************************/

tr(\cfuncref(FName, Args)) -->		% PL_*, S*, etc. function
    pl_c_func_prefix(Prefix),
    c_identifier(Name), paren_arg(Chars),
    !,
    {  atom_concat(Prefix, Name, FName),
       expand_urldefs(Chars, Args)
    }.
tr(\cfuncref(FName, Args)) -->		% Pl* C++ function
    pl_cxx_func_prefix(Prefix),
    cxx_identifier(Name), paren_arg(Chars),
    !,
    {  atom_concat(Prefix, Name, FName),
       expand_urldefs(Chars, Args)
    }.
tr(\cfuncref(FName, Args)) -->		% C++ function or func()
    cxx_identifier(FName), paren_arg(Chars),
    {   sub_atom(FName, _, _, _, ::)
    ;   sub_atom(FName, _, _, _, ~)
    ;   Chars == []
    },
    !,
    {  expand_urldefs(Chars, Args)
    }.
tr(Object) -->				% Prolog predicate references
    unquoted_atom(Name),                % or just words
    !,
    tr_name(Name, Object).
tr([\begin(code),Code,\end(code),'\n\n',\noindent,'\n']) -->
    "\\begin{code}", string(CodeChars), "\\end{code}",
    !,
    whites, newlines,
    { expand_code(CodeChars, Code) }.
tr([\begin(verbatim),Code,\end(verbatim),'\n\n',\noindent,'\n']) -->
    "\\begin{verbatim}", string(CodeChars), "\\end{verbatim}",
    !,
    whites, newlines,
    { expand_code(CodeChars, Code) }.
tr(Result) -->
    "\\file{", string_without("}", FileChars), "}",
    { phrase(doc2tex(FileOut), FileChars),
      (   member(\bnfmeta(_), FileOut)
      ->  Result = \metafile({FileOut})
      ;   Result = \file({FileOut})
      )
    }.
tr(Line) -->
    "\\index{", index_line(Codes),
    blanks_to_nl,
    !,
    { append([`\\index{`, Codes, `%\n`], Line) }.
tr(Codes) -->
    here(Mark),
    "\\fmtseq{", tex_urlarg(1),
    !,
    matched(Mark, Codes).
tr(Codes) -->
    here(Mark),
    "\\satom{", tex_urlarg(1),
    !,
    matched(Mark, Codes).
tr(Codes) -->
    here(Mark),
    "\\url{", tex_urlarg(1),
    !,
    matched(Mark, Codes).
tr(['\\href{', URL, '}']) -->
    "\\href{", here(Mark),
    tex_urlarg(1),
    !,
    matched(Mark, Codes),
    { append(URLCodes, `}`, Codes),
      atom_codes(URL, URLCodes),
      (   uri_is_global(URL)
      ->  true
      ;   format(user_error, '~NERROR: Illegal URL in \\href: "~w"~n', [URL]),
          halt(1)
      )
    }.
tr(Codes) -->
    here(Mark),
    "\\verb", [Del], string(_Verb), [Del],
    !,
    matched(Mark, Codes).
tr(['{',\Command,'}']) -->
    "{", urldef(Command),
    !.
tr(\bnfmeta(Name)) -->
    "<", c_identifier(Name), ">",
    !.
tr(C) -->
    [C].

%!  paren_arg(-Codes)//
%
%   Read  a  string  between  parenthesis,    keeping  track  of  nested
%   parenthesis.
%
%   @tbd Test for too long argument lists?

paren_arg(Codes) -->
    "(", paren_arg(Codes, [0')]).

paren_arg([], [Close]) -->
    [Close],
    !.
paren_arg([H|T], Stack) -->
    [H],
    { code_type(H, paren(Close)) },
    !,
    paren_arg(T, [Close|Stack]).
paren_arg([H|T], [H|Stack]) -->
    [H],
    !,
    paren_arg(T, Stack).
paren_arg([H|T], Stack) -->
    [H],
    paren_arg(T, Stack).


expand_urldefs(Codes, String) :-
    phrase(doc2tex(Parsed), Codes),
    with_output_to(string(String), emit(Parsed, current_output)).

pl_c_func_prefix('PL_') --> "PL_", !.
pl_c_func_prefix('S')   --> "S", !.
pl_c_func_prefix('Plx') --> "Plx", !. % SWI-cpp2-plx.h
pl_c_func_prefix('Pl')  --> "Pl".  % SWI-cpp2.h

pl_cxx_func_prefix('Pl') --> "Pl", !. % SWI-cpp2.h

tr_name(Module, [\index(Index),\qpredref(Module,Name,write(Arity))]) -->
    ":", unquoted_atom(Name), "/", arity(Arity), !,
    { format(string(Index), '~w:~w/~w', [Module,Name,Arity]) }.
tr_name(Name, [\index(Index), \predref(Name,write(Arity))]) -->
    "/", arity(Arity), !,
    { format(string(Index), '~w/~w', [Name,Arity]) }.
tr_name(Name, Name) -->
    [].

%!  expand_code(+Chars, -String) is det.
%
%   Expand tabs in code

expand_code(Chars, String) :-
    expand_code(Chars, 0, Codes),
    string_codes(String, Codes).

expand_code([], _, []).
expand_code([0'\t|T0], X, T) :-
    !,
    Spaces is 8 - X mod 8,
    mkspaces(Spaces, T, T1),
    X1 is X + Spaces,
    expand_code(T0, X1, T1).
expand_code([0'\n|T0], _, [0'\n|T]) :-
    !,
    expand_code(T0, 0, T).
expand_code([H|T0], X, [H|T]) :-
    X1 is X+1,
    expand_code(T0, X1, T).

mkspaces(0, T, T) :- !.
mkspaces(I, [0'\s|T0], T) :-
    I2 is I - 1,
    mkspaces(I2, T0, T).

%!  index_line(-Codes)
%
%   Match a sequence of \index{...} commands

index_line(Codes) -->
    here(Mark),
    index_line,
    matched(Mark, Codes).

index_line -->
    tex_arg(1),
    (   "\\index{"
    ->  index_line
    ;   []
    ).

tex_arg(1) --> "}", !.
tex_arg(Level) --> "\\{", !, tex_arg(Level).
tex_arg(Level) --> "\\}", !, tex_arg(Level).
tex_arg(Level) --> "{", !, { Level1 is Level+1 }, tex_arg(Level1).
tex_arg(Level) --> "}", !, { Level1 is Level-1 }, tex_arg(Level1).
tex_arg(Level) --> [_], tex_arg(Level).


%!  here(-Mark)// is det.
%!  matched(+Mark, -Codes)// is det.

here(List,List,List).

matched(Mark, Codes) -->
    here(End),
    { diff_list(Mark, End, Codes) }.

diff_list(Mark, End, []) :-
    same_term(Mark, End),
    !.
diff_list([H|T0], End, [H|T]) :-
    diff_list(T0, End, T).


		 /*******************************
		 *            OUTPUT		*
		 *******************************/

emit([], _) :-
    !.
emit([H|T], Out) :-
    !,
    emit(H, Out),
    emit(T, Out).
emit(\Term, Out) :-
    !,
    Term =.. [Name|Args],
    format(Out, '\\~w', [Name]),
    maplist(emit_tex_arg(Out), Args).
emit(Module:Name/Arity, Out) :-
    !,
    format(Out, '~w:~w/~w', [Module, Name, Arity]).
emit(Module:Name, Out) :-
    !,
    format(Out, '~w:~w', [Module, Name]).
emit(Name/Arity, Out) :-
    !,
    format(Out, '~w/~w', [Name, Arity]).
emit(write(Term), Out) :-
    format(Out, '~w', [Term]).
emit(Atom, Out) :-
    atom(Atom),
    !,
    write(Out, Atom).
emit(String, Out) :-
    string(String),
    !,
    write(Out, String).
emit(C, Out) :-
    integer(C),
    !,
    put_code(Out, C).

emit_tex_arg(Out, [Arg]) :-
    !,
    write(Out, '['),
    emit(Arg, Out),
    write(Out, ']').
emit_tex_arg(Out, {Arg}) :-
    !,
    write(Out, '{'),
    emit(Arg, Out),
    write(Out, '}').
emit_tex_arg(Out, Arg) :-
    write(Out, '{'),
    +emit(Arg, Out),
    write(Out, '}').

:- meta_predicate +(0).

+(Goal) :-
    (   Goal
    ->  true
    ;   gtrace,Goal
    ).


		 /*******************************
		 *         GENERAL RULES	*
		 *******************************/

%!  unquoted_atom(-Name)//

unquoted_atom(Name) -->
	[C0], { code_type(C0, prolog_atom_start) }, !,
	prolog_id_cont(CL),
	{ atom_codes(Name, [C0|CL]) }.

prolog_id_cont([H|T]) -->
	[H], { code_type(H, prolog_identifier_continue) }, !,
	prolog_id_cont(T).
prolog_id_cont([]) --> "".

%!  c_identifier(-Identifier)//

c_identifier(Name) -->
    [C0], { code_type(C0, csymf) }, !,
    c_id_cont(CL),
    { atom_codes(Name, [C0|CL]) }.

c_id_cont([H|T]) -->
	[H], { code_type(H, csym) }, !,
	c_id_cont(T).
c_id_cont([]) --> "".

%!  cxx_identifier(-Identifier)//
% This covers thing such as Class::method

cxx_identifier(Name) -->
    [C0], { code_type(C0, csymf) }, !,
    cxx_id_cont(CL),
    { atom_codes(Name, [C0|CL]) }.

cxx_id_cont([0':,0':|T]) -->
    "::", !, % For C++ "::"
    cxx_id_cont(T).
cxx_id_cont([0'~|T]) --> % '
    "~", !, % For C++ destructor
    cxx_id_cont(T).
cxx_id_cont([H|T]) -->
    [H], { code_type(H, csym) }, !,
    cxx_id_cont(T).
cxx_id_cont([]) --> "".


%!  arity(-Spec)

arity(Arity) -->
    uint(Arity).
arity(Range) -->
    "[", uint(A0), arity_list(AL), "]",
    { term_string([A0|AL], Range, [spacing(standard)]) }.

arity_list([AH|AT]) -->
    ",", !, uint(AH),
    arity_list(AT).
arity_list([]) -->
    "".


%!  uint(-Integer)//

uint(I) -->
    uint_codes(Codes),
    { number_codes(I, Codes)
    }.

uint_codes([D0|D]) -->
	digit(D0),
	digits(D).

newlines -->
    "\n", !,
    newlines.
newlines -->
    "".

		 /*******************************
		 *             URLDEF		*
		 *******************************/

load_urldefs(StyFile, Defs) :-
    absolute_file_name(StyFile, InFile,
                       [ access(read)
                       ]),
    setup_call_cleanup(
        open(InFile, read, In),
        read_string(In, _, String),
        close(In)),
    split_string(String, "\n", "\n", Lines),
    convlist(urldef_clause, Lines, Defs).

urldef_clause(Line, (urldef(Command) --> StringCurl)) :-
    string_concat("\\urldef{\\", Rest, Line),
    string_codes(Rest, Codes),
    phrase(urldef(Command, String), Codes, _),
    string_concat(String, "}", StringCurl).

urldef(Command, String) -->
    c_identifier(Command), "}\\satom{",
    here(Mark),
    tex_urlarg(1),
    matched(Mark, Codes),
    { append(Arg, `}`, Codes),
      string_codes(String, Arg)
    }.

tex_urlarg(1) --> "}", !.
tex_urlarg(Level) --> "{", !, { Level1 is Level+1 }, tex_urlarg(Level1).
tex_urlarg(Level) --> "}", !, { Level1 is Level-1 }, tex_urlarg(Level1).
tex_urlarg(Level) --> [_], tex_urlarg(Level).


term_expansion(urldef(command,list,rest), Clauses) :-
    source_file(load_urldefs(_,_), File),
    file_directory_name(File, ManDir),
    atom_concat(ManDir, '/pl.sty', StyFile),
    load_urldefs(StyFile, Clauses).

urldef(command,list,rest).
