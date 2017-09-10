/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2009-2017, University of Amsterdam
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

:- module(pltotex,
          [ pltotex/2
          ]).
:- use_module(library(doc_latex)).
:- use_module(library(main)).
:- use_module(library(apply)).
:- use_module(library(option)).
:- use_module(library(lists)).

:- initialization(main, main).

pltotex(File, Options) :-
    (   option(preload(Library), Options)
    ->  use_module(Library)
    ;   true
    ),
    wiki_extension(Ext),
    file_name_extension(_, Ext, File),
    !,
    tex_file(File, Out, Options),
    merge_options(Options, [stand_alone(false)], LatexOptions),
    doc_latex(File, Out, LatexOptions).
pltotex(Lib, Options) :-
    (   file_name_extension(_, pl, Lib)
    ->  Spec = Lib
    ;   atom_to_term(Lib, Spec, _)
    ),
    absolute_file_name(Spec, File,
                       [ access(read),
                         file_type(prolog)
                       ]),
    tex_file(File, Out, Options),
    user:use_module(File),          % we want the operators in user
    merge_options(Options, [stand_alone(false)], LatexOptions),
    doc_latex(File, Out, LatexOptions).

wiki_extension(txt).
wiki_extension(md).

tex_file(_, TeXFile, Options) :-
    option(out(Base), Options),
    !,
    file_name_extension(Base, tex, TeXFile).
tex_file(File, TeXFile, _) :-
    file_base_name(File, Local),
    file_name_extension(Base0, _, Local),
    strip(Base0, 0'_, Base),
    file_name_extension(Base, tex, TeXFile).

strip(In, Code, Out) :-
    atom_codes(In, Codes0),
    delete(Codes0, Code, Codes),
    atom_codes(Out, Codes).

%!  main(+Argv)
%
%   The entry point

main(Argv) :-
    partition(is_option, Argv, OptArgs, Files),
    maplist(to_option, OptArgs, Options0),
    flatten(Options0, Options),
    maplist(process_file(Options), Files).

is_option(Arg) :-
    sub_atom(Arg, 0, _, _, --).

to_option('--section', section_level(section)).
to_option('--subsection', section_level(subsection)).
to_option('--subsubsection', section_level(subsubsection)).
to_option('--rdf11',
          [ preload(library(semweb/rdf11)), modules([rdf11,rdf_db]) ]) :- !.
to_option('--rdfdb',
          [ preload(library(semweb/rdf_db)), module(rdf_db)]) :- !.
to_option(Arg, Option) :-
    atom_concat(--, Opt, Arg),
    sub_atom(Opt, B, _, A, =),
    !,
    sub_atom(Opt, 0, B, _, Name),
    sub_atom(Opt, _, A, 0, Value),
    Option =.. [Name, Value].

process_file(Options, File) :-
    pltotex(File, Options).

