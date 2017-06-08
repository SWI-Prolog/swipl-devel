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
:- use_module(library(lists)).

:- initialization(main, main).

pltotex(Lib, Options) :-
    (   file_name_extension(_, pl, Lib)
    ->  Spec = Lib
    ;   atom_to_term(Lib, Spec, _)
    ),
    absolute_file_name(Spec, File,
                       [ access(read),
                         file_type(prolog)
                       ]),
    tex_file(File, Out),
    user:use_module(File),          % we want the operators in user
    doc_latex(File, Out,
              [ stand_alone(false)
              | Options
              ]).

tex_file(File, TeXFile) :-
    file_base_name(File, Local),
    file_name_extension(Base0, _, Local),
    strip(Base0, 0'_, Base),
    file_name_extension(Base, tex, TeXFile).

strip(In, Code, Out) :-
    atom_codes(In, Codes0),
    delete(Codes0, Code, Codes),
    atom_codes(Out, Codes).


main(Argv) :-
    partition(is_option, Argv, OptArgs, Files),
    maplist(to_option, OptArgs, Options),
    maplist(process_file(Options), Files).

is_option(Arg) :-
    sub_atom(Arg, 0, _, _, --).

to_option('--section', section_level(section)).
to_option('--subsection', section_level(subsection)).
to_option('--subsubsection', section_level(subsubsection)).

process_file(Options, File) :-
    pltotex(File, Options).

