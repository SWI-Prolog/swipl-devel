/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2009-2018, University of Amsterdam
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

:- module(pltotex,
          [ pltotex/2
          ]).
:- use_module(library(doc_latex)).
:- use_module(library(pldoc/doc_process)).
:- use_module(library(prolog_xref)).
:- use_module(library(main)).
:- use_module(library(apply)).
:- use_module(library(option)).
:- use_module(library(lists)).
:- use_module(library(error)).

:- initialization(main, main).

pltotex(File, Options) :-
    wiki_extension(Ext),
    file_name_extension(_, Ext, File),
    !,
    preload(Options),
    tex_file(File, Out, Options),
    find_markdown_file(File, MarkDown, Options),
    merge_options(Options, [stand_alone(false)], LatexOptions0),
    summaries(Out, LatexOptions, LatexOptions0),
    doc_latex(MarkDown, Out, LatexOptions).
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
    ensure_doc_loaded(File),
    merge_options(Options, [stand_alone(false)], LatexOptions0),
    summaries(Out, LatexOptions, LatexOptions0),
    doc_latex(File, Out, LatexOptions).

preload(Options) :-
    option(preload(Library), Options),
    !,
    user:use_module(Library),
    ensure_doc_loaded(Library).
preload(_).

%!  ensure_doc_loaded(+File)
%
%   Make sure PlDoc comments for File are loaded. If they are now loaded
%   use the cross-referencer to force loading the documentation.

ensure_doc_loaded(File) :-
    (   doc_file_has_comments(File)
    ->  true
    ;   xref_source(File, [comments(store)]),
        (   doc_file_has_comments(File)
        ->  true
        ;   format(user_error, 'WARNING: no comments for ~w~n', [File])
        )
    ).

wiki_extension(txt).
wiki_extension(md).

%!  find_markdown_file(+Spec, -File, +Options) is det.
%
%   Find a Markdown input file, either at   the given location or in the
%   directory specified by the source(Directory) option.

find_markdown_file(Spec, File, _Options) :-
	exists_file(Spec), !,
	File = Spec.
find_markdown_file(Spec, File, Options) :-
	option(source(Dir), Options),
	atomic_list_concat([Dir,/,Spec], File),
	exists_file(File),
        !.
find_markdown_file(Spec, _File, _Options) :-
	existence_error(markdown_file, Spec).

%!  tex_file(+Input, -TexOutput, +Options) is det.
%
%   Determine the TeX output file from the Input and Options. By default
%   the TeX output is  the  name  of   the  input  file  after  deleting
%   underscores and replacing the extension   with  `tex`. The following
%   options control the behaviour:
%
%     - out(+File)
%       Specify the base name of the TeX file
%     - outdir(+Dir)
%       Specify an different directory for the output.

tex_file(Input, TexOutput, Options) :-
    tex_file_base(Input, TexOutput0, Options),
    (   option(outdir(Dir), Options)
    ->  atomic_list_concat([Dir, TexOutput0], /, TexOutput)
    ;   TexOutput = TexOutput0
    ),
    file_directory_name(TexOutput, TeXDir),
    ensure_dir(TeXDir).

tex_file_base(_, TeXFile, Options) :-
    option(out(Base), Options),
    !,
    file_name_extension(Base, tex, TeXFile).
tex_file_base(File, TeXFile, Options) :-
    file_base_name(File, Local),
    file_name_extension(Base0, _, Local),
    strip(Base0, 0'_, Base),
    file_name_extension(Base, tex, TeXFile0),
    (   option(outdir(_), Options)
    ->  TeXFile = TeXFile0
    ;   file_directory_name(File, Dir),
        atomic_list_concat([Dir, TeXFile0], /, TeXFile)
    ).

strip(In, Code, Out) :-
    atom_codes(In, Codes0),
    delete(Codes0, Code, Codes),
    atom_codes(Out, Codes).

%!  summaries(+TexFile, -TexOptions, +Options) is det.
%
%   Setup the creation of `summaries.d` if =|--summaries|= is given.

summaries(TexFile, TexOptions, Options) :-
    option(summaries(true), Options),
    !,
    file_directory_name(TexFile, TexDir),
    atomic_list_concat([TexDir, 'summaries.d'], /, SummaryDir),
    file_base_name(TexFile, TexLocalFile),
    atomic_list_concat([SummaryDir, TexLocalFile], /, SummaryTeXFile),
    ensure_dir(SummaryDir),
    TexOptions = [summary(SummaryTeXFile)|Options].
summaries(_, Options, Options).

ensure_dir(Dir) :-
	exists_directory(Dir), !.
ensure_dir(Dir) :-
	make_directory(Dir).

%!  main(+Argv)
%
%   The entry point

main(Argv) :-
    partition(is_option, Argv, OptArgs, Files),
    once(maplist(to_option, OptArgs, Options0)),
    flatten(Options0, Options),
    maplist(process_file(Options), Files).

is_option(Arg) :-
    sub_atom(Arg, 0, _, _, --).

to_option('--section', section_level(section)).
to_option('--subsection', section_level(subsection)).
to_option('--subsubsection', section_level(subsubsection)).
to_option(Opt, preload(library(File))) :-
    atom_concat('--lib=', File, Opt).
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
to_option(Arg, Option) :-
    atom_concat('--no-', Opt, Arg),
    !,
    Option =.. [Opt,false].
to_option(Arg, Option) :-
    atom_concat(--, Opt, Arg),
    Option =.. [Opt,true].

process_file(Options, File) :-
    (   option(trace(true), Options)
    ->  trace
    ;   true
    ),
    pltotex(File, Options).

