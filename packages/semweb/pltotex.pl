:- module(pltotex,
	  [ pltotex/2,
	    pltotex/0
	  ]).
:- use_module(library(doc_latex)).
:- use_module(library(main)).
:- use_module(library(error)).
:- use_module(library(apply)).
:- use_module(library(lists)).

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
	user:use_module(File),		% we want the operators in user
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


%%	pltotex
%
%	Usage: pl -q -s pltotex.pl -g pltotex -- file ...

pltotex :-
	main.

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

