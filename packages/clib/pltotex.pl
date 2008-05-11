:- module(pltotex,
	  [ pltotex/3,
	    pltotex/0
	  ]).
:- asserta(user:file_search_path(foreign, '.')).
:- asserta(user:file_search_path(library, '.')).

:- use_module(library(doc_latex)).
:- use_module(library(main)).
:- use_module(library(error)).

pltotex(Lib, Out, Options) :-
	user:use_module(Lib),		% we want the operators in user
	doc_latex(Lib, Out,
		  [ stand_alone(false)
		  | Options
		  ]).

pltotex(File) :-
	use_module(File, []),
	file_base_name(File, Local),
	file_name_extension(Base, _, Local),
	file_name_extension(Base, tex, TeXFile),
	doc_latex(File, TeXFile,
		  [ stand_alone(false)
		  ]).

%%	pltotex
%
%	Usage: pl -q -s pltotex.pl -g pltotex -- file ...

pltotex :-
	main.

main(Argv) :-
	maplist(pltotex, Argv).
