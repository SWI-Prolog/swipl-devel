:- use_module(library(doc_latex)).
:- use_module(library('http/html_write')).

txttotex :-
	current_prolog_flag(argv, Argv),
	append(_, [--|Files], Argv),
	maplist(txttotex, Files).

txttotex(File) :-
	file_name_extension(Base, _, File),
	file_name_extension(Base, tex, TexFile),
	doc_latex(File, TexFile, [stand_alone(false)]).
