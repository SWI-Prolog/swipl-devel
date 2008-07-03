:- asserta(user:file_search_path(library, ..)).

:- load_files([ library(doc_latex),
		library('http/html_write'),
		library('http/json'),
		library('http/json_convert'),
		library('http/http_json')
	      ],
	      [ silent(true)
	      ]).

txttotex :-
	current_prolog_flag(argv, Argv),
	append(_, [--|Files], Argv), !,
	maplist(txttotex, Files).

txttotex(File) :-
	file_name_extension(Base, _, File),
	file_name_extension(Base, tex, TexFile),
	doc_latex(File, TexFile, [stand_alone(false)]).
