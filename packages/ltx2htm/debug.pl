user:file_search_path(foreign, '.').
user:file_search_path(library, '.').

:- use_module(latex2html).

test :-
	latex2html(manual).
