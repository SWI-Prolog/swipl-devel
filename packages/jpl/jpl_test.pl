:- asserta(file_search_path(foreign, '.')).
:- asserta(file_search_path(jpl_examples, 'examples/prolog')).
:- asserta(file_search_path(jar, '.')).

:- [jpl].

		 /*******************************
		 *	       DEMOS		*
		 *******************************/

jpl_demo :-
	absolute_file_name(jpl_examples(.),
			   [ file_type(directory),
			     access(read)
			   ],
			   ExampleDir),
	atom_concat(ExampleDir, '/*.pl', Pattern),
	expand_file_name(Pattern, Examples),
	tag_basename(Examples, Entries),
	menu('Select JPL example', Entries, Example),
	consult(Example).

tag_basename([], []).
tag_basename([H|T0], [H:B|T]) :-
	file_base_name(H, B),
	tag_basename(T0, T).

