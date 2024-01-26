# SWI-Prolog cli applications

This directory is accessible using the file_search_path/2 alias `app`.
Prolog files in this directory can be started using

    swipl name [arg ...]

where _name_ is the name of the script in this file without extension.
The script file  must define initialization/2 using  the class `main`,
e.g.:

    :- initialization(mystart, main).

Here, mystart/0 typically uses library(main), resulting in

    :- use_module(library(main)).
	:- initialization(main, main).

	main(Argv) :-
	argv_options(Argv, Positional, Options),
		...

Option typing and  help can be specified  using opt_type/3, opt_help/3
and opt_meta/2.   See library(main) or  one of the examples  below for
details.
