:- module(libtotex,
	  [ libtotex/3,
	    libtotex/0
	  ]).
:- use_module(library(doc_latex)).
:- use_module(library(main)).
:- use_module(library(error)).

libtotex(Lib, Out, Options) :-
	user:use_module(Lib),		% we want the operators in user
	doc_latex(Lib, Out,
		  [ stand_alone(false)
		  | Options
		  ]).

libtotex(LibAtom) :-
	atom_to_term(LibAtom, Term, _),
	must_be(ground, Term),
	absolute_file_name(Term, File,
			   [ file_type(prolog),
			     access(read)
			   ]),
	file_base_name(File, Local),
	file_name_extension(Base, _, Local),
	file_name_extension(Base, tex, TeXLocalFile),
	atom_concat('lib/', TeXLocalFile, TeXFile),
	atom_concat('lib/summaries.d/', TeXLocalFile, SummaryTeXFile),
	ensure_dir('lib/summaries.d'),
	libtotex(File, TeXFile,
		 [ summary(SummaryTeXFile)
		 ]).

ensure_dir(Dir) :-
	exists_directory(Dir), !.
ensure_dir(Dir) :-
	make_directory(Dir).


%%	libtotex
%
%	Usage: pl -q -s libtotex.pl -g libtotex -- file ...

libtotex :-
	main.

main(Argv) :-
	maplist(libtotex, Argv).
