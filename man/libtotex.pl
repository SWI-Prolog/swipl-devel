:- module(libtotex,
	  [ libtotex/3,
	    libtotex/0
	  ]).
:- use_module(library(doc_latex)).
:- use_module(library(main)).
:- use_module(library(error)).

libtotex(Lib, Out, Options) :-
	use_module(Lib, []),
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
	libtotex(File, TeXFile,
		 [ summary(SummaryTeXFile)
		 ]).

libtotex :-
	main.

main(Argv) :-
	maplist(libtotex, Argv).
