:- module(libtotex,
	  [ libtotex/3,
	    libtotex/0
	  ]).
:- use_module(library(doc_latex)).
:- use_module(library(main)).
:- use_module(library(error)).
:- use_module(library(apply)).
:- use_module(library(lists)).

libtotex(Lib, Out, Options) :-
	use_module(user:Lib),		% we want the operators in user
	doc_latex(Lib, Out,
		  [ stand_alone(false)
		  | Options
		  ]).

libtotex(Options, LibAtom) :-
	atom_to_term(LibAtom, Term, _),
	must_be(ground, Term),
	absolute_file_name(Term, File,
			   [ file_type(prolog),
			     access(read)
			   ]),
	file_base_name(File, Local),
	file_name_extension(Base0, _, Local),
	strip(Base0, 0'_, Base),
	file_name_extension(Base, tex, TeXLocalFile),
	option(outdir(Dir), Options, lib),
	atomic_list_concat([Dir, /, TeXLocalFile], TeXFile),
	atomic_list_concat([Dir, '/summaries.d'], SummaryDir),
	atomic_list_concat([SummaryDir, /, TeXLocalFile], SummaryTeXFile),
	ensure_dir(SummaryDir),
	libtotex(File, TeXFile,
		 [ summary(SummaryTeXFile)
		 | Options
		 ]).

ensure_dir(Dir) :-
	exists_directory(Dir), !.
ensure_dir(Dir) :-
	make_directory(Dir).

strip(In, Code, Out) :-
	atom_codes(In, Codes0),
	delete(Codes0, Code, Codes),
	atom_codes(Out, Codes).


%%	libtotex
%
%	Usage: pl -q -s libtotex.pl -g libtotex -- file ...

libtotex :-
	main.

main(Argv) :-
	partition(is_option, Argv, OptArgs, Files),
	maplist(to_option, OptArgs, Options),
	maplist(libtotex(Options), Files).

is_option(Arg) :-
	sub_atom(Arg, 0, _, _, --).

to_option('--section', section_level(section)).
to_option('--subsection', section_level(subsection)).
to_option('--subsubsection', section_level(subsubsection)).
to_option(Opt, output(TexBase)) :-
	atom_concat('--out=', TexBase, Opt).
to_option(Opt, outdir(Dir)) :-
	atom_concat('--outdir=', Dir, Opt).
