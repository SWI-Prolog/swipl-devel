:- module(libtotex,
	  [ libtotex/3,
	    libtotex/0
	  ]).
:- use_module(library(doc_latex)).
:- use_module(library(pldoc/doc_process)).
:- use_module(library(main)).
:- use_module(library(error)).
:- use_module(library(apply)).
:- use_module(library(lists)).

libtotex(Lib, Out, Options) :-
	use_module(user:Lib),			% we want the operators in user
	ensure_doc_loaded(Lib),
	doc_latex(Lib, Out,
		  [ stand_alone(false)
		  | Options
		  ]).

ensure_doc_loaded(File) :-
	(   doc_file_has_comments(File)
	->  true
	;   load_files(user:File, [if(true)]),
	    (	doc_file_has_comments(File)
	    ->	true
	    ;	format(user_error, 'WARNING: no comments for ~w~n', [File])
	    )
	    %xref_source(File, [comments(store)])
	).

libtotex(Options, TxtFile) :-
	file_name_extension(Base, Ext, TxtFile),
	markdown_ext(Ext), !,
        file_name_extension(Base, tex, TexFile),
	file_directory_name(TexFile, Dir),
	file_base_name(TexFile, TeXLocalFile),
	atomic_list_concat([Dir, '/summaries.d'], SummaryDir),
	atomic_list_concat([SummaryDir, /, TeXLocalFile], SummaryTeXFile),
	doc_latex(TxtFile, TexFile,
		  [ stand_alone(false),
		    summary(SummaryTeXFile)
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

markdown_ext(txt).
markdown_ext(md).

ensure_dir(Dir) :-
	exists_directory(Dir), !.
ensure_dir(Dir) :-
	make_directory(Dir).

strip(In, Code, Out) :-
	atom_codes(In, Codes0),
	delete(Codes0, Code, Codes),
	atom_codes(Out, Codes).

load_prolog([], []).
load_prolog([load(File)|T0], T) :- !,
	user:consult(File),
	load_prolog(T0, T).
load_prolog([H|T0], [H|T]) :-
	load_prolog(T0, T).


%%	libtotex
%
%	Usage: swipl -q -s libtotex.pl -g libtotex -- file ...

libtotex :-
	main.

main(Argv) :-
	partition(is_option, Argv, OptArgs, Files),
	maplist(to_option, OptArgs, AllOptions),
	load_prolog(AllOptions, Options),
	maplist(libtotex(Options), Files).

is_option(Arg) :-
	sub_atom(Arg, 0, _, _, --).

to_option('--section', section_level(section)).
to_option('--subsection', section_level(subsection)).
to_option('--subsubsection', section_level(subsubsection)).
to_option(Opt, output(TexBase)) :-
	atom_concat('--out=', TexBase, Opt).
to_option(Opt, module(Module)) :-
	atom_concat('--module=', Module, Opt).
to_option(Opt, outdir(Dir)) :-
	atom_concat('--outdir=', Dir, Opt).
to_option(Opt, load(File)) :-
	atom_concat('--load=', Atom, Opt),
	atom_to_term(Atom, File, _).
to_option(Opt, load(library(File))) :-
	atom_concat('--lib=', File, Opt).

