:-module(term_table,
	 [ bug/1,
	   term/3,
	   ci_term/4,
	   plural/2,
	   singular/2,
	   find_term1/4,
	   find_term/3,
	   find_term_from_substring/3
	 ]).

bug(R) :-
	compare_strings(aat_new, 'nootèone', 'nootètwo', R).



:-dynamic stored_defs_handle/1.

user:file_search_path(foreign, Lib) :-
	feature(arch, Arch),
	concat('lib/', Arch, Lib).

:-use_module(library(table)).


:- initialization
	new_order_table(aat_new,
			[ case_insensitive,
			  tag("("),
			  ignore("'`<>"),
			  break(" ,-:"),
			  32=95,        %map underscore to space
%			  45=95,
			  232=101,
			  233=101,
			  136=101	% OEM \^e --> e
			]).


find_database_file(DB,Type,File):-
        Path =.. [DB,'.'],
	absolute_file_name(Path,
			   [ file_type(directory),
			     access(read)
			   ],
			   AAT),
	ensure_slash(AAT, AATDir),
	concat_atom([AATDir,DB,'_',Type, '.dat'], File).

ensure_slash(Name, Dir) :-
	concat(_, /, Name), !,
	Dir = Name.
ensure_slash(Name, Dir) :-
	concat(Name, /, Dir).


defs_table(Words):-
	stored_defs_handle(Words),!.
defs_table(Words):-
%       find_database_file(aat,term,File),
File = 'test.dat',
	new_table(File,
		  [  word(atom,[sorted(aat_new)]),
		     category(atom,[width(1)]),
		     index(integer,[width(8)])],
		  [ field_separator(0'!)],
		  Words),
	asserta(stored_defs_handle(Words)).


term(Word,Index,Category):-
	defs_table(DHandle),
	in_table(DHandle,
		 [word(Word),
                  category(Category),
		  index(Index)],
		  _RPos).


ci_term(Word,Term,Index,Category):-
	defs_table(DHandle),
	in_table(DHandle,
		 [word(Word,=(aat_new)),
                  category(Category),
		  index(Index)],
		  RPos),
        read_table_record(DHandle,RPos,_Next,
			  record(Term,_,_)).



find_term(Search_String,Term,Index):-
	defs_table(DHandle),
	in_table(DHandle,
		 [word(Search_String,prefix(aat_new)),
		     index(Index)],
		  _RPos),
	ident(Index,Term).

find_term_from_substring(Search_String,Term,Index):-
	defs_table(DHandle),
	in_table(DHandle,
		 [word(Search_String,substring(aat_new)),
		     index(Index)],
		  _RPos),
	ident(Index,Term).


find_term1(Search_String,Term,Index,Category):-
	defs_table(DHandle),
	in_table(DHandle,
		 [word(Search_String,prefix(aat_new)),
                  category(Category),
		     index(Index)],
		  RPos),
%	ident(Index,Term),
        read_table_record(DHandle,RPos,_Next,
			  record(Term,_,_)).


plural(X,Y):-
	term(X,I,a),
	idterm(I,Y,m),!.

plural(X,Y):-
	term(X,I,q),
	find_term1(X,T1,I,a),
	idterm(I,Y,q),
	X \=Y,
	find_term1(Y,T2,I,m),
	T1 \= T2,!.

singular(X,Y):-
	term(X,I,m),
	idterm(I,Y,a),!.

singular(X,Y):-
	term(X,I,q),
	find_term1(X,T1,I,m),
	idterm(I,Y,q),
	X \=Y,
	find_term1(Y,T2,I,a),
	T1 \= T2,!.
