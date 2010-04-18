#!/usr/bin/swipl -q -g main,halt -t halt(1) -s

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This  file  emulates  SICStus   the    splfr   program,  which  extracts
declarations for foreign  resources  from  a   Prolog  file,  creates  a
wrapper, compiles this and finally  generates   a  shared object that is
automatically loaded into SWI-Prolog.

Note that this implementation  is  only   partial.  It  was  written for
running Alpino (www.let.rug.nl/vannoord/alp/Alpino/) and  only processes
the commandline options needed for this.

To use this facility, copy this file to   a  directory in your $PATH and
edit the first line to reflect the location of SWI-Prolog.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- use_module(library(prolog_source)).
:- use_module(library(qpforeign)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(option)).

:- debug(swipl_frl).

main :-
	current_prolog_flag(argv, Argv),
	append(_, [--|Av], Argv), !,
	swipl_frl(Av).

swipl_frl(Av) :-
	partition(longoption, Av, LongOptions, Av2),
	maplist(longoption, LongOptions, NVList),
	partition(plfile, Av2, PlFiles, Rest),
	PlFiles = [PlFile],
	file_name_extension(Base, _Ext, PlFile),
	create_glue(PlFile, GlueFile),
	option(cflag(CFlags), NVList, ''),
	atomic_list_concat([GlueFile,CFlags|Rest], ' ', Cmd0),
	format(atom(Cmd), 'swipl-ld -shared -o ~w ~w', [Base, Cmd0]),
	debug(swipl_frl, '~w', [Cmd]),
	shell(Cmd).

plfile(Name) :-
	\+ sub_atom(Name, 0, _, _, -),
	file_name_extension(_, pl, Name).

longoption(Name) :-
	sub_atom(Name, 0, _, _, --).

longoption(Option, Name=Value) :-
	atom_concat(--, Rest, Option),
	sub_atom(Rest, B, _, A, =),
	sub_atom(Rest, 0, B, _, Name),
	sub_atom(Rest, _, A, 0, Value).

%%	create_glue(+PrologFile, -GlueFile) is det
%
%	Create the glue foreign resources  in   PrologFile.  The glue is
%	written to GlueFile.

create_glue(File, Glue) :-
	file_name_extension(Base, _Ext, File),
	atom_concat(Base, '_swi_glue', GlueBase),
	file_name_extension(GlueBase, c, Glue),
	load_resource_decls(File, Module),
	create_module_glue(Module, Base, GlueBase).

create_module_glue(Module, Base, GlueBase) :-
	Module:foreign_resource(Resource, _),
	make_foreign_resource_wrapper(Module:Resource, Base, GlueBase).


%%	load_resource_decls(+Source, -Module)
%
%	Load SICSTus/Quintus resource declarations   from Source. Module
%	is the module in which the resources are loaded.

load_resource_decls(Source, Module) :-
	expects_dialect(sicstus),
	prolog_canonical_source(Source, Id),
	setup_call_cleanup(prolog_open_source(Id, In),
			   process(In, no_module, Module),
			   prolog_close_source(In)).


process(In, State0, Module) :-
	prolog_read_source_term(In, _, Expanded, []),
	process_terms(Expanded, State0, State1),
	(   State1 = end_of_file(EndState)
	->  state_module(EndState, Module)
	;   process(In, State1, Module)
	).

process_terms([], State, State) :- !.
process_terms([H|T], State0, State) :- !,
	process_term(H, State0, State1),
	(   State1 == end_of_file
	->  State = State1
	;   process_terms(T, State1, State)
	).
process_terms(T, State0, State) :-
	process_term(T, State0, State).

process_term(end_of_file, State, end_of_file(State)) :- !.
process_term((:- module(Name, _)), _, module(Name)) :- !,
	clean_resources(Name).
process_term(Term, State, State) :-
	foreign_term(Term, Assert), !,
	state_module(State, M),
	assertz(M:Assert).
process_term(_, State, State).

foreign_term(foreign_resource(Name, Funcs), foreign_resource(Name, Funcs)).
foreign_term(foreign(Func, Pred), foreign(Func, c, Pred)).
foreign_term(foreign(Func, Lang, Pred), foreign(Func, Lang, Pred)).

state_module(module(M), M).
state_module(no_module, user).

clean_resources(Module) :-
	forall(foreign_term(_, T), Module:retractall(T)).
