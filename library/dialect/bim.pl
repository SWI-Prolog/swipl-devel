/*  Part of SWI-Prolog

    Author:        Henk Vandecasteele
    E-mail:        henk.vandecasteele@cs.kuleuven.ac.be
    WWW:           http://www.swi-prolog.org
    Copyright (C): Public domain
*/

/** <module> BIM compatibility layer

This file contains some predicates that   are  defined in BIM-prolog and
not in SWI-prolog (or at least not  with   the  same meaning). In case a
predicate has a different meaning in  SWI-prolog   and  in proLog by BIM
renaming is done.  Remark  that  some   predicates  are  only  partially
covered, feel free to add.

@author  Henk Vandecasteele
         Departement Computerwetenschappen
         Katholiek Universiteit Leuven
         Celestijnenlaan 200A
         3001 Heverlee
         BELGIUM
         henk.vandecasteele@cs.kuleuven.ac.be
*/

:- module(bim,
       [ please/2,
	 cputime/1,
	 setdebug/0,
	 bim_erase/1,			% BIM-compatible erase/1
	 bim_erase/2,			% BIM-compatible erase/2
	 rerecord/2,
	 erase_all/1,
	 (record)/3,
	 bim_recorded/3,			% BIM-compatible recorded/3
	 inttoatom/2,
	 atomconcat/3,
	 update/1,
	 printf/2,
	 bim_random/1,			% conflicts with library(random)
	 index/2,
	 predicate_type/2,
	 vread/2,
	 bindVariables/1,
	 writeClause/2
       ]).

:- op(700, xfx, <>).

		 /*******************************
		 *	     EXPANSION		*
		 *******************************/

:- multifile
	user:goal_expansion/2,
	user:file_search_path/2,
	user:prolog_file_type/2,
	bim_expansion/2.
:- dynamic
	user:goal_expansion/2,
	user:file_search_path/2,
	user:prolog_file_type/2.

user:goal_expansion(In, Out) :-
	prolog_load_context(dialect, yap),
	bim_expansion(In, Out).

%%	bim_expansion(+In, +Out)
%
%	goal_expansion rules to emulate YAP behaviour in SWI-Prolog. The
%	expansions  below  maintain  optimization    from   compilation.
%	Defining them as predicates would loose compilation.

bim_expansion(erase(Key), bim_erase(Key)).
bim_expansion(erase(Key1, Key2), bim_erase(Key1, Key2)).
bim_expansion(recorded(Key1, Key2, Value), bim_recorded(Key1, Key2, Value)).
bim_expansion(random(Int), bim_random(Int)).

% please/2 has no meaning in SWI-prolog (can't we map most actions to
% other things (JW?). (Maybe, but it would not be very useful as please/2
% is usually called on-line. (HV) )

please(_, _).

cputime(Time):-
	statistics(cputime, Time).

% setdebug/0 has no meaning in SWI-prolog.

setdebug.

% erase/1 both exist in SWI-prolog and proLog by BIM.

bim_erase(Key):-
	recorded(Key, _, Reference),
	erase(Reference).
bim_erase(_).


rerecord(Key, Value):-
	recorded(Key, _,Reference),!,
	erase(Reference),
	recorda(Key, Value).
rerecord(Key, Value):-
	recorda(Key, Value).



% the record-database with two keys of proLog by BIM is implemented with
% assert and retract.

erase_all(Key):- !,
	retractall(data__(_, Key, _)).

erase_all(_).

record(Key1, Key2, Value):-
        assert(data__(Key1, Key2, Value)).


% recorded/3 has a different meaning in SWI-prolog.

:- dynamic data__/3.

bim_recorded(Key1, Key2, Value):-
	data__(Key1, Key2, Value).

bim_erase(Key1, Key2):-
	retract(data__(Key1, Key2, _)).


inttoatom(Int, Atom):-
	atom_number(Atom, Int).

atomconcat(Atom1, Atom2, Atom3):-
	atom_concat(Atom1, Atom2, Atom3).

:- module_transparent update/1.

update(Clause):-
	functor(Clause, Name, Arity),
	functor(NewClause, Name, Arity),
	retract(NewClause),!,
	asserta(Clause).
update(Clause):-
	asserta(Clause).

printf(String, Param):-
	writef(String, Param).

bim_random(X):- X is random(1000000).


%%	index(+PI, +Indices) is det.
%
%	Index in the given arguments.  SWI-Prolog performs JIT indexing.

index(Pred/Nr, Indices):-
	print_message(warning, decl_no_effect(index(Pred/Nr, Indices))).


predicate_type(reconsult(_), builtin).
predicate_type(Head, builtin):-
	predicate_property(Head, built_in).

predicate_type(_, user).

vread(Term, Variables):-
	read_term(Term, [variable_names(Variables)]).


bindVariables([X = X | Bindings]):-
	bindVariables(Bindings).
bindVariables([]).

% writeClause/2 does the reverse of read_variables/2.  Hm? It used too.

writeClause(Clause, Bindings) :-
	bindVariablesForPortray(Bindings),
	portray_clause(Clause),
	fail.
writeClause(_, _).

bindVariablesForPortray([X = '$$VAR'(X) | Bindings]) :-
	bindVariablesForPortray(Bindings).
bindVariablesForPortray([]).
