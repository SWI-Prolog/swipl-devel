% $Id$
%
% This file contains some predicates that are defined in BIM-prolog and
% not in SWI-prolog (or at least not with the same meaning).
% In case a predicate has a different meaning in SWI-prolog 
% and in proLog by BIM renaming is done.
% Remark that some predicates are only partially covered, feel free to add.
%
% author: Henk Vandecasteele
%         Departement Computerwetenschappen
%         Katholiek Universiteit Leuven
%         Celestijnenlaan 200A
%         3001 Heverlee
%         BELGIUM
%         henk.vandecasteele@cs.kuleuven.ac.be
%
% modified by Jan Wielemaker:
%
%	  - Added module-declaration
%	  - Mapped writeClause/2 onto portray_clause/1
%	  - deleted the my... (SWI-Prolog can redefine predicates)
%   
% modified by Henk Vandecasteel:
%
%         - Added some missing predicates in the module-declaration.
%         - Index/2 of proLog by BIM is transformed to index/1.
%         - Fixed some bugs in the record-database-predicates.

module(bim,
       [ please/2,
	 cputime/1,
	 include/1,
	 setdebug/0,
	 reconsult/1,
	 erase/1,			% BIM-compatible erase/1
	 rerecord/2,
	 erase_all/1,
	 record/3,
	 recorded/3,			% BIM-compatible recorded/3
	 inttoatom/2,
	 atomconcat/3,
	 update/1,
	 printf/2,
	 random/1,
	 index/2,
	 predicate_type/2,
	 vread/2,
	 bindVariables/1,
	 writeClause/2
       ]).

:- op(700, xfx, <>).

% please/2 has no meaning in SWI-prolog (can't we map most actions to
% other things (JW?). (Maybe, but it would not be very useful as please/2 
% is usually called on-line. (HV) )

please(_, _).

cputime(Time):- 
	statistics(cputime, Time).

% include/1 does not exist in SWI-prolog.

include(_).

% setdebug/0 has no meaning in SWI-prolog.

setdebug.

:- module_transparent reconsult/1.

reconsult(X):-
	consult(X).

% erase/1 both exist in SWI-prolog and proLog by BIM.

erase(Key):- 
	system:recorded(Key, _, Reference),
	system:erase(Reference).
erase(_).


rerecord(Key, Value):-
	system:recorded(Key, _,Reference),!,
	system:erase(Reference),
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

recorded(Key1, Key2, Value):-
	data__(Key1, Key2, Value).

erase(Key1, Key2):-
	retract(data__(Key1, Key2, _)).


inttoatom(Int, Atom):-
	int_to_atom(Int, Atom).

atomconcat(Atom1, Atom2, Atom3):-
	concat(Atom1, Atom2, Atom3).

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

random(X):- X is random(1000000).


% Mapping the BIM index/2 to the SWI index/1 directive.

instantiateOther(Index, Nr, Nr):- index(Index).
instantiateOther(Index, ONr, Nr):-
 	NNr is ONr +1,
	arg(NNr, Index, 0),
	instantiateOther(Index, NNr, Nr).
instantiateOther(Index, ONr, Nr):-
	NNr is ONr + 1,
	instantiateOther(Index, NNr, Nr).


index(Pred/Nr, (Index1, Index2, Index3)):- 
	functor(Term, Pred, Nr), 
	arg(Index1, Term, 1),
	arg(Index2, Term, 1),
	arg(Index3, Term, 1),
	instantiateOther(Term, 0, Nr).
index(Pred/Nr, (Index1, Index2)):-
	functor(Term, Pred, Nr), 
	arg(Index1, Term, 1),
	arg(Index2, Term, 1),
	instantiateOther(Term, 0, Nr).
index(Pred/Nr, Index):-
	functor(Term, Pred, Nr), 
	arg(Index, Term, 1),
	instantiateOther(Term, 0, Nr).
	


predicate_type(reconsult(_), builtin). 
predicate_type(Head, builtin):-
	predicate_property(Head, built_in).

predicate_type(_, user). 

vread(Term, Variables):-
	read_variables(Term, Variables).


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
