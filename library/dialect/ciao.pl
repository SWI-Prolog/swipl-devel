/*  Part of SWI-Prolog

    Author:        Jan Wielemaker, Edison Mera
    E-mail:        J.Wielemaker@uva.nl, efmera@gmail.com
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2009, University of Amsterdam and
                   2013, Process Design Center, Breda, The Netherlands.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(ciao,
	  [
	   '$ciao_meta'/2,
	   '$ciao_meta'/3,
	   '$ciao_meta'/4,
	   '$ciao_meta'/5,
	   '$ciao_meta'/6,
	   '$ciao_meta'/7,
	   '$ciao_meta'/8,
	   op(1150, fx, data)
	  ]).
:- use_module(library(error)).
:- use_module(library(apply)).
:- use_module(library(debug)).

/** <module> Ciao Prolog compatibility module

This module sets up support for loading   Ciao Prolog modules that start
with a :- module(Name, Exports,   Packages) directive. Upon encountering
this directive, it is rewritten into   a  SWI-Prolog module declaration,
followed by a series of directives to setup Ciao compatibility.

Typical usage for loading Ciao code is:

    ==
    :- use_module(library(dialect/ciao)).
    ==

@tbd	Create much more of the Ciao infrastructure.
*/

:- multifile
	system:goal_expansion/2,
	system:term_expansion/2,
	ciao_trans/4,
	ciao_trans_db/5,
	ciao_goal_expansion/2,
	ciao_term_expansion/2,
	user:file_search_path/2.


		 /*******************************
		 *	       PATHS		*
		 *******************************/

user:file_search_path(engine, library(dialect/ciao/engine)).


		 /*******************************
		 *    MODULES & DECLARATIONS	*
		 *******************************/

:- create_prolog_flag(multi_arity_warnings,   off, [type(atom)]).
:- create_prolog_flag(discontiguous_warnings, on,  [type(atom)]).

:- multifile
	declaration/1,		 % +Head
	ciao:declaration_hook/2. % +Head,-Exp

:- dynamic
	lock_expansion/0,
	old_flag/3.

compilation_module(CM) :-	% Kludge: compilations module must be
				% imported in a separated module and
				% its methods invoked from there --EMM
	'$set_source_module'(M, M),
	atom_concat(M, '$ciao', CM).
				% TODO: missing support for directives
				% add_clause_translation/1 and
				% add_term_translation/1 --EMM

call_lock(Goal) :-
    setup_call_cleanup((\+ lock_expansion,assertz(lock_expansion)),
		       Goal, retract(lock_expansion)).

system:goal_expansion(In, Out) :-
	prolog_load_context(dialect, ciao),
	compilation_module(CM),
	ciao_trans(CM, goal, In, Out). % Ciao Goal Translations
system:goal_expansion(In, Out) :-
	prolog_load_context(dialect, ciao),
	ciao_goal_expansion(In, Out). % SWI Compatibility issues
system:term_expansion(In, Out) :-
	prolog_load_context(dialect, ciao),
	compilation_module(CM),
	call_lock((ciao_trans(CM, sentence, In, Out0), % Sentence Translations
		   '$expand':expand_terms(call_term_expansion([system-[term_expansion/2]]),
							      Out0, _, Out, _) % Remaining
		  )).
system:term_expansion(In, Out) :-
	prolog_load_context(dialect, ciao),
	ciao_term_expansion(In, Out).

package_file(F, P) :-
	( atom(F) -> P = library(F)
	; functor(F, _, 1) -> P = F
	).

package_directive(Package, Directive) :-
	expand_term((:- use_package(Package)), Directive).

ciao_term_expansion((:- module(Name, Public, Packages)),
		      [ (:- module(Name, Public)),
			(:- style_check(-atom)),
			(:- style_check(-singleton)),
			(:- expects_dialect(ciao)),
			(:- use_module(engine(basic_props))),
			(:- use_module(engine(io_aux))),
			(:- use_module(engine(exceptions)))
		      |	Directives
		      ]) :-
	maplist(package_directive, Packages, Directives).

map_ciaoname_rec(Ciao, Path, Path/Ciao) :- atom(Ciao), !.
map_ciaoname_rec(Ciao0, Path, SWI) :-
	Ciao0 =.. [F, Ciao],
	map_ciaoname_rec(Ciao, Path/F, SWI). 

map_ciaoname_(Path, Path) :- atom(Path), !.
map_ciaoname_(Ciao0, SWI) :-
	Ciao0 =.. [F, Ciao],
	map_ciaoname_rec(Ciao, F, SWI).

map_ciaoname(CiaoName, SWIName) :-
	CiaoName =.. [F, C],
	SWIName =.. [F, S],
	map_ciaoname_(C, S).

ciao_term_expansion((:- use_package(CiaoPack)),
		    (:- include(SWIName))) :-
	package_file(CiaoPack, CiaoName),
	map_ciaoname(CiaoName, SWIName).
ciao_term_expansion((:- new_declaration(Name/Arity)),
		    ciao:declaration(Head)) :-
	functor(Head, Name, Arity).
ciao_term_expansion((:- package(_Package)), []).
ciao_term_expansion((:- Decl), Exp) :-
	declaration(Decl),
	(   ciao:declaration_hook(Decl, Exp)
	->  true
	;   functor(Decl, Name, Arity),
	    prolog_load_context(module, Module),
	    current_predicate(Module:Name/Arity)
	->  Exp = (:- Decl)
	;   Exp = []
	).

%%	map_metaspecs(+CiaoSpec, -SWISpec)// is det.
%
%	Map a Ciao meta-predicate to a SWI-Prolog one.
%
%	@see http://ciao-lang.org/docs/branches/1.14/13646/CiaoDE-1.14.2-13646_ciao.html/modules.html#meta_predicate/1
%       TODO: list(Spec) not supported yet
map_metaspecs(Var, _) -->
	{ var(Var), !,
	  instantiation_error(Var)
	}.
map_metaspecs((A0,B0), (A,B)) --> !,
	map_metaspecs(A0, A),
	map_metaspecs(B0, B).
map_metaspecs(Head0, Head) -->
	{ functor(Head0,  Name, Arity),
	  functor(Head,   Name, Arity),
	  functor(HeadIn, Name, Arity),
	  HeadIn =.. [Name|ArgsIn],
	  meta_expansion(Head0, Head, HeadIn, M, RequiresModule, ArgsOut, [])
	},
	( { ArgsIn == ArgsOut } -> []
	; { HeadOut =.. [Name|ArgsOut] },
	  ( {RequiresModule==1} ->
	    [ (:- module_transparent(Name/Arity)) ],
	    { Body = (context_module(M), HeadOut) }
	  ; { Body = HeadOut }
	  ),
	  [ (HeadIn :- Body) ]
	).

map_metaspec(Var, ?) :-
	var(Var), !.
map_metaspec(goal, 0).
map_metaspec(clause, :).
map_metaspec(fact, :).
map_metaspec(spec, :).
map_metaspec(pred(N), N).
map_metaspec(?, ?).
map_metaspec(+, +).
map_metaspec(-, -).

module_sensitive(goal).
module_sensitive(clause).
module_sensitive(fact).
module_sensitive(spec).
module_sensitive(pred(_)).

meta_expansion(Head0, Head, HeadIn, M, RequiresModule) -->
	meta_expansion_args(1, Head0, Head, HeadIn, M, RequiresModule).

meta_expansion_arg(Spec, TSpec, Arg, _, _) -->
	{map_metaspec(Spec, TSpec)}, !,
	[Arg].
meta_expansion_arg(addmodule(Spec), TSpec, Arg, M, 1) --> !,
	meta_expansion_arg(Spec, TSpec, Arg, M),
	[M].
meta_expansion_arg(addterm(Spec), TSpec, Arg0, M, R) --> !,
	meta_expansion_arg(Spec, TSpec, Arg0, M, R),
	{ module_sensitive(Spec) -> Arg0 = _:Arg
	; Arg0 = Arg
	},
	[Arg].
meta_expansion_arg(addmodule, TSpec, Arg, M, R) --> !,
	meta_expansion_arg(addmodule(?), TSpec, Arg, M, R).
meta_expansion_arg(addterm, TSpec, Arg, M, R) --> !,
	meta_expansion_arg(addterm(?), TSpec, Arg, M, R).
meta_expansion_arg(Spec, Spec, Arg, _, _) --> [Arg].

meta_expansion_args(N, Meta, Head, HeadIn, M, R) -->
	{arg(N, Meta, Spec)},
	{arg(N, Head, TSpec)},
	{arg(N, HeadIn, Arg)},
	meta_expansion_arg(Spec, TSpec, Arg, M, R),
	{N1 is N + 1},
	!,
	meta_expansion_args(N1, Meta, Head, HeadIn, M, R).
meta_expansion_args(_, _, _, _, _, _) --> [].

ciao_term_expansion((:- use_module(CiaoName)), (:- use_module(SWIName))) :-
	map_ciaoname(CiaoName, SWIName).
ciao_term_expansion((:- use_module(CiaoName, L)), (:- use_module(SWIName, L))) :-
	map_ciaoname(CiaoName, SWIName).
ciao_term_expansion((:- include(CiaoName)), (:- include(SWIName))) :-
	map_ciaoname(CiaoName, SWIName).
ciao_term_expansion((:- reexport(CiaoName)), (:- reexport(SWIName))) :-
	map_ciaoname(CiaoName, SWIName).
ciao_term_expansion((:- reexport(CiaoName, L)), (:- reexport(SWIName, L))) :-
	map_ciaoname(CiaoName, SWIName).
ciao_term_expansion((:- meta_predicate(CiaoSpec)),
		      [ (:- meta_predicate(SWISpec))
		      | Wrappers
		      ]) :-
	(   phrase(map_metaspecs(CiaoSpec, SWISpec), Wrappers)
	->  true
	;   debug(ciao, 'Failed to translate ~q',
		  [(:- meta_predicate(CiaoSpec))]),
	    fail
	).
ciao_term_expansion((:- data(Data)), (:- dynamic(Data))).
ciao_term_expansion((:- primitive_meta_predicate(CiaoSpec)), SWIDecl) :-
	expand_term((:- meta_predicate(CiaoSpec)), SWIDecl).
ciao_term_expansion((:- redefining(F/A)), (:- redefine_system_predicate(H))) :-
	functor(H, F, A).
ciao_term_expansion((:- load_compilation_module(CiaoName)),
		    [(:- CM:use_module(SWIName))]) :-
	compilation_module(CM),
	map_ciaoname(CiaoName, SWIName).
ciao_term_expansion((:- add_sentence_trans(F/A, P)),
		    ciao:ciao_trans_db(CM, sentence, P, F, A)) :-
	compilation_module(CM),
	( current_predicate(CM:F/A) -> true
	; throw(error(existence_error(add_sentence_trans, F/A), _))
	).
ciao_term_expansion((:- add_goal_trans(F/A, P)),
		    ciao:ciao_trans_db(CM, goal, P, F, A)) :-
	compilation_module(CM),
	( current_predicate(CM:F/A) -> true
	; throw(error(existence_error(add_goal_trans, F/A), _))
	).
ciao_term_expansion((H :- B), Clause) :-
				% In Ciao, A :- A is one of the ways
				% to tell the system A is a built-in.
	H == B, !,
	functor(H, F, A),
	Clause = (:- export(F/A)).
ciao_term_expansion((:- impl_defined(L)), Clauses) :-
	'$set_source_module'(M, M),
	findall(H, ( sequence_contains(L, bad_spec_error(impl_defined), F, A),
		     \+ current_predicate(M:F/A),
		     functor(H, F, A)
		   ),
		Clauses). % Define dummy implementations to the
		          % impl_defined predicates that do not have a
		          % real implementation to avoid warnings.

bad_spec_error(impl_defined, Spec) :-
	throw(error(domain_error(predname, Spec), _)).

:- meta_predicate sequence_contains(+,1,-,-).
sequence_contains(V, BadP, _, _) :- var(V), !,
	call(BadP, V), fail.
sequence_contains([], _, _, _) :- !, fail.
sequence_contains([S|Ss], BadP, F, A) :- !,
        ( sequence_contains(S, BadP, F, A)
        ; sequence_contains(Ss, BadP, F, A)
        ).
sequence_contains((S,Ss), BadP, F, A) :- !,
        ( sequence_contains(S, BadP, F, A)
        ; sequence_contains(Ss, BadP, F, A)
        ).
sequence_contains(F/A, _, F, A) :-
        atom(F), integer(A), !.
sequence_contains(S, BadP, _, _) :-
        call(BadP, S), fail.

get_expansor(F, A, M, Dict, Term0, Term, TR) :-
	functor(TR, F, A),
	arg(1, TR, Term0),
	arg(2, TR, Term),
	ignore(arg(3, TR, M)),
	ignore(arg(4, TR, Dict)).

call_sentence_expansion([],        _,  _, _,    Term,  Pos,  Term, Pos).
call_sentence_expansion([F/A|PIs], CM, M, Dict, Term0, Pos0, Term, Pos) :-
	( get_expansor(F, A, M, Dict, Term0, Term1, Expansor),
	  CM:Expansor ->
	  '$expand':expand_terms(ciao:call_sentence_expansion(PIs, CM, M, Dict),
				 Term1, Pos0, Term, Pos)
	; call_sentence_expansion(PIs, CM, M, Dict, Term0, Pos0, Term, Pos)
	).

call_goal_expansion([],        _,  _, _,    Term,  Term).
call_goal_expansion([F/A|PIs], CM, M, Dict, Term0, Term) :-
	( get_expansor(F, A, M, Dict, Term0, Term1, Expansor),
	  CM:Expansor -> true
	; Term0 = Term1
	),
	call_goal_expansion(PIs, CM, M, Dict, Term1, Term).

call_expansion(sentence, PIs, CM, M, Dict, Term0, Term) :-
	call_sentence_expansion(PIs, CM, M, Dict, Term0, _, Term, _).
call_expansion(goal, PIs, CM, M, Dict, Term0, Term) :-
	call_goal_expansion(PIs, CM, M, Dict, Term0, Term).

:- use_module(library(prolog_clause), []). % read_term_at_line/6
:- use_module(library(pairs), [pairs_values/2]).

get_dictionary(Term, M, Dict) :-
	( source_location(File, Line),
	  prolog_clause:read_term_at_line(File, Line, M, RawTerm, _TermPos, Dict),
	  subsumes(RawTerm, Term) -> true
	; Dict = []
	).

get_expansors(CM, Trans, PIs) :-
	findall(P-(F/A), ciao_trans_db(CM, Trans, P, F, A), UKPIs),
	keysort(UKPIs, KPIs),
	pairs_values(KPIs, PIs).

% :- meta_predicate ciao_trans(:, ?, ?, ?).
ciao_trans(CM, Trans, Term0, Term) :-
	get_expansors(CM, Trans, PIs),
	PIs \= [],
	'$set_source_module'(M, M),
	get_dictionary(Term0, M, Dict),
	call_expansion(Trans, PIs, CM, M, Dict, Term0, Term).

swi_meta_arg(_,    Arg,   Arg) :-
	(var(Arg) ; atom(Arg)), !.
swi_meta_arg(_,    M:Arg, M:Arg) :-
	(var(M)   ; atom(M)),
	(var(Arg) ; atom(Arg)), !.
swi_meta_arg(_,    '$ciao_meta'(Arg), '$ciao_meta'(Arg)) :- !.
swi_meta_arg(Meta, Arg, '$ciao_meta'(Arg)) :- integer(Meta), Meta > 0, !.
swi_meta_arg(_, _, Arg, Arg).

swi_meta_args(Spec, CiaoGoal, SWIGoal) :-
	functor(CiaoGoal, F, A),
	functor(SWIGoal, F, A),
	swi_meta_args(1, Spec, CiaoGoal, SWIGoal).

swi_meta_args(N, Spec, CiaoGoal, SWIGoal) :-
	arg(N, Spec, Meta),
	!,
	arg(N, CiaoGoal, CiaoArg),
	arg(N, SWIGoal, SWIArg),
	swi_meta_arg(Meta, CiaoArg, SWIArg),
	N1 is N + 1,
	swi_meta_args(N1, Spec, CiaoGoal, SWIGoal).
swi_meta_args(_, _, _, _).

%% In Ciao the order of argument for a meta-predicate must be changed,
%% so the first meta-argument is placed in the first place, and the
%% rest at the end, to facilitate indexation. For example,
%% in Ciao, call(p(a,b),c,d,e) must be converted to p(c,a,b,d,e):

:- meta_predicate
	'$ciao_meta'(1, ?),
	'$ciao_meta'(2, ?, ?),
	'$ciao_meta'(3, ?, ?, ?),
	'$ciao_meta'(4, ?, ?, ?, ?),
	'$ciao_meta'(5, ?, ?, ?, ?, ?),
	'$ciao_meta'(6, ?, ?, ?, ?, ?, ?),
	'$ciao_meta'(7, ?, ?, ?, ?, ?, ?, ?).

'$ciao_meta'(M:P0, A1) :-
	P0 =.. [F|Args],
	P =.. [F, A1|Args],
	call(M:P).
'$ciao_meta'(M:P0, A1, A2) :-
	P0 =.. [F|Args],
	P =.. [F, A1|Args],
	call(M:P, A2).
'$ciao_meta'(M:P0, A1, A2, A3) :-
	P0 =.. [F|Args],
	P =.. [F, A1|Args],
	call(M:P, A2, A3).
'$ciao_meta'(M:P0, A1, A2, A3, A4) :-
	P0 =.. [F|Args],
	P =.. [F, A1|Args],
	call(M:P, A2, A3, A4).
'$ciao_meta'(M:P0, A1, A2, A3, A4, A5) :-
	P0 =.. [F|Args],
	P =.. [F, A1|Args],
	call(M:P, A2, A3, A4, A5).
'$ciao_meta'(M:P0, A1, A2, A3, A4, A5, A6) :-
	P0 =.. [F|Args],
	P =.. [F, A1|Args],
	call(M:P, A2, A3, A4, A5, A6).
'$ciao_meta'(M:P0, A1, A2, A3, A4, A5, A6, A7) :-
	P0 =.. [F|Args],
	P =.. [F, A1|Args],
	call(M:P, A2, A3, A4, A5, A6, A7).

ciao_foldl(L, S, O, R) :- foldl(O, L, S, R).

ciao_goal_expansion(atom_concat(A, B),          atomic_list_concat(A, B)) :- !.
ciao_goal_expansion(asserta_fact(Fact),         asserta(Fact)) :- !.
ciao_goal_expansion(asserta_fact(Fact, Ref),    asserta(Fact, Ref)) :- !.
ciao_goal_expansion(assertz_fact(Fact),         assertz(Fact)) :- !.
ciao_goal_expansion(assertz_fact(Fact, Ref),    assertz(Fact, Ref)) :- !.
ciao_goal_expansion(retract_fact(Fact),         retract(Fact)) :- !.
ciao_goal_expansion(retract_fact_nb(Fact),      retract(Fact)) :- !.
ciao_goal_expansion(retract_fact(Fact, Ref),    retract(Fact, Ref)) :- !.
ciao_goal_expansion(retract_fact_nb(Fact, Ref), retract(Fact, Ref)) :- !.
ciao_goal_expansion(retractall_fact(Fact),      retractall(Fact)) :- !.
ciao_goal_expansion(current_fact(Fact),         clause(Fact, _)) :- !.
ciao_goal_expansion(current_fact(Fact, Ref),    clause(Fact, _, Ref)) :- !.
ciao_goal_expansion(current_fact_nb(Fact),      clause(Fact, _)) :- !.
ciao_goal_expansion(current_fact_nb(Fact, Ref), clause(Fact, _, Ref)) :- !.
ciao_goal_expansion('$exit'(Code),              halt(Code)) :- !.
ciao_goal_expansion('$metachoice'(Choice),      prolog_current_choice(Choice)) :- !.
ciao_goal_expansion('$metacut'(Choice),         prolog_cut_to(Choice)) :- !.
ciao_goal_expansion('$meta_call'(Goal),         call(Goal)) :- !.
ciao_goal_expansion('$setarg'(Arg, Term, Value, on), setarg(Arg, Term, Value)) :- !.
ciao_goal_expansion('$setarg'(Arg, Term, Value, true), nb_setarg(Arg, Term, Value)) :- !.
ciao_goal_expansion(instance(A, B),             subsumes_term(B, A)) :- !.
ciao_goal_expansion(varset(A, B),               term_variables(A, B)) :- !.
ciao_goal_expansion(foldl(L, S, O, R),          ciao_foldl(L, S, O, R)) :- !.
ciao_goal_expansion(attach_attribute(V, A),     put_attr(V, attributes, A)) :- !.
ciao_goal_expansion(detach_attribute(V),        del_attr(V, attributes)) :- !.
ciao_goal_expansion(update_attribute(V, A),     put_attr(V, attributes, A)) :- !.
ciao_goal_expansion(get_attribute(V, A),        get_attr(V, attributes, A)) :- !.
ciao_goal_expansion(mktemp_in_tmp(T, F),        tmp_file(T, F)) :- !.

ciao_goal_expansion(current_prolog_flag(F, V),  G) :-
	F == discontiguous_warnings,
	!,
	G = (style_check(?(discontiguous)) -> V = on ; v = off).
ciao_goal_expansion(set_prolog_flag(F, V), G) :-
	F == discontiguous_warnings,
	!,
	( V == on  -> G = style_check(+(discontiguous))
	; V == off -> G = style_check(-(discontiguous))
	).
ciao_goal_expansion(push_prolog_flag(Flag, NewValue), G) :- !,
	expand_push_prolog_flag(Flag, NewValue, G).
ciao_goal_expansion(push_ciao_flag(Flag, NewValue), G) :- !,
	expand_push_prolog_flag(Flag, NewValue, G).
ciao_goal_expansion(pop_prolog_flag(Flag), G) :- !,
	expand_pop_prolog_flag(Flag, G).
ciao_goal_expansion(pop_ciao_flag(Flag), G) :- !,
	expand_pop_prolog_flag(Flag, G).

ciao_goal_expansion(CiaoGoal, SWIGoal) :-
	\+ functor(CiaoGoal, '$ciao_meta', _),
	'$set_source_module'(M, M),
	predicate_property(M:CiaoGoal, meta_predicate(Spec)),
	swi_meta_args(Spec, CiaoGoal, SWIGoal),
	CiaoGoal \= SWIGoal.

expand_push_prolog_flag(Flag, NewValue, G) :-
	'$set_source_module'(M, M),
	G = ( nonvar(Flag),
	      prolog_flag(Flag, OldValue, NewValue),
	      asserta(ciao:old_flag(M, Flag, OldValue))).

expand_pop_prolog_flag(Flag, G) :-
	'$set_source_module'(M, M),
	G = ( nonvar(Flag),
	      once(retract(ciao:old_flag(M, Flag, OldValue))),
	      prolog_flag(Flag, _, OldValue)).

		 /*******************************
		 *	    LIBRARY SETUP	*
		 *******************************/

%%	push_ciao_library
%
%	Pushes searching for dialect/ciao in   front of every library
%	directory that contains such as sub-directory.

push_ciao_library :-
	(   absolute_file_name(library(dialect/ciao), Dir,
			       [ file_type(directory),
				 access(read),
				 solutions(all),
				 file_errors(fail)
			       ]),
	    asserta((user:file_search_path(library, Dir) :-
		    prolog_load_context(dialect, ciao))),
	    fail
	;   true
	).


:- push_ciao_library.
