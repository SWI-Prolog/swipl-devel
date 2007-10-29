/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2006, University of Amsterdam

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
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(pldoc_modes,
	  [ process_modes/6,		% +Lines, +M, +FP, -Modes, -Av, -RLines
	    store_modes/2,		% +Modes, +SourcePos
	    mode/2,			% ?:Head, -Det
	    is_mode/1,			% @Mode
	    mode_indicator/1,		% ?Atom
	    modes_to_predicate_indicators/2, % +Modes, -PIs
	    compile_clause/2		% +Term, +File:Line
	  ]).
:- use_module(library(lists)).
:- use_module(library(memfile)).
:- use_module(library(operators)).

/** <module> Analyse PlDoc mode declarations

This  module  analyzes  the  formal  part  of  the  documentation  of  a
predicate. The formal  part  is  processed   by  read_term/3  using  the
operator declarations in this module.

@author	  Jan Wielemaker
@license  GPL
*/

:- op(750, xf, ...).			% Repeated argument: Arg...
:- op(700, fx, +).			% allow +Arg
:- op(700, fx, -).			% allow -Arg
:- op(700, fx, ?).			% allow ?Arg
:- op(700, fx, :).			% allow ?Arg
:- op(700, fx, @).			% allow @Arg
:- op(700, fx, !).			% allow !Arg
:- op(200, xf, //).			% allow for Head// is det.

		 /*******************************
		 *	       MODES		*
		 *******************************/

%	TBD: Extensive tests on style and completeness

:- dynamic
	mode/3.				% ?Mode, ?Module, ?Det

%%	process_modes(+Lines:lines, +Module, +FilePos,
%%		      -Modes:list, -Args:list(atom),
%%		      -RestLines:lines) is det.
%
%	Process the formal header lines  (upto   the  first blank line),
%	returning the remaining lines and  the   names  of the arguments
%	used in the various header lines.
%	
%	@param FilePos	Term File:Line with the position of comment
%	@param Modes	List if mode(Head, Bindings) terms
%	@param Args	List of argument-names appearing in modes

process_modes(Lines, Module, FilePos, ModeDecls, Vars, RestLines) :-
	mode_lines(Lines, ModeText, [], RestLines),
	modes(ModeText, Module, FilePos, ModeDecls),
	extract_varnames(ModeDecls, Vars0, []),
	sort(Vars0, Vars).
	
%%	mode_lines(+Lines, -ModeText:codes, ?ModeTail:codes, -Lines) is det.
%
%	Extract the formal header. For %%   comments these are all lines
%	starting with %%. For /** comments these  are all lines upto the
%	first blank line.

mode_lines(Lines0, ModeText, ModeTail, Lines) :-
	percent_mode_line(Lines0, ModeText, ModeTail0, Lines1), !,
	percent_mode_lines(Lines1, ModeTail0, ModeTail, Lines).
mode_lines(Lines0, ModeText, ModeTail, Lines) :-
	non_empty_lines(Lines0, ModeText, ModeTail, Lines).

percent_mode_line([1-[0'%|L]|Lines], ModeText, ModeTail, Lines) :-	%' 
	append(L, [10|ModeTail], ModeText).

percent_mode_lines(Lines0, ModeText, ModeTail, Lines) :-
	percent_mode_line(Lines0, ModeText, ModeTail1, Lines1), !,
	percent_mode_lines(Lines1, ModeTail1, ModeTail, Lines).
percent_mode_lines(Lines, Mode, Mode, Lines).

non_empty_lines([], ModeTail, ModeTail, []).
non_empty_lines([_-[]|Lines], ModeTail, ModeTail, Lines) :- !.
non_empty_lines([_-L|Lines0], ModeText, ModeTail, Lines) :-
	append(L, [10|ModeTail0], ModeText),
	non_empty_lines(Lines0, ModeTail0, ModeTail, Lines).


%%	modes(+Text:codes, +Module, +FilePos, -ModeDecls) is det.
%
%	Read mode declaration. This consists of a number of Prolog terms
%	which may or may not be closed by  a Prolog full-stop. 
%	
%	@param Text		Input text as list of codes.
%	@param Module		Module the comment comes from
%	@param ModeDecls	List of mode(Term, Bindings)

modes(Text, Module, FilePos, Decls) :-
	prepare_module_operators(Module), 
	modes(Text, FilePos, Decls).

modes(Text, FilePos, Decls) :-
	catch(read_mode_terms(Text, FilePos, '', Decls), E, true),
	(   var(E)
	->  !
	;   E = error(syntax_error(end_of_file), _)
	->  fail
	;   !, print_message(warning, E),
	    Decls = []
	).
modes(Text, FilePos, Decls) :-
	catch(read_mode_terms(Text, FilePos, ' . ', Decls), E, true),
	(   var(E)
	->  !
	;   print_message(warning, E),
	    fail
	).
modes(_, _, []).

read_mode_terms(Text, File:Line, End, Terms) :-
	new_memory_file(MemFile),
	open_memory_file(MemFile, write, Out),
	format(Out, '~s~w', [Text, End]),
	close(Out),
	open_memory_file(MemFile, read, In),
	set_stream(In, file_name(File)),
	stream_property(In, position(Pos0)),
	set_line(Pos0, Line, Pos),
	set_stream_position(In, Pos),
	call_cleanup(read_modes(In, Terms),
		     (	 close(In),
			 free_memory_file(MemFile))).

set_line('$stream_position'(CharC, _, LinePos, ByteC),
	 Line,
	 '$stream_position'(CharC, Line, LinePos, ByteC)).

read_modes(In, Terms) :-
	read_mode_term(In, Term0),
	read_modes(Term0, In, Terms).

read_modes(mode(end_of_file,[]), _, []) :- !.
read_modes(T0, In, [T0|Rest]) :-
	read_mode_term(In, T1),
	read_modes(T1, In, Rest).

read_mode_term(In, mode(Term, Bindings)) :-
	read_term(In, Term,
		  [ variable_names(Bindings),
		    module(pldoc_modes)
		  ]).


%%	prepare_module_operators is det.
%
%	Import operators from current source module.

:- dynamic
	prepared_module/2.

prepare_module_operators(Module) :-
	(   prepared_module(Module, _)
	->  true
	;   unprepare_module_operators,
	    public_operators(Module, Ops),
	    (	Ops \== []
	    ->	push_operators(Ops, Undo),
		asserta(prepared_module(Module, Undo))
	    ;	true
	    )
	).

unprepare_module_operators :-
	forall(retract(prepared_module(_, Undo)),
	       pop_operators(Undo)).
	

%%	public_operators(+Module, -List:list(op(Pri,Assoc,Name))) is det.
%
%	List is the list of operators exported from Module through its
%	module header.
%	
%	@tbd	Must be provided by a public interface from the core

public_operators(Module, List) :-
	Goal = Module:'$exported_op'(P,A,N),
	predicate_property(Goal, _), !,
	findall(op(P,A,N), Module:'$exported_op'(P,A,N), List).
public_operators(_, []).


%%	extract_varnames(+Bindings, -VarNames, ?VarTail) is det.
%
%	Extract the variables names.
%	
%	@param Bindings		Nested list of Name=Var
%	@param VarNames		List of variable names
%	@param VarTail		Tail of VarNames

extract_varnames([], VN, VN) :- !.
extract_varnames([H|T], VN0, VN) :- !,
	extract_varnames(H, VN0, VN1),
	extract_varnames(T, VN1, VN).
extract_varnames(mode(_, Bindings), VN0, VN) :- !,
	extract_varnames(Bindings, VN0, VN).
extract_varnames(Name=_, [Name|VN], VN).

%%	store_modes(+Modes, +SourcePos) is det.
%
%	Assert modes into the database with the given position.
%	
%	@param Modes	  List if mode-terms.  See process_modes/5.
%	@param SourcePos  Term File:Line

store_modes([], _).
store_modes([mode(Mode, _Bindings)|T], Pos) :-
	store_mode(Mode, Pos),
	store_modes(T, Pos).

store_mode(Head0 is Det, Pos) :- !,
	dcg_expand(Head0, Head),
	compile_clause('$mode'(Head, Det), Pos).
store_mode(Head0, Pos) :-
	dcg_expand(Head0, Head),
	compile_clause('$mode'(Head, unknown), Pos).
	
dcg_expand(//(Head0), Head) :- !,
	Head0 =.. [Name|List0],
	maplist(remove_argname, List0, List1),
	append(List1, [?list, ?list], List2),
	Head =.. [Name|List2].
dcg_expand(Head0, Head) :-
	remove_argnames(Head0, Head).

remove_argnames(Head0, Head) :-
	functor(Head0, Name, Arity),
	functor(Head, Name, Arity),
	remove_argnames(0, Arity, Head0, Head).

remove_argnames(Arity, Arity, _, _) :- !.
remove_argnames(I0, Arity, H0, H) :-
	I is I0 + 1,
	arg(I, H0, A0),
	remove_argname(A0, A),
	arg(I, H, A),
	remove_argnames(I, Arity, H0, H).

remove_argname(T, ?(any)) :-
	var(T), !.
remove_argname(T0..., T...) :- !,
	remove_argname(T0, T).
remove_argname(A0, A) :-
	mode_ind(A0, M, A1), !,
	remove_aname(A1, A2),
	mode_ind(A, M, A2).
remove_argname(A0, ?A) :-
	remove_aname(A0, A).

remove_aname(Var, any) :-
	var(Var), !.
remove_aname(_:Type, Type) :- !.


%%	mode(:Head, ?Det) is nondet.
%
%	True if there is a mode-declaration for Head with Det.
%	
%	@param	Head	Callable term.  Arguments are a mode-indicator
%			followed by a type.
%	@param  Det	One of =unknown=, =det=, =semidet=, or =nondet=.

:- module_transparent
	mode/2.

mode(Head, Det) :-
	var(Head), !,
	current_module(M),
	'$c_current_predicate'(_, M:'$mode'(_,_)),
	M:'$mode'(H,Det),
	qualify(M,H,Head).
mode(M:Head, Det) :-
	current_module(M),
	'$c_current_predicate'(_, M:'$mode'(_,_)),
	M:'$mode'(Head,Det).

qualify(system, H, H) :- !.
qualify(user,   H, H) :- !.
qualify(M,      H, M:H).


%%	is_mode(@Head) is semidet.
%
%	True if Head is a valid mode-term.

is_mode(Var) :-
	var(Var), !, fail.
is_mode(Head is Det) :-
	is_det(Det),
	is_head(Head).

is_det(Var) :-
	var(Var), !, fail.
is_det(det).
is_det(semidet).
is_det(nondet).
is_det(multi).

is_head(Var) :-
	var(Var), !, fail.
is_head(Head//) :- !,
	is_head(Head).
is_head(Head) :-
	callable(Head),
	functor(Head, _Name, Arity),
	is_head_args(0, Arity, Head).

is_head_args(A, A, _) :- !.
is_head_args(I0, Arity, Head) :-
	I is I0 + 1,
	arg(I, Head, Arg),
	is_head_arg(Arg),
	is_head_args(I, Arity, Head).

is_head_arg(Arg) :-
	var(Arg), !.
is_head_arg(Arg) :-
	Arg =.. [Ind,Arg1],
	mode_indicator(Ind),
	is_head_arg(Arg1).
is_head_arg(Arg:Type) :-
	var(Arg),
	is_type(Type).

is_type(Type) :-
	callable(Type).

%%	mode_indicator(?Ind:atom) is nondet.
%
%	Our defined argument-mode indicators

mode_indicator(+).			% Instantiated to type
mode_indicator(-).			% Unbound
mode_indicator(?).			% Partially instantiated to type
mode_indicator(:).			% Meta-argument (implies +)
mode_indicator(@).			% Not instantiated by pred
mode_indicator(!).			% Mutable term

mode_ind(+(X), +, X).
mode_ind(-(X), -, X).
mode_ind(?(X), ?, X).
mode_ind(:(X), :, X).
mode_ind(@(X), @, X).
mode_ind(!(X), !, X).


%%	modes_to_predicate_indicators(+Modes:list, -PI:list) is det.
%	
%	Create a list of predicate indicators represented by Modes. Each
%	predicate indicator is  of  the   form  atom/integer  for normal
%	predicates or atom//integer for DCG rules.
%	
%	@param Modes	Mode-list as produced by process_modes/5
%	@param PI	List of Name/Arity or Name//Arity without duplicates

modes_to_predicate_indicators(Modes, PIs) :-
	modes_to_predicate_indicators2(Modes, PIs0),
	list_to_set(PIs0, PIs).

modes_to_predicate_indicators2([], []).
modes_to_predicate_indicators2([mode(H,_B)|T0], [PI|T]) :-
	mode_to_pi(H, PI),
	modes_to_predicate_indicators2(T0, T).

mode_to_pi(Head is _Det, PI) :- !,
	head_to_pi(Head, PI).
mode_to_pi(Head, PI) :-
	head_to_pi(Head, PI).

head_to_pi(//(Head), Name//Arity) :- !,
	functor(Head, Name, Arity).
head_to_pi(Head, Name/Arity) :-
	functor(Head, Name, Arity).

%%	compile_clause(+Term, +FilePos) is det.
%	
%	Add a clause to the  compiled   program.  Unlike  assert/1, this
%	associates the clause with the   given source-location, makes it
%	static code and removes the  clause   of  the  file is reloaded.
%	Finally,  as  we  create  clauses   one-by-one,  we  define  our
%	predicates as discontiguous.
%	
%	@param Term	Clause-term
%	@param FilePos	Term of the form File:Line, where File is a
%			canonical filename.

compile_clause(Term, FilePos) :-
	'$set_source_module'(SM, SM),
	clause_head(Term, Head),
	functor(Head, Name, Arity),
	(   SM == user
	->  multifile(SM:(Name/Arity))
	;   discontiguous(SM:(Name/Arity))
	),
	'$record_clause'(Term, FilePos, _Ref).

clause_head((Head :- _Body), Head) :- !.
clause_head(Head, Head).
