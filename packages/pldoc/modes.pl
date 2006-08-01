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
	  [ process_modes/4,		% +Lines, -Modes, -Args, -RestLines
	    assert_modes/1,		% +Modes
	    mode/2,			% ?:Head, -Det
	    is_mode/1,			% @Mode
	    mode_indicator/1		% ?Atom
	  ]).
:- use_module(library(lists)).
:- use_module(library(memfile)).

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

%%	process_modes(+Lines:lines, -Modes:list, -Args:list(atom),
%%		      -RestLines:lines) is det.
%
%	Process the formal header lines  (upto   the  first blank line),
%	returning the remaining lines and  the   names  of the arguments
%	used in the various header lines.
%	
%	@param Modes	List if mode(Head, Bindings) terms

process_modes(Lines, ModeDecls, Vars, RestLines) :-
	mode_lines(Lines, ModeText, [], RestLines),
	modes(ModeText, ModeDecls),
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


%%	modes(+Text:codes, -ModeDecls) is det.
%
%	Read mode declaration. This consists of a number of Prolog terms
%	which may or may not be closed by  a Prolog full-stop. 
%	
%	@param Text		Input text as list of codes.
%	@param ModeDecls	List of mode(Term, Bindings)

modes(Text, Decls) :-
	catch(read_mode_terms(Text, '', Decls), E, true),
	(   var(E)
	->  !
	;   E = error(syntax_error(end_of_file), _)
	->  fail
	;   !, print_message(warning, E),	% TBD: location!
	    Decls = []
	).
modes(Text, Decls) :-
	catch(read_mode_terms(Text, ' . ', Decls), E, true),
	(   var(E)
	->  !
	;   print_message(warning, E),
	    fail
	).
modes(_, []).

read_mode_terms(Text, End, Terms) :-
	new_memory_file(File),
	open_memory_file(File, write, Out),
	format(Out, '~s~w', [Text, End]),
	close(Out),
	open_memory_file(File, read, In),
	call_cleanup(read_modes(In, Terms),
		     (	 close(In),
			 free_memory_file(File))).

read_modes(In, Terms) :-
	read_mode_term(In, Term0),
	read_modes(Term0, In, Terms).

read_modes(mode(end_of_file,[]), _, []) :- !.
read_modes(T0, In, [T0|Rest]) :-
	read_mode_term(In, T1),
	read_modes(T1, In, Rest).

read_mode_term(In, mode(Term, Bindigns)) :-
	read_term(In, Term,
		  [ variable_names(Bindigns),
		    module(pldoc_modes)
		  ]).

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

%%	assert_modes(+Modes) is det.

assert_modes(Modes) :-
	maplist(retract_old_mode, Modes),
	maplist(assert_mode, Modes).

retract_old_mode(Spec is _) :- !,
	retract_old_mode(Spec).
retract_old_mode(Spec) :-
	functor(Spec, Name, Arity),
	functor(Gen, Name, Arity),
	prolog_load_context(module, M),
	retractall(mode(Gen, M, _)).

assert_mode(Spec is Det) :- !,
	prolog_load_context(module, M),
	assert(mode(Spec, M, Det)).
assert_mode(Spec) :-
	assert_mode(Spec is unknown).

%%	mode(:Head, ?Determinism) is nondet.

mode(Module:Head, Det) :- !,
	mode(Head, Module, Det).
mode(Spec, Det) :-
	strip_module(Spec, Module, Head),
	mode(Head, Module, Det).

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

