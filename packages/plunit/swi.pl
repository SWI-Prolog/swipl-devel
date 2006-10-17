/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2006, University of Amsterdam

    This file is covered by the `The Artistic License', also in use by
    Perl.  See http://www.perl.com/pub/a/language/misc/Artistic.html
*/

:- module(swi,
	  [ (=@=)/2,			% @Term1, @Term2
	    forall/2,			% :Cond, :Action, 
	    ignore/1,			% :Goal
	    call/3,			% :Goal, +A1, +A2
	    source_location/2,		% -File, -Line
	    option/2,			% +Term, +List
	    option/3			% +Term, +List, +Default
	  ]).
:- meta_predicate
	forall(:,:),
	ignore(:),
	call(:,+,+).

:- use_module(library(lists)).
:- use_module(library(terms)).
:- op(700, xfx, =@=).

/** <module> SWI-Predicates for SICStus

This module defines some SWI-Prolog specific   predicates to support the
PlUnit environment in SICStus. Tested and  developed with SICStus Prolog
3.12.7.

@author		Jan Wielemaker
@license	artistic
*/

%%	=@=(A, B)
%
%	True if A is structural equivalent to  B. This means either A ==
%	B, or B is like a copy of A.

A =@= B :-
	variant(A, B).

%%	forall(:Cond, :Action) is semidet.
%
%	True if for all solutions of Cond, Action is true

forall(Cond, Action) :-
	\+ (Cond, \+ Action).

%%	ignore(:Goal)
%
%	Ignore failure of Goal.

ignore(Goal) :-
	(   Goal
	->  true
	;   true
	).

%%	call(:Goal, +A1, +A2)
%
%	Call with extended arguments

call(M:Goal, A1, A2) :-
	Goal =.. List,
	append(List, [A1,A2], List2),
	Goal2 =.. List2,
	call(M:Goal2).

%%	source_location(File, Line)
%
%	Location from where we just read the last term.

source_location(File, Line) :-
	prolog_load_context(file, File),
	prolog_load_context(term_position, Pos),
	stream_position_data(line_count, Pos, Line).


		 /*******************************
		 *	      OPTIONS		*
		 *******************************/

%%	option(?Option, +OptionList, +Default)
%
%	Get  an  option  from  a  OptionList.  OptionList  can  use  the
%	Name=Value as well as the Name(Value) convention.
%	
%	@param Option	Term of the form Name(?Value).

option(Opt, Options, Default) :-	% make option processing stead-fast
	arg(1, Opt, OptVal),
	nonvar(OptVal), !,
	functor(Opt, OptName, 1),
	functor(Gen, OptName, 1),
	option(Gen, Options, Default),
	Opt = Gen.
option(Opt, Options, _) :-
	get_option(Opt, Options), !.
option(Opt, _, Default) :-
	arg(1, Opt, Default).

%%	option(?Option, +OptionList)
%
%	Get  an  option  from  a  OptionList.  OptionList  can  use  the
%	Name=Value as well as the Name(Value) convention. Fails silently
%	if the option does not appear in OptionList.
%	
%	@param Option	Term of the form Name(?Value).

option(Opt, Options) :-	% make option processing stead-fast
	arg(1, Opt, OptVal),
	nonvar(OptVal), !,
	functor(Opt, OptName, 1),
	functor(Gen, OptName, 1),
	option(Gen, Options),
	Opt = Gen.
option(Opt, Options) :-
	get_option(Opt, Options), !.


get_option(Opt, Options) :-
	memberchk(Opt, Options), !.
get_option(Opt, Options) :-
	functor(Opt, OptName, 1),
	arg(1, Opt, OptVal),
	memberchk(OptName=OptVal, Options), !.
