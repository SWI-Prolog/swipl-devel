/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2005, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/


:- module(prolog_clause,
	  [ clause_info/4,		% +ClauseRef, -File, -TermPos, -VarNames
	    predicate_name/2,		% +Head, -Name
	    clause_name/2		% +ClauseRef, -Name
	  ]).
:- use_module(library(debug), [debug/3]).
:- use_module(library(lists), [append/3]).
:- use_module(library(occurs), [sub_term/2]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module started life as part of the   GUI tracer. As it is generally
useful for debugging  purposes  it  has   moved  to  the  general Prolog
library. Being only applicable to debugging   and  sitting very close to
the kernel, do not rely too much that its functionality will be stable.

The tracer library  tracer/clause.pl  adds   caching  and  dealing  with
dynamic predicates using listing to  XPCE   objects  to  this. Note that
clause_info/4 as below can be pretty slow.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%%	clause_info(+ClauseRef, -File, -TermPos, -VarNames)
%
%	Fetches source information for the given clause.

clause_info(ClauseRef, File, TermPos, NameOffset) :-
	debug(clause_info, 'clause_info(~w)... ', [ClauseRef]),
	clause_property(ClauseRef, file(File)),
	'$clause'(Head, Body, ClauseRef, VarOffset),
	(   Body == true
	->  DecompiledClause = Head
	;   DecompiledClause = (Head :- Body)
	),
	File \== user,			% loaded using ?- [user].
	clause_property(ClauseRef, line_count(LineNo)),
	(   current_module(Module, File)
	->  true
	;   strip_module(user:Head, Module, _)
	),
	debug(clause_info, 'from ~w:~d ... ', [File, LineNo]),
	read_term_at_line(File, LineNo, Module, Clause, TermPos0, VarNames),
	debug(clause_info, 'read ...', []),
	unify_clause(Clause, DecompiledClause, TermPos0, TermPos),
	debug(clause_info, 'unified ...', []),
	make_varnames(Clause, VarOffset, VarNames, NameOffset),
	debug(clause_info, 'got names~n', []), !.

%%	unify_term(+T1, +T2)
%	
%	Unify the two terms, where T2 is created by writing the term and
%	reading it back in, but  be   aware  that  rounding problems may
%	cause floating point numbers not to  unify. Also, if the initial
%	term has a string object, it is written   as "..." and read as a
%	code-list. We compensate for that.
%	
%	NOTE: Called directly from  library(trace/clause)   for  the GUI
%	tracer.

unify_term(X, X) :- !.
unify_term(X1, X2) :-
	compound(X1),
	compound(X2),
	functor(X1, F, Arity),
	functor(X2, F, Arity), !,
	unify_args(0, Arity, X1, X2).
unify_term(X, Y) :-
	float(X), float(Y), !.
unify_term(X, Y) :-
	string(X),
	is_list(Y),
	string_to_list(X, Y), !.
unify_term(_:X, Y) :-
	unify_term(X, Y), !.
unify_term(X, _:Y) :-
	unify_term(X, Y), !.
unify_term(X, Y) :-
	format('[INTERNAL ERROR: Diff:~n'),
	portray_clause(X),
	format('~N*** <->~n'),
	portray_clause(Y),
	break.

unify_args(N, N, _, _) :- !.
unify_args(I, Arity, T1, T2) :-
	A is I + 1,
	arg(A, T1, A1),
	arg(A, T2, A2),
	unify_term(A1, A2),
	unify_args(A, Arity, T1, T2).


%	Must be a user-programmable hook!

alternate_syntax(prolog, _,    true,
			       true).
alternate_syntax(pce_class, M, pce_expansion:push_compile_operators(M),
			       pce_expansion:pop_compile_operators) :-
	current_prolog_flag(xpce, true).
alternate_syntax(system, _,    style_check(+dollar),
			       style_check(-dollar)).

system_module(system) :- !.
system_module(Module) :-
	sub_atom(Module, 0, _, _, $), !.

read_term_at_line(File, Line, Module, Clause, TermPos, VarNames) :-
	catch(open(File, read, In), _, fail),
	call_cleanup(read(Line, In, Module, Clause, TermPos, VarNames),
		     close(In)).

read(Line, Handle, Module, Clause, TermPos, VarNames) :-
	seek_to_line(Handle, Line),
	read(Handle, Module, Clause, TermPos, VarNames).

%%	read(+Stream, +Module, -Clause, -TermPos, -VarNames)
%	
%	Read clause from Stream at current position with unknown syntax.
%	It returns the term read  at  that   position,  as  well  as the
%	subterm position info and variable names.
%	
%	NOTE: Called directly from  library(trace/clause)   for  the GUI
%	tracer.

read(Handle, Module, Clause, TermPos, VarNames) :-
	(   system_module(Module)
	->  Syntax = system
	;   true
	),
	stream_property(Handle, position(Here)),
	alternate_syntax(Syntax, Module, Setup, Restore),
	peek_char(Handle, X),
	debug(clause_info, 'Using syntax ~w (c=~w)', [Syntax, X]),
	Setup,
	catch(read_term(Handle, Clause,
			[ subterm_positions(TermPos),
			  variable_names(VarNames),
			  module(Module)
			]),
	      Error,
	      true),
	Restore,
	(   var(Error)
	->  !
	;   set_stream_position(Handle, Here),
	    fail
	).
	

%%	make_varnames(+ReadClause, +Offsets, +Names, -Term) is det.
%	
%	Create a Term varnames(...) where each argument contains the name
%	of the variable at that offset.  If the read Clause is a DCG rule,
%	name the two last arguments <DCG_list> and <DCG_tail>
%	
%	@param Offsets	List of Offset=Var
%	@param Names	List of Name=Var
%
%	@bug Called directly from library(trace/clause) for the GUI tracer.

make_varnames((Head --> _Body), Offsets, Names, Bindings) :- !,
	functor(Head, _, Arity),
	In is Arity,
	memberchk(In=IVar, Offsets),
	Names1 = ['<DCG_list>'=IVar|Names],
	Out is Arity + 1,
	memberchk(Out=OVar, Offsets),
	Names2 = ['<DCG_tail>'=OVar|Names1],
	make_varnames(xx, Offsets, Names2, Bindings).
make_varnames(_, Offsets, Names, Bindings) :-
	length(Offsets, L),
	functor(Bindings, varnames, L),
	do_make_varnames(Offsets, Names, Bindings).

do_make_varnames([], _, _).
do_make_varnames([N=Var|TO], Names, Bindings) :-
	(   find_varname(Var, Names, Name)
	->  true
	;   Name = '_'
	),
	AN is N + 1,
	arg(AN, Bindings, Name),
	do_make_varnames(TO, Names, Bindings).

find_varname(Var, [Name = TheVar|_], Name) :-
	Var == TheVar, !.
find_varname(Var, [_|T], Name) :-
	find_varname(Var, T, Name).

%%	unify_clause(+Read, +Decompiled, +ReadTermPos, -RecompiledTermPos).
%	
%	What you read isn't always what goes into the database. The task
%	of this predicate is to establish the relation between the term
%	read from the file and the result from decompiling the clause.
%	
%	This really must be more flexible, dealing with much more
%	complex source-translations, falling back to a heristic method
%	locating as much as possible.

unify_clause(Read, Read, TermPos, TermPos) :- !.
					% XPCE send-methods 
unify_clause(:->(Head, Body), (PlHead :- PlBody), TermPos0, TermPos) :- !,
	pce_method_clause(Head, Body, PlHead, PlBody, TermPos0, TermPos).
					% XPCE get-methods
unify_clause(:<-(Head, Body), (PlHead :- PlBody), TermPos0, TermPos) :- !,
	pce_method_clause(Head, Body, PlHead, PlBody, TermPos0, TermPos).
					% Unit test clauses
unify_clause((TH :- Body),
	     (_:'unit body'(_, _) :- !, Body),
	     TP0, TP) :-
	(   TH = test(_,_)
	;   TH = test(_)
	), !,
	TP0 = term_position(F,T,FF,FT,[HP,BP]),
	TP  = term_position(F,T,FF,FT,[HP,term_position(0,0,0,0,[FF-FT,BP])]).
					% module:head :- body
unify_clause((Head :- Read),
	     (Head :- _M:Compiled), TermPos0, TermPos) :-
	unify_clause((Head :- Read), (Head :- Compiled), TermPos0, TermPos1),
	TermPos1 = term_position(TA,TZ,FA,FZ,[PH,PB]),
	TermPos  = term_position(TA,TZ,FA,FZ,
				 [ PH,
				   term_position(0,0,0,0,[0-0,PB])
				 ]).
unify_clause(Read, Compiled1, TermPos0, TermPos) :-
	Read = (_ --> List, _),
	is_list(List),
	catch(expand_term(Read, Compiled2), E, expand_failed(E, Read)),
	Compiled2 = (DH :- _),
	functor(DH, _, Arity),
	DArg is Arity - 1,
	arg(DArg, DH, List),
	nonvar(List),
	TermPos0 = term_position(F,T,FF,FT,[ HP,
					     term_position(_,_,_,_,[_,BP])
					   ]), !,
	TermPos1 = term_position(F,T,FF,FT,[ HP, BP ]),
	match_module(Compiled2, Compiled1, TermPos1, TermPos).
					% general term-expansion
unify_clause(Read, Compiled1, TermPos0, TermPos) :-
	catch(expand_term(Read, Compiled2), E, expand_failed(E, Read)),
	match_module(Compiled2, Compiled1, TermPos0, TermPos).
					% I don't know ...
unify_clause(_, _, _, _) :-
	debug(clause_info, 'Could not unify clause', []),
	fail.

unify_clause_head(H1, H2) :-
	strip_module(H1, _, H),
	strip_module(H2, _, H).

match_module((H1 :- B1), (H2 :- B2), Pos0, Pos) :- !,
	unify_clause_head(H1, H2),
	unify_body(B1, B2, Pos0, Pos).
match_module(H1, H2, Pos, Pos) :-	% deal with facts
	unify_clause_head(H1, H2).

%%	expand_failed(+Exception, +Term)
%
%	When debugging, indicate that expansion of the term failed.

expand_failed(E, Read) :-
	debugging(clause_info),
	message_to_string(E, Msg),
	debug(clause_info, 'Term-expand ~p failed: ~w', [Read, Msg]),
	fail.

%%	unify_body(+Read, +Decompiled, +Pos0, -Pos)
%	
%	Deal with translations implied by the compiler.  For example,
%	compiling (a,b),c yields the same code as compiling a,b,c.
%	
%	Pos0 and Pos still include the term-position of the head.

unify_body(B, B, Pos, Pos) :-
	\+ sub_term(brace_term_position(_,_,_), Pos), !.
unify_body(R, D,
	   term_position(F,T,FF,FT,[HP,BP0]),
	   term_position(F,T,FF,FT,[HP,BP])) :-
	ubody(R, D, BP0, BP).
	   
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Some remarks.

a --> { x, y, z }.
    This is translated into "(x,y),z), X=Y" by the DCG translator, after
    which the compiler creates "a(X,Y) :- x, y, z, X=Y".
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%%	ubody(+Read, +Decompiled, +TermPosRead, -TermPosForDecompiled)
%
%	@param Read		Clause read _after_ expand_term/2
%	@param Decompiled	Decompiled clause
%	@param TermPosRead	Sub-term positions of source

ubody(B, B, P, P) :-
	\+ sub_term(brace_term_position(_,_,_), P), !.
ubody(B0, B,
      brace_term_position(F,T,A0),
      Pos) :-
	B0 = (_,_=_), !,
	T1 is T - 1,
	ubody(B0, B,
	      term_position(F,T,
			    F,T,
			    [A0,T1-T]),
	      Pos).
ubody(B0, B,
      brace_term_position(F,T,A0),
      term_position(F,T,F,T,[A])) :- !,
	ubody(B0, B, A0, A).
ubody(C0, C, P0, P) :-
	nonvar(C0), nonvar(C),
	C0 = (_,_), C = (_,_), !,
	conj(C0, P0, GL, PL),
	mkconj(C, P, GL, PL).
ubody(X0, X,
      term_position(F,T,FF,TT,PA0),
      term_position(F,T,FF,TT,PA)) :-
	meta(X0), !,
	X0 =.. [_|A0],
	X  =.. [_|A],
	ubody_list(A0, A, PA0, PA).
	
ubody_list([], [], [], []).
ubody_list([G0|T0], [G|T], [PA0|PAT0], [PA|PAT]) :-
	ubody(G0, G, PA0, PA),
	ubody_list(T0, T, PAT0, PAT).


conj(Goal, Pos, GoalList, PosList) :-
	conj(Goal, Pos, GoalList, [], PosList, []).

conj((A,B), term_position(_,_,_,_,[PA,PB]), GL, TG, PL, TP) :- !,
	conj(A, PA, GL, TGA, PL, TPA),
	conj(B, PB, TGA, TG, TPA, TP).
conj((A,B), brace_term_position(_,T,PA), GL, TG, PL, TP) :-
	B = (_=_), !,
	conj(A, PA, GL, TGA, PL, TPA),
	T1 is T - 1,
	conj(B, T1-T, TGA, TG, TPA, TP).
conj(A, P, [A|TG], TG, [P|TP], TP).


mkconj(Goal, Pos, GoalList, PosList) :-
	mkconj(Goal, Pos, GoalList, [], PosList, []).

mkconj(Conj, term_position(0,0,0,0,[PA,PB]), GL, TG, PL, TP) :-
	nonvar(Conj),
	Conj = (A,B), !,
	mkconj(A, PA, GL, TGA, PL, TPA),
	mkconj(B, PB, TGA, TG, TPA, TP).
mkconj(A0, P0, [A|TG], TG, [P|TP], TP) :-
	ubody(A, A0, P, P0).


		 /*******************************
		 *    PCE STUFF (SHOULD MOVE)	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	<method>(Receiver, ... Arg ...) :->
		Body

mapped to:

	send_implementation(Id, <method>(...Arg...), Receiver)

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

pce_method_clause(Head, Body, _:PlHead, PlBody, TermPos0, TermPos) :- !,
	pce_method_clause(Head, Body, PlBody, PlHead, TermPos0, TermPos).
pce_method_clause(Head, Body,
		  send_implementation(_Id, Msg, Receiver), PlBody,
		  TermPos0, TermPos) :- !,
	debug(clause_info, 'send method ...', []),
	arg(1, Head, Receiver),
	functor(Head, _, Arity),
	pce_method_head_arguments(2, Arity, Head, Msg),
	debug(clause_info, 'head ...', []),
	pce_method_body(Body, PlBody, TermPos0, TermPos).
pce_method_clause(Head, Body,
		  get_implementation(_Id, Msg, Receiver, Result), PlBody,
		  TermPos0, TermPos) :- !,
	debug(clause_info, 'get method ...', []),
	arg(1, Head, Receiver),
	debug(clause_info, 'receiver ...', []),
	functor(Head, _, Arity),
	arg(Arity, Head, PceResult),
	debug(clause_info, '~w?~n', [PceResult = Result]),
	pce_unify_head_arg(PceResult, Result),
	Ar is Arity - 1,
	pce_method_head_arguments(2, Ar, Head, Msg),
	debug(clause_info, 'head ...', []),
	pce_method_body(Body, PlBody, TermPos0, TermPos).

pce_method_head_arguments(N, Arity, Head, Msg) :-
	N =< Arity, !,
	arg(N, Head, PceArg),
	PLN is N - 1,
	arg(PLN, Msg, PlArg),
	pce_unify_head_arg(PceArg, PlArg),
	debug(clause_info, '~w~n', [PceArg = PlArg]),
	NextArg is N+1,
	pce_method_head_arguments(NextArg, Arity, Head, Msg).
pce_method_head_arguments(_, _, _, _).

pce_unify_head_arg(V, A) :-
	var(V), !,
	V = A.
pce_unify_head_arg(A:_=_, A) :- !.
pce_unify_head_arg(A:_, A).

%	pce_method_body(+SrcBody, +DbBody, +TermPos0, -TermPos
%
%	Unify the body of an XPCE method.  Goal-expansion makes this
%       rather tricky, especially as we cannot call XPCE's expansion
%	on an isolated method.
%
%	TermPos0 is the term-position term of the whole clause!
%	
%	Further, please note that the body of the method-clauses reside
%	in another module than pce_principal, and therefore the body
%	starts with an I_CONTEXT call. This implies we need a
%	hypothetical term-position for the module-qualifier.

pce_method_body(A0, A, TermPos0, TermPos) :-
	TermPos0 = term_position(F, T, FF, FT,
				 [ HeadPos,
				   BodyPos0
				 ]),
	TermPos  = term_position(F, T, FF, FT,
				 [ HeadPos,
				   term_position(0,0,0,0, [0-0,BodyPos])
				 ]),
	pce_method_body2(A0, A, BodyPos0, BodyPos).


pce_method_body2(::(_,A0), A, TermPos0, TermPos) :- !,
	TermPos0 = term_position(_, _, _, _, [_Cmt,BodyPos0]),
	TermPos  = BodyPos,
	expand_goal(A0, A, BodyPos0, BodyPos).
pce_method_body2(A0, A, TermPos0, TermPos) :-
	A0 =.. [Func,B0,C0],
	control_op(Func), !,
	A =.. [Func,B,C],
	TermPos0 = term_position(F, T, FF, FT,
				 [ BP0,
				   CP0
				 ]),
	TermPos  = term_position(F, T, FF, FT,
				 [ BP,
				   CP
				 ]),
	pce_method_body2(B0, B, BP0, BP),
	expand_goal(C0, C, CP0, CP).
pce_method_body2(A0, A, TermPos0, TermPos) :-
	expand_goal(A0, A, TermPos0, TermPos).

control_op((,)).
control_op((;)).
control_op((->)).
control_op((*->)).

		 /*******************************
		 *     EXPAND_GOAL SUPPORT	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
With the introduction of expand_goal, it  is increasingly hard to relate
the clause from the database to the actual  source. For one thing, we do
not know the compilation  module  of  the   clause  (unless  we  want to
decompile it).

Goal expansion can translate  goals   into  control-constructs, multiple
clauses, or delete a subgoal.

To keep track of the source-locations, we   have to redo the analysis of
the clause as defined in init.pl
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

expand_goal(G, G, P, P) :-
        var(G), !.
expand_goal(M0, M, P0, P) :-
	meta(M0), !,
	P0 = term_position(F,T,FF,FT,PL0),
	P  = term_position(F,T,FF,FT,PL),
	functor(M0, Functor, Arity),
	functor(M,  Functor, Arity),
	expand_meta_args(PL0, PL, 1, M0, M).
expand_goal(A, B, P0, P) :-
        goal_expansion(A, B0, P0, P1), !,
	expand_goal(B0, B, P1, P).
expand_goal(A, A, P, P).

expand_meta_args([], [], _, _, _).
expand_meta_args([P0|T0], [P|T], I, M0, M) :-
	arg(I, M0, A0),
	arg(I, M,  A),
	expand_goal(A0, A, P0, P),
	NI is I + 1,
	expand_meta_args(T0, T, NI, M0, M).

meta((_  ,  _)).
meta((_  ;  _)).
meta((_  -> _)).
meta((_ *-> _)).
meta((\+ _)).
meta((not(_))).
meta((call(_))).
meta((once(_))).
meta((ignore(_))).
meta((forall(_, _))).

goal_expansion(send(R, Msg), send_class(R, _, SuperMsg), P, P) :-
	compound(Msg),
	Msg =.. [send_super, Selector | Args], !,
	SuperMsg =.. [Selector|Args].
goal_expansion(get(R, Msg, A), get_class(R, _, SuperMsg, A), P, P) :-
	compound(Msg),
	Msg =.. [get_super, Selector | Args], !,
	SuperMsg =.. [Selector|Args].
goal_expansion(send_super(R, Msg), send_class(R, _, Msg), P, P).
goal_expansion(get_super(R, Msg, V), get_class(R, _, Msg, V), P, P).
goal_expansion(SendSuperN, send_class(R, _, Msg), P, P) :-
	compound(SendSuperN),
	SendSuperN =.. [send_super, R, Sel | Args],
	Msg =.. [Sel|Args].
goal_expansion(GetSuperN, get_class(R, _, Msg, Answer), P, P) :-
	compound(GetSuperN),
	GetSuperN =.. [get_super, R, Sel | AllArgs],
	append(Args, [Answer], AllArgs),
	Msg =.. [Sel|Args].
goal_expansion(G0, G, P, P) :-
	user:goal_expansion(G0, G),	% TBD: we need the module!
	G0 \== G.			% \=@=?

		 /*******************************
		 *	  PRINTABLE NAMES	*
		 *******************************/

:- module_transparent
	predicate_name/2.
:- multifile
	user:prolog_predicate_name/2,
	user:prolog_clause_name/2.

hidden_module(user).
hidden_module(system).
hidden_module(pce_principal).		% should be config
hidden_module(Module) :-		% SWI-Prolog specific
	import_module(Module, system).

thaffix(1, st) :- !.
thaffix(2, nd) :- !.
thaffix(_, th).

predicate_name(Predicate, PName) :-
	strip_module(Predicate, Module, Head),
	(   user:prolog_predicate_name(Module:Head, PName)
	->  true
	;   functor(Head, Name, Arity),
	    (   hidden_module(Module)
	    ->  sformat(PName, '~q/~d', [Name, Arity])
	    ;   sformat(PName, '~q:~q/~d', [Module, Name, Arity])
	    )
	).

%%	clause_name(+Ref, -Name)
%
%	Provide a suitable description of the indicated clause.

clause_name(Ref, Name) :-
	user:prolog_clause_name(Ref, Name), !.
clause_name(Ref, Name) :-
	nth_clause(Head, N, Ref), !,
	predicate_name(Head, PredName),
	thaffix(N, Th),
	sformat(Name, '~d-~w clause of ~w', [N, Th, PredName]).
clause_name(_, '<meta-call>').


		 /*******************************
		 *        LOW-LEVEL STUFF	*
		 *******************************/
	
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
These predicates communicate about lines.  We   should  consider using a
line-cache for this for speed.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%%	seek_to_line(+Stream, +Line)
%
%	Seek to indicated line-number.

seek_to_line(Fd, N) :-
	N > 1, !,
	skip(Fd, 10),
	NN is N - 1,
	seek_to_line(Fd, NN).
seek_to_line(_, _).
