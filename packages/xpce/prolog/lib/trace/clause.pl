/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (C): 1985-2002, University of Amsterdam

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
	  [ clause_info/4,		% +ClaseRef, -File, -TermPos, -VarNames
	    clear_clause_info_cache/0,

	    predicate_name/2,		% +Head, -Name
	    predicate_classification/2,	% +Goal, -Classification
	    clause_name/2		% +ClauseRef, -Name
	  ]).


:- pce_global(@dynamic_source_buffer,
	      new(emacs_buffer(@nil, '*dynamic code*'))).

debug(_, _).
%debug(Fmt, Args) :- format(Fmt, Args), flush.

		 /*******************************
		 *	       CACHE		*
		 *******************************/

:- dynamic
	clause_info_cache/4.
:- multifile
	user:prolog_event_hook/1.

user:prolog_event_hook(erased(Ref)) :-
	retract(clause_info_cache(Ref, _, _, _)),
	debug('Retracted info for ~d~n', [Ref]),
	fail.				% allow other hooks

clear_clause_info_cache :-
	retractall(clause_info_cache(_, _, _, _)).

%	clause_info(+ClaseRef, -File, -TermPos, -VarNames)
%
%	Fetches source information for the given clause.

clause_info(ClauseRef, File, TermPos, NameOffset) :-
	clause_info_cache(ClauseRef, File, TermPos, NameOffset), !.
clause_info(ClauseRef, File, TermPos, NameOffset) :-
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
	debug('Clause ~w from ~w:~d ...', [ClauseRef, File, LineNo]),
	open(File, read, Handle),
	seek_to_line(Handle, LineNo),
	(   read(Handle, Module, Clause, TermPos0, VarNames)
	->  close(Handle)
	;   close(Handle),
	    fail
	),
	debug('read ...', []),
	unify_clause(Clause, DecompiledClause, TermPos0, TermPos),
	debug('unified ...', []),
	make_varnames(VarOffset, VarNames, NameOffset),
	debug('got names~n', []), !,
	asserta(clause_info_cache(ClauseRef, File, TermPos, NameOffset)),
	debug('Added to info-cache~n', []).
clause_info(ClauseRef, S, TermPos, NameOffset) :-
	'$clause'(Head, Body, ClauseRef, VarOffset),
	(   Body == true
	->  Clause = Head
	;   Clause = (Head :- Body)
	),
	S = @dynamic_source_buffer,
	clause_name(ClauseRef, ClauseName),
	send(S, attribute, comment,
	     string('Decompiled listing of %s', ClauseName)),
	send(S, clear),
	debug('Writing clause ~w to string ~p ... ', [ClauseRef, S]),
	pce_open(S, write, Fd),
	telling(Old), set_output(Fd),
	portray_clause(Clause),
	tell(Old),
	close(Fd),
	debug('ok, reading ... ', []),
	pce_open(S, read, Handle),
	read(Handle, user, Clause, TermPos, VarNames),
	close(Handle),
	debug('ok ...', []),
	make_varnames(VarOffset, VarNames, NameOffset),
	debug('got names~n', []), !.
clause_info(_, _, _, _) :-
	debug('FAILED~n', []),
	fail.


%	Must be a user-programmable hook!

alternate_syntax(prolog,    true,
			    true).
alternate_syntax(pce_class, pce_expansion:push_compile_operators,
			    pce_expansion:pop_compile_operators).
alternate_syntax(system,    style_check(+dollar),
			    style_check(-dollar)).

system_module(system) :- !.
system_module(Module) :-
	sub_atom(Module, 0, _, _, $), !.

read(Handle, Module, Clause, TermPos, VarNames) :-
	(   system_module(Module)
	->  Syntax = system
	;   true
	),
	alternate_syntax(Syntax, Setup, Restore),
	Setup,
	seek(Handle, 0, current, Here),
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
	;   seek(Handle, Here, bof, _),
	    fail
	).
	
make_varnames(Offsets, Names, Bindings) :-
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

%	unify_clause(+Read, +Decompiled, +ReadTermPos, -RecompiledTermPos).
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
					% module:head :- body
unify_clause((Head :- Read),
	     (Head :- _M:Compiled), TermPos0, TermPos) :-
	unify_clause((Head :- Read), (Head :- Compiled), TermPos0, TermPos1),
	TermPos1 = term_position(TA,TZ,FA,FZ,[PH,PB]),
	TermPos  = term_position(TA,TZ,FA,FZ,
				 [ PH,
				   term_position(0,0,0,0,[0-0,PB])
				 ]).
					% general term-expansion
unify_clause(Read, Compiled1, TermPos0, TermPos) :-
	expand_term(Read, Compiled2),
	match_module(Compiled2, Compiled1, TermPos0, TermPos).
					% I don't know ...
unify_clause(_, _, _, _) :-
	send(@nil, report, warning, 'Could not unify clause'),
	fail.

unify_clause_head(H1, H2) :-
	strip_module(H1, _, H),
	strip_module(H2, _, H).

match_module((H1 :- B1), (H2 :- B2), Pos0, Pos) :- !,
	unify_clause_head(H1, H2),
	unify_body(B1, B2, Pos0, Pos).
match_module(H1, H2, Pos, Pos) :-	% deal with facts
	unify_clause_head(H1, H2).

%	unify_body(+Read, +Decompiled, +Pos0, -Pos)
%	
%	Deal with translations implied by the compiler.  For example,
%	compiling (a,b),c yields the same code as compiling a,b,c.
%	
%	Pos0 and Pos still include the term-position of the head.

unify_body(B, B, Pos, Pos) :-
	\+ subterm(brace_term_position(_,_,_), Pos), !.
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

ubody(B, B, P, P) :-
	\+ subterm(brace_term_position(_,_,_), P), !.
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
	C0 = (_,_), !,
      conj(C0, P0, GL, PL),
      mkconj(C,  P,  GL, PL).
ubody(((A,B),C), (A,B,C),		% {},X expansion
      term_position(F1,T1,FF1,TT1,
		    [ brace_term_position(F1,T2,PA),
		      PC
		    ]),
      term_position(F1,T1,FF1,TT1,
		    [ PA,
		      term_position(F1,T2,0,0,[0-0,PC])
		    ])) :- !.
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

mkconj((A,B), term_position(0,0,0,0,[PA,PB]), GL, TG, PL, TP) :- !,
	mkconj(A, PA, GL, TGA, PL, TPA),
	mkconj(B, PB, TGA, TG, TPA, TP).
mkconj(A0, P0, [A|TG], TG, [P|TP], TP) :-
	ubody(A0, A, P0, P).


subterm(X,X).
subterm(X, T) :-
	compound(T),
	arg(_, T, A),
	subterm(X, A).


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
	debug('send method ...', []),
	arg(1, Head, Receiver),
	functor(Head, _, Arity),
	pce_method_head_arguments(2, Arity, Head, Msg),
	debug('head ...', []),
	pce_method_body(Body, PlBody, TermPos0, TermPos).
pce_method_clause(Head, Body,
		  get_implementation(_Id, Msg, Receiver, Result), PlBody,
		  TermPos0, TermPos) :- !,
	debug('get method ...', []),
	arg(1, Head, Receiver),
	debug('receiver ...', []),
	functor(Head, _, Arity),
	arg(Arity, Head, PceResult),
	debug('~w?~n', [PceResult = Result]),
	pce_unify_head_arg(PceResult, Result),
	Ar is Arity - 1,
	pce_method_head_arguments(2, Ar, Head, Msg),
	debug('head ...', []),
	pce_method_body(Body, PlBody, TermPos0, TermPos).

pce_method_head_arguments(N, Arity, Head, Msg) :-
	N =< Arity, !,
	arg(N, Head, PceArg),
	PLN is N - 1,
	arg(PLN, Msg, PlArg),
	pce_unify_head_arg(PceArg, PlArg),
	debug('~w~n', [PceArg = PlArg]),
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

pce_method_body(::(_,A0), A, TermPos0, TermPos) :- !,
	TermPos0 = term_position(F, T, FF, FT,
				 [ HeadPos,
				   term_position(_,_,_,_, [_,BodyPos0])
				 ]),
	TermPos  = term_position(F, T, FF, FT,
				 [ HeadPos,
				   term_position(0,0,0,0, [0-0,BodyPos])
				 ]),
	expand_goal(A0, A, BodyPos0, BodyPos).
pce_method_body(A0, A, TermPos0, TermPos) :-
	TermPos0 = term_position(F, T, FF, FT,
				 [ HeadPos,
				   BodyPos0
				 ]),
	TermPos  = term_position(F, T, FF, FT,
				 [ HeadPos,
				   term_position(0,0,0,0, [0-0,BodyPos])
				 ]),
	expand_goal(A0, A, BodyPos0, BodyPos).


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
	goal_expansion(G0, G).

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
	'$default_module'(Module, system, system).

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

%	clause_name(+Ref, -Name)
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

predicate_classification(Goal, Style) :-
	predicate_property(Goal, Prop),
	map_property(Prop, Style), !.
predicate_classification(_, user).

style_property(built_in).
style_property(foreign).
style_property(dynamic).
style_property(undefined).
style_property(transparent).

map_property(Prop, Prop) :-
	style_property(Prop), !.


		 /*******************************
		 *        LOW-LEVEL STUFF	*
		 *******************************/
	
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
These predicates communicate about lines.  We   should  consider using a
line-cache for this for speed.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%	seek_to_line(+Stream, +Line)
%
%	Seek to indicated line-number.

seek_to_line(_, 1) :- !.
seek_to_line(Fd, N) :-
	skip(Fd, 10),
	NN is N - 1,
	seek_to_line(Fd, NN).
