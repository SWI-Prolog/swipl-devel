/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

:- module(prolog_clause,
	  [ clause_info/4,		% +ClaseRef, -File, -TermPos, -VarNames
	    clear_clause_info_cache/0,

	    predicate_name/2,		% +Head, -Name
	    predicate_classification/2,	% +Goal, -Classification
	    clause_name/2		% +ClauseRef, -Name
	  ]).


:- pce_global(@dynamic_source_buffer, new(text_buffer)).

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
	'$clause'(Head, Body, ClauseRef, VarOffset),
	(   Body == true
	->  DecompiledClause = Head
	;   DecompiledClause = (Head :- Body)
	),
	clause_property(ClauseRef, file(File)),
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

unify_clause(Read, Read, TermPos, TermPos) :- !.
unify_clause(:->(Head, Body), (PlHead :- PlBody), TermPos0, TermPos) :- !,
	pce_method_clause(Head, Body, PlHead, PlBody, TermPos0, TermPos).
unify_clause(:<-(Head, Body), (PlHead :- PlBody), TermPos0, TermPos) :- !,
	pce_method_clause(Head, Body, PlHead, PlBody, TermPos0, TermPos).
unify_clause(Read, Compiled1, TermPos, TermPos) :-
	expand_term(Read, Compiled2),
	match_module(Compiled1, Compiled2).
unify_clause(_, _, _, _) :-
	send(@nil, report, warning, 'Could not unify clause'),
	fail.

unify_clause_head(H1, H2) :-
	strip_module(H1, _, H),
	strip_module(H2, _, H).

match_module((H1 :- B), (H2 :- B)) :- !,
	unify_clause_head(H1, H2).
match_module(H1, H2) :-			% deal with facts
	unify_clause_head(H1, H2).


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

pce_method_body(::(_,A0), A, TermPos0, TermPos) :- !,
	TermPos0 = term_position(F, T, FF, FT,
				 [ HeadPos,
				   term_position(_,_,_,_, [_,BodyPos0])
				 ]),
	TermPos = term_position(F, T, FF, FT, [HeadPos, BodyPos]),
	expand_goal(A0, A, BodyPos0, BodyPos).
pce_method_body(A0, A, TermPos0, TermPos) :-
	TermPos0 = term_position(F, T, FF, FT,
				 [ HeadPos,
				   BodyPos0
				 ]),
	TermPos  = term_position(F, T, FF, FT,
				 [ HeadPos,
				   BodyPos
				 ]),
	expand_goal(A0, A, BodyPos0, BodyPos).


		 /*******************************
		 *     EXPAND_GOAL SUPPORT	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
With the introduction of expand_goal, it  is increasingly hard to relate
the clause from the database to the actual  source. For one thins, we do
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
:- dynamic
	user:prolog_predicate_name/2.	% hook!
:- multifile
	user:prolog_predicate_name/2.

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
	nth_clause(Head, N, Ref),
	predicate_name(Head, PredName),
	thaffix(N, Th),
	sformat(Name, '~d-~w clause of ~w', [N, Th, PredName]).

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
