/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

:- module(pce_prolog_tracer,
	  [ prolog_show_frame/2,	% +Frame, +Options
	    clause_info/4,		% +Ref, -File, -TermPos, -VarOffset
	    clear_clause_info_cache/0
	  ]).
:- use_module(gui).
:- use_module(util).
:- use_module(clause).

:- visible(+cut_call).


		 /*******************************
		 *	      INTERCEPT		*
		 *******************************/

:- dynamic
	last_action/1,
	show_unify_as/2.

user:prolog_trace_interception(Port, Frame, BFR, Action) :-
	current_prolog_flag(gui_tracer, true),
	notrace(pce_prolog_tracer:intercept(Port, Frame, BFR, GuiAction)),
	map_action(GuiAction, Frame, Action).

map_action(creep, _, continue) :-
	traceall.
map_action(skip, _, continue) :-
	get_tracer(selected_frame, Frame),
	prolog_frame_attribute(Frame, level, Level),
	Skip is Level + 1,
	trace,
	prolog_skip_level(_, Skip).
map_action(into, _, continue) :-
	visible(+unify),
	traceall.
map_action(leap, _, continue) :-
	prolog_skip_level(_, very_deep),
	notrace.
map_action(retry, _, retry(Frame)) :-
	traceall,
	get_tracer(selected_frame, Frame).
map_action(fail, _, fail) :-
	traceall.
map_action(nodebug, _, nodebug).
map_action(leap, _, continue) :-
	notrace.
map_action(abort, _, continue) :-
	abort.
map_action(halt, _, continue) :-
	halt.
map_action(finish, _, continue) :-
	get_tracer(selected_frame, Frame),
	prolog_frame_attribute(Frame, level, Level),
	trace,
	prolog_skip_level(_, Level).

traceall :-
	prolog_skip_level(_, very_deep),
	trace.

intercept(_, _, _, _) :-
	setting(active, false), !,
	fail.
intercept(Port, Frame, BFR0, Action) :-
	fix_bfr(BFR0, Frame, BFR),
	do_intercept(Port, Frame, BFR, Action0),
	fix_action(Port, Action0, Action),
	send_if_tracer(report(status, '%s ...', Action)),
	retractall(last_action(_)),
	asserta(last_action(Action)).

fix_bfr(none, FR, FR).
fix_bfr(BFR,  _,  BFR).

fix_action(fail, skip,   creep) :- !.
fix_action(exit, skip,   creep) :- !.
fix_action(_,    Action, Action).

do_intercept(call, Frame, BFR, Action) :-
	(   (   last_action(retry)
	    ;	prolog_frame_attribute(Frame, top, true),
		debug('Toplevel frame~n', [])
	    ;	prolog_frame_attribute(Frame, parent, Parent),
		(   prolog_frame_attribute(Parent, hidden, true)
		;   prolog_frame_attribute(Parent, goal, ParentGoal),
		    predicate_property(ParentGoal, built_in)
		)
	    )
	->  Action = into,
	    asserta(show_unify_as(Frame, call))
	;   show(Frame, BFR, 1, call),
	    action(Action)
	).
do_intercept(exit, Frame, BFR, Action) :-
	(   last_action(finish)
	->  show(Frame, BFR, 0, exit),
	    action(Action)
	;   last_action(leap)
	->  Action = leap
	;   Action = creep
	).
do_intercept(fail, Frame, BFR, Action) :-
	(   prolog_frame_attribute(Frame, goal, Goal),
	    (	predicate_property(Goal, built_in)
	    ;	predicate_property(Goal, foreign)
	    )
	->  Up = 1
	;   Up = 0
	),
	show(Frame, BFR, Up, fail),
	action(Action).
do_intercept(exception, Frame, BFR, Action) :-
	(   prolog_frame_attribute(Frame, goal, Goal),
	    (	predicate_property(Goal, built_in)
	    ;	predicate_property(Goal, foreign)
	    )
	->  Up = 1
	;   Up = 0
	),
	show(Frame, BFR, Up, exception),
	action(Action).
do_intercept(redo, Frame, _BFR, into) :-
	prolog_frame_attribute(Frame, goal, GT),
	debug('Redo on ~p~n', [GT]),
	asserta(show_unify_as(Frame, redo)).
do_intercept(unify, Frame, BFR, Action) :-
	visible(-unify),
	(   show_unify_as(Frame, How)
	;   How = unify
	), !,
	retractall(show_unify_as(_, _)),
	debug('Show unify port as ~w~n', [How]),
	show(Frame, BFR, 0, unify, How),
	prolog_frame_attribute(Frame, goal, Goal),
	predicate_name(user:Goal, Pred),
	send_tracer(report(status, '%s: %s', How?label_name, Pred)),
	action(Action).
do_intercept(break(PC), Frame, BFR, Action) :-
	prolog_frame_attribute(Frame, goal, Goal),
	prolog_frame_attribute(Frame, clause, ClauseRef),
	'$fetch_vm'(ClauseRef, PC, NPC, _VMI),
	predicate_name(user:Goal, Pred),
	send_tracer(report(status, 'Break in: %s', Pred)),
	prolog_show_frame(Frame,
			  [ pc(NPC),
			    bfr(BFR),
			    port(call),
			    style(break),
			    stack,
			    source,
			    bindings
			  ]),
	action(Action).
do_intercept(cut_call(PC), Frame, BFR, Action) :-
	prolog_frame_attribute(Frame, goal, Goal),
	predicate_name(user:Goal, Pred),
	send_tracer(report(status, 'Cut in: %s', Pred)),
	prolog_show_frame(Frame,
			  [ pc(PC),
			    bfr(BFR),
			    port(call),
			    style(call),
			    stack,
			    source,
			    bindings
			  ]),
	action(Action).
do_intercept(cut_exit(PC), Frame, BFR, Action) :-
	prolog_show_frame(Frame,
			  [ pc(PC),
			    bfr(BFR),
			    port(exit),
			    style(call),
			    stack,
			    source,
			    bindings
			  ]),
	action(Action).


show(StartFrame, BFR, Up, Port) :-
	prolog_frame_attribute(StartFrame, goal, Goal),
	predicate_name(user:Goal, Pred),
	send_tracer(report(status, '%s: %s', Port?label_name, Pred)),
	show(StartFrame, BFR, Up, Port, Port).

show(StartFrame, BFR, Up, Port, Style) :-
	prolog_show_frame(StartFrame,
			  [ port(Port),
			    bfr(BFR),
			    stack
			  ]),
	find_frame(Up, StartFrame, Port, PC, Frame),
	prolog_show_frame(Frame,
			  [ pc(PC),
			    port(Port),
			    style(Style),
			    source,
			    bindings
			  ]).


		 /*******************************
		 *         SHOW LOCATION	*
		 *******************************/

attribute(Attributes, Att) :-
	memberchk(Att, Attributes), !.

attribute(Attributes, Att, _) :-
	memberchk(Att, Attributes), !.
attribute(_, Att, Def) :-
	arg(1, Att, Def).


prolog_show_frame(Frame, Attributes) :-
	show_stack(Frame, Attributes),
	show_bindings(Frame, Attributes),
	show_source(Frame, Attributes),
	(   setting(auto_raise, true)
	->  send_tracer(expose)
	;   true
	).


show_source(Frame, Attributes) :-
	attribute(Attributes, source), !,
	debug('source for #~w: ', [Frame]),
	(   attribute(Attributes, pc(PC)),
	    attribute(Attributes, port(Port), call),
	    attribute(Attributes, style(Style), Port),
	    debug('Show source, PC = ~w, Port = ~w~n', [PC, Port]),
	    (   (PC == call ; PC == fail ; PC == exception )
	    ->  prolog_frame_attribute(Frame, goal, Goal),
		find_source(Goal, File, Line),
		debug('At ~w:~d~n', [File, Line]),
		send_tracer(show_line(File, Line, Style))
	    ;   (   prolog_frame_attribute(Frame, clause, ClauseRef),
		    debug('ClauseRef = ~w, PC = ~w~n', [ClauseRef, PC]),
		    ClauseRef \== 0
		->  subgoal_position(ClauseRef, PC, File, CharA, CharZ),
		    debug('~p.~n', [show_range(File, CharA, CharZ, Style)]),
		    send_tracer(show_range(File, CharA, CharZ, Style)),
		    (	clause_property(ClauseRef, erased)
		    ->	send_tracer(report(warning,
					   'Running erased clause; source location may be incorrect'))
		    ;	true
		    )
		;   prolog_frame_attribute(Frame, goal, Goal),
		    find_source(Goal, File, Line),
		    send_tracer(show_line(File, Line, Style))
		)
	    )
	->  true
	;   send_tracer(file(@nil))
	).
show_source(_, _).


find_frame(N, Start, _, PC, Frame) :-
	N > 0, 
	debug('Frame = ~w; ', [Start]),
	prolog_frame_attribute(Start, pc, PC0),
	prolog_frame_attribute(Start, parent, Frame0), !,
	debug('parent = ~w~n', [Frame0]),
	NN is N - 1,
	find_frame2(NN, Frame0, PC0, Frame, PC).
find_frame(_, Frame, Port, Port, Frame).

find_frame2(0, F, PC, F, PC).
find_frame2(N, F0, _, F, PC) :-
	prolog_frame_attribute(F0, parent, F1),
	prolog_frame_attribute(F0, pc, PC1),
	NN is N - 1,
	find_frame2(NN, F1, PC1, F, PC).

subgoal_position(ClauseRef, unify, File, CharA, CharZ) :- !,
	clause_info(ClauseRef, File, TPos, _),
	head_pos(ClauseRef, TPos, PosTerm),
	arg(1, PosTerm, CharA),
	arg(2, PosTerm, CharZ).
subgoal_position(ClauseRef, choice, File, CharA, CharZ) :- !,
	clause_info(ClauseRef, File, TPos, _),
	arg(2, TPos, CharA),
	CharZ is CharA + 1.		% i.e. select the dot.
subgoal_position(ClauseRef, exit, File, CharA, CharZ) :- !,
	clause_info(ClauseRef, File, TPos, _),
	arg(2, TPos, CharA),
	CharZ is CharA + 1.		% i.e. select the dot.
subgoal_position(ClauseRef, fail, File, CharA, CharZ) :- !,
	subgoal_position(ClauseRef, exit, File, CharA, CharZ).
subgoal_position(ClauseRef, exception, File, CharA, CharZ) :- !,
	subgoal_position(ClauseRef, exit, File, CharA, CharZ).
subgoal_position(ClauseRef, PC, File, CharA, CharZ) :-
	clause_info(ClauseRef, File, TPos, _),
	'$clause_term_position'(ClauseRef, PC, List),
	debug('Term-position: ~w~n', [List]),
	find_subgoal(List, TPos, PosTerm),
	arg(1, PosTerm, CharA),
	arg(2, PosTerm, CharZ).


head_pos(Ref, Pos, HPos) :-
	clause_property(Ref, fact), !,
	HPos = Pos.
head_pos(_, term_position(_, _, _, _, [HPos,_]), HPos).

%	warning, ((a,b),c)) --> compiled to (a, (b, c))!!!

find_subgoal([], Pos, Pos).
find_subgoal([A|T], term_position(_, _, _, _, PosL), SPos) :-
	nth1(A, PosL, Pos),
	find_subgoal(T, Pos, SPos).


		 /*******************************
		 *             ACTION		*
		 *******************************/

action(Action) :-
	get_tracer(action, Action0),
	debug('Got action ~w~n', [Action0]),
	action(Action0, Action).

action(break, Action) :- !,
	break,
	format(user_error, 'Continuing the debug session~n', []),
	action(Action).
action(Action, Action).


		 /*******************************
		 *	      STACK		*
		 *******************************/

show_stack(Frame, Attributes) :-
	attribute(Attributes, stack), !,
	debug('stack ...', []),
	attribute(Attributes, port(Port), call),
	attribute(Attributes, pc(PC), Port),
	attribute(Attributes, bfr(BFR), Frame),
	setting(stack_depth, Depth),
	setting(choice_depth, MaxChoice),
	get_tracer(member(stack), StackBrowser),
	send(StackBrowser, clear),
	stack_frames(Depth, Frame, PC, CallFrames),
	debug('Stack frames: ~w~n', [CallFrames]),
	level_range(CallFrames, Range),
	debug('Levels ~w, BFR = ~w~n', [Range, BFR]),
	(   first_alternative(BFR, CHP0)
	->  choicepoints(MaxChoice, CHP0, Range, ChoicePoints)
	;   ChoicePoints = []
	),
	display_stack(StackBrowser, CallFrames, ChoicePoints).
show_stack(_, _).

stack_frames(0, _, _, []) :- !.
stack_frames(Depth, F, PC, Frames) :-
	(   prolog_frame_attribute(F, hidden, true)
	->  RestFrames = Frames,
	    ND is Depth
	;   Frames = [frame(F, PC)|RestFrames],
	    ND is Depth - 1
	),
	(   prolog_frame_attribute(F, parent, Parent),
	    (   prolog_frame_attribute(F, pc, PCParent)
	    ->	true
	    ;	PCParent = foreign
	    )
	->  stack_frames(ND, Parent, PCParent, RestFrames)
	;   RestFrames = []
	).

choicepoints(0, _, _, []) :- !.
choicepoints(Max, F, Range, [frame(F, choice)|Choice]) :-
	prolog_frame_attribute(F, hidden, false),
	prolog_frame_attribute(F, level, Flev),
	in_range(Flev, Range), !,
	(   alternative_frame(F, Alt)
	->  NMax is Max - 1,
	    choicepoints(NMax, Alt, Range, Choice)
	;   Choice = []
	).
choicepoints(_, _, _, []).

level_range(Frames, H-L) :-
	Frames = [F0|_],
	last(Frames, FT),
	flevel(F0, L),
	flevel(FT, H).

last([T], T).
last([_|L], T) :-
	last(L, T).

flevel(frame(Frame, _), L) :-
	prolog_frame_attribute(Frame, level, L),
	debug('Frame ~d at level ~d~n', [Frame, L]).

in_range(Level, Low-_High) :-
	Level >= Low.
%	between(Low, High, Level).

alternative_frame(Fr, Alt) :-
	prolog_frame_attribute(Fr, alternative, F0),
	debug('Choice for #~w is #~w~n', [Fr, F0]),
	(   prolog_frame_attribute(F0, has_alternatives, true)
	->  Alt0 = F0
	;   debug('   Not a real choice, looking for next~n', []),
	    alternative_frame(F0, Alt0)
	),
	(   prolog_frame_attribute(Alt0, hidden, true)
	->  debug('   Hidden frame, looking for next~n', []),
	    alternative_frame(Alt0, Alt)
	;   Alt = Alt0
	).

first_alternative(Fr, Alt) :-
	(   prolog_frame_attribute(Fr, has_alternatives, true)
	->  Alt = Fr
	;   alternative_frame(Fr, Alt)
	).

show_stack_location(Frame, PC) :-
	get_tracer(member(stack), StackBrowser),
	send(StackBrowser, selection, Frame, PC).


		 /*******************************
		 *	       BINDINGS		*
		 *******************************/

show_args_pc(call).
show_args_pc(fail).
show_args_pc(exception).
show_args_pc(foreign).

show_bindings(Frame, Attributes) :-
	attribute(Attributes, bindings), !,
	debug('bindings ... ', []),
	get_tracer(member(bindings), Browser),
	(   attribute(Attributes, pc(PC))
	->  true
	;   PC = @default
	),
	show_stack_location(Frame, PC),
	send(Browser, clear),
	send(Browser, prolog_frame, Frame),
	(   show_args_pc(PC)
	->  show_arguments(Frame, Attributes)
	;   send(Browser, label, 'Bindings'),
	    prolog_frame_attribute(Frame, clause, ClauseRef),
	    debug('(clause ~w) ', [ClauseRef]),
	    clause_info(ClauseRef, _, _, VarNames),
	    (   setting(cluster_variables, true)
	    ->  frame_bindings(Frame, VarNames, Bindings),
		display_bindings(Bindings, Browser)
	    ;   forall(frame_binding(Frame, VarNames, Name=Value),
		       show_binding([Name]=Value, Browser))
	    )
	).
show_bindings(_, _).

show_arguments(Frame, _Attributes) :-
	get_tracer(member(bindings), Browser),
	send(Browser, label, 'Arguments'),
	prolog_frame_attribute(Frame, goal, Goal),
	goal_arity(Goal, Arity),
	(   between(1, Arity, ArgN),
	        prolog_frame_attribute(Frame, argument(ArgN), Value),
	        sformat(VS, '~p', [Value]),
	        send(Browser, append,
		     dict_item(ArgN, string('%2d = %s', ArgN, VS), ArgN)),
	    fail
	;   true
	).

goal_arity(_:Goal, Arity) :- !,
	goal_arity(Goal, Arity).
goal_arity(Goal, Arity) :-
	functor(Goal, _, Arity).

display_bindings([], _).
display_bindings([B|BT], Browser) :-
	show_binding(B, Browser),
	display_bindings(BT, Browser).

show_binding(B, _) :-
	debug('Binding ~w~n', [B]),
	fail.
show_binding([_] = Var, _) :-
	var(Var),
	setting(show_unbound, false), !.
show_binding([V0:ArgN|VT] = Value, Browser) :-
	new(S, string('%s\t = ', V0)),
	forall(member(V:_, VT),
	       send(S, append, string('%s = ', V))),
	sformat(VS, '~p', [Value]),
	send(S, append, VS),
	send(Browser, append, dict_item(V0, S, ArgN)).

frame_binding(Frame, VarNames, (Name:N)=Value) :-
	arg(N, VarNames, Name),
	Name \== '_',
	prolog_frame_attribute(Frame, argument(N), Value).

frame_bindings(Frame, VarNames, Bindings) :-
	functor(VarNames, _, Arity),
	frame_bindings(0, Arity, Frame, VarNames, B0),
	cluster_bindings(B0, Bindings).

frame_bindings(Arity, Arity, _, _, []) :- !.
frame_bindings(N, Arity, Frame, VarNames, [(Name:I)=Value|T]) :-
	I is N + 1,
	arg(I, VarNames, Name),
	Name \== '_', !,
	prolog_frame_attribute(Frame, argument(I), Value),
	frame_bindings(I, Arity, Frame, VarNames, T).
frame_bindings(N, Arity, Frame, VarNames, T) :-
	I is N + 1,
	frame_bindings(I, Arity, Frame, VarNames, T).

cluster_bindings([], []).
cluster_bindings([Name=Value|BR], [[Name|Names]=Value|CR]) :-
	clustered_binding(BR, BT, Value, Names),
	cluster_bindings(BT, CR).

clustered_binding([], [], _, []).
clustered_binding([Name=Val|BR], BT, Value, [Name|NT]) :-
	Val == Value, !,
	clustered_binding(BR, BT, Value, NT).
clustered_binding([B|BR], [B|BT], Value, C) :-
	clustered_binding(BR, BT, Value, C).


:- initialization
   set_prolog_flag(gui_tracer, true).
