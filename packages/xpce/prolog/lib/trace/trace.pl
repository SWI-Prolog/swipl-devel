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

:- module(pce_prolog_tracer,
	  [ prolog_show_frame/2,	% +Frame, +Options
	    clause_info/4,		% +Ref, -File, -TermPos, -VarOffset
	    clear_clause_info_cache/0
	  ]).
:- consult([ clause,
	     util,
	     source,
	     break,
	     gui
	   ]).

:- visible(+cut_call).


		 /*******************************
		 *	      INTERCEPT		*
		 *******************************/

:- dynamic
	last_action/1,
	show_unify_as/2,
	showing/1.

user:prolog_trace_interception(Port, Frame, CHP, Action) :-
	current_prolog_flag(gui_tracer, true),
	notrace(intercept(Port, Frame, CHP, GuiAction)),
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
map_action(abort, _, abort).
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
intercept(Port, Frame, CHP, Action) :-
	send_tracer(current_break(@nil)),
	debug('*** do_intercept(~w, ~w, ~w, _) ...~n', [Port, Frame, CHP]), 
	do_intercept(Port, Frame, CHP, Action0),
	fix_action(Port, Action0, Action),
	debug('*** ---> Action = ~w~n', [Action]),
	send_if_tracer(report(status, '%s ...', Action)),
	retractall(last_action(_)),
	asserta(last_action(Action)).

fix_action(fail, skip,   creep) :- !.
fix_action(exit, skip,   creep) :- !.
fix_action(_,    Action, Action).

do_intercept(call, Frame, CHP, Action) :-
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
	;   show(Frame, CHP, 1, call),
	    action(Action)
	).
do_intercept(exit, Frame, CHP, Action) :-
	(   %last_action(finish)
	    prolog_frame_attribute(Frame, goal, Goal),
	    \+(( predicate_property(Goal, built_in)
	       ; predicate_property(Goal, foreign)
	      )),
	    \+(( last_action(skip),
		 prolog_skip_level(L,L),
		 prolog_frame_attribute(Frame, level, FL),
		 FL >= L
	      ))
	->  show(Frame, CHP, 0, exit),
	    action(Action)
	;   last_action(leap)
	->  Action = leap
	;   Action = creep
	).
do_intercept(fail, Frame, CHP, Action) :-
	(   prolog_frame_attribute(Frame, goal, Goal),
	    (	predicate_property(Goal, built_in)
	    ;	predicate_property(Goal, foreign)
	    )
	->  Up = 1
	;   Up = 0
	),
	show(Frame, CHP, Up, fail),
	action(Action).
do_intercept(exception(Except), Frame, CHP, Action) :-
	(   prolog_frame_attribute(Frame, goal, Goal),
	    (	predicate_property(Goal, built_in)
	    ;	predicate_property(Goal, foreign)
	    )
	->  Up = 1
	;   Up = 0
	),
	show(Frame, CHP, Up, exception(Except)),
	action(Action).
do_intercept(redo, Frame, _CHP, into) :-
	prolog_frame_attribute(Frame, goal, GT),
	debug('Redo on ~p~n', [GT]),
	asserta(show_unify_as(Frame, redo)).
do_intercept(unify, Frame, CHP, Action) :-
	visible(-unify),
	(   show_unify_as(Frame, How)
	;   How = unify
	), !,
	retractall(show_unify_as(_, _)),
	debug('Show unify port as ~w~n', [How]),
	show(Frame, CHP, 0, unify, How),
	prolog_frame_attribute(Frame, goal, Goal),
	predicate_name(user:Goal, Pred),
	send_tracer(report(status, '%s: %s', How?label_name, Pred)),
	action(Action).
do_intercept(break(PC), Frame, CHP, Action) :-
	prolog_frame_attribute(Frame, goal, Goal),
	prolog_frame_attribute(Frame, clause, ClauseRef),
	'$fetch_vm'(ClauseRef, PC, NPC, _VMI),
	predicate_name(user:Goal, Pred),
	send_tracer(report(status, 'Break in: %s', Pred)),
	send_tracer(current_break(tuple(ClauseRef, PC))),
	prolog_show_frame(Frame,
			  [ pc(NPC),
			    choice(CHP),
			    port(call),
			    style(break),
			    stack,
			    source,
			    bindings
			  ]),
	action(Action).
do_intercept(cut_call(PC), Frame, CHP, Action) :-
	prolog_frame_attribute(Frame, goal, Goal),
	predicate_name(user:Goal, Pred),
	send_tracer(report(status, 'Cut in: %s', Pred)),
	prolog_show_frame(Frame,
			  [ pc(PC),
			    choice(CHP),
			    port(call),
			    style(call),
			    stack,
			    source,
			    bindings
			  ]),
	action(Action).
do_intercept(cut_exit(PC), Frame, CHP, Action) :-
	prolog_show_frame(Frame,
			  [ pc(PC),
			    choice(CHP),
			    port(exit),
			    style(call),
			    stack,
			    source,
			    bindings
			  ]),
	action(Action).


show(StartFrame, CHP, Up, exception(Except)) :- !,
	show(StartFrame, CHP, Up, exception, exception),
	message_to_string(Except, Message),
	send_tracer(report(warning, 'Exception: %s', Message)).
show(StartFrame, CHP, Up, Port) :-
	prolog_frame_attribute(StartFrame, goal, Goal),
	predicate_name(user:Goal, Pred),
	send_tracer(report(status, '%s: %s', Port?label_name, Pred)),
	show(StartFrame, CHP, Up, Port, Port).

show(StartFrame, CHP, Up, Port, Style) :-
	send_tracer(current_frame(StartFrame)),
	prolog_show_frame(StartFrame,
			  [ port(Port),
			    choice(CHP),
			    stack
			  ]),
	find_frame(Up, StartFrame, Port, PC, Frame),
	retractall(showing(_)),
	assert(showing(Frame)),
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
	(   '$clause_term_position'(ClauseRef, PC, List)
	->  debug('Term-position: ~w~n', [List]),
	    (   find_subgoal(List, TPos, PosTerm)
	    ->  true
	    ;   PosTerm = TPos,
		send_tracer(report(warning,
				   'Clause source-info could not be parsed')),
		fail
	    ),
	    arg(1, PosTerm, CharA),
	    arg(2, PosTerm, CharZ)
	;   send_tracer(report(warning,
			       'No clause-term-position for ref=%s at PC=%s',
			       ClauseRef, PC)),
	    fail
	).


head_pos(Ref, Pos, HPos) :-
	clause_property(Ref, fact), !,
	HPos = Pos.
head_pos(_, term_position(_, _, _, _, [HPos,_]), HPos).

%	warning, ((a,b),c)) --> compiled to (a, (b, c))!!!  We try to correct
%	that in clause.pl.  This is work in progress.

find_subgoal([], Pos, Pos).
find_subgoal([A|T], term_position(_, _, _, _, PosL), SPos) :-
	nth1(A, PosL, Pos),
	find_subgoal(T, Pos, SPos).
find_subgoal([1|T], brace_term_position(_,_,Pos), SPos) :-
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
	attribute(Attributes, choice(CHP), Frame),
	setting(stack_depth, Depth),
	setting(choice_depth, MaxChoice),
	get_tracer(member(stack), StackBrowser),
	send(StackBrowser, clear),
	stack_frames(Depth, Frame, PC, CallFrames),
	debug('Stack frames: ~w~n', [CallFrames]),
	level_range(CallFrames, Range),
	debug('Levels ~w, CHP = ~w~n', [Range, CHP]),
	choice_frames(MaxChoice, CHP, Range, [], ChoiceFrames),
	debug('Choicepoints: ~p~n', [ChoiceFrames]),
	display_stack(StackBrowser, CallFrames, ChoiceFrames).
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

%	choice_frames(+Max, +CHP, +MinLevel-MaxLevel, -Frames)

choice_frames(_, none, _, _, []) :- !.
choice_frames(Max, CHP, Range, Seen, [frame(Frame, choice)|Frames]) :-
	Max > 0,
	earlier_choice(CHP, CH),
	visible_choice(CH),
	prolog_choice_attribute(CH, frame, Frame),
	\+ memberchk(Frame, Seen),
	prolog_frame_attribute(Frame, level, Flev),
	in_range(Flev, Range), !,
	NMax is Max - 1,
	(   prolog_choice_attribute(CH, parent, Prev)
	->  choice_frames(NMax, Prev, Range, [Frame|Seen], Frames)
	;   Frames = []
	).
choice_frames(_, _, _, _, []).

%	earlier_choice(+Here, -Visible)
%	
%	Return earliers choices on backgtracking.

earlier_choice(CHP, CHP).
earlier_choice(CHP, Next) :-
	prolog_choice_attribute(CHP, parent, Parent),
	earlier_choice(Parent, Next).

%	visible_choice(+CHP)
%	
%	A visible choice is a choice-point that realises a real choice
%	and is created by a visible frame.

visible_choice(CHP) :-
	prolog_choice_attribute(CHP, type, Type),
	real_choice_type(Type),
	prolog_choice_attribute(CHP, frame, Frame),
	prolog_frame_attribute(Frame, hidden, false),
	debug('Choice ~w of type ~w running frame ~w~n',
	      [CHP, Type, Frame]).

real_choice_type(clause).
real_choice_type(foreign).
real_choice_type(jump).


level_range(Frames, H-L) :-
	Frames = [F0|_],
	last(FT, Frames),
	flevel(F0, L),
	flevel(FT, H).

flevel(frame(Frame, _), L) :-
	prolog_frame_attribute(Frame, level, L),
	debug('Frame ~d at level ~d~n', [Frame, L]).

in_range(Level, Low-_High) :-
	Level >= Low.
%	between(Low, High, Level).

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
	    catch(clause_info(ClauseRef, _, _, VarNames), E,
		  (print_message(error, E), fail)),
	    frame_bindings(Frame, VarNames, Bindings),
	    send(Browser, bindings, Bindings),
	    debug('(ok) ', [])
	).
show_bindings(_, _).

show_arguments(Frame, _Attributes) :-
	get_tracer(member(bindings), Browser),
	send(Browser, label, 'Arguments'),
	frame_arguments(Frame, Args),
	send(Browser, bindings, Args).

%	frame_arguments(+Frame, -Args)
%
%	Return arguments of the frame as [I:I=Value, ...], compatible with
%	the normal binding list.

frame_arguments(Frame, Args) :-
	prolog_frame_attribute(Frame, goal, Goal),
	(   Goal = _:Head
	->  functor(Head, _, Arity)
	;   functor(Goal, _, Arity)
	),
	frame_arguments(1, Arity, Frame, Args).

frame_arguments(I, Arity, Frame, [I:I=Value|T]) :-
	I =< Arity, !,
	prolog_frame_attribute(Frame, argument(I), Value),
	NI is I + 1,
	frame_arguments(NI, Arity, Frame, T).
frame_arguments(_, _, _, []).


frame_bindings(Frame, VarNames, Bindings) :-
	functor(VarNames, _, Arity),
	frame_bindings(0, Arity, Frame, VarNames, B0),
	(   setting(cluster_variables, true)
	->  cluster_bindings(B0, Bindings)
	;   Bindings = B0
	).

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
