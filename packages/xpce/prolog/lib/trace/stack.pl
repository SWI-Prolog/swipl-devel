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

:- module(prolog_stack,
	  [ display_stack/3		% Window, Call, Choice
	  ]).
:- use_module(util).
:- use_module(clause).

:- system_module.

%	display_stack(+StackWindow, +CallList, +ChoiceList)

display_stack(Window, Call, Choice) :-
	get(@pce, convert, normal, font, Font),
	get(Font, height, FH),
	merge_choices(Choice, Call, AllFrames),
	level_tag_frames(AllFrames, Tagged),
	keysort(Tagged, SortedByLevel),
	assign_y(SortedByLevel, 0, FH+4, Rows),
	display_levels(Rows, Choice, Window),
	link_parents(Call, Window),
	Call = [Current|_],
	(   Choice = [FirstChoice|_],
	    same_frame(Current, FirstChoice)
	->  link_choices(Choice, Window)
	;   link_choices([Current|Choice], Window)
	),
	send(Window, compute),
	send(Window, scroll_vertical, goto, file, 1000).

same_frame(frame(F, _), frame(F, _)).

frame_level(frame(F, _), L) :-
	prolog_frame_attribute(F, level, L).

merge_choices([], Call, Call).
merge_choices([C0|CT], Call, T) :-
	member(C, Call),
	same_frame(C0, C), !,
	merge_choices(CT, Call, T).
merge_choices([C0|CT], Call, [C0|T]) :-
	merge_choices(CT, Call, T).
	
level_tag_frames([], []).
level_tag_frames([H|T], [Level-H|LT]) :-
	frame_level(H, Level),
	level_tag_frames(T, LT).

assign_y([], _, _, []).
assign_y([L0-F0|Frames], Y, FH, [Y-[F0|T0]|T]) :-
	same_level(L0, Frames, T0, RFrames),
	NY is Y + FH,
	assign_y(RFrames, NY, FH, T).

same_level(L, [L-F0|T], [F0|FT], R) :- !,
	same_level(L, T, FT, R).
same_level(_, T, [], T).

display_levels([], _, _).
display_levels([Y-Frames|T], Choice, Window) :-
	predsort(cmpframes(Choice), Frames, SortedFrames),
	debug('Sorted frames: ~p~n', [SortedFrames]),
	SortedFrames = [frame(Frame, PC)|_],
	prolog_frame_attribute(Frame, level, Level),
	send(Window, display, text(Level, left, normal), point(5, Y)),
	(   PC == choice
	->  X0 is 30 + 150
	;   X0 is 30
	),
	display_frames(SortedFrames, Window, X0, Y),
	display_levels(T, Choice, Window).


cmpframes(Choice, Result, frame(Fr1, choice), frame(Fr2, choice)) :- !,
	nth1(I1, Choice, frame(Fr1, choice)),
	nth1(I2, Choice, frame(Fr2, choice)),
	compare(Result, I1, I2).
cmpframes(_, <, _, frame(_, choice)) :- !.
cmpframes(_, >, frame(_, choice), _) :- !.
cmpframes(_, Result, F1, F2) :-
	compare(Result, F1, F2).

display_frames([], _, _, _).
display_frames([F|T], Window, X, Y) :-
	v_stack_frame(F, V),
	send(V, set, X, Y),
	send(Window, append, V),
	X2 is X + 150,
	display_frames(T, Window, X2, Y).


v_stack_frame(frame(Frame, choice), V) :- !,
	frame_label(Frame, Label),
	new(V, prolog_stack_frame(Frame, Label, choice, choicepoint)).
v_stack_frame(frame(Frame, PC), V) :-
	frame_label(Frame, Label),
	frame_style(Frame, PC, Style),
	new(V, prolog_stack_frame(Frame, Label, PC, Style)).

frame_label(Frame, Label) :-
	prolog_frame_attribute(Frame, goal,  Goal),
	predicate_name(user:Goal, Label).

frame_style(Frame, PC, Style) :-
	integer(PC),
	prolog_frame_attribute(Frame, has_alternatives, A),
	det_style(A, Style).
frame_style(Frame, _, Style) :-
	prolog_frame_attribute(Frame, goal, Goal),
	predicate_classification(Goal, Style).
	
det_style(false, deterministic).
det_style(true,  choicepoint).

link_parents([H1,H2|T], W) :- !,
	H1 = frame(F1, PC1),
	H2 = frame(F2, PC2),
	get(W, member, F1, PC1, V1),
	get(W, member, F2, PC2, V2),
	new(_, hyper(V1, V2, parent, child)),
	link_parents([H2|T], W).
link_parents(_, _).

link_choices([H1,H2|T], W) :- !,
	H1 = frame(F1, _),
	H2 = frame(F2, _),
	get(W, member, F1, V1),
	get(W, member, F2, V2),
	get(V1, pc, PC1),
	get(V2, pc, PC2),
	(   (   PC1 == choice
	    ;   PC2 == choice
	    )
	->  Handle = @default
	;   Handle = east
	),
	new(_, connection(V1, V2, @prolog_choice_link, Handle)),
	link_choices([H2|T], W).
link_choices(_, _).

clean(P) :-
	send(P?graphicals, for_all,
	     if(message(@arg1, instance_of, text),
		message(@prolog, clean_level, @arg1))).

clean_level(Text) :-
	get(@pce, convert, Text?string, int, Level),
	(   get(Text?device?graphicals, find,
		and(message(@arg1, instance_of, prolog_stack_frame),
		    @arg1?level == Level),
		_)
	->  true
	;   free(Text)
	).

		 /*******************************
		 *	 STACK-VIEW WINDOW	*
		 *******************************/

:- pce_begin_class(prolog_stack_view, picture,
		   "Overview of the parent stack-frames").

variable(members,       hash_table,  get, "Frame --> Visualiser table").

initialise(B) :->
	get(@pce, convert, normal, font, Font),
	get(Font, ex, Ex),
	send(B, send_super, initialise, size := size(20 * Ex, 100)),
	send(B, label, 'Stack'),
	send(B, hor_stretch, 100),
	send(B, hor_shrink, 100),
	send(B, slot, members, new(chain_table)).

clean(B) :->
	clean(B).

clear(B) :->
	send(B?graphicals, for_all, message(@arg1, destroy)).


member(B, F:int, PC:[name|int], V:prolog_stack_frame) :<-
	"Find visualiser from frame"::
	get(B, members, Table),
	get(Table, member, F, Chain),
	(   PC == @default
	->  get(Chain, head, V)
	;   get(Chain, find, @arg1?pc == PC, V)
	).


append(B, V:prolog_stack_frame) :->
	send(B, display, V),
	get(V, frame_reference, Frame),
	send(B?members, append, Frame, V).


up(B, Times:[int]) :->
	"Select (Nth) parent"::
	default(Times, 1, N),
	get(B, selection, Fr),
	get(B, member, Fr, V),
	(   N < 0
	->  D is -N,
	    step(D, B, child,  V, V2)
	;   step(N, B, parent, V, V2)
	),
	send(V2, select, @on).
	

step(0, _, _, V, V) :- !.
step(N, B, child, V, V2) :-
	get(V, hypered, child, Ch), !,
	(   get(Ch, frame_reference, Frame),
	    prolog_frame_attribute(Frame, goal, Goal),
	    predicate_property(user:Goal, foreign),
	    get(Ch, hypered, child, Ch2)
	->  NN is N - 1,
	    step(NN, B, child, Ch2, V2)
	;   NN is N - 1,
	    step(NN, B, child, Ch, V2)
	).
step(N, B, parent, V, V2) :-
	get(V, frame_reference, Frame),
	prolog_parent(Frame, Parent, PC),
	(   debug('Looking for parent ~d, PC=~d~n', [Parent, PC]),
	    get(B, member, Parent, PC, V1)
	->  true
	;   v_stack_frame(frame(Parent, PC), V1),
	    send(V1, compute),
	    send(V1, do_set, V?x, V?y - V1?height),
	    new(_, hyper(V, V1, parent, child)),
	    send(V?device, append, V1)
	), !,
	NN is N - 1,
	step(NN, B, parent, V1, V2).
step(_, B, parent, V, V) :- !,
	send(B, report, warning, 'Top frame').
step(_, B, child, V, V) :- !,
	send(B, report, warning, 'Deepest frame').

%	prolog_parent(+Frame, -Parent, -PC)
%
%	Find parent executing Prolog. If our direct parent is a foreign
%	frame, keep walking up.  Same if the parent appears to be a hidden
%	frame, we keep walking up for a user-frame

prolog_parent(Frame, Parent, PC) :-
	prolog_frame_attribute(Frame, pc, PC0), !,
	prolog_frame_attribute(Frame, parent, MyParent),
	(   prolog_frame_attribute(MyParent, hidden, false)
	->  Parent = MyParent,
	    PC = PC0
	;   prolog_parent(MyParent, Parent, PC)
	).
prolog_parent(Frame, Parent, PC) :-
	prolog_frame_attribute(Frame, parent, Super),
	prolog_parent(Super, Parent, PC).

down(B, Times:[int]) :->
	"Select (Nth) child"::
	default(Times, 1, N),
	NN is -N,
	send(B, up, NN).

selection(B, What:'int|prolog_stack_frame', PC:[int|name]) :->
	"Select the given frame"::
	(   integer(What)
	->  (   get(B, member, What, PC, V)
	    ->  send(B, send_super, selection, V)
	    ;   send(B, send_super, selection, @nil)
	    )
	;   send(B, send_super, selection, What)
	).

selection(B, Fr:int) :<-
	"Return selected frame visualiser"::
	get(B, get_super, selection, Chain),
	get(Chain, size, 1),
	get(Chain, head, Gr),
	get(Gr, frame_reference, Fr).

frame_finished(B, Fr:int) :->
	get(B, members, Table),
	get(Table, member, Fr, Frames),
	(   send(Frames, empty)
	->  true
	;   send(Frames, for_all, message(@arg1, destroy)),
	    clean(B)
	).

:- pce_end_class.


		 /*******************************
		 *	 FRAME VISUALISER	*
		 *******************************/

:- pce_global(@prolog_choice_link,
	      new(link(link, link, line(arrows := second)))).

:- pce_begin_class(prolog_stack_frame, figure,
		   "Frame of the stack-view").

handle(-5,  h/2, link, east).
handle(w+5, h/2, link, west).

style_image(deterministic,	'det.xpm').
style_image(choicepoint,	'ndet.xpm').
style_image(built_in,		'builtin.xpm').
style_image(foreign,		'foreign.xpm').
style_image(dynamic,		'dynamic.xpm').
style_image(undefined,		'undefined.xpm').
style_image(transparent,	'meta.xpm').
style_image(user,		'user.xpm').

variable(frame_reference,  int,		get, "Reference of Prolog frame").
variable(pc,		   'int|name',	get, "Location in the frame").
variable(frame_level,	   int,		get, "Nesting of the frame").

:- pce_global(@prolog_stack_frame_recogniser,
	      new(click_gesture(left, '', single,
				message(@receiver, select)))).

initialise(D, Frame:int, Label:char_array, PC:'int|name', Style:name) :->
	send(D, send_super, initialise),
	send(D, border, 3),
	send(D, shadow, 1),
	send(D, pen, 1),
	send(D, background, colour(white)),
	style_image(Style, Image),
	send(D, display, new(B, bitmap(Image))),
	send(D, display, text(Label, left, normal), point(B?right_side, 0)),
	send(D, slot, frame_reference, Frame),
	send(D, slot, pc, PC),
	prolog_frame_attribute(Frame, level, Level),
	send(D, slot, frame_level, Level).

unlink(D) :->
	(   get(D, device, Dev),
	    Dev \== @nil,
	    get(Dev, members, Table),
	    send(Table, delete, D?frame_reference)
	->  true
	;   true
	),
	send(D, send_super, unlink).


event(D, Ev:event) :->
	(   send(D, send_super, event, Ev)
	;   send(@prolog_stack_frame_recogniser, event, Ev)
	).


select(D, Show:[bool]) :->
	"Make this frame the selected one"::
	send(D?device, selection, D),
	(   Show \== @off
	->  send(D?frame, show_frame, D?frame_reference, D?pc)
	;   true
	).

help_message(D, _What:{tag,summary}, _Ev:[event], Msg:string) :<-
	"Fetch associated help message"::
	get(D, frame_reference, FR),
	get(D, pc, PC),
	new(Msg, string('Frame #%d at PC = %s', FR, PC)).

:- pce_end_class.
