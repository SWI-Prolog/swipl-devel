/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

:- module(draw_export,
	  [ describe_drawing/2		% +Canvas, -Term
	  ]).
:- use_module(library(pce)).
:- require([ atom_length/2
	   , chain_list/2
	   , flatten/2
	   , get_chain/3
	   , is_list/1
	   , maplist/3
	   , memberchk/2
	   , send_list/3
	   , sformat/3
	   ]).

:- pce_autoload(drag_and_drop_gesture, library(dragdrop)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Translate a PceDraw drawing into a Prolog term for later display.

A drawing is a term, informally described as:

	Drawing		::= drawing([Element])
	Element		::= display(Term [, point(X, Y)])
			  | connect(Class(From, To,
					  FromHandle, ToHandle,
					  Link))
			  | compound(Term, Drawing, point(X, Y))
	Term		::= new(Var, PlainTerm)
			  | new(ClassName)
	PlainTerm	::= NewTerm[+Attribute ...]
	NewTerm		::= ClassName(Term ...)
	Attribute	::= SendMethod(Term ...)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

class_map(draw_text,
	  text(string, format, font),
	  [background, colour]).
class_map(draw_box,
	  box(width, height),
	  [pen, texture, radius, fill_pattern, colour]).
class_map(draw_ellipse,
	  ellipse(width, height),
	  [pen, texture, fill_pattern, colour]).
class_map(draw_line,
	  line(start_x, start_y, end_x, end_y),
	  [pen, texture, colour, first_arrow, second_arrow]).
class_map(draw_path,
	  path(kind, @default, points),
	  [pen, texture, colour, first_arrow, second_arrow]).
class_map(draw_connection,
	  connection(from, to, link, from_handle, to_handle),
	  []).
class_map(draw_compound,
	  figure,
	  [pen, texture, radius, colour]).
class_map(arrow,
	  arrow(length, wing, style, fill_pattern),
	  [ pen, colour]).
class_map(font,
	  font(family, style, points),
	  []).
class_map(colour,
	  font(@default, red, green, blue),
	  []).
class_map(point,
	  point(x, y),
	  []).
class_map(handle,
	  handle(x_position, y_position, kind, name),
	  []).
class_map(+, +(left, right), []).
class_map(-, -(left, right), []).
class_map(/, /(left, right), []).
class_map(*, *(left, right), []).
	  
no_position(line).
no_position(path).

describe_drawing(Canvas, drawing(Terms)) :-
	get_chain(Canvas, graphicals, Graphicals),
	name_connected_graphicals(Graphicals, Mapping),
	split_graphicals(Graphicals, Objects, Connections),
	maplist(describe_object(Mapping), Objects, ObjectTerms),
	maplist(describe_connection(Mapping),
		Connections, ConnectionTerms),
	flatten([ObjectTerms, ConnectionTerms], Terms).

split_graphicals([], [], []).
split_graphicals([H|T], G, [H|C]) :-
	send(H, instance_of, connection), !,
	split_graphicals(T, G, C).
split_graphicals([H|T], [H|G], C) :-
	split_graphicals(T, G, C).

name_connected_graphicals([], []).
name_connected_graphicals([H|T0], [H=_|T]) :-
	get(H, connections, Chain),
	\+ send(Chain, empty), !,
	name_connected_graphicals(T0, T).
name_connected_graphicals([_|T0], T) :-
	name_connected_graphicals(T0, T).

describe_object(Mapping, Obj, compound(Term, Drawing, point(X,Y))) :-
	send(Obj, instance_of, device), !,
	get(Obj, position, point(X, Y)),
	object_to_term(Obj, Mapping, Term),
	describe_drawing(Obj, Drawing).
describe_object(Mapping, Obj, display(Term)) :-
	no_position(NoPosClass),
	send(Obj, instance_of, NoPosClass), !,
	object_to_term(Obj, Mapping, Term).
describe_object(Mapping, Obj, display(Term, point(X,Y))) :-
	get(Obj, position, point(X, Y)),
	object_to_term(Obj, Mapping, Term).

describe_connection(Mapping, Connection, Var=Term) :-
	memberchk(Connection = Var, Mapping), !,
	describe_connection_(Mapping, Connection, Term).
describe_connection(Mapping, Connection, Term) :-
	describe_connection_(Mapping, Connection, Term).

describe_connection_(Mapping, Connection, connect(Term)) :-
	get(Connection, from, From),
	get(Connection, to, To),
	memberchk(From = VFrom, Mapping),
	memberchk(To   = VTo,   Mapping),
	describe_handle(Connection, from, X),
	describe_handle(Connection, to, Y),
	new(Line, line),
	Term0 = connection(VFrom, VTo, X, Y),
	modifiers([pen, colour, texture, first_arrow, second_arrow],
		  Connection, Line, [], Term0, Term1),
	simplify_attributes(Term1, Term).


		 /*******************************
		 *	      HANDLES		*
		 *******************************/

describe_handle(C, from, T) :- !,
	get(C, from, From),
	get(C, from_handle, HN),
	get(From, handle, HN, Handle),
	object_to_term(Handle, T).
describe_handle(C, to, T) :- !,
	get(C, to, To),
	get(C, to_handle, HN),
	get(To, handle, HN, Handle),
	object_to_term(Handle, T).


		 /*******************************
		 *	  OBJECT --> TERM	*
		 *******************************/

object_to_term(Object, Term) :-
	object_to_term(Object, [], Term).

object_to_term(Atomic, _, Atomic) :-
	atomic(Atomic), !.
object_to_term(Font, _, Alias) :-	% replace font by alias
	send(Font, instance_of, font),
	get(@display?font_table, find_key,
	    message(@arg2, equal, Font),
	    Alias), !.
object_to_term(Colour, _, colour(Name)) :-
	send(Colour, instance_of, colour),
	get(Colour, kind, named), !,
	get(Colour, name, Name).
object_to_term(Chain, Mapping, Term) :-
	send(Chain, instance_of, chain),
	chain_list(Chain, List),
	objects_to_terms(List, Mapping, Terms),
	Term =.. [chain|Terms].
object_to_term(@h_var, _, h) :- !.
object_to_term(@w_var, _, w) :- !.
object_to_term(@x_var, _, x) :- !.
object_to_term(@y_var, _, y) :- !.
object_to_term(@Global, _, @Global) :-
	atom(Global),
	\+ send(@Global, instance_of, font), !.
object_to_term(String, _, Atom) :-
	send(String, instance_of, char_array), !,
	get(String, value, Atom).
object_to_term(Obj, Mapping, Term) :-
	get(Obj, '_class_name', Class),
	class_map(Class, TermDef, Attributes),
	TermDef =.. [Name|InitAttributes],
	init_attributes(InitAttributes, Obj, Mapping, InitTerms),
	Term0 =.. [Name|InitTerms],
	(   memberchk(Obj = Var, Mapping)
	->  Term1 = new(Var, Term0)
	;   Term1 = Term0
	),
	modifiers(Attributes, Obj, Mapping, Term1, Term2),
	(   atom(Term2)
	->  Term = new(Term2)
	;   Term = Term2
	).

init_attributes([], _, _, []).
init_attributes([H|T0], Obj, Mapping, [A|T]) :-
	init_attribute(H, Obj, Mapping, A),
	init_attributes(T0, Obj, Mapping, T).

init_attribute(H, Obj, Mapping, A) :-
	atom(H), !,
	get(Obj, H, A0),
	object_to_term(A0, Mapping, A).
init_attribute(H, _, _, H).

modifiers([], _, _, Term, Term) :- !.
modifiers(Attributes, Obj, Mapping, Term0, Term) :-
	(   Term0 = new(_, Term1)
	->  true
	;   Term1 = Term0
	),
	new(Cmp, Term1),
	modifiers(Attributes, Obj, Cmp, Mapping, Term0, Term2),
	simplify_attributes(Term2, Term),
	free(Cmp).

modifiers([], _, _, _, Term, Term).
modifiers([S|T], Obj, Cmp, Mapping, Term0, Term) :-
	get(Obj, S, V0),
	get(Cmp, S, V1),
	(   send(V0, equal, V1)
	->  modifiers(T, Obj, Cmp, Mapping, Term0, Term)
	;   object_to_term(V0, Mapping, V),
	    Att =.. [S,V],
	    modifiers(T, Obj, Cmp, Mapping, Term0+Att, Term)
	).

objects_to_terms([], _, []).
objects_to_terms([H0|T0], Mapping, [H|T]) :-
	object_to_term(H0, Mapping, H),
	objects_to_terms(T0, Mapping, T).


simplify_attributes(Term0+first_arrow(@draw_default_arrow)
		         +second_arrow(@draw_default_arrow),
		    Term1+arrows(both)) :- !,
	simplify_attributes(Term0, Term1).
simplify_attributes(Term0+first_arrow(@draw_default_arrow),
		    Term1+arrows(first)) :- !,
	simplify_attributes(Term0, Term1).
simplify_attributes(Term0+second_arrow(@draw_default_arrow),
		    Term1+arrows(second)) :- !,
	simplify_attributes(Term0, Term1).
simplify_attributes(Term0+colour(colour(Name)),
		    Term1+colour(Name)) :- !,
	simplify_attributes(Term0, Term1).
simplify_attributes(Term, Term).

		 /*******************************
		 *	      CLASSES		*
		 *******************************/

resource(drawing, image, image('16x16/drawing.xpm')).

:- pce_begin_class(draw_drag_drawing, bitmap,
		   "Draw to drop the drawing in a PceEmacs Window").

initialise(DD) :->
	send(DD, send_super, initialise, resource(drawing)),
	send(DD, help_message, tag, 'Export drawing as Prolog Source').

:- pce_global(@draw_drag_drawing_recogniser,
	      make_draw_drag_drawing_recogniser).

make_draw_drag_drawing_recogniser(G) :-
	new(DD, drag_and_drop_gesture),
	new(PG, popup_gesture(new(P, popup))),
	send_list(P, append,
		  [ menu_item(help, message(@arg1, give_help)),
		    menu_item(copy_to_clipboard, message(@arg1, copy))
		  ]),
	new(G, handler_group(DD, PG)).

event(DD, Ev:event) :->
	(   send(@draw_drag_drawing_recogniser, event, Ev)
	;   send(DD, send_super, event, Ev)
	).

prolog_source(DD, Source:string) :<-
	get(DD, frame, Draw),
	get(Draw, canvas, Canvas),
	describe_drawing(Canvas, DrawingTerm),
	new(TB, text_buffer),
	pce_open(TB, write, Fd),
	pretty_print(Fd, DrawingTerm),
	close(Fd),
	get(TB, contents, Source),
	free(TB).

copy(DD) :->
	"Export drawing in the selection"::
	get(DD, prolog_source, String),
	send(@display, copy, String),
	send(DD, report, status, 'Drawing exported to clipboard').

give_help(_) :->
	"Jump to help-page"::
	send(@helper, give_help, pcedraw, exportpl).

:- pce_end_class.

		 /*******************************
		 *	  PRETTY PRINTING	*
		 *******************************/

pretty_print(Term) :-
	current_output(Fd),
	pretty_print(Fd, Term).

pretty_print(Fd, Term) :-
	line_position(Fd, Pos),
	pretty_print(Term, Fd, Pos).

pretty_print(Term, Fd, Pos) :-
	numbervars(Term, 0, _),
	pp(Term, Fd, Pos, 1200),
	fail.
pretty_print(_, _, _).

pp(Term, Fd, Pos, _Pri) :-
	sformat(Tmp, '~q', [Term]),
	atom_length(Tmp, N),
	Pos + N =< 72, !,
	format(Fd, '~s', [Tmp]).
pp([], Fd, _, _) :- !,
	format(Fd, [], []).
pp(Term, Fd, Pos, _Pri) :-
	is_list(Term), !,
	format(Fd, '[ ', []),
	NPos is Pos + 2,
	pplist(Term, Fd, NPos),
	indent(Fd, Pos),
	format(Fd, ']', []).
pp(Term, Fd, Pos, _Pri) :-
	functor(Term, Name, _),
	current_op(OpPri, Type, Name),
	ppop(Type, OpPri, Term, Name, Fd, Pos), !.
pp(Term, Fd, Pos, _Pri) :-
	functor(Term, Name, Arity), !,
	format(Fd, '~q(', [Name]),
	atom_length(Name, NL),
	NPos is Pos + NL + 1,
	ppargs(0, Arity, Term, Fd, NPos),
	format(Fd, ')', []).
pp(Term, Fd, _Pos, _Pri) :-
	format(Fd, '~q', [Term]).

pplist([], _, _).
pplist([H|T], Fd, Pos) :-
	pp(H, Fd, Pos, 999),
	(   T = [_|_]
	->  format(Fd, ',', []),
	    indent(Fd, Pos),
	    pplist(T, Fd, Pos)
	;   T == []
	->  true
	;   ListPos is Pos - 2,
	    indent(Fd, ListPos),
	    format(Fd, '| ', []),
	    pp(T, Fd, Pos, 999)
	).

ppop(yfx, OpPri, Term, Op, Fd, Pos) :-
	ppyfx(Term, OpPri, Op, Fd, Pos).

ppyfx(Term, OpPri, Op, Fd, Pos) :-
	functor(Term, Op, 2), !,
	arg(1, Term, A1),
	ppyfx(A1, OpPri, Op, Fd, Pos),
	format(Fd, ' ~w', [Op]),
	NPos is Pos + 2,
	indent(Fd, NPos),
	arg(2, Term, A2),
	APri is OpPri-1,
	pp(A2, Fd, NPos, APri).
ppyfx(Term, OpPri, _, Fd, Pos) :-
	pp(Term, Fd, Pos, OpPri).

ppargs(N, N, _, _, _) :- !.
ppargs(N, Arity, Term, Fd, Pos) :-
	A is N + 1,
	arg(A, Term, Arg),
	pp(Arg, Fd, Pos, 999),
	(   A < Arity
	->  format(Fd, ',', []),
	    indent(Fd, Pos),
	    ppargs(A, Arity, Term, Fd, Pos)
	;   true
	).

indent(Fd, N) :-
	nl(Fd),
	Tabs is N // 8,
	Spaces is N mod 8,
	putn(Tabs, Fd, 9),
	putn(Spaces, Fd, 32).

putn(0, _, _) :- !.
putn(N, Fd, C) :-
	put(Fd, C),
	NN is N - 1,
	putn(NN, Fd, C).
	
