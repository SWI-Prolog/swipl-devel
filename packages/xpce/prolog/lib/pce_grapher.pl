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

:- module(pce_grapher,
	  [ grapher/1,			% +Message
	    grapher/2			% +Grapher, +Message
	  ]).
:- style_check(+dollar).		% avoid tracing this module
:- use_module(library(pce)).
:- use_module(library(pce_util)).
:- use_module(library(pce_tagged_connection)).
:- use_module(library(print_graphics)).
:- use_module(library(lists)).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The pce_grapher library is created  to   provide  graph visualisation in
Prolog programs. Operations on  the  grapher   are  undoable  to support
backtracing Prolog predicates. In its simple mode one predicate is used:
grapher/1. The argument is either a single   action or (more commonly) a
list of actions. Actions in  the  simple   mode  of  operation are given
below. Please study the source for more complicated operations.

	* node(Node [, Image])
	Add named node to the graph if it is not already there.  If
	Image is provided it must be an XPCE graphical and will be used
	instead of the default small circle.  If Node already exists and
	Image is provided the image is changed.  Examples:

		node(amsterdam)
		node(amsterdam, box(7,7))
		node(amsterdam, bitmap('amsterdam.gif'))

	* arc(From, To)
	  arc(From, To, Label)
	  arc(From, To, Option ...)
	Add an arc between two nodes.  If one of the two nodes is not
	in the graph it is added to the graph.  If Label is present the
	given label is added to the link.  Options are of the form
	Name := Value where the options below are provided.  Options
	may be in any order but must follow the From and To.

		+ label := Atom
		  As arc(From, To, Label)
		+ pen := Integer
		  Thickness of the drawing pen.  Default is 1.  Must
		  be zero or more.
		+ colour := Colour
		  Colour of the link.  Default is black.  Colours can
		  be specified by name or as '#RRGGBB' where RR, GG and
		  BB are the hexadecimal red, green and blue components.
		  Colournames can be found using the ?- manpce. tool using
		  "File/Demo programs" and selecting one of "Colours" or
		  "HSV Colours"
		+ arrows := Arrows
		  Where Arrows is one of none, first, second or both with
		  the obvious meaning.

	* selected(Node [, Boolean])
	  selected(From, To, [, Boolean])
	Select (highlight) a node or link.  Default is to select the 
	object,  Using @off for Boolean the object is deselected.

	* selection(Node)
	Deselect all nodes and relations and select the specified Node.

	* selection(Nodes)
	Deselect all nodes and relations and select the members of the
	given list of nodes.

	* selection(@nil)
	Deselect all nodes.

	* clear
	Remove everything.  This operation cannot be undone.

	* step
	Wait and display a menu to single step, fast-forward or abort.
	If the stepper-mode is fast_forward or Prolog is in the tracer
	the step operation is ignored.

	* mode(Mode)
	If Mode is `step', the step operation will stop.  If `fast_forward'
	it will simply be a no-op.

	* persist
	Normally used at the end of an action to make a non-backtrackable
	change.

EXAMPLES
========

Assume  we  have  a   predicate    train(From,   To,   Train)  providing
train-connections  between  named  stations  with  a  named  train.  The
following draws the initial graph and if it   exists resets it to a sane
state.

  train_graph :-
	  findall(arc(From, To), train(From, To, _), Arcs),
	  grapher([ clear,		% Clear the graph
		    mode(step),		% Use single stepping
		    Arcs,		% Add the graphs
		    persist		% Do not allow backtracing
		  ]).

To change the circle from the station of departure to a box and select
it, do:

	grapher([ node(Departure, box(7,7)),
		  selection(Departure)
		])

To visualise transition from Here to Next  using a given Train and wait,
use the call below. Arrows := second adds an arrow to the link.

	grapher([ arc(Here, Next, Train, arrows := second),
		  selection(Next),	% Select Next
		  step			% Single step
		])

The exmple above draws the entire station graph before searching a path.
This graph can also be built incrementally.  In this case we initialise
the system using:

	grapher([ clear,
		  node(Departure, box(7,7)),
		  persist,		% persist this
		  selection(Departure)
		])

and we draw the steps using the call   below. Note that we first add the
link, make it persistent, then add the   train information (which can be
undone), select our location and wait for the user.

	grapher([ arc(Here, Next),	% Add arc
		  persist,		% ... persistent
		  arc(Here, Next, Train, arrows := second),
		  selection(Next),	% Select Next
		  step			% Single step
		])


PROBLEMS
========

Besides being written in a hurry and not  yet well tested there are some
integration problems with this code that make it less ideal.

	* Backtracking and cuts
	If the choicepoint of grapher/1 is destroyed using a cut the
	current implementation cannot undo if backtracking happens at
	a higher level.  The current SWI-Prolog implementation doesn't
	give a sensible way to avoid that problem.

	* Tracing
	Although this module is locked as a system module, it is not
	unlikely to make debugging harder due to the extra choicepoints
	created.

	* Undo
	Not all operations can be undo and the undo isn't very relyable
	if arbitrary operations are executed on the grapher.  Notably
	using an undoable operation followed by `clear' will cause troubles
	if the action is actually undone.  Use `clear' only at initialisation.

	* Abort
	Is quite likely to fail from time to time.  This must be fixed in
	the XPCE/SWI-Prolog interaction.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_global(@grapher, make_grapher). :- pce_global(@grapher_undo,
		  new(var('chain*', grapher_undo, @nil))).
:- pce_global(@grapher_app, make_grapher_app).

make_grapher_app(A) :-
	new(A, application(grapher)),
	send(A, kind, service).

make_grapher(G) :-
	send(new(G, grapher), open),
	send(G, wait).

%	grapher(+MessageOrList)
%	
%	Send a message or list of messages to the grapher.  Leaves a
%	choicepoint which undos the modifications if we backtrack into
%	it.

grapher(Message) :-
	grapher(@grapher, Message).

grapher(G, Message) :-
	append(Actions, [persist], Message), !,
	actions(Actions, G),
	send(G, flush).
grapher(G, Message) :-
	new(Undo, chain),
	(   send(@grapher_undo, assign, Undo),
	    call_cleanup(actions(Message, G),
			 send(@grapher_undo, assign, @nil))
	;   send(Undo, for_all,
		 message(@arg1, execute)),
	    fail
	),
	notrace(send(G, flush)).

actions([], _) :- !.
actions([H|T], G) :- !,
	actions(H, G),
	actions(T, G).
actions(persist, _) :- !,
	send(@grapher_undo, clear).
actions(step, G) :- !,
	(   tracing
	->  true
	;   get(G, mode, fast_forward)
	->  true
	;   notrace(get(G, prompt_step, Action)),
	    (	Action == forward
	    ->	true
	    ;	Action == fast_forward
	    ->	send(G, mode, fast_forward)
	    ;	Action == abort
	    ->	abort
	    )
	).
actions(Msg, G) :-
	notrace(send(G, Msg)).

undoable :-
	\+ get(@grapher_undo, '_value', @nil).

		 /*******************************
		 *	   GRAPHER WINDOW	*
		 *******************************/

resource(forward,      image, image('16x16/vcr_forward.xpm')).
resource(fast_forward, image, image('16x16/vcr_fast_forward.xpm')).
resource(layout,       image, image('16x16/graph.xpm')).
resource(abort,        image, library('trace/icons/abort.xpm')).


:- pce_begin_class(grapher, picture,
		   "Picture showing graph").
:- use_class_template(print_graphics).

variable(nodes,	    hash_table := new(hash_table), get,
	 "Id --> node table").
variable(new_nodes, chain := new(chain),           get,
	 "Nodes added since last ->layout").
variable(layouting, bool := @off, get,
	 "Layout is in progress").
variable(mode,      {step,fast_forward} := step,   both,
	 "Mode of operation").

class_variable(size,	size,  size(400,400)).

initialise(G, Label:[name], Size:[size]) :->
	default(Label, 'SWI-Prolog Grapher', TheLabel),
	send_super(G, initialise, TheLabel, Size),
	send(G, application, @grapher_app),
	send(G, create_popup).

:- pce_group(arcs).

arc(G, From:from=name, To:to=name,
    Label:label=[name]*,
    Pen:pen=[int],
    Colour:colour=[colour],
    Arrows:arrows=[{first,second,both}]) :->
	"Add an arc with parameters"::
	get(G, node, From, @on, FN),
	get(G, node, To, @on, TN),
	get(FN, connect, TN, C),
	(   Arrows \== @default,
	    \+ get(C, from_node, FN)
	->  reverse_arrows(Arrows, Arrs)
	;   Arrs = Arrows
	),
	if_provided(C, label,  Label),	% textual or graphics label
	if_provided(C, pen,    Pen),	% thickness of the line
	if_provided(C, colour, Colour),	% colour of the line
	if_provided(C, arrows, Arrs).	% arrows at its ends

reverse_arrows(second, first).
reverse_arrows(first,  second).
reverse_arrows(both,   both).

if_provided(_, _, @default) :- !.
if_provided(Obj, Method, Value) :-
	Msg =.. [Method,Value],
	(   undoable
	->  get(Obj, Method, Old),
	    send(@grapher_undo, prepend,
		 message(Obj, Method, Old))
	;   true
	),
	send(Obj, Msg).

:- pce_group(nodes).

node(G, Name:label=name, Img:image=[image|graphical]) :->
	"Find/create a new node"::
	get(G, node, Name, @on, Img, _Node).

node(G, Name:label=name, Create:create=[bool], Img:image=[image|graphical],
     Node:graph_node) :<-
	"Find/create a new node"::
	get(G, nodes, Nodes),
	(   get(Nodes, member, Name, Node)
	->  (   Img == @default
	    ->	true
	    ;	send(Node, image, Img)
	    )
	;   Create == @on
	->  get(G, create_node, Name, Img, Node),
	    send(G, append, Node),
	    (	undoable
	    ->	send(@grapher_undo, prepend,
		     message(Node, destroy))
	    ;	true
	    )
	).

to_node(G, From:[name|graph_node], Node:graph_node) :<-
	"Convert to a node"::
	(   atom(From)
	->  get(G, node, From, Node)
	;   Node = From
	).

create_node(_G, Name:label=name, Img:image=[image|graphical],
	    Node:graph_node) :<-
	"Create a new node from with given label"::
	new(Node, graph_node(Name, Img)).

:- pce_group(highlight).

%	->selected: From, Selected
%	->selected: From, To, Selected

selected(G, From:name, To:[bool|name], Selected:[bool]) :->
	"Highlight node or connection"::
	default(Selected, @on, Val),
	get(G, node, From, FN),
	(   atom(To)			% an arc
	->  default(Selected, @on, Val),
	    get(G, node, To, TN),
	    get(FN, connected, TN, C),
	    send(C, selected, Val)
	;   default(To, @on, Val)
	->  send(FN, selected, Val)
	).

selection(G, Obj:'name|graphical|chain*') :->
	"Set selection (using undo)"::
	(   undoable
	->  get(G, selection, Old),
	    send(@grapher_undo, prepend,
		 message(G, selection, Old))
	;   true
	),
	(   Obj == @nil
	->  send_super(G, selection, Obj)
	;   atom(Obj)
	->  get(G, node, Obj, Node),
	    send_super(G, selection, Node)
	;   get(Obj, map, ?(G, to_node, @arg1), Nodes),
	    send_super(G, selection, Nodes)
	).

flash(G, From:name, To:[name], Time:[real]) :->
	"Highlight for some time"::
	default(Time, 0.2, Delay),
	send(G, selected, From, To, @on),
	send(timer(Delay), delay),
	send(G, selected, From, To, @off).

:- pce_group(part).

append(G, N:'name|graph_node') :->
	"Display node at computed position"::
	(   atom(N)
	->  get(G, create_node, N, Node)
	;   Node = N
	),
	send(G, place_random, Node),
	send(G, display, Node),
	get(Node, name, Name),
	send(G?nodes, append, Name, Node),
	send(G?new_nodes, append, Node).

deleted_node(G, N:graph_node) :->
	"Node was deleted; update <-nodes"::
	get(N, name, Name),
	send(G?nodes, delete, Name),
	send(G?new_nodes, delete_all, N).

clear(G) :->
	"Really destroy all nodes and arcs"::
	send_super(G, clear, destroy).

:- pce_group(layout).

place_random(G, N:graphical) :->
	"Place N at random location (first in middle)"::
	get(N?area, size, size(W, H)),
	get(G, visible, area(X, Y, PW, PH)),
	(   send(G?graphicals, empty)
	->  GX is X +(PW-W)//2,
	    GY is Y +(PH-H)//2
	;   B is 10,			% Border
	    GX is X + B + random(PW-W-2*B),
	    GY is Y + B + random(PH-H-2*B)
	),
	send(N, set, GX, GY).

layout(D, All:all=[bool], Animate:animate=[bool]) :->
	"Produce automatic layout"::
	send(D, slot, layouting, @on),
	call_cleanup(layout(D, All, Animate),
		     send(D, slot, layouting, @off)).

layout(D, All, Animate) :-
	new(Nodes, chain),
	send(D?graphicals, for_all,
	     if(message(@arg1, instance_of, graph_node),
		message(Nodes, append, @arg1))),
	get(D, visible, Area),
	(   All == @on
	->  MoveOnly = @default,
	    send(D, save_positions, Nodes)
	;   get(D, new_nodes, MoveOnly),
	    send(D, save_positions, MoveOnly)
	),
	(   MoveOnly \== @default,
	    send(MoveOnly, empty)
	->  true
	;   Animate == @off
	->  send(Nodes?head, layout, 2, 40,
		 iterations := 200,
		 area := Area,
		 network := Nodes,
		 move_only := MoveOnly)
	;   Steps = 50,			% Animated move
	    Interations is 200//50,
	    (	between(1, Steps, _),
		send(Nodes?head, layout, 2, 40,
		     iterations := Interations,
		     area := Area,
		     network := Nodes,
		     move_only := MoveOnly),
		(   get(D, request_compute, @nil)
		->  true		% No object has been moved
		;   send(D, flush),
		    sleep(0.01),
		    fail
		)
	    ;	true
	    )
	->  true
	),
	send(D?new_nodes, clear).

save_positions(_D, For:chain) :->
	"Save positions if undoable"::
	(   undoable
	->  chain_list(For, List),
	    (	member(Gr, List),
		get(Gr, position, P),
		send(@grapher_undo, prepend, message(Gr, position, P)),
		fail
	    ;	true
	    )
	;   true
	).

compute(D) :->
	"Incorporate layout of new nodes"::
	(   get(D, layouting, @off),
	    get(D, new_nodes, New),
	    \+ send(New, empty)
	->  send(D, layout, animate := @off)
	;   true
	),
	send_super(D, compute).

reset(D) :->
	"Extend graceful recovery reset after a crash"::
	send_super(D, reset),
	send(D, slot, layouting, @off).

:- pce_group(event).

create_popup(G) :->
	send(G, popup, new(P, popup)),
	new(NonEmpty, not(message(G?graphicals, empty))),
	send_list(P, append,
		  [ menu_item(layout,
			      message(G, layout, @on),
			      condition := NonEmpty),
		    gap,
		    menu_item(print,
			      message(G, print),
			      condition := NonEmpty),
		    menu_item(copy_graph,
			      message(G, copy_graph),
			      condition := @pce?window_system == windows),
		    menu_item(clear,
			      message(G, clear),
			      condition := NonEmpty)
		  ]).

step(G) :->
	"Step for next action"::
	send(G, flush),
	(   get(G, mode, step)
	->  get(G, prompt_step, Action),
	    (	Action == forward
	    ->	true
	    ;	Action == fast_forward
	    ->	send(G, mode, fast_forward)
	    ;	Action == abort
	    ->	abort
	    )
	;   true
	).

prompt_step(G, Reply:{forward,fast_forward,abort}) :<-
	"Prompt for single step operation"::
	send(@display, synchronise),
	new(D, dialog('Step grapher')),
	send(D, gap, size(0,0)),
	send(D, border, size(3,3)),
	send(D, append,
	     new(F, button(forward, message(D, return, forward)))),
	send(D, append,
	     new(FF, button(fast_forward, message(D, return, fast_forward)))),
	send(D, append,
	     new(L, button(layout, message(G, layout, @on)))),
	send(D, append, 
	     new(A, button(abort, message(D, return, abort)))),
	send(F,  label, image(resource(forward))),
	send(FF, label, image(resource(fast_forward))),
	send(A,  label, image(resource(abort))),
	send(L,  label, image(resource(layout))),
	(   true
	->  send(D?tile, border, 0),	% Dubious.  Why is there a tile?
	    send(D, create),
	    get(D, area, area(_,_,DW,DH)),
	    get(G, visible, area(X,Y,W,H)),
	    DX is X+W-DW,
	    DY is Y+H-DH,
	    send(D, do_set, DX, DY),
	    send(G, display, D),
	    get(D, confirm, Reply)
	;   get(D, frame, Frame),
	    send(Frame, kind, popup),
	    send(Frame, create),
	    get(Frame, area, area(_,_,W,H)),
	    get(G, area, area(_,_,DW,DH)),
	    get(G, display_position, point(X,Y)),
	    FX is X+DW-W,
	    FY is Y+DH-H,
	    send(D, transient_for, G?frame),
	    send(D, modal, transient),
	    get(D, confirm, point(FX, FY), Reply)
	),
	send(D, destroy).

:- pce_group(clipboard).


copy_diagram(Canvas) :->
	"Export to the Windows clipboard"::
	new(MF, win_metafile),
	send(MF, draw_in, Canvas?graphicals),
	send(@display, selection_owner, MF,
	     primary,			% which
	     @receiver,			% fetch object
	     message(@receiver, free),	% loose selection
	     emf),
	send(Canvas, report, status, 'Placed graph on clipboard').

:- pce_end_class(grapher).


		 /*******************************
		 *	       NODES		*
		 *******************************/

:- pce_begin_class(graph_node(name), device,
		   "Node in a graph").

variable(highlight, bool := @off, get, "Selected state").

:- pce_global(@graph_node_format, make_graph_node_format).

make_graph_node_format(F) :-
	new(F, format(horizontal, 1, @on)),
	send(F, row_sep, 0),
	send(F, adjustment, vector(center)).

:- pce_global(@graph_north_handle, new(handle(w/2, 0, graph, north))).
:- pce_global(@graph_south_handle, new(handle(w/2, h, graph, south))).
:- pce_global(@graph_west_handle,  new(handle(0, h/2, graph, west))).
:- pce_global(@graph_east_handle,  new(handle(w, h/2, graph, east))).

initialise(N, Name:name, Image:[image|graphical]) :->
	"Create from Name and Image"::
	send_super(N, initialise),
	send(N, name, Name),
	send(N, format, @graph_node_format),
	(   Image == @default
	->  get(N, default_image, Img)
	;   send(Image, instance_of, image)
	->  new(Img, bitmap(Image))
	;   Img = Image
	),
	send(N, prepare_image, Img),
	send(N, display, Img),
	send(N, display, text(Name)).

device(N, Dev:device*) :->
	"Chance device (admin)"::
	(   Dev == @nil,
	    get(N, device, Old),
	    send(Old, instance_of, grapher)
	->  send(Old, deleted_node, N)
	;   true
	),
	send_super(N, device, Dev).

default_image(_N, Img:graphical) :<-
	"Default node image"::
	new(Img, circle(7)),
	send(Img, pen, 2).

prepare_image(_N, Img:graphical) :->
	"Prepare image for creating connections"::
	send_list(Img, handle,
		  [ @graph_north_handle,
		    @graph_south_handle,
		    @graph_west_handle,
		    @graph_east_handle
		  ]),
	send(Img, name, image).

image(N, Img:graphical) :->
	get(N, image, Old),
	(   undoable
	->  send(@grapher_undo, prepend,
		 message(N, image, Old))
	;   true
	),
	send(Old, device, @nil),
	(   get_chain(Old, connections, List),
	    member(C, List),
	    get(C, from, From),
	    get(C, to, To),
	    (	Old == From
	    ->	send(C, relate, Img, To)
	    ;	send(C, relate, From, Img)
	    ),
	    fail
	;   true
	),
	send(N, prepare_image, Img),
	send(N, display, Img),
	send(Img, hide).		% make top one

:- pce_group(part).

image(N, Img:graphical) :<-
	get(N, member, image, Img).

label(N, Label:text) :<-
	get(N, member, text, Label).

:- pce_group(connect).

connect(N, To:graph_node, C:graph_connection) :<-
	"Return existing/create connection"::
	(   get(N, connected, To, C)
	->  true
	;   new(C, graph_connection(N, To))
	).

connect(N, To:graph_node, Label:[name]) :->
	"Create connection with attributes"::
	get(N, connect, To, C),
	send(C, label, Label).

connected(N, To:graph_node, Link:[link], FN:[name], TN:[name],
	  C:graph_connection) :<-
	"Find connection between two nodes"::
	get(N, image, FromImg),
	get(To, image, ToImg),
	get(FromImg, connected, ToImg, Link, FN, TN, C).

:- pce_group(selected).

selected(N, Val:bool) :<-
	get(N, highlight, Val).

selected(N, Val:bool) :->
	"Pretty selected visualisation"::
	get(N, selected, Old),
	(   Val == Old
	->  true
	;   send(N, slot, highlight, Val),
	    send(N?graphicals, for_all,
		 message(@arg1, selected, Val)),
	    (	undoable
	    ->	send(@grapher_undo, prepend,
		     message(N, selected, Old))
	    ;	true
	    )
	).

:- pce_group(event).

:- pce_global(@graph_node_recogniser, make_graph_node_recogniser).
:- pce_global(@graph_node_popup, make_graph_node_popup).
	      
make_graph_node_recogniser(G) :-
	new(C, move_gesture(left)),
	new(P, popup_gesture(@receiver?popup)),
	new(G, handler_group(P, C)).

make_graph_node_popup(P) :-
	Node = @arg1,
	new(P, popup),
	send_list(P, append,
		  [ menu_item(delete,
			      message(Node, destroy))
		  ]).

event(N, Ev:event) :->
	(   send_super(N, event, Ev)
	->  true
	;   send(@graph_node_recogniser, event, Ev)
	).

popup(_, Popup:popup) :<-
	"Popup menu for the node"::
	Popup = @graph_node_popup.

:- pce_end_class(graph_node).


		 /*******************************
		 *	       LINK		*
		 *******************************/

:- pce_global(@graph_link, new(link(graph, graph, @default,
				    graph_connection))).

:- pce_begin_class(graph_connection, tagged_connection,
		   "Connection between two nodes").

variable(highlight, bool := @off, get, "Selected state").
variable(saved_pen, int*,	  get, "Pen saved over selection").

class_variable(label_font, font, italic).

initialise(C, From:graph_node, To:graph_node,
	   Link:[link], FH:[name], TH:[name]) :->
	"Create connection between two graph nodes"::
	default(Link, @graph_link, TheLink),
	get(From, image, IF),
	get(To, image, TF),
	send_super(C, initialise, IF, TF, TheLink, FH, TH).

label(C, Label:[name|graphical]*) :->
	"Label the arc"::
	(   Label == @default		% @default: leave as is
	->  true
	;   Label == @nil		% @nil: no label
	->  send(C, tag, @nil)
	;   atom(Label)			% atom: opaque italic text
	->  get(C, label_font, Font),
	    send(C, tag, new(T, text(Label, center, Font))),
	    send(T, background, @default)
	;   send(C, tag, Label)		% graphical: use as label
	).

label(C, Label:'name|graphical*') :<-
	"Current label"::
	get(C, tag, Tag),
	(   Tag == @nil
	->  Label = @nil
	;   get(Tag, class_name, text)	% dubious.  Should _know_ it is
	->  get(Tag, string, Label)	% a default text
	;   Label = Tag
	).

:- pce_group(selection).

selected(C, Val:bool) :<-
	get(C, highlight, Val).

selected(C, Val:bool) :->
	"Pretty selected visualisation"::
	get(C, selected, Old),
	(   Val == Old
	->  true
	;   send(C, slot, highlight, Val),
	    (	Val == @on
	    ->	get(C, pen, Pen),
		send(C, slot, saved_pen, Pen),
		NewPen is Pen + 1,
		send_super(C, pen, NewPen)
	    ;	get(C, saved_pen, Pen),
		send_super(C, pen, Pen)
	    ),
	    (	get(C, tag, Tag),
		Tag \== @nil
	    ->	send(Tag, selected, Val)
	    ;	true
	    ),
	    (	undoable
	    ->	send(@grapher_undo, prepend,
		     message(C, selected, Old))
	    ;	true
	    )
	).

pen(C, P:'0..') :->
	"Set pen (consider selection)"::
	send(C, slot, saved_pen, P),
	(   get(C, highlight, @on),
	    NP is P + 1
	;   NP = P
	),
	send_super(C, pen, NP).


		 /*******************************
		 *               C		*
		 *******************************/

from_node(C, N:graph_node) :<-
	"Graph-node at `from' side"::
	get(C, from, Img),
	Img \== @nil,
	get(Img, device, N).
	
to_node(C, N:graph_node) :<-
	"Graph-node at `to' side"::
	get(C, to, Img),
	Img \== @nil,
	get(Img, device, N).

:- pce_end_class(graph_connection).

