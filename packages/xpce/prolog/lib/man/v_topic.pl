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


:- module(man_topic, []).

:- use_module(library(pce)).
:- require([ get_chain/3
	   , member/2
	   , send_list/3
	   ]).

:- pce_begin_class(man_topic_browser, man_frame,
		   "Topic browser").

variable(topics,	man_module,		get, "`Topic' manual module").
variable(selection,	man_topic_card*,	get, "Selected topic").

initialise(TB, Manual:man_manual) :->
	"Create from Manual"::
	send(TB, send_super, initialise, Manual, 'Topic Browser'),

	get(Manual, module, topics, @on, Module),
	send(TB, slot, topics, Module),
	
	picture(Picture, Module),
	dialog(Dialog),

	send(TB, append, Picture),
	send(Dialog, below, Picture),
	send(TB, edit_mode, Manual?edit_mode),
	send(TB, expand_node, TB?tree?root),
	
	send(TB, open).


		/********************************
		*            DIALOG		*
		********************************/

dialog(D) :-
	new(D, dialog),
	new(TB, D?frame),

	send(D, append, new(TN, text_item(name,    '', @nil))),
	send(D, append, new(TS, text_item(summary, '',
					  if(TN?selection \== '',
					     message(D?create_member,
						     execute))))),
	send(TN, length, 15),
	send(TS, length, 40),
	send(D, append, button(create, block(message(TB, create_topic,
						     TN?selection,
						     TS?selection),
					     message(TN, clear),
					     message(TS, clear),
					     message(TN, caret)))),
	send(D, append, button(help,   message(TB, help))),
	send(D, append, button(quit,   message(TB, quit))).



		/********************************
		*          COMMUNICATION	*
		********************************/

edit_mode(TB, Val:bool) :->
	"Switch edit mode on/off"::
	get(TB, dialog_member, Dialog),
	send_list([ Dialog?create_member
		  , Dialog?name_member
		  , Dialog?summary_member
		  ],
		  active, Val).
	

node(TB, Card:man_topic_card, Node) :<-
	"Find node displaying card"::
	get(TB?tree, find, @arg1?card == Card, Node).


selected(TB, Obj:object*) :->
	"Set the selection"::
	get(TB, tree, Tree),
	send(Tree, for_all, message(@arg1, inverted, @off)),
	(   Obj \== @nil,
	    send(Obj, instance_of, man_topic_card)
	->  send(TB, slot, selection, Obj),
	    (   get(TB, node, Obj, Node)
	    ->  send(Node, inverted, @on)
	    ;   true
	    )
	;   send(TB, slot, selection, @nil)
	).


release_selection(TB) :->
	send(TB, selected, @nil).


unrelated(TB, From:object*, Rel:name, To:object*) :->
	"Trap delated relations"::
	send(TB, related, From, Rel, To).


related(TB, From:object*, Rel:name, _To:object*) :->
	"Trap added relations"::
	Rel == see_also,
	get(TB, current, From),
	send(TB, update_related).


		/********************************
		*            EDITING		*
		********************************/

create_topic(TB, Name:string, Summary:string, Super:[man_topic_card]) :->
	"Add a new topic from name and summary"::
	(   Super == @default
	->  get(TB, selection, SuperTopic),
	    (   SuperTopic == @nil
	    ->  send(@display, inform, 'Please first select a super-topic'),
	        fail
	    ;   true
	    )
	;   SuperTopic = Super
	),
	send(Name, strip),
	(   get(Name, size, 0)
	->  send(@display, inform, 'Please enter a topic name first')
	;   new(Topic, man_topic_card(TB?topics, Name)),
	    send(Topic, store, summary, Summary),
	    send(SuperTopic, relate, subs, Topic),
	    send(Topic, relate, super, SuperTopic),
	    get(TB, node, SuperTopic, SuperNode),
	    send(SuperNode, font, font(helvetica, bold, 12)),
	    add_card(SuperNode, Topic)
	).


rename_node(TB, Node:node) :->
	"Rename node to name in dialog"::
	get(TB?dialog_member?name_member, selection, NewName),
	(   NewName == ''
	->  send(@display, inform, 'First type a name in ''Name''')
	;   send(Node?card, store, name, NewName),
	    send(Node, string, NewName),
	    send(TB?dialog_member?name_member, clear)
	),
	get(TB?dialog_member?summary_member, selection, NewName),
	(   NewSumm == ''
	->  true
	;   send(Node?card, store, summary, NewSumm),
	    send(TB?dialog_member?summary_member, clear)
	).


delete_card(_TB, Node:node) :->
	"Destroy a card"::
	get(Node, card, Card),
	send(@display, confirm, 'Really delete card `%s''', Card?name),
	(   get(Card, related, subs, Subs),
	    \+ send(Subs, empty)
	->  send(@display, inform, 'Only leaf-nodes can be deleted')
	;   send(Node, delete_tree),
	    send(Card, free)
	).


son_node(TB, Node:node) :->
	"Relate node to selection"::
	get(TB, selection, Selection), Selection \== @nil,
	send(Selection, relate, subs, Node?card),
	send(Node?card, relate, super, Selection),
	send(?(TB, node, Selection), son, Node).


remove_son_node(TB, Node:node) :->
	"Unrelate a node from selection"::
	get(TB, selection, Selection), Selection \== @nil,
	send(Selection, unrelate, subs, Node?card),
	send(Node?card, unrelate, super, Selection),
	send(?(TB, node, Selection), unrelate, Node).
	

below_node(TB, Node:node) :->
	"Move node below selection or to be the first"::
	get_chain(Node, parents, Parents),
	(   get(TB, selection, Selection),
	    Selection \== @nil,
	    get(TB, node, Selection, SelectedNode),
	    member(Parent, Parents),
	    send(Parent?sons, member, SelectedNode)
	->  send(Parent?card, move_relation_after, subs, Node?card, Selection),
	    send(Node, move_after, SelectedNode)
	;   member(Parent, Parents)
	->  send(Parent?card, move_relation_after, subs, Node?card),
	    send(Node, move_after)
	),
	send(TB, request_selection, Node?card, @off).


		/********************************
		*            PICTURE		*
		********************************/

:- pce_global(@man_topic_node_handler, make_man_topic_node_handler).

make_man_topic_node_handler(H) :-
	new(TB, @arg1?frame),
	new(Manual, TB?manual),
	new(CanEdit, Manual?edit_mode == @on),
	new(Selection, Manual?selection),
	Node = @arg1,
	new(Card, Node?card),

	new(P, popup),
	send_list(P, append,
		  [ menu_item(select,
			      message(TB, request_selection, Card, @on),
			      @default, @on,
			      Selection \== Card)
		  , menu_item(expand,
			      message(TB, expand_node, Node),
			      @default, @off,
			      and(message(Node?sons, empty),
				  ?(Card, man_related, subs)))
		  , menu_item(expand_tree,
			      message(TB, expand_tree, Node),
			      @default, @off,
			      ?(Card, man_related, subs))
		  , menu_item(collapse_node,
			      message(TB, collapse_node, Node),
			      @default, @on,
			      not(message(Node?sons, empty)))
		  , menu_item(relate,
			      message(TB, request_relate, Card),
			      @default, @off,
			      and(CanEdit,
				  Selection \== @nil,
				  Selection \== Card,
				  not(message(Card, man_related,
					      see_also, Selection))))
		  , menu_item(rename,
			      message(TB, rename_node, Node),
			      @default, @on,
			      and(CanEdit,
				  TB?dialog_member?name_member?selection \== ''))
		  , menu_item(move_below,
			      message(TB, below_node, Node),
			      @default, @off,
			      CanEdit)
		  , menu_item(make_daughter,
			      message(TB, son_node, Node),
			      @default, @off,
			      CanEdit)
		  , menu_item(remove_daughter,
			      message(TB, remove_son_node, Node),
			      @default, @on,
			      CanEdit)
		  , menu_item(delete_card,
			      message(TB, delete_card, Node),
			      @default, @off,
			      CanEdit)
		  ]),

	HNode = @receiver,
	new(HTool, HNode?frame),

	new(H, handler_group(popup_gesture(P),
			     click_gesture(left, c, single,
					   message(HTool, below_node, HNode)),
			     click_gesture(left, '', single,
					   message(HTool, request_selection,
						   HNode?card, @off)),
			     click_gesture(left, '', double,
					   message(HTool, request_selection,
						   HNode?card, @on)))).
			     

picture(P, Topics) :-
	new(P, picture),
	(   get(Topics, card, root, Root)
	->  true
	;   new(Root, man_topic_card(Topics, 'Contents', root)),
	    send(Root, store, summary, string("Root of the topic index"))
	),
	create_node(Root, Node),
	send(P, display, new(T, tree(Node))),
	send(T, level_gap, 25),
	send(T, node_handler, @man_topic_node_handler).


create_node(Card, Node) :-
	(   get(Card, related, subs, Subs),
	    Subs \== @nil,
	    \+ send(Subs, empty)
	->  Font = font(helvetica, bold,  12)
	;   Font = font(helvetica, roman, 12)
	), 
	new(Node, node(text(Card?name, left, Font))),
	send(Node, attribute, attribute(card, Card)).
	

tree(TB, Tree) :<-
	"Get the associated tree object"::
	get(TB?picture_member, tree_member, Tree).


expand_node(_TB, Node:node) :->
	"Make all sub-topics visible"::
	(   get(Node?card, related, subs, SubCards)
	->  send(SubCards, for_all, message(@prolog, add_card, Node, @arg1))
	;   true
	).

add_card(Node, Card) :-
	(   get(Node?sons, find, @arg1?card == Card, _)
	->  true
	;   create_node(Card, Sub),
	    send(Node, son, Sub)
	).


expand_tree(TB, Node:node) :->
	"Expand all nodes below this one"::
	send(TB, expand_node, Node),
	send(Node?sons, for_all, message(TB, expand_tree, @arg1)).


collapse_node(_TB, Node:node) :->
	"Undisplay all subnodes"::
	send(Node?sons, for_all, message(@arg1, delete_tree)).


:- pce_end_class.
