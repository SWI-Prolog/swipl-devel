/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
\paragraph{Class parse_tree} Defines the tree  object itself. It defines
the  global  visual  characteristics  of  the  tree.  The  hierarchy  is
represented by node objects.  The  root   and  all  underlying nodes are
created directly from the Prolog term   that represents the hierarchy in
Prolog.

Finally, an event-handler is associated  with   all  nodes  that invokes
->clicked on the node after a   left-mouse-button  single click has been
recognised.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(parse_tree, tree).

initialise(T, Tree:prolog) :->
	send(T, send_super, initialise, parse_node(Tree)),
	send(T, direction, list),
	send(T, level_gap, 20),
	send(T, node_handler,
	     click_gesture(left, '', single,
			   message(@event?receiver?node, clicked))).
	
:- pce_end_class.

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
\paragraph{Class parse_tree} contains the magic.  Unlike XPCE version 4,
version 5 can built trees bottom-up, i.e.\  first creating the leaves of
the tree and gradually relating the  leaves   using  nodes higher in the
hierarchy. Finally the whole hierarchy is associated to a tree object.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(parse_node, node).

variable(value,  prolog,  get, "Associated value").

initialise(N, Tree:prolog) :->
	Tree = node(Value, Sons),
	send(N, send_super, initialise, new(text)),
	send(N, value, Value),
	(   Sons == []
	->  send(N, collapsed, @nil)	% do not show [+] mark
	;   forall(member(Son, Sons),
		   send(N, son, parse_node(Son)))
	).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Set the associated value, as well as the label. <Value> is stored in the
<-value instance-variable of type prolog. With  the return of the ->slot
message, the scope in which <Value> is  passed to XPCE ends, and <Value>
is thus recorded into the  Prolog   database.  Later  destruction of the
prolog_term object automatically frees the Prolog record.

Note that the label still needs to be translated to a type acceptable to
XPCE. Passing the  term  directly  forces   the  string  to  convert the
argument to XPCE native data.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

value(N, Value:prolog) :->
	"Set <-value and change label"::
	send(N, slot, value, Value),
	term_to_atom(Value, Label),
	send(N?image, string, Label).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Get the associated term, which  is   retrieved  from the Prolog recorded
database.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

clicked(N) :->
	get(N, value, Value),
	format('User clicked "~p"~n', [Value]).

:- pce_end_class.

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Finally, we define a Prolog tree and  a   the  code to visualise it in a
window.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

tree(node(sentence,
	  [ node(subject(pce), []),
	    node(verb(is), []),
	    node(adjective(nice), [])
	  ])).

show_tree :-
	tree(Tree),
	new(P, picture('Parse Tree')),
	send(P, display, parse_tree(Tree), point(10,10)),
	send(P, open).
