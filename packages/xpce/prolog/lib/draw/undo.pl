/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

:- module(draw_undo, []).
:- use_module(library(pce)).
:- require([ append/3
	   , default/3
	   ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module defines the undo facility   for PceDraw. Undo is implemented
by redefining the methods that  manipulate   the  graphical objects such
that they inform the redo system how  the reverse operation is peformed.
These operations are recorded as XPCE code objects (usually messages).

As one user operation  may  map  into   multiple  actions  to  undo (for
example, moving the selection), a sequence  of action is bracketed using
->open_undo_group and ->close_undo_group messages to  the manager. These
two  methods  maintain  an   <-open_count.    Actions   presented  using
->undo_action are added to an  `and'   object,  which  is XPCE's natural
notion of a sequence  of  actions.   The  `and'  object  currently under
construction is stored in the instance  variable <-action. The logic has
a number of rules  that  avoid   unnecessary  built-up  of  actions. For
example, moving the same object twice  only   requires  the  undo of the
first, storing the original position, to be remembered. This is probably
the most PceDraw dependend part of this class.

The final ->close_undo_group checks whether   the  <-action represents a
non-no-op sequence of actions, and finally adds  the `and' object to the
list of undo actions.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

		 /*******************************
		 *    CLASS DRAW-UNDO-MANAGER	*
		 *******************************/

:- pce_begin_class(draw_undo_manager, chain,
		   "List of undo/redo actions").

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The variable <-length represents the number of steps remembered. This is
currently not implemented. <-report_to is  the   object  that opened us.
This  is  intended  for  sending  ->report   messages  to.  See  `visual
<-report_to' for a description of  this mechanism. <-direction remembers
whether we are `undoing' or `redoing'.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

variable(length,	int,		get,  "#Steps remembered").
variable(at_start,	bool := @off,	get,  "Signals no more undo's").
variable(report_to,	any,		get,  "Normally my client").
variable(action,	and*,		none, "Collected action (sofar)").
variable(open_count,	int,		get,  "Count for opened").
variable(direction,	{forwards,backwards}*, get, "Current undo direction").

initialise(UB, ReportTo:any, Size:[int]) :->
	"Create with Size steps"::
	default(Size, 10, TheSize),
	send(UB, send_super, initialise),
	send(UB, slot, report_to, ReportTo),
	send(UB, slot, length, TheSize),
	send(UB, slot, open_count, 0).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
->open_undo_group associates a new <-action, and resets the undo-pointer
to the tail of the undo list. Further ->open_undo_group simply increment
<-open_count, so actions making a group can   call  each other, and only
the outer-most group, normally invoked  from   the  GUI will combine the
undo messages.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

open_undo_group(UB) :->
	"Add a new entry"::
	get(UB, open_count, OC),
	(   OC == 0
	->  debug('**** New Undo ****~n', []),
	    send(UB, slot, action, new(and)),
	    send(UB, current, @nil),
	    send(UB, slot, at_start, @off)
	;   true
	),
	NC is OC + 1,
	send(UB, slot, open_count, NC).   


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
discardable_undo/1 deals with two  situations:   empty  undos  are cases
where a group was started and ended,  but nothing was actually modified.
The create-resize gestures create objects, and remove them if the object
is smaller then the minimal size. The second clause checks for this.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

discardable_undo(And) :-
	send(And, empty), !.
discardable_undo(And) :-
	get(And, tail, T),
	classify_message(T, cut(Gr)),
	get(And?members, find,
	    message(@prolog, classify_message, @arg1, un_cut, Gr),
	    _).

classify_message(Msg, Action, Object) :-
	Term =.. [Action, Object],
	classify_message(Msg, Term).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
->close_undo_group closes the group opened  by ->open_undo_group. If the
count drops to 0, the  <-actions  is   appended  to  the manager itself.
Special cases are if the group  can   be  discarded  (see above), or the
<-action is a `redo', and  there  is   an  `undo'  just before it. These
couples may be created  by  the   user  scanning  backwards and forwards
through the undo chain for the right spot, and can be deleted.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

close_undo_group(UB) :->
	"Add undo group"::
	get(UB, open_count, OC),
	NC is OC - 1,
	send(UB, slot, open_count, NC),
	(   NC == 0
	->  get(UB, slot, action, Msg),
	    (	discardable_undo(Msg)
	    ->	debug('**** Discarded undo~n', [])
	    ;   (   get(Msg, attribute, undo, forwards),
		    get(UB?tail, attribute, undo, backwards)
		->  send(UB, delete_tail),
		    debug('**** Removed undo/redo pair~n', [])
		;   send(UB, append, Msg),
		    send(UB, slot, action, @nil),
		    send(UB, current, @nil),
		    debug('**** Closed undo~n', [])
		)
	    )
	;   true
	).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
->reset is called from `draw_canvas->reset', which  in turn is called on
aborts and other resets of the system. It clears the grouping system, as
the ->close_undo_group calls will not come.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

reset(UB) :->
	"Reset after abort"::
	send(UB, slot, action, @nil),
	send(UB, slot, open_count, 0),
	send(UB, slot, at_start, @off),
	send(UB, current, @nil).


clear(UB) :->
	send(UB, send_super, clear),
	send(UB, reset).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
classify_message/2 extracts the vital information   from a message, such
that the checking whether messages  may   be  removed can be implemented
easily using Prolog matching rules.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

classification(do_set,    do_set(receiver)).
classification(cut,       cut(receiver)).
classification(un_cut,    un_cut(receiver)).
classification(set_point, set_point(receiver, argument(1))).

classify_message(M, X) :-
	send(M, instance_of, message),
	get(M, selector, Sel),
	classification(Sel, Term),
	functor(Term, Name, Arity),
	functor(X, Name, Arity),
	class_args(0, Arity, M, Term, X).

class_args(Arity, Arity, _, _, _).
class_args(N, Arity, M, In, Out) :-
	NN is N + 1,
	arg(NN, In, What),
	What =.. List,
	append(List, [Val], L2),
	Goal =.. [get, M | L2],
	Goal,
	arg(NN, Out, Val),
	class_args(NN, Arity, M, In, Out).
	
merge(do_set(G), do_set(G)).
merge(set_point(G, P), set_point(G, P)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
->undo_action is called from the various  shape manipulation codes. Most
of the calls come from the shape module.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

undo_action(UB, M:code) :->
	"Add an action to undo"::
	get(UB, slot, action, A),
	(   A \== @nil
	->  (   get(A, head, H),
		classify_message(M, CM),
		classify_message(H, CH),
		merge(CM, CH)
	    ->  debug('~t~16|(merged)~n', [])
	    ;	send(A, prepend, M)
	    )
	;   true
	),
	object(M, Term),
	debug('~t~8|Added to group: ~w~n', [Term]).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This message may be used by toplevel undo-group if the last added action
undos all relevant operations.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

clear_group(UB) :->
	"Empty the current action group"::
	(   get(UB, open_count, 1)	% can only clear on outer
	->  get(UB, slot, action, And),
	    send(And?members, clear)
	;   true
	).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
->undo basically just picks the current undo message and executes it. It
sets up a group, so the `undo of   the  undo' (redo) will be appended to
the chain automatically. `chain <->current'  is   used  to  remember the
current location in the undo chain. If there  is no current, no undo has
been executed previously, and the system will  use the last. If the head
has been executed, <-at_start is set to @on to indicate such.

Finally, <-actions recorded as a result of an undo are marked with their
direction (undo/redo) to be able  to   remove  undo/redo  pairs from the
chain.  See also ->close_undo_group.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

undo(UB) :->
	"Undo the latest action"::
	(   get(UB, at_start, @on)
	->  send(UB, report, warning, 'No further undo')
	;   (   (   get(UB, current, Current)
		;   get(UB, tail, Current)
		)
	    ->  send(UB, open_undo_group),	% reopen for `redo' action
	        send(Current, execute),
		get(UB, slot, action, Action),
		send(Action, attribute, undo, UB?direction),
		(   get(UB, previous, Current, Prev)
		->  true
		;   Prev = @nil
		),
		send(UB, close_undo_group),
		(   Prev \== @nil
		->  send(UB, current, Prev)
		;   send(UB, slot, at_start, @on)
		)
	    )
	).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
->start_undo, ->end_undo and ->direction are used   to  control the undo
process. ->start_undo sets the pointer to the end, so the last operation
will be undone. ->end_undo clears   the <-direction. ->direction changes
the direction of the undo.  Basically,   any  change of direction simply
implies to go back to the end  of   the  chain,  to undo recorded `redo'
operations.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

start_undo(UB) :->
	"Open an undo session"::
	send(UB, slot, direction, backwards),
	send(UB, current, @nil),
	send(UB, slot, at_start, @off).


end_undo(UB) :->
	"Close an undo session"::
	send(UB, slot, direction, @nil).


direction(UB, Dir:{forwards,backwards}) :->
	"Determine undo direction"::
	(   get(UB, direction, Dir)
	->  true
	;   send(UB, slot, direction, Dir),
	    send(UB, current, @nil),
	    send(UB, slot, at_start, @off)
	).


can_undo(UB) :->
	"succeeds if ready for undo"::
	\+ send(UB, empty),
	get(UB, at_start, @off).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
->open opens a visualisation of  the  undo   buffer,  so  the process is
represented in a natural manner to the   user.  The visualiser is made a
`transient' window of PceDraw,  so  the   window  manager  will properly
connect the two windows, and the ->modal  message on the undo visualiser
makes it impossible to interact  with   the  drawing canvas itself while
undoing.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

open(UB, V:[frame]) :->
	"Visualise the status"::
	(   V == @default
	->  send(draw_undo_view(UB), open)
	;   get(V, area, area(X, Y, _W, _H)),
	    new(UV, draw_undo_view(UB)),
	    send(UV, transient_for, V),
	    send(UV, open, point(X+200, Y+30))
	).

:- pce_end_class.


		 /*******************************
		 *	       VISUAL		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The relation between the undo manager   (data object) and the visualiser
is managed by a hyper.  Hypers   guarantee  consistency of the database,
should one of the objects be destroyed,  while they can be programmed to
make the existence of one side being   dependant on the existence of the
other.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(draw_part_hyper, hyper).

initialise(H, Whole:object, Part:object, PartName:[name], WholeName:[name]) :->
	default(PartName, part, PN),
	default(WholeName, whole, WN),
	send(H, send_super, initialise, Whole, Part, PN, WN).

delete_from(H) :->
	get(H, to, Part),
	free(Part),
	free(H).

:- pce_end_class.

		 /*******************************
		 *	CLASS DRAW-UNDO-VIEW    *
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This class defines the rather trivial visualiser for the undo buffer.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(draw_undo_view, dialog,
		   "Window to visualise undo process").

variable(index,	      int,		    get, "Current index").

initialise(UV, UB:draw_undo_manager) :->
	"Create for undo manager"::
	send(UV, send_super, initialise, 'Undo'),
	new(_, draw_part_hyper(UB, UV, visualiser, buffer)),
	Low = 0,
	get(UB, size, High),
	send(UV, slot, index, High),
	send(UV, append, slider(undo, Low, High, High,
				message(UV, goto, @arg1))),
	send(UV, append, button(undo,
				message(UV, undo))),
	send(UV, append, button(redo,
				message(UV, redo))),
	send(UV, append, button(quit,
				and(message(UB, end_undo),
				    message(UV, destroy)))),
	send(UV, modal, transient),
	send(UB, start_undo).
	    

undo_buffer(UV, UB:draw_undo_manager) :<-
	"Find the buffer I am showing"::
	get(UV, hypered, buffer, UB).


index(UV, Idx:int) :->
	get(UV, member, undo, Slider),
	send(Slider, selection, Idx),
	send(UV, slot, index, Idx).


undo(UV) :->
	get(UV, member, undo, Slider),
	get(Slider, low, Low),
	get(UV, index, Here),
	(   Here == Low
	->  send(UV, report, warning, 'No further undo available')
	;   get(UV, undo_buffer, UB),
	    send(UB, direction, backwards),
	    send(UB, undo),
	    NHere is Here - 1,
	    send(UV, index, NHere)
	).
	
redo(UV) :->
	get(UV, member, undo, Slider),
	get(Slider, high, High),
	get(UV, index, Here),
	(   Here == High
	->  send(UV, report, warning, 'At end-point')
	;   get(UV, undo_buffer, UB),
	    send(UB, direction, forwards),
	    send(UB, undo),
	    NHere is Here + 1,
	    send(UV, index, NHere)
	).
	
goto(UV, Goto:int) :->
	goto(UV, Goto).

goto(UV, Goto) :-
	get(UV, index, Here),
	(   Goto > Here
	->  send(UV, redo),
	    goto(UV, Goto)
	;   Goto < Here
	->  send(UV, undo),
	    goto(UV, Goto)
	;   true
	).
	
:- pce_end_class.


		 /*******************************
		 *	       DEBUG		*
		 *******************************/

debug(_, _) :- !.
%debug(Fmt, Args) :-
%	format(Fmt, Args).
