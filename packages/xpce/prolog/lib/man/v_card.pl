/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/


:- module(man_card_editor,
	[ 
	]).

:- use_module(library(pce)).
:- use_module(util).

:- pce_autoload(behaviour_item, library('man/behaviour_item')).

:- pce_begin_class(man_card_editor,	man_frame,
		   "Display textual and relational attributes").

		/********************************
		*            CREATE		*
		********************************/

initialise(CE, Manual:man_manual) :->
	"Create from manual"::
	send(CE, send_super, initialise, Manual, 'Card Viewer'),

	send(CE, append, new(TE, man_editor)),
	send(TE, name, editor),
	send(new(D, dialog), below, TE),
	fill_dialog(D),

	send(CE, create),			  % compute layout before
						  % setting selection
	send(CE, edit_mode, Manual?edit_mode),
	send(CE, selected, Manual?selection).


fill_dialog(D) :-
	send(D, name, dialog),
	get(D, frame, CE),

	send(D, append, button(help, message(CE, help))),
	send(D, append, button(quit, message(CE, quit))),
	send(D, append,
	     new(Goto, behaviour_item(goto, '',
				      if(@arg1 \== @nil,
					 message(CE, request_selection,
						 @arg1)))),
	     right),
	send(Goto, advance, clear),
	send(D, append, new(label), right).	% reporter


		/********************************
		*          READ STATUS		*
		********************************/

editor(CE, Editor) :<-
	"Text attribute editor"::
	get(CE, member, editor, Editor).


		/********************************
		*           EDIT MODE		*
		********************************/

edit_mode(CE, Val:bool) :->
	"Switch editors edit_mode"::
	get(CE, editor, Editor),
	(   Val == @off
	->  send(Editor, save_if_modified),
	    send(CE, label, 'Card Viewer', 'Card Viewer')
	;   send(CE, label, 'Card Editor', 'Card Editor')
	),
	send(Editor, edit_mode, Val).

		 /*******************************
		 *	     SELECTION		*
		 *******************************/

selected(CE, Obj:object) :->
	"Display selected object"::
	send(CE?editor, selection, Obj).

:- pce_end_class.

