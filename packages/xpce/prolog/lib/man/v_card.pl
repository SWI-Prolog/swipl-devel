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

