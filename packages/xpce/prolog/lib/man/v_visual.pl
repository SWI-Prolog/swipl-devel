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

:- module(pce_visual, []).
:- use_module(library(pce)).
:- use_module(library(toolbar)).
:- use_module(library(pce_report)).
:- use_module(pce_op).			% should move
:- require([ concat_atom/2
	   , ignore/1
	   , portray_object/2
	   , send_list/3
	   , shell/1
	   , term_to_atom/2
	   ]).

:- pce_autoload(tile_hierarchy,	  library('man/v_tile')).
:- pce_autoload(toc_window,	  library(pce_toc)).
:- pce_autoload(select_graphical, v_select).

		/********************************
		*        ICON GENERATION	*
		********************************/

resource(builtin_class,	      image, image('16x16/builtin_class.xpm')).
resource(user_class,	      image, image('16x16/user_class.xpm')).
resource(builtin_class_flash, image, image('16x16/builtin_classflash.xpm')).
resource(user_class_flash,    image, image('16x16/user_classflash.xpm')).
resource(help,		      image, image('16x16/help.xpm')).
resource(grab,		      image, image('16x16/handpoint.xpm')).
resource(vishier,	      image, image('32x32/vishier.xpm')).

:- pce_extend_class(visual).

rc(built_in, flash,   builtin_class_flash).
rc(built_in, noflash, builtin_class).
rc(host,     flash,   user_class_flash).
rc(host,     noflash, user_class).

vis_icon(V, Icon:image) :<-
	Creator = V->>class->>creator,
	(   V->>has_send_method(flash)
	->  Flash = flash
	;   Flash = noflash
	),
	rc(Creator, Flash, RC),
	new(Icon, image(resource(RC))).

vis_icon_label(V, Label:name) :<-
	(   object(V, Term)
	->  term_to_atom(Term, Label)
	;   Label = V->>class_name
	).

vis_expandable(V) :->
	\+ (ContainsMethod = class(graphical)->>get_method(contains),
	    ContainsMethod = V->>class->>get_method(contains)).

:- pce_end_class.


		 /*******************************
		 *	     VIS-WINDOW		*
		 *******************************/

:- pce_begin_class(vis_window, toc_window,
		   "Visual Hierarchy Window").

variable(freed_message,   message, get, "Message to trap destruction").
variable(changed_message, message, get, "Message to trap change").

class_variable(size, size, size(300, 300)).

initialise(V, Root:[visual]) :->
	default(Root, @display_manager, TheRoot),
	V*>>initialise,
	V=>>freed_message(new(F, message(V, freed, @arg2))),
	V=>>changed_message(new(C, message(V, changed, @arg1))),
	F->>debug_class(service),
	C->>debug_class(service),
	Id    = TheRoot->>object_reference,
	Label = TheRoot->>vis_icon_label,
	Icon  = TheRoot->>vis_icon,
	V->>root(toc_folder(Label, Id, Icon, Icon)).

unlink(V) :->
	FMsg = V->>freed_message,
	CMsg = V->>changed_message,
	@classes->>for_some(if(@arg2?freed_messages \== @nil,
			       message(@arg2?freed_messages, delete, FMsg))),
	@classes->>for_some(if(@arg2?changed_messages \== @nil,
			       message(@arg2?changed_messages, delete, CMsg))),
	V*>>unlink.

vis_expandable(_V) :->
	fail.

expand_node(V, Id:'name|int') :->
	Ref  = @pce->>object_from_reference(Id),
	(   Subs = Ref->>contains,
	    \+ Subs->>empty,
	    V->>report(status, '')
	->  Subs->>for_all(message(V, add_visual, @arg1, Id))
	;   V->>report(status, '%O: Empty', Ref)
	).
%	V*>>expand_node(Id).

add_visual(V, Visual:visual, SuperId:[name|int]) :->
	"Add a visual object to the tree"::
	Id = Visual->>object_reference,
	(   _ = V->>node(Id)  
	->  true
	;   (   SuperId == @default
	    ->	TheSuperId = Visual->>contained_in->>object_reference
	    ;	TheSuperId = SuperId
	    ),
	    Label = Visual->>vis_icon_label,
	    Icon  = Visual->>vis_icon,
	    (   Visual->>vis_expandable
	    ->  V->>son(TheSuperId, toc_folder(Label, Id, Icon, Icon))
	    ;   V->>son(TheSuperId, toc_file(Label, Id, Icon))
	    ),
	    V->>prepare(Visual)
	).
	

prepare(V, Visual:visual) :->
	"Ensure the freed-message is trapped"::
	Visual->>'_inspect'(@on),
	Class = Visual->>class,
	FreedMsg   = V->>freed_message,
	ChangedMsg = V->>changed_message,
	Class->>freed_message(FreedMsg),
	Class->>changed_message(ChangedMsg).


freed(V, Visual:visual) :->
	"Handle a freed node"::
	Id   = Visual->>object_reference,
	Node = V->>node(Id),
	Node->>delete_tree.


changed(V, Visual:visual) :->
	"Handle a changed node"::
	Id    = Visual->>object_reference,
	Node  = V->>node(Id),
	Label = Visual->>vis_icon_label,
	Node->>image->>label(Label).


visualise(V, Visual:visual) :->
	"Add this object to the tree"::
	(   Node = V->>node(Visual)
	->  V->>selection(Node)
	;   (   make_path(V, Visual)
	    ->	true
	    ;	V->>report(warning, '%O is not displayed', Visual)
	    )
	).

make_path(V, Visual) :-
	Id = Visual->>object_reference,
	_  = V->>node(Id), !.
make_path(V, Visual) :-
	Super	= Visual->>contained_in,
	SuperId = Super->>object_reference,
	make_path(V, Super),
	V->>add_visual(Visual, SuperId),
	V*>>expand_node(SuperId).

select_node(V, Id:'name|int') :->
	"A node has been selected"::
	Visual = @pce->>object_from_reference(Id),
	(   Visual->>has_send_method(flash)
	->  Visual->>flash
	;   true
	),
	term_to_atom(Visual, Copy),
	@display->>copy(Copy),
	ClassName = Visual->>class_name,
	V->>report(status, 'Class: %s, Reference: @%s', ClassName, Id).

:- free(@vis_node_popup).		% development
:- pce_global(@vis_node_popup, make_vis_node_popup).

make_vis_node_popup(P) :-
	new(V, @arg1?window),
	new(Visual, V?selection),
	new(P, popup(visual, message(@arg2?window, @arg1))),
%	P->>append(menu_item(copy_reference,
%			     end_group := @on)),
	P->>append(menu_item(source,
			     condition := Visual?class?creator == host)),
	P->>append(class_details),
	P->>append(object_details).

popup(_V, _Id:'name|int', @vis_node_popup:popup) :<-
	true.

selection(V, Visual:visual) :<-
	Ref    = V->>tree->>selection->>head->>identifier,
	Visual = @pce->>object_from_reference(Ref).

copy_reference(V) :->
	Visual = V->>selection,
	term_to_atom(Visual, Copy),
	@display->>copy(Copy),
	V->>report(status, 'Copied: "%s"', Copy).

source(V) :->
	Class = V->>selection->>class,
	V->>request_source(Class).

class_details(V) :->
	Class = V->>selection->>class,
	V->>request_tool_focus(Class).

object_details(V) :->
	Visual = V->>selection,
	V->>frame->>manual->>inspect(Visual).

:- pce_end_class.

		 /*******************************
		 *               TOOL		*
		 *******************************/

:- pce_begin_class(vis_frame, man_frame,
		   "Visual Hierarchy Tool").

initialise(F, Manual:man_manual) :->
	F*>>initialise(Manual, 'Visual Hierarchy'),
	F->>icon(resource(vishier)),
%	F->>icon(image('32x32/vishier.xpm')),
%	send(F, icon, resource(vishier)),
	F->>append(new(TD, tool_dialog)),
	send_list(TD, append,
		  [ tool_button(help,
				resource(help),
				'Help on the inspector'),
		    tool_button(grab,
				resource(grab),
				'Select object from screen')
		  ]),
	TD->>append(graphical(0,0,20,0), right),
	TD->>append(new(TI, text_item(add, '',
				      message(F, visualise_from_atom, @arg1))),
		    right),
	TI->>reference(point(0, TI?height)),

	new(V, vis_window)->>below(TD),
	new(report_dialog)->>below(V).
	
window(F, Window:vis_window) :<-
	Window = F->>member(vis_window).

visualise_from_atom(F, Atom:name) :->
	"Handle description of an object as atom"::
	(   term_to_atom(Term, Atom)
	->  (	object(Term)
	    ->  (   Term->>instance_of(visual)
		->  F->>window->>visualise(Term)
		;   F->>report(warning, '%O is not a visual object', Term)
		)
	    ;	F->>report(warning, '%s is not an object', Atom)
	    )
	;   F->>report(warning, '%s: syntax error', Atom)
	).
		    
vis_expandable(_V) :->
	fail.

grab(F) :->
	"Add a new object from the screen"::
	new(D, select_graphical('Select object from screen')),
	D->>attribute(report_to, F),
	Obj = D->>select(@arg1?frame \== F, F?area?center),
	D->>destroy,
	Obj \== @nil,
	F->>window->>visualise(Obj).

:- pce_end_class(vis_frame).
