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

:- module(pce_tagged_connection, []).
:- use_module(library(pce)).
:- require([ forall/2
	   ]).

:- pce_begin_class(tagged_connection, connection,
		   "Connection with centered tag").

variable(tag,	graphical*,	get, "Associated tag").

tag(C, Tag:graphical*) :->
	"Associate (new) tag with the connection"::
	get(C, tag, Old),
	(   Old == Tag
	->  true
	;   (	Old \== @nil
	    ->	send(Old, free)
	    ;	true
	    ),
	    send(C, slot, tag, Tag),
	    update_tag(C, _All)
	).


unlink(C) :->
	"Destroy tags"::
	send(C, send_super, unlink),
	get(C, tag, Tag),
	(   Tag \== @nil
	->  free(Tag)
	;   true
	).

device(C, Dev:device*) :->
	"Update the tag"::
	send(C, send_super, device, Dev),
	update_tag(C, device).

displayed(C, Val:bool) :->
	"Update the tag"::
	send(C, send_super, displayed, Val),
	update_tag(C, displayed).

points(C, X1:[int], Y1:[int], X2:[int], Y2:[int]) :->
	"Update the tag"::
	send(C, send_super, points, X1, Y1, X2, Y2),
	update_tag(C, center).

tag_attribute(center).
tag_attribute(device).
tag_attribute(displayed).

update_tag(C, _) :-
	get(C, tag, @nil), !.
update_tag(C, What) :-
	get(C, tag, Tag),
	forall(tag_attribute(What), send(Tag, What, C?What)).

:- pce_end_class.
