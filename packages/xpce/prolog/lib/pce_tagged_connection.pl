/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

:- module(pce_tagged_connection, []).
:- use_module(library(pce)).
:- require([ forall/2
	   ]).

:- pce_begin_class(tagged_connection, connection,
		   "Connection with centered tag").

variable(tag,	graphical*,	get, "Associated tag").

tag(C, Tag:graphical) :->
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
