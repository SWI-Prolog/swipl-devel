/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This file may  be make screendumps  of applications with  one or  more
popup menu's on  it.  To include a  popup in the screendump,  activate
the popup  as  normal.  If  the  control-key is  depressed  while  the
mouse-button is released  the popup menu  will not disappear  from the
screen.

After making the  screendump the popup may be  removed by  clicking it
with the same mouse-button as the one that opened it,

This file may be used together with PceDraw's `Import Frame' option to
created annotated screendumps.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- module(stayup_popup, []).
:- use_module(library(pce)).

:- pce_extend_class(popup).

close(P) :->
	"No-op if control- is depressed"::
	(   send(@event, has_modifier, c)
	->  true
	;   (   get(P, slot, pullright, PR), PR \== @nil
	    ->  send(PR, close),
	        send(P, slot, pullright, @nil)
	    ;   true
	    ),
	    (   get(P, device, Dev), Dev \== @nil
	    ->  send(Dev, show, @off),
	        send(Dev, sensitive, @off),
		send(Dev, clear)
	    )
	).

:- pce_end_class.
	
