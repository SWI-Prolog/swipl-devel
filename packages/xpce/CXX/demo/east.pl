/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

:- (object(@v_form_class) -> true ; pceload(east)).

east :-
	send(new(@p, picture), open),
	send(@p, recogniser, new(Kb, key_binding)),
	send(Kb, function, o,
	     message(@p, display,
		     create(v_form, open, hello), @event?position)),
	send(Kb, function, y,
	     message(@p, display,
		     create(v_form, yesno, hello), @event?position)),
	send(Kb, function, '3',
	     message(@p, display,
		     create(v_form, multi3, hello), @event?position)),
	send(Kb, function, '5',
	     message(@p, display,
		     create(v_form, multi5, hello), @event?position)).
