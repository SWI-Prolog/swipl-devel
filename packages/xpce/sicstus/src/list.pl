/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1999 University of Amsterdam. All rights reserved.
*/

% NOTE: This file must be *compiled* for this to work.

list(Id) :-
	Head = send_implementation(Id, _, _),
	prolog:'$current_clauses'(Head, Root, pce_principal),
	prolog:'$current_instance'(Head, Body, Root, _Ptr),
	portray_clause((Head :- Body)).
