/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

:- pce_begin_class(person, object, "Person super-class").

variable(name,		name,	both,	"Name of the person").
variable(date_of_birth, name,	both,	"Textual description of date").

initialise(P, Name:name, BornAt:name) :->
	send(P, send_super, initialise),
	send(P, name, Name),
	send(P, date_of_birth, BornAt).

father(P, M:male) :<-
	"Get my father"::
	get(P, hypered, father, M).

mother(P, M:female) :<-
	"Get my mother"::
	get(P, hypered, mother, M).

sons(P, Sons:chain) :<-
	"Get my sons"::
	get(P, all_hypers, Hypers),
	new(Sons, chain),
	send(Hypers, for_all,
	     if(@arg1?forward_name == son,
		message(Sons, append, @arg1?to))).

daughters(P, Daughters:chain) :<-
	"Get my daughters"::
	get(P, all_hypers, Hypers),
	new(Daughters, chain),
	send(Hypers, for_all,
	     if(@arg1?forward_name == daughter,
		message(Daughters, append, @arg1?to))).

:- pce_end_class.


:- pce_begin_class(female, person, "Female person").

mary(F, Man:male) :->
	"Marry with me"::
	(   get(F, husband, Man)
	->  send(F, report, error, '%N is already maried to %N', F, Man),
	    fail
	;   new(_, hyper(F, Man, man, woman))
	).

husband(F, Man:male) :<-
	"To whom am I maried?"::
	get(F, hypered, man, Man).

deliver(F, M:male, Name:name, Date:name, Sex:{male,female}, Child:person) :<-
	"Deliver a child"::
	(   Sex == male
	->  new(Child, male(Name, Date)),
	    new(_, hyper(F, Child, son, mother)),
	    new(_, hyper(M, Child, son, father))
	;   new(Child, female(Name, Date)),
	    new(_, hyper(F, Child, daughter, mother)),
	    new(_, hyper(M, Child, daughter, father))
	).

:- pce_end_class.


:- pce_begin_class(male, person, "Male person").

mary(M, F:female) :->
	"Marry with me"::
	send(F, mary, M).

wife(M, Female:female) :<-
	"To whom am I maried?"::
	get(M, hypered, woman, Female).

:- pce_end_class.


