/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 2000 University of Amsterdam. All rights reserved.
*/

:- module(db,
	  [ db_open/4,			% +File, +Mode, -Handle, +Options
	    db_close/1,			% +Handle
	    db_closeall/0,		% 
	    db_put/3,			% +DB, +Key, +Value
	    db_del/2,			% +DB, +Key
	    db_del/3,			% +DB, +Key, ?Value
	    db_delall/3,		% +DB, +Key, +Value
	    db_get/3,			% +DB, +Key, -Value
	    db_getall/3,		% +DB, +Key, -ValueList
	    db_init/1,			% +Options
	    db_transaction/1,		% :Goal
	    db_atom/3			% +DB, ?Atom, ?Id
	  ]).
:- load_foreign_library(db4pl).

db_delall(DB, Key, Value) :-
	var(Value), !,
	db_del(DB, Key).		% this is much faster
db_delall(DB, Key, Value) :-
	(   db_del(DB, Key, Value),
	    fail
	;   true
	).

:- at_halt(db_closeall).
