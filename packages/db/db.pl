/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
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

:- module(db,
	  [ db_open/4,			% +File, +Mode, -Handle, +Options
	    db_close/1,			% +Handle
	    db_closeall/0,		% 
	    db_put/3,			% +DB, +Key, +Value
	    db_del/3,			% +DB, +Key, ?Value
	    db_delall/3,		% +DB, +Key, +Value
	    db_enum/3,			% +DB, -Key, -Value
	    db_get/3,			% +DB, +Key, -Value
	    db_getall/3,		% +DB, +Key, -ValueList
	    db_init/1,			% +Options
	    db_transaction/1,		% :Goal
	    db_atom/3			% +DB, ?Atom, ?Id
	  ]).
:- initialization
   load_foreign_library(foreign(db4pl)).

db_delall(DB, Key, Value) :-
	var(Value), !,
	db_del(DB, Key).		% this is much faster
db_delall(DB, Key, Value) :-
	(   db_del(DB, Key, Value),
	    fail
	;   true
	).

:- at_halt(db_closeall).

		 /*******************************
		 *	       MESSAGES		*
		 *******************************/

:- multifile
	prolog:message/3.

prolog:message(error(package(db, Code), context(_, Message))) -->
	[ 'DB: Error ~w: ~w'-[Code, Message] ].
