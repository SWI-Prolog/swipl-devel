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

:- module(start_emacs,
	  [ emacs/0
	  , emacs/1				% x File
	  , start_emacs/0
	  , emacs_server/0
	  , emacs_toplevel/0
	  ]).
:- use_module(library(pce)).
:- require([ append/3
	   , maplist/3
	   , unix/1
	   ]).

:- pce_autoload(emacs,	    library('emacs/emacs')).
:- pce_autoload(emacs_view, library('emacs/emacs')).

:- pce_global(@emacs_buffers, new(dict)).
:- pce_global(@emacs, new(emacs(@emacs_buffers))).


start_emacs :-
	send(@emacs, start).


emacs_server :-
	start_emacs,
	send(@pce, trap_errors, @off),
	send(@pce, console_label, 'PceEmacs Server').


emacs :-
	start_emacs,
	new(Scratch, emacs_buffer(@nil, '*scratch*')),
	send(Scratch, open).
	

emacs(File) :-
	start_emacs,
	new(B, emacs_buffer(File)),
	send(B, open).

emacs_toplevel :-
	send(@pce, trap_errors, @off),
	current_prolog_flag(argv, Argv),
	files(Argv, Files),
	(   Files = [_|_]
	->  start_emacs,
	    maplist(make_buffer, Files, [B0|_]),
	    send(B0, open)
	;   emacs
	).

make_buffer(File, Buffer) :-
	new(Buffer, emacs_buffer(File)).

files(List, Files) :-
	append(_, ['--'|Files], List), !.
files(List, Files) :-
	append(_, ['-x', _State|Files], List), !.
files([_Prog|Files], Files).
