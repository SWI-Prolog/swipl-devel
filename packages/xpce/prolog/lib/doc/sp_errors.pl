/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

:- module(sp_errors, []).

:- multifile
	user:message_hook/3.

:- pce_global(@sp_warnings, make_sp_warning_list).

make_sp_warning_list(L) :-
	new(L, emacs_hit_list('SP (XML/SGML) warnings')),
	send(L, clear_on_append, @on),
	send(L, expose_on_append, @on),
	send(L, message, caret).

user:message_hook(sp(File, Line, LinePos, Message), _, _Lines) :-
	start_emacs,
	new(Buffer, emacs_buffer(File)),
	get(Buffer, scan, 0, line, Line-1, start, SOL),
	Pos is SOL + LinePos,
	send(@sp_warnings, append_hit, Buffer, Pos, @default, Message),
	fail.
