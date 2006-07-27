/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org/packages/xpce/
    Copyright (C): 2006, University of Amsterdam

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

:- module(emacs_dde_server,
	  [ start_emacs_dde_server/1,	% +Force
	    win_register_emacs/0	% +Externsion
	  ]).

%	start_emacs_dde_server
%	
%	If there is no DDE server, register it as PceEmacs using the
%	topic `control'.

start_emacs_dde_server(_) :-
	dde_current_service('PceEmacs', control), !.
start_emacs_dde_server(true) :-
	open_dde_conversation('PceEmacs', control, Handle),
	dde_execute(Handle, 'close-server'),
	close_dde_conversation(Handle),
	send(@emacs, report, status, 'Closed server on other PceEmacs'),
	fail.
start_emacs_dde_server(false) :-
	open_dde_conversation('PceEmacs', control, Handle), !,
	close_dde_conversation(Handle),
	send(@emacs, report, status, 'Server on other PceEmacs').
start_emacs_dde_server(_) :-
	dde_register_service('PceEmacs'(control, Item),
			     handle_request(Item)).

handle_request(Item) :-
	atom_concat('edit ', WinFile, Item), !,
	prolog_to_os_filename(File, WinFile),
	new(B, emacs_buffer(File)),
	new(W, emacs_frame(B)),
	send(W, sticky_window),
	send(B, check_modified_file).
handle_request('close-server') :-
	dde_unregister_service('PceEmacs'),
	send(@emacs, report, status, 'Closed DDE server').
handle_request(Item) :-
	format(user_error, 'PceEmacs DDE server: unknown request: ~q', [Item]),
	fail.

win_register_emacs :-
	current_prolog_flag(argv, [Me|_]),
	shell_register_dde('prolog.type', edit,
			   'PceEmacs', control, 'edit %1', Me).
	
