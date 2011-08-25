/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2011, University of Amsterdam
			      VU University Amsterdam

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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(win_dde,
	  [ dde_request/3,		% +Handle, +Key, -Value
	    dde_execute/2,		% +Handle, +Command
	    dde_poke/3,			% +Handle, +Item, +Data
	    dde_register_service/2,	% +Template, +Goal
	    dde_unregister_service/1,	% +Service
	    dde_current_service/2,	% -Service, -Topic
	    dde_current_connection/2	% -Service, -Topic
	  ]).

:- meta_predicate
	dde_register_service(+, 0).

/** <module> Windows DDE interface

This module provides both a client and   server interface to Windows DDE
(Dynamic Data Exchange) facilities. DDE provides a proprietary means for
interprocess communication based on Windows  messages. This implies that
applications that wish to use these   predicates  must provide a Windows
`message loop'. Typically, this  is  provided   by  the  GUI frontend of
SWI-Prolog. If SWI-Prolog is embedded,   the  embedding environment must
provide the message loop.

Errors from the DDE layer are mapped to error-terms of this format

  ==
  error(dde_error(Operation, Message), Context)
  ==

The interaction between DDE and threads is   unclear  to us. The current
implementation uses a DDE instance for each   thread that executes a DDE
predicate. Although not protected, we advice  to use a particular handle
only from the thread in which it  was created and to unregister services
from the thread that created them.

@see	library(socket) provides platform independent interprocess
	communication.
*/

		 /*******************************
		 *	      CLIENT		*
		 *******************************/

%%	dde_request(+Handle, +Key, -Value) is det.
%
%	Make a DDE request  with  default   timeout  value  (meaning the
%	request  can  block  forever).   Value    is   unified   with  a
%	string-object (see string_list/2) on success.

dde_request(Handle, Key, Value) :-
	dde_request(Handle, Key, Value, 0).

%%	dde_execute(+Handle, +Command) is det.
%
%	Make a DDE execute request with default timeout value.

dde_execute(Handle, Command) :-
	dde_execute(Handle, Command, 0).

%%	dde_poke(+Handle, +Item, +Data) is det.
%
%	Make a DDE poke request with default timeout value.

dde_poke(Handle, Item, Data) :-
	dde_poke(Handle, Item, Data, 0).


		 /*******************************
		 *	      SERVER		*
		 *******************************/

:- dynamic
	dde_service/6,
	dde_current_connection/3.

%%	dde_register_service(Template, +Goal) is det.
%
%	Register a DDE service  in   this  SWI-Prolog instance. Template
%	takes one of the formats
%
%		$ Service(Topic, Item, Value) :
%		Register a service for dde_request/3
%
%		$ Service(Topic, Command) :
%		Register a service for dde_execute/2
%
%	Topic is either an atom or a variable.  In the latter case, any
%	topic is confirmed on the Service.  Item, Value and Command are
%	variables used to pass the arguments into the Goal.

dde_register_service(Template, Goal) :-
	Template =.. [Service, Topic, Item, Value], !,
	strip_module(Goal, Module, PlainGoal),
	'$dde_register_service'(Service, on),
	asserta(win_dde:dde_service(Service, Topic, Item,
				    Value, Module, PlainGoal)).
dde_register_service(Template, Goal) :-
	Template =.. [Service, Topic, Command], !,
	strip_module(Goal, Module, PlainGoal),
	'$dde_register_service'(Service, on),
	asserta(win_dde:dde_service(Service, Topic, -,
				    Command, Module, PlainGoal)).
dde_register_service(Template, _Goal) :-
	throw(error(type_error(dde_service, Template), _)).

%%	dde_unregister_service(+Service) is det.
%
%	Unregister a previously registered service. Succeeds silently if
%	the service was not registered.
%
%	@see dde_current_service/2.

dde_unregister_service(Service) :-
	(   retract(dde_service(Service, _, _, _, _, _))
	->  '$dde_register_service'(Service, off)
	;   true
	),
	retractall(dde_service(Service, _, _, _, _, _)).

dde_unregister_all_services :-
	dde_current_service(Service, _),
	dde_unregister_service(Service),
	fail ; true.

:- at_halt(dde_unregister_all_services). % required by Windows!

%%	dde_current_service(?Service, ?Topic)
%
%	Unifies Service and Topic with currently supported servers/topics

dde_current_service(Service, Topic) :-
	dde_service(Service, Topic, _, _, _, _).

%%	dde_current_connection(?Service, ?Topic)
%
%	Unifies Service and Topic with the currently open server connections.

dde_current_connection(Service, Topic) :-
	dde_current_connection(_, Service, Topic).

		 /*******************************
		 *	    CALL-BACKS		*
		 *******************************/

%	'$dde_connect'(+Service, +Topic, +IsSelf)
%
%	Called by the DDEML XTYP_CONNECT request.  IsSelf is 0 if the
%	connection is requested by another Prolog, 1 otherwise.

'$dde_connect'(Service, Topic, _Self) :-
	dde_service(Service, Topic, _, _, _, _).

%	'$dde_connect_confirm'(+Service, +Topic, +Handle)
%
%	Called by the DDEML XTYP_CONNECT_CONFIRM request.  Used to update
%	our list of current conversations.

'$dde_connect_confirm'(Service, Topic, Handle) :-
	asserta(dde_current_connection(Handle, Service, Topic)).

%	'$dde_disconnect'(+Handle)
%
%	Called by the DDEML XTYP_DISCONNECT request.  Used to update our
%	list of current conversations.

'$dde_disconnect'(Handle) :-
	retractall(dde_current_connection(Handle, _, _)).

%	'$dde_request'(+Handle, +Topic, +Item, -Answer)
%
%	Called by the DDEML XTYP_REQUEST request.  Answer should be unified
%	with a Prolog object that can be translated into a textual value
%	(atom, string, number or list-of-ascii-values).

'$dde_request'(Handle, Topic, Item, Answer) :-
	dde_current_connection(Handle, Service, Topic),
	dde_service(Service, Topic, Item, Value, Module, Goal), !,
	Module:Goal,
	Answer = Value.
'$dde_request'(_Handle, Topic, _Item, _Answer) :-
	throw(error(existence_error(dde_topic, Topic), _)).

%	'$dde_execute'(+Handle, +Topic, +Command)
%
%	Called by the DDEML XTYP_EXECUTE request.  Command is a SWI-Prolog
%	string object.  Use string_to_atom or string_to_list to convert it
%	into a standard object.

'$dde_execute'(Handle, Topic, Command) :-
	dde_current_connection(Handle, Service, Topic),
	dde_service(Service, Topic, _, Command, Module, Goal), !,
	Module:Goal.
'$dde_execute'(_Handle, Topic, _Command) :-
	throw(error(existence_error(dde_topic, Topic), _)).


		 /*******************************
		 *	       ERRORS		*
		 *******************************/

:- multifile
	prolog:error_message//1.


prolog:error_message(dde_error(Op,Msg)) -->
	[ 'DDE: ~w failed: ~w'-[Op,Msg] ].
