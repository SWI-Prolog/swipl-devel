/*  $Id$

    Part of SWI-Prolog
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1999 University of Amsterdam. All rights reserved.
*/

:- module(socket,
	  [ tcp_socket/1,		% -Socket
	    tcp_close_socket/1,		% +Socket
	    tcp_open_socket/3,		% +Socket, -Read, -Write
	    tcp_connect/2,		% +Socket, -Address
	    tcp_bind/2,			% +Socket, +Address
	    tcp_accept/3,		% +Master, -Slave, -PeerName
	    tcp_listen/2,		% +Socket, +BackLog
	    tcp_fcntl/3,		% +Socket, +Command, ?Arg
	    tcp_host_to_address/2	% ?HostName, ?Ip-nr
	  ]).
:- use_module(library(shlib)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
These predicates are documented in the source-distribution of the package
`clib'.  See also the SWI-Prolog home-page at

	http://www.swi.psy.uva.nl/projects/SWI-Prolog/
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- initialization
   load_foreign_library(foreign(socket), install_socket).
