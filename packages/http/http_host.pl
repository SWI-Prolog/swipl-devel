/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2007-2010, University of Amsterdam,
			      VU University Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(http_host,
	  [ http_current_host/4		% +Request, -Host, -Port, +Options
	  ]).
:- use_module(library('http/thread_httpd')).
:- use_module(library(socket)).
:- use_module(library(option)).
:- use_module(library(settings)).

:- setting(http:public_host, atom, '',
	   'Name the outside world can use to contact me').
:- setting(http:public_port, integer, 80,
	   'Port on the public server').


/** <module> Obtain public server location

This library finds the public address of the running server. This can be
used to construct URLs that are visible   from anywhere on the internet.
This module was introduced to  deal  with   OpenID,  where  a  reques is
redirected to the OpenID server, which in   turn redirects to our server
(see http_openid.pl).

The address is  established  from   the  settings  http:public_host  and
http:public_port if provided. Otherwise it is deduced from the request.
*/


%%	http_current_host(+Request, -Hostname, -Port, Options) is det.
%
%	Current global host and port of the HTTP server.  This is the
%	basis to form absolute address, which we need for redirection
%	based interaction such as the OpenID protocol.  Options are:
%
%	    * global(+Bool)
%	    If =true= (default =false=), try to replace a local hostname
%	    by a world-wide accessible name.

http_current_host(_, Host, Port, _) :-
	setting(http:public_host, PublicHost), PublicHost \== '', !,
	Host = PublicHost,
	setting(http:public_port, Port).
http_current_host(Request, Host, Port, Options) :-
	(   memberchk(x_forwarded_host(Forwarded), Request)
	->  Port = 80,
	    primary_forwarded_host(Forwarded, Host)
	;   memberchk(host(Host0), Request),
	    (	option(global(true), Options, false)
	    ->	global_host(Host0, Host)
	    ;	Host = Host0
	    ),
	    option(port(Port), Request, 80)
	->  true
	;   gethostname(Host),
	    http_current_server(_Pred, Port)		% TBD: May be more
	).


%%	primary_forwarded_host(+Spec, -Host) is det.
%
%	x_forwarded host contains multiple hosts seperated   by  ', ' if
%	there are multiple proxy servers in   between.  The first one is
%	the one the user's browser knows about.

primary_forwarded_host(Spec, Host) :-
	sub_atom(Spec, B, _, _, ','), !,
	sub_atom(Spec, 0, B, _, Host).
primary_forwarded_host(Host, Host).


%%	global_host(+HostIn, -Host)
%
%	Globalize a hostname. Used if we need  to pass our hostname to a
%	client and expect the client to be   able to contact us. In this
%	case we cannot use a  name  such   as  `localhost'  or the plain
%	hostname of the machine. We assume   (possibly  wrongly) that if
%	the host contains a '.', it is globally accessible.
%
%	If the heuristics used by  this   predicate  do not suffice, the
%	setting http:public_host can be used to override.

global_host(_, Host) :-
	setting(http:public_host, PublicHost), PublicHost \== '', !,
	Host = PublicHost.
global_host(localhost, Host) :- !,
	gethostname(Host).
global_host(Local, Host) :-
	sub_atom(Local, _, _, _, '.'), !,
	Host = Local.
global_host(Local, Host) :-
	tcp_host_to_address(Local, IP),
	tcp_host_to_address(Host, IP).


