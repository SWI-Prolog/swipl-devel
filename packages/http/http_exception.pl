/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2009, University of Amsterdam

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

:- module(http_exception,
	  [ map_exception_to_http_status/3
	  ]).

/** <module> Internal module of the HTTP server

@see	http_header.pl, http_wrapper.pl
*/

%%	map_exception_to_http_status(+Exception, -Reply, -HdrExtra)
%	
%	Map certain defined  exceptions  to   special  reply  codes. The
%	http(not_modified)   provides   backward     compatibility    to
%	http_reply(not_modified).

map_exception_to_http_status(http(not_modified),
	      not_modified,
	      [connection('Keep-Alive')]) :- !.
map_exception_to_http_status(http_reply(Reply),
	      Reply,
	      [connection(Close)]) :- !,
	(   keep_alive(Reply)
	->  Close = 'Keep-Alive'
	;   Close = close
	).
map_exception_to_http_status(http_reply(Reply, HdrExtra0),
	      Reply,
	      HdrExtra) :- !,
	(   memberchk(close(_), HdrExtra0)
	->  HdrExtra = HdrExtra0
	;   HdrExtra = [close(Close)|HdrExtra0],
	    (   keep_alive(Reply)
	    ->  Close = 'Keep-Alive'
	    ;   Close = close
	    )
	).
map_exception_to_http_status(error(existence_error(http_location, Location), _),
	      not_found(Location),
	      [connection(close)]) :- !.
map_exception_to_http_status(error(permission_error(http_location, access, Location), _),
	      forbidden(Location),
	      [connection(close)]) :- !.
map_exception_to_http_status(error(threads_in_pool(_Pool), _),
	      busy,
	      [connection(close)]) :- !.
map_exception_to_http_status(E,
	      resource_error(E),
	      [connection(close)]) :-
	resource_error(E), !.
map_exception_to_http_status(E,
	      server_error(E),
	      [connection(close)]).

resource_error(error(resource_error(_), _)).

%%	keep_alive(+Reply) is semidet.	
%
%	If true for Reply, the default is to keep the connection open.

keep_alive(not_modified).
keep_alive(file(_Type, _File)).
keep_alive(tmp_file(_Type, _File)).
keep_alive(stream(_In, _Len)).
keep_alive(cgi_stream(_In, _Len)).


		 /*******************************
		 *	    IDE SUPPORT		*
		 *******************************/

% See library('trace/exceptions')

:- multifile
	prolog:general_exception/2.

prolog:general_exception(http_reply(_), http_reply(_)).
prolog:general_exception(http_reply(_,_), http_reply(_,_)).
