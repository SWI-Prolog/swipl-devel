/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2007, University of Amsterdam

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


:- module(rdf_http_plugin, []).
:- use_module(library('http/http_open')).
:- use_module(library('semweb/rdf_db')).
:- use_module(library(date)).

/** <module> RDF HTTP Plugin

This module allows loading data into the semantic web library directly
from an HTTP server.
*/

:- multifile
	rdf_db:rdf_open_hook/3,
	rdf_db:rdf_input_info_hook/3,
	rdf_db:url_protocol/1.

rdf_db:rdf_open_hook(url(http, URL), Stream, Format) :-
	atom(Format), !,
	http_open(URL, Stream,
		  [ header(content_type, Type),
		    request_header('Accept' = 'text/rdf+xml; q=1, \
					       application/rdf+xml; q=1, \
					       application/x-turtle; q=0.9, \
					       text/xml; q=0.5, \
					       text/html; q=0.5, \
					       application/xhtml+xml; q=0.5, \
					       */*; q=0.1')
		  ]),
	(   ground(Format)
	->  true
	;   guess_format(Type, URL, Format)
	).


%%	guess_format(+ContentType, +URL, -Format)
%
%	Guess the file format. We first try the official mime-types, but
%	as it is  quite  likely  many   web-servers  do  not  have these
%	registered, we use the filename extension as a backup.
%	
%	@bug	The turtle parser only parses a subset of n3.

guess_format('text/rdf+xml',	      _, xml).
guess_format('application/rdf+xml',   _, xml).
guess_format('application/x-turtle',  _, turtle).
guess_format('application/turtle',    _, turtle).
guess_format('text/rdf+n3',	      _, turtle). % Bit dubious
guess_format('text/html',	      _, xhtml).
guess_format('application/xhtml+xml', _, xhtml).
guess_format(_Mime, URL, Format) :-
	zip_extension(URL, PlainUrl, Format, PlainFormat),
	file_name_extension(_Base, Ext, PlainUrl),
	rdf_db:rdf_file_type(Ext, PlainFormat).


zip_extension(URL0, URL, gzip(Format), Format) :-
	file_name_extension(URL, gz, URL0), !.
zip_extension(URL, URL, Format, Format).


rdf_db:rdf_input_info_hook(url(http, URL), Modified, Format) :-
	http_open(URL, Stream,
		  [ header(content_type, Type),
		    header(last_modified, Date),
		    method(head)
		  ]),
	close(Stream),
	Date \== '',
	guess_format(Type, URL, Format),
	parse_time(Date, Modified).

rdf_db:url_protocol(http).

rdf_db:exists_url(url(http, URL)) :-
	catch(http_open(URL, Stream,
			[ method(head)
			]), _, fail),
	close(Stream).
