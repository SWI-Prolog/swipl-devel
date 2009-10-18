/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2009, University of Amsterdam

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

@tbd	Loading a single URL gives four requests, (1) through
	rdf_db:exists_url/1, (2) through rdf_db:rdf_input_info_hook/3
	and (3) for the actual loading.
*/

:- multifile
	rdf_db:rdf_open_hook/3,
	rdf_db:rdf_input_info_hook/3,
	rdf_db:url_protocol/1.

%%	rdf_extra_headers(-List)
%
%	Send extra headers with the request. Note that, although we also
%	process RDF embedded in HTML, we do  not explicitely ask for it.
%	Doing so causes some   (e.g., http://w3.org/2004/02/skos/core to
%	reply with the HTML description rather than the RDF).

rdf_extra_headers(
	[ request_header('Accept' = 'text/rdf+xml,\
				     application/rdf+xml; q=0.9, \
				     text/turtle,\
				     application/x-turtle; q=0.8, \
				     */*; q=0.1')
	      ]).


rdf_db:rdf_open_hook(url(http, URL), Stream, Format) :-
	atom(Format), !,
	rdf_extra_headers(Extra),
	http_open(URL, Stream,
		  [ header(content_type, Type)
		  | Extra
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
guess_format('text/turtle',	      _, turtle).
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
	rdf_extra_headers(Extra),
	http_open(URL, Stream,
		  [ header(content_type, Type),
		    header(last_modified, Date),
		    method(head)
		  | Extra
		  ]),
	close(Stream),
	guess_format(Type, URL, Format),
	(   Date == ''
	->  get_time(Modified)
	;   parse_time(Date, Modified)
	).

rdf_db:url_protocol(http).

rdf_db:exists_url(url(http, URL)) :-
	rdf_extra_headers(Extra),
	catch(http_open(URL, Stream,
			[ method(head)
			| Extra
			]), _, fail),
	close(Stream).
