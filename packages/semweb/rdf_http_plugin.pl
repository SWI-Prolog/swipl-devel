/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2006, University of Amsterdam

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
	rdf_db:rdf_input_info/3.

rdf_db:rdf_open_hook(url(http, URL), Stream, Format) :-
	http_open(URL, Stream,
		  [ header(content_type, Type)
		  ]),
	guess_format(Type, URL, Format).


%%	guess_format(+ContentType, +URL, -Format)
%
%	Guess the file format. We first try the official mime-types, but
%	as it is  quite  likely  many   web-servers  do  not  have these
%	registered, we use the filename extension as a backup.

guess_format('text/rdf+xml', _, xml).
guess_format('application/x-turtle', _, turtle).
guess_format('application/turtle',   _, turtle).
guess_format(_, URL, Format) :-
	file_name_extension(_, Ext, URL),
	rdf_db:rdf_file_type(Ext, Format).


rdf_db:rdf_input_info(url(http, URL), Modified, Format) :-
	http_open(URL, Stream,
		  [ header(content_type, Type),
		    header(last_modified, Date),
		    method(head)
		  ]),
	close(Stream),
	Date \== '',
	guess_format(Type, URL, Format),
	parse_time(Date, Modified).
