/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2010, University of Amsterdam
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


:- module(rdf_http_plugin, []).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_header)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(date)).
:- use_module(library(error)).


/** <module> RDF HTTP Plugin

This module allows loading data into   the semantic web library directly
from an HTTP server. The following example  loads the RDF core data into
the RDF database.

    ==
    :- use_module(library(semweb/rdf_db)).
    :- use_module(library(semweb/rdf_http_plugin)).

	...,
	rdf_load('http://www.w3.org/1999/02/22-rdf-syntax-ns')
    ==
*/

:- multifile
	rdf_content_type/2.		% Allow defining additional
					% content-type mappings.

:- multifile
	rdf_db:rdf_open_hook/8,
	rdf_db:url_protocol/1.

rdf_db:url_protocol(http).


%%	rdf_extra_headers(-List)
%
%	Send extra headers with the request. Note that, although we also
%	process RDF embedded in HTML, we do  not explicitely ask for it.
%	Doing so causes some   (e.g., http://w3.org/2004/02/skos/core to
%	reply with the HTML description rather than the RDF).

rdf_extra_headers(
	[ request_header('Accept' = 'application/rdf+xml, \
				     text/rdf+xml; q=0.9, \
				     text/turtle, \
				     application/x-turtle; q=0.8, \
				     */*; q=0.1')
	]).


rdf_db:rdf_open_hook(http, SourceURL, HaveModified, Stream, Cleanup,
		     Modified, Format, Options) :-
	modified_since_header(HaveModified, Header),
	TypeHdr = [ header(content_type, ContentType),
		    header(last_modified, ModifiedText)
		  ],
	rdf_extra_headers(Extra),
	append([Extra, TypeHdr, Header, Options], OpenOptions),
	catch(http_open(SourceURL, Stream0, OpenOptions), E, true),
	(   var(E)
	->  (   open_envelope(ContentType, SourceURL,
			      Stream0, Stream, Format)
	    ->	Cleanup = close(Stream),
		(   nonvar(ModifiedText),
		    parse_time(ModifiedText, ModifiedStamp)
		->  Modified = last_modified(ModifiedStamp)
		;   Modified = unknown
		)
	    ;	close(Stream0),
		domain_error(content_type, ContentType)
	    )
	;   subsumes_term(error(_, context(_, status(304, _))), E)
	->  Modified = not_modified,
	    Cleanup = true
	;   throw(E)
	).


%%	modified_since_header(+LastModified, -ExtraHeaders) is det.
%
%	Add an =|If-modified-since|= if we have a version with the given
%	time-stamp.

modified_since_header(HaveModified, []) :-
	var(HaveModified), !.
modified_since_header(HaveModified,
		      [ request_header('If-modified-since' =
				       Modified)
		      ]) :-
	http_timestamp(HaveModified, Modified).

%%	open_envelope(+ContentType, +SourceURL, +Stream0, -Stream,
%%		      ?Format) is semidet.
%
%	Open possible envelope formats.

open_envelope('application/x-gzip', SourceURL, Stream0, Stream, Format) :-
	rdf_db:rdf_storage_encoding(_, gzip), !,
	(   var(Format)
	->  file_name_extension(BaseURL, _GzExt, SourceURL),
	    file_name_extension(_, Ext, BaseURL),
	    rdf_db:rdf_file_type(Ext, Format)
	;   true
	),
	rdf_zlib_plugin:zopen(Stream0, Stream, []).
open_envelope(_, _, Stream, Stream, Format) :-
	nonvar(Format), !.
open_envelope(ContentType, SourceURL, Stream, Stream, Format) :-
	major_content_type(ContentType, Major),
	(   rdf_content_type(Major, Format)
	->  true
	;   Major == 'text/plain'	% server is not properly configured
	->  file_name_extension(_, Ext, SourceURL),
	    rdf_db:rdf_file_type(Ext, Format)
	).

major_content_type(ContentType, Major) :-
	sub_atom(ContentType, Pre, _, _, (;)), !,
	sub_atom(ContentType, 0, Pre, _, Major).
major_content_type(Major, Major).

%%	rdf_content_type(+ContentType, +URL)
%
%	Deduce the RDF encoding from the mime-type.
%
%	@bug	The turtle parser only parses a subset of n3.

rdf_content_type('text/rdf',		  xml).
rdf_content_type('text/rdf+xml',	  xml).
rdf_content_type('application/rdf+xml',	  xml).
rdf_content_type('application/x-turtle',  turtle).
rdf_content_type('application/turtle',	  turtle).
rdf_content_type('text/turtle',		  turtle).
rdf_content_type('text/rdf+n3',		  turtle).	% Bit dubious
rdf_content_type('text/html',		  xhtml).
rdf_content_type('application/xhtml+xml', xhtml).
rdf_content_type('application/x-gzip',	  gzip).
