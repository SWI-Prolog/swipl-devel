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

:- module(http_json,
	  [ reply_json/1,		% +JSON
	    reply_json/2,		% +JSON, Options
	    http_read_json/2,		% +Request, -JSON
	    http_read_json/3		% +Request, -JSON, +Options
	  ]).
:- use_module(http_client).
:- use_module(http_header).
:- use_module(http_stream).
:- use_module(json).
:- use_module(library(option)).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(memfile)).

:- multifile
	http_client:http_convert_data/4,
	http_client:post_data_hook/3.


/** <module> HTTP JSON Plugin module

This  module  inserts  the  JSON  parser  for  documents  of  MIME  type
=|application/jsonrequest|= and =|application/json|=   requested through
the http_client.pl library.

Typically JSON is used by Prolog HTTP servers.  Below is a skeleton for
handling a JSON request, answering in JSON. 

==
handle(Request) :-
	http_read_json(Request, JSONIn),
	json_to_prolog(JSONIn, PrologIn), 
	<compute>(PrologIn, PrologOut),		% application body
	prolog_to_json(PrologOut, JSONOut),
	reply_json(JSONOut).
==

This module also integrates JSON support into the http client provided
by http_client.pl. Posting a JSON query and processing the JSON reply
(or any other reply understood by http_read_data/3) is as simple as
below, where Term is a JSON term as described in json.pl and reply is
of the same format if the server replies with JSON.

==
	...,
	http_post(URL, json(Term), Reply, [])
==

@see	JSON Requests are discussed in http://json.org/JSONRequest.html
@see	json.pl describes how JSON objects are represented in Prolog terms.
@see	json_convert.pl converts between more natural Prolog terms and json
terms.
*/

http_client:http_convert_data(In, Fields, Data, Options) :-
	memberchk(content_type(Type), Fields),
	json_type(Type), !,
	(   memberchk(content_length(Bytes), Fields)
	->  stream_range_open(In, Range, [size(Bytes)]),
	    set_stream(Range, encoding(utf8)),
	    call_cleanup(json_read(Range, Data, Options), close(Range))
	;   json_read(In, Data, Options)
	).


%%	json_type(?MIMEType:atom) is semidet.
%
%	True if MIMEType is a JSON mimetype.

json_type('application/jsonrequest').
json_type('application/json').


%	http_client:post_data_hook(+Data, +Out:stream, +HdrExtra) is semidet.
%
%	Hook into http_post_data/3 that allows for
%	
%	==
%	http_post(URL, json(Term), Reply, Options)
%	==
%	
%	@tbd avoid creation of intermediate data using chunked output.

http_client:post_data_hook(json(Term), Out, HdrExtra) :-
	http_client:post_data_hook(json(Term, []), Out, HdrExtra).
http_client:post_data_hook(json(Term, Options), Out, HdrExtra) :-
	option(content_type(Type), HdrExtra, 'application/json'),
	new_memory_file(MemFile),
	open_memory_file(MemFile, write, Handle),
	format(Handle, 'Content-type: ~w~n~n', [Type]),
	json_write(Handle, Term, Options),
	close(Handle),
	open_memory_file(MemFile, read, RdHandle),
	call_cleanup(http_post_data(cgi_stream(RdHandle), Out, HdrExtra),
		     (	 close(RdHandle),
			 free_memory_file(MemFile))).


%%	http_read_json(+Request, -JSON) is det.
%%	http_read_json(+Request, -JSON, +Options) is det.
%
%	Extract JSON data posted to this HTTP request.
%	
%	@error	domain_error(mimetype, Found) if the mimetype is
%		not known (see json_type/1).
%	@error	domain_error(method, Method) if the request is not
%		a POST request.

http_read_json(Request, JSON) :-
	http_read_json(Request, JSON, []).

http_read_json(Request, JSON, Options) :-
	select_option(content_type(Type), Options, Rest), !,
	delete(Request, content_type(_), Request2),
	request_to_json([content_type(Type)|Request2], JSON, Rest).
http_read_json(Request, JSON, Options) :-
	request_to_json(Request, JSON, Options).

request_to_json(Request, JSON, Options) :-
	memberchk(method(Method), Request),
	memberchk(content_type(Type), Request),
	(   Method == post
	->  true
	;   domain_error(method, Method)
	),
	(   json_type(Type)
	->  true
	;   domain_error(mimetype, Type)
	),
	http_read_data(Request, JSON, Options).

	
%%	reply_json(+JSONTerm) is det.
%%	reply_json(+JSONTerm, +Options) is det.
%
%	Formulate a JSON  HTTP  reply.   See  json_write/2  for details.
%	Options accepts content_type(+Type)  and   options  accepted  by
%	json_write/3.

reply_json(Term) :-
	format('Content-type: application/json~n~n'),
	json_write(current_output, Term).

reply_json(Term, Options) :-
	select_option(content_type(Type), Options, Rest, 'application/json'),
	format('Content-type: ~w~n~n', [Type]),
	json_write(current_output, Term, Rest).
