/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

:- module(http_client, []).
:- use_module(library(pce)).
:- use_module(library(url)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module defines the clas http_client, a subclass of class socket for
fetching data from HTTP servers (aka `web-servers').

It can deal with two HTTP operations

	# GET
	Get data from an URL

	# HEAD
	Get only the header from an URL.

The GET operation is implemented by ->fetch_data:

http_client ->fetch_data: Into:object, Confirm:[code]*, Location:[string]
	Fetches data from the server.  Data is stored into `Into' in
	chunks using `->append: string' to this object.

	If confirm is not @nil, ->fetch_data returns after connecting
	and sending the request to the server.  On completion, the
	Confirm message is executed with

		@arg1	The http_client object
		@arg2	The `Into' object

	If `Confirm' is @nil, ->fetch_data blocks until all data has
	been received.

	In addition to returning the data, the header of the HTTP reply
	is attached to the data in a sheet using the attribute `http_header'.
	Field-names are in lowercase.  The date and last-modified fields
	appear as XPCE date objects and the content-length as an XPCE
	integer.

http_client <-data --> String
	Simple interface to get data.  Returns a string with http_header
	attribute sheet (see above).  Blocks until all data has been received.

http_client <-header --> Sheet
	Yield a sheet containing the header only the header data for the
	URL.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(http_client, socket,
		   "Client socket for HTTP Protocol").

:- pce_global(@nl_regex, new(regex('\n'))).
:- pce_global(@http_reply_regex,
	      new(regex('HTTP/\\([0-9]\\.[0-9]\\)\\s *\\(\\sd+\\)\\s *\\(\\w*\\)'))).
:- pce_global(@http_field_regex,
	      new(regex('\\([a-zA-Z0-9-]+\\):\\s *\\(.*\\)$'))).
:- pce_global(@http_empty_line_regex,
	      new(regex('\\s *\r?$'))).

:- initialization
   new(_, error(http_bad_header,
		'%O: Bad header line: %s',
		warning)),
   new(_, error(url_bad_protocol,
		'%O: Can only handle %s URLs',
		error)),
   new(_, error(url_bad_syntax,
		'%O: Not a legal URL: %s')).

variable(host,	       name,	       get,  "Host-name of URL").
variable(location,     string,	       get,  "Location on server").
variable(message,      code*,	       both, "Message send on completion").
variable(data_object,  object*,	       get,  "Object holding data").
variable(received,     int*,	       get,  "Data received").
variable(remaining,    int*,	       get,  "Remaining data").
variable(request,      {header,data}*, get,  "Last request").
variable(req_status,   name*,          get,  "Progress").
variable(http_version, name,	       both, "Used version (1.0/1.1)").
variable(user_agent,   name,	       both, "Provided User-Agent").
variable(verbose,      {silent,connect,transfer} := connect,
				       both, "Verbosity").

initialise(S, URL:prolog) :->
	"Create from URL or parsed URL"::
	(   atomic(URL)
	->  (   parse_url(URL, Parts)
	    ->  true
	    ;   send(S, error, url_bad_syntax, URL),
		fail
	    )
	;   Parts = URL
	),
	(   memberchk(protocol(http), Parts)
	->  true
	;   send(S, error, url_bad_protocol, 'HTTP'),
	    fail
	),
	option(host(Host), Parts),
	option(port(Port), Parts, 80),
	http_location(Parts, Location),
	send(S, slot, host, Host),
	send(S, slot, location, Location),
	send(S, slot, http_version, '1.0'),
	send(S, slot, user_agent, 'XPCE/SWI-Prolog'),
	send_super(S, initialise, tuple(Host, Port)).

:- pce_group(feedback).

req_status(S, Status:{connecting,connected,header,data,complete}) :->
	"Indicate status changes"::
	send(S, slot, req_status, Status),
	(   get(S, verbose, silent)
	->  true
	;   message(Status, S, Message),
	    ignore(send(S, Message))
	).

message(connecting, S, report(progress, 'Connecting %s', Host)) :-
	get(S, host, Host).
message(connected, _, report(progress, 'Connected')).
message(header,	   _, report(progress, 'Receiving header')).
message(data,	   _, report(progress, 'Receiving data')).
message(complete,  _, report(done)).

progress(S, Progress:int, What:{percent,bytes}) :->
	"Indicate progress"::
	(   get(S, verbose, transfer),
	    send(S, report, progress, 'Received %d %s', Progress, What)
	->  true
	;   true
	).

:- pce_group(connect).

connect(S) :->
	"Connect to <-host"::
	(   get(S, status, connected)
	->  true
	;   send(S, req_status, connecting),
	    send_super(S, connect),
	    send(S, req_status, connected)
	).

:- pce_group(fetch).

fetch_data(S,
	   Object:into=object,
	   Confirm:confirm=[code]*,
	   Location:location=[string]) :->
	"Fetch data from location into Object"::
	send(S, slot, data_object, Object),
	(   Confirm == @default
	->  true
	;   send(S, slot, message, Confirm)
	),
	(   Location == @default
	->  true
	;   send(S, slot, location, Location)
	),
	send(S, slot, request, data),
	send(S, send_header, 'GET'),
	(   get(S, message, Msg),
	    Msg \== @nil
	->  true
	;   send(S, wait)
	).

header(S, Location:[string], Header:sheet) :<-
	"Fetch header data from location as a sheet"::
	new(Object, object),
	send(S, slot, data_object, Object),
	(   Location == @default
	->  true
	;   send(S, slot, location, Location)
	),
	send(S, slot, request, header),
	send(S, send_header, 'HEAD'),
	send(S, wait),
	get(Object, http_header, Header),
	send(Header, lock_object, @on),
	send(Object, delete_attribute, http_header),
	get(Header, unlock, _).

data(S, Data:string) :<-
	"Fetch data from location as string"::
	new(TB, text_buffer),
	send(TB, undo_buffer_size, 0),
	send(S, fetch_data, TB),
	get(TB, contents, Data),
	get(TB, http_header, Header),
	send(Data, attribute, http_header, Header),
	free(TB).

:- pce_group(internal).

send_header(S, Action:name) :->
	"Send request header to HTTP server"::
	get(S, host, Host),
	send(S, slot, received, 0),
	send(S, prepare_header),
	send(S, slot, req_status, @nil),
	send(S, connect),
	get(S, http_version, Version),
	get(S, user_agent, Agent),
	get(S, location, Location),
	send(S, format, '%s %s HTTP/%s\r\n', Action, Location, Version),
	send(S, format, 'Host: %s\r\n', Host),
	send(S, format, 'User-Agent: %s\r\n', Agent),
	send(S, format, '\r\n').

wait(S) :->
	"Wait for completion"::
	repeat,
	(   get(S, req_status, complete)
	->  !
	;   send(@display, dispatch),
	    fail
	).

prepare_header(S) :->
	"Prepare for receiving header info"::
	send(S, record_separator, @nl_regex),
	send(S, input_message, message(S, header_line, @arg1)),
	send(S?data_object, attribute, http_header, new(sheet)).

header_line(S, Line:string) :->
	"Handle the header lines as they come in"::
	(   send(@http_field_regex, match, Line)
	->  get(@http_field_regex, register_value, Line, 1, name, FieldName0),
	    get(FieldName0, downcase, FieldName),
	    get(@http_field_regex, register_value, Line, 2, string, Value),
	    send(Value, strip),
	    field_type(FieldName, Type),
	    get(@pce, convert, Value, Type, FieldValue),
	    get(S, data_object, DataObject),
	    send(DataObject?http_header, value, FieldName, FieldValue)
	;   send(@http_reply_regex, match, Line)
	->  get(S, data_object, DataObject),
	    get(@http_reply_regex, register_value, Line, 2, int, ReplyStatus),
	    send(DataObject?http_header, value, status, ReplyStatus),
	    send(S, req_status, header)
	;   send(@http_empty_line_regex, match, Line)
	->  send(S, prepare_data)
	;   send(S, error, http_bad_header, Line)
	).

field_type('content-length', int).
field_type('date',	     date).
field_type('last-modified',  date).
field_type(_,		     name).

prepare_data(S) :->
	"Header has been received, prepare for content"::
	(   get(S, request, header)
	->  send(S, req_status, complete)
	;   (   get(S, data_object, Object),
	        get(Object?http_header, value, 'content-length', Length)
	    ->  send(S, slot, remaining, Length)
	    ;   send(S, slot, remaining, @nil)
	    ),
	    send(S, input_message, message(S, data_record, @arg1)),
	    send(S, record_separator, @nil)
	).

data_record(S, Record:string) :->
	"Receive a data record"::
	get(S, data_object, Data),
	(   get(S, remaining, Rem),
	    Rem \== @nil
	->  get(Record, size, Read),
	    NewRem is Rem - Read,
	    get(Data?http_header, value, 'content-length', Len),
	    Percent is ((Len-NewRem)*100)//Len,
	    send(S, progress, Percent, percent),
	    (	NewRem >= 0
	    ->	send(Data, append, Record),
		send(S, slot, remaining, NewRem),
		(   NewRem == 0
		->  send(S, complete)
		;   true
		)
	    ;	send(Record, truncate, Rem),
		send(Data, append, Record),
		send(S, complete)
	    )
	;   send(Data, append, Record),
	    get(Record, size, Bytes),
	    get(S, received, Rec0),
	    Rec is Rec0 + Bytes,
	    send(S, slot, received, Rec),
	    send(S, progress, Rec, bytes)
	).
	    
end_of_file(S) :->
	"Implicit completion"::
	send(S, close),
	(   get(S, req_status, complete)
	->  true
	;   send(S, complete)
	).

complete(S) :->
	"We received all data"::
	send(S, req_status, complete),
	(   get(S, message, Message),
	    Message \== @nil
	->  get(S, data_object, Data),
	    send(Message, forward, S, Data)
	;   send(S, req_status, complete)
	).

:- pce_end_class.


%	option(Option(?Value), OptionList, Default)

option(Opt, Options) :-
	memberchk(Opt, Options).

option(Opt, Options, _) :-
	option(Opt, Options), !.
option(Opt, _, Default) :-
	arg(1, Opt, Default).


