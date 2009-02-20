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

:- module(http_log,
	  [ http_log_stream/1,		% -Stream
	    http_log/2,			% +Format, +Args
	    http_log_close/1		% +Reason
	  ]).
:- use_module(library(settings)).
:- use_module(library(broadcast)).

:- setting(http:logfile, atom, 'httpd.log',
	   'File in which to log HTTP requests').

/** <module> HTTP Logging module

Simple module for logging HTTP requests to a file. Logging is enabled by
loading this file and ensure the setting   http:logfile is not the empty
atom. The default  file  for  writing   the  log  is  =|httpd.log|=. See
library(settings) for details.

The  level  of  logging  can  modified  using  the  multifile  predicate
http_log:nolog/1 to hide HTTP  request  fields   from  the  logfile  and
http_log:password_field/1   to   hide   passwords   from   HTTP   search
specifications (e.g. =|/topsecret?password=secret|=.
*/

:- multifile
	nolog/1,
	password_field/1.

% If the log settings change,  simply  close   the  log  and  it will be
% reopened with the new settings.

:- listen(settings(changed(http:logfile, _, New)),
	  http_log_close(changed(New))).
:- listen(http(Message),
	  http_message(Message)).


http_message(request_start(Id, Request)) :- !,
	http_log_stream(Stream),
	log_started(Request, Id, Stream).
http_message(request_finished(Id, Code, Status, CPU, Bytes)) :- !,
	http_log_stream(Stream),
	log_completed(Code, Status, Bytes, Id, CPU, Stream).


		 /*******************************
		 *	   LOG ACTIVITY		*
		 *******************************/

:- dynamic
	log_stream/1.

%%	http_log_stream(-Stream) is semidet.
%	
%	Returns handle to open logfile. Fails if no logfile is open and
%	none is defined.

http_log_stream(Stream) :-
	log_stream(Stream), !,
	Stream \== [].
http_log_stream(Stream) :-
	setting(http:logfile, File),
	File \== '', !,
	with_mutex(http_log,
		   (   open(File, append, Stream,
			    [ close_on_abort(false),
			      encoding(utf8),
			      buffer(line)
			    ]),
		       get_time(Time),
		       format(Stream,
			      'server(started, ~0f).~n',
			      [ Time ]),
		       assert(log_stream(Stream)),
		       at_halt(close_log(stopped))
		   )).
http_log_stream(_) :-
	assert(log_stream([])).

%%	http_log_close(+Reason) is det.
%
%	If there is a currently open HTTP logfile, close it after adding
%	a term server(Reason, Time).  to  the   logfile.  This  call  is
%	intended for cooperation with the Unix logrotate facility
%	using the following schema:
%	
%	    * Move logfile (the HTTP server keeps writing to the moved
%	    file)
%	    * Inform the server using an HTTP request that calls
%	    http_log_close/1
%	    * Compress the moved logfile
%	    
%	@author Suggested by Jacco van Ossenbruggen    

http_log_close(Reason) :-
	with_mutex(http_log, close_log(Reason)).

close_log(Reason) :-
	retract(log_stream(Stream)), !,
	(   Stream == []
	->  true
	;   get_time(Time),
	    format(Stream, 'server(~q, ~0f).~n', [ Reason, Time ]),
	    close(Stream)
	).
close_log(_).

%%	http_log(+Format, +Args) is det.
%
%	Write message from Format and Args   to log-stream. See format/2
%	for details. Succeed without side  effects   if  logging  is not
%	enabled.

http_log(Format, Args) :-
	(   http_log_stream(Stream)
	->  format(Stream, Format, Args)
	;   true
	).


%%	log_started(+Request, +Id, +Stream) is det.
%
%	Write log message that Request was started to Stream.
%	
%	@param	Filled with sequence identifier for the request

log_started(Request, Id, Stream) :-
	get_time(Now),
	log_request(Request, LogRequest),
	format_time(string(HDate), '%+', Now),
	format(Stream,
	       '/*~s*/ request(~q, ~0f, ~q).~n',
	       [HDate, Id, Now, LogRequest]).

%%	log_request(+Request, -Log)
%	
%	Remove passwords from the request to avoid sending them to the
%	logfiles.

log_request([], []).
log_request([search(Search0)|T0], [search(Search)|T]) :- !,
	mask_passwords(Search0, Search),
	log_request(T0, T).
log_request([H|T0], T) :-
	nolog(H), !,
	log_request(T0, T).
log_request([H|T0], [H|T]) :-
	log_request(T0, T).

mask_passwords([], []).
mask_passwords([Name=_|T0], [Name=xxx|T]) :-
	password_field(Name), !,
	mask_passwords(T0, T).
mask_passwords([H|T0], [H|T]) :-
	mask_passwords(T0, T).

%%	password_field(+Field) is semidet.
%
%	Multifile predicate that can be defined to hide passwords from
%	the logfile.

password_field(password).
password_field(pwd0).
password_field(pwd1).
password_field(pwd2).


%%	nolog(+HTTPField)
%
%	Multifile  predicate  that  can  be   defined  to  hide  request
%	parameters from the request logfile.

nolog(input(_)).
nolog(accept(_)).
nolog(accept_language(_)).
nolog(accept_encoding(_)).
nolog(accept_charset(_)).
nolog(pool(_,_,_,_)).
nolog(referer(R)) :-
	sub_atom(R, _, _, _, password), !.

%%	log_completed(+Code, +Status, +Bytes, +Id, +CPU, +Stream) is det.
%
%	Write log message to Stream from a call_cleanup/3 call.
%	
%	@param Status	2nd argument of call_cleanup/3
%	@param Id	Term identifying the completed request
%	@param CPU0	CPU time at time of entrance
%	@param Stream	Stream to write to (normally from http_log_stream/1).

log_completed(Code, Status, Bytes, Id, CPU, Stream) :-
	is_stream(Stream),
	log_check_deleted(Stream), !,
	log(Code, Status, Bytes, Id, CPU, Stream).
log_completed(Code, Status, Bytes, Id, CPU0, _) :-
	http_log_stream(Stream), !,	% Logfile has changed!
	log_completed(Code, Status, Bytes, Id, CPU0, Stream).
log_completed(_,_,_,_,_,_).


%%	log_check_deleted(+Stream) is semidet.
%
%	If the link-count of the stream has   dropped  to zero, the file
%	has been deleted/moved. In this case the  log file is closed and
%	log_check_deleted/6 will open a  new   one.  This  provides some
%	support for cleaning up the logfile   without  shutting down the
%	server.
%	
%	@see logrotate(1) to manage logfiles on Unix systems.

log_check_deleted(Stream) :-
	stream_property(Stream, nlink(Links)),
	Links == 0, !,
	http_log_close(log_file_deleted),
	fail.
log_check_deleted(_).


log(Code, ok, Bytes, Id, CPU, Stream) :- !,
	format(Stream, 'completed(~q, ~2f, ~q, ~q, ok).~n',
	       [ Id, CPU, Bytes, Code ]).
log(Code, Status, Bytes, Id, CPU, Stream) :-
	(   map_exception(Status, Term)
	->  true
	;   message_to_string(Status, String),
	    Term = error(String)
	),
	format(Stream, 'completed(~q, ~2f, ~q, ~q, ~q).~n',
	       [ Id, CPU, Bytes, Code, Term ]).

map_exception(http_reply(Reply), Reply).
map_exception(error(existence_error(http_location, Location), _Stack),
	      error(404, Location)).
