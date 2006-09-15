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

:- module(pldoc_log,
	  [ doc_log_requests/1		% +File
	  ]).
:- use_module(library(socket)).

:- dynamic
	logging/1.			% +Stream

%%	doc_log_requests(+File) is det.
%
%	Start logging to File.

doc_log_requests(File) :-
	open(File, append, Out,
	     [ encoding(utf8)
	     ]),
	assert(logging(Out)).

:- multifile
	pldoc_http:log_hook/3.


%%	log_hook(+Port, +Nr, +Data) is semidet.
%
%	Multifile hook called from doc_server/2.
%	
%	@param Port	One of =started=, =enter= or =exit=.
%	@param Nr	Sequence number of request
%	@param Data	For =enter=, the HTTP request data.  For =exit=,
%			the 2nd argument of call_cleanup and =started=
%			a term port(Port).

pldoc_http:log_hook(Port, Nr, Data) :-
	logging(Out), !,
	log(Port, Nr, Data, Out).

log(Port, Nr, Data, Out) :-
	filter_data(Port, Data, Log),
	get_time(Now),
	format_time(string(Time), '%+', Now),
	format(Out, '~w ~q ~w ~q~n', [Time, Port, Nr, Log]),
	flush_output(Out).

filter_data(enter, Request0, Request) :- !,
	filter_request(Request0, Request).
filter_data(exit, exception(http_reply(Reply)), Reply).
filter_data(_, Data, Data).

%%	filter_request(+RequestIn, -RequestOut) is det.
%
%	Filter  the  HTTP  request  header.   Maps  field  values  using
%	log_value/2 and selects fields using log_field/1.

filter_request([], []).
filter_request([H0|T0], [H1|T]) :-
	log_value(H0, H1), !,
	filter_request(T0, T).
filter_request([H|T0], [H|T]) :-
	functor(H, Name, _),
	log_field(Name), !,
	filter_request(T0, T).
filter_request([_|T0], T) :-
	filter_request(T0, T).


%%	log_value(+FieldIn, -FieldOut) is semidet.
%
%	Allow changing fields into somewhat more readable format.
%	
%	@bug	tcp_host_to_address can take long.  It might be better
%		to have this done in a seperate thread.

log_value(peer(ip(A,B,C,D)), peer(Name)) :- !,
	(   catch(tcp_host_to_address(Name, ip(A,B,C,D)), _, fail)
	->  true
	;   concat_atom([A,B,C,D], '.', Name)
	).


%%	log_field(+Field) is semidet.
%
%	True if Field is a field from the request header that must be in
%	the logfile.

log_field(peer).
log_field(method).
log_field(path).
log_field(search).
log_field(referer).
log_field(user_agent).


