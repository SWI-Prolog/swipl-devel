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


pldoc_http:log_hook(Port, Nr, Data) :-
	logging(Out), !,
	log(Port, Nr, Data, Out).

log(Port, Nr, Data, Out) :-
	filter_data(Port, Data, Log),
	get_time(Now),
	format_time(string(Time), '%+', Now),
	format(Out, '~w ~w ~w ~q~n', [Time, Port, Nr, Log]),
	flush_output(Out).

filter_data(enter, Request0, Request) :- !,
	filter_request(Request0, Request).
filter_data(_, Data, Data).

filter_request([], []).
filter_request([H|T0], [H|T]) :-
	functor(H, Name, _),
	log_field(Name), !,
	filter_request(T0, T).
filter_request([_|T0], T) :-
	filter_request(T0, T).

%%	log_field(+Field) is semidet.
%
%	True if Field is a field from the request header that must be in
%	the logfile.

log_field(peer).
log_field(method).
log_field(path).
log_field(search).


