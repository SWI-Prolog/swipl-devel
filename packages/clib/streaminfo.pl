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

:- module(stream_info,
	  [ stream_info/1		% +Stream
	  ]).

:- initialization
   load_foreign_library(foreign(streaminfo)).

%%	stream_info(+Stream) is det.
%
%	Print detailed information about a stream   or  a file-number to
%	the error output. The  output  of   this  command  is  meant for
%	experts and requires  knowledge  about   the  implementation  of
%	streams. It has been  added  to   diagnose  leaking  streams  in
%	web-servers. For example,  on  linux   systems  we  can  examine
%	process file-descriptors using
%
%	==
%	% ls -l /proc/<pid>/fd
%	==
%
%	If now (say) descriptor 15 is open   where  it should not be, we
%	can this command to find the associated Prolog streams and print
%	as mush as possible information about the stream.
%
%	==
%	?- stream_info(15).
%	==
%
%	@param	Stream	A stream-handle, alias name or (integer) system
%		file handle.

stream_info(Stream) :-
	is_stream(Stream), !,
	forall(stream_property(Stream, P),
	       print_property(P)),
	nl,
	catch('$stream_info'(Stream), E, true),
	(   nonvar(E)
	->  format('~w:~t~25|~q~n', ['pending exception', E])
	;   true
	).
stream_info(FileNo) :-
	integer(FileNo), !,
	findall(S, stream_property(S, file_no(FileNo)), Streams),
	length(Streams, Len),
	format('File no ~w is connected to ~d streams~n', [FileNo, Len]),
	forall(member(Stream, Streams),
	       (   format('****************~nStream ~p:~n', [Stream]),
		   stream_info(Stream))).

print_property(P) :-
	P =.. [Name,Value], !,
	format('~w:~t~25|~q~n', [Name, Value]).
print_property(input) :- !.
print_property(output) :- !.
print_property(P) :-
	format('~p~n', [P]).
