/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2002, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(mime_pack,
	  [ mime_pack/3			% +Input, +Stream, ?Boundary
	  ]).
:- use_module(mimetype).
:- use_module(html_write).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Simple and partial implementation of MIME   encoding. MIME is covered by
RFC 2045 which I've read from

	http://www.cis.ohio-state.edu/cgi-bin/rfc/rfc2045.html

MIME deconding is now  arranged  through   library(mime)  from  the clib
package, based on the  external  librfc2045   library.  Most  likely the
functionality of this package will be moved to the same library someday.
Packing however is a lot simpler then parsing.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%	mime_pack(+Inputs, +OutputStream, Boundary)
%
%	Pack a number of inputs into a MIME package using a specified
%	or generated boundary.

mime_pack(Inputs, OutputStream, Boundary) :-
	make_boundary(Inputs, Boundary),
	pack_list(Inputs, OutputStream, Boundary).

pack_list([], Out, Boundary) :-
	format(Out, '--~w--\r\n', [Boundary]).
pack_list([H|T], Out, Boundary) :-
	format(Out, '--~w\r\n', [Boundary]),
	pack(H, Out),
	format(Out, '\r\n', []),
	pack_list(T, Out, Boundary).

pack(X) :- 
	\+ ground(X), !,
	throw(error(instantiation_error, _)).
pack(Name=Value, Out) :- !,
	format(Out, 'Content-Disposition: form-data; name="~w"\n', [Name]),
	pack(Value).
pack(html(HTML), Out) :-
	format(Out, 'Content-Type: text/html\r\n\r\n', []),
	print_html(Out, HTML).
pack(file(File), Out) :-
	(   file_mime_type(File, Type)
	->  true
	;   Type = text/plain
	),
	format(Out, 'Content-Type: ~w\r\n\r\n', [Type]),
	(   Type = text/_
	->  OpenOptions = []
	;   OpenOptions = [type(binary)]
	),
	open(File, read, In, OpenOptions),
	copy_stream_data(In, Out),
	close(In).
pack(stream(In, Len), Out) :- !,
	copy_stream_data(In, Out, Len).
pack(stream(In), Out) :- !,
	copy_stream_data(In, Out).
pack(mime(Atts, Data, []), Out) :- !,		% mime_parse compatibility
	write_mime_attributes(Atts, Out),
	write(Out, Data).
pack(mime(_Atts, '', Parts), Out) :-
	make_boundary(Parts, Boundary),
	format('Content-type: multipart/mixed\r\n\r\n'),
	mime_pack(Parts, Out, Boundary).

write_mime_attributes([], Out) :- !,
	format(Out, '\r\n', []).
write_mime_attributes(Atts, Out) :-
	select(type(Type), Atts, A1), !,
	(   select(character_set(CharSet), A1, A2)
	->  format(Out, 'Content-type: ~w; charset=~w\r\n', [Type, CharSet]),
	    write_mime_attributes(A2, Out)
	;   format(Out, 'Content-type: ~w\r\n', [Type]),
	    write_mime_attributes(A1, Out)
	).
write_mime_attributes([_|T], Out) :-
	write_mime_attributes(T, Out).

%	make_boundary(+Inputs, ?Boundary)
%
%	Generate a boundary.  This should check all input sources whether
%	the boundary is enclosed.

make_boundary(_, Boundary) :-
	atomic(Boundary), !.
make_boundary(_, Boundary) :-
	get_time(Now),
	A is random(1<<16),
	B is random(1<<16),
	C is random(1<<16),
	D is random(1<<16),
	E is random(1<<16),
	sformat(Boundary, '~0f~16r~16r~16r~16r~16r',
		[Now, A, B, C, D, E]).

