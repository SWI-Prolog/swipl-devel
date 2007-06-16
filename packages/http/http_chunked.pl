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

:- module(http_chunked,
	  [ http_chunked_open/3		% +Stream, -DataStream, +Options
	  ]).

:- initialization
   load_foreign_library(foreign(http_chunked)).

/** <module> Chunked data streams

This module realises a filtering stream,  encoding and decoding the HTTP
_Chunked_ encoding. This encoding is an obligatory  part of the HTTP 1.1
specification. Messages are split into  chunks,   each  preceeded by the
length of the chunk. Chunked  encoding   allows  sending messages over a
serial link (typically a TCP/IP stream) for  which the reader knows when
the message is ended. Unlike standard HTTP   though, the sender does not
need to know the message length  in   advance.  The  protocol allows for
sending short chunks. This is  supported   totally  transparent  using a
flush on the output stream.

This module only implements the chunk mechanism   and  no other parts of
the  HTTP  protocol   to   facilitate    lightweight   reuse   in  other
connection-based protocols.

@see	The HTTP 1.1 protocol http://www.w3.org/Protocols/rfc2616/rfc2616.html
@author Jan Wielemaker
*/

%%	http_chunked_open(+RawStream, -DataStream, +Options) is det.
%
%	Create a stream to realise HTTP   chunked  encoding or decoding.
%	The technique is similar to library(zlib), using a Prolog stream
%	as a filter on another stream.  Options:
%	
%		* close_parent(+Bool)
%		If =true= (default =false=), the parent stream is closed
%		if DataStream is closed.
%		
%		* max_chunk_size(+PosInt)
%		Define the maximum size of a chunk.  Default is the
%		default buffer size of fully buffered streams (4096).
%		Larger values may improve throughput.  It is also
%		allowed to use =|set_stream(DataStream, buffer(line))|=
%		on the data stream to get line-buffered output. See
%		set_stream/2 for details. Switching buffering to =false=
%		is supported.
%		
%	Here is example code to write a chunked data to a stream
%	
%	==
%		http_chunked_open(Out, S, []),
%		format(S, 'Hello world~n', []),
%		close(S).
%	==
%	
%	If a stream is known to contain chunked data, we can extract
%	this data using
%	
%	==
%		http_chunked_open(In, S, []),
%		read_stream_to_codes(S, Codes),
%		close(S).
%	==
%	
%	The current implementation does not  generate chunked extensions
%	or an HTTP trailer. If such extensions  appear on the input they
%	are silently ignored. This  is  compatible   with  the  HTTP 1.1
%	specifications. Although a filtering  stream   is  an  excellent
%	mechanism for encoding and decoding   the core chunked protocol,
%	it does not well support out-of-band data.
%	
%	@error	io_error(read, Stream) where the message context provides
%		an indication of the problem.  This error is raised if
%		the input is not valid HTTP chunked data.

