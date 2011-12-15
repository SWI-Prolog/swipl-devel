/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2010, VU University Amsterdam

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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(fastrw,
	  [ fast_read/1,		% -Term
	    fast_write/1,		% +Term
	    fast_read/2,		% +Stream, -Term
	    fast_write/2,		% +Stream, +Term
	    fast_write_to_string/3	% +Term, -String, ?Tail
	  ]).

/** <module> Fast reading and writing of terms

This library provides the SICStus   and  Ciao library(fastrw) interface.
The idea behind this library  is  to   design  a  fast serialization for
Prolog  terms.  Ideally,  this  should    be   portable  between  Prolog
implementation. The current implementation provides the API simply using
canonical read/write.

Note that the stream encoding must  be   the  same. Typically, you would
like  to  use  these  predicate  using    UTF-8   encoded  streams.  See
set_stream/2.

@tbd	Establish a fast and portable binary format.
@compat The format is not compatible to SICStus/Ciao (which are not
	compatible either).  Funture versions of this library might
	implement a different encoding.
@see	PL_record_external() for a C-based fast binary format.
*/

%%	fast_read(-Term)
%
%	The next term is read from current standard input and is unified
%	with Term. The syntax of the term   must  agree with fast_read /
%	fast_write format. If the end  of   the  input has been reached,
%	Term is unified with the term   =end_of_file=.  Further calls to
%	fast_read/1 will then cause an error.

fast_read(Term) :-
	read_term(Term, []).

%%	fast_write(+Term)
%
%	Output Term in a way that   fast_read/1  and fast_read/2 will be
%	able to read it back.

fast_write(Term) :-
	fast_write(current_output, Term).

%%	fast_write(+Stream, +Term)
%
%	Output Term to Stream in a  way that fast_read/1 and fast_read/2
%	will be able to read it back.

fast_write(Stream, Term) :-
	write_term(Stream, Term,
		   [ attributes(ignore),
		     ignore_ops(true),
		     quoted(true),
		     partial(true)
		   ]),
	format(Stream, '.~n', []).

%%	fast_read(+Stream, -Term)
%
%	The next term is read from  Stream   and  unified with Term. The
%	syntax of the term must  agree with fast_read/fast_write format.
%	If the end of the input has   been reached, Term is unified with
%	the term =end_of_file=. Further calls   to fast_read/2 will then
%	cause an error.

fast_read(Stream, Term):-
	read_term(Stream, Term, []).

%%	fast_write_to_string(+Term, -String, ?Tail)
%
%	Perform a fast-write to the difference-slist String\Term.

fast_write_to_string(T, S, R) :-
	with_output_to(codes(S,R), fast_write(T)).
