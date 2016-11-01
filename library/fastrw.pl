/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2010-2016, VU University Amsterdam

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
:- use_module(library(lists)).

/** <module> Fast reading and writing of terms

This library provides the SICStus   and  Ciao library(fastrw) interface.
The idea behind this library  is  to   design  a  fast serialization for
Prolog  terms.  Ideally,  this  should    be   portable  between  Prolog
implementation. Unfortunately there is no   portably  binary term format
defined.

The current implementation  is  based   on  PL_record_external(),  which
provides a binary representation of terms  that is processed efficiently
and can handle cycles as well as attributes.   We try to keep the format
compatible between versions, but this is   not guaranteed. Conversion is
always possible by reading a database  using   the  old version, dump it
using write_canonical/1 and read it into the new version.

This library is built upon the following built in predicates:

  - fast_term_serialized/2 translates between a term and its
  serialization as a byte string.
  - fast_read/2 and fast_write/2 read/write binary serializations.

@tbd	Establish a portable binary format.
@compat The format is not compatible to SICStus/Ciao (which are not
	compatible either).  Funture versions of this library might
	implement a different encoding.
@bug	The current implementation of fast_read/1 *is not safe*.
	It is guaranteed to safely read terms written using
	fast_write/1, but may crash on arbitrary input.  The
	implementation does perform some basic sanity checks,
	including validation of the magic start byte.
*/

%%	fast_read(-Term)
%
%	The next term is read from current standard input and is unified
%	with Term. The syntax of the term   must  agree with fast_read /
%	fast_write format. If the end  of   the  input has been reached,
%	Term is unified with the term =end_of_file=.

fast_read(Term) :-
	fast_read(current_input, Term).

%%	fast_write(+Term)
%
%	Output Term in a way that   fast_read/1  and fast_read/2 will be
%	able to read it back.

fast_write(Term) :-
	fast_write(current_output, Term).

%%	fast_write_to_string(+Term, -String, ?Tail)
%
%	Perform a fast-write to the difference-slist String\Tail.

fast_write_to_string(T, S, R) :-
	fast_term_serialized(T, String),
	string_codes(String, Codes),
	append(Codes, R, S).
