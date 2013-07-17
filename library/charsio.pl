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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(charsio,
	  [ format_to_chars/3,		% +Format, +Args, -Codes
	    format_to_chars/4,		% +Format, +Args, -Codes, ?Tail
	    write_to_chars/2,		% +Term, -Codes
	    write_to_chars/3,		% +Term, -Codes, ?Tail
	    atom_to_chars/2,		% +Atom, -Codes
	    atom_to_chars/3,		% +Atom, -Codes, ?Tail
	    number_to_chars/2,		% +Number, -Codes
	    number_to_chars/3,		% +Number, -Codes, ?Tail
					% read predicates
	    read_from_chars/2,		% +Codes, -Term
	    read_term_from_chars/3,	% +Codes, -Term, +Options
	    open_chars_stream/2,	% +Codes, -Stream
	    with_output_to_chars/2,	% :Goal, -Codes
	    with_output_to_chars/3,	% :Goal, -Codes, ?Tail
	    with_output_to_chars/4	% :Goal, -Stream, -Codes, ?Tail
	  ]).
:- use_module(library(error)).
/* :- use_module(library(memfile)). */	% see open_chars_stream/2

:- meta_predicate
	with_output_to_chars(0, -),
	with_output_to_chars(0, -, ?),
	with_output_to_chars(0, -, -, ?).

:- predicate_options(read_term_from_chars/3, 3,
		     [pass_to(system:read_term/3, 3)]).

/** <module> I/O on Lists of Character Codes

This module emulates the Quintus/SICStus  library charsio.pl for reading
and writing from/to lists of character   codes. Most of these predicates
are straight calls into similar SWI-Prolog primitives.  Some can even be
replaced by ISO standard predicates.

@compat The naming of this library is not in line with the ISO standard.
We believe that the SWI-Prolog  native   predicates  form a more elegant
alternative for this library.
*/

%%	format_to_chars(+Format, +Args, -Codes) is det.
%
%	Use format/2 to write to a list of character codes.

format_to_chars(Format, Args, Codes) :-
	format(codes(Codes), Format, Args).

%%	format_to_chars(+Format, +Args, -Codes,	?Tail) is det.
%
%	Use format/2 to write to a difference list of character codes.

format_to_chars(Format, Args, Codes, Tail) :-
	format(codes(Codes, Tail), Format, Args).

%%	write_to_chars(+Term, -Codes)
%
%	Write a term to a code  list.  True   when  Codes  is  a list of
%	character codes written by write/1 on Term.

write_to_chars(Term, Codes) :-
	format(codes(Codes), '~w', [Term]).

%%	write_to_chars(+Term, -Codes, ?Tail)
%
%	Write a term to a code list.  Codes\Tail is a difference list of
%	character codes produced by write/1 on Term.

write_to_chars(Term, Codes, Tail) :-
	format(codes(Codes, Tail), '~w', [Term]).

%%	atom_to_chars(+Atom, -Codes) is det.
%
%	Convert Atom into a list of character codes.
%
%	@deprecated	Use ISO atom_codes/2.

atom_to_chars(Atom, Codes) :-
	atom_codes(Atom, Codes).

%%	atom_to_chars(+Atom, -Codes, ?Tail) is det.
%
%	Convert Atom into a difference list of character codes.

atom_to_chars(Atom, Codes, Tail) :-
	format(codes(Codes, Tail), '~a', [Atom]).

%%	number_to_chars(+Number, -Codes) is det.
%
%	Convert Atom into a list of character codes.
%
%	@deprecated	Use ISO number_codes/2.

number_to_chars(Number, Codes) :-
	number_codes(Number, Codes).

%%	number_to_chars(+Number, -Codes, ?Tail) is det.
%
%	Convert Number into a difference list of character codes.

number_to_chars(Number, Codes, Tail) :-
	must_be(number, Number),
	format(codes(Codes, Tail), '~w', [Number]).

%%	read_from_chars(+Codes, -Term) is det.
%
%	Read Codes into Term.
%
%	@compat	The SWI-Prolog version does not require Codes to end
%		in a full-stop.

read_from_chars("", end_of_file) :- !.
read_from_chars(List, Term) :-
	atom_to_term(List, Term, _).

%%	read_term_from_chars(+Codes, -Term, +Options) is det.
%
%	Read Codes into Term.  Options are processed by read_term/3.
%
%	@compat sicstus

read_term_from_chars(Codes, Term, Options) :-
	setup_call_cleanup(
	    ( open_chars_stream(Codes, Stream, '\n.\n'),
	      '$push_input_context'(read_from_chars)
	    ),
	    read_term(Stream, Term0, Options),
	    ( '$pop_input_context',
	      close(Stream)
	    )),
	Term = Term0.

%%	open_chars_stream(+Codes, -Stream) is det.
%
%	Open Codes as an input stream.
%
%	@bug	Depends on autoloading library(memfile).  As many
%		applications do not need this predicate we do not
%		want to make the entire library dependent on
%		autoloading.

open_chars_stream(Codes, Stream) :-
	open_chars_stream(Codes, Stream, '').

open_chars_stream(Codes, Stream, Postfix) :-
	new_memory_file(MF),
	setup_call_cleanup(
	    open_memory_file(MF, write, Out),
	    format(Out, '~s~w', [Codes, Postfix]),
	    close(Out)),
	open_memory_file(MF, read, Stream,
			 [ free_on_close(true)
			 ]).

%%	with_output_to_chars(:Goal, -Codes) is det.
%
%	Run Goal as with once/1.  Output written to =current_output=
%	is collected in Codes.

with_output_to_chars(Goal, Codes) :-
	with_output_to(codes(Codes), Goal).

%%	with_output_to_chars(:Goal, -Codes, ?Tail) is det.
%
%	Run Goal as with once/1.  Output written to =current_output=
%	is collected in Codes\Tail.

with_output_to_chars(Goal, Codes, Tail) :-
	with_output_to(codes(Codes, Tail), Goal).

%%	with_output_to_chars(:Goal, -Stream, -Codes, ?Tail) is det.
%
%	Same as with_output_to_chars/3 using  an   explicit  stream. The
%	difference list Codes\Tail contains  the   character  codes that
%	Goal has written to Stream.

with_output_to_chars(Goal, Stream, Codes, Tail) :-
	with_output_to(codes(Codes, Tail), with_stream(Stream, Goal)).

with_stream(Stream, Goal) :-
	current_output(Stream),
	call(Goal).
