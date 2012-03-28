/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2009-2011, University of Amsterdam
			      VU University Amsterdam

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

:- module(codesio,
	  [ format_to_codes/3,		% +Format, +Args, -Codes
	    format_to_codes/4,		% +Format, +Args, -Codes, ?Tail
	    write_to_codes/2,		% +Term, -Codes
	    write_to_codes/3,		% +Term, -Codes, ?Tail
	    write_term_to_codes/3,	% +Term, -Codes, +Options
	    write_term_to_codes/4,	% +Term, -Codes, ?Tail, +Options
					% read predicates
	    read_from_codes/2,		% +Codes, -Term
	    read_term_from_codes/3,	% +Codes, -Term, +Options
	    open_codes_stream/2,	% +Codes, -Stream
	    with_output_to_codes/2,	% :Goal, -Codes
	    with_output_to_codes/3,	% :Goal, -Codes, ?Tail
	    with_output_to_codes/4	% :Goal, -Stream, -Codes, ?Tail
	  ]).
:- use_module(library(error)).
/* :- use_module(library(memfile)). */	% see open_codes_stream/2

:- meta_predicate
	with_output_to_codes(0, -),
	with_output_to_codes(0, -, ?),
	with_output_to_codes(0, -, -, ?).

:- predicate_options(read_term_from_codes/3, 3,
		     [pass_to(system:read_term/3, 3)]).
:- predicate_options(write_term_to_codes/3, 3,
		     [pass_to(system:write_term/3, 3)]).
:- predicate_options(write_term_to_codes/4, 4,
		     [pass_to(system:write_term/3, 3)]).

/** <module> I/O on Lists of Character Codes

This module emulates the SICStus  library   codesio.pl  for  reading and
writing from/to lists of character codes.   Most of these predicates are
straight calls into similar SWI-Prolog primitives.

This library is based on library(charsio)   that originates from Quintus
Prolog. The naming is updated to reflect  the ISO naming conventions and
the ISO predicats atom_codes/2, etc  are   obviously  removed  from this
library.

@compat SICStus 4
*/

%%	format_to_codes(+Format, +Args, -Codes) is det.
%
%	Use format/2 to write to a list of character codes.

format_to_codes(Format, Args, Codes) :-
	format(codes(Codes), Format, Args).

%%	format_to_codes(+Format, +Args, -Codes, ?Tail) is det.
%
%	Use format/2 to write to a difference list of character codes.

format_to_codes(Format, Args, Codes, Tail) :-
	format(codes(Codes, Tail), Format, Args).

%%	write_to_codes(+Term, -Codes)
%
%	Codes is a list of character codes produced by write/1 on Term.

write_to_codes(Term, Codes) :-
	format(codes(Codes), '~w', [Term]).

%%	write_to_codes(+Term, -Codes, ?Tail)
%
%	Codes is a difference-list of character codes produced by write/1 on Term.

write_to_codes(Term, Codes, Tail) :-
	format(codes(Codes, Tail), '~w', [Term]).

%%	write_term_to_codes(+Term, -Codes, +Options) is det.
%
%	True  when  Codes  is  a  string  that  matches  the  output  of
%	write_term/3 using Options.

write_term_to_codes(Term, Codes, Options) :-
	format(codes(Codes), '~W', [Term, Options]).

%%	write_term_to_codes(+Term, -Codes, ?Tail, +Options) is det.
%
%	True  when  Codes\Tail  is  a  difference  list  containing  the
%	character codes that matches the   output  of write_term/3 using
%	Options.

write_term_to_codes(Term, Codes, Tail, Options) :-
	format(codes(Codes, Tail), '~W', [Term, Options]).

%%	read_from_codes(+Codes, -Term) is det.
%
%	Read Codes into Term.
%
%	@compat	The SWI-Prolog version does not require Codes to end
%		in a full-stop.

read_from_codes("", end_of_file) :- !.
read_from_codes(List, Term) :-
	atom_to_term(List, Term, _).

%%	read_term_from_codes(+Codes, -Term, +Options) is det.
%
%	Read Codes into Term.  Options are processed by read_term/3.
%
%	@compat sicstus

read_term_from_codes(Codes, Term, Options) :-
	setup_call_cleanup(open_codes_stream(Codes, Stream, '\n.\n'),
			   read_term(Stream, Term0, Options),
			   close(Stream)),
	Term = Term0.

%%	open_codes_stream(+Codes, -Stream) is det.
%
%	Open Codes as an input stream.
%
%	@bug	Depends on autoloading library(memfile).  As many
%		applications do not need this predicate we do not
%		want to make the entire library dependent on
%		autoloading.

open_codes_stream(Codes, Stream) :-
	open_codes_stream(Codes, Stream, '').

open_codes_stream(Codes, Stream, Postfix) :-
	new_memory_file(MF),
	setup_call_cleanup(
	    open_memory_file(MF, write, Out),
	    format(Out, '~s~w', [Codes, Postfix]),
	    close(Out)),
	open_memory_file(MF, read, Stream,
			 [ free_on_close(true)
			 ]).

%%	with_output_to_codes(:Goal, Codes) is det.
%
%	Run Goal with as once/1.  Output written to =current_output=
%	is collected in Codes.

with_output_to_codes(Goal, Codes) :-
	with_output_to(codes(Codes), Goal).

%%	with_output_to_codes(:Goal, -Codes, ?Tail) is det.
%
%	Run Goal with as once/1.  Output written to =current_output=
%	is collected in Codes\Tail.

with_output_to_codes(Goal, Codes, Tail) :-
	with_output_to(codes(Codes, Tail), Goal).

%%	with_output_to_codes(:Goal, -Stream, -Codes, ?Tail) is det.
%
%	As  with_output_to_codes/2,  but  Stream  is  unified  with  the
%	temporary stream.

with_output_to_codes(Goal, Stream, Codes, Tail) :-
	with_output_to(codes(Codes, Tail), with_stream(Stream, Goal)).

with_stream(Stream, Goal) :-
	current_output(Stream),
	call(Goal).
