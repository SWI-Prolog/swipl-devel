/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2008, University of Amsterdam

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

:- module(pure_input,
	  [ phrase_from_file/2,		% :Grammar, +File
	    phrase_from_file/3,		% :Grammar, +File, +Options
	    stream_to_lazy_list/2
	  ]).
:- use_module(library(option)).

/** <module> Pure Input from files

This module is part of pio.pl,   dealing with _pure_ _input_: processing
input streams from the outside  world   using  pure  predicates, notably
grammar rules (DCG).  Using  pure   predicates  makes  non-deterministic
processing of input much simpler.

Pure input uses coroutining (freeze/2) to   read input from the external
source into a list _|on demand|_. The   overhead of lazy reading is more
than compensated for by using block reads based on read_pending_input/3.

@tbd	Provide support for alternative input readers, e.g. reading
	terms, tokens, etc.
@tbd	Support non-repositioning streams, such as sockets and pipes.
@author Ulrich Neumerkel
@author Jan Wielemaker
*/

:- predicate_options(phrase_from_file/3, 3,
		     [ buffer_size(positive_integer),
		       pass_to(system:open/4, 4)
		     ]).

%%	phrase_from_file(:Grammar, +File) is nondet.
%
%	Process the content of File  using   the  DCG  rule Grammar. The
%	space usage of this mechanism depends on   the length of the not
%	committed part of Grammar. Committed parts of the temporary list
%	are reclaimed by the  garbage  collector,   while  the  list  is
%	extended on demand.  Here  is  a   very  simple  definition  for
%	searching a string in a file:
%
%	==
%	... --> []|[_],... .
%
%	file_contains(File, Pattern) :-
%		phrase_from_file((..., Pattern, ...), File).
%
%	match_count(File, Pattern, Count) :-
%		findall(x, file_contains(File, Pattern), Xs),
%		length(Xs, Count).
%	==
%
%	This can be called as (note that   the  pattern must be a string
%	(code list)):
%
%	==
%	?- match_count('pure_input.pl', "file", Count).
%	==

:- meta_predicate
	phrase_from_file(//, +),
	phrase_from_file(//, +, +).

phrase_from_file(Grammar, File) :-
	phrase_from_file(Grammar, File, []).

%%	phrase_from_file(:Grammar, +File, +Options) is nondet.
%
%	As phrase_from_file/2, providing additional Options. Options are
%	passed to open/4, except for =buffer_size=,   which is passed to
%	set_stream/2. If not specified, the default   buffer size is 512
%	bytes. Of particular importance are   the  open/4 options =type=
%	and =encoding=.

phrase_from_file(Grammar, File, Options) :-
	strip_module(Grammar, M, G),
	(   select_option(buffer_size(BS), Options, OpenOptions)
	->  true
	;   BS=512,
	    OpenOptions = Options
	),
	qphrase_file(M:G, File, BS, OpenOptions).


qphrase_file(QGrammar, File, BS, Options) :-
	setup_call_cleanup(open(File, read, In, Options),
			       qphrase_stream(QGrammar, In, BS),
			       close(In)).

qphrase_stream(QGrammar, In, BuffserSize) :-
	 set_stream(In, buffer_size(BuffserSize)),
	 stream_to_lazy_list(In, List),
	 phrase(QGrammar, List).


%%	stream_to_lazy_list(+Stream, -List) is det.
%
%	Create a lazy list representing the   character codes in Stream.
%	It must be possible to reposition Stream.   List  is a list that
%	ends  in  a  delayed  goal.  List   can  be  unified  completely
%	transparent to a (partial)  list   and  processed  transparently
%	using DCGs, but please be aware that a lazy list is not the same
%	as a materialized list in all respects.
%
%	Typically, this predicate is used as   a building block for more
%	high level safe predicates such as phrase_from_file/2.
%
%	@tbd	Enhance of lazy list throughout the system.

stream_to_lazy_list(Stream, List) :-
	stream_property(Stream, position(Pos)),
	freeze(List, read_to_input_stream(Stream, Pos, List)).

read_to_input_stream(Stream, Pos, List) :-
	set_stream_position(Stream, Pos),
	(   at_end_of_stream(Stream)
	->  List = []
	;   read_pending_input(Stream, List, Tail),
	    stream_to_lazy_list(Stream, Tail)
	).
