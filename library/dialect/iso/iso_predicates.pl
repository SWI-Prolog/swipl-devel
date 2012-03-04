/*  Part of SWI-Prolog

    Author:        Jan Wielemaker & Paulo Moura
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2007-2012, University of Amsterdam,
			      Paulo Moura,
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

    Alternatively, this program may be distributed under the Perl
    Artistic License, version 2.0.
*/


:- module(iso_predicates,
	  [ iso_builtin_predicate/1,	% ?Term
	    iso_builtin_function/1	% ?Term
	  ]).

/** <module> Define ISO builtin predicates

This module describes the ISO  core   standard  built-in  predicates. It
originates from Logtalk. Note that, although  the artistic license would
allow redistribution under a new name and   interface  using the GPL, we
preserve the original license.

@author  Paulo Moura
@author  Jan Wielemaker
@licence GPL+SWI-exception or Artistic 2.0
@see	 predicate_property/2, property =iso=.
*/

%%	iso_builtin_predicate(?Head:callable) is nondet.
%
%	True if Head describes  a  builtin   defined  by  the ISO Prolog
%	standard (ISO/IEC 1321 l-l).

iso_builtin_predicate(true).
iso_builtin_predicate(fail).
iso_builtin_predicate(call(_)).
iso_builtin_predicate(!).
iso_builtin_predicate((Goal; _)) :-
	(   var(Goal)
	->  true
	;   Goal \= '*->'(_,_)		% exclude SWI's soft-cut.
	).
iso_builtin_predicate((_, _)).
iso_builtin_predicate((_ -> _)).
iso_builtin_predicate(catch(_, _, _)).
iso_builtin_predicate(throw(_)).

iso_builtin_predicate((_ = _)).
iso_builtin_predicate((_ \= _)).
iso_builtin_predicate(unify_with_occurs_check(_, _)).

iso_builtin_predicate(var(_)).
iso_builtin_predicate(nonvar(_)).
iso_builtin_predicate(atom(_)).
iso_builtin_predicate(atomic(_)).
iso_builtin_predicate(number(_)).
iso_builtin_predicate(integer(_)).
iso_builtin_predicate(float(_)).
iso_builtin_predicate(compound(_)).

iso_builtin_predicate((_ @=< _)).
iso_builtin_predicate((_ @< _)).
iso_builtin_predicate((_ @>= _)).
iso_builtin_predicate((_ @> _)).
iso_builtin_predicate((_ == _)).
iso_builtin_predicate((_ \== _)).

iso_builtin_predicate(functor(_, _, _)).
iso_builtin_predicate(arg(_, _, _)).
iso_builtin_predicate(_ =.. _).
iso_builtin_predicate(copy_term(_, _)).

iso_builtin_predicate(_ is _).

iso_builtin_predicate((_ =< _)).
iso_builtin_predicate((_ < _)).
iso_builtin_predicate((_ >= _)).
iso_builtin_predicate((_ > _)).
iso_builtin_predicate((_ =:= _)).
iso_builtin_predicate((_ =\= _)).

iso_builtin_predicate(clause(_, _)).
iso_builtin_predicate(current_predicate(_)).

iso_builtin_predicate(asserta(_)).
iso_builtin_predicate(assertz(_)).
iso_builtin_predicate(retract(_)).
iso_builtin_predicate(abolish(_)).

iso_builtin_predicate(findall(_, _, _)).
iso_builtin_predicate(bagof(_, _, _)).
iso_builtin_predicate(setof(_, _, _)).

iso_builtin_predicate(current_input(_)).
iso_builtin_predicate(current_output(_)).
iso_builtin_predicate(set_input(_)).
iso_builtin_predicate(set_output(_)).
iso_builtin_predicate(open(_, _, _, _)).
iso_builtin_predicate(open(_, _, _)).
iso_builtin_predicate(close(_, _)).
iso_builtin_predicate(close(_)).
iso_builtin_predicate(flush_output(_)).
iso_builtin_predicate(flush_output).
iso_builtin_predicate(stream_property(_, _)).
iso_builtin_predicate(at_end_of_stream).
iso_builtin_predicate(at_end_of_stream(_)).
iso_builtin_predicate(set_stream_position(_, _)).

iso_builtin_predicate(get_char(_, _)).
iso_builtin_predicate(get_char(_)).
iso_builtin_predicate(get_code(_, _)).
iso_builtin_predicate(get_code(_)).
iso_builtin_predicate(peek_char(_, _)).
iso_builtin_predicate(peek_char(_)).
iso_builtin_predicate(peek_code(_, _)).
iso_builtin_predicate(peek_code(_)).
iso_builtin_predicate(put_char(_, _)).
iso_builtin_predicate(put_char(_)).
iso_builtin_predicate(put_code(_, _)).
iso_builtin_predicate(put_code(_)).
iso_builtin_predicate(nl).
iso_builtin_predicate(nl(_)).

iso_builtin_predicate(get_byte(_, _)).
iso_builtin_predicate(get_byte(_)).
iso_builtin_predicate(peek_byte(_, _)).
iso_builtin_predicate(peek_byte(_)).
iso_builtin_predicate(put_byte(_, _)).
iso_builtin_predicate(put_byte(_)).

iso_builtin_predicate(read_term(_, _, _)).
iso_builtin_predicate(read_term(_, _)).
iso_builtin_predicate(read(_)).
iso_builtin_predicate(read(_, _)).
iso_builtin_predicate(write_term(_, _, _)).
iso_builtin_predicate(write_term(_, _)).
iso_builtin_predicate(write(_)).
iso_builtin_predicate(write(_, _)).
iso_builtin_predicate(writeq(_)).
iso_builtin_predicate(writeq(_, _)).
iso_builtin_predicate(write_canonical(_)).
iso_builtin_predicate(write_canonical(_, _)).
iso_builtin_predicate(op(_, _, _)).
iso_builtin_predicate(current_op(_, _, _)).
iso_builtin_predicate(char_conversion(_, _)).
iso_builtin_predicate(current_char_conversion(_, _)).

iso_builtin_predicate(\+ _).
iso_builtin_predicate(once(_)).
iso_builtin_predicate(repeat).

iso_builtin_predicate(atom_length(_, _)).
iso_builtin_predicate(atom_concat(_, _, _)).
iso_builtin_predicate(sub_atom(_, _, _, _, _)).
iso_builtin_predicate(atom_chars(_, _)).
iso_builtin_predicate(atom_codes(_, _)).
iso_builtin_predicate(char_code(_, _)).
iso_builtin_predicate(number_chars(_, _)).
iso_builtin_predicate(number_codes(_, _)).

iso_builtin_predicate(set_prolog_flag(_, _)).
iso_builtin_predicate(current_prolog_flag(_, _)).
iso_builtin_predicate(halt).
iso_builtin_predicate(halt(_)).

%	ISO wg17 proposal on threads
%	Note that the list below only contains the predicates that are
%	-in my opinion- beyond discussion.

iso_builtin_predicate(thread_create(_,_,_)).
iso_builtin_predicate(thread_self(_)).
iso_builtin_predicate(thread_join(_,_)).
iso_builtin_predicate(thread_detach(_)).
iso_builtin_predicate(thread_signal(_,_)).
iso_builtin_predicate(thread_property(_,_)).
iso_builtin_predicate(mutex_create(_,_)).
iso_builtin_predicate(mutex_destroy(_)).
iso_builtin_predicate(with_mutex(_,_)).
iso_builtin_predicate(mutex_lock(_)).
iso_builtin_predicate(mutex_unlock(_)).
iso_builtin_predicate(mutex_trylock(_)).
iso_builtin_predicate(mutex_property(_,_)).
iso_builtin_predicate(message_queue_create(_,_)).
iso_builtin_predicate(message_queue_destroy(_)).
iso_builtin_predicate(message_queue_property(_,_)).
iso_builtin_predicate(thread_send_message(_,_)).
iso_builtin_predicate(thread_get_message(_)).
iso_builtin_predicate(thread_get_message(_,_)).
iso_builtin_predicate(thread_peek_message(_)).
iso_builtin_predicate(thread_peek_message(_,_)).

%	Predicates from ISO/IEC DTR 13211-1:2006
%	Note that predicates not supported yet by YAP are not in here.

%iso_builtin_predicate(subsumes(_,_)).
iso_builtin_predicate(callable(_)).
iso_builtin_predicate(ground(_)).
iso_builtin_predicate(compare(_, _, _)).
iso_builtin_predicate(numbervars(_,_,_)).
iso_builtin_predicate(predicate_property(_,_)).
iso_builtin_predicate(retractall(_)).
%iso_builtin_predicate(forall(_,_)).
iso_builtin_predicate(call(_,_)).
iso_builtin_predicate(call(_,_,_)).
iso_builtin_predicate(call(_,_,_,_)).
%iso_builtin_predicate(call_cleanup(_,_)).
%iso_builtin_predicate(append(_,_,_)).
iso_builtin_predicate(length(_,_)).
%iso_builtin_predicate(member(_,_)).
iso_builtin_predicate(sort(_,_)).
iso_builtin_predicate(keysort(_,_)).

%%	iso_builtin_function(?Head:callable) is nondet.
%
%	True if Head describes a builtin  arithmetic function as defined
%	by the ISO Prolog standard (ISO/IEC 1321 l-l).

iso_builtin_function(_ + _).
iso_builtin_function(_ - _).
iso_builtin_function(_ * _).
iso_builtin_function(_ // _).
iso_builtin_function(_ / _).
iso_builtin_function(_ rem _).
iso_builtin_function(_ mod _).
iso_builtin_function(_ ** _).
iso_builtin_function(_ >> _).
iso_builtin_function(_ << _).
iso_builtin_function(_ /\ _).
iso_builtin_function(_ \/ _).
iso_builtin_function(- _).
iso_builtin_function(+ _).
iso_builtin_function(min(_,_)).
iso_builtin_function(max(_,_)).
iso_builtin_function(abs(_)).
iso_builtin_function(sign(_)).
iso_builtin_function(float_integer_part(_)).
iso_builtin_function(float_fractional_part(_)).
iso_builtin_function(float(_)).
iso_builtin_function(floor(_)).
iso_builtin_function(truncate(_)).
iso_builtin_function(round(_)).
iso_builtin_function(ceiling(_)).
iso_builtin_function(sin(_)).
iso_builtin_function(cos(_)).
iso_builtin_function(asin(_)).
iso_builtin_function(acos(_)).
iso_builtin_function(atan(_)).
iso_builtin_function(atan2(_,_)).
iso_builtin_function(exp(_)).
iso_builtin_function(log(_)).
iso_builtin_function(sqrt(_)).
iso_builtin_function(\(_)).
