/*  Part of SWI-Prolog

    Author:        Jan Wielemaker & Paulo Moura
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2007, University of Amsterdam, Paulo Moura

    This program is distributed under the Perl Artistic License.
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
@licence Perl Artistic License
*/

%%	iso_builtin_predicate(?Head:callable) is nondet.
%
%	True if Head describes  a  builtin   defined  by  the ISO Prolog
%	standard (ISO/IEC 1321 l-l).

iso_builtin_predicate(true).
iso_builtin_predicate(fail).
iso_builtin_predicate(call(_)).
iso_builtin_predicate(!).
iso_builtin_predicate((_; _)).
iso_builtin_predicate((_, _)).
iso_builtin_predicate((_ -> _)).
iso_builtin_predicate((_ -> _ ; _)).
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
iso_builtin_function(atan(_)).
iso_builtin_function(exp(_)).
iso_builtin_function(log(_)).
iso_builtin_function(sqrt(_)).
iso_builtin_function(\(_)).
