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

:- module(json,
	  [ json_read/2,		% +Stream, -JSONTerm
	    json_read/3,		% +Stream, -JSONTerm, +Options
	    atom_json_term/3,		% ?Atom, ?JSONTerm, +Options
	    json_write/2,		% +Stream, +Term
	    json_write/3		% +Stream, +Term
	  ]).
:- use_module(library(record)).
:- use_module(library(memfile)).
:- use_module(library(error)).
:- use_module(library(option)).

:- initialization
   load_foreign_library(foreign(json)).

/** <module> JSON support

This module supports reading and writing JSON objects. The canonical
Prolog representation for JSON terms is defined as:

    * A JSON object is mapped to a term object(NameValueList), where
    NameValueList is a list of Name=Value. Name is an atom created from
    the JSON string. I.e.

    ==
    { "name":"SWI-Prolog", "url":"http://www.swi-prolog.org" } -->
    object([name='SWI-Prolog', url='http://www.swi-prolog.org'])
    ==
    
    * A JSON array is mapped to a Prolog list of JSON values.

    * A JSON string is mapped to a Prolog atom
    
    * A JSON number is mapped to a Prolog number
    
    * The JSON constants =true= and =false= are mapped -like JPL- to
    @(true) and @(false). 

    * The JSON constant =null= is mapped to the Prolog term @(null)


@author Jan Wielemaker
*/

:- record json_options(null:ground = @(null),
		       true:ground = @(true),
		       false:ground = @(false),
		       value_string_as:oneof([atom,string]) = atom).


		 /*******************************
		 *	       MAP		*
		 *******************************/

%%	atom_json_term(+Atom, -JSONTerm, +Options) is det.
%%	atom_json_term(-Text, +JSONTerm, +Options) is det.
%
%	Convert between textual  representation  and   a  JSON  term. In
%	_write_ mode, the option as(Type) defines the output type, which
%	is one of =atom=, =string= or =codes=.

atom_json_term(Atom, Term, Options) :-
	ground(Atom),
	atom_to_memory_file(Atom, MF),
	open_memory_file(MF, read, In),
	call_cleanup(json_read(In, Term, Options),
		     (	 close(In),
			 free_memory_file(MF))).
atom_json_term(Result, Term, Options) :-
	select_option(as(Type), Options, Options1),
	(   type_term(Type, Result, Out)
	->  true
	;   must_be(oneof([atom,string,codes]), Type)
	),
	with_output_to(Out,
		       json_write(current_ouput, Term, Options1)).

type_term(atom,   Result, atom(Result)).
type_term(string, Result, string(Result)).
type_term(codes,  Result, codes(Result)).


		 /*******************************
		 *	     READING		*
		 *******************************/

%%	json_parse(+Stream, -Term) is det.
%%	json_parse(+Stream, -Term, +Options) is det.
%
%	Read next JSON value from Stream into a Prolog term. Options
%	are:
%	
%		* null(NullTerm)
%		Term used to represent JSON =null=.
%		* true(TrueTerm)
%		* false(FalsTerm)
%		* value_string_as(Type)

json_read(Stream, Term) :-
	default_json_options(Options),
	json_value(Stream, Term, _, Options).

json_read(Stream, Term, Options) :-
	make_json_options(Options, OptionTerm),
	json_value(Stream, Term, _, OptionTerm).


json_value(Stream, Term, Next, Options) :-
	get_code(Stream, C0),
	ws(C0, Stream, C1),
	json_term(C1, Stream, Term, Next, Options).

json_term(0'{, Stream, object(Pairs), Next, Options) :- !,
	ws(Stream, C),
	json_pairs(C, Stream, Pairs, Options),
	get_code(Stream, Next).
json_term(0'[, Stream, Array, Next, Options) :- !,
	ws(Stream, C),
	json_array(C, Stream, Array, Options),
	get_code(Stream, Next).
json_term(0'", Stream, String, Next, Options) :- !,
	get_code(Stream, C1),
	json_string_codes(C1, Stream, Codes),
	json_options_value_string_as(Options, Type),
	codes_to_type(Type, Codes, String),
	get_code(Stream, Next).
json_term(0'-, Stream, Number, Next, _Options) :- !,
	json_number_codes(Stream, Codes, Next),
	number_codes(Number, [0'-|Codes]).
json_term(D, Stream, Number, Next, _Options) :-
	between(0'0, 0'9, D), !,
	json_number_codes(Stream, Codes, Next),
	number_codes(Number, [D|Codes]).
json_term(C, Stream, Constant, Next, Options) :-
	get_code(Stream, C1),
	json_identifier_codes(C1, Stream, Codes, Next),
	atom_codes(ID, [C|Codes]),
	json_constant(ID, Constant, Options).

json_pairs(0'}, _, [], _) :- !.
json_pairs(C0, Stream, [Pair|Tail], Options) :- 
	json_pair(C0, Stream, Pair, C, Options),
	ws(C, Stream, Next),
	json_pairs(Next, Stream, Tail, Options).

json_pair(C0, Stream, Name=Value, Next, Options) :-
	json_string_as_atom(C0, Stream, Name),
	ws(Stream, C),
	C == 0':,
	json_value(Stream, Value, Next, Options).
	

json_array(0'], _, [], _) :- !.
json_array(C0, Stream, [Value|Tail], Options) :- 
	json_term(C0, Stream, Value, C, Options),
	ws(C, Stream, Next),
	(   Next == 0',
	->  ws(Stream, C1),
	    json_array(C1, Stream, Tail, Options)
	;   Next == 0']
	->  Tail = []
	).

codes_to_type(atom, Codes, Atom) :-
	atom_codes(Atom, Codes).
codes_to_type(string, Codes, Atom) :-
	string_to_list(Atom, Codes).
codes_to_type(codes, Codes, Codes).

json_string_as_atom(0'", Stream, Atom) :-
	get_code(Stream, C1),
	json_string_codes(C1, Stream, Codes),
	atom_codes(Atom, Codes).

json_string_codes(0'", _, []) :- !.
json_string_codes(0'\\, Stream, [H|T]) :- !,
	get_code(Stream, C0),
	(   escape(C0, Stream, H)
	->  true
	;   syntax_error(illegal_string_escape, Stream)
	),
	get_code(Stream, C1),
	json_string_codes(C1, Stream, T).
json_string_codes(C, Stream, [C|T]) :-
	get_code(Stream, C1),
	json_string_codes(C1, Stream, T).

escape(0'\\, _, 0'\\) :- !.
escape(0'/, _, 0'/) :- !.
escape(0'b, _, 0'\b) :- !.
escape(0'f, _, 0'\f) :- !.
escape(0'n, _, 0'\n) :- !.
escape(0'r, _, 0'\r) :- !.
escape(0't, _, 0'\t) :- !.
escape(0'u, Stream, C) :- !,
	get_code(Stream, C1),
	get_code(Stream, C2),
	get_code(Stream, C3),
	get_code(Stream, C4),
	code_type(C1, xdigit(D1)),
	code_type(C2, xdigit(D2)),
	code_type(C3, xdigit(D3)),
	code_type(C4, xdigit(D4)),
	C is D1<<12+D2<<8+D3<<4+D4.		  

json_number_codes(Stream, Codes, Next) :-
	get_code(Stream, C1),
	json_number_codes(C1, Stream, Codes, Next).

json_number_codes(C1, Stream, [C1|Codes], Next) :-
	number_code(C1), !,
	get_code(Stream, C2),
	json_number_codes(C2, Stream, Codes, Next).
json_number_codes(C, _, [], C).
	
number_code(C) :-
	between(0'0, 0'9, C), !.
number_code(0'.).
number_code(0'-).
number_code(0'e).
number_code(0'E).

json_identifier_codes(C1, Stream, [C1|T], Next) :-
	between(0'a, 0'z, C1), !,
	get_code(Stream, C2),
	json_identifier_codes(C2, Stream, T, Next).
json_identifier_codes(C, _, [], C).


json_constant(true, Constant, Options) :- !,
	json_options_true(Options, Constant).
json_constant(false, Constant, Options) :- !,
	json_options_false(Options, Constant).
json_constant(null, Constant, Options) :- !,
	json_options_null(Options, Constant).

%%	ws(+Stream, -Next) is det.
%%	ws(+C0, +Stream, -Next)
%
%	Skip white space on the Stream, returning the first non-ws
%	character.  Also skips =|//|= ... comments.

ws(Stream, Next) :-
	get_code(Stream, C0),
	ws(C0, Stream, Next).

ws(C0, Stream, C) :-
	ws(C0), !,
	get_code(Stream, C1),
	ws(C1, Stream, C).
ws(0'/, Stream, C) :- !,
	get_code(Stream, Cmt1), !,
	expect(Cmt1, 0'/, Stream),
	skip(Stream, 0'\n),
	get_code(Stream, C).
ws(C, _, C).

ws(0' ).
ws(0'\t).
ws(0'\n).
ws(0'\r).

expect(C, C, _) :- !.
expect(_, 0'/, Stream) :- !,
	syntax_error(illegal_comment, Stream).

syntax_error(Message, Stream) :-
	stream_error_context(Stream, Context),
	throw(error(syntax_error(json(Message)), Context)).

stream_error_context(Stream, stream(Stream, Line, LinePos, CharNo)) :-
	character_count(Stream, CharNo),
	line_position(Stream, LinePos),
	line_count(Stream, Line).


		 /*******************************
		 *	    JSON OUTPUT		*
		 *******************************/

%%	json_write_string(+Stream, +Text) is det.
%
%	Write a JSON string to  Stream.  Stream   must  be  opened  in a
%	Unicode capable encoding, typically UTF-8.

% foreign json_write_string/2.

%%	json_write_indent(+Stream, +Indent, +TabDistance) is det.
%
%	Newline and indent to  Indent.  A   Newline  is  only written if
%	line_position(Stream, Pos) is not 0. Then   it  writes Indent //
%	TabDistance tab characters and Indent mode TabDistance spaces.

% foreign json_write_indent/3.

%%	json_write(+Stream, +Term) is det.
%%	json_write(+Stream, +Term, +Options) is det.
%
%	Write a JSON term to Stream.  The   JSON  object  is of the same
%	format as produced by json_read/2, though we allow for some more
%	flexibility with regard to pairs in  objects. All of Name=Value,
%	Name-Value and Name(Value) produce the  same output. In addition
%	to  the  options  recognised  by  json_read/3,  we  process  the
%	following options are recognised:
%	
%	    * width(+Width)
%	    Width in which we try to format the result.  Too long lines
%	    switch from _horizontal_ to _vertical_ layout for better
%	    readability. If performance is critical and human
%	    readability is not an issue use Width = 0, which causes a
%	    single-line output.
%	    
%	    * step(+Step)
%	    Indentation increnment for next level.  Default is 2.
%	    
%	    * tab(+TabDistance)
%	    Distance between tab-stops.  If equal to Step, layout
%	    is generated with one tab per level.
%	
%	@tbd	Allow for inline expansion or some other mechanism
%		to make it easy to send arbitrary Prolog data.

:- record json_write_state(indent:nonneg = 0,
			   step:positive_integer = 2,
			   tab:positive_integer = 8,
			   width:nonneg = 72).
			   
json_write(Stream, Term) :-
	json_write(Stream, Term, []).
json_write(Stream, Term, Options) :-
	make_json_write_state(Options, State, Options1),
	make_json_options(Options1, OptionTerm),
	json_write_term(Term, Stream, State, OptionTerm).

json_write_term(Var, _, _, _) :-
	var(Var), !,
	instantiation_error(Var).
json_write_term(object(Pairs), Stream, State, Options) :- !,
	space_if_not_at_left_margin(Stream),
	write(Stream, '{'),
	(   json_write_state_width(State, Width),
	    (   Width == 0
	    ->  true
	    ;   json_write_state_indent(State, Indent),
		json_print_length(object(Pairs), Width, Indent, _)
	    )
	->  set_width_of_json_write_state(0, State, State2),
	    write_pairs_hor(Pairs, Stream, State2, Options),
	    write(Stream, '}')
	;   step_indent(State, State2),
	    write_pairs_ver(Pairs, Stream, State2, Options),
	    indent(Stream, State),
	    write(Stream, '}')
	).
json_write_term(List, Stream, State, Options) :-
	is_list(List), !,
	space_if_not_at_left_margin(Stream),
	write(Stream, '['),
	(   json_write_state_width(State, Width),
	    (   Width == 0
	    ->  true
	    ;   json_write_state_indent(State, Indent),
		json_print_length(List, Width, Indent, _)
	    )
	->  set_width_of_json_write_state(0, State, State2),
	    write_array_hor(List, Stream, State2, Options),
	    write(Stream, ']')
	;   step_indent(State, State2),
	    write_array_ver(List, Stream, State2, Options),
	    indent(Stream, State),
	    write(Stream, ']')
	).
json_write_term(Number, Stream, _State, _Options) :-
	number(Number), !,
	write(Stream, Number).
json_write_term(True, Stream, _State, Options) :-
	json_options_true(Options, True), !,
	write(Stream, true).
json_write_term(False, Stream, _State, Options) :-
	json_options_true(Options, False), !,
	write(Stream, false).
json_write_term(Null, Stream, _State, Options) :-
	json_options_true(Options, Null), !,
	write(Stream, null).
json_write_term(String, Stream, _State, _Options) :-
	json_write_string(Stream, String).

write_pairs_hor([], _, _, _).
write_pairs_hor([H|T], Stream, State, Options) :-
	json_pair(H, Name, Value),
	json_write_string(Stream, Name),
	write(Stream, ':'),
	json_write_term(Value, Stream, State, Options),
	(   T == []
	->  true
	;   write(Stream, ', '),
	    write_pairs_hor(T, Stream, State, Options)
	).
	
write_pairs_ver([], _, _, _).
write_pairs_ver([H|T], Stream, State, Options) :-
	indent(Stream, State),
	json_pair(H, Name, Value),
	json_write_string(Stream, Name),
	write(Stream, ':'),
	json_write_term(Value, Stream, State, Options),
	(   T == []
	->  true
	;   write(Stream, ','),
	    write_pairs_ver(T, Stream, State, Options)
	).


json_pair(Var, _, _) :-
	var(Var), !,
	instantiation_error(Var).
json_pair(Name=Value, Name, Value) :- !.
json_pair(Name-Value, Name, Value) :- !.
json_pair(NameValue, Name, Value) :-
	NameValue =.. [Name, Value].


write_array_hor([], _, _, _).
write_array_hor([H|T], Stream, State, Options) :-
	json_write_term(H, Stream, State, Options),
	(   T == []
	->  true
	;   write(Stream, ', '),
	    write_array_hor(T, Stream, State, Options)
	).

write_array_ver([], _, _, _).
write_array_ver([H|T], Stream, State, Options) :-
	indent(Stream, State),
	json_write_term(H, Stream, State, Options),
	(   T == []
	->  true
	;   write(Stream, ','),
	    write_array_ver(T, Stream, State, Options)
	).
	

indent(Stream, State) :-
	json_write_state_indent(State, Indent),
	json_write_state_tab(State, Tab),
	json_write_indent(Stream, Indent, Tab).

step_indent(State0, State) :-
	json_write_state_indent(State0, Indent),
	json_write_state_step(State0, Step),
	NewIndent is Indent+Step,
	set_indent_of_json_write_state(NewIndent, State0, State).

space_if_not_at_left_margin(Stream) :-
	line_position(Stream, 0), !.
space_if_not_at_left_margin(Stream) :-
	put_char(Stream, ' ').


%%	json_print_length(+Value, +Max, +Len0, +Len) is semidet.  
%
%	True if Len-Len0 is the print-length of Value on a single line
%	and Len-Len0 =< Max.
%	
%	@tbd	Escape sequences in strings are not considered.

json_print_length(object(Pairs), Max, Len0, Len) :- !,
	Len1 is Len0 + 2,
	Len1 =< Max,
	pairs_print_length(Pairs, Max, Len1, Len).
json_print_length(Array, Max, Len0, Len) :-
	is_list(Array), !,
	Len1 is Len0 + 2,
	Len1 =< Max,
	array_print_length(Array, Max, Len1, Len).
json_print_length(Number, Max, Len0, Len) :-
	number(Number), !,
	atom_length(Number, AL),
	Len is Len0 + AL,
	Len =< Max.
json_print_length(String, Max, Len0, Len) :-
	string_len(String, Len0, Len),
	Len =< Max.

pairs_print_length([], _, Len, Len).
pairs_print_length([H|T], Max, Len0, Len) :-
	pair_len(H, Max, Len0, Len1),
	(   T == []
	->  Len = Len1
	;   Len2 is Len1 + 2,
	    Len2 =< Max,
	    pairs_print_length(T, Max, Len2, Len)
	).

pair_len(Name=Value, Max, Len0, Len) :-
	string_len(Name, Len0, Len1),
	Len2 is Len1+2,
	Len2 =< Max,
	json_print_length(Value, Max, Len2, Len).

array_print_length([], _, Len, Len).
array_print_length([H|T], Max, Len0, Len) :-
	json_print_length(H, Max, Len0, Len1),
	(   T == []
	->  Len = Len1
	;   Len2 is Len1+2,
	    Len2 =< Max,
	    array_print_length(T, Max, Len2, Len)
	).

string_len(String, Len0, Len) :-
	atom_length(String, AL),
	Len is Len0 + AL + 2.



		 /*******************************
		 *	     MESSAGES		*
		 *******************************/

:- multifile
	prolog:error_message/3.

prolog:error_message(syntax_error(json(Id))) -->
	[ 'JSON syntax error: ' ],
	json_syntax_error(Id).

json_syntax_error(illegal_comment) -->
	[ 'Illegal comment' ].
json_syntax_error(illegal_string_escape) -->
	[ 'Illegal escape sequence in string' ].
