/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org/packages/xpce/
    Copyright (C): 2010, University of Amsterdam

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

:- module(re_parse,
	  [ re_parse_loop/5
	  ]).


:- meta_predicate
	re_parse_loop(+, +, +, :, +),
	parse_line(+, +, +, :).

%	re_parse_loop(+File, +RegExVar, +ActionVar, +Generator, +EndRegEx)

re_parse_loop(File, ReVar, AVar, PatternGenerator, End) :-
	regex(End, EndRe),
	repeat,
	    (	get(File, read_line, L)
	    ->	(   EndRe \== @nil,
		    send(EndRe, match, L)
		->  !
		;   (	parse_line(L, ReVar, AVar, PatternGenerator)
		    ->	fail		% force next line
		    ;   send(File, report, warning,
			     'Failed to handle line ``%s''''', L),
		        fail
		    )
		)
	    ;	!,
		send(File, close)
	    ).


regex(@nil, @nil) :- !.
regex(String, Regex) :-
	get(string(String), value, Expanded),
	new(Regex, regex(Expanded)).


parse_line(Line, ReVar, AVar, PatternGenerator) :-
	PatternGenerator,
	regex(ReVar, Re),
	send(Re, match, Line),
	pattern_action(Re, Line, AVar, Goal),
	Goal, !.


pattern_action(Re, L, Template, Goal) :-
	functor(Template, Name, Arity),
	functor(Goal,     Name, Arity),
	End is Arity + 1,
	pattern_action(1, End, Re, L, Template, Goal).


pattern_action(N, N, _, _, _, _) :- !.
pattern_action(N, Arity, Re, L, Template, Goal) :-
	arg(N, Template, Arg),
	(   integer(Arg)
	->  get(Re, register_value, L, Arg, Value),
	    arg(N, Goal, Value)
	;   nonvar(Arg),
	    Arg = Index:Type
	->  get(Re, register_value, L, Index, RawValue),
	    get(@pce, convert, RawValue, Type, Value),
	    arg(N, Goal, Value)
	;   arg(N, Goal, Arg)
	),
	NN is N + 1,
	pattern_action(NN, Arity, Re, L, Template, Goal).



