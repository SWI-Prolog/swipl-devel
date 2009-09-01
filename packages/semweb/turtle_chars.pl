/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2006, University of Amsterdam

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

:- module(turtle_unicode,
	  [ mkclassify/1,
	    run/0
	  ]).

run :-
	mkclassify('turtle_chars.c', 'static ').

%%	mkclassify(+File)
%
%	Generate the core of xml_unicode.c.

mkclassify(File) :-
	mkclassify(File, '').

mkclassify(File, Decl) :-
	tell(File),
	call_cleanup(forall(list(List, _),
			    mkfunc(List, Decl)),
		     told).

mkfunc(Name, Decl) :-
	format('~wint~n', [Decl]),
	format('wcis_~w(int c)~n', [Name]),
	format('{ '),
	list(Name, List),
	mkswitch(List),
	format('}~n~n').

mkswitch(List) :-
	mkswitch(List, 2).

mkswitch([Low-High], Indent) :- !,
	indent(Indent),
	format('return (c >= 0x~|~`0t~16r~4+ && c <= 0x~|~`0t~16r~4+);~n', [Low, High]).
mkswitch([Value], Indent) :- !,
	indent(Indent),
	format('return (c == 0x~|~`0t~16r~4+);', [Value]).
mkswitch(List, Indent) :-
	split(List, Low, High),
	end(Low, MaxLow),
	indent(Indent),
	NextIndent is Indent + 2,
	format('if ( c <= 0x~|~`0t~16r~4+ )~n', [MaxLow]),
	indent(Indent),
	format('{ '),
	mkswitch(Low, NextIndent),
	indent(Indent),
	format('} else~n'),
	indent(Indent),
	format('{ '),
	mkswitch(High, NextIndent),
	indent(Indent),
	format('}~n').

end(List, Max) :-
	last(List, Last),
	(   Last = _-Max
	->  true
	;   Max = Last
	).

split(List, Low, High) :-
	length(List, Len),
	Mid is Len//2,
	length(Low, Mid),
	append(Low, High, List).

indent(N) :-
	line_position(current_output, Pos),
	Spaces is N - Pos,
	format('~*c', [Spaces, 32]).



list(name_start_char,
     [ 0'A-0'Z,
       0'_,
       0'a-0'z,
       0x00C0-0x00D6,
       0x00D8-0x00F6,
       0x00F8-0x02FF,
       0x0370-0x037D,
       0x037F-0x1FFF,
       0x200C-0x200D,
       0x2070-0x218F,
       0x2C00-0x2FEF,
       0x3001-0xD7FF,
       0xF900-0xFDCF,
       0xFDF0-0xFFFD,
       0x10000-0xEFFFF
     ]).

list(name_extender_char,
     [ 0'-,
       0'0-0'9,
       0x00B7,
       0x0300-0x036F,
       0x203F-0x2040
     ]).
