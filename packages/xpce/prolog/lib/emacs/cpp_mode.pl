/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (C): 1985-2002, University of Amsterdam

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

:- module(emacs_cpp_mode, []).
:- use_module(library(pce)).
:- require([ forall/2
	   , ignore/1
	   ]).
:- set_prolog_flag(character_escapes, false).

:- emacs_begin_mode(cpp, c,
		    "Mode for (XPCE) C++ programs",
		    [ pce_insert_include_files = button(pce),
		      pce_collect_selectors    = button(pce),
		      pce_replace_selectors    = button(pce),
		      pce_unreplace_selectors  = button(pce)
		    ],
		    []).

from_pce('Arg',			'Pce').
from_pce('Object',		'Pce').
from_pce('Global',		'Pce').
from_pce('Status',		'Pce').

from_pce('Funcall',		'Call').
from_pce('Variable',		'Class').
from_pce('Receiver',		'Class').
from_pce('Cell',		'Chain').
from_pce('Pointer',		'Pointer').

canonise(Headers) :-
	forall(from_pce(F, T), ignore(send(Headers, replace, F, T))),
	send(Headers, sort), send(Headers, unique),
	ignore(send(Headers, move_after, 'Pce')).


pce_insert_include_files(M) :->
	"Collect the used Pce classes and insert includes"::
	get(M, collect, regex('#\s *include\s +<pce/\([A-Za-z]+\).h>'), 1, CE),
	canonise(CE),

	get(M, collect, regex('\bPce\([A-Z][a-zA-Z]*\)'), 1, Ch),
	canonise(Ch),
	
	(   send(CE, equal, Ch)
	->  send(M, report, status, 'No changes necessary')
	;   send(Ch, for_all,
		 message(M, insert,
			 create(string, '#include <pce/%s.h>\n', @arg1)))
	).


pce_collect_selectors(M) :->
	"Collect selectors and generate PcN... lines"::
	get(M, collect, regex('\b\(send\|get\)("\(\w+\)"'), 2, Used),
	get(M, collect, regex('^PceArg\s +\bPcN\(\w+\)\b'), 1, Defined),
	(   send(Used, equal, Defined)
	->  send(M, report, status, 'No changes necessary')
	;   send(Used, for_all,
		 message(M, insert,
			 create(string, 'static PceArg PcN%s("%s");\n',
				@arg1, @arg1)))
	).


pce_replace_selectors(M) :->
	"Replace all ""bla"" by PcN..."::
	get(M, collect,
	    regex('^\(static\s +\)?PceArg\s +\bPcN\(\w+\)\b'), 2, Defined),
	send(Defined, for_all, message(M, pce_replace_selector, @arg1)).

pce_replace_selector(M, Name:char_array) :->
	"Replace all occurrences after the caret of ""name"" by PcNname"::
	get(M, text_buffer, TB),
	send(regex(string('"%s"', Name)), for_all, TB,
		   message(@arg1, replace, @arg2, string('PcN%s', Name)),
		   M?caret).

pce_unreplace_selectors(M) :->
	"Replace all PcNbla by ""bla"""::
	get(M, text_buffer, TB),
	send(regex('PcN\(\w+\)'), for_all, TB,
	     message(@arg1, replace, @arg2, '"\1"'),
	     M?caret).


collect(M, Re:regex, Reg:[int], Result) :<-
	"Collect specified register of regex matches"::
	get(M, text_buffer, TB),
	new(Result, chain),
	send(Re, for_all, TB,
	     message(Result, append, ?(@arg1, register_value,
				       @arg2, Reg, name))),
	send(Result, sort),
	send(Result, unique).

:- emacs_end_mode.
