/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

:- module('emacs_c++_mode', []).
:- use_module(library(pce)).
:- require([ forall/2
	   , ignore/1
	   ]).

:- pce_begin_class('emacs_c++_mode', emacs_c_mode).

:- initialization
	new(_KB, emacs_key_binding('c++', c)).

:- initialization
	new(_X, syntax_table('c++', c)).

:- initialization
	new(MM, emacs_mode_menu('c++', c)),
	send(MM, append, pce, pce_insert_include_files),
	send(MM, append, pce, pce_collect_selectors),
	send(MM, append, pce, pce_replace_selectors),
	send(MM, append, pce, pce_unreplace_selectors).

from_pce('Arg',			'Pce').
from_pce('Object',		'Pce').
from_pce('Global',		'Pce').
from_pce('Status',		'Pce').

from_pce('Funcall',		'Call').
from_pce('MethodCall',		'Call').
from_pce('MethodFuncall',	'Call').
from_pce('Variable',		'Class').
from_pce('Receiver',		'Class').
from_pce('Cell',		'Chain').

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

:- pce_end_class.
