/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/


:- module(pce_bibtex, []).
:- use_module(library(pce)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This library file defines a parser  for   BibTeX  database files.  It is
intended as an example  for  using   XPCE's  text-manipulation  and data
representation techniques and may  be  used   as  a  starting  point for
implementing a BibTeX database manager program.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

		 /*******************************
		 *	      DATABASE		*
		 *******************************/

:- pce_begin_class(bibtex_db,	hash_table, "BibTeX DataBase").

variable(strings,	sheet,	get, "Table of strings").
variable(file,		file*,	get, "File it was loaded from").

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A BibTeX database is  represented  by   a  hash_table  mapping keys onto
bibtex_entry objects.  A bibtex_entry is subclass of class sheet.

The  database  as  a  whole  also  stores    a  sheet  for  the  defined
string-constants as well as a reference to   the file it was loaded from
(if any).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

initialise(DB, File:[file]*) :->
	"Create BibTeX database from file"::
	send(DB, send_super, initialise),
	send(DB, slot, strings, new(sheet)),
	(   File \== @default
	->  send(DB, slot, file, File),
	    send(DB, load, File)
	).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The database is parsed using a text_buffer.    This  allows us to easily
exploit the regular-expression capabilities of XPCE implemented by class
regex.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

load(DB, File:file) :->
	"Load entries from file"::
	new(TB, text_buffer),
	send(TB, insert_file, 0, File),
	send(DB, parse, TB),
	free(TB).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Toplevel loop to parse a BibTeX file.  First of all, the syntax-table of
the text_buffer is  modified  to  treat   the  newline  as  white_space,
allowing the '\s ' operator in regular expressions to include newlines.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

parse(DB, TB:text_buffer) :->
	"Parse all entries from given text-buffer"::
	new(Here, number(0)),
	get(TB, size, Size),
	send(TB, syntax, new(S, syntax_table)),
	send(S, add_syntax, '\n', white_space),
	repeat,
		send(DB, parse_entry, TB, Here),
		get(Here, value, Size), !.


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Regular expression constants for the various   fields  of a BibTeX file.
Note that GNU-regular expression define `syntax categories': '\s ' means
`any white character (as defined by the  syntax table)', '\w' means `any
word character'.  '\S ' and '\W' are   negations.  See class regex for a
full description.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_global(@bibtex_head_regex,
   new(regex('@\(\w+\){'))).
:- pce_global(@bibtex_key_regex,
   new(regex('\s *\(\S +\)\s *,'))).
:- pce_global(@bibtex_att_regex,
   new(regex('\s *\(\w+\)\s *=\s *\("\(\\"\|[^"]\)*"\|\w+\|[^,]+\)\s *,?'))).
:- pce_global(@bibtex_end_attr_regex,
   new(regex('\s *}\s *'))).


parse_entry(DB, TB:text_buffer, Here:number) :->
	"Parse 1 entry"::
	send(@bibtex_head_regex, search, TB, Here), !,
	get(@bibtex_head_regex, register_value, TB, 1, name, T0),
	send(Here, value, @bibtex_head_regex?register_end),
	get(T0, downcase, Type),
	(   Type == string
	->  (	send(@bibtex_att_regex, match, TB, Here)
	    ->	get(@bibtex_att_regex, register_value, TB, 1, name, SName),
		get(@bibtex_att_regex, register_value, TB, 2, SValue),
		send(DB?strings, value, SName, SValue),
		send(Here, value, @bibtex_att_regex?register_end)
	    ;	send(TB, report, error, 'Illegal string at %d', Here),
		fail
	    )
	;   send(@bibtex_key_regex, match, TB, Here),
	    get(@bibtex_key_regex, register_value, TB, 1, name, Key),
	    send(DB, append, Key, new(E, bibtex_entry(Type, Key))),
	    send(Here, value, @bibtex_key_regex?register_end),
	    repeat,
	    (   send(E, parse_attribute, TB, Here)
	    ->	(   send(@bibtex_end_attr_regex, match, TB, Here)
		->  !,
		    send(Here, value, @bibtex_end_attr_regex?register_end)
		;   fail
		)
	    ;	!,
		send(TB, report, error, 'Cannot parse attribute at %d', Here),
		fail
	    )
	).

:- pce_end_class.


		 /*******************************
		 *	       ENTRY		*
		 *******************************/

:- pce_begin_class(bibtex_entry, sheet).

initialise(E, Type:name, Key:name) :->
	send(E, send_super, initialise),
	send(E, value, type, Type),
	send(E, value, key, Key).

	
parse_attribute(E, TB:text_buffer, Here:number) :->
	"Parse attribute-value pair"::
	send(@bibtex_att_regex, match, TB, Here),
	get(@bibtex_att_regex, register_value, TB, 1, name, AName),
	get(@bibtex_att_regex, register_value, TB, 2, AValue),
	send(E, value, AName, AValue),
	send(Here, value, @bibtex_att_regex?register_end).

:- pce_end_class.
