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

:- module(emacs_tags,
	[ emacs_tag/3
	, emacs_tag_file/1
        , emacs_init_tags/1
	, emacs_complete_tag/2
	]).

:- meta_predicate emacs_complete_tag(+, :).

:- use_module(library(pce)).
:- require([ call/2
	   , concat/3
	   , forall/2
	   ]).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Make Emacs tags (produced with etags) information available to Prolog.
Predicates:

emacs_init_tags(+Directory|+TagFile)
  Reads the tags into the Prolog database.

emacs_tag(+Symbol, -File, -LineNo)
  Lookup tag in loaded tag-table
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- dynamic
	tag_string/1,
	tag_file/2,
	tag_dir/1.


:- pce_global(@emacs_tag_file_regex,
	      new(regex('\f\n\\([^,]+\\),\\sd+$'))).
:- pce_global(@emacs_tag_line_regex,
	      new(regex('.*\\Sd\\(\\sd+\\),\\sd+$'))).

emacs_tag(Name, File, LineNo) :-
	tag_string(String), !,
	new(Re, regex('')),
	get(Re, quote, Name, QName),
	(   send(Re, pattern, string('\\\\b%s\\\\b', QName))
	;   send(Re, pattern, string('\\\\b%s', QName))
	;   send(Re, pattern, string('\\\\b%s%c', QName, 1))
	),
	get(Re, search, String, Start), !,
	send(@emacs_tag_file_regex, search, String, Start, 0),
	get(@emacs_tag_file_regex, register_value, String, 1, FNS),
	tag_dir(Dir),
	new(S, string('%s', Dir)),
	send(S, ensure_suffix, /),
	send(S, append, FNS),
	get(S, value, File),
	send(@emacs_tag_line_regex, match, String, Start),
	get(@emacs_tag_line_regex, register_value, String, 1, LNS),
	get(@pce, convert, LNS, int, LineNo).
	    

emacs_complete_tag(Name, Goal) :-
	tag_string(String), !,
	(   Name == ''
	->  new(Re, regex('\\b[a-zA-Z_]\\w*'))
	;   new(Re, regex('')),
	    get(Re, quote, Name, QName),
	    send(Re, pattern, string('\\\\b%s\\\\w*', QName))
	),
	new(Here, number(0)),
	repeat,
	    (	send(Re, search, String, Here)
	    ->  get(Re, register_end, End),
		send(Here, value, End),
		get(Re, register_value, String, 0, name, Symbol),
		call(Goal, Symbol),
		fail
	    ;	!
	    ).


emacs_init_tags(TagFile) :-
	send(file(TagFile), exists), !,
	(   get(file(TagFile), time, TagDate),
	    tag_file(TagFile, LoadedTagDate),
	    send(LoadedTagDate, equal, TagDate)
	->  true
	;   forall(tag_string(Str), free(Str)),
	    retractall(tag_string(_)),
	    retractall(tag_file(_, _)),
	    retractall(tag_dir(_)),
	    get(file(TagFile), time, TagTime),
	    send(TagTime, lock_object, @on),
	    load_tags(TagFile),
	    get(file(TagFile), directory_name, Dir),
	    assert(tag_dir(Dir)),
	    assert(tag_file(TagFile, TagTime))
	).
emacs_init_tags(Dir) :-
	send(directory(Dir), exists), !,
	atom_concat(Dir, '/TAGS', TagFile),
	emacs_init_tags(TagFile).
	

load_tags(File) :-
	new(F, file(File)),
	send(F, open, read),
	get(F, read, TagString),
	send(TagString, lock_object, @on),
	asserta(tag_string(TagString)),
	send(F, close),
	send(F, free).

emacs_tag_file(File) :-
	tag_file(File, _).


		 /*******************************
		 *	COMPLETION SUPPORT	*
		 *******************************/

:- pce_begin_class(emacs_tag_item, text_item).

initialise(TI, Label:name, Default:[name], Message:[code]*) :->
	send_super(TI, initialise, Label, Default, Message),
	send(TI, style, combo_box).

completions(TI, Text:name, Symbols:chain) :<-
	"Complete symbol from current TAGS-table"::
	(   tag_string(_)
	->  new(Symbols, chain),
	    catch(emacs_complete_tag(Text, add_completion(Symbols)),
		  too_many_matches,
		  send(TI, report, error, 'Too many matches')),
	    send(Symbols, sort)
	;   send(TI, report, warning, 'No tag-table loaded'),
	    fail
	).

add_completion(Chain, _Match) :-
	get(Chain, size, 5000),
	send(Chain, sort, @default, @on),
	get(Chain, size, 5000),
	throw(too_many_matches).
add_completion(Chain, Match) :-
	send(Chain, append, Match).

:- pce_end_class.
