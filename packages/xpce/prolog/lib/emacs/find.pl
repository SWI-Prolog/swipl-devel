/*  $Id$

    Part of SWI-Prolog

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2002 SWI, University of Amsterdam. All rights reserved.
*/

:- module(emacs_find, []).
:- use_module(library(pce)).
:- use_module(library(pce_tick_box)).
:- use_module(library(hyper)).
:- use_module(library(pce_report)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module defines a dialog window to   execute search in editors. This
module is defined for use with PceEmacs   to realise `novice user' find,
but contains no PceEmacs specific code and   can  therefore be used with
any instance of class editor.

To initiate a find, simply do

	send(editor_find_dialog(Editor), open)

See also the commented test/0 predicate  at   the  end  of this file and
fundamental_mode.pl

TBD: remember location or do smarter placement?
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_global(@editor_find_history, new(chain)).

:- pce_begin_class(editor_find_dialog, dialog, "Find in XPCE editor").

variable(search_origin,	int*, get, "Start of the search").
variable(search_point,	int*, get, "Current search location").

initialise(D, E:editor) :->
	"Create for editor"::
	send_super(D, initialise, 'Find in text'),
	send(D, append, new(TI, text_item(find))),
	send(TI, value_set, @editor_find_history),
	send(D, append, new(M, menu(options, marked, @nil))),
	send(M, multiple_selection, @on),
	send(M, show_label, @off),
	send(M, auto_value_align, @off),
	send_list(M, append,
		  [ regex, whole_word,
		    match_case, wrapped
		  ]),
	get(E, exact_case, MatchCase),
	send(M, selected, match_case, MatchCase),
	send(D, append, button(forwards)),
	send(D, append, button(backwards)),
	send(D, append, button(done)),
	send(D, append, button(cancel)),
	send(D, default_button, forwards),
	new(_, partof_hyper(E, D, search, editor)),
	send(new(report_dialog), below, D).

editor(D, E:editor) :<-
	"Get associated editor"::
	get(D, hypered, editor, E).

open(D) :->
	"Open for attached editor"::
	get(D, editor, E),
	get(E, frame, Frame),
	send(D, transient_for, Frame),
	get(E, display_position, point(DX, DY)),
	get(E?area, size, size(EW, EH)),
	CX is DX+EW//2,
	CY is DY+EH//2,
	send(D, open_centered, point(CX, CY)).
	
cancel(D) :->
	"Cancel search dialog"::
	(   get(D, search_origin, Origin),
	    Origin \== @nil
	->  get(D, editor, E),
	    send(E, caret, Origin)
	;   true
	),
	send(D, destroy).

done(D) :->
	"Accept search"::
	send(D, destroy).

append_history(_, Find:name) :->
	"Add search to history"::
	send(@editor_find_history, delete_all, Find),
	send(@editor_find_history, prepend, Find).

regex(D, Re:regex) :<-
	"Compile target regular expression from options"::
	get(D, member, find, TI),
	get(D, member, options, M),
	get(TI, selection, For),
	send(D, append_history, For),
	get(M, selected, match_case, MatchCase),
	(   get(M, selected, regex, @on)
	->  Pattern = For
	;   get(new(regex), quote, For, Pattern)
	),
	(   get(M, selected, whole_word, @on)
	->  (   send(Pattern, prefix, '\\b')
	    ->	true
	    ;	send(Pattern, prepend, '\\b')
	    ),
	    send(Pattern, ensure_suffix, '\\b')
	;   true
	),
	new(Re, regex(Pattern, MatchCase)).

use_wrapped(D, Wrapped:bool) :<-
	"Use wrapped search?"::
	get(D, member, options, M),
	get(M, selected, wrapped, Wrapped).

wrapped(D, Dir:{forwards,backwards}, Wrapped:bool) :<-
	"Are we wrapped?"::
	get(D, search_point, Point),
	get(D, search_origin, Origin),
	(   Dir == forwards
	->  (   Point >= Origin
	    ->	Wrapped = @off
	    ;	Wrapped = @on
	    )
	;   (   Point =< Origin
	    ->	Wrapped = @off
	    ;	Wrapped = @on
	    )
	).


point(D, Dir:{forwards,backwards}, Here:int) :<-
	(   get(D, search_point, Point),
	    Point \== @nil
	->  (   get(D, hypered, fragment, Frag)
	    ->	(   Dir == forwards
		->  get(Frag, end, Here)
		;   get(Frag, start, Here)
		)
	    ;	Here = Point
	    )
	;   get(D, editor, Editor),
	    get(Editor, caret, Here),
	    send(D, slot, search_origin, Here),
	    send(D, slot, search_point, Here)
	).


prepare_style(D) :->
	"Register style for found"::
	get(D, editor, E),
	(   get(E, style, found, _)
	->  true
	;   send(E, style, found, style(background := green))
	).


found(D, Start:int, End:int, Dir:{forwards,backwards}) :->
	"Indicate a search-hit"::
	send(D, prepare_style),
	(   Dir == forwards
	->  Point = End
	;   Point = Start
	),
	get(D, editor, E),
	send(D, slot, search_point, Point),
	send(E, caret, Point),
	Len is End-Start,
	(   get(D, hypered, fragment, Fragment)
	->  send(Fragment, start, Start),
	    send(Fragment, length, Len)
	;   new(Fragment, fragment(E, Start, Len, found)),
	    new(_, partof_hyper(D, Fragment, fragment, find_dialog))
	),
	(   get(D, wrapped, Dir, @on)
	->  send(D, report, status, 'Wrapped')
	;   send(D, report, status, '')
	).
		

forwards(D) :->
	"Execute search forwards"::
	get(D, regex, Regex),
	get(D, editor, Editor),
	get(D, point, forwards, Here),
	(   (   get(Regex, search, Editor, Here, Start)
	    ->  true
	    ;   get(D, use_wrapped, @on),
		get(Regex, search, Editor, 0, Start),
		Start < Here
	    )
	->  get(Regex, register_end, End),
	    send(D, found, Start, End, forwards)
	;   send(D, report, warning, 'No match')
	).

backwards(D) :->
	"Execute search backwards"::
	get(D, regex, Regex),
	get(D, editor, Editor),
	get(D, point, backwards, Here),
	(   (   get(Regex, search, Editor, Here, 0, Start)
	    ->  true
	    ;   get(D, use_wrapped, @on),
		get(Editor?text_buffer, size, Size),
		get(Regex, search, Editor, Size, 0, Start),
		Start > Here
	    )
	->  get(Regex, register_end, End),
	    send(D, found, Start, End, backwards)
	;   send(D, report, warning, 'No match')
	).


:- pce_end_class(editor_find_dialog).

/*
test :-
	send(new(V, view), open),
	send(V, style, found, style(background := green)),
	send(V, load, 'find.pl'),
	send(V, key_binding, '\\C-f', message(@prolog, find_in, V)).

find_in(V) :-
	send(editor_find_dialog(V), open).
*/
