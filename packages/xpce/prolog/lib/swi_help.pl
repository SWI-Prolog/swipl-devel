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

:- module(pui_help,
	  [ prolog_help/0,
	    prolog_help/1,
	    prolog_apropos/1,
	    prolog_help_topic/1,
	    prolog_explain/1
	  ]).

:- use_module(library(pce)).
:- use_module(library(pce_edit)).
:- use_module(library(persistent_frame)).
:- pce_autoload(toc_window, library(pce_toc)).
:- use_module(library(helpidx)).
:- require([ start_emacs/0
	   , '$apropos_match'/2
	   , absolute_file_name/3
	   , append/3
	   , atom_length/2
	   , between/3
	   , chain_list/2
	   , concat_atom/2
	   , concat_atom/3
	   , default/3
	   , explain/2
	   , forall/2
	   , ignore/1
	   , maplist/3
	   , member/2
	   , send_list/3
	   , term_to_atom/2
	   , (volatile)/1
	   ]).

resource(manual,	image,	image('16x16/manual.xpm')).
resource(book,		image,  image('16x16/book2.xpm')).
resource(cfunction,	image,  image('16x16/funcdoc.xpm')).
resource(predicate,	image,  image('16x16/preddoc.xpm')).


		 /*******************************
		 *	      TOPLEVEL		*
		 *******************************/

prolog_help :-
	section(S, 'Getting started quickly', _, _),
	concat_atom(S, -, Id),
	term_to_atom(Spec, Id),
	prolog_help(Spec).

prolog_help(Topic) :-
	help_atom(Topic, Atom),
	(   atomic(Atom)
	->  send(@pui_help_window, give_help, Atom)
	;   get(@pui_help_window, member, pui_editor, Editor),
	    send(Editor, clear),
	    forall(member(A, Atom),
		   send(@pui_help_window, give_help, A, @off, @off)),
	    send(Editor, home)
	),
	send(@pui_help_window?frame, expose).

prolog_apropos(Keywd) :-
	send(@pui_help_window, apropos, Keywd),
	send(@pui_help_window?frame, expose).


prolog_explain(Term) :-
	term_to_atom(Term, Atom),
	send(@pui_help_window, explain, Atom),
	send(@pui_help_window?frame, expose).


		 /*******************************
		 *	    MAIN WINDOW		*
		 *******************************/

:- pce_global(@pui_help_window, make_pui_help_window).

make_pui_help_window(W) :-
	send(new(W, pui_manual), open).

:- pce_begin_class(pui_manual, persistent_frame,
		   "Frame for Prolog help").

variable(history, chain, get, "History list").

initialise(F) :->
	send(F, send_super, initialise, 'SWI-Prolog help'),
	send(F, append, new(MBD, dialog)),
	send(MBD, name, menu_bar_dialog),
	send(new(E, pui_editor), below, MBD),
	send(new(dialog), below, E),
	send(F, slot, history, new(chain)),
	send(F, fill_menu_bar),
	send(F, fill_dialog),
	send(F, table_of_contents).

give_help(F, What:name, Clear:[bool], ScrollToStart:[bool]) :->
	get(F, display, Display),
	send(Display, busy_cursor),
	get(F, member, pui_editor, Editor),
	ignore(send(Editor, give_help, What, Clear, ScrollToStart)),
	send(Display, busy_cursor, @nil).

apropos(F, Atom:name) :->
	"Search summary descriptions"::
	send(F, add_history, string('apropos:%s', Atom)),
	get(F, member, pui_editor, Editor),
	give_apropos(Editor, Atom).

explain(F, Text:name) :->
	"Explain what we know about of term"::
	send(F, add_history, string('explain:%s', Text)),
	get(F, member, pui_editor, Editor),
	send(Editor, editable, @on),
	do_explain_text(Editor, Text),
	send(Editor, editable, @off).

go(F, What:name) :->
	"Handle Go menu (history)"::
	(   sub_atom(What, 0, _, A, 'apropos:')
	->  sub_atom(What, _, A, 0, For),
	    send(F, apropos, For)
	;   sub_atom(What, 0, _, A, 'explain:')
	->  sub_atom(What, _, A, 0, For),
	    send(F, explain, For)
	;   send(F, give_help, What)
	).

about(_F) :->
	send(@display, inform,
	     '%s\n\n%s\n%s',
	     'SWI-Prolog manual browser version 3.0',
	     'Jan Wielemaker',
	     'E-mail: jan@swi-prolog.org').

fill_menu_bar(F) :->
	get(F, member, menu_bar_dialog, D),
	send(D, gap, size(0, 0)),
	send(D, pen, 0),
					% the menu-bar
	send(D, append, new(MB, menu_bar)),
	send(MB, append, new(File, popup(file))),
	send(MB, append, new(Settings, popup(settings))),
	send(MB, append, new(Hist, popup(go, message(F, go, @arg1)))),
	send(MB, append, new(Help, popup(help))),

	send_list(File, append,
		  [ menu_item(exit,
			      message(F, destroy))
		  ]),

	send_list(Settings, append,
		  [ menu_item('User init file ...',
			      message(F, edit_preferences, prolog))
		  ]),

	send(Hist, update_message,
	     message(F, fill_history_popup, @receiver)),

	send_list(Help, append,
		  [ menu_item(about,
			      message(F, about),
			      end_group := @on),

		    menu_item('Package documentation ...',
			      message(F, open_package_doc)),
		    menu_item('SWI-Prolog WWW home (on www) ...',
			      message(F, open_url, pl)),
		    menu_item('SWI-Prolog FAQ (on www) ...',
			      message(F, open_url, pl_faq)),
		    menu_item('SWI-Prolog Quick Start (on www) ...',
			      message(F, open_url, pl_quick)),
		    menu_item('SWI-Prolog Manual (on www) ...',
			      message(F, open_url, pl_man)),
		    menu_item('SWI-Prolog Mailing list (on www) ...',
			      message(F, open_url, pl_mail)),
		    menu_item('SWI-Prolog Download (on www) ...',
			      message(F, open_url, pl_download),
			      end_group := @on),
		    
		    menu_item('XPCE (GUI) Manual ...',
			      message(@prolog, manpce)),
		    menu_item('XPCE User Guide (on www) ...',
			      message(F, open_url, xpce_man),
			      end_group := @on),

		    menu_item('SWI-Prolog bug report (on www) ...',
			      message(F, open_url, pl_bugs)),
		    gap,
		    menu_item('Donate to SWI-Prolog (on www) ...',
			      message(F, open_url, pl_donate))

		  ]).


open_url(F, Id) :->
	"Open WWW browser on url with given id"::
	send(F, report, progress, 'Starting browser ...'),
	Spec =.. [Id, '.'],
	www_open_url(Spec),
	send(F, report, done).


open_package_doc(_F) :->
	(   absolute_file_name(swi('doc/packages/index.html'),
			       [ access(read),
				 file_errors(fail)
			       ],
			       Page)
	->  atom_concat('file://', Page, URL),
	    www_open_url(URL)
	;   send(@display, inform,
		 'Package documentation is not installed.  You may\n \
		  wish to view it on http://www.swi-prolog.org')
	).


edit_preferences(_F, Which:name) :->
	"Edit preferences"::
	prolog_edit_preferences(Which).


fill_dialog(F) :->
	get(F, member, dialog, D),
					% the other items
	new(TI, text_item(on, '')),
	setof(Name, prolog_help_topic(Name), Names),
	chain_list(ValueSet, Names),
	send(TI, value_set, ValueSet),
	send(D, append, button(help,
			       message(F, give_help, TI?selection))),
	send(D, append, button(search,
			       message(F, apropos, TI?selection)),
	     right),
	send(D, append, button(explain,
			       message(F, explain, TI?selection)),
	     right),
	send(D, append, TI, right),
	send(D, append, label(reporter), right),
	send(D, default_button, help).

prolog_help_topic(Name) :-
	predicate(Name, _, _, _, _).
prolog_help_topic(Name) :-
	function(Name, _, _).


add_history(F, What:name) :->
	get(F, history, Chain),
	send(Chain, delete_all, What),
	send(Chain, prepend, What).

fill_history_popup(F, P:popup) :->
	send(P, clear),
	send(F?history, for_all,
	     message(@prolog, append_history_menu_item, P, @arg1)).

append_history_menu_item(Popup, What) :-
	atom_codes(What, S),
	phrase(section(Section), S),
	section(Section, Title, _, _), !,
	send(Popup, append, menu_item(What, @default, Title)).
append_history_menu_item(Popup, What) :-
	sub_atom(What, 0, _, A, 'apropos:'), !,
	sub_atom(What, _, A, 0, For),
	send(Popup, append, menu_item(What, @default,
				      string('Search %s', For))).
append_history_menu_item(Popup, What) :-
	sub_atom(What, 0, _, A, 'explain:'), !,
	sub_atom(What, _, A, 0, For),
	send(Popup, append, menu_item(What, @default,
				      string('Explain %s', For))).
append_history_menu_item(Popup, What) :-
	send(Popup, append, menu_item(What, @default, What)).

table_of_contents(F) :->
	(   get(F, member, pui_toc, _)
	->  true
	;   get(F, member, pui_editor, Editor),
	    new(PT, pui_toc),
	    send(PT, left, Editor?tile),
	    send(PT, expand_node, manual)
	).

:- pce_end_class.


		 /*******************************
		 *	  THE HELP EDITOR	*
		 *******************************/

:- pce_begin_class(pui_editor, view,
		   "Editor for PUI help text").

variable(displayed_ranges, prolog, get, "Currently displayed ranges").

initialise(V) :->
	send_super(V, initialise),
	get(V, editor, Editor),
	send(V, setup_isearch),
	send(Editor?text_cursor, displayed, @off),
	get(Editor, image, Image),
	send(Editor, style, button,    style(bold := @on,
					     colour := forestgreen)),
	send(Editor, style, title,     style(font := bold)),
	send(Editor, style, section,   style(font := boldlarge)),
	send(Editor, style, bold,      style(bold := @on)),
	send(Editor, style, underline, style(underline := @on)),
	send(Image, wrap, none),
	send(Image, recogniser,
	     click_gesture(left, '', single,
			   and(message(V?display, busy_cursor),
			       if(message(V, jump, ?(Image, index, @event))),
			       message(V?display, busy_cursor, @nil)))).

setup_isearch(V) :->
	get(V, editor, Editor),
	send(Editor, send_method,
	     send_method(insert_self, vector('[int]', '[char]'),
			 if(@receiver?editable == @on,
			    message(@receiver, send_class, insert_self,
				    @arg1, @arg2),
			    and(message(@receiver, isearch_forward),
				message(@receiver, '_start_isearch',@arg2))))),
	send(V?editor, send_method,
	     send_method('_isearch', vector(event_id),
			 if(@arg1 == 13,
			    and(message(@receiver, caret,
					@receiver?selection_end),
				message(@receiver, '_changed_region',
					@receiver?selection_start,
					@receiver?selection_end),
				message(@receiver, focus_function, @nil)),
			    message(@receiver, send_class, '_isearch',
				    @arg1)))).
	

displayed_ranges(V, Displayed:prolog) :->
	send(V, slot, displayed_ranges, Displayed),
	(   get(V?frame, member, pui_toc, TocTree)
	->  send(TocTree, select_range, Displayed)
	;   true
	).

clear(V) :->
	send_super(V, clear),
	send(V, displayed_ranges, []).

give_help(V, What:name,
     Clear:[bool], ScrollToStart:[bool],
     FailOnError:fail_on_error=[bool]) :->
	"Display help message"::
	send(V, editable, @on),
	(   manual_range(What, Ranges)
	->  (   Clear == @off
	    ->	true
	    ;	send(V, clear)
	    ),
	    manual_file(ManFile),
	    new(F, file(ManFile)),
	    get(V, caret, Start),
	    send(F, open, read),
	    append_ranges(V, F, Ranges),
	    send(F, close),
	    get(V, caret, End),
	    send(V, markup, Start, End),
	    get(V, text_buffer, TB),
	    mark_titles(TB, Start, End),
	    send(V?frame, add_history, What),
	    (   ScrollToStart \== @off
	    ->  send(V, caret, Start),
		send(V, scroll_to, Start)
	    ;   true
	    ),
	    send(V, displayed_ranges, Ranges)
	;   FailOnError == @on
	->  fail
	;   (   Clear == @off
	    ->	true
	    ;	send(V, clear)
	    ),
	    send(V, format, 'Could not find help for "%s".\n', What),
	    send(V, caret, 0)
	),
	send(V, editable, @off).

append_ranges(V, F, [H|T]) :- !,
	append_ranges(V, F, H),
	append_ranges(V, F, T).
append_ranges(_, _, []) :- !.
append_ranges(V, F, From-To) :-
	send(F, seek, From),
	get(F, read, To-From, Text),
	send(V, insert, Text),
	send(V, insert, @pui_ff),
	send(V, newline).

home(V, Where:[int]) :->
	default(Where, 0, Caret),
	send(V, scroll_to, Caret),
	send(V, caret, Caret).


		 /*******************************
		 *	       TITLES		*
		 *******************************/

:- pce_global(@pui_ff, new(string('\f'))).

mark_titles(_, From, To) :-
	From >= To, !.
mark_titles(TB, From, To) :-
	get(TB, character, From, 12), !,
	send(TB, delete, From, 1),
	(   between(1, 3, L),
	    get(TB, scan, From, line, L, start, I2),
	    get(TB, character, I2, C2),
	    C2 > 32
	->  mark_titles(TB, I2, To)
	;   true
	).
mark_titles(TB, From, To) :-
	get(TB, scan, From, line, 0, end, EL),
%	Len is EL - From,
%	new(_, fragment(TB, From, Len, title)),
	(   get(TB, find, EL, @pui_ff, 1, start, Idx)
	->  mark_titles(TB, Idx, To)
	;   true
	).
	
		 /*******************************
		 *     CROSS-REFERENCE LINKS	*
		 *******************************/

:- volatile
	regex_db/2.
:- dynamic
	regex_db/2.

:- pce_global(@ul_regex, new(regex('.\b\\(.\\)'))).

regex(bold,	  '\\(\\(.\\)\b\\2\\)+').
regex(underline,  '\\(\\(.\\)\b_\\)+').
regex(underline2,  '\\(_\b\\(.\\)\\)+').
regex(predicate,  '\\(\\w+\\)/\\(\\sd+\\)').
regex(predicate2, '\\w+/\\[\\sd+[-,]\\sd+\\]').
regex(function,   'PL_\\w+()').
regex(section,	  '\\([Ss]ection\\|[Cc]hapter\\)\\s +\\sd+\\(\\.\\sd+\\)*').
regex(location,	  '\\([a-zA-Z]:\\)?\\(/[-_a-zA-Z0-9~+=.]*\\)+:\\sd+').
regex(clause,	  '\\sd+-th clause of \\w+:.*/\\sd+').
regex(method,     '\\w+\\(->\\|<-\\)\\w+').

regex_object(Id, Re) :-
	regex_db(Id, Re), !.
regex_object(Id, Re) :-
	regex(Id, Pattern),
	new(Re, regex(Pattern)),
	send(Re, compile, @on),
	send(Re, lock_object, @on),
	assert(regex_db(Id, Re)).

markup(V, From:int, To:int) :->
	get(V, text_buffer, TB),
	(   regex(Id, _),
	    regex_object(Id, Re),
	    send(Re, for_all, TB,
		 message(V, mark_fragment, @arg1),
		 From, To),
	    fail
	;   true
	).

mark_fragment(V, Re:regex) :->
        (   documented(Re, V, Type)
	->  get(Re, register_start, Start),
	    get(Re, register_size, Len),
	    new(_F, fragment(V, Start, Len, Type))
	;   true
	).

font_style(bold, bold).
font_style(underline, underline).
font_style(underline2, underline).

documented(Re, V, button) :-
	regex_db(predicate, Re), !,
	get(V, text_buffer, TB),
	get(Re, register_value, TB, 1, name, Name),
	get(Re, register_value, TB, 2, int,  Arity),
	predicate(Name, Arity, _, _, _).
documented(Re, V, Style) :-
	regex_db(Style0, Re),
        font_style(Style0, Style), !,
        get(V, text_buffer, TB),
        get(Re, register_value, TB, 0, String),
	send(@ul_regex, for_all, String,
	     message(@ul_regex, replace, String, '\\1')),
	send(Re, replace, TB, String).
documented(_, _, button).


jump(V, Caret:[int]) :->
	"Jump to current fragment"::
	(   Caret == @default
	->  get(V, caret, C)
	;   C = Caret
	),
	get(V, find_fragment, message(@arg1, overlap, C), Fragment),
	get(Fragment, string, JumpTo),
	(   send(V, give_help, JumpTo, fail_on_error := @on)
	->  true
	;   get(JumpTo, value, Atom),
	    try_to_edit(Atom)
	).

insert_section(V, Text) :->
	get(V, caret, Caret),
	(   Caret == 0
	->  Begin = Caret
	;   send(V, newline, 2),
	    get(V, caret, Begin)
	),
	send(V, insert, Text),
	get(V, caret, NewCaret),
	new(_, fragment(V, Begin, NewCaret-Begin, section)),
	send(V, newline, 2).

drop(V, Id:name) :->
	send(V, give_help, Id, fail_on_error := @on).

:- pce_end_class.


		 /*******************************
		 *       MANUAL UTILITIES	*
		 *******************************/

%	manual_file(-File)
%	Find the database file of the manual.  If the manual cannot be
%	found, display an error message.

manual_file(File) :-
	absolute_file_name(library('MANUAL'),
			   [access(read)],
			   File), !.
manual_file(_File) :-
	send(@nil, report, error, 'Can''t find manual database MANUAL'),
	fail.

		 /*******************************
		 *     ATOMIC SPECIFICATIONS	*
		 *******************************/

%	manual_range(+What, -From, -To)
%	Find the character range for the given help topic, which is of
%	the form
%			name/arity
%			function()
%			n.m..

manual_range(What, Ranges) :-
	atom_codes(What, S),
	phrase(manual_spec(Ranges), S).

manual_spec(From-To) -->
	name_and_arity(Name, SArity),
	{ predicate(Name, Arity, _Summary, From, To),
	  (   var(SArity)
	  ->  var(Arity)
	  ;   SArity = Arity
	  )
	}.
manual_spec(From-To) -->
	pl_function(Name), !,
	{ function(Name, From, To)
	}.
manual_spec(From-To) -->
	(   "section",
	    blanks
	->  {true}
	;   {true}
	),
	section(SN),
	{ section(SN, _Summary, From, To)
	}.
manual_spec(Ranges) -->
	word(S),
	{ atom_codes(Name, S),
	  findall(From-To, predicate(Name, _, _, From, To), Ranges),
	  Ranges \== []
	}.
manual_spec(Ranges) -->
	[C1|CT],
	"/[",
	word(_),
	{ atom_codes(Name, [C1|CT]),
	  findall(From-To, predicate(Name, _, _, From, To), Ranges),
	  Ranges \== []
	}.

section([S0|ST]) -->
	integer(S0),
	subsections(ST).

subsections([S0|ST]) -->
	section_separator,
	integer(S0), !,
	subsections(ST).
subsections([]) -->
	[].

section_separator -->
	".", !.
section_separator -->
	"-", !.

name_and_arity(Name, Arity) -->
	[C1|CT],
	"/",
	(   integer(Arity)
	->  []
	;   "_"
	), !,
	{ atom_codes(Name, [C1|CT])
	}.

pl_function(Name) -->
	identifier(S),
	"()", !,
	{ atom_codes(Name, S)
	}.
pl_function(Name) -->
	"PL_",
	identifier(S), !,
	{ append("PL_", S, Chars),
	  atom_codes(Name, Chars)
	}.


word([C0|CT]) -->
	[C0],
	{ C0 > 0' }, !,
	word(CT).
word([]) -->
	[].

identifier([C0|CT]) -->
	alnum(C0), !,
	identifier(CT).
identifier([]) -->
	[].

blanks -->
	blank, !,
	blanks.
blanks -->
	[].

blank -->
	[C],
	{ C =< 32 }.

integer(N) -->
	digit(D0),
	digits(DT),
	{ number_chars(N, [D0|DT])
	}.

digits([D0|DT]) -->
	digit(D0), !,
	digits(DT).
digits([]) -->
	[].

digit(D) -->
	[D],
	{ between(0'0, 0'9, D)
	}.

alnum(C) -->
	[C],
	{ between(0'a, 0'z, C)
	; between(0'A, 0'Z, C)
	; between(0'0, 0'9, C)
	; C = 0'_
	}.


help_atom(Name/Arity, Atom) :- !,
	(   var(Arity)
	->  atom_concat(Name, '/_', Atom)
	;   concat_atom([Name, /, Arity], Atom)
	).
help_atom(S1-S0, Atom) :- !,
	help_atom(S1, A0),
	concat_atom([A0, '.', S0], Atom).
help_atom(C, C) :-
	integer(C), !.
help_atom(F, F) :-
	atom(F),
	sub_atom(F, 0, _, _, 'PL_'), !.
help_atom(Name, List) :-
	findall(Name/Arity, predicate(Name, Arity, _, _, _), Preds),
	Preds \== [],
	maplist(help_atom, Preds, List).

		 /*******************************
		 *	     APROSPOS		*
		 *******************************/

give_apropos(V, Atom) :-
	send(V, clear),
	send(V, editable, @on),
	ignore(predicate_apropos(V, Atom)),
	ignore(function_apropos(V, Atom)),
	ignore(section_apropos(V, Atom)),
	send(V, editable, @off),
	send(V, home).

apropos_predicate(Pattern, Name, Arity, Summary) :-
	predicate(Name, Arity, Summary, _, _),
	(   apropos_match(Pattern, Name)
	->  true
	;   apropos_match(Pattern, Summary)
	).

predicate_apropos(V, Pattern) :-
	setof(triple(Name, Arity, Summary),
	      apropos_predicate(Pattern, Name, Arity, Summary),
	      Names),
	send(V, insert_section,
	     string('Predicates from "%s":', Pattern)),
	forall(member(triple(Name, Arity, Summary), Names),
	       (   help_atom(Name/Arity, Jump),
		   append_apropos(V, Jump, Summary)
	       )).

function_apropos(V, Pattern) :-
	findall(Name, (function(Name, _, _),
		       apropos_match(Pattern, Name)), Names),
	Names \= [],
	send(V, insert_section,
	     string('Foreign language interface functions from "%s":', Pattern)),
	forall(member(Name, Names),
	       (   atom_concat(Name, '()', Jump),
		   append_apropos(V, Jump, 'Interface Function')
	       )).


section_apropos(V, Pattern) :-
	findall(Index-Name, (section(Index, Name, _, _),
			     apropos_match(Pattern, Name)), Names),
	Names \== [],
	send(V, insert_section,
	     string('Sections from "%s":', Pattern)),
	forall(member(Index-Title, Names),
	       (   concat_atom(Index, '.', Jump),
		   append_apropos(V, Jump, Title)
	       )).

append_apropos(V, Jump, Summary) :-
	get(V, caret, Caret),
	send(V, format, '%-30s%s\n', Jump, Summary),
	atom_length(Jump, Len),
	new(_, fragment(V, Caret, Len, button)).

apropos_match(A, B) :-
	'$apropos_match'(A, B).			% C defined for performance

		 /*******************************
		 *	       EXPLAIN		*
		 *******************************/

do_explain_text(V, Text) :-
	catch(term_to_atom(Term, Text), _, fail), !,
	do_explain(V, Term).
do_explain_text(V, Text) :-
	send(V, clear),
	send(V, insert_section, string('Explanation for "%s"', Text)),
	send(V, format,
	     '"%s" is not correct Prolog syntax.', Text).

do_explain(V, Term) :-
	send(V, clear),
	term_to_atom(Term, Atom),
	send(V, insert_section, string('Explanation for "%s"', Atom)),
	findall(Explanation, explain(Term, Explanation), Explanations),
	get(V, caret, Start),
	forall(member(E, Explanations),
	       (   send(V, insert, string(E)),
		   send(V, newline)
	       )),
	get(V, caret, End),
	send(V, markup, Start, End),
	send(V, caret, 0),
	send(V, scroll_to, 0).


		 /*******************************
		 *	    EDIT/SOURCE		*
		 *******************************/

try_to_edit(Spec) :-
	atom_codes(Spec, S),
	phrase(source(File, Line), S), !,
	start_emacs,
	send(@emacs, goto_source_location, source_location(File, Line)).

source(File, Line) -->			% path:line
	[F0|FT],
	":",
	integer(Line), !,
	{ atom_codes(File, [F0|FT])
	}.
source(File, Line) -->			% n-th clause of module:name/arity
	integer(NClause),
	"-th clause of ",
	[M0|MT], ":",
	[N0|NT], "/",
	integer(Arity),
	{ atom_codes(Name, [N0|NT]),
	  atom_codes(Module, [M0|MT]),
	  functor(Head, Name, Arity),
	  nth_clause(Module:Head, NClause, CRef),
	  clause_property(CRef, file(File)),
	  clause_property(CRef, line_count(Line))
	}.
source(File, Line) -->			% XPCE method
	blanks,
	identifier(ClassChars),
	blanks,
        (   "->"
	->  { Method = '->'(Class, Selector)
	    }
	;   "<-",
	    { Method = '<-'(Class, Selector)
	    }
	),
	identifier(SelectorChars),
	{ atom_codes(Class, ClassChars),
	  atom_codes(Selector, SelectorChars),
	  pce_edit:method(Method, Obj),
	  get(Obj, source, Location),
	  Location \== @nil,
	  get(Location, file_name, File),
	  get(Location, line_no, Line)
	}.
	
	
		 /*******************************
		 *       TABLE-OF-CONTENTS	*
		 *******************************/

:- pce_begin_class(pui_toc, toc_window,
		   "Prolog manual table-of-contents").

initialise(PT) :->
	send(PT, send_super, initialise),
	send(PT, root,
	     toc_folder('Manual', manual, resource(manual), resource(book))).


		 /*******************************
		 *	       OPEN		*
		 *******************************/

select_node(PT, Id:name) :->
	"Show selected section"::
	send(PT?frame, give_help, Id).


		 /*******************************
		 *	      EXPAND		*
		 *******************************/

expand_node(PT, Node:any) :->
	expand_node(PT, Node),
	send(PT, send_super, expand_node, Node).

expand_node(PT, manual) :- !,
	forall(section([N], _, _, _),
	       send(PT, son, manual, pui_section_node([N]))).
expand_node(PT, Section) :-
	atom_codes(Section, S),
	phrase(section(List), S),
	subsection(List, I),
	forall(section(I, _, _, _),
	       send(PT, son, Section, pui_section_node(I))),
	forall(predicate_in_section(List, Name/Arity),
	       send(PT, son, Section, pui_predicate_node(Name, Arity))),
	forall(function_in_section(List, Name),
	       send(PT, son, Section, pui_function_node(Name))).

can_expand(Section) :-
	subsection(Section, Sub),
	section(Sub, _, _, _), !.
can_expand(Section) :-
	plain_predicate_in_section(Section, _), !.
can_expand(Section) :-
	plain_function_in_section(Section, _), !.


plain_predicate_in_section(Section, Name/Arity) :-
	section(Section, _, SFrom, STo),
	predicate(Name, Arity, _, PFrom, PTo),
	PFrom > SFrom,
	PTo < STo.

predicate_in_section(Section, Pred) :-
	plain_predicate_in_section(Section, Pred),
	\+ (subsection(Section, Sub),
	    plain_predicate_in_section(Sub, Pred)).

plain_function_in_section(Section, Name) :-
	section(Section, _, SFrom, STo),
	function(Name, PFrom, PTo),
	PFrom > SFrom,
	PTo < STo.

function_in_section(Section, Func) :-
	plain_function_in_section(Section, Func),
	\+ (subsection(Section, Sub),
	    plain_function_in_section(Sub, Func)).

subsection(Sec, Sub) :-
	append(Sec, [_], Sub).
	
select_range(PT, Ranges:prolog) :->
	"Select selections in range"::
	(   Ranges == []
	->  send(PT, selection, @nil)
	;   get(PT, node, manual, Root),
	    send(Root, collapsed, @off),
	    send(Root?sons, for_all,
		 message(@arg1, select_range, prolog(Ranges))),
	    get(PT, selection, Nodes),
	    send(PT, normalise, Nodes, y)
	).
	    
in_range(F-T, Node, How) :-
	get(Node, start, S),
	get(Node, end, E),
	\+  (   F>=E
	    ;   T=<S
	    ),
	(   S>=F,E=<T
	->  How = all
	;   How = partial
	).
in_range([H|_], Node, How) :-
	in_range(H, Node, How).
in_range([_|T], Node, How) :-
	in_range(T, Node, How).


:- pce_end_class.

:- pce_begin_class(pui_section_node, toc_folder,
		   "Represent section of the manual").

variable(start,	int, get, "Start index").
variable(end,	int, get, "End index").

initialise(N, Index:prolog) :->
	section(Index, Title, Start, End),
	(   can_expand(Index)
	->  CanExpand = @on
	;   CanExpand = @off
	),
	concat_atom(Index, '.', Id),
	send_super(N, initialise, Title?capitalise, Id,
		   resource(manual), resource(book),
		   CanExpand),
	(   font(Index, Font)
	->  send(N, font, Font)
	;   true
	),
	send(N, slot, start, Start),
	send(N, slot, end, End).

font([_], bold).

select_range(N, Ranges:prolog) :->
	"Select nodes in given ranges"::
	(   in_range(Ranges, N, all)
	->  send(N, selected, @on)
	;   in_range(Ranges, N, partial)
	->  send(N, collapsed, @off),
	    send(N?sons, for_all,
		 message(@arg1, select_range, prolog(Ranges)))
	;   true
	).

:- pce_end_class(pui_section_node). 


:- pce_begin_class(pui_predicate_node, toc_file,
		   "Represent a single predicate").

variable(start,	int, get, "Start index").
variable(end,	int, get, "End index").

initialise(N, Name:name, Arity:prolog) :->
	predicate(Name, SArity, _Summary, Start, End),
	SArity =@= Arity,
	help_atom(Name/Arity, Id),
	send_super(N, initialise, Id, Id, resource(predicate)),
	send(N, slot, start, Start),
	send(N, slot, end, End).

select_range(N, Ranges:prolog) :->
	"Select nodes in given ranges"::
	(   in_range(Ranges, N, _)
	->  send(N, selected, @on)
	;   true
	).

:- pce_end_class.


:- pce_begin_class(pui_function_node, toc_file,
		   "Represent a single C-function").

variable(start,	int, get, "Start index").
variable(end,	int, get, "End index").

initialise(N, Name:name) :->
	function(Name, Start, End),
	send_super(N, initialise, Name, Name, resource(cfunction)),
	send(N, slot, start, Start),
	send(N, slot, end, End).

select_range(N, Ranges:prolog) :->
	"Select nodes in given ranges"::
	(   in_range(Ranges, N, _)
	->  send(N, selected, @on)
	;   true
	).

:- pce_end_class.


