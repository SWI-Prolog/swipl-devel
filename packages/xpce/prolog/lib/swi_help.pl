/*  $Id$

    Part of SWI-Prolog
    Designed and implemented by Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1996 University of Amsterdam. All rights reserved.
*/

:- module(pui_help,
	  [ prolog_help/0,
	    prolog_help/1,
	    prolog_apropos/1,
	    prolog_help_topic/1
	  ]).

:- use_module(library(pce)).
:- pce_autoload(toc_window, library(pce_toc)).
:- use_module(library(helpidx)).

		 /*******************************
		 *	      TOPLEVEL		*
		 *******************************/

prolog_help :-
	prolog_help(help/1).

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
	get(@pui_help_window, member, pui_editor, Editor),
	give_apropos(Editor, Keywd),
	send(@pui_help_window?frame, expose).


		 /*******************************
		 *	    MAIN WINDOW		*
		 *******************************/

:- pce_global(@pui_help_window, make_pui_help_window).

make_pui_help_window(W) :-
	send(new(W, pui_manual), open).

:- pce_begin_class(pui_manual, frame,
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
	send(F, fill_dialog).

give_help(F, What:name, Clear:[bool], ScrollToStart:[bool]) :->
	get(F, member, pui_editor, Editor),
	send(Editor, give_help, What, Clear, ScrollToStart).

apropos(F, Atom:name) :->
	get(F, member, pui_editor, Editor),
	give_apropos(Editor, Atom).

explain(F, Text:name) :->
	get(F, member, pui_editor, Editor),
	send(Editor, editable, @on),
	do_explain(Editor, Text),
	send(Editor, editable, @off).

about(_F) :->
	send(@display, inform,
	     '%s\n\n%s\n%s',
	     'SWI-Prolog manual browser version 0.1',
	     'Jan Wielemaker',
	     'E-mail: jan@swi.psy.uva.nl').

fill_menu_bar(F) :->
	get(F, member, menu_bar_dialog, D),
	send(D, gap, size(0, 3)),
	send(D, pen, 0),
					% the menu-bar
	send(D, append, new(MB, menu_bar)),
	send(MB, append, new(File, popup(file))),
	send(MB, append, new(View, popup(view))),
	send(MB, append, new(Hist, popup(history, message(F, give_help, @arg1)))),
	send_list(File, append,
		  [ menu_item(about,
			      message(F, about),
			      end_group := @on),
		    menu_item(quit,
			      message(F, destroy))
		  ]),
	send_list(View, append,
		  [ menu_item(table_of_contents,
			      message(F, table_of_contents))
		  ]),
	send(Hist, update_message,
	     message(F, fill_history_popup, @receiver)).


fill_dialog(F) :->
	get(F, member, dialog, D),
					% the other items
	new(TI, text_item(on, '')),
	setof(Name, prolog_help_topic(Name), Names),
	chain_list(ValueSet, Names),
	send(TI, value_set, ValueSet),
	send(D, append, button(help,
			       and(message(F, give_help, TI?selection),
				   message(F, default_action, help)))),
	send(D, append, button(apropos,
			       and(message(F, apropos, TI?selection),
				   message(F, default_action, apropos))),
	     right),
	send(D, append, button(explain,
			       and(message(F, explain, TI?selection),
				   message(F, default_action, explain))),
	     right),
	send(D, append, TI, right),
	send(D, append, label(reporter), right),
	send(F, default_action, help).

prolog_help_topic(Name) :-
	predicate(Name, _, _, _, _).
prolog_help_topic(Name) :-
	function(Name, _, _).

default_action(F, Action:name) :->
	get(F, member, dialog, D),
	send(D?graphicals, for_all, 
	     if(message(@arg1, instance_of, button),
		message(@arg1, default_button,
			when(@arg1?name == Action, @on, @off)))).

add_history(F, What:name) :->
	get(F, history, Chain),
	send(Chain, delete_all, What),
	send(Chain, prepend, What).

fill_history_popup(F, P:popup) :->
	send(P, clear),
	send(F?history, for_all,
	     message(@prolog, append_history_menu_item, P, @arg1)).

append_history_menu_item(Popup, What) :-
	atom_chars(What, S),
	phrase(section(Section), S),
	section(Section, Title, _, _), !,
	send(Popup, append, menu_item(What, @default, Title)).
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

initialise(V) :->
	send(V, send_super, initialise),
	get(V, editor, Editor),
	send(V, setup_isearch),
	send(Editor?text_cursor, displayed, @off),
	get(Editor, image, Image),
	send(Editor, style, button,  style(bold := @on)),
	send(Editor, style, title,   style(font := bold)),
	send(Editor, style, section, style(font := boldlarge)),
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
	

clear(_, @off) :- !.
clear(V, _) :-
	send(V, clear).

give_help(V, What:name,
     Clear:[bool], ScrollToStart:[bool],
     FailOnError:fail_on_error=[bool]) :->
	"Display help message"::
	send(V, editable, @on),
	(   manual_range(What, Ranges)
	->  clear(V, Clear),
	    manual_file(ManFile),
	    new(F, file(ManFile)),
	    get(V, caret, Start),
	    send(F, open, read),
	    append_ranges(V, F, Ranges),
	    send(F, close),
	    get(V, caret, End),
	    get(V, text_buffer, TB),
	    mark_titles(TB, Start, End),
	    send(V, markup, Start, End),
	    send(V?frame, add_history, What),
	    (   ScrollToStart \== @off
	    ->  send(V, caret, Start),
		send(V, scroll_to, Start)
	    ;   true
	    )
	;   FailOnError == @on
	->  fail
	;   clear(V, Clear),
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
	Len is EL - From,
	new(_, fragment(TB, From, Len, title)),
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

regex(predicate,  '\(\w+\)/\(\sd+\)').
regex(predicate2, '\w+/\[\sd+[-,]\sd+\]').
regex(function,   'PL_\w+()').
regex(section,	  '\([Ss]ection\|[Cc]hapter\)\s +\sd+\(\.\sd+\)*').
regex(location,	  '\(/[-_a-zA-Z0-9~+=.]*\)+:\sd+').
regex(clause,	  '\sd+-th clause of \w+:\w+/\sd+').

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
	documented(Re, V),
	get(Re, register_start, Start),
	get(Re, register_size, Len),
	new(_F, fragment(V, Start, Len, button)).

documented(Re, V) :-
	regex_db(predicate, Re), !,
	get(V, text_buffer, TB),
	get(Re, register_value, TB, 1, name, Name),
	get(Re, register_value, TB, 2, int,  Arity),
	predicate(Name, Arity, _, _, _).
documented(_, _).

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
	atom_chars(What, S),
	phrase(manual_spec(Ranges), S).

manual_spec(From-To) -->
	name_and_arity(Name, Arity),
	{ predicate(Name, Arity, _Summary, From, To)
	}.
manual_spec(From-To) -->
	[C1|CT],
	"()", !,
	{ atom_chars(Name, [C1|CT]),
	  function(Name, From, To)
	}.
manual_spec(From-To) -->
 	"PL_",
	word(S), !,
	{ append("PL_", S, S1),
	  atom_chars(Name, S1),
	  function(Name, From, To)
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
	{ atom_chars(Name, S),
	  findall(From-To, predicate(Name, _, _, From, To), Ranges),
	  Ranges \== []
	}.
manual_spec(Ranges) -->
	[C1|CT],
	"/[",
	word(_),
	{ atom_chars(Name, [C1|CT]),
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
	integer(Arity), !,
	{ atom_chars(Name, [C1|CT])
	}.


word([C0|CT]) -->
	[C0],
	{ C0 > 0' }, !,
	word(CT).
word([]) -->
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


help_atom(Name/Arity, Atom) :- !,
	concat_atom([Name, /, Arity], Atom).
help_atom(S1-S0, Atom) :- !,
	help_atom(S1, A0),
	concat_atom([A0, '.', S0], Atom).
help_atom(C, C) :-
	integer(C), !.
help_atom(F, F) :-
	atom(F),
	append("PL_", _, Prefix),
	atom_chars(F, Prefix), !.
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
	       (   concat_atom([Name, /, Arity], Jump),
		   append_apropos(V, Jump, Summary)
	       )).

function_apropos(V, Pattern) :-
	setof(Name, (function(Name, _, _),
		     apropos_match(Pattern, Name)), Names),
	send(V, insert_section,
	     string('Foreign language interface functions from "%s":', Pattern)),
	forall(member(Name, Names),
	       (   concat(Name, '()', Jump),
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

do_explain(V, TextToExplain) :-
	send(V, clear),
	send(V, insert_section, string('Explanation for "%s"', TextToExplain)),
	term_to_atom(Term, TextToExplain), !,
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
do_explain(V, TextToExplain) :-
	send(V, format,
	     '"%s" is not correct Prolog syntax.', TextToExplain).
	       

		 /*******************************
		 *	    EDIT/SOURCE		*
		 *******************************/

try_to_edit(Spec) :-
	atom_chars(Spec, S),
	phrase(source(File, Line), S),
	start_emacs,
	send(@emacs, goto_source_location, source_location(File, Line)).

source(File, Line) -->			% path:line
	[F0|FT],
	":",
	integer(Line), !,
	{ atom_chars(File, [F0|FT])
	}.
source(File, Line) -->			% n-th clause of module:name/arity
	integer(NClause),
	"-th clause of ",
	[M0|MT], ":",
	name_and_arity(Name, Arity),
	{ atom_chars(Module, [M0|MT]),
	  functor(Head, Name, Arity),
	  nth_clause(Module:Head, NClause, CRef),
	  clause_property(CRef, file(File)),
	  clause_property(CRef, line_count(Line))
	}.
	
	
		 /*******************************
		 *       TABLE-OF-CONTENTS	*
		 *******************************/

:- pce_begin_class(pui_toc, toc_window,
		   "Prolog manual table-of-contents").

initialise(PT) :->
	send(PT, send_super, initialise),
	send(PT, root, toc_folder('Manual', manual)).

		 /*******************************
		 *	       OPEN		*
		 *******************************/

open_node(PT, Id:name) :->
	send(PT?frame, give_help, Id).

		 /*******************************
		 *	      EXPAND		*
		 *******************************/

expand_node(PT, Node:any) :->
	expand_node(PT, Node).

expand_node(PT, manual) :- !,
	forall(section([N], Title, _, _),
	       send(PT, son, manual, toc_folder(Title, N))).
expand_node(PT, Section) :-
	atom_chars(Section, S),
	phrase(section(List), S),
	subsection(List, I),
	forall(section(I, Title, _, _),
	       (   concat_atom(I, '.', Id),
		   send(PT, son, Section, toc_folder(Title, Id))
	       )),
	forall(predicate_in_section(List, Name/Arity),
	       (   concat_atom([Name, /, Arity], Id),
		   send(PT, son, Section, toc_file(Id, Id))
	       )),
	forall(function_in_section(List, Name),
	       send(PT, son, Section, toc_file(Name, Name))).

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
	


:- pce_end_class.
