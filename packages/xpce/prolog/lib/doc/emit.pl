/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 2000 University of Amsterdam. All rights reserved.
*/

:- module(doc_emit,
	  [ emit/3
	  ]).
:- use_module(library(pce)).

:- use_module(doc(layout)).
:- use_module(doc(table)).
:- use_module(doc(util)).
:- use_module(library(help_message)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This is the central  module  for   the  XPCE/Prolog  document  rendering
library. It supports the translation of   arbitrary  Prolog terms into a
constellation of XPCE document rendering objects.

It does this in two steps. The  first step translates the arbitrary (but
often the output from xml2pl) into  a   list  of `action' represented as
\Term, where \ acts as prefix operator (getting LaTeX like-syntax).

The translator is extended by defining clauses for the multi-file predicate

	doc:emit(+Term, +Parbox, +Mode).

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- multifile
	doc:emit/3,
	doc:action/3.


		 /*******************************
		 *	     EMITTING		*
		 *******************************/

%	emit(+Content, +Parbox, +Mode)
%
%	Apply (show in, render) Content in Parbox using the defined
%	Mode (an instance of class doc_mode).

emit([], _, _) :- !.
emit(Term, PB, Mode) :-
	doc:emit(Term, PB, Mode), !.
emit([Text|T], PB, Mode) :-
	atomic(Text), !,
	get(Mode, style, Style),
	(   get(Mode, space_mode, preserve)
	->  append_pre_atom(Text, PB, Mode)
	;   get(Mode, space, Space),
	    get(Mode, ignore_blanks, BlankMode),
	    send(PB, cdata, Text, Style, Space, BlankMode),
	    blank_mode(BlankMode, NewBlankMode),
	    send(Mode, ignore_blanks, NewBlankMode)
	),
	emit(T, PB, Mode).
emit([\H|T], PB, Mode) :-
	action(H, PB, Mode), !,
	emit(T, PB, Mode).
emit([@Ref|T], PB, Mode) :- !,
	send(PB, append, @Ref),
	emit(T, PB, Mode).
emit([H|T], PB, Mode) :-
	print_message(warning, doc(failed(emit(H)))),
	emit(T, PB, Mode).
	
blank_mode(leading,  none) :- !.
blank_mode(both,     trailing) :- !.
blank_mode(Mode,     Mode).


:- discontiguous
	action/3.			% multifile?

%	action(+Command, +ParBox, +Mode)
%
%	Execute a basic action on the ParBox.  This realises the final
%	stage of the rendering process.

%	Blank handling

action(ignorespaces, _, Mode) :-
	send(Mode, ignore_blanks, leading).
action(space(SpaceMode), _, Mode) :-
	send(Mode, space_mode, SpaceMode).
action(pre(Text), PB, Mode) :-
	append_pre(Text, PB, Mode).

append_pre([], _, _).
append_pre([H|T], PB, Mode) :-
	append_pre_atom(H, PB, Mode),
	append_pre(T, PB, Mode).

append_pre_atom(H, PB, Mode) :-
	get(Mode, style, Style),
	(   sub_atom(H, Before, Len, _, '\n')
	->  AfterChars is Before+Len,
	    sub_atom(H, 0, Before, _, A),
	    sub_atom(H, AfterChars, _, 0, H1),
	    send(PB, append, tbox(A, Style)),
	    send(PB, append, @br),
	    append_pre_atom(H1, PB, Mode)
	;   send(PB, append, tbox(H, Style))
	).
	

%	Paragraphs

action(Action, PB, Mode) :-
	doc:action(Action, PB, Mode), !.
action(par, PB, Mode) :-		% \par
	action(parskip, PB, Mode),
	action(parindent, PB, Mode).
action(parskip, PB, Mode) :-		% \parskip
	send(PB, instance_of, parbox), 
	get(Mode, parsep, ParSep),
	send(PB, append, @br),
	send(PB, append, ParSep),
	send(PB, append, @br).
action(parindent, PB, Mode) :-		% \parindent
	send(PB, instance_of, parbox), 
	get(Mode, parindent, ParIndent),
	send(PB, append, ParIndent).

%	Local environments (grouping)

action(group(Group), PB, Mode) :-
	get(Mode, clone, Clone),
	emit(Group, PB, Clone).

%	Font and appearance manipulation

action(setfont(Attribute, Value), _, Mode) :-
	send(Mode, set_font, Attribute, Value).
action(ul, _, Mode) :-
	send(Mode, underline, @on).
action(colour(Colour), _, Mode) :-
	send(Mode, colour, Colour).

%	List building commands

action(list(Options, Content), PB, Mode) :-
	get(Mode, clone, Clone),
	option(class(Class), Options, bullet_list),
	send(PB, append, @br),
	get(PB, line_width, LW),
	NewTerm =.. [Class, LW],
	new(GrBox, grbox(new(LB, NewTerm), top, @hfill_rubber)),
	send(PB, append, GrBox),
	send(PB, append, @br),
	emit(Content, LB, Clone).
action(li(ItemContent), LB, Mode) :-
	(   (   send(LB, instance_of, bullet_list)
	    ;   send(LB, instance_of, enum_list)
	    )
	->  get(LB, make_item, [], Mode, PB)
	;   print_message(warning, doc(expected_context(li, list))),
	    PB=LB
	),
	emit(ItemContent, PB, Mode).
action(dt(ItemTitle), LB, Mode) :-
	(   send(LB, instance_of, definition_list)
	->  get(LB, make_item, ItemTitle, Mode, PB),
	    send(LB, attribute, open_item, PB)
	;   print_message(warning, doc(expected_context(dt, definition_list)))
	).
action(dd(ItemContent), LB, Mode) :-
	(   send(LB, instance_of, definition_list),
	    (   get(LB, attribute, open_item, PB),
		PB \== @nil
	    ->  send(LB, attribute, open_item, @nil)
	    ;   get(LB, make_item, [], Mode, PB)
	    )
	->  true
	;   print_message(warning, doc(expected_context(dd, definition_list))),
	    PB = LB
	),
	emit(ItemContent, PB, Mode).

%	Misc browser control

action(title(Title), PB, _) :-		% browser control
	get(PB, frame, Frame),
	send(Frame, label, Title).
action(body(Attributes), PB, Mode) :-
	apply_options(Attributes, body_option, PB+Mode).

body_option(bgcolour(Colour), PB+_) :-
	get(PB, window, Window),
	send(Window, has_send_method, background), !,
	(   get(@pce, convert, Colour, colour, Obj),
	    send(Window, background, Obj)
	;   true
	).
body_option(background(URL), PB+Mode) :-
	get(PB, window, Window),
	send(Window, has_send_method, background), !,
	get(Mode, base_url, BaseUrl),
	get_url_to_file(URL, BaseUrl, File),
	new(Img, image),
	(   send(Img, load,  File)
	->  send(Window, background, Img)
	;   true
	).
body_option(text(Colour), _+Mode) :-
	send(Mode, colour, Colour).
body_option(_, _).


%	Buttons and anchors

action(button(Message, Content, Balloon), PB, Mode) :-
	new(BO, button_box(@nil)),
	new(BC, button_box(Message, Balloon)),
	new(_, hyper(BO, BC, close, open)),
	send(PB, append, BO),
	get(Mode, link_colour, LinkColour),
	get(Mode, clone, Clone),
	send(Clone, colour, LinkColour),
	send(Clone, underline, @on),
	emit(Content, PB, Clone),
	send(PB, append, BC).
action(anchor(Label, Content), PB, Mode) :-
	new(AO, anchor_box(Label)),
	new(AC, anchor_box(@nil)),
	new(_, hyper(AO, AC, close, open)),
	send(PB, append, AO),
	emit(Content, PB, Mode),
	send(PB, append, AC).


%	Text boxes

action(parbox(Content, Options), PB, Mode) :-
	get(Mode, alignment, DefAlignment),
	option(width(Width), Options, @default),
	option(rubber(Rubber), Options, @default),
	option(align(Alignment), Options, DefAlignment),
	option(valign(VAlign), Options, @default),
	new(GrBox, grbox(new(P, pbox(Width, Alignment)),
			 VAlign, Rubber)),
	(   option(auto_crop(Crop), Options)
	->  send(P, auto_crop, Crop)
	;   true
	),
	get(Mode, clone, Clone),
	emit(Content, P, Clone),
	send(PB, append, GrBox).

%	Table building commands

action(table(Options, Content), PB, Mode) :-
	option(align(Align), Options, center),
	get(Mode, clone, Clone),
	new(Table, doc_table(Options)),
	emit(Content, Table, Clone),	% Before ->append to avoid ->compute
	(   Align == center
	->  send(PB, append, @br),
	    new(PB2, parbox(0, center)),
	    send(PB, append, grbox(PB2, center, @table_rubber)),
	    send(PB2, append, new(TB, grbox(Table))),
	    send(PB, append, @br)
	;   send(PB, append, new(TB, grbox(Table, Align)))
	),
	send(TB, compute),		% initial dimensions
	send(Table, compute_dimensions).
action(tr, Table, _Mode) :-		% compatibility
	(   send(Table, instance_of, doc_table)
	->  send(Table, next_row, [])
	;   print_message(warning, doc(expected_context(tr, table))),
	    send(Table, append, @br)
	).
action(tr(Options, Content), Table, Mode) :-
	(   send(Table, instance_of, doc_table)
	->  send(Table, next_row, Options)
	;   print_message(warning, doc(expected_context(tr, table))),
	    send(Table, append, @br)
	),
	emit(Content, Table, Mode).
action(td(Options, Content), Table, Mode) :-
	(   send(Table, instance_of, doc_table)
	->  get(Mode, clone, Clone),
	    send(Clone, ignore_blanks, both),
	    get(Table, make_cell, Options, PB),
	    emit(Content, PB, Clone),
	    send(Table, compute_cell_rubber, PB)
	;   print_message(warning, doc(expected_context(td, table))),
	    emit(Content, Table, Mode)	% not in table: emit content anyway
	).
action(col(Options), Table, _Mode) :-
	send(Table, instance_of, doc_table),
	send(Table, col_spec, Options).
action(tbody(Options), Table, _Mode) :-
	send(Table, instance_of, doc_table),
	send(Table, row_group(Options)).
action(thead(_Options, Content), Table, Mode) :-
	send(Table, instance_of, doc_table),
	get(Table, def_alignment, DefAlign),
	send(Table, def_alignment, center),
	get(Mode, clone, Clone),
	send(Clone, set_font, weight, bold),
	emit(Content, Table, Clone),
	send(Table, def_alignment, DefAlign).
	
%	footnotes

action(footnote(Text), PB, Mode) :-
	send(PB, append,
	     new(T, grbox(bitmap('16x16/note.xpm')))),
%	     new(T, tbox('\247\', @symbol_style))),
	new(FN, pbox(500, left)),
	get(Mode, clone, Clone),
	emit(Text, FN, Clone),
	send(FN, auto_crop, @on),
	send(T, attribute, footnote, FN).

%	preformatted text

action(preformatted(Text), PB, Mode) :-
	get(Mode, style, Style),
	send(PB, append, tbox(Text, Style)).


		 /*******************************
		 *	      MESSAGES		*
		 *******************************/

:- multifile
	prolog:message/3.

prolog:message(doc(failed(emit(Term)))) -->
	[ 'Failed to emit ~p'-[Term] ].
prolog:message(doc(expected_context(Elem, Expected))) -->
	[ 'Found element "~w" without expected "~w" context'-
	  [Elem, Expected] ].
prolog:message(doc(ignored_attribute(Elem, Att))) -->
	[ 'Ignored ~p in ~w'-[Att, Elem] ].
