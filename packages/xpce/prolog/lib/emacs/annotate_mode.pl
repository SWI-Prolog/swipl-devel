/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

:- module(emacs_annotate_mode, []).
:- use_module(library(pce)).
:- set_prolog_flag(character_escapes, false).


		 /*******************************
		 *	    THE BUFFER		*
		 *******************************/

:- pce_begin_class(emacs_annotate_buffer, emacs_buffer,
		   "Emacs Buffer with associated styles").

variable(font,		[font]  := @default, get, "Main Font").
variable(styles,	[sheet] := @default, get, "Sheet with styles").
variable(margin_width,	int := 75,	     get, "Width of annotate margin").

attach(B, Editor:editor) :->
	"Load <-font and <-styles"::
	send(B, send_super, attach, Editor),
	get(B, font, Font),
	get(B, styles, Styles),
%	get(B, margin_width, Width),		% See ->setup_mode
	(   Font \== @default
	->  send(Editor, font, Font)
	;   send(B, slot, font, Editor?font)
	),
	(   Styles \== @default
	->  send(Editor, styles, Styles)
	;   send(B, slot, styles, Editor?styles)
	),
%	send(Editor, margin_width, Width).
	true.

detach(B, Editor:editor) :->
	"Save <-font and <-styles"::
	send(B, send_super, detach, Editor),
	get(Editor, font, Font),
	get(Editor, styles, Styles),
	get(Editor, margin_width, Width),
	send(B, slot, font, Font),
	send(B, slot, styles, Styles),
	send(B, slot, margin_width, Width).


loaded(B) :->
	"Called after it has been reloaded (register style-names)"::
	get(B, styles, Styles),
	(   Styles \== @default
	->  annotate_style_name_type(Type),
	    send(Styles, for_all, message(Type?context, add, @arg1?name))
	;   true
	).


do_save(B, File:file) :->
	"Do the actual saving (as an XPCE object)"::
	send(B, save_in_file, File),
	send(B, check_point_undo),
	send(B, modified, @off).

:- pce_end_class.


		 /*******************************
		 *	   EDITING MODE		*
		 *******************************/

:- pce_autoload(style_item, library(pce_style_item)).

:- initialization
	new(KB, emacs_key_binding(emacs_annotate, emacs_text)),
	send(KB, function, '\C-c\C-d', define_style).

:- initialization
	new(_, syntax_table(annotate)).

:- initialization
	new(MM, emacs_mode_menu(annotate, text)),

	send(MM, append, annotate, define_style),
	send(MM, append, annotate,
	     emacs_argument_item(make_fragment, @emacs_mode?style_names)),
	send(MM, append, annotate, delete_fragment),
	send(MM, append, annotate, margin_width).


annotate_style_name_type(Type) :-
	get(@types, member, annotate_style_name, Type), !.
annotate_style_name_type(Type) :-
	new(Type, type(annotate_style_name, name_of, new(chain))).

:- initialization
	annotate_style_name_type(_).


:- pce_begin_class(emacs_annotate_mode, emacs_text_mode).

setup_mode(E) :->
	"little hack ..."::
	send(E, margin_width, E?text_buffer?margin_width).


style_names(E, Names:chain) :<-
	"Return a list of associated styles"::
	get(E?styles?members, map, @arg1?name, Names),
	send(Names, sort).

:- pce_global(@emacs_annotate_margin_recogniser,
	      make_emacs_annotate_margin_recogniser).

make_emacs_annotate_margin_recogniser(R) :-
	new(Editor, @receiver?device),
	new(Fragment, ?(@receiver, fragment, @event)),
	new(Mode, Editor?mode),

	new(S1, click_gesture(left, '', single,
			      or(message(Mode, selected_fragment,
					 Fragment),
				 message(Mode, selected_fragment, @nil)))),
	new(O1, click_gesture(left, '', double,
			      message(Mode, open_fragment, Fragment))),

	new(R, handler_group(S1, O1)).


margin_width(E, Width:int) :->
	"Set margin and attach event-handling"::
	send(E?editor, margin_width, Width),
	(   Width \== 0
	->  send(E?margin, recogniser, @emacs_annotate_margin_recogniser)
	;   true
	).


define_style(E, Name:'annotate_style_name|name') :->
	"Define a (new) fragment style"::
	(   get(E?styles, value, Name, Style)
	->  true
	;   new(Style, style)
	),
	new(D, dialog('Define Fragment Style')),
	send(D, append, new(NI, text_item(name, Name))),
	send(D, append, new(SI, style_item(style, Style))),
	send(SI, pen, 1),
	send(SI, border, 5),
	send(SI, radius, 7),
	send(D, append,
	     button(ok, message(D, return,
				create(tuple, NI?selection, SI?selection)))),
	send(D, append,
	     button(cancel, message(D, return, @nil))),
	send(D, default_button, ok),
	get(D, confirm_centered, RVal),
	send(D, destroy),
	(   RVal \== @nil
	->  get(RVal, first, StyleName),
	    get(RVal, second, StyleObject),
	    send(E, style, StyleName, StyleObject),
	    annotate_style_name_type(Type),
	    send(Type?context, add, StyleName),
	    send(E, modified, @on)
	;   fail
	).
	

make_fragment(E, Style:annotate_style_name) :->
	"Create a fragment from the current selection"::
	get(E, selection, point(Start, End)),
	Length is End - Start,
	get(E, text_buffer, TB),
	new(_, fragment(TB, Start, Length, Style)),
	send(E, modified, @on).


fragment(E, F:fragment) :<-
	"Find most local fragment around <-caret"::
	get(E, caret, Caret),
	get(E, find_all_fragments, message(@arg1, overlap, Caret), Frags),
	send(Frags, sort, ?(@arg1?size, compare, @arg2?size)),
	get(Frags, head, F).


delete_fragment(E) :->
	"Delete most smallest fragment around caret"::
	get(E, fragment, F),
	send(F, free),
	send(E, modified, @on).	


selected_fragment(E, F:fragment*) :->
	"Make argument the current fragment"::
	send(E?editor, selected_fragment, F),
	(   F \== @nil
	->  send(E, report, status, '"%s" fragment', F?style)
	;   send(E, report, status, '')
	).


open_fragment(_E, _F:fragment) :->
	"To be implemented"::
	true.


:- pce_end_class.

