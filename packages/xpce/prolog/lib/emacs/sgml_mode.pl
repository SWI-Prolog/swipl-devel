/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1999 University of Amsterdam. All rights reserved.
*/

:- module(emacs_sgml_mode, []).
:- use_module(library(pce)).
:- use_module(library(emacs_extend)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module exploits the SGML/XML  parser   from  the SWI-Prolog package
sgml2pl to provide syntax colouring for SGML,  XML and HTML modes. Based
on a true parser, we  can  provide   much  better  feedback as heuristic
parsers used in most syntax-driven editors.  For example, we can provide
feedback on SHORTREF matches in  SGML   mode  by highlighting the tokens
acting as a short reference.  We  can   also  easily  give  the scope of
elements that are closed by omited elements.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- emacs_begin_mode(sgml, language,
		    "Mode for editing SGML documents",
		    % BINDINGS
		    [ open_document	     = button(sgml),
		      reload_dtd	     = button(sgml),
		      colourise_buffer       = button(sgml),
		      colourise_and_recenter = key('\\C-l'),
		      tag_selection          = key('\\e,'),
		      colourise_element      = key('\\C-c\\C-s'),
		      forward_move_out       = key('\\ee')
		    ],
		    % SYNTAX TABLE
		    [ '"'  = string_quote('"'),
		      '\'' = string_quote('\''),
		      paragraph_end('\\s *$\\|^<p>\\|\\s +<')
		    ]).

class_variable(auto_colourise_size_limit, int, 100000,
	       "Auto-colourise if buffer is smaller then this").
class_variable(auto_colourise_all_limit, int, 20000,
	       "Auto-colourise whole buffer if small enough").

variable(dialect,
	 {sgml,xml,html}:=sgml,
	 both,
	 "?ML Dialect used to parse").
variable(upcase_elements,
	 bool := @off,
	 both,
	 "Upcase inserted elements?").
variable(parser,
	 prolog,
	 none,
	 "Associated (DTD) parser").
variable(auto_colourise_size_limit,
	 int,
	 both,
	 "Auto-colourise if buffer is smaller then this").

%	make_parser(M, Parser)
%
%	Create a parser suitable for the current mode and load the DTD
%	into it.

make_parser(M, Parser) :-
	get(M, dialect, Dialect),
	get(M, text_buffer, TB),
	get(TB, file, File),
	get(File, name, FileName),
	get(M, dialect, Dialect),
	(   Dialect == html
	->  TheDialect = sgml,
	    dtd(html, DTD),
	    Options = [dtd(DTD)]
	;   TheDialect = Dialect,
	    Options = []
	),
	new_sgml_parser(Parser, Options),
	set_sgml_parser(Parser, file(FileName)),
	set_sgml_parser(Parser, dialect(TheDialect)).


%	load_dtd(+Mode, +Parser)
%
%	Load the document DTD into the given parser.

load_dtd(M, _) :-
	get(M, dialect, html), !.
load_dtd(M, Parser) :-
	get(M, text_buffer, TB),
	new(Re, regex('<!DOCTYPE', @off)),
	(   send(Re, search, TB)
	->  get(Re, register_start, Start),
	    pce_open(TB, read, In),
	    seek(In, Start, bof, _),
	    catch(sgml_parse(Parser,
			     [ source(In),
			       parse(declaration)
			     ]),
		  E,
		  show_message(M, E)),
	    close(In)
	;   send(M, report, warning, 'No <!DOCTYPE declaration')
	).


parser(M, Reload:[bool], Parser:prolog) :<-
	"Fetch the default parser"::
	(   Reload \== @on,
	    get(M, slot, parser, Parser),
	    Parser = sgml_parser(_)
	->  true
	;   send(M, destroy_dtd),
	    make_parser(M, Parser),
	    load_dtd(M, Parser),
	    send(M, slot, parser, Parser),
	    set_sgml_parser(Parser, doctype(_)) % use for partial parsing
	).


dtd(M, Reload:[bool], DTD:prolog) :<-
	"Fetch the current DTD"::
	get(M, parser, Reload, Parser),
	get_sgml_parser(Parser, dtd(DTD)).


reload_dtd(M) :->
	"Reload the DTD"::
	get(M, dtd, _).


destroy_dtd(M) :->
	"Destroy the associated DTD object"::
	(   get(M, slot, parser, Parser),
	    Parser = sgml_parser(_)
	->  free_sgml_parser(Parser),
	    send(M, slot, parser, [])
	;   true
	).


		 /*******************************
		 *	     INITIALISE		*
		 *******************************/

setup_mode(E) :->
	"Switch editor into fill-mode"::
	send(E, fill_mode, @on).


unlink(M) :->
	send(M, destroy_dtd),
	send_super(M, unlink).


open_document(M, DT:doctype=name) :->
	"Insert <!DOCTYPE line"::
	send(M, format, '<!DOCTYPE %s SYSTEM "">\n\n', DT),
	send(M, backward_char, 4).


		 /*******************************
		 *	       HELP		*
		 *******************************/

help_on_mode(M) :->
	(   absolute_file_name(sgml_mode,
			       [ extensions([html]),
				 access(read),
				 file_errors(fail)
			       ],
			       HTML)
	->  atom_concat('file:', HTML, URI),
	    www_open_url(URI)
	;   send(M, report, warning, 'Can''t find help file')
	).

		 /*******************************
		 *	 RECOULOR POLICIES	*
		 *******************************/

idle(M) :->
	"Idle event was received, colour the current element"::
	get(M?text_buffer, size, Size),
	get(M, auto_colourise_all_limit, Limit),
	(   Size < Limit
	->  send(M, colourise_buffer)
	;   send(M, colourise_element, @off)
	).

setup_styles(M) :->
	"Associate defined syntax-styles"::
	(   get(M, attribute, styles_initialised, @on)
	->  true
	;   send(M, reload_styles),
	    send(M, attribute, styles_initialised, @on)
	).

set_caret_and_inform(M) :->
	get(M, editor, Editor),
	get(Editor?image, index, @event, Caret),
	send(M, caret, Caret),
	get(M?text_buffer, find_all_fragments,
	    message(@arg1, overlap, Caret),
	    Fragments),
	send(Fragments, sort, ?(@arg1?length, compare, @arg2?length)),
	get(Fragments, find, ?(@arg1, attribute, balloon), Frag),
	get(Frag, balloon, Balloon),
	send(M, report, warning, 'SGML warning: %s', Balloon).

event(M, Ev:event) :->
	"Show insert-menu on right-down"::
	send(Ev, is_a, ms_right_down),
	(   get(M?image, index, Ev, I)
	->  send(M, caret, I)
	;   true
	),
	send(M, show_menu, Ev).

reload_styles(M) :->
	"Force reloading the styles"::
	retractall(style_name(_,_)),
	(   style(Class, Name, Style),
	    send(M, style, Name, Style),
	    assert(style_name(Class, Name)),
	    fail
	;   true
	).

colourise_element(M, Warn:[bool]) :->
	"Colour element at location"::
	send(M, setup_styles),
	get(M, caret, Caret),
	get(M, text_buffer, TB),
	new(Re, regex('<\\w+')),
	make_parser(M, Parser),
	load_dtd(M, Parser),
	set_sgml_parser(Parser, doctype(_)),
	pce_open(TB, read, In),
	(   get(TB, scan, Caret, line, -2, start, Start),
%	    format('Starting from ~w~n', [Start]),
	    find_element(M, Parser, Re, In, Start, From-To),
	    Caret < To
	->  send(M, remove_syntax_fragments, From, To),
%	    colour_item(element, TB, From, To),
	    seek(In, From, bof, _),
	    set_sgml_parser(Parser, charpos(From)),
	    colourise(M, Parser,
		      [ source(In),
			parse(element)
		      ])
	;   Warn == @off
	->  true
	;   send(M, report, warning, 'Could not find element')
	),
	close(In),
	free_sgml_parser(Parser).

%	find_element(+Mode, +Parser, +BeginRegex, +In, +Caret, -From-To)
%
%	Find the start and end of the current element.  We do so by scanning
%	backwards to '<\\w+' (Re).  Then we parse the element and see where
%	it ends.  If this isn't passed the current caret location we look
%	further backward.
%
%	This predicate is non-deterministic, broadening the scope on
%	backtracking.

:- dynamic
	caret/1,			% Caret
	tag/1.				% begin/end of environment

find_element(M, Caret, Range) :-
	get(M, parser, Parser),
	get(M, text_buffer, TB),
	pce_open(TB, read, In),
	new(Re, regex('<\\w+')),
	(   find_element(M, Parser, Re, In, Caret, Range)
	->  close(In)
	;   close(In),
	    fail
	).

find_element(M, Parser, Re, In, Caret, Range) :-
	get(M, text_buffer, TB),
	send(Re, search, TB, Caret, 0),
	get(Re, register_start, 0, Start0),
	find_element(M, Parser, Re, In, Caret, Start0, Range).

find_element(M, Parser, _Re, In, Caret, Start, Start-To) :-
	\+ get(M?text_buffer, find_fragment,
	       and(message(@arg1, overlap, Start),
		   @arg1?parsed == @off),
	       _),
	seek(In, Start, bof, _),
	set_sgml_parser(Parser, charpos(Start)),
	retractall(caret(Caret)),
	retractall(tag(_)),
	assert(caret(Caret)),
	catch(sgml_parse(Parser,
			 [ source(In),
			   parse(element),
			   syntax_errors(quiet),
			   call(begin, emacs_sgml_mode:find_on_begin),
			   call(end,   emacs_sgml_mode:find_on_end)
			 ]),
	      E,
	      show_message(M, E)),
					% charpos/1 yields start-position
	get_sgml_parser(Parser, charpos(_, To)),
%	format('Found ~d-~d~n', [Start, To]),
	To-1 > Caret.
find_element(M, Parser, Re, In, Caret, Start0, Range) :-
	get(M, text_buffer, TB),
	send(Re, search, TB, Start0, 0),
	get(Re, register_start, 0, Start1),
	find_element(M, Parser, Re, In, Caret, Start1, Range).

find_on_begin(Tag, Attributes, Parser) :-
	caret(Caret),
	get_sgml_parser(Parser, charpos(From, To)),
	From =< Caret,
	retractall(tag(_)),
	get_sgml_parser(Parser, event_class(Class)),
	asserta(tag(begin(Tag, Attributes, Class, From-To))).
find_on_end(_Tag, Parser) :-
	caret(Caret),
	\+ tag(end(_,_,_)),
	get_sgml_parser(Parser, charpos(From, To)),
	To >= Caret,
	get_sgml_parser(Parser, event_class(Class)),
	asserta(tag(end(Class, From, To))).


		 /*******************************
		 *	   COLOURISING		*
		 *******************************/

colourise_and_recenter(M) :->
	"Colour according to syntax and recenter"::
	send(M, auto_colourise_buffer),
	send(M, recenter).

colourise_buffer(M) :->
	OldTime is cputime,
	new(Class, class(sgml_mode_fragment)),
	get(Class, no_created, OldCreated),

	send(M, setup_styles),
	send(M, remove_syntax_fragments),
	send(M, report, progress, 'Colourising buffer ...'),
	colourise_buffer(M),
	Used is cputime - OldTime,
	get(Class, no_created, NewCreated),
	Created is NewCreated - OldCreated,
	send(M, report, done,
	     'done, %.2f seconds, %d fragments', Used, Created).

:- dynamic
	current_tb/2.

colourise_buffer(M) :-
	make_parser(M, Parser),
	get(M, text_buffer, TB),
	pce_open(TB, read, In),
	colourise(M, Parser,
		  [ source(In)
		  ]),
	free_sgml_parser(Parser).

colourise(M, Parser, Options) :-
	get_sgml_parser(Parser, file(File)),
	get(M, text_buffer, TB),
	asserta(current_tb(TB, File), Ref),
	catch(sgml_parse(Parser,
			 [ call(begin, emacs_sgml_mode:on_begin),
			   call(end,   emacs_sgml_mode:on_end),
			   call(cdata, emacs_sgml_mode:on_cdata),
			   call(decl,  emacs_sgml_mode:on_decl),
			   call(error, emacs_sgml_mode:on_error)
			 | Options
			 ]),
	      E,
	      show_message(M, E)),
	erase(Ref).
	
on_begin(_Tag, _Attributes, Parser) :-
	get_sgml_parser(Parser, file(File)),
	current_tb(TB, File),
%	format('Tag ~w, Attr = ~p~n', [Tag, Attributes]),
	get_sgml_parser(Parser, charpos(From, To)),
	get_sgml_parser(Parser, event_class(Class)),
	Class \== omitted,
	colour_item(tag(begin, Class), TB, From, To).
on_end(_Tag, Parser) :-
	get_sgml_parser(Parser, file(File)),
	current_tb(TB, File),
	get_sgml_parser(Parser, charpos(From, To)),
	get_sgml_parser(Parser, event_class(Class)),
%	format('At ~d-~d: Class = ~w~n', [From, To, Class]),
	Class \== omitted,
	colour_item(tag(end, Class), TB, From, To).
on_cdata(_CDATA, Parser) :-
	get_sgml_parser(Parser, file(File)),
	current_tb(TB, File),
	get_sgml_parser(Parser, charpos(From, To)),
	(   get_sgml_parser(Parser, context([Tag|_]))
	->  (   get_sgml_parser(Parser, dtd(DTD)),
	        dtd_property(DTD, element(Tag, _, Model)),
		(   Model == cdata
		;   Model == rcdata
		)
	    ->	Type = cdata
	    ;	Type = pcdata
	    )
	),
%	format('CDATA from ~d-~d~n', [From, To]),
	colour_item(cdata(Type), TB, From, To, Fragment),
	(   Type == cdata
	->  send(Fragment, parsed, @off)
	;   true
	).
on_decl(DECL, Parser) :-
	get_sgml_parser(Parser, file(File)),
	current_tb(TB, File),
	get_sgml_parser(Parser, event_class(explicit)),
	get_sgml_parser(Parser, charpos(From, To)),
%	format('Decl ~d-~d: ~w~n', [From, To, DECL]),
	(   DECL == ''
	->  colour_item(comment, TB, From, To, Fragment),
	    send(Fragment, parsed, @off)
	;   send(regex('DOCTYPE', @off), match, DECL)
	->  colour_item(doctype, TB, From, To)
	;   new(Re, regex('\\w*')),
	    send(Re, match, DECL),
	    get(Re, register_value, DECL, 0, name, DeclType0),
	    get(DeclType0, downcase, DeclType),
%	    format('Decl(~w)~n', [DeclType]),
	    colour_item(decl(DeclType), TB, From, To)
	).
on_error(Severity, Message, Parser) :-
	current_tb(TB, File),
	(   get_sgml_parser(Parser, file(File))
	->  get_sgml_parser(Parser, charpos(From, To)),
	    colour_item(error(Severity), TB, From, To, Fragment),
	    (   Fragment \== @nil
	    ->  send(Fragment, message,  Message),
		send(Fragment, severity, Severity)
	    ;   true
	    )
	;   format(user_error, 'SGML: Error in other file!~n', [])
	).

%	colour_item(+Class, +TB, +Pos)
%
%	colourise region if a style is defined for this class.

colour_item(Class, TB, From, To) :-
	colour_item(Class, TB, From, To, _Fragment).

colour_item(Class, TB, From, To, Fragment) :-
	style_name(Class, Name), !,
	Len is To - From,
	Len > 0,
	new(Fragment, sgml_mode_fragment(TB, From, Len, Name)).
colour_item(_, _, _, _, @nil).
	

		 /*******************************
		 *	       STYLES		*
		 *******************************/

style(tag(begin, shortref),	style(colour := blue,
				      background := grey90,
				      bold   := @on)).
style(tag(begin, _),		style(colour := blue,
				      bold   := @on)).
style(tag(end, shorttag),	style(colour := blue,
				      bold   := @on)).
style(tag(end, shortref),	style(colour := blue,
				      background := grey90,
				      bold   := @on)).
style(tag(end, _),     		style(colour := blue)).
style(cdata(cdata),		style(colour := sea_green)).
style(doctype,			style(bold := @on)).
style(comment,			style(colour := dark_green,
				      background := grey90)).
style(decl(_),			style(colour := purple)).
style(error(warning),		style(background := orange)).
style(error(_),			style(background := hotpink)).
style(entity,	     		style(colour := dark_green)).
style(element,	     		style(background := pale_turquoise)).

:- dynamic
	style_name/2.

style(Class, Name, Style) :-
	style(Class, Style),
	copy_term(Class, Copy),
	numbervars(Copy, 0, _),
	term_to_atom(Copy, Name).


		 /*******************************
		 *	     TAGGING		*
		 *******************************/

set_insert_point(M, Point:[int]) :->
	"Set mark at point if not set"::
	get(M, mark, Mark),
	(   Mark == 0
	->  (   Point == @default
	    ->	send(M, mark, M?caret)
	    ;	send(M, mark, Point)
	    )
	;   true
	).

insert_begin(M, Tag:name) :->
	"Insert begin-tag and required attributes"::
	fix_case(M, Tag, TheTag),
	send(M, format, '<%s>', TheTag),
	get(M, dtd, DTD),
	findall(A, dtd_property(DTD, attribute(Tag, A, _, required)), List),
	send(M, backward_char),
	insert_attributes(List, M),
	send(M, forward_char).


insert_attributes([], _).
insert_attributes([H|T], M) :-
	send(M, format, ' %s=""', H),
	send(M, set_insert_point, M?caret-1),
	insert_attributes(T, M).


insert_end(M, Tag:name) :->
	"Insert end-tag for element"::
	fix_case(M, Tag, TheTag),
	send(M, format, '</%s>', TheTag).


fix_case(M, Tag, TheTag) :-
	(   get(M, upcase_elements, @on)
	->  get(Tag, upcase, TheTag)
	;   TheTag = Tag
	).


style_for_event(Ev, Style) :-
	(   send(Ev, has_modifier, c)
	->  Style = inline
	;   send(Ev, has_modifier, s)
	->  Style = block
	;   send(Ev, has_modifier, m)
	->  Style = shorttag
	;   Style = @default
	).


show_menu(M, Ev:event) :->
	"Show menu to insert-tag/tag selection"::
	(   send(M, in_tag)
	->  send(M, show_attribute_menu, Ev)
	;   send(M, show_element_menu, Ev)
	).


in_tag(M) :->
	"Test whether caret is between <>"::
	get(M, caret, Caret),
	get(M, text_buffer, TB),
	send(regex('<[^>]*'), match, TB, Caret, 0),
	send(regex('[^<]*[>/]'), match, TB, Caret). % / for shortag


show_element_menu(M, Ev:event) :->
	"Show menu for inserting a new element"::
	(   get(M, allowed_elements, List),
	    delete(List, '#pcdata', Elems),
	    Elems \== [],
	    sort(Elems, Sorted)
	->  (   get(M, selection, point(A,B)), B > A
	    ->	Label = tag_selection
	    ;	Label = insert_element
	    ),
	    get(Ev, button, Button),
	    style_for_event(Ev, Style),
	    new(G, popup_gesture(new(P, popup(Label,
					      message(M, popup_tag_selection,
						      @arg1, Style))),
				 Button, new(modifier))),
	    send(P, show_label, @on),
	    length(Sorted, Len),
	    Cols is max(1, Len // 20),
	    send(P, columns, Cols),
	    send_list(P, append, Sorted),
	    send(G, event, Ev)
	;   send(M, report, warning, 'No element allowed here')
	).


show_attribute_menu(M, Ev:event) :->
	"Show menu for adding attributes"::
	get(M, caret, Caret),
	get(M, text_buffer, TB),
	new(Re, regex('<[^>]*')),
	send(Re, match, TB, Caret, 0),
	get(Re, register_start, 0, Start),
	(   get(M, looking_at_element, Start, E)
	->  make_parser(M, Parser),
	    load_dtd(M, Parser),
	    get_sgml_parser(Parser, dtd(DTD)),
	    dtd_property(DTD, attributes(E, Atts)),
	    (	Atts == []
	    ->	free_sgml_parser(Parser),
		send(M, report, warning, 'Element "%s" has no attributes', E)
	    ;	sort(Atts, Sorted),
%	        format('Atts = ~p~n', [Sorted]),
	        get(Ev, button, Button),
		new(G, popup_gesture(new(P, popup(add_attribute,
						  message(M, insert_attribute,
							  @arg1))),
				     Button,
				     new(modifier))),
		send(P, show_label, @on),
		length(Sorted, Len),
		Cols is max(1, Len // 10),
		send(P, columns, Cols),
		fill_attribute_menu(Sorted, DTD, E, P, M),
		free_sgml_parser(Parser),
		send(G, event, Ev)
	    )
	;   send(M, report, warning, 'Not in begin-tag')
	).


fill_attribute_menu([], _, _, _, _).
fill_attribute_menu([A|T], DTD, E, P, Mode) :-
	dtd_property(DTD, attribute(E, A, Type, Default)),
	add_attribute_menu(Type, Default, A, P, Mode),
	fill_attribute_menu(T, DTD, E, P, Mode).
	
add_attribute_menu(nameof(List), Def, A, P, Mode) :- !,
	send(P, append, new(P2, popup(A, message(Mode, insert_attribute,
						 A, @arg1)))),
	add_attribute_values(List, Def, P2).
add_attribute_menu(Type, Def, A, P, _Mode) :- !,
	type_label(Type, TypeLabel),
	send(P, append, new(MI, menu_item(A, @default,
					  string('%s (%s)', A, TypeLabel)))),
	(   Def == required
	->  send(MI, font, bold)
	;   true
	).

type_label(list(Type), Label) :- !,
	atom_concat(Type, s, Label).
type_label(Type, Type).

add_attribute_values([], _, _).
add_attribute_values([H|T], Def, P) :-
	send(P, append, new(MI, menu_item(H))),
	(   Def == default(H)
	->  send(MI, font, bold)
	;   true
	),
	add_attribute_values(T, Def, P).


insert_attribute(M, Att:name, Val:'[name|int|real]') :->
	"Add attribute-value pair"::
	get(M, text_buffer, TB),
	get(M, caret, Caret),
	new(Re, regex('\\(\\s +\\|[/>]\\)')),
	send(Re, search, TB, Caret),	% find place to insert
	get(Re, register_start, 0, Where),
	(   send(regex('\\s +'), match, TB, Where)
	->  get(Re, register_end, 0, NewCaret), % after blanks
	    send(M, caret, NewCaret)
	;   send(M, caret, Where),
	    send(M, format, ' ')
	),
	(   Val == @default
	->  send(M, format, '%s=""', Att),
	    get(M, caret, C),
	    (	send(M, looking_at, '\\s \\|[/>]')
	    ->  true
	    ;	send(M, format, ' ')
	    ),
	    send(M, caret, C-1)
	;   send(M, format, '%s="%s"', Att, Val),
	    (	send(M, looking_at, '\\s \\|[/>]')
	    ->  true
	    ;	send(M, format, ' ')
	    )
	),
	send(M, mark_undo).		% called from popup!


popup_tag_selection(M, Tag:name, Style0:[{inline,shorttag,block}]) :->
	"->tag_selection wrapper for popup"::
	(   Style0 == @default
	->  style_for_event(@event, Style)
	;   Style = Style0
	),
	send(M, tag_selection, Tag, Style),
	send(M, mark_undo).


tag_selection(M, Tag:[name], Style:[{inline,block,shorttag}]) :->
	"Tag the current selection using element"::
	(   Tag == @default
	->  new(TI, text_item(element)),
	    (   get(M, allowed_elements, List),
%	        format('Allowed: ~p~n', [List]),
		delete(List, '#pcdata', Elems),
		sort(Elems, Sorted)
	    ->  send(TI, value_set, Sorted)
	    ;   true
	    ),
	    get(M, prompt_using, TI, String),
	    get(String, value, TheTag)
	;   TheTag = Tag
	),
	(   get(M, selection, point(A,B)),
	    B > A
	->  send(M, tag_region, TheTag, A, B, Style),
	    send(M, selection, 0, 0),
	    send(M, colourise_element)
	;   send(M, insert_element, TheTag, Style)
	).


tag_region(M, Tag:[name], From:int, To:int,
	   Style:[{inline,block,shorttag}]) :->
	"Tag a defined region"::
	fix_case(M, Tag, TheTag),
	get(M, text_buffer, TB),
	(   Style == shorttag
	->  send(TB, insert, To, /),
	    send(TB, insert, From, string('<%s/', TheTag))
	;   Style == block
	->  (   get(M, column, To, 0)
	    ->	send(TB, insert, To, string('</%s>\n', TheTag))
	    ;	send(TB, insert, From, string('\n</%s>\n', TheTag))
	    ),
	    (   get(M, column, From, 0)
	    ->	send(TB, insert, From, string('<%s>\n', TheTag))
	    ;	send(TB, insert, From, string('\n<%s>\n', TheTag))
	    )
	;   Style == inline
	->  send(TB, insert, To, string('</%s>', TheTag)),
	    send(TB, insert, From, string('<%s>', TheTag))
	;   get(M, column, From, 0),
	    get(M, column, To, 0)
	->  send(M, tag_region, Tag, From, To, block)
	;   send(M, tag_region, Tag, From, To, inline)
	).
	    

insert_element(M, Tag:element=name, Style:[{inline,shorttag,block}]) :->
	"Insert a new empty element"::
	(   get(M, dtd, DTD),
	    dtd_property(DTD, element(Tag, Omit, Content))
	->  true
	;   Omit = omit(false, false),
	    Content = '#pcdata'
	),
	fix_case(M, Tag, TheTag),
	send(M, prepare_insert),
	send(M, mark, 0),		% put insert position here
	insert_by_style(Style, M, TheTag, Omit, Content, _),
	send(M, colourise_element),
	(   get(M, mark, Mark),
	    Mark > 0
	->  send(M, caret, Mark)
	;   true
	).

prepare_insert(M) :->
	"Find location to insert a new tag"::
	get(M, caret, Caret),
	(   find_element(M, Caret, From-_To)
	->  get(M, looking_at_element, From, E),
%	    format('~p: Inserting in "~w" at ~w~n', [M, E, From]),
	    get(M, dtd, DTD),
	    dtd_property(DTD, element(E, _, Content)),
	    (	mixed_content(Content)
	    ->	true
	    ;	get(M, column, From, Col0),
		Col is Col0+2,
		get(M, text_buffer, TB),
		get(TB, scan, Caret, line, 0, start, SOL),
		(   new(Re, regex('\\s *')),
		    send(Re, match, TB, SOL, Caret),
		    get(Re, register_end, Caret)
		->  true		% at a blank line
		;   send(M, newline)
		),
		send(M, align_line, Col)
	    )
	;   true
	).

insert_by_style(_, M, Tag, _, empty, End) :- !,
	send(M, insert_begin, Tag),
	(   get(M, dialect, xml)
	->  send(M, backward_char),
	    send(M, format, /),
	    send(M, forward_char)
	;   true
	),
	get(M, caret, End).
insert_by_style(Style, M, Tag, _, Model, End) :-
	required_content(Model, List),
	(   mixed_content(Model)
	->  def_style(Style, inline, TheStyle),
	    insert_by_style(TheStyle, M, Tag, End),
	    send(M, set_insert_point)
	;   insert_by_style(block, M, Tag, End0),
	    get(M, text_buffer, TB),
	    new(Mark, fragment(TB, End0, 0)),
	    insert_sub_elements(List, M),
	    get(Mark, start, End),
	    free(Mark)
	).

mixed_content(M) :-
	term_member('#pcdata', M), !.

term_member(X, X).
term_member(X, C) :-
	compound(C),
	arg(_, C, A),
	term_member(X, A).

insert_by_style(shorttag, M, Tag, End) :- !,
	send(M, insert_begin, Tag),
	send(M, backward_delete_char),
	send(M, format, '//'),
	get(M, caret, End),
	send(M, backward_char).
insert_by_style(inline, M, Tag, End) :- !,
	send(M, insert_begin, Tag),
	get(M, caret, New),
	send(M, insert_end, Tag),
	get(M, caret, End),
	send(M, caret, New).
insert_by_style(block, M, Tag, End) :- !,
	send(M, insert_begin, Tag),
	get(M, caret, Insert),
	send(M, newline_and_indent),
	send(M, insert_end, Tag),
	get(M, caret, End),
	send(M, caret, Insert).
insert_by_style(@default, M, Tag, End) :-
	get(M, text_buffer, TB),
	get(M, caret, Caret),
	get(TB, scan, Caret, line, 0, start, SOL),
	(   send(regex('\\s *$'), match, TB, SOL)
	->  insert_by_style(block, M, Tag, End)
	;   insert_by_style(inline, M, Tag, End)
	).


def_style(@default, Style, Style) :- !.
def_style(Style, _, Style).

insert_sub_elements([], _).
insert_sub_elements([H|T], M) :-
	send(M, format, '  '),
	get(M, dtd, DTD),
	dtd_property(DTD, element(H, Omit, Content)),
	send(M, prepare_insert),
	insert_by_style(@default, M, H, Omit, Content, End),
	(   T == []
	->  true
	;   send(M, caret, End),
	    insert_sub_elements(T, M)
	).


required_content(empty, []).
required_content(cdata, []).
required_content(Model, Elems) :-
	phrase(required_content(Model), Elems).

required_content((A,B)) --> !,
	required_content(A),
	required_content(B).
required_content(&(A,B)) --> !,
	required_content(A),
	required_content(B).
required_content('|'(_,_)) --> !,
	[].
required_content(?(_)) -->
	[].
required_content(*(_)) -->
	[].
required_content(+(A)) -->
	required_content(A).
required_content('#pcdata') --> !,
	[].
required_content(A) -->
	[A].


looking_at_element(M, From:int, Elem:name) :<-
	new(Re, regex('<\\([-_:a-zA-Z0-9]+\\)')),
	get(M, text_buffer, TB),
	send(Re, match, TB, From),
	get(Re, register_value, TB, 1, name, Elem).


allowed_elements(M, Allowed:prolog) :<-
	"Show elements allowed here"::
	get(M, caret, Caret),
	get(M, text_buffer, TB),
	new(Re, regex('<\\w+')),
	make_parser(M, Parser),
	load_dtd(M, Parser),
	get_sgml_parser(Parser, dtd(DTD)),
	set_sgml_parser(Parser, doctype(_)),
	pce_open(TB, read, In),
	(   find_element(M, Parser, Re, In, Caret, From-_To),
	    get(M, looking_at_element, From, E),
%	    format('Looking at ~w~n', [E]),
	    (	dtd_property(DTD, doctype(E))
	    ;   dtd_property(DTD, element(E, omit(_, false), _))
	    )
	->  seek(In, From, bof, _),
	    set_sgml_parser(Parser, charpos(From)),
	    Len is Caret - From,
	    catch(sgml_parse(Parser,
			     [ source(In),
			       content_length(Len),
			       syntax_errors(quiet),
			       parse(input)	% do not complete document
			     ]),
		  E,
		  show_message(M, E)),
	    get_sgml_parser(Parser, allowed(Allowed))
	;   dtd_property(DTD, doctype(DocType)),
	    atom(DocType)
	->  Allowed = [DocType]
	;   send(M, report, warning, 'No current element'),
	    Allowed = []
	),
	close(In),
	free_sgml_parser(Parser).

report_allowed(M) :->			% DEBUGGING
	"Report allowed elements at point"::
	get(M, allowed_elements, Allowed),
	concat_atom(Allowed, ', ', Atom),
	send(M, report, status, 'Allowed: %s', Atom).

show_message(M, E) :-
	message_to_string(E, String),
	send(M, report, warning, 'Caught error: %s', String).


		 /*******************************
		 *	   MOVING AROUND	*
		 *******************************/

forward_move_out(M) :->
	"Move forwards to end of current element"::
	get(M, caret, Caret),
	(   find_element(M, Caret, _From-To)
	->  send(M, caret, To)
	;   send(M, report, warning, 'Cannot find element')
	).

:- emacs_end_mode.



		 /*******************************
		 *	       XML		*
		 *******************************/

:- emacs_begin_mode(xml, sgml,
		    "Mode for editing XML documents",
		    [],
		    []).

initialise(M) :->
	send_super(M, initialise),
	send(M, dialect, xml).

open_document(M, DTD:doctype=name) :->
	"Insert document header"::
	send(M, format, '<?xml version="1.0"?>\n'),
	send_super(M, open_document, DTD).

:- emacs_end_mode.


		 /*******************************
		 *	       HTML		*
		 *******************************/

:- emacs_begin_mode(html, sgml,
		    "Mode for editing HTML documents",
		    [],
		    []).

initialise(M) :->
	send_super(M, initialise),
	send(M, dialect, html).

open_document(M) :->
	"Insert document header"::
	send(M, format,
	     '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2//EN">\n\n'),
	send(M, insert_element, html).

:- emacs_end_mode.


		 /*******************************
		 *	      FRAGMENT		*
		 *******************************/

:- pce_begin_class(sgml_mode_fragment, emacs_colour_fragment,
		   "Provide colourised region").

variable(parsed,   bool := @on,	     both, "@off for unparsed fragments").

:- pce_end_class.
