/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

:- module(pce_help, []).
:- use_module(library(pce)).
:- use_module(library(pce_prompter)).
:- require([ auto_call/1
	   , ignore/1
	   , forall/2
	   , member/2
	   , pce_help_file/2
	   , term_to_atom/2
	   , absolute_file_name/3
	   , atom_to_term/3
	   , concat/3
	   , default/3
	   , send_list/3
	   , sformat/3
	   ]).

resource(help,	image, image('32x32/help.xpm')).

:- pce_global(@helper, new(helper)).
:- pce_global(@finder, new(finder)).
:- pce_autoload(finder, library(find_file)).


		 /*******************************
		 *	      HELPER		*
		 *******************************/

:- pce_begin_class(helper, sheet, "Helper toplevel").

variable(buffers, sheet, get, "Sheet of open buffers").

:- send(@class, save_style, external).

initialise(Helper) :->
	send(Helper, send_super, initialise),
	send(Helper, slot, buffers, new(sheet)),
	send(@pce, exit_message, message(Helper, save_if_modified)).


save_if_modified(Helper) :->
	"Save unsaved buffers"::
	get(Helper, buffers, Sheet),
	send(Sheet, for_all,
	     message(@arg1?value?editors, for_all,
		     message(@arg1, save_if_modified))).


help_file(Helper, Name:name, File:file) :->
	"Attach a help-file"::
	send(File, absolute_path),
	(   get(Helper, value, Name, OldFile)
	->  (	send(File, same, OldFile)
	    ->	true
	    ;	send(Helper, report, warning,
		     'Redefined help file for database %s', Name),
		send(Helper, value, Name, File),
		ignore(send(Helper?buffers, delete, Name))
	    )
	;   send(Helper, value, Name, File)
	).


give_help(Helper, Database:name, Label:[name]) :->
	"View given database at label"::
	get(Helper, buffer, Database, @on, Buffer),
	(   get(Buffer?editors, head, Editor)
	->  true
	;   get(Buffer, open, Editor)
	),
	(   get(Buffer, size, 0)
	->  send(Editor, editable, @on)
	;   Label == @default
	->  send(Editor, caret, 0)
	;   send(Editor, goto, Label)
	).


buffer(Helper, Database:name, Interactive:[bool], Buffer:hlp_buffer) :<-
	"Return hlp-buffer holding help-text"::
	(   get(Helper?buffers, value, Database, Buffer)
	->  true
	;   (	get(Helper, value, Database, RC)
	    ;	new(RC, pce_help_file:resource(Database, help))
	    )
	->  (   pce_help_file:resource(Database, help, FileSpec),
	        absolute_file_name(FileSpec, Path)
	    ->  new(File, file(Path))
	    ;	File = @nil
	    ),
	    (	send(RC, exists)
	    ->	(   send(RC, check_object),
		    get(RC, object, Buffer),
		    send(Buffer, instance_of, hlp_buffer)
		->  send(Helper?buffers, value, Database, Buffer),
		    send(Buffer, file, File)
		;   send(Helper, report, error,
			 'Illegal data-format in "%N"', RC),
		    fail
		)
	    ;	Interactive == @on,
	        (   get(@pce, is_runtime_system, @on)
		->  send(@display, inform,
			 'No help available for "%s"', Database),
		    fail
		;   send(@display, confirm,
			 'No help-database "%s"\n\nCreate it?', Database),
		    new(Buffer, hlp_buffer),
		    send(Buffer, file, File)
		)
	    )
	;   term_to_atom(DBTerm, Database),
	    functor(DBTerm, _, 1),
	    arg(1, DBTerm, DBname)
	->  assert(pce_help_file:resource(DBname, help, DBTerm)),
	    get(Helper, buffer, DBname, Buffer)
	;   Interactive == @on,
	    send(Helper, report, error,
		 'No help-database called %s', Database),
	    fail
	).

:- pce_end_class.


		 /*******************************
		 *	       FRAGMENT		*
		 *******************************/

:- pce_begin_class(hlp_fragment, fragment, "Help fragment").

variable(label, 'name|int*', both, "Reference label of this text").

number_section(_Frag, _I:vector) :->
	"Dummy"::
	true.

emptied(F) :->
	"Destroy empty fragments"::
	send(F, free).

:- pce_end_class.

		 /*******************************
		 *	 (SECTION) HEADER	*
		 *******************************/

:- pce_begin_class(hlp_header, hlp_fragment, "Header in help-file").

:- pce_global(@clean_header_regex, new(regex('\\(\\sd+\\.\\)+\\s *'))).
:- pce_global(@empty_header_regex, new(regex('\\s *$'))).

level(section,		1, '%d.  ').
level(subsection,	2, '%d.%d.  ').
level(subsubsection,	3, '%d.%d.%d.  ').
level(subsubsubsection,	4, '%d.%d.%d.%d.  ').

number_section(Frag, I:vector) :->
	"Number section-header"::
	(   send(@empty_header_regex, match, Frag), fail % TBD: get this right
	->  send(Frag, free)
	;   send(@clean_header_regex, for_all, Frag,
		 message(@arg1, replace, @arg2, '')),
	    get(Frag, style, Style),
	    level(Style, Level, Format),
	    forall(level(_, L, _),
		   (   L > Level
		   ->  get(I, element, L, Number),
		       send(Number, value, 0)
		   ;   true
		   )),
	    get(I, element, Level, N),
	    send(N, plus, 1),
	    get(class(string), get_vector, instance, Format, I, Str),
	    send(Frag, insert, 0, Str)
	).

emptied(F) :->
	(   get(F, style, Style),
	    level(Style, _, _)
	->  get(F, text_buffer, TB),
	    free(F),
	    send(TB, request_renumber, @on)
	;   true
	).

:- pce_end_class.


		 /*******************************
		 *	       BUFFER		*
		 *******************************/

:- pce_begin_class(hlp_buffer, text_buffer, "Help text-buffer").

variable(request_renumber,	bool := @off,	both, "Renumber required").
variable(file,			file*,		both, "Associated file").

renumber(TB) :->
	"Renumber section structure"::
	new(I, vector),
	forall(level(_, Level, _), send(I, element, Level, number(0))),
	send(TB, for_all_fragments, message(@arg1, number_section, I)).


open(TB, E:hlp_editor) :<-
	"Create a hlp_frame for the buffer"::
	new(E, hlp_editor(TB)),
	send(E, open).


open(TB) :->
	get(TB, open, _).

:- pce_end_class.


		 /*******************************
		 *	    THE EDITOR		*
		 *******************************/

:- initialization
   new(KB, key_binding(hlp_editor, editor)),
   send(KB, function, '\\C-h',     prefix),
   send(KB, function, '\\C-hb',    show_key_bindings),
   send(KB, function, '\\C-c1',    make_section),
   send(KB, function, '\\C-c2',    make_subsection),
   send(KB, function, '\\C-c3',    make_subsubsection),
   send(KB, function, '\\C-c4',    make_subsubsubsection),
   send(KB, function, '\\C-ce',    make_example),
   send(KB, function, '\\C-ci',    make_emphasized),
   send(KB, function, '\\C-cb',    make_bold),
   send(KB, function, '\\C-ct',    make_title),
   send(KB, function, '\\C-c\\C-e', toggle_edit_mode),
   send(KB, function, '\\C-c\\C-l', assign_label),
   send(KB, function, '\\C-c\\C-b', make_button),
   send(KB, function, '\\C-c\\C-f', follow),
   send(KB, function, '\\C-c\\C-d', delete_fragment),
   send(KB, function, '\\C-cC',	  make_table_of_contents),
   send(KB, function, '\\C-cc',	  make_local_table_of_contents),
   send(KB, function, '\\C-x\\C-s', save),
   send(KB, function, '\\C-x2',    split_view),
   send(KB, function, '\\C-x1',    delete_other_views),
   send(KB, function, '\\C-xi',	  insert_file).


:- pce_begin_class(hlp_editor, editor, "Simple hyper-text editor").

class_variable(title_font,	      font,  font(helvetica, bold, 24)).
class_variable(section_font,	      font,  boldhuge).
class_variable(subsection_font,       font,  boldlarge).
class_variable(subsubsection_font,    font,  bold).
class_variable(subsubsubsection_font, font,  bold).
class_variable(example_font,	      font,  fixed).
class_variable(text_font,	      font,  normal).
class_variable(bold_font,	      font,  bold).
class_variable(emphasize_font,	      font,  italic).
class_variable(size,		      size,  size(88, 20)).
class_variable(jump_style,	      style, when(@colour_display,
						  style(colour := dark_green,
							underline := @on),
						  style(underline := @on))).
class_variable(keyword_style,	      style, style(font := bold)).
						    

initialise(E, Data:[file|text_buffer]) :->
	"Create hyper-text editor for help-system"::
	send(E, send_super, initialise, new(hlp_buffer)),
	get(E, title_font, 	      TitleFont),
	get(E, section_font, 	      SectionFont),
	get(E, subsection_font,	      SubSectionFont),
	get(E, subsubsection_font,    SubSubSectionFont),
	get(E, subsubsubsection_font, SubSubSubSectionFont),
	get(E, example_font,	      ExampleFont),
	get(E, text_font,	      TextFont),
	get(E, bold_font,	      BoldFont),
	get(E, emphasize_font,	      EmFont),
	get(E, jump_style,	      JumpStyle),
	get(E, keyword_style,	      KeywordStyle),

	send(E, font, TextFont),
	send(E, bindings, hlp_editor),

	send(E, style, title,		style(font := TitleFont)),
	send(E, style, section,		style(font := SectionFont)),
	send(E, style, subsection,	style(font := SubSectionFont)),
	send(E, style, subsubsection,	style(font := SubSubSectionFont)),
	send(E, style, subsubsubsection,style(font := SubSubSubSectionFont)),
	send(E, style, example, 	style(font := ExampleFont)),
	send(E, style, button,		JumpStyle),
	send(E, style, bold,		style(font := BoldFont)),
	send(E, style, emphasize,	style(font := EmFont)),
	send(E, style, keyword,		KeywordStyle),

	send(E, fill_mode, @on),
	send(E, right_margin, 80),
	send(E, editable, @off),

	send(E?image, recogniser,
	     new(C, click_gesture(left, '', double,
				  message(E, follow)))),
	send(C, condition, E?button),
	send(E?image, recogniser,
	     popup_gesture(@hlp_editor_popup, right, c)),


	(   send(Data, instance_of, file)
	->  send(E, load, Data)
	;   send(Data, instance_of, hlp_buffer)
	->  send(E, text_buffer, Data)
	;   true
	).


:- pce_global(@hlp_editor_popup, make_hlp_editor_popup).

make_hlp_editor_popup(P) :-
	new(Editor, @arg2?device),
	new(P, popup(options, message(Editor, @arg1))),
	send_list(P, append,
		  [ menu_item(toggle_edit_mode, end_group := @on),
		    split_view,
		    menu_item(delete_other_views, end_group := @on),
		    menu_item(bold,
			      message(@arg1?device, make_selection, bold)),
		    menu_item(emphasize,
			      message(@arg1?device, make_selection, emphasize))
		  ]).


unlink(E) :->
	"Save if modified"::
	ignore(send(E, save_if_modified)),
	send(E, send_super, unlink).


file(E, File:file*) :<-
	"Return file of buffer"::
	get(E?text_buffer, file, File).


compute(E) :->
	"Honour renumber if needed"::
	(   get(E, request_renumber, @on)
	->  send(E, renumber),
	    send(E, request_renumber, @off)
	;   true
	),
	send(E, send_super, compute).


open(E) :->
	"Create a frame for the editor"::
	send(hlp_frame(E), open).
	

		 /*******************************
		 *	    SPLIT VIEW		*
		 *******************************/

split_view(E) :->
	"Split the current view in two"::
	get(E, device, V),
	send(E?frame, fixed_size, @on),
	new(E2, hlp_editor(E?text_buffer)),
	new(V2, view(editor := E2)),
	(   get(V, below, Below),
	    Below \== @nil
	->  send(V2, above, Below)
	;   true
	),
	send(V2, below, V),
	send(E2, caret, E?caret),
	send(E2, normalise).

delete_other_views(E) :->
	"Delete all views except for this one"::
	get(E, device, V),
	get(V, frame, Frame),
	send(Frame, fixed_size, @on),
	send(Frame?members, for_all,
	     if(and(message(@arg1, instance_of, view),
		    @arg1 \== V),
		and(message(Frame, delete, @arg1),
		    message(@arg1, destroy)))).


		 /*******************************
		 *	     EDIT MODE		*
		 *******************************/

toggle_edit_mode(E) :->
	"Toggle between edit and view mode"::
	(   get(E, editable, @on)
	->  send(E, editable, @off)
	;   send(E, editable, @on)
	),
	send(E, report, status, 'Edit mode is %N', E?editable).


		 /*******************************
		 *	      REGION		*
		 *******************************/

region(E, P:point) :<-
	"Get region (normalised)"::
	(   get(E, selection, point(Start, End)),
	    End > Start
	->  new(P, point(Start, End))
	;   get(E, mark, Mark),
	    get(E, caret, Caret),
	    (   Mark < Caret
	    ->  new(P, point(Mark, Caret))
	    ;   new(P, point(Caret, Mark))
	    )
	).

		 /*******************************
		 *	 SECTION STRUCTURE	*
		 *******************************/

make_header(E, Section:{section,subsection,subsubsection,subsubsubsection}) :->
	"Create section header of indicated type"::
	get(E, caret, Caret),
	get(E, scan, Caret, line, 0, start, Start),
	get(E, scan, Caret, line, 0, end, End),
	new(F, hlp_header(E, Start, End - Start, Section)),
	get(E, find_all_fragments,
	    and(message(@arg1, overlap, F),
		message(@arg1, instance_of, hlp_header),
		@arg1 \== F),
	    Frags),
	send(Frags, for_all, message(@arg1, free)),
	send(E, renumber).


make_section(E) :->
	"Turn current line into section-heading"::
	send(E, make_header, section).

make_subsection(E) :->
	"Turn current line into subsection-heading"::
	send(E, make_header, subsection).

make_subsubsection(E) :->
	"Turn current line into subsubsection-heading"::
	send(E, make_header, subsubsection).


make_subsubsubsection(E) :->
	"Turn current line into subsubsubsection-heading"::
	send(E, make_header, subsubsubsection).


open_line(E, Times:[int]) :->
	"Ensure headers are not expanded"::
	default(Times, 1, Tms),
	get(E, caret, Caret),
	(   get(E, find_fragment,
		and(message(@arg1, instance_of, hlp_header),
		    @arg1?start == Caret),
		Frag)
	->  get(Frag, start, S),
	    get(Frag, length, L),
	    send(E, send_super, open_line, Times),
	    send(Frag, start, S+Tms),
	    send(Frag, length, L)
	;   send(E, send_super, open_line, Times)
	).


newline(E, Times:[int]) :->
	"Ensure headers are not expanded"::
	get(E, caret, Caret),
	(   get(E, find_fragment,
		and(message(@arg1, instance_of, hlp_header),
		    @arg1?end == Caret),
		Frag)
	->  get(Frag, length, L),
	    send(E, send_super, newline, Times),
	    send(Frag, length, L)
	;   send(E, send_super, newline, Times)
	).


		 /*******************************
		 *	TABLES OF CONTENTS	*
		 *******************************/

make_table_of_contents(E) :->
	"Insert table of contents at point"::
	get(E, first_fragment, F),
	make_table_of_contents(E, F, @nil).

make_table_of_contents(_E, F, F) :- !.
make_table_of_contents(E, F, End) :-
	get(F, style, Name),
	(   indentation(Name, Indent)
	->  send(E, beginning_of_line),
	    send(E, open_line),
	    send(E, insert, F?string),
	    send(E, align_line, Indent),
	    link_contents_entry(E, F),
	    send(E, next_line)
	;   true
	),
	get(F, next, F1),
	make_table_of_contents(E, F1, End).

indentation(section, 8).
indentation(subsection, 10).
indentation(subsubsection, 12).
indentation(subsubsubsection, 14).

link_contents_entry(E, F) :-
	get(E, scan, E?caret, line, 0, start, SOL),
	new(R, regex('\\s *\\(.*\\)')),
	send(R, match, E, SOL),
	get(R, register_start, 1, CLS),
	get(R, register_end, 1, CLE),
	(   get(F, label, Label),
	    Label \== @nil
	->  true
	;   get(R, register_value, E, 1, string, Id),
	    send(Id, downcase),
	    send(Id, translate, ' ', '_'),
	    get(Id, value, Label),
	    send(F, label, Label)
	),
	new(Button, hlp_fragment(E, CLS, CLE-CLS, button)),
	send(Button, label, Label).


make_local_table_of_contents(E) :->
	"Make table of contents for chapter/section"::
	get(E, section_header_fragment, SectionHeader),
	SectionHeader \== @nil,
	get(E, next_section_header_fragment, SectionHeader, NextSectionHeader),
	get(SectionHeader, next, FirstFrag),
	make_table_of_contents(E, FirstFrag, NextSectionHeader).


section_header_fragment(E, F:hlp_fragment*) :<-
	"Find header fragment of this section"::
	get(E, first_fragment, F0),
	get(E, caret, Caret),
	section_header_fragment(F0, Caret, @nil, F).

section_header_fragment(F0, Caret, L, F) :-
	get(F0, start, S),
	S =< Caret, !,
	get(F0, style, Style),
	(   indentation(Style, _),
	    L1 = F0
	;   L1 = L
	),
	get(F0, next, F1), F1 \== @nil,
	section_header_fragment(F1, Caret, L1, F).
section_header_fragment(_, _, L, L).

next_section_header_fragment(_E, F0:hlp_fragment, F:hlp_fragment*) :<-
	"Find start of next fragment"::
	get(F0, style, Style),
	get(F0, next, F1),
	next_section_header_fragment(F1, Style, F).

next_section_header_fragment(F0, Style, F) :-
	get(F0, style, Style), !,
	F = F0.
next_section_header_fragment(F0, Style, F) :-
	get(F0, next, F1), F1 \== @nil,
	next_section_header_fragment(F1, Style, F).
next_section_header_fragment(_, _, @nil).


		 /*******************************
		 *	OTHER ENVIRONMENTS	*
		 *******************************/

make_selection(E, Style:name) :->
	"Turn selection in indicated style"::
	get(E, selection, point(Start, End)),
	new(_, hlp_fragment(E, Start, End - Start, Style)).


make_example(E) :->
	"Turn region into example"::
	get(E, region, point(Start, End)),
	new(_, hlp_fragment(E, Start, End - Start, example)).

make_title(E) :->
	"Turn region into title"::
	get(E, region, point(Start, End)),
	new(_, hlp_fragment(E, Start, End - Start, title)).

make_bold(E) :->
	"Turn region into bold"::
	get(E, region, point(Start, End)),
	new(_, hlp_fragment(E, Start, End - Start, bold)).

make_emphasized(E) :->
	"Turn region into emphasized"::
	get(E, region, point(Start, End)),
	new(_, hlp_fragment(E, Start, End - Start, emphasize)).

make_keyword(E) :->
	"Turn region into keyword"::
	get(E, region, point(Start, End)),
	new(_, hlp_fragment(E, Start, End - Start, keyword)).

default_link_destination(E, Dest:name) :<-
	"Label of fragment at mark"::
	(   get(E, mark, Mark),
	    get(E, find_all_fragments,
		and(message(@arg1, overlap, Mark),
		    @arg1?label \== @nil),
		Frags),
	    get(Frags, head, DestFrag)
	->  get(DestFrag, label, Dest)
	;   Dest = ''
	).

make_button(E) :->
	"Turn region into a button"::
	get(E, region, point(Start, End)),
	new(F, hlp_fragment(E, Start, End - Start, button)),
	get(E, default_link_destination, Dest),
	(   prompter('Make a button',
		     [ type:{label,prolog,manpce} = Type/label,
		       destination:name = Label/Dest
		     ])
	->  make_label(Type, Label, TheLabel),
	    send(F, label, TheLabel)
	;   send(F, free)
	).

make_label(label, Label, Label) :- !,
	(   Label == ''
	->  send(@nil, report, error, 'No label specified'),
	    fail
	;   true
	).
make_label(prolog, Goal, Label) :-
	(   term_to_atom(_Term, Goal)
	->  atom_concat('prolog://', Goal, Label)
	;   send(@nil, report, error, 'Syntax error in Prolog goal'),
	    fail
	).
make_label(manpce, Where, Label) :-
	atom_concat('manpce://', Where, Label).


		 /*******************************
		 *	       KEYWORDS		*
		 *******************************/

collect_keywords(E) :->
	"Collect all keywords and make buttons for them"::
	get(E, keyword_fragments, Frags),
	send(Frags, sort, ?(@arg1?string, compare, @arg2?string, @on)),
	new(N, number(0)),
	send(Frags, for_all,
	     and(new(H, var),
		 if(@arg1?label == @nil,
		    message(@arg1, label, N)),
		 message(E, beginning_of_line),
		 message(E, open_line),
		 assign(H, E?caret),
		 message(E, insert, @arg1?string),
		 message(create(hlp_fragment, E, H, E?caret - H, button),
			 label, @arg1?label),
		 message(E, next_line),
		 message(N, plus, 1))).


keyword_fragments(E, Frags:chain) :<-
	"Collect chain with all keyword fragments"::
	get(E, find_all_fragments, @arg1?style == keyword, Frags).

		 /*******************************
		 *	       ISPELL		*
		 *******************************/

%	Start the ispell tool on this buffer.  This used to be included
%	using a pce_autoload/2 declaration, but this forces ispell to
%	go into each stand-alone executable.  Hence this solution.

ispell(E) :->
	"Start ispell on this help-file"::
	(   absolute_file_name(demo(ispell),
			       [ file_type(prolog),
				 access(read)
			       ], _)
	->  ensure_loaded(demo(ispell))
	;   send(E, report, error, 'Cannot find demo(ispell)'),
	    fail
	),
	new(Ispell, ispell),
	get(E, text_buffer, TB),
	send(Ispell, buffer, TB),
	send(Ispell, label, string('Ispell Help buffer %s', E?file?name)),
	send(Ispell, open),
	send(Ispell, spell).


		 /*******************************
		 *	       GOTO		*
		 *******************************/


:- pce_global(@hlp_external_regex, new(regex('^\\(.+\\):\\(\\w+$\\)'))).
:- pce_global(@hlp_prolog_regex, new(regex('^prolog://\\(.*\\)$'))).
:- pce_global(@hlp_manpce_regex, new(regex('^manpce://\\(.*\\)$'))).

button(E, Button:hlp_fragment) :<-
	"Find button at caret"::
	get(E, fragment, @arg1?style == button, Button).


caret(E, Index:[int]) :->
	"Move the caret, but preview links"::
	send(E, send_super, caret, Index),
	(   get(E, button, Button)
	->  send(E, report, status, 'Goto "%s"?', Button?label)
	;   true
	).


follow(E) :->
	"Follow button"::
	get(E, button, Button),
	send(E, goto, Button?label, Button).


goto(E, Label:name, Button:[hlp_fragment]) :->
	"Goto named label and select it"::
	goto(E, Label, Button).

goto(E, Label, _) :-
	send(@hlp_prolog_regex, match, Label), !,
	get(@hlp_prolog_regex, register_value, Label, 1, name, GoalAtom),
	(   atom_to_term(GoalAtom, Goal, Bindings)
	->  (   user:Goal
	    ->  report_bindings(E, Bindings)
	    ;   send(E, report, status, 'Failed: %s', GoalAtom)
	    )
	;   send(E, report, error, 'Syntax error in %s', GoalAtom)
	).
goto(_, Label, _) :-
	send(@hlp_manpce_regex, match, Label), !,
	get(@hlp_manpce_regex, register_value, Label, 1, name, Target),
	auto_call(manpce(Target)).
goto(_, Label, _) :-
	send(@hlp_external_regex, match, Label), !,
	get(@hlp_external_regex, register_value, Label, 1, name, DB),
	get(@hlp_external_regex, register_value, Label, 2, name, Lbl),
	send(@helper, give_help, DB, Lbl).
goto(E, Label, _) :-
	get(E, find_all_fragments,
	    and(@arg1?style \== button,
		@arg1?label == Label),
	    Fragments),
	(   send(Fragments, empty)
	->  send(E, report, warning, 'Cannot find label %s', Label),
	    fail
	;   send(E, goto_fragment, Fragments?head)
	).

report_bindings(E, []) :- !,
	send(E, report, status, 'Yes').
report_bindings(E, Bindings) :- !,
	new(S, string),
	forall(member(N=V, Bindings),
	       (   (   get(S, size, 0)
		   ->  true
		   ;   send(S, append, ', ')
		   ),
		   sformat(Str, '~w = ~p', [N, V]),
		   send(S, append, Str)
	       )),
	send(S, translate, 10, 32),	% newlines to spaces
	send(E, report, status, S).

goto_fragment(E, Frag:fragment) :->
	"Make fragment the current one"::
	send(E, caret, Frag?start),
	send(E, normalise, Frag?start, Frag?end),
	send(E, selection, Frag?start, Frag?end).


		 /*******************************
		 *	  DELETE FRAGMENT	*
		 *******************************/

delete_fragment(E) :->
	"Delete fragment at caret (making normal text)"::
	get(E, caret, Caret),
	get(E, find_all_fragments,
	    message(@arg1, overlap, Caret),
	    Fragments),
	send(Fragments, sort, ?(@arg1?length, compare, @arg1?length)),
	get(Fragments, find,
	    and(message(E?display, confirm,
			'Delete %s fragment', @arg1?style)),
	    F),
	free(F).


		 /*******************************
		 *	   WHAT FRAGMENT	*
		 *******************************/

what_fragments(E) :->
	"Inform fragments around caret"::
	get(E, caret, Caret),
	get(E, find_all_fragments,
	    message(@arg1, overlap, Caret),
	    Fragments),
	new(S, string),
	send(Fragments, for_all,
	     message(S, append,
		     create(string, '"%s" fragment "%s" holding "%s"\n',
			    @arg1?style, @arg1?label, @arg1?string))),
	send(@display, inform, 'Fragments around point:\n\n%s', S).


		 /*******************************
		 *     LABELS AND REFERENCES	*
		 *******************************/

fragment(E, Cond:[code], Fragment:fragment) :<-
	"Find smallest fragment at caret"::
	default(Cond, new(and), C),
	get(E, caret, Caret),
	get(E, find_all_fragments,
	    and(message(@arg1, overlap, Caret), C),
	    Fragments),
	send(Fragments, sort, ?(@arg1?length, compare, @arg2?length)),
	get(Fragments, head, Fragment).

	
assign_label(E) :->
	"Edit/assign label to fragment below caret"::
	get(E, fragment, Fragment),
	get(Fragment, label, Old),
	( Old == @nil -> Def = '' ; Def = Old ),
	prompter('Label for fragment',
		 [ label:name = Label/Def
		 ]),
	send(Fragment, label, Label),
	send(E, modified, @on),
	send(E, report, status, 'Label assigned').
		 

		 /*******************************
		 *	      LOAD/SAVE		*
		 *******************************/

load(E, File:file) :->
	"Load file into editor"::
	(   send(File, exists)
	->  (   send(File, check_object)
	    ->  get(File, object, Obj),
		(	send(Obj, instance_of, text_buffer)
		->	send(E, text_buffer, Obj)
		;	send(E, report, error, "File is not a hyper-file"),
			fail
		)
	    ;   send(E, send_super, load, File)
	    )
	;   true
	),
	send(E?text_buffer, file, File).
	

save_if_modified(E) :->
	"Save to current file if modified"::
	(   get(E, modified, @off)
	->  true
	;   send(@display, confirm, 'Save help-file "%s"?', E?file?name),
	    send(E, save)
	).


save(E, File:[file]) :->
	"Save in named or default file"::
	(   get(E, modified, @off)
	->  send(E, report, status, 'No changes need saving')
	;   (   File \== @default
	    ->  TheFile = File
	    ;   get(E, file, TheFile), TheFile \== @nil
	    ->  true
	    ;   get(E?text_buffer, file, TheFile), TheFile \== @nil
	    ->  true
	    ;   send(E, report, error, 'No File'),
		fail
	    ),
	    ignore(send(TheFile, backup)),
	    send(E?text_buffer, save_in_file, TheFile),
	    send(E?text_buffer, file, TheFile),
	    send(E, report, status, 'Saved in %N', TheFile),
	    send(E, modified, @off)
	).
	

insert_file(E) :->
	"Insert file at point"::
	get(@finder, file, @on, File),
	send(E?text_buffer, insert_file, 0, File).


		 /*******************************
		 *		HELP		*
		 *******************************/

show_key_bindings(E) :->
	"Show current key-binding table"::
	auto_call(show_key_bindings(E)).

:- pce_end_class.

		 /*******************************
		 *	      FRAME		*
		 *******************************/

:- pce_begin_class(hlp_frame, frame, "Frame holding help-editor").

variable(editor,	editor,		get,  "Editor of the system").
variable(fixed_size,	bool := @off,	both, "Donot resize on ->fit").

delegate_to(editor).

initialise(F, Data:'[file|hlp_buffer|hlp_editor]') :->
	send(F, send_super, initialise, 'Help View'),
	(   send(Data, instance_of, hlp_editor)
	->  E = Data
	;   new(E, hlp_editor(Data))
	),
	send(F, icon, resource(help)),
	send(F, append, new(V, view(editor := E))),
	send(F, slot, editor, E),
	send(new(D, dialog), above, V),
	send(D, append, new(MB, menu_bar)),
	send(D, gap, size(0,0)),
	send(D, append, graphical(0,0,20,1), right),
	send(D, append, label(reporter), right),
	new(CV, F?current_view),
	new(CanEdit, condition := (CV?editable == @on)),
	send(MB, append, new(Fp, popup(file, message(CV, @arg1)))),
	send(MB, append, new(Ep, popup(edit, message(CV, @arg1)))),
	send_list(Fp, append,
		  [ menu_item(help,
			      message(@helper, give_help,
				      help, reading)),
		    menu_item(show_key_bindings,
			      message(CV, show_key_bindings),
			      end_group := @on),
		    menu_item(split_view),
		    menu_item(delete_other_views, end_group := @on),
		    menu_item(toggle_edit_mode, end_group := @on),
		    menu_item(quit, message(F, destroy))
		  ]),
	send_list(Ep, append,
		  [ menu_item(make_title, end_group := @on, CanEdit),

		    menu_item(make_section, CanEdit),
		    menu_item(make_subsection, CanEdit),
		    menu_item(make_subsubsection, CanEdit),
		    menu_item(make_subsubsubsection,
			      end_group := @on, CanEdit),

		    menu_item(make_keyword, end_group := @on, CanEdit),

		    menu_item(make_example, CanEdit),
		    menu_item(make_emphasized, CanEdit),
		    menu_item(make_bold, end_group := @on, CanEdit),

		    menu_item(assign_label, CanEdit),
		    menu_item(make_button, CanEdit),
		    menu_item(delete_fragment, end_group := @on, CanEdit),
		    
		    menu_item(make_table_of_contents, CanEdit),
		    menu_item(make_local_table_of_contents, CanEdit),
		    menu_item(collect_keywords, end_group := @on, CanEdit),
		    
		    menu_item(what_fragments, end_group := @on),

		    menu_item(save, CanEdit),
		    menu_item(insert_file, end_group := @on, CanEdit),

		    menu_item(ispell)
		  ]).


current_view(F, View:view) :<-
	"Return the `current' view in the frame"::
	(   get(F?members, find,
		and(message(@arg1, instance_of, view),
		    ?(@arg1, selection)),
		View)
	->  true
	;   get(F, member, view, View)
	).


fit(F) :->
	"Request to fit the contents"::
	(   get(F, fixed_size, @on)
	->  send(F, resize)
	;   send(F, send_super, fit)
	).

:- pce_end_class.


:- initialization pce_help_file(help, pce_help('help.hlp')).
