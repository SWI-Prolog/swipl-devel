/*  language_mode.pl,v 1.5 1993/05/06 10:13:14 jan Exp

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

:- module(emacs_language_mode, []).
:- use_module(library(pce)).
:- require([ auto_call/1
	   , chain_list/2
	   , default/3
	   , ignore/1
	   , member/2
	   ]).

:- emacs_begin_mode(language, fundamental,
		    "Edit (programming) languages",
	[ indent_line			= key('TAB'),
	  backward_delete_char_untabify	= key(backspace),
	  align_close_bracket		= key(']') + key('}') + key(')'),
	  insert_section_header		= key('\eh'),
	  insert_comment_block		= key('\C-c\C-q'),
	  insert_line_comment		= key('\e;'),
	  find_tag			= key('\e.') + button(browse)
	],
	[ '"'  = string_quote('"'),
	  '''' = string_quote(''''),
	  paragraph_end(regex('\s *$\|/\* - - -\|- - -.*\*/$'))
	]).

:- pce_autoload(emacs_tag_item, library(emacs_tags)).

variable(comment_column,	int,	both, "Column for line comment").

initialise(M) :->
	"Inititialise comment_column"::
	send(M, send_super, initialise),
	send(M, comment_column, @emacs_comment_column).


		 /*******************************
		 *	COMMENT; HEADERS	*
		 *******************************/

line_comment(E, CS:name) :<-
	"Fetch the line-comment start sequence"::
	get(E, syntax, Syntax),
	member(CSlen, [1, 2]),
	get(Syntax, comment_start, CSlen, CS),
	get(Syntax, comment_end, CSlen, CE),
	send(CE, equal, string('\n')).

insert_line_comment(E) :->
	"Insert (line) comment"::
	member(CSlen, [1, 2]),
	get(E?syntax, comment_start, CSlen, CS), !,
	get(E, caret, Caret),
	get(E, text_buffer, TB),
	get(TB, scan, Caret, line, 0, start, SOL),
	get(TB, scan, Caret, line, 0, end,   EOL),
	(   get(regex(?(regex(''), quote, CS)), search, TB, SOL, EOL, Start)
	->  send(E, caret, Start),
	    send(E, align, E?comment_column),
	    send(E, forward_char, CSlen + 1)
	;   send(E, end_of_line),
	    send(E, just_one_space),
	    send(E, align, E?comment_column),
	    get(E?syntax, comment_end, CSlen, CE),
	    (	send(CE, equal, string('\n'))
	    ->  send(E, format, '%s ', CS)
	    ;   send(E, format, '%s  %s', CS, CE),
		send(E, backward_char, CSlen + 1)
	    )
	).


comment_region(E) :->
	"Toggle-Comment the region using line-comments"::
	get(E, line_comment, Comment),
	get(E, region, tuple(Start, End)),
	get(E, text_buffer, TB),
	get(TB, scan, Start, line, 0, start, S0),
	comment_lines(TB, S0, End, Comment).

comment_lines(_TB, S0, End, _Comment) :-
	S0 >= End, !.
comment_lines(TB, S0, End, Comment) :-
	atom_length(Comment, L),
	(   get(regex(''), quote, Comment, RE),
	    send(regex(RE), match, TB, S0)
	->  send(TB, delete, S0, L),
	    S1 = S0,
	    End1 is End - L
	;   send(TB, insert, S0, Comment),
	    S1 is S0 + L,
	    End1 is End + L
	),
	(   get(TB, scan, S1, line, 1, start, S2)
	->  comment_lines(TB, S2, End1, Comment)
	;   true
	).
	

insert_comment_block(E) :->
	"Insert header/footer for long comment"::
	send(E, insert,
'/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
'),
	send(E, previous_line, 2).


insert_section_header(E) :->
	"Insert Prolog/C section header"::
	send(E, insert, 
'		 /*******************************
		 *               C		*
		 *******************************/
').


		 /*******************************
		 *          INDENTATION		*
		 *******************************/

indent_line(E) :->
	"Indent current line"::
	send(E, beginning_of_text_on_line),
	(   send(E, indent_close_bracket_line)
	;   send(E, indent_expression_line)
	;   send(E, align_with_previous_line)
	).


indent_close_bracket_line(E, Brackets:[name], Base:[int]) :->
	"Indent a line holding a bracket"::
	default(Brackets, ')}]', B1),
	get(E, text_buffer, TB),
	get(E, caret, Caret),
	get(TB, character, Caret, Char),
	get(B1, index, Char, _),
	get(TB, matching_bracket, Caret, OpenPos),
	(   Base \== @default
	->  OpenPos >= Base
	;   true
	),
	get(E, column, OpenPos, Col),
	send(E, align_line, Col).


indent_expression_line(E, Brackets:[name], Base:[int]) :->
	"Indent current line according to expression"::
	default(Brackets, ')}]', B1),
	atom_chars(B1, B2),
	get(E, text_buffer, TB),
	member(Bracket, B2),
	    pce_catch_error(mismatched_bracket,
			    get(TB, matching_bracket, E?caret,
				Bracket, OpenPos)), !,
	    (	Base \== @default
	    ->  OpenPos >= Base
	    ;	true
	    ),
	    get(TB, scan, OpenPos, line, 0, end, EOL),
	    (	send(E, looking_at, '[,|]')
	    ->	get(E, column, OpenPos, Col)
	    ;   get(TB, skip_comment, OpenPos+1, EOL, P1),
		get(E, column, P1, Col)
	    ),
	    send(E, align_line, Col), !.

		
		/********************************
		*           ALIGNMENT		*
		********************************/

align_with_previous_line(E, Leading:[regex]) :->
	"Align current_line with the one above"::
	get(E, caret, Caret),
	get(E, scan, Caret, line, -1, start, LineStart),
	get(E, scan, Caret, term, -1, start, TermStart),
	(   TermStart < LineStart
	->  get(E, indentation, TermStart, Leading, Indent)
	;   get(E, indentation, LineStart, Leading, Indent)
	),
	send(E, align_line, Indent).


align_close_bracket(E, Times:[int], Id:[event_id]) :->
	"Insert and align with matching open bracket"::
	send(E, insert_self, Times, Id),
	get(E, caret, Caret),
	get(E, scan, Caret, line, 0, start, SOL),
	get(E, scan, Caret, line, 0, end,   EOL),
	(   get(E, skip_comment, SOL, EOL, P0),
	    Caret =:= P0+1
	->  get(E, matching_bracket, Caret-1, Open),
	    get(E, column, Open, Col),
	    send(E, caret, Caret),
	    send(E, align, Col, Caret-1)
	;   true
	).


		 /*******************************
		 *           UNTABIFY		*
		 *******************************/

backward_delete_char_untabify(M, Times:[int]) :->
	"Delete characters backward"::
	get(M, caret, Caret),
	get(M, character, Caret-1, Char),
	(   send(M?syntax, has_syntax, Char, white_space)
	->  get(M, column, Col),
	    default(Times, 1, Tms),
	    send(M, align, Col-Tms),
	    (	get(M, column, Col)
	    ->	send(M, backward_delete_char, Times)
	    ;	true
	    )
	;   send(M, backward_delete_char, Times)
	).


		 /*******************************
		 *	      TAGS		*
		 *******************************/

visit_tag_table(M, Table:tag_file='file|directory') :->
	"Load specified GNU-Emacs (etags) tag-table"::
	(   send(Table, instance_of, directory)
	->  find_tag_from_dir(Table, TagFile)
	;   TagFile = Table
	),
	get(TagFile, absolute_path, TagFileName),
	(   send(TagFile, access, read)
	->  auto_call(emacs_init_tags(TagFileName)),
	    send(M, report, status, 'Loaded TAG table %s', TagFileName)
	;   send(M, report, warning, '%s: not accessible', TagFileName),
	    fail
	).

find_tag_from_dir(Dir, File) :-
	get(Dir, file, 'TAGS', File),
	send(File, exists), !.
find_tag_from_dir(Dir, File) :-
	get(Dir, parent, Parent),
	find_tag_from_dir(Parent, File).

expand_tag(M, Tag:[name], TheTag:name) :<-
	"Expand tag using tag-table"::
	(   auto_call(emacs_tag_file(_))
	->  true
	;   get(M, directory, Dir),
	    (	send(?(Dir, file, 'TAGS'), exists)
	    ->	get(Dir, path, Path),
		auto_call(emacs_init_tags(Path))
	    ;	send(M, noarg_call, visit_tag_table)
	    )
	),
	(   Tag == @default
	->  get(M, word, DefTag),
	    new(I, emacs_tag_item('Find tag', DefTag)),
	    get(M, prompt_using, I, TagString),
	    get(TagString, value, TheTag)
	;   TheTag = Tag
	).


find_tag(M, Tag:[name], Editor:editor) :<-
	"Jump to indicated tag entry"::
	get(M, expand_tag, Tag, TheTag),
	(   auto_call(emacs_tag(TheTag, File, Line)),
	    new(B, emacs_buffer(File)),
	    send(B, open),
	    get(B?editors, head, Editor),
	    send(Editor, line_number, Line),
	    adjust_tag(Editor, TheTag)
	;   send(M, report, warning, 'Cannot find tag %s', TheTag),
	    fail
	).


find_tag(M, Tag:[name]) :->
	"Jump to indicated tag entry"::
	ignore(get(M, find_tag, Tag, _)). % avoid delegation to menu-bar


adjust_tag(E, Tag) :-
	get(E, text_buffer, TB),
	get(E, caret, Here),
	new(Re, regex('')),
	get(Re, quote, Tag, QTag),
	(   send(Re, pattern, string('\\b%s\\b', QTag))
	;   send(Re, pattern, string('\\b%s', QTag))
	),
	closest(Re, TB, Here, Pos), !,
	send(E, caret, Pos).


closest(Re, TB, Here, Pos) :-
	get(Re, search, TB, Here, P1), !,
	(   get(Re, search, TB, Here, 0, P2)
	->  closest_element(P1, P2, Here, Pos)
	;   Pos = P1
	).
closest(Re, TB, Here, Pos) :-
	get(Re, search, TB, Here, 0, Pos).

closest_element(A, B, Here, A) :-
	abs(A-Here) =< abs(B-Here), !.
closest_element(A, B, Here, B) :-
	abs(B-Here) < abs(A-Here).


		 /*******************************
		 *         MISCELLANEOUS	*
		 *******************************/

beginning_of_text_on_line(E) :->
	"Position caret at first non-white on line"::
	get(E, caret, Caret),
	get(E, scan, Caret, line, 0, start, SOL),
	get(E, scan, Caret, line, 0, end,   EOL),
	get(E, skip_comment, SOL, EOL, P0),
	send(E, caret, P0).


		 /*******************************
		 *	      HELP		*
		 *******************************/

pce_ifhostproperty(prolog(swi),
[ (make_prolog_help_topic :-
	object(@prolog_help_topic_type), !),
  (make_prolog_help_topic :-
	new(Topics, quote_function(@prolog?help_topics)),
	new(@prolog_help_topic_type,
	    type(prolog_help_topic, value_set, Topics))),

  (:- initialization make_prolog_help_topic),

  (help_topics(@prolog_help_topics) :-
	object(@prolog_help_topics), !),
  (help_topics(@prolog_help_topics) :-
	setof(Name, prolog_help_topic(Name), Names),
	chain_list(Chain, [''|Names]),
	send(Chain, name_reference, prolog_help_topics)),

  (do_help(M, What) :-
	(   What == ''
	->  help
	;   help(What)
	))
],
[ (:- initialization get(type(name), copy, prolog_help_topic, _)),

  (do_help(M, What) :-
	send(M, report, warning, 'No manual interface for this Prolog'))
]).

prolog_manual(M, What:prolog_help_topic) :->
	"Display section from the Prolog manual"::
	do_help(M, What).

:- emacs_end_mode.

