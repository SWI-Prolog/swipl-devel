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
	  insert_file_header		= key('\\C-c\\C-f'),
	  insert_section_header		= key('\\eh'),
	  insert_comment_block		= key('\\C-c\\C-q'),
	  insert_line_comment		= key('\\e;'),
	  find_tag			= key('\\e.') + button(browse)
	],
	[ '"'  = string_quote('"'),
	  '''' = string_quote(''''),
	  paragraph_end(regex('\\s *$\\|/\\* - - -\\|- - -.*\\*/$'))
	]).


variable(comment_column,	int,	both, "Column for line comment").
variable(show_line_numbers,	[bool], get,  "Show line numbers?").

setup_mode(E) :->
	"Switch editor into fill-mode"::
	send_super(E, setup_mode),
	send(E, fill_mode, @on).

initialise(M) :->
	"Inititialise comment_column"::
	send(M, send_super, initialise),
	send(M, slot, show_line_numbers, @default),
	send(M, comment_column, @emacs_comment_column).


		 /*******************************
		 *	COMMENT; HEADERS	*
		 *******************************/

:- pce_group(comment).

line_comment(E, CS:name) :<-
	"Fetch the line-comment start sequence"::
	get(E, syntax, Syntax),
	member(CSlen, [1, 2]),
	get(Syntax, comment_start, CSlen, CS),
	get(Syntax, comment_end, CSlen, CE),
	send(CE, equal, '\n').

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
	    (	send(CE, equal, '\n')
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
	

fill_comment_paragraph(M, Justify:justify=[bool|int], From:[int]) :->
	"Fill paragraph in (line) comment"::
	(   get(M?syntax, comment_start, 1, CS)
	->  true
	;   send(M, report, warning, 'No line-comment character defined'),
	    fail
	),
	new(Re, regex(string('^%s?[ \t]*$', CS))),
	new(LeadRe, regex(string('%s[ \t]*', CS))),
	get(M, caret, Caret),
	get(M, text_buffer, TB),
	(   From \== @default
	->  Start = From
	;   get(Re, search, TB, Caret, 0, StartPar),
	    get(TB, scan, StartPar, line, 1, start, Start)
	->  true
	;   Start = 0
	),
	(   get(Re, search, TB, Start, End)
	->  true
	;   get(TB, size, End)
	),
	free(Re),
	send(M, fill_comment, Start, End, LeadRe, Justify),
	free(LeadRe).

%	->fill_comment
%
%	Fill a region using a regex that defines leading comment

fill_comment(M,
	     Start:from=int, End:to=int,
	     Re:leading=regex, Justify:justify=[bool|int]) :->
	"Fill paragraph given `leading' regex"::
	(   (Justify == @default ; Justify == @off ; Justify == 0)
	->  TheJustify = @off
	;   TheJustify = @on
	),
	get(M, text_buffer, TB),
	get(M, caret, Caret),
	new(CaretF, fragment(TB, Caret, 0)),
	new(EndF, fragment(TB, End, 0)),
	get(Re, match, TB, Start, LeadChars),
	get(TB, contents, Start, LeadChars, Lead),
	LeadEnd is Start + LeadChars,
	get(M, column, LeadEnd, LeadCol),
	uncomment(M, Re, LeadCol, Start, EndF),
	get(M, right_margin, RM0),
	RM is RM0 - LeadCol,
	send(M, fill, Start, EndF?start, 0, RM, TheJustify),
	comment(M, Start, EndF, Lead, LeadCol),
	send(M, caret, CaretF?start),
	free(EndF),
	free(CaretF).
	
uncomment(_M, _Re, _LeadCol, Here, EndF) :-
	get(EndF, start, End),
	Here >= End, !.
uncomment(M, Re, LeadCol, Here, EndF) :-
	get(M, text_buffer, TB),
	get(Re, match, TB, Here, Len),
	send(M, caret, Here),
	send(M, column, LeadCol),
	get(M, caret, AtLeadCol),
	DelLen is min(Len, AtLeadCol-Here),
	send(TB, delete, Here, DelLen),
	get(TB, scan, Here, line, 1, start, NextHere),
	uncomment(M, Re, LeadCol, NextHere, EndF).

comment(_, Here, EndF, _, _Col) :-
	get(EndF, start, End),
	Here >= End, !.
comment(M, Here, EndF, Lead, Col) :-
	get(M, text_buffer, TB),
	send(TB, insert, Here, Lead),
	get(Lead, size, Size),
	send(M, align, Col, Here+Size),
	get(TB, scan, Here, line, 1, start, NextHere),
	comment(M, NextHere, EndF, Lead, Col).


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
		 *	    FILE HEADER		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
->insert_file_header
	Inserts a .fileheader it finds in the current directory or one of
	its parent directories.  If no file it found it uses a default.

	Next it makes the substitutions from file_header_parameter/3 and
	finally, if the header contains %. it removes this and sets the
	caret at this position.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

insert_file_header(M) :->
	"Insert .fileheader or default file-header"::
	get(M, directory, Dir),
	find_file_header(Dir, Header),
	(   file_header_parameter(Escape, M, Value),
	    substitute(Header, Escape, Value),
	    fail
	;   true
	),
	(   new(Here, regex('%\\.')),
	    send(Here, search, Header)
	->  send(Here, register_value, Header, ''),
	    get(Here, register_start, Offset),
	    get(M, caret, Caret),
	    send(M, insert, Header),
	    send(M, caret, Caret+Offset)
	;   send(M, insert, Header)
	).


find_file_header(Dir, Header) :-
	get(Dir, file, '.fileheader', File),
	send(File, access, read), !,
	send(File, open, read),
	get(File, read, Header),
	send(File, close).
find_file_header(Dir, Header) :-
	get(Dir, parent, Parent),
	find_file_header(Parent, Header).
find_file_header(_, Header) :-
	new(Header, string),
	send_list(Header, append,
		  [ '/*  File:    %F\n',
		    '    Author:  %U\n',
		    '    Created: %D\n',
		    '    Purpose: %.\n',
		    '*/\n\n'
		  ]).

file_header_parameter('%Y', _,  Year) :-
	get(new(date), year, Year).
file_header_parameter('%F', M, FileName) :-
	get(M?file, base_name, FileName).
file_header_parameter('%U', _, UserName) :-
	get(@pce, user, User),
	(   get(@pce, user_info, gecos, User, UserName)
	->  true
	;   UserName = User
	).
file_header_parameter('%D', _, Date) :-
	new(D, date),
	get(D, year, Year),
	get(D, month_name, @on, Month),
	get(D, day, Day),
	new(Date, string('%s %2d %d', Month, Day, Year)).

substitute(String, From, To) :-
	send(regex(From), for_all, String,
	     message(@arg1, replace, @arg2, To)).


		 /*******************************
		 *	       FILLING		*
		 *******************************/

:- pce_group(fill).


fill_paragraph(M, Justify:[int]) :->
	"Fill comment paragraph"::
	get(M, caret, Caret),
	(   (   get(M, scan_syntax, 0, Caret, tuple(comment, Start))
	    ->  get(M, column, Start, 0)
	    ;	get(M, column, Caret, 0),
		Start = Caret
	    ),
	    get(M?syntax, comment_start, 1, CS),
	    send(M, looking_at, CS, Start)
	->  send(M, fill_comment_paragraph, Justify)
	;   send_super(M, fill_paragraph, Justify)
	).


justify_paragraph(M) :->
	"->fill_paragraph using right-margin"::
	send(M, fill_paragraph, 1).


auto_fill(M, From:[int]) :->
	"Auto fill in comment mode"::
	(   From == @default
	->  get(M, caret, Caret)
	;   Caret = From
	),
	(   get(M, scan_syntax, 0, Caret, tuple(comment, Start)),
	    get(M, column, Start, 0)
	->  (	get(M?syntax, comment_start, 1, CS),
	        send(M, looking_at, CS, Start)
	    ->	send(M, fill_comment_paragraph, @off, Start)
	    ;	get(M, editor, Editor),
	        send_class(Editor, editor, auto_fill(Caret))
	    )
	).


		 /*******************************
		 *          INDENTATION		*
		 *******************************/

:- pce_group(indent).

newline_and_indent(E, Arg:[int]) :->
	"Insert newline and indent as TAB"::
	send(E, newline, Arg),
	send(E, indent_line).


indent_line(E) :->
	"Indent current line"::
	send(E, beginning_of_text_on_line),
	(   send(E, indent_close_bracket_line)
	;   send(E, indent_expression_line)
	;   send(E, indent_comment_line)
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
	atom_codes(B1, B2),
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

		
%	->indent_comment_line
%


indent_comment_line(M) :->
	"Copy leading comment of previous line"::
	get(M, text_buffer, TB),
	get(TB?syntax, comment_start, 1, CS),
	get(M, caret, Caret),
	get(M, scan, Caret, line, -1, start, SOPL),
	new(LeadRe, regex(string('%s[ \t]*', CS))),
	get(LeadRe, match, TB, SOPL, Len), 	% Previous holds comment
	get(M, scan, Caret, line, 0, start, SOL),
	send(M, looking_at, string('%s?[ \t]*$', CS), SOL),
	get(TB, contents, SOPL, Len, Lead),
	get(M, scan, SOL, line, 0, end, EOL),
	send(TB, delete, SOL, EOL-SOL),
	send(M, insert, Lead).


		/********************************
		*           ALIGNMENT		*
		********************************/

alignment_of_previous_line(E, Leading:[regex], Indent:int) :<-
	"Find the indentation of the previous line"::
	get(E, caret, Caret),
	get(E, scan, Caret, line, -1, start, LineStart),
	get(E, scan, Caret, term, -1, start, TermStart),
	(   TermStart < LineStart
	->  get(E, indentation, TermStart, Leading, Indent)
	;   get(E, indentation, LineStart, Leading, Indent)
	).


align_with_previous_line(E, Leading:[regex]) :->
	"Align current_line with the one above"::
	get(E, alignment_of_previous_line, Leading, Indent),
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

:- initialization
   pce_define_type(emacs_tag, name).

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

ensure_loaded_tags(M) :->
	"Make sure we have a tag-table loaded"::
	(   auto_call(emacs_tag_file(_))
	->  true
	;   get(M, directory, Dir),
	    (	send(?(Dir, file, 'TAGS'), exists)
	    ->	get(Dir, path, Path),
		auto_call(emacs_init_tags(Path))
	    ;	send(M, noarg_call, visit_tag_table)
	    )
	).

default_tag(M, DefTag:name) :<-
	"Return default tag from current word"::
	(   get(M, word, Word),
	    send(regex('[a-zA-Z0-9_]*$'), match, Word)
	->  DefTag = Word
	;   DefTag = ''
	).


expand_tag(M, Tag:[name], TheTag:name) :<-
	"Expand tag using tag-table"::
	send(M, ensure_loaded_tags),
	(   Tag == @default
	->  get(M, word, DefTag),
	    new(I, emacs_tag_item('Find tag', DefTag)),
	    get(M, prompt_using, I, TagString),
	    get(TagString, value, TheTag)
	;   TheTag = Tag
	).


find_tag(M, Tag:emacs_tag, Editor:editor) :<-
	"Jump to indicated tag entry"::
	(   auto_call(emacs_tag(Tag, File, Line)),
	    new(B, emacs_buffer(File)),
	    send(B, open),
	    get(B?editors, head, Editor),
	    send(Editor, line_number, Line),
	    adjust_tag(Editor, Tag)
	;   send(M, report, warning, 'Cannot find tag %s', Tag),
	    fail
	).


find_tag(M, Tag:emacs_tag) :->
	"Jump to entry from TAG table"::
	ignore(get(M, find_tag, Tag, _)). % avoid delegation to menu-bar


adjust_tag(E, Tag) :-
	get(E, text_buffer, TB),
	get(E, caret, Here),
	new(Re, regex('')),
	get(Re, quote, Tag, QTag),
	(   send(Re, pattern, string('\\\\b%s\\\\b', QTag))
	;   send(Re, pattern, string('\\\\b%s', QTag))
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
	(   get(E?syntax, comment_start, 1, CS),
	    new(LeadRe, regex(string('%s[ \t]*', CS))),
	    get(E, text_buffer, TB),
	    get(LeadRe, match, TB, SOL, Len)
	->  AtText is SOL+Len,
	    send(E, caret, AtText)
	;   get(E, scan, Caret, line, 0, end,   EOL),
	    get(E, skip_comment, SOL, EOL, P0),
	    send(E, caret, P0)
	).


new_caret_position(M, Caret:int) :->
	"Update line number"::
	send_super(M, new_caret_position, Caret),
	(   get(M, frame, Frame),
	    send(Frame, has_send_method, show_line_number),
	    (	get(M, show_line_numbers, @off)
	    ->	send(Frame, show_line_number, @nil)
	    ;   (   (   get(M, show_line_numbers, @default)
		    ->  Caret < 50000
		    ;   get(M, show_line_numbers, @on)
		    )
		->  get(M, line_number, Line),
		    send(Frame, show_line_number, Line)
		;   send(Frame, show_line_number, too_expensive)
		)
	    ;	true
	    )
	;   true
	).

show_line_numbers(M, Show:bool) :->
	"Show/do not show line numbers"::
	send(M, slot, show_line_numbers, Show),
	send(M, new_caret_position, M?caret).


		 /*******************************
		 *	      HELP		*
		 *******************************/

prolog_manual(_, On:[name]) :->
	"Open Prolog manual"::
	(   On == @default
	->  help
	;   help(On)
	).

:- emacs_end_mode.

