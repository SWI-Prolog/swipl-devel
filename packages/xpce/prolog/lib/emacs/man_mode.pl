/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

:- module(emacs_man_mode, []).
:- use_module(library(pce)).
:- require([ member/2
	   ]).

:- pce_begin_class(emacs_man_mode, emacs_fundamental_mode).

:- initialization
	new(KB, emacs_key_binding(man, fundamental)),
	send(KB, function, '\C-c\C-f',	goto_man_page).

:- initialization
	new(X, syntax_table(man)),
	send(X, syntax, '"',  string_quote, '\'),
	send(X, syntax, '''', string_quote, '\'),

	send(X, syntax,     '#',  comment_start),
	send(X, add_syntax, '\n', comment_end).

:- initialization
	new(MM, emacs_mode_menu(man, fundamental)),
	send(MM, append, man, goto_man_page).

		 /*******************************
		 *	       SETUP		*
		 *******************************/

setup_mode(M) :->
	"Associate bold and underline styles"::
	send(M, style, underline, style(underline := @on)),
	send(M, style, bold, style(bold := @on)).


		 /*******************************
		 *	       CLICKS		*
		 *******************************/

:- pce_global(@event_mode, new(@event?receiver?window?editor?mode)).
:- pce_global(@emacs_man_recogniser,
	      new(click_gesture(left, '', double,
				message(@event_mode, goto_man_page)))).

event(_M, Ev:event) :->
	"Connect double-click to goto_man_page"::
	send(@emacs_man_recogniser, event, Ev).


		 /*******************************
		 *	 FIND MANUAL PAGE	*
		 *******************************/

man(M, Spec:name) :->
	"Switch to given manual page"::
	get(M, text_buffer, TB),
	(   get(Spec, scan, '%s %s', vector(Section, Page))
	;   get(Spec, scan, '%[a-z](%[^)]', vector(Page, Section))
	;   Page = Spec
	),
	(   member(Section, [1, 2, 3, 4, 5, 6, 7, 8, 9, l]),
	    new(File, file(string('cat%s/%s.%s', Section, Page, Section))),
	    (	get(@pce, environment_variable, 'MANPATH', Path)
	    ->	true
	    ;	Path = '/usr/man'
	    ),
	    pce_catch_error(cannot_find_file, send(File, find, Path)), !,
	    send(TB, name, string('%s(%s)', Page, Section)),
	    send(M, clear),
	    send(M, insert_file, File),
	    send(M, caret, 0),
	    send(M, clean)
	;   (	var(Section)
	    ->	new(P, process(man, Page)),
		send(TB, name, string('%s', Page)),
		send(M, report, status, 'Running man %s ...', Page)
	    ;	(   get(@pce, operating_system, solaris)
		->  new(P, process(man, '-s', Section, Page)),
		    send(TB, name, string('%s(%s)', Page, Section)),
		    send(M, report, status, 'Running man -s %s %s ...',
			 Section, Page)
		;   new(P, process(man, Section, Page)),
		    send(TB, name, string('%s(%s)', Page, Section)),
		    send(M, report, status, 'Running man %s %s ...',
			 Section, Page)
		)
	    ),
	    send(P, use_tty, @off),
	    send(P, record_separator, @nil),
	    send(P, input_message,
		 and(message(TB, append, @arg1),
		     message(M, caret, 0))),
	    send(P, send_method,
		 send_method(end_of_file, new(vector),
			     message(M, clean))),
	    send(M, clear),
	    send(P, open)
	).


:- pce_global(@emacs_man_underline_fragment_regex,
	      new(regex(string('\\(_\b.\\)+\\|\\(.\b_\\)+')))).
:- pce_global(@emacs_man_bold_fragment_regex,
	      new(regex(string('\\(.\b.\\)+')))).
:- pce_global(@emacs_man_underline_regex,
	      new(regex(string('_\b\\|\b_')))).
:- pce_global(@emacs_man_bold_regex,
	      new(regex(string('\b.')))).
:- pce_global(@emacs_man_title_regex,
	      new(regex(string('\n+Sun Release.*\n*\\(\\su+\\).*\\1.*\n+')))).
:- pce_global(@emacs_man_newline_regex,
	      new(regex(string('\n\n+')))).

clean(M) :->
	"Remove ^H_ from the entry"::
	send(M, report, progress, 'Cleaning ...'),
	get(M, text_buffer, TB),
	send(@emacs_man_underline_fragment_regex, for_all, TB,
	     create(fragment, TB,
		    @arg1?register_start,
		    @arg1?register_end - @arg1?register_start,
		    underline)),
	send(@emacs_man_underline_regex, for_all, TB,
	     message(@arg1, replace, @arg2, '')),
	send(@emacs_man_bold_fragment_regex, for_all, TB,
	     create(fragment, TB,
		    @arg1?register_start,
		    @arg1?register_end - @arg1?register_start,
		    bold)),
	send(@emacs_man_bold_regex, for_all, TB,
	     message(@arg1, replace, @arg2, '')),
	send(@emacs_man_title_regex, for_all, TB,
	     message(@arg1, replace, @arg2, string('\n\n'))),
	send(@emacs_man_newline_regex, for_all, TB,
	     message(@arg1, replace, @arg2, string('\n\n'))),
	get(M, skip_comment, 0, Start),
	send(TB, delete, 0, Start),
	send(M?text_buffer, modified, @off),
	send(M, report, done).


goto_man_page(M) :->
	"Find man page from caret"::
	get(M, word, Page),
	get(M, text_buffer, TB),
	get(M, caret, Caret),
	new(Re, regex('\w+(\(\sd\w?\))')),
	(   send(Re, match, TB, Caret)
	->  get(Re, register_value, TB, 1, Section),
	    send(Section, downcase),
	    send(M, man, string('%s %s', Section, Page))
	;   send(M, man, Page)
	).


:- pce_end_class.

