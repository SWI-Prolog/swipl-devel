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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Aims

This file  defines  a window-based  interfaced to  the GNU distributed
ispell program.  It has been written with the following goals in mind:

        # Test-case for editor/fragment handling, processes and regular
          expressions
        # Demo of text facilities, browsers, dialog-windows and process
          interface
        # A useful tool for correcting spelling errors

Functionality

The UI consists of a view (text-window)  for the text to be corrected,
two browsers, one with an alphabetical  list of errors detected by the
spelling  checker and one with a  list of words  that are close to the
error currently selected.  Finally,  there   is a dialog window   that
allows for various commands.

When a file is loaded  (using `load'), it  may be checked by  pressing
`spell'.  This will run `ispell -l -t | sort -u' to  get a sorted list
of errors.  Each  error  is displayed in   the error browser and  each
occurrence of the error in the text is marked with bold font.

Next, errors may be selected by clicking them in the error browser, or
double-clicking  them  in the text-window.   In  both cases, the error
will be  highlighted in the  error-browser and underlined in the text.
The error will appear  in the  `word' and  `correction' fields of  the
dialog-window and  near-misses of the dictionary will  be shown in the
corrections-browser.  Next, the user has several options:

	# `Next'
	  Don't change this occurrence, but search for the next.
	# `Replace'
	  Replace this occurrence with the value of `correction' and
	  search for the next.
        # `Replace All'
	  Replace all occurrences of this error by the value of
	  `correction' .
	# `Dict'
	  Remove this error from the text and error-browser and add
	  the word to the user's personal dictionary.

Clicking a correction  of the correction-browser  sets  the text entry
field `correction'.  This field may also be edited directly.

Finally, the text-window  is an EMACS-like  editor  and errors may  be
corrected directly.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- module(ispell,
	  [ ispell/0
	  , ispell/1
	  ]).
:- use_module(library(pce)).
:- require([ send_list/3
	   ]).

ispell :-
	new(S, ispell),
	send(S, open).
ispell(File) :-
	new(S, ispell(File)),
	send(S, open).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Tell PCE/Prolog that  class   finder is defined   in the  library  and
@finder  is a  global  unique instance  of it.   PCE  will generate an
exception when @finder is referred to the  first time.  This exception
will force PCE/Prolog    to load `library(find_file)'  and  create  an
instance of the finder.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_autoload(finder, library(find_file)).
:- pce_global(@finder, new(finder)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The  entire ispell  program is implemented  as   a single user-defined
class, which is a subclass of class frame.  Class frame is well suited
for  this purpose as  it provides easy access  to  its parts and parts
have easy access to their overall frame.

In this example there is no need to create  subclasses for the various
parts.  All specialisation needed can  easily  be achieved by  setting
options and attaching message objects.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(ispell, frame).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Create a class-variable defining  the  spell   program  used  to  get an
initial list of errors. The user  may   choose  another spell program by
adding the following line to his/her ~/.xpce/Defaults file:

	ispell.spell_program: myspell

The  variables `word'  and  `fragment'   define the  word    currently
examined/replaced  and the current fragment.   They are manipulated by
`ispell ->select'.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

class_variable(spell_program, name, 'ispell -t -l', "Spell command").
class_variable(error_style,   style,
	       when(@colour_display,
		    style(colour := red),
		    style(bold := @on)),
	       "Style used to highlight errors").

variable(word,	   name*,	get,	"Currently handled word").
variable(fragment, fragment*,	get,	"Currently handled fragment").
variable(ispell,   process*,	none,	"The ispell -a process").

		/********************************
		*         CREATE/DESTROY	*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The ispell UI consists of a view to display the  text to be corrected,
a browser for an errors, one for  alternatives suggested by the ispell
unix program (corrections) and a dialog for  invoking commands such as
loading a file, running spell,  replacing, etc.  The first section  of
the  method  below  creates the  frame,  attaches all  the windows and
defines the layout of the windows.

Next, the two browsers are given a label and a name,  so we can easily
find them using the `frame <-member' facility.

Next, the view is  prepared.   It   will  display  fragments  of style
`error' in bold face, underline  the   current  fragment (= error) and
select the current  fragment  on  a   double  click.   Note  that  the
click_gesture is attached to the image of  the editor, so that it will
only be invoked when the user double-clicks   in  the text-area of the
editor and not in the scrollbar.

Finally, the dialog  is filled and the  browser's actions on selection
are setup.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

initialise(F, File:[file]) :->
	"Create from file"::
	send(F, send_super, initialise, 'Ispell'),
	send(F, append, new(V, view)),
	send(F, append, new(D, dialog)),
	send(F, append, new(B, browser)),
	send(F, append, new(C, browser)),
	send(C, below, B),
	send(B, left, V),
	send(D, below, V),

	send(B, name, errors),
	send(C, name, corrections),
	send(B?list_browser, label, 'Errors'),
	send(C?list_browser, label, 'Corrections'),

	get(F, error_style, ErrorStyle),
	send(V, style, error, ErrorStyle),
	send(V, selected_fragment_style, style(underline := @on)),
	send(V?image, recogniser,
	     click_gesture(left, '', double,
			   message(F, select_current_fragment))),
	fill_dialog(D),
	send(B, select_message, message(F, select, @arg1?key)),
	send(C, select_message, message(F, correction, @arg1?key)),
	send(F, clear_errors),

	(   File \== @default
	->  send(F, file, File)
	;   true
	).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The method ->unlink is called when the ispell frame  is destroyed.  It
will destroy the inferior ispell process when it has been created.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

unlink(F) :->
	"Destroy the ispell program when running"::
	send(F, clear_errors),
	get(F, slot, ispell, Ispell),
	(   Ispell \== @nil
	->  send(Ispell, close)
	;   true
	),
	send(F, send_super, unlink).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Fill the dialog window.  Most of the dialog items simply send a message
to the ispell frame.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

fill_dialog(D) :-
	get(D, frame, F),
	send(D, append, label(reporter)),

	send(D, append, button(load, message(F, file))),
	send(D, append, button(save, message(F, save))),
	send(D, append, button(spell, message(F, spell))),
	send(D, append, button(quit, message(F, wm_delete))),

	send(D, append, new(W, text_item(word, ''))),
	send(D, append, new(C, text_item(correction, ''))),
	send(W, editable, @off),
	send(W, pen, 0),

	send(D, append, button(next, message(F, next))),
	send(D, append, button(replace,
			       message(F, replace, C?selection))),
	send(D, append, button(replace_all,
			       message(F, action, C?selection))),
	send(D, append, button(dict,
			       message(F, action, @default, @on))),
	send(D, append, button(undo, message(F?view, undo))).


		/********************************
		*      GETTING THE PARTS	*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
These methods  obtain  the  various  parts  of the ispell application.
This makes it easy to modify the  organisation of the  application and
allows us to write expressions as:

	send(F?view, undo).

to invoke some method on a part.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

view(F, View) :<-
	"View part"::
	get(F, member, view, View).


errors(F, Browser) :<-
	"Browser for errors"::
	get(F, member, errors, Browser).


corrections(F, Browser) :<-
	"Browser for corrections"::
	get(F, member, corrections, Browser).


dialog(F, Dialog) :<-
	"Dialog window"::
	get(F, member, dialog, Dialog).


		/********************************
		*           FEEDBACK		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This method deactivates a number of dialog items.  It uses the catch_all
mechanism of class device to find the references:

	get(Device, member, Name, Reference)

is equivalent to

	get(Device, Name_member, Reference)

but the latter allows us to use the ?/2 infix notation.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

activate_replacement_items(F, Val:bool) :->
	"(De)activate items used for replacement"::
	get(F, dialog, D),
	send_list([ D?word_member
		  , D?correction_member
		  , D?replace_member
		  , D?replace_all_member
		  , D?dict_member
		  , D?next_member
		  ], active, Val).


		 /*******************************
		 *	USER-DEFINED SAVE	*
		 *******************************/

save(F) :->
	"->save_buffer the <-view"::
	send(F?view, save_buffer).


save_action(F, Action:code, Label:[name]) :->
	"Modify the save action"::
	get(F, dialog, D),
	get(D, member, save, SaveButton),
	send(SaveButton, message, Action),
	(   Label \== @default
	->  send(SaveButton, selection, Label?label_name)
	;   true
	).


		 /*******************************
		 *	  LOADING BUFFER	*
		 *******************************/

buffer(F, TB:text_buffer) :->
	"Load text-buffer (cooperation with editors)"::
	send(F?view, text_buffer, TB),
	send(F, confirm_done, @off),
	get(F, dialog, Dialog),
	get(Dialog, member, load, BL), send(BL, active, @off),
	get(Dialog, member, save, BS), send(BS, active, @off).


		/********************************
		*         LOADING FILES		*
		********************************/

file(F, File:[file]) :->
	"Load file (or ask for one)"::
	send(F, clear_errors),
	(   File == @default
	->  get(@finder, file, open, LoadName),
	    new(LoadFile, file(LoadName))
	;   LoadFile = File
	),
	send(F?view, load, LoadFile),
	send(F, label, string('Ispell: %s', LoadFile?base_name), 'Ispell').


		/********************************
		*     RUN SPELL AND MARK ALL	*
		********************************/

clear_errors(F) :->
	"Reset all error info"::
	send(F, select, @nil),
	get(F?view, text_buffer, TB),
	get(TB, find_all_fragments, @arg1?style == error, ErrorFrags),
	send(ErrorFrags, for_all, message(@arg1, free)),
	send(ErrorFrags, done),
	send(F?errors, clear),
	send(F?corrections, clear).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The method below  runs the spell program  and marks all words returned
by it.  First, a process object for the spell process  is created.  As
we want to use the pipe (|) shell construct,  we'll use /bin/sh to run
the process.  Each  line of input from the   process is passed  to the
method `ispell ->mark_word'.  Next,  the contents of  the view is sent
to the process and we wait until all input from the process is handled
using `process ->wait'.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

spell(F) :->
	send(F, clear_errors),

	get(F, spell_program, Prog),
	new(P, process('/bin/sh', '-c', string('%s | sort -u', Prog))),
	send(P, use_tty, @off),
	send(P, input_message, message(F, mark_word, @arg1)),
	send(F, report, progress, 'Running "%s" ...', Prog),
	send(P, open),
	send(P, append, F?view?contents),
	send(P, close),
	send(P, wait),				% wait for completion
	send(P, free),
	get(F?errors?dict?members, size, Errors),
	send(F, report, done, 'Spelling done. %d Errors', Errors).
	

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Mark all occurrences of an error.  First, the error is appended to the
`errors' browser.   The `dict_item <->object' slot is  used to store a
chain  of fragments  for this  error.    Next a regular expression  is
created and all occurrences of the regular expression are marked using
a fragment.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_global(@ispell_mark_re, new(regex(''))).

mark_word(F, S:string) :->
	"Mark all occurrences of word"::
	send(S, strip),
	get(S, value, Word),
	send(F, report, progress, 'Marking %s ...', Word),
	get(F, errors, B),
	send(B, append, dict_item(Word, @default, new(Errors, chain))),
	get(F, view, View),
	get(View, text_buffer, TB),
	get(@ispell_mark_re, quote, S, Q),
	send(Q, prepend, '\\b'),
	send(Q, append, '\\b'),
	send(@ispell_mark_re, pattern, Q),
	mark_all(TB, 0, @ispell_mark_re, Word, Errors, error),
	send(F, report, done).

mark_all(TB, Index, Re, Word, Errors, Kind) :-
	send(Re, search, TB, Index), !,
	get(Re, register_start, RS),
	get(Re, register_end, RE),
	L is RE - RS,
	new(Fragment, fragment(TB, RS, L, Kind)),
	send(Errors, append, Fragment),
	mark_all(TB, RE, Re, Word, Errors, Kind).
mark_all(_, _, _, _, _, _).


		/********************************
		*         SELECT WORDS		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Select the word after a double click.  The first click (of the double)
has positioned the caret,  so we find  all fragments  overlapping  the
caret position and take the smallest.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

select_current_fragment(F) :->
	"Select fragment of caret"::
	get(F, view, View),
	get(View, caret, Caret),
	get(View, find_all_fragments,
	    message(@arg1, overlap, Caret), Fragments),
	send(Fragments, sort, @arg1?length < @arg2?length),
	get(Fragments, head, Fragment),
	get(Fragment, string, Word),
	send(F, select, Word, Fragment),
	send(F?errors, selection, Word),
	send(F?errors, normalise, Word).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Set the `selection' of ispell.  When `word' is @nil, the  selection is
cleared.  Otherwise   the word   is    send to  ispell   to   list the
alternatives (ispell will invoke `ispell  ->ispell_utterance' for each
line of output).  The word is selected in the `errors' browser and the
view.  The dialog is updated.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

select(F, Word:name*, Fragment:[fragment]) :->
	"Select some word"::
	get(F, dialog, Dialog),
	(   Word == @nil			% clear selection
	->  send(F, slot, word, @nil),
	    send(F, slot, fragment, @nil),
	    send(Dialog?word_member, selection, ''),
	    send(Dialog?correction_member, selection, ''),
	    send(F?corrections, clear),
	    send(F, activate_replacement_items, @off)
	;   send(F?ispell, append_line, Word),
	    get(F, errors, Browser),
	    get(Browser, member, Word, DI),
	    get(DI, object, Errors),
	    (   get(F, word, Word)		% same word
	    ->  true
	    ;   send(Dialog?word_member, selection, Word),
		send(Dialog?correction_member, selection, Word),
	        send(F, report, status,
		     '%d occurrences of "%s"', Errors?size, Word),
	        send(F, slot, word, Word)
	    ),
	    send(F, activate_replacement_items, @on),
	    (   Fragment \== @default
	    ->  send(F, slot, fragment, Fragment)
	    ;   (   send(Errors, empty)
		->  fail
		;   send(F, slot, fragment, Errors?head)
		)
	    ),
	    get(F, fragment, Frag),
	    get(F, view, View),
	    send(View, caret, Frag?start),
	    send(View, selected_fragment, Frag)
	).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Activated if the user clicks a word in the `corrections'  browser.  It
updates the `correction' text_item.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

correction(F, Word:name) :->
	"Put value in correction field"::
	get(F?dialog, correction_member, TI),
	send(TI, selection, Word).


		/********************************
		*       ISPELL PROGRAM		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Interface to the  interactive   inferior ispell program   to  generate
possible alternatives  and   to  add words    to the user's   personal
dictionary.

`ispell  <-ispell' returns the  ispell process if this already running
or creates one  otherwise.   The ispell  process will invoke   `ispell
->ispell_utterance' for each input record, which is by default a line.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

ispell(F, Ispell) :<-
	"Get the ispell process or start it"::
	(    get(F, slot, ispell, Ispell),
	     Ispell \== @nil
	->   true
	;    new(Ispell, process(ispell, '-a')),
	     send(Ispell, use_tty, @off),
	     send(Ispell, input_message, message(F, ispell_utterance, @arg1)),
	     send(Ispell, open),
	     send(F, slot, ispell, Ispell)
	).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
If Ispell utters `& alternative  ...' the alternatives should be added
to the  `corrections' browser.   First   the & prefix  is  tested.   A
regular expression is used to   filter the individual words from  the
line.  `regex ->for_all' invokes  its  argument code object for   each
match it can find in the argument text.  @arg1  is the regex, @arg2 is
the object searched.  Register 0 of a regular expression refers to the
entire match.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_global(@re_word_ispell_3, new(regex('[:,] \\([^,]*\\)'))).
:- pce_global(@re_word_ispell_4, new(regex('\\s +\\(\\S +\\)'))).

ispell_utterance(F, Line:string) :->
	"Handle line of output from ispell"::
	get(F, corrections, Corrections),
	(   get(Line, size, 1)
	->  true
	;   send(Corrections, clear)
	),	
	(   send(Line, prefix, '& ')
	->  send(Line, strip),
	    (   get(Line, index, ':', _)
	    ->  Re = @re_word_ispell_3
	    ;   Re = @re_word_ispell_4
	    ),
	    send(Re, for_all, Line, 
		 message(Corrections, append,
			 ?(@arg1, register_value, @arg2, 1)))
	;   send(Line, prefix, '#'),
	    send(F, report, warning, 'No suggestions')
	).


		/********************************
		*           REPLACEMENTS	*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
`ispell ->action' handles the current word for all matching fragments.
If `R' is non-default, all fragments are replaced with  this text.  If
`AddToDict' is  non-default, the word is added   to the dictionary and
its matches are removed from the view and browser.

Note the use   of `view ->mark_undo'  (actually implemented  at  class
editor).  Normally, an editor will mark all changes made to it under a
single  event as a `undo-unit'.  As   the editor is  not receiving any
events here, we'll have to indicate the unit of undo ourselves.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

action(F, R:[name], AddToDict:[bool]) :->
	"Replace and/or add to dictionary"::
	get(F, errors, Browser),
	get(F, word, Word),
	get(Browser, member, Word, DI),
	get(DI, object, Errors),
	(   R \== @default
	->  send(Errors, for_all, message(@arg1, string, R)),
	    send(F?view, mark_undo),
	    ToDict = R
	;   ToDict = Word
	),
	(   AddToDict == @on
	->  send(F?ispell, format, '*%s\\n', ToDict),
	    send(F, report, status, 'Added "%s" to dictionary', ToDict)
	;   true
	),
	send(Errors, for_all, message(@arg1, free)),
	send(Errors, free),
	send(DI, free),
	send(F, select, @nil).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
`ispell ->replace'  replaces the  current  (underlined) fragment   and
selects the next fragment.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

replace(F, R:name) :->
	"Replace current fragment"::
	get(F, view, View),
	get(View, selected_fragment, Fr),
	send(Fr, string, R),
	send(F?view, mark_undo),
	send(F, next).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
`ispell ->next' will unmark the current fragment and skip to the next.
The first section  gathers  the current word,  fragment  and chain  of
fragments belonging to  this word.  Next, we  gather the index in  the
fragment chain  of   the  current  fragment and  delete    the current
fragment.  Finally, there are various possibilities:  there are no more
errors in the chain, there is a next, or we have to wrap around to the
first.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

next(F) :->
	"Select next fragment, deleting this one"::
	get(F, word, Word),
	get(F, fragment, F1),
	get(F, errors, Browser),
	get(Browser, member, Word, DI),
	get(DI, object, Errors),
	
	get(Errors, index, F1, IF1),
	send(Errors, delete, F1),
	send(F1, free),

	(   send(Errors, empty)
	->  send(Browser, delete, Word),
	    send(F, select, @nil)
	;   (   get(Errors, nth1, IF1, F2)
	    ;   get(Errors, head, F2)
	    )
	->  send(F, select, Word, F2)
	).

:- pce_end_class.

