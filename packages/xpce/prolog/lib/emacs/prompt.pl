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

:- module(emacs_prompt,
	  [ make_item/6
	  ]).
:- use_module(library(pce)).

:- pce_autoload(behaviour_item,	       library('man/behaviour_item')).
:- pce_autoload(prolog_predicate_item, library(prolog_predicate_item)).
:- pce_autoload(emacs_tag_item,	       library(emacs_tags)).

resource(back,	image, image('16x16/fatleft_arrow.xpm')).
resource(forw,	image, image('16x16/fatright_arrow.xpm')).

%	make_item(+Mode, +Label, +Default, +Type, +History, -Item)
%
%	Create a dialog item for editing an object of the specified
%	type.

					% deal with dialog special value
make_item(Mode, Label, @emacs_prompt_for, Type, History, Item) :- !,
	make_item(Mode, Label, @default, Type, History, Item).
					% totally unknown
make_item(_Mode, Label, Default, @default, _History, Item) :- !,
	new(Item, text_item(Label, Default)).
					% files and directories
make_item(Mode, Label, Default, Type, _History, Item) :-
	(   send(Type, includes, file)
	;   send(Type, includes, save_file)
	;   send(Type, includes, directory)
	), !,
	(   send(Default, instance_of, file)
	->  get(Default, name, D2)
	;   D2 = Default
	),
	(   send(D2, instance_of, char_array),
	    send(D2, prefix, '/')
	->  DefPath = D2
	;   send(D2, instance_of, char_array),
	    send(regex('[a-zA-Z0-9_-.]+$'), match, D2)
	->  new(DefPath, string('%s/%s', Mode?directory?path, D2))
	;   new(DefPath, string('%s', Mode?directory?path)),
	    send(DefPath, ensure_suffix, /)
	),
	(   (   send(Type, includes, file)
	    ;   send(Type, includes, save_file)
	    ),
	    send(Type, includes, directory)
	->  new(Item, emacs_file_or_directory_item(Label, DefPath))
	;   send(Type, includes, file)
	->  new(Item, file_item(Label, DefPath)),
	    send(Item, exists, @on)
	;   send(Type, includes, save_file)
	->  new(Item, file_item(Label, DefPath)),
	    send(Item, exists, @off)
	;   new(Item, directory_item(Label, DefPath))
	),
	send(Item, length, 40).
					% emacs commands
make_item(_Mode, Label, Default, Type, _History, Item) :-
	send(Type, includes, emacs_mode_command), !,
	default(Default, '', Selection),
	new(Item, emacs_command_item(Label, Selection)),
	send(Item, type, Type).
					% emacs buffer
make_item(Mode, Label, Default, Type, _History, Item) :-
	send(Type, includes, emacs_buffer), !,
	(   Default == @default
	->  (   get(Mode, last_buffer, LB)
	    ->  get(LB, name, Selection)
	    ;	Selection = ''
	    )
	;   Selection = Default
	),
	new(Item, emacs_complete_item(Label, Selection)),
	send(Item, type, Type),
	get(@emacs, buffers, Buffers),
	send(Item, value_set, Buffers).
					% XPCE behaviour
make_item(_Mode, Label, Default, Type, _History, Item) :-
	send(Type, includes, behaviour), !,
	default(Default, '', Selection),
	new(Item, behaviour_item(Label, Selection)).
					% XPCE classes
make_item(_Mode, Label, Default, Type, _History, Item) :-
	send(Type, includes, class), !,
	default(Default, '', Selection),
	new(Item, emacs_complete_item(Label, Selection)),
	send(Item, type, Type),
	new(Classes, chain),
	send(@classes, for_all, message(Classes, append, @arg1)),
	send(Item, value_set, Classes).
					% Prolog predicates
make_item(_Mode, Label, Default, Type, _History, Item) :-
	get(@pce, convert, prolog_predicate, class, _),
	send(Type, includes, prolog_predicate), !,
	default(Default, '', Selection),
	new(Item, prolog_predicate_item(Label, Selection)).
					% Emacs tags
make_item(Mode, Label, _Default, Type, History, Item) :-
	get(Type, name, emacs_tag), !,
	send(Mode, ensure_loaded_tags),
	get(Mode, default_tag, DefTag),
	new(Item, emacs_tag_item(Label, DefTag)),
	(   History \== @default
	->  send(Item, value_set, History)
	;   true
	).
					% Anything else
make_item(_Mode, Label, Default, Type, History, Item) :-
	default(Default, '', Selection),
	(   get(Type, value_set, _)
	->  new(Item, emacs_complete_item(Label, Selection))
	;   new(Item, text_item(Label, Selection)),
	    (   History \== @default
	    ->  send(Item, value_set, History)
	    ;   true
	    )
	),
	send(Item, type, Type).


:- pce_begin_class(emacs_file_or_directory_item, file_item,
		   "Return file or directory object").

selection(I, Sel:'file|directory') :<-
	get_super(I, selection, Name),
	new(Dir, directory(Name)),
	(   send(Name, suffix, /)
	->  Sel = Dir
	;   send(Dir, exists)
	->  Sel = Dir
	;   free(Dir),
	    new(Sel, file(Name))
	).

:- pce_end_class(emacs_file_or_directory_item).


:- pce_begin_class(emacs_complete_item, text_item,
		   "More Emacs-compatible completer").

selection(TI, Value:any) :<-
	"Return unique value if possible"::
	get(TI, value_text, Text),
	get(Text, string, String),
	get(TI, type, Type),
	(   send(TI, is_complete, String, Type)
	->  get_super(TI, selection, Value)
	;   get(TI, completions, String, Completions),
	    (	object(Completions, chain(Value))
	    ->	true
	    ;	get(Completions, size, 0)
	    ->	send(TI, report, warning, 'No match'),
		fail
	    ;	send(TI, report, warning, 'Ambiguous'),
		fail
	    )
	).
	    

is_complete(_TI, Name:name, Type:type) :->
	"Test whether Name satisfies type"::
	(   send(Type, includes, emacs_buffer)
	->  get(@emacs, buffer, Name, _)
	;   get(@pce, convert, Name, Type, _)
	).


completions(TI, From:char_array, Unique:chain) :<-
	get(TI, value_set, Set),
	(   Set == @nil
	->  fail
	;   Set == @default
	->  get(TI, type, Type),
	    get(Type, value_set, TheSet)
	;   get(@pce, convert, Set, chain, TheSet)
	->  true
	),
	get(TheSet, find_all, message(@arg1?print_name, prefix, From), Unique),
	send(Unique, sort, unique := @on).

:- pce_end_class(emacs_complete_item).


:- pce_begin_class(emacs_command_item, emacs_complete_item,
		   "Prompt for a M-x command").

canonise(TI) :->
	get(TI, value_text, Text),
	get(Text, string, String),
	new(Displayed, string('%s', String)),	% make sure it is a string
	send(Displayed, translate, -, '_'),
	send(TI, displayed_value, Displayed).

complete(TI, Ev:[event_id]) :->
	send(TI, canonise),
	send_super(TI, complete, Ev).

selection(TI, Name:name) :<-
	"Return unique value if possible"::
	send(TI, canonise),
	get_super(TI, selection, Name).

:- pce_end_class(emacs_command_item).


		 /*******************************
		 *	   PROMPT DIALOG	*
		 *******************************/


:- pce_begin_class(emacs_prompt_dialog, dialog,
		   "Prompt for method arguments").

variable(implementation, any,	     get,  "Processed implementation").
variable(hindex,	 int*,	     both, "History index").

:- pce_global(@emacs_prompt_dialog_recogniser, make_prompt_binding).

make_prompt_binding(G) :-
	new(Window, @receiver?window),
	new(Back, message(Window, backwards)),
	new(Forw, message(Window, forwards)),

	new(G, key_binding(emacs_prompter, text_item)),
	send(G, function, 'TAB', complete_or_next),
	send(G, function, 'SPC', insert_self),
	send(G, function, 'RET', message(Window, on_return)),
	send(G, function, page_up, Back),
	send(G, function, '\\ep', Back), 	% traditional Emacs
	send(G, function, page_down, Forw),
	send(G, function, '\\en', Forw),
	send(G, function, '\\es', message(Window, sticky_window)),
	send(G, function, '\\C-g', and(message(@receiver, keyboard_quit),
				       message(Window, cancel))).

initialise(D, Mode:emacs_mode, Impl:any, Argv:vector) :->
	(   send(Impl, has_get_method, summary),
	    get(Impl, summary, Summary), Summary \== @nil
	->  Label = Summary
	;   get(Impl, name, Name),
	    Label = string('PceEmacs arguments for %s', Name)
	),
	send_super(D, initialise, Label),
	send(D, slot, implementation, Impl),
	send(D, append, label(title, Label)),
	send(D, fill, Mode, Impl, Argv),
	send(D, append_buttons),
	send(D, resize_message, message(D, layout, @arg2)).

append_buttons(D) :->
	"Append ok, cancel and history buttons"::
	get(D, implementation, Impl),
	(   get(Impl, attribute, emacs_history, History),
	    \+ send(History, empty)
	->  send(D, append, new(B, button(backwards)), next_row),
	    send(D, append, new(O, button(ok))),
	    send(D, append, new(C, button(cancel))),
	    send(D, append, new(F, button(forwards))),
	    send(B, alignment, left),
	    send(F, alignment, right),
	    send(B, label, image(resource(back))),
	    send(F, label, image(resource(forw))),
	    send_list([B,C,O,F], reference, point(0,0))
	;   send(D, append, button(ok), next_row),
	    send(D, append, button(cancel))
	),
	send(D, default_button, ok).


fill(D, Mode:emacs_mode, Impl:any, Argv:vector) :->
	"Add the items"::
	(   between(1, 10, ArgN),
	    get(Argv, element, ArgN, @emacs_prompt_for),
	    get(Impl, argument_type, ArgN, ArgType),
	    (	get(ArgType, argument_name, ArgName),
		ArgName \== @nil
	    ->	true
	    ;	get(ArgType, name, ArgName)
	    ),
	    (	get(Mode, default, ArgType, Default)
	    ->	true
	    ;	Default = @default
	    ),
	    make_item(Mode, ArgName, Default, ArgType, @default, Item),
	    (	send(Item, instance_of, text_item)
	    ->	send(Item, recogniser, @emacs_prompt_dialog_recogniser),
		send(Item, value_font, fixed)
	    ;   true
	    ),
	    send(Item, attribute, argn, ArgN),
	    send(D, append, Item),
	    (	send(Item, instance_of, file_item)
	    ->	send(D, append, button('Browse ...',
				       message(Item, browse)), right)
	    ;	true
	    ),
	    fail
	;   true
	).

ok(D) :->
	send(D, return, ok).
cancel(D) :->
	send(D, destroy).

on_return(D) :->
	"Go to first not-filled item or execute"::
	(   get(D?graphicals, find,
		and(?(@arg1, attribute, argn),
		    not(@arg1?selection)),
		NotFilled)
	->  send(D, keyboard_focus, NotFilled)
	;   send(D, ok)
	).

sticky_window(D, Val:[bool]) :->
	"Toggle sticky status of window"::
	get(D, transient_for, Frame),
	(   send(Frame, has_send_method, sticky_window)
	->  send(Frame, sticky_window, Val)
	;   fail
	).

prompt(D, V:emacs_view, Argv:vector) :->
	"Prompt on behalf of the given window"::
	send(D, transient_for, V),
	send(D, modal, transient),
	get(V?editor, display_position, point(X,Y)),
	get(V?editor?area, size, size(W, H)),
	get(D, confirm_centered, point(X+W/2, Y+H/2), Ok),
	(   Ok == ok
	->  send(D, collect, Argv)
	;   send(D, destroy),
	    fail
	).

collect(D, Argv:vector) :->
	"Fill the vector"::
	send(D?graphicals, for_all,
	     if(?(@arg1, attribute, argn),
		message(D, collect_from, @arg1, Argv))).


collect_from(_, Item:dialog_item, Argv:vector) :->
	get(Item, argn, ArgN),
	get(Item, selection, Value),
	(   send(Item, instance_of, prolog_predicate_item)
	->  new(TheValue, prolog_predicate(Value))
	;   TheValue = Value
	),
	send(Argv, element, ArgN, TheValue).


:- pce_group(history).


backwards(D) :->
	"Go back in the command-history"::
	get(D, implementation, Impl),
	get(Impl, emacs_history, History),
	get(D, hindex, Idx),
	(   Idx == @nil
	->  Nidx = 1
	;   Nidx is Idx + 1
	),
	(   History \== @nil,
	    get(History, nth1, Nidx, ArgVector)
	->  send(D, hindex, Nidx),
	    send(D, fill_from_argv, ArgVector)
	;   send(D, report, warning, 'No more history'),
	    fail
	).


forwards(D) :->
	"Go forwards in the command-history"::
	get(D, implementation, Impl),
	get(Impl, emacs_history, History),
	get(D, hindex, Idx),
	(   (Idx == @nil ; Idx =< 1 ; History == @nil)
	->  send(D, report, warning, 'Back at start'),
	    fail
	;   Nidx is Idx - 1
	),
	get(History, nth1, Nidx, ArgVector),
	send(D, hindex, Nidx),
	send(D, fill_from_argv, ArgVector).


fill_from_argv(D, Argv:vector) :->
	"Fill items from given vector"::
	send(D?graphicals, for_all,
	     if(?(@arg1, attribute, argn),
		message(@arg1, selection,
			?(Argv, element, ?(@arg1, attribute, argn))))).

:- pce_end_class(emacs_prompt_dialog).
