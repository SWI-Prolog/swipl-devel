/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

:- module(emacs_window, []).
:- use_module(library(pce)).
:- require([ between/3
	   , concat/3
	   , concat_atom/2
	   , default/3
	   , forall/2
	   , ignore/1
	   ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@current_emacs_mode is a variable  pointing   to  the current emacs-mode
object.  Pushed by `emacs_key_binding  ->fill_arguments_and_execute' and
various others.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_global(@current_emacs_mode, new(var)).

:- pce_begin_class(emacs_window, frame, "Frame for the PceEmacs editor").

:- pce_global(@emacs_image_recogniser,
	      new(handler(button,
			  message(@receiver?device?(mode), event, @event)))).

resource(confirm_done,	bool,	false,	     "Donot confirm emacs-windows").
resource(size,		size,	size(80,32), "Size of text-field").

variable(sticky_window, bool,   get,  "When @on, window won't be killed").
variable(pool,		[name], both, "Window pool I belong too").

initialise(F, B:emacs_buffer) :->
	"Create window for buffer"::
	send(F, send_super, initialise, B?name),
	send(F, slot, sticky_window, @off),
	send(F, append, new(MBD, dialog)),
	send(MBD, resize_message, message(@receiver, layout, @arg2)),
	send(MBD, gap, size(0,3)),
	send(MBD, pen, 0),
	send(MBD, display, new(menu_bar)),

	get(F, resource_value, size, Size),
	send(new(V, view(@default, @default, @default,
			 new(E, emacs_editor(B, Size?width, Size?height)))),
	     below, MBD),
	send(new(MW, emacs_mini_window), below, V),
	send(E?image,  recogniser, @emacs_image_recogniser),
	send(E, recogniser, handler(keyboard, message(MW, event, @arg1))),

	get(B, mode, ModeName),
	send(V, mode, ModeName),
	send(F, keyboard_focus, V),

	send(F, open),
	send(F, pool, B?pool).


input_focus(F, Val:bool) :->
	"Activate the window"::
	send(F, send_super, input_focus, Val),
	(   send(F, unlinking)
	->  true
	;   send(F, active, Val)
	).


buffer(F, B:emacs_buffer) :->
	"Switch to the given emacs buffer"::
	send(F?editor, text_buffer, B),
	get(B, mode, ModeName),
	send(F?view, mode, ModeName),
	send(F, pool, B?pool),
	send(B, update_label),
	send(F, report, status, '').


view(F, View:view) :<-
	get(F, member, view, View).


editor(F, Editor:emacs_editor) :<-
	"Editor component of the frame"::
	get(F, member, view, V),
	get(V, editor, Editor).


mode(F, Mode:emacs_mode) :<-
	"Current mode object of the editor"::
	get(F, editor, E),
	get(E, mode, Mode).


menu_bar(F, MB:menu_bar) :<-
	"The menu_bar object at the top"::
	get(F, member, dialog, D),
	get(D, member, menu_bar, MB).


active(F, Val:bool) :->
	"Indicate active status"::
	get(F, member, view, View),
	(   Val == @on
	->  send(@emacs, selection, View?text_buffer)
	;   send(@emacs, selection, @nil)
	),
	get(View, text_cursor, C),
	send(C, active, Val).


sticky_window(F, Val:[bool]) :->
	"[toggle] sticky status"::
	default(Val, F?sticky_window?negate, V2),
	send(F, slot, sticky_window, V2),
	get(F, member, mini_window, WM),
	get(WM, member, sticky_indicator, Bitmap),
	(   get(F, sticky_window, @on)
	->  send(Bitmap, image, 'sticky.bm')
	;   send(Bitmap, image, 'nosticky.bm')
	).


fit(F) :->
	"Request to fit the contents"::
	(   get(F, attribute, fitted, @on)
	->  send(F, resize)
	;   send(F, send_super, fit),
	    send(F, attribute, fitted, @on)
	).


		 /*******************************
		 *	    PROMPTING		*
		 *******************************/

:- pce_global(@emacs_nil,
	      new(constant(emacs_nil,
			   string("Nil that does not match @nil")))).
:- pce_global(@prompt_recogniser,
 	new(handler_group(handler('\t',
				  message(@receiver, complete)),
			  handler('\r',
				  and(@receiver?modified == @off,
				      message(@receiver, apply, @on))),
			  handler('\C-g',
				  and(message(@receiver, keyboard_quit),
				      message(@receiver?frame, return,
					      @emacs_nil)))))).


prompt_using(F, Item:dialog_item, Rval:any) :<-
	"Prompt for value in dialog using Item"::
	get(F, member, mini_window, W),

	send(W, prompter, Item),
	send(Item, message, message(F, return, @receiver?selection)),
	send(Item, recogniser, @prompt_recogniser),
	
	send(F, keyboard_focus, W),
	get(F, confirm, RawVal),
	get(F, member, view, View),
	send(F, keyboard_focus, View),

	send(RawVal, lock_object, @on),	% protect for gc
	send(Item, message, @nil),
	send(W, prompter, @nil),
	RawVal \== @emacs_nil,		% must be able to pass @nil!

	Rval = RawVal.
	

:- pce_autoload(behaviour_item, library('man/behaviour_item')).
:- pce_autoload(directory_item, library('file_item')).

prompt(F, Label:char_array, Default:[any], Type:[type], Rval:any) :<-
	"Prompt for a value in the mini-window"::
	default(Default, '', Selection),
	(   Type == @default
	->  new(Item, text_item(Label, Selection))
	;   (	send(Type, includes, file)
	    ;	send(Type, includes, directory)
	    )
	->  (	send(Default, instance_of, file)
	    ->	get(Default, name, D2)
	    ;	D2 = Default
	    ),
	    (	send(D2, instance_of, char_array),
		send(D2, prefix, '/')
	    ->	DefPath = D2
	    ;	send(D2, instance_of, char_array),
		send(regex('[a-zA-Z0-9_-.]+$'), match, D2)
	    ->	new(DefPath, string('%s/%s',
				    F?editor?(mode)?directory?path,
				    D2))
	    ;	new(DefPath, string('%s', F?editor?(mode)?directory?path)),
		send(DefPath, ensure_suffix, /)
	    ),
	    (	send(Type, includes, file)
	    ->	new(Item, file_item(Label, DefPath))
	    ;	new(Item, directory_item(Label, DefPath))
	    )
	;   send(Type, includes, emacs_mode_command)
	->  new(Item, emacs_command_item(Label, Selection)),
	    send(Item, type, Type)
	;   send(Type, includes, emacs_buffer)
	->  new(Item, text_item(Label, Selection)),
	    send(Item, type, Type),
	    get(@emacs, buffers, Buffers),
	    send(Item, value_set, Buffers)
	;   send(Type, includes, behaviour)
	->  new(Item, behaviour_item(Label, Selection))
	;   send(Type, includes, class)
	->  new(Item, text_item(Label, Selection)),
	    send(Item, type, Type),
	    new(Classes, chain),
	    send(@classes, for_all, message(Classes, append, @arg1)),
	    send(Item, value_set, Classes)
	;   new(Item, text_item(Label, Selection)),
	    send(Item, type, Type)
	),
	send(Item, length, 60),
	send(Item, pen, 0),
	
	get(F, prompt_using, Item, Rval),
	send(Item, free).


:- pce_end_class.

:- pce_begin_class(emacs_command_item, text_item).

cannonise(TI) :->
	get(TI, value_text, Text),
	get(Text, string, String),
	get(String, copy, Str2),
	send(Str2, translate, -, '_'),
	send(TI, displayed_value, Str2).

complete(TI, Ev:[event_id]) :->
	send(TI, cannonise),
	send(TI, send_super, complete, Ev).

selection(TI, Name:name) :<-
	send(TI, cannonise),
	get(TI, get_super, selection, Name).

:- pce_end_class.


:- pce_begin_class(emacs_mini_window, dialog, "Prompt and feedback window").

variable(prompter, 	 dialog_item*,  get,	"Current prompter").
variable(report_count,	 number,	get,	"Count to erase report").
variable(report_type,	 name*,		both,	"Last type of report").

initialise(D) :->
	send(D, send_super, initialise),
	send(D, slot, report_count, number(0)),
	send(D, gap, size(10, 2)),
	send(D, pen, 0),
	send(D, display, new(StickyIndicator, bitmap('nosticky.bm', @on))),
	send(StickyIndicator, name, sticky_indicator),
	send(StickyIndicator, recogniser,
	     click_gesture(left, '', single,
			   message(@receiver?frame, sticky_window))),
	send(D, display, label(reporter), point(25, 2)),
	get(text_item(''), height, MH),
	send(D, height, MH),
	send(D, name, mini_window).

'_compute_desired_size'(_) :->
	"We have fixed size"::
	true.

report(D, Type:name, Fmt:[char_array], Args:any ...) :->
	"Report"::
	(   get(D, report_type, ReportType),
	    ok_to_overrule(Type, ReportType)
	->  send(D, report_type, Type),
	    get(D, member, reporter, Label),
	    get(D, report_count, RC),
	    (   Fmt == '', Type == status	% clear
	    ->  send(Label, clear),
		send(RC, value, 0)
	    ;   (	get(D, prompter, Prompter), Prompter \== @nil
		->	send(Label, x, Prompter?width + 10)
		;	send(Label, x, 25)
		),
		send(Label, displayed, @on),
		send(Label, send_vector, report, Type, Fmt, Args),
		send(RC, value, 10)
	    )
	;   true
	).

ok_to_overrule(_, @nil).
ok_to_overrule(_, status).
ok_to_overrule(_, progress).
ok_to_overrule(_, done).
ok_to_overrule(warning, warning).
ok_to_overrule(inform, _).
ok_to_overrule(error, _).

:- pce_global(@emacs_mini_window_bindings, make_emacs_mini_window_bindings).

make_emacs_mini_window_bindings(B) :-
	new(B, key_binding(emacs_mini_window)),
	send(B, function, '\es', sticky_window),
	send(B, function, '\en', m_x_next),
	send(B, function, '\ep', m_x_previous).
	      

event(D, Ev:event) :->
	"Process event"::
	(   send(Ev, is_a, keyboard)
	->  (   get(D, prompter, Prompter),
	        Prompter \== @nil
	    ->  (   send(@emacs_mini_window_bindings, event, Ev)
		->  true
		;   get(D, member, reporter, Reporter),
		    send(Reporter, displayed, @off),
		    ignore(send(Ev, post, Prompter))
		)
	    ;   send(D, report_type, @nil),
%		send(Ev, is_a, keyboard),
		get(D, report_count, RC),
		send(RC, minus, 1),
		send(RC, equal, 0),
		send(D, report, status, ''),
		fail			% never accept this
	    )
	;   send(D, send_super, event, Ev)
	).


m_x_next(D) :->
	"Handle M-n to get next value"::
	(   get(D?frame?mode, m_x_next, NewDefault)
	->  send(D?prompter, displayed_value, NewDefault?print_name)
	;   send(D?prompter, restore)
	).


m_x_previous(D) :->
	"Handle M-p to get previous value"::
	get(D?frame?mode, m_x_previous, NewDefault),
	send(D?prompter, displayed_value, NewDefault?print_name).


geometry(D, X:[int], Y:[int], W:[int], H:[int]) :->
	"Change size, center contents vertically"::
	send(D, send_super, geometry, X, Y, W, H),
	get(D, height, DH),
	DH2 is DH//2,
	send(D?graphicals, for_all,
	     message(@arg1, center_y, DH2)).


prompter(D, Prompter:dialog_item*) :->
	"Display the prompter"::
	get(D, member, reporter, Reporter),
	(   get(D, prompter, OldPrompter), OldPrompter \== @nil
	->  send(D, erase, OldPrompter)
	;   true
	),
	(   Prompter == @nil
	->  send(Reporter, clear),
	    send(Reporter, displayed, @on)
	;   send(Reporter, displayed, @off),
	    get(Prompter, height, H),
	    get(D, height, DH),
	    PY is (DH-H)//2,
	    send(D, display, Prompter, point(25, PY)),
	    get(Prompter, height, H),
	    MinH is H,
	    (	DH < MinH
	    ->	send(D, height, MinH)
	    ;	true
	    )
	),
	send(D, slot, prompter, Prompter).

:- pce_end_class.

:- pce_begin_class(emacs_editor, editor, "Generic PceEmacs editor").

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
We use a pool of  modes  recorded  in   <-modes  to  avoid  the need for
destruction of mode objects during the livetime  of the editor.  This is
dangerous as the mode might still be `running' some command.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

variable(mode,		  emacs_mode*,	get, "Mode of operation").
variable(modes,		  chain,	get, "Modes part of this editor").

initialise(E, TB:[text_buffer], W:[int], H:[int]) :->
	send(E, send_super, initialise, TB, W, H),
	send(E, slot, modes, new(chain)).


unlink(E) :->
	"Unlink from mode object"::
	get(E, modes, Modes),
	send(Modes, for_all, message(@arg1, free)),
	send(E, send_super, unlink).


typed(E, Id:event_id) :->
	"Handle typing via mode"::
	get(E, mode, Mode),
	ignore(send(Mode, typed, Id, E)). % don't delegate to frame


mode(E, ModeName:mode_name) :->
	"Associate argument mode"::
	get(E, mode, OldMode),
	(   get(OldMode, name, ModeName)
	->  send(E, syntax, OldMode?syntax)
	;   (	get(E?modes, find, @arg1?name == ModeName, Mode)
	    ->  send(E, slot, mode, Mode)
	    ;	send(E, slot, mode, ModeName),	% Converted to object
	        get(E, mode, Mode),		% The object
	        send(Mode, editor, E),
		send(E?modes, append, Mode)
	    ),
	    send(E?text_buffer, mode, ModeName),
	    send(E, syntax, Mode?syntax),
	    send(E, setup_mode),
	    ignore(send(E, fill_menu_bar, E?frame?menu_bar)),
	    send(E, report, status, 'Switched to ``%s'''' mode', ModeName)
	).

	
sticky_window(E, Val:[bool]) :->
	"Change sticky status of window"::
	send(E?frame, sticky_window, Val).


preview_drop(E, Obj:object*) :->
	"Delegate to mode"::
	get(E, mode, Mode),
	get(Mode?class, send_method, preview_drop, _), % avoid delegation
	send(Mode, preview_drop, Obj).


drop(E, Obj:object) :->
	"Delegate to mode"::
	get(E, mode, Mode),
	get(Mode?class, send_method, drop, _),	       % avoid delegation
	send(Mode, drop, Obj).


import_selection(E) :->
	"Import the (primary) selection or the cut_buffer"::
	get(E, display, Display),
	(   pce_catch_error(get_selection, get(Display, selection, String))
	;   get(Display, cut_buffer, 0, String)
	), !,
	(   get(E, frame, Frame),
	    get(Frame, member, mini_window, MiniWindow),
	    get(MiniWindow, prompter, TI),
	    send(TI, instance_of, text_item)
	->  send(TI, insert, @default, String)
	;   send(E, insert, String)
	).


catch_all(E, Selector:name, Args:unchecked ...) :->
        "Delegate to mode"::
        get(E, mode, Mode),
	get(Mode?class, send_method, Selector, _),
        send(@pce, last_error, @nil),
        send(Mode, send_vector, Selector, Args).


		 /*******************************
		 *          UTILITIES		*
		 *******************************/

looking_at(E, Re:regex, Where:[int]) :->
	"Test if regex macthes from the caret"::
	(   Where == @default
	->  get(E, caret, C)
	;   C = Where
	),
	get(E, text_buffer, TB),
	send(Re, match, TB, C).

:- pce_end_class.

		 /*******************************
		 *	    EMACS MODES		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PceEmacs  modes  are  tricky  stuff.   We    like  to  have  comfortable
programming, which implies we should  represent   a  PceEmacs  mode as a
class.   This  provides  us  with  a   good  programming  interface  and
inheritance as well as all the other goodies of OO programming.

We also would like to be able to   switch PceEmacs windows from one mode
to another.  This conflicts, as instances  cannot be migrated from class
to class.  Therefore, PceEmacs modes are objects  that are attached to a
emacs_editor.  An emacs_mode delegates to the   editor  in this mode and
editors delegate to their mode (with an explicit method to avoid endless
loops  if  the   method   is   defined    on   neither).    The   method
`pce_editor->mode' attaches a mode to an editor.

Next, we would like users to be able  to extend these classes to provide
custom methods and possibly redefine methods.  This has been implemented
using tricky meta-programming: class emacs_mode_class   is a subclass of
class `class' (representing classes).   This   class  defines the method
->load_user_extensions, which is called when an instance of the class is
created.   The  classes  defining  emacs_modes  are  instances  of  this
emacs_mode_class class.  To understand this, try:

	?- new(X, class(myclass, class)),
	   new(Y, myclass(myobject, object)),
	   new(Z, myobject).

and verify how the objects and classes are related.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


:- pce_begin_class(emacs_mode_class, class,
		   "Class for emacs modes").

variable(user_extensions_loaded, bool := @off, get,
	 "Test if extensions are loaded").

load_user_extensions(C) :->
	"Load mode extensions from ~/lib/xpce/emacs/"::
	(   get(C, user_extensions_loaded, @on)
	->  true
	;   get(C, name, Name),
	    (   concat(emacs_, Base, Name),
		concat(Base, '.pl', FileName),
		get(directory('~/lib/xpce/emacs'), file, FileName, File),
		send(File, access, read)
	    ->  get(File, absolute_path, Path),
		ensure_loaded(Path)
	    ;   true
	    ),
	    send(C, slot, user_extensions_loaded, @on),
	    get(C, super_class, Super),
	    (	send(Super, has_send_method, load_user_extensions)
	    ->	send(Super, load_user_extensions)
	    ;	true
	    )
	).

:- pce_end_class.

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Finally, we have to tell pce_begin_class/3 the  meta-class we want to be
using.  If pce_begin_class/3 makes a subclass, it will make the subclass
of the same  meta-class  as  its   super-class.   Thus,  a  subclass  of
emacs_mode will be an instance   of  class(emacs_mode_class), instead of
class(class).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(emacs_mode_class:emacs_mode(name), object,
		   "Generic PceEmacs mode class").

variable(name,		  name,		get,  "Name of the mode").
variable(syntax,	  syntax_table,	get,  "Syntax for this mode").
variable(bindings,	  key_binding,	get,  "Key-binding table").
variable(editor,	  editor*,	both, "Associated editor").
variable(m_x_history,	  chain*,	both, "Current M-x command history").
variable(m_x_index,	  int*,		both, "M-p/M-n current index").
variable(m_x_argn,	  int*,		both, "M-p/M-n current argument").
variable(keep_selection,  bool := @off, both, "Keep selection for this method").

delegate_to(editor).

		/********************************
		*         CREATE/REUSE		*
		********************************/

initialise(M) :->
	"Create"::
	get(M, class_name, ClassName),
	mode_name(ClassName, Name),
	send(@mode_name_type?context, add, Name), % make sure
	send(M, send_super, initialise),
	send(M, slot, name, Name),
	send(M, slot, syntax, Name),		  % converts to object
	send(M, bindings).


mode_name(emacs_mode, default) :- !.
mode_name(Mode, Name) :-
	(   concat(emacs_, M1, Mode)
	;   M1 = Mode
	), !,
	(   concat(Name, '_mode', M1)
	;   Name = M1
	), !.


table_name(ClassName, TableName) :-
	concat(TableName, '_mode', ClassName).


bindings(M) :->
	"Associate key_binding table"::
	get(M, class_name, ClassName),
	binding_name(ClassName, Name),
	get(@key_bindings, member, Name, Table), !,
	send(M, slot, bindings, Table).


syntax(M) :->
	"Associate syntax table"::
	get(M, class_name, ClassName),
	binding_name(ClassName, Name),
	get(@syntax_tables, member, Name, Table), !,
	send(M, slot, syntax, Table).


binding_name(ClassName, Name) :-
	table_name(ClassName, Name).
binding_name(ClassName, Name) :-
	mode_name(ClassName, ModeName),
	key_binding_name(ModeName, Name).
binding_name(ClassName, Name) :-
	get(@pce, convert, ClassName, class, Class),
	get(Class, super_class, Super),
	send(Super, is_a, emacs_mode),
	get(Super, name, SuperName),
	binding_name(SuperName, Name).


convert(_, Name:name, Mode:emacs_mode) :<-
        "Convert name into a mode object"::
        concat_atom([emacs_, Name, '_mode'], ModeClassName),
        new(Mode, ModeClassName).


		 /*******************************
		 *	   THE MODE MENU	*
		 *******************************/

:- pce_global(@emacs_mode, new(@event?window?frame?(mode))).

mode_menu(M, MM:emacs_mode_menu) :<-
	"Return the mode-menu structure for this mode"::
	get(M, class_name, ClassName),
	mode_menu_name(ClassName, Name),
	get(@emacs_mode_menus, member, Name, MM), !.


mode_menu_name(ClassName, Name) :-
	mode_name(ClassName, Name).
mode_menu_name(ClassName, Name) :-
	get(@pce, convert, ClassName, class, Class),
	get(Class, super_class, Super),
	send(Super, is_a, emacs_mode),
	get(Super, name, SuperName),
	mode_menu_name(SuperName, Name).

fill_menu_bar(M, MB:menu_bar) :->
	"Fill the menu_bar"::
	get(M, mode_menu, ModeMenu),
	send(MB, clear),		% ???
	send(ModeMenu, for_all,
	     and(if(?(MB, member, @arg1?name),
		    message(M, fill_menu,
			    ?(MB, member, @arg1?name), @arg1?value),
		    and(assign(new(P, var),
			       create(popup, @arg1?name,
				      message(@emacs_mode,
					      noarg_call, @arg1))),
			message(MB, append, P,
				when(@arg1?name == help, right, @default)),
			message(M, fill_menu, P, @arg1?value))))).


fill_menu(M, P:popup, Entries:chain) :->
	"Add specified entries to the given popup"::
	send(Entries, for_all,
	     message(P, append,
		     when(message(@arg1, instance_of, name),
			  ?(M, menu_item, @arg1),
			  @arg1?clone))).


menu_item(M, Selector:name, MI:menu_item) :<-
	"Generate a menu-item for selector"::
	new(MI, menu_item(Selector)),
	(   get(M, bindings, KeyBindings),
	    get(KeyBindings, binding, Selector, Key)
	->  send(MI, accelerator, Key)
	;   true
	),
	(   get(M, send_method, Selector, tuple(_, Impl))
	->  (	forall((between(1, 10, ArgN),
	                get(Impl, argument_type, ArgN, ArgType)),
		       send(ArgType, includes, default))
	    ->	true
	    ;	send(MI, label, string('%s ...', Selector?label_name))
	    )
	;   send(MI, active, @off)
	).


%:- pce_global(@c_method, new(var)).	% debugging

open_history(M, Impl:behaviour, Force:[bool]) :->
	"(Initialise) history for behaviour"::
	(   (Force == @on ; get(M, m_x_history, @nil))
	->  (   get(Impl, attribute, emacs_history, History)
	    ->  true
	    ;   send(Impl, attribute, emacs_history, new(History, chain))
	    ),
%	    send(@c_method, assign, Impl, global),
	    send(M, m_x_history, History),
	    send(M, m_x_index, @nil)
	;   true
	).


close_history(M, Argv:[vector]) :->
	"Close open history adding Argv"::
	get(M, m_x_history, History),
	(   Argv \== @nil, Argv \== @default
	->  clean_argv(Argv),
	    (	get(History, find,
		    message(@prolog, same_argv, Argv, @arg1),
		    Old)
	    ->	send(History, move_after, Old)
	    ;	send(History, prepend, Argv)
	    )
%	    send(@pce, send_vector, write_ln,
%		 'History:', @c_method?print_name, ':', Argv)
	;   true
	),
	send(M, m_x_history, @nil),
	send(M, m_x_index, @nil).


same_argv(V1, V2) :-
	get(V1, size, S1),
	get(V2, size, S1),
	forall(between(1, S1, N),
	       (   get(V1, element, N, E1), get(E1, print_name, P1),
		   get(V2, element, N, E2), get(E2, print_name, P2),
		   send(P1, equal, P2)
	       )).


%	clean_argv(+Vector)
%	Replace vector elements with their written version to avoid the
%	risk of illegal-object references.

clean_argv(Vector) :-
	send(Vector, for_all,
	     message(Vector, element, @arg2, @arg1?print_name)).

noarg_call(M, Selector:name) :->
	"Invoke method without arguments (prompt)"::
	(   get(M, send_method, Selector, tuple(_, Impl))
	->  send(@current_emacs_mode, assign, M),
	    send(M, open_history, Impl, @on),
	    new(Argv, vector),
	    between(1, 10, ArgN),
	    (   get(Impl, argument_type, ArgN, ArgType)
	    ->  (   send(ArgType, includes, default)
		->  send(Argv, element, ArgN, @default)
		;   get(M, interactive_argument, Impl, ArgN, Arg),
		    get(ArgType, check, Arg, CheckedArg)
		->  send(Argv, element, ArgN, CheckedArg)
		;   !, fail
		),
		fail			% force backtracing
	    ;   !
	    ),
	    result(send(M, send_vector, Selector, Argv), YesNo),
	    (	object(M)
	    ->  send(M, close_history, Argv),
		send(M, mark_undo),
		(   YesNo == no
		->  send(M, report, status, '%s command failed', Selector)
		;   true
		)
	    ;	true
	    )
	;   send(M, report, error, 'No implementation for ``%s''''', Selector)
	).


result(Goal, yes) :-
	Goal, !.
result(_, no).

arg_call(M, Selector:name, Arg:any) :->
	"Invoke method from pullright menu"::
	send(M, Selector, Arg).


		 /*******************************
		 *           SETUP		*
		 *******************************/

setup_mode(M) :->
	"Initialise mode (->load_user_extensions)"::
	send(M, load_user_extensions).


load_user_extensions(M) :->
	"Load mode extensions from ~/lib/xpce/emacs/"::
	send(M?class, load_user_extensions).


		/********************************
		*           TYPING		*
		********************************/

typed(M, Id:event_id, Editor:editor) :->
	"Handle typed character for editor"::
	get(M, text_buffer, TB),
	send(TB, check_auto_save),
	
	(   get(M, focus_function, F), F \== @nil
 	->  (   send(M, F, Id)
	    ->  true
	    ;   send(M, focus_function, @nil),
		send(M, typed, Id, Editor)	  % failed: unfocus and resent
 	    )
	;   get(M, bindings, Binding),
	    send(Binding, typed, Id, M),
	    (	object(M),		% may be deleted ...
	        get(M, focus_function, @nil)
	    ->  (   get(M, keep_selection, @off)
		->  send(M?editor, selection, 0, 0)
		;   true
		),
		send(M, keep_selection, @off)
	    ;	true
	    )
	).


		 /*******************************
		 *	     SELECTION		*
		 *******************************/

selection(E, From:int, To:int) :->
	"Set selection and keep it one action"::
	send(E?editor, selection, From, To),
	send(E, keep_selection, @on).

select_line(E, Line:int) :->
	"Set selection and keep it one action"::
	send(E?editor, select_line, Line),
	send(E, keep_selection, @on).


		 /*******************************
		 *	     EVENTS		*
		 *******************************/

event(M, Ev:event) :->
	"Allow for gestures to be appended to modes"::
	get(M, all_recognisers, Chain),
	get(Chain, find,
	    message(@arg1, event, Ev), _).


		 /*******************************
		 *	  FIX DELEGATION	*
		 *******************************/

file(M, File:file*) :<-
	"Return associated file"::
	get(M?text_buffer, file, File).


		 /*******************************
		 *	PROMPTING ARGUMENTS	*
		 *******************************/

prompt(M, Label:char_array, Default:[any], Type:[type], Rval:any) :<-
	"Prompt for a value in the mini-window"::
	get(M?frame, prompt, Label, Default, Type, Rval).


prompt_using(M, Item:graphical, Rval:any) :<-
	"Prompt using dialog item in the mini-window"::
	get(M?frame, prompt_using, Item, Rval).


interactive_argument(M, Implementation:any, Which:int, Value:any) :<-
	"Prompt for interactive argument of specified type"::
	send(M, open_history, Implementation),
	get(M, frame, EmacsWindow),
	get(Implementation, argument_type, Which, Type),
	send(M, m_x_argn, Which),
	(   get(M, m_x_index, Idx), Idx \== @nil % use M-x History!
	->  get(M, m_x_history, History),
	    get(History, nth1, Idx, HistArgv),
	    get(HistArgv, element, Which, DefaultValue)
	->  true
	;   get(M, selected, Selection),
	    get(Type, check, Selection, DefaultValue)
	->  true
	;   DefaultValue = @default
	),
	(   get(Type, argument_name, Label), Label \== @nil
	->  true
	;   get(Type, name, Label)
	),
	get(EmacsWindow, prompt, Label, DefaultValue, Type, Value).


m_x_previous(M, Value:any) :<-
	"Read next value from the M-x history"::
	get(M, m_x_index, Idx),
	(   Idx == @nil
	->  Nidx = 1
	;   Nidx is Idx + 1
	),
	(   get(M, m_x_history, H), H \== @nil,
	    get(H, nth1, Nidx, ArgVector)
	->  get(ArgVector, element, M?m_x_argn, Value),
	    send(M, m_x_index, Nidx)
	;   send(M, report, warning, 'No (more) history'),
	    fail
	).

	
m_x_next(M, Value:any) :<-
	"Read previous value from the M-x history"::
	get(M, m_x_index, Idx),
	(   (Idx == @nil ; Idx =< 1)
	->  send(M, report, warning, 'Back at start'),
	    fail
	;   Nidx is Idx - 1
	),
	get(M?m_x_history, nth1, Nidx, ArgVector),
	get(ArgVector, element, M?m_x_argn, Value),
	send(M, m_x_index, Nidx).
	

		 /*******************************
		 *	      REPORT		*
		 *******************************/

report_to(M, E:editor) :<-
	"Send reports to the <-editor"::
	get(M, editor, E).

:- pce_end_class.


:- pce_begin_class(emacs_mode_menu(name), sheet).

:- pce_global(@emacs_mode_menus, new(hash_table)). % name ---> mode_menu object

variable(name,		name,		get,	"Name of the menu").

initialise(MM, Name:name, Super:[emacs_mode_menu]) :->
	"Create from name and super (default) menu"::
	send(MM, send_super, initialise),
	send(MM, slot, name, Name),
	send(@emacs_mode_menus, append, Name, MM),
	send(MM, protect),
	(   Super \== @default
	->  send(Super, for_all,
		 message(MM, value, @arg1?name, @arg1?value?copy))
	;   true
	).


lookup(_, Name:name, _Super:emacs_mode_menu, MM) :<-
	"Reuse existing mode menu"::
	get(@emacs_mode_menus, member, Name, MM).


convert(_, Name:name, MM:emacs_mode_menu) :<-
	"Convert name to mode-menu object"::
	(   get(@emacs_mode_menus, member, Name, MM)
	->  true
	;   get(@pce, convert, string('emacs_%s_mode', Name), class, Class),
	    get(Class, name, ClassName),
	    mode_menu_name(ClassName, MenuName),
	    get(@emacs_mode_menus, member, MenuName, MM)
	).


append(MM, Name:name, Action:'name|menu_item', Before:[name]) :->
	"Append item for specified menu"::
	(   get(MM, value, Name, Chain)
	->  true
	;   send(MM, value, Name, new(Chain, chain))
	),
        (   send(Chain, member, Action)
	->  true
	;   send(Chain, append, Action)
	),
	(   Before \== @default
	->  ignore(send(Chain, move_before, Action, Before))
	;   true
	).

:- pce_end_class.


		 /*******************************
		 *	   KEY-BINDINGS		*
		 *******************************/

:- pce_begin_class(emacs_key_binding, key_binding,
		   "Specialised key_binding for history").

key_binding_name(@default, @default) :- !.
key_binding_name(@nil, @nil) :- !.
key_binding_name(KB, Name) :-
	object(KB),
	get(KB, name, KBName), !,
	key_binding_name(KBName, Name).
key_binding_name(editor, editor) :- !.
key_binding_name(X, Internal) :-
	concat('emacs$', X, Internal).


initialise(KB, Name:[name]*, Super:[key_binding]) :->
	default(Super, editor, S),
	key_binding_name(Name, IName),
	key_binding_name(S, IS),
	send(KB, send_super, initialise, IName, IS).

convert(_, IName:name, KB:emacs_key_binding) :<-
	"Handle mapped names"::
	key_binding_name(Name, IName),
	get(type(key_binding), convert, Name, KB).

lookup(_, Name:name, _Super:[key_binding], KB:emacs_key_binding) :<-
	"Find existing mappings"::
	key_binding_name(Name, IName),
	get(@key_bindings, member, IName, KB).

fill_arguments_and_execute(KB, Id:event_id, Receiver:emacs_mode,
			   Selector:name, Argv:any ...) :->
	"Open/close the argument processing"::
	send(@current_emacs_mode, assign, Receiver),
	(   get(Receiver, send_method, Selector, tuple(_, Impl)),
	    get(Impl, attribute, emacs_history, _)
	->  send(Receiver, open_history, Impl, @on)
	;   send(Receiver, close_history)
	),
	send(KB, send_super_vector,
	     fill_arguments_and_execute, Id, Receiver, Selector, Argv).

execute(KB, Receiver:emacs_mode, Selector:name, Argv:any ...) :->
	"Push history if available"::
	(   get(Receiver, m_x_history, @nil)
	->  true
	;   send(Receiver, close_history, Argv?copy)
	),
	send(KB, send_super_vector, execute, Receiver, Selector, Argv).

:- pce_end_class.


		 /*******************************
		 *	   ARGUMENT ITEM	*
		 *******************************/

:- pce_begin_class(emacs_argument_item, menu_item,
		   "Item with pullright for args").

initialise(I, Name:name, ValueSet:'chain|function') :->
	send(I, send_super, initialise, Name),
	send(I, popup,
	     new(P, emacs_argument_popup(Name,
					 message(@emacs_mode, arg_call,
						 Name, @arg1)))),
	(   send(ValueSet, '_instance_of', chain)
	->  send(P, members, ValueSet)
	;   send(P, update_message,
		 message(@receiver, members, ValueSet))
	).

:- pce_end_class.


:- pce_begin_class(emacs_argument_popup, popup,
		   "Emacs mode menu pullright popup").

members(I, Members:chain) :->
	"->clear and attach new members (do not capitalise)"::
	get(Members, map,
	    create(menu_item, @arg1, @default, @arg1?print_name),
	    Items),
	send(I, send_super, members, Items).

:- pce_end_class.
