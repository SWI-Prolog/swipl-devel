/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1997 University of Amsterdam. All rights reserved.
*/

:- module(pce_config_editor, []).
:- use_module(library(pce)).
:- use_module(library(pce_config)).

:- pce_autoload(toc_window,		library(pce_toc)).
:- pce_autoload(font_item,		library(pce_font_item)).
:- pce_autoload(file_item,		library(file_item)).
:- pce_autoload(colour_item,		library(pce_colour_item)).
:- pce_autoload(colour_palette_item,	library(pce_colour_item)).
:- pce_autoload(directory_item, 	library(directory_item)).
:- pce_autoload(set_item,		library(pce_set_item)).

		 /*******************************
		 *	      TYPES		*
		 *******************************/

builtin_config_type(bool,		[ editor(config_bool_item),
					  term(map([@off=false, @on=true]))
					]).
builtin_config_type(font,		[ editor(font_item),
					  term([family, style, points]),
					  icon('16x16/font.xpm')
					]).
builtin_config_type(colour,		[ editor(colour_item),
					  term(name, @arg1?kind == named),
					  term([@default, red, green, blue])
					]).
builtin_config_type(setof(colour),	[ editor(colour_palette_item),
					  icon('16x16/cpalette2.xpm')
					]).
builtin_config_type(file,		[ editor(file_item)
					]).
builtin_config_type(directory,		[ editor(directory_item)
					]).
builtin_config_type(_,			[ editor(config_generic_item)
					]).

config_type(Type, Attributes) :-
	current_config_type(Type, Attributes).
config_type(Type, Attributes) :-
	builtin_config_type(Type, Attributes).

config_attribute(Key, Attribute) :-
	config_attributes(Key, Attributes),
	(   memberchk(Attribute, Attributes)
	->  true
	;   memberchk(type(Type), Attributes),
	    config_type(Type, TypeAttributes),
	    memberchk(Attribute, TypeAttributes)
	).


		 /*******************************
		 *	    TMP-CONFIG		*
		 *******************************/

:- dynamic
	tmp_config/2.			% +Key, +Value

set_tmp_config(Key, Value) :-
	retractall(tmp_config(Key, _)),
	asserta(tmp_config(Key, Value)).

get_tmp_config(Key, Value) :-
	tmp_config(Key, RawValue), !,
	Value = RawValue.
get_tmp_config(Key, Value) :-
	get_config(Key, Value).


		 /*******************************
		 *	     CLASSES		*
		 *******************************/


:- pce_begin_class(pce_config_editor, frame,
		   "library(pce_config): configuration editor").

variable(module,	name,	get, "Viewed module").
variable(current_id,	chain*,	get, "Id of current config key").

initialise(F, M:name) :->
	retractall(tmp_config(M:_, _)),
	send(F, send_super, initialise, 'Properties'),
	send(F, slot, module, M),
	send(F, append, new(D1, dialog)),
	send(D1, size, size(400,300)),
	send(D1, hor_shrink, 100),
	send(D1, hor_stretch, 100),
	send(D1, ver_shrink, 100),
	send(D1, ver_stretch, 100),
	send(new(D2, dialog), below, D1),
	send(new(TOC, pce_config_toc(M)), left, D1),
	send(D1, name, value_dialog),
	send(D2, append, button(ok,	 and(message(F, ok),
					     message(F, destroy)))),
	send(D2, append, button(cancel,  message(F, cancel))),
	send(D2, append, button(restore, message(F, defaults))),
	send(D2, append, button(save, 	 message(F, save))),
	send(TOC, expand_root).
	
:- pce_group(user_actions).


cancel(F) :->
	"Cancel (restore old settings)"::
	get(F, module, M),
	retractall(tmp_config(M:_, _)),
	send(F, destroy).

save(F) :->
	send(F, ok),
	get(F, module, M),
	save_config(M:_DefaultFile).

ok(F) :->
	"OK: make changes permanent"::
	send(F, save_if_modified),
	get(F, module, M),
	forall(retract(tmp_config(M:Path, Value)),
	       set_config(M:Path, Value)).
	

config_base_name(_/Base, Base) :- !.
config_base_name(Base, Base).

defaults(F) :->
	"Restore (factory) defaults"::
	(   get(F, current_id, Id),
	    chain_to_path(Id, CPath)
	->  config_base_name(CPath, Current)
	;   Current = current
	),
	new(D, dialog('Restore defaults')),
	send(D, append, label(comment, 'Restore application defaults?')),
	send(D, append, button(all_settings, message(D, return, all))),
	send(D, append, new(RC, button(Current, message(D, return, current)))),
	send(D, append, button(cancel, message(D, return, cancel))),
	send(D, transient_for, F),
	send(D, modal, transient),
	get(F, module, M),
	(   nonvar(CPath),
	    config_attributes(M:CPath, CAtts),
	    memberchk(default(CDef), CAtts)
	->  true
	;   send(RC, active, @off)
	),
	get(D, confirm_centered, F?area?center, Result),
	send(D, destroy),
	(   Result == all
	->  forall((config_attributes(M:Path, Atts),
	    	    memberchk(default(Def), Atts)),
		   set_tmp_config(M:Path, Def))
	;   Result == current
	->  set_tmp_config(M:CPath, CDef)
	;   Result == cancel
	->  fail
	),
	(   nonvar(CPath)
	->  get_tmp_config(M:CPath, Value0),
	    get(F, member, value_dialog, VD),
	    get(VD, config_item, Item),
	    (	is_list(Value0)
	    ->	chain_list(Value, Value0)
	    ;	Value = Value0
	    ),
	    send(Item, selection, Value)
	;   true
	).

:- pce_group(save).

save_if_modified(F) :->
	"Save (temporary) value if config has modified"::
	(   get(F, current_id, Id),
	    Id \== @nil,
	    get(F, member, value_dialog, D),
	    get(D, attribute, config_item, Item),
	    get(Item, modified, @on)
	->  get(Item, selection, NewValue),
	    value_to_prolog(NewValue, PlValue),
	    chain_to_path(Id, Path),
	    get(F, module, M),
	    set_tmp_config(M:Path, PlValue)
	;   true
	).


value_to_prolog(@off, false) :- !.
value_to_prolog(@on, true) :- !.
value_to_prolog(Chain, List) :-
	send(Chain, instance_of, chain), !,
	chain_list(Chain, List0),
	maplist(value_to_prolog, List0, List).
value_to_prolog(Obj, Term) :-
	object(Obj),
	get(Obj, class_name, ClassName),
	term_description(ClassName, Attributes, Condition),
	send(Condition, forward, Obj),
	value_to_prolog(Attributes, Obj, Term).
value_to_prolog(Obj, Term) :-
	object(Obj),
	get(Obj, class_name, ClassName),
	term_description(ClassName, Attributes),
	value_to_prolog(Attributes, Obj, Term).
value_to_prolog(V, V).

value_to_prolog(map(Mapping), Obj, Term) :- !,
	memberchk(Obj=Term, Mapping).
value_to_prolog(Attributes, Obj, Term) :-
	is_list(Attributes), !,
	get(Obj, class_name, ClassName),
	maplist(prolog_value_argument(Obj), Attributes, InitArgs),
	Term =.. [ClassName|InitArgs].
value_to_prolog(Attribute, Obj, Term) :-
	prolog_value_argument(Obj, Attribute, Term).

					% unconditional term descriptions
term_description(Type, TermDescription) :-
	config_type(Type, Attributes),
	member(term(TermDescription), Attributes).
term_description(Type, TermDescription, Condition) :-
	config_type(Type, Attributes),
	member(term(TermDescription, Condition), Attributes).

prolog_value_argument(Obj, Arg, ArgTerm) :-
	atom(Arg), !,
	get(Obj, Arg, V0),
	value_to_prolog(V0, ArgTerm).
prolog_value_argument(Obj, Arg, Value) :-
	functor(Arg, ?, _),
	get(Arg, '_forward', Obj, Value).
prolog_value_argument(_, Arg, Arg).

select_key(F, Id:chain) :->
	"Show selected key"::
	send(F, save_if_modified),
	send(F, slot, current_id, Id),
	chain_to_path(Id, Path),
	get(F, module, M),
	get(F, member, value_dialog, D),
	make_config_item(M:Path, Item),
	send(D, clear),
	(   config_attributes(M:Path, Attributes),
	    memberchk(comment(Comment), Attributes)
	->  (   is_list(Comment)
	    ->	new(C, string),
		forall(member(C0, Comment), send(C, append, C0))
	    ;	C = Comment
	    ),
	    get(D, gap, size(GW, _)),
	    get(D, width, DW),
	    TW is DW - 2*GW,
	    new(Lbl, text(C, left, normal)),
	    send(Lbl, margin, TW, wrap),
	    send(D, append, Lbl)
	;   send(D, append, new(graphical))
	),
	send(D, append, Item),
	send(D, attribute, config_item, Item),
	send(D, append, new(graphical)),
	send(D, layout, D?size).

:- pce_end_class.

:- pce_begin_class(pce_config_toc(module), toc_window,
		   "Properties hierarchy").

initialise(T, Module:name) :->
	send(T, send_super, initialise),
	config_attributes(Module:config, AppAtts),
	memberchk(application(AppName), AppAtts),
	send(T, root, toc_folder(AppName, new(chain))).

expand_node(T, Id:chain) :->
	chain_to_path(Id, Path),
	get(T, module, M),		% delegates to <-frame
	findall(Sub, dir_path_members(M, Path, Sub), Subs),
	expand_nodes(Subs, [], T, Id).

dir_path_members(M, Super, Sub) :-
	config_attributes(M:Path, _),
	path_prefix(Super, Path, Sub).

path_prefix(-, Sub, Sub).
path_prefix(Pref, Pref/Sub, Sub).
path_prefix(Pref, Super/Sub0, Sub/Sub0) :-
	path_prefix(Pref, Super, Sub).

expand_nodes([], _, _, _).
expand_nodes([H|T], Done, Tree, Parent) :-
	atom(H),
	get(Parent, copy, Id),
	send(Id, append, H),
	chain_to_path(Id, Path),
	get(Tree, module, M),
	(   config_attribute(M:Path, icon(Icon))
	->  new(Image, image),
	    send(Image, load, Icon)
	;   Image = @default
	),
	send(Tree, son, Parent, toc_file(H?label_name, Id, Image)),
	expand_nodes(T, Done, Tree, Parent).
expand_nodes([H|T], Done, Tree, Parent) :-
	root(H, Root),
	(   memberchk(Root, Done)
	->  expand_nodes(T, Done, Tree, Parent)
	;   get(Parent, copy, Id),
	    send(Id, append, Root),
	    send(Tree, son, Parent, toc_folder(Root?label_name, Id)),
	    expand_nodes(T, [Root|Done], Tree, Parent)
	).

root(A/_, R) :- !,
	root(A, R).
root(A, A).

select_node(T, Id:chain) :->
	send(T?frame, select_key, Id).

chain_to_path(Chain, Path) :-
	chain_list(Chain, List),
	list_to_path(List, Path).

list_to_path([], -).
list_to_path([H|T], Path) :- !,
	list_to_path2(T, H, Path).

list_to_path2([], P, P).
list_to_path2([H|T], P0, P) :-
	list_to_path2(T, P0/H, P).

:- pce_end_class.

		 /*******************************
		 *	      ITEMS		*
		 *******************************/

make_config_item(Key, Item) :-
	config_attributes(Key, Attributes),
	memberchk(type(Type), Attributes),
	(   Key = _:_/Name
	;   Key = _:Name
	), !,
	(   get_tmp_config(Key, Value0)
	->  (   is_list(Value0)
	    ->  chain_list(Value, Value0)
	    ;   Value = Value0
	    )
	;   Value = @default
	),
	make_item(Type, Name, Value, Item).


make_item(Type, Label, Value, Item) :-
	config_editor_class(Type, Class),
	Class \== config_generic_item, !,
	Term =.. [Class, Label, Value],
	new(Item, Term).
make_item(setof(ItemType), Label, Value, Item) :- !,
	make_item(ItemType, '', @default, SingleItem),
	new(Item, set_item(SingleItem, Label)),
	(   Value \== @default
	->  send(Item, selection, Value)
	;   true
	).
make_item(Type, Label, Value, Item) :-
	config_editor_class(Type, Class),
	Term =.. [Class, Label, Value],
	new(Item, Term),
	(   Class == config_generic_item
	->  send(Item, config_type, Type)
	;   true
	).

config_editor_class(Type, Class) :-
	config_type(Type, Attributes),
	memberchk(editor(Class), Attributes).


		 /*******************************
		 *	      BOOLEAN		*
		 *******************************/


:- pce_begin_class(config_bool_item, menu,
		   "Edit boolean switch").

initialise(I, Name:name, Default:[bool]) :->
	send(I, send_super, initialise, Name, choice),
	send(I, append, menu_item(@on, @default, true)),
	send(I, append, menu_item(@off, @default, false)),
	send(I, layout, horizontal),
	(   Default \== @default
	->  send(I, selection, Default)
	;   true
	),
	send(I, modified, @off).	% should be in class menu!?

selection(I, Selection:bool) :->	% force type conversion
	send(I, send_super, selection, Selection).

:- pce_end_class.


		 /*******************************
		 *      CONFIG-GENERIC-ITEM	*
		 *******************************/

:- pce_begin_class(config_generic_item, text_item,
		   "Most primitive item").

variable(config_type, name, both, "Registered type").

initialise(I, Name:name, Initial:[any]) :->
	default(Initial, '', Value),
	value_to_text(Value, Text),
	send(I, send_super, initialise, Name, Text).

selection(I, Sel:any) :->
	value_to_text(Sel, Text),
	send(I, send_super, selection, Text).
selection(I, Sel:any) :<-
	get(I, get_super, selection, Text),
	get(I, config_type, Type),
	text_to_value(Type, Text, Sel).

:- pce_end_class.

value_text(@on, true).
value_text(@on, false).
value_text(@nil, nil).

value_to_text(Obj, Text) :-
	value_text(Obj, Text), !.
value_to_text(Text, Text) :-
	send(Text, instance_of, char_array), !.
value_to_text(Obj, Text) :-
	get(Obj, print_name, Text).

text_to_value(_, Text, Obj) :-
	value_text(Obj, Text), !.
text_to_value(Type, Text, Obj) :-
	get(@pce, convert, Type, class, _Class), !,
	(   get(@pce, convert, Text, Type, Obj)
	->  true
	;   term_to_atom(Term, Text),
	    new(Obj0, Term),
	    get(@pce, convert, Obj0, Type, Obj)
	).
