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

:- module(pce_config_editor, []).
:- use_module(library(pce)).
:- use_module(library(pce_config)).
:- require([ chain_list/2
	   , default/3
	   , forall/2
	   , is_list/1
	   , member/2
	   , memberchk/2
	   , term_to_atom/2
	   ]).

:- pce_autoload(toc_window,		library(pce_toc)).
:- pce_autoload(font_item,		library(pce_font_item)).
:- pce_autoload(file_item,		library(file_item)).
:- pce_autoload(colour_item,		library(pce_colour_item)).
:- pce_autoload(colour_palette_item,	library(pce_colour_item)).
:- pce_autoload(directory_item, 	library(file_item)).
:- pce_autoload(set_item,		library(pce_set_item)).

		 /*******************************
		 *	      TYPES		*
		 *******************************/

config_attribute(Key, Attribute) :-
	config_attributes(Key, Attributes),
	(   memberchk(Attribute, Attributes)
	->  true
	;   memberchk(type(Type), Attributes),
	    current_config_type(Type, _DefModule, TypeAttributes),
	    memberchk(Attribute, TypeAttributes)
	).

config_icon(Key, Icon) :-
	config_attributes(Key, Attributes),
	memberchk(type(Type), Attributes),
	current_config_type(Type, DefModule, TypeAttributes),
	memberchk(icon(Name), TypeAttributes),
	(   new(Icon, DefModule:resource(Name, image))
	;   Icon = Name
	).


		 /*******************************
		 *	    TMP-CONFIG		*
		 *******************************/

:- dynamic
	tmp_config/3.			% +Key, +Value

set_tmp_config(Key, Value) :-
	(   retract(tmp_config(Key, _, Type))
	->  true
	;   config_attributes(Key, Attributes),
	    memberchk(type(Type), Attributes)
	),
	assert(tmp_config(Key, Value, Type)).

get_tmp_config(Key, Value) :-
	tmp_config(Key, RawValue, Type), !,
	config_term_to_object(Type, RawValue, Value).
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
	retractall(tmp_config(M:_, _, _)),
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
	retractall(tmp_config(M:_, _, _)),
	send(F, destroy).

save(F) :->
	send(F, ok),
	get(F, module, M),
	save_config(M:_DefaultFile).

ok(F) :->
	"OK: make changes permanent"::
	send(F, save_if_modified),
	get(F, module, M),
	forall(retract(tmp_config(M:Path, Value, Type)),
	       pce_config:set_config_term(M, Path, Value, Type)). % export?
	

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
	    get(F, module, M),
	    chain_to_path(Id, Path),
	    config_attributes(M:Path, Attributes),
	    memberchk(type(Type), Attributes),
	    config_term_to_object(Type, PlValue, NewValue),
	    set_tmp_config(M:Path, PlValue)
	;   true
	).


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
		forall(member(C0, Comment),
		       (   send(C, ensure_suffix, ' '),
			   send(C, append, C0)
		       ))
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
	(   get_config(Module:config/application, AppName)
	->  true
	;   AppName = application
	),
	send(T, root, toc_folder(AppName?label_name, new(chain))).

expand_node(T, Id:chain) :->
	chain_to_path(Id, Path),
	get(T, module, M),		% delegates to <-frame
	findall(Sub, dir_path_members(M, Path, Sub), Subs),
	expand_nodes(Subs, [], T, Id).

dir_path_members(M, Path, Sub) :-
	config_attributes(M:Path/Sub, Attributes),
	atom(Sub),
	memberchk(type(_), Attributes).
dir_path_members(M, Super, tree(Sub)) :-
	config_attributes(M:Path, Attributes),
	memberchk(type(_), Attributes),
	path_prefix(Super, Path, Sub0),
	\+ atom(Sub0),
	root(Sub0, Sub).

path_prefix(-, Sub, Sub).
path_prefix(Pref, Pref/Sub, Sub).
path_prefix(Pref, Super/Sub0, Sub/Sub0) :-
	path_prefix(Pref, Super, Sub).

root(A/_, R) :- !,
	root(A, R).
root(A, A).

expand_nodes([], _, _, _).
expand_nodes([H|T], Done, Tree, Parent) :-
	memberchk(H, Done), !,
	expand_nodes(T, Done, Tree, Parent).
expand_nodes([tree(H)|T], Done, Tree, Parent) :- !,
	get(Parent, copy, Id),
	send(Id, append, H),
	send(Tree, son, Parent, toc_folder(H?label_name, Id)),
	expand_nodes(T, [tree(H)|Done], Tree, Parent).
expand_nodes([H|T], Done, Tree, Parent) :-
	atom(H),
	get(Parent, copy, Id),
	send(Id, append, H),
	chain_to_path(Id, Path),
	get(Tree, module, M),
	(   config_icon(M:Path, Source),
	    new(Image, image),
	    send(Image, load, Source)
	->  true
	;   Image = @default
	),
	send(Tree, son, Parent, toc_file(H?label_name, Id, Image)),
	expand_nodes(T, [H|Done], Tree, Parent).

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


make_item({}(Names), Label, Value, Item) :- !,
	curl_to_chain(Names, Chain),
	new(Item, text_item(Label, Value)),
	send(Item, value_set, Chain).
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
	current_config_type(Type, _, Attributes),
	memberchk(editor(Class), Attributes).


		 /*******************************
		 *	      BOOLEAN		*
		 *******************************/


:- pce_begin_class(config_bool_item, menu,
		   "Edit boolean switch").

initialise(I, Name:name, Default:[bool]) :->
	send(I, send_super, initialise, Name, choice),
	send(I, append, menu_item(@on, @default, true?label_name)),
	send(I, append, menu_item(@off, @default, false?label_name)),
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
		 *      CONFIG-ONE-OF-ITEM	*
		 *******************************/

curl_to_chain(Term, Chain) :-
	new(Chain, chain),
	curl_to_chain_(Term, Chain).

curl_to_chain_((A,B), Chain) :- !,
	curl_to_chain_(A, Chain),
	curl_to_chain_(B, Chain).
curl_to_chain_(A, Chain) :-
	send(Chain, append, A).


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
