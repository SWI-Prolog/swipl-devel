/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1997 University of Amsterdam. All rights reserved.
*/

:- module(pce_config,
	  [ register_config/1,		% +PredicateName
	    register_config_type/2,	% +Type, +Attributes
					% fetch/set
	    get_config/2,		% +Key, -Value
	    set_config/2,		% +Key, +Value
	    add_config/2,		% +Key, +Value
	    del_config/2,		% +Key, +Value
					% edit/save/load
	    edit_config/1,		% +Graphical
	    save_config/1,		% +File
	    load_config/1,		% +File
	    ensure_loaded_config/1,	% +File
					% Type conversion
	    config_term_to_object/2,	% ?Term, ?Object
	    config_term_to_object/3,	% +Type, ?Term, ?Object
					% +Editor interface
	    config_attributes/2,	% ?Key, -Attributes
	    current_config_type/2	% +Type, -Attributes
	  ]).

:- meta_predicate
	register_config(:),
	register_config_type(:, +),
	current_config_type(:, +),
	get_config_type(:, -),
	get_config_term(:, -, -),
	get_config(:, -),
	set_config(:, +),
	add_config(:, +),
	del_config(:, +),
	save_config(:),
	load_config(:), 
	ensure_loaded_config(:),
	edit_config(:),
	config_attributes(:, -).

:- asserta(user:library_directory(.)).	% testing

:- use_module(library(pce)).
:- use_module(library(broadcast)).
:- pce_autoload(pce_config_editor,	library(pce_configeditor)).

:- multifile user:file_search_path/2.
:- dynamic   user:file_search_path/2.

user:file_search_path(config, ConfigHome) :-
	expand_file_name(~, [Home]),
	concat_atom([Home, /, '.xpce'], ConfigHome).

version(1).				% version of the config package

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Database
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- dynamic
	config_type/3,			% Type, Module, Attributes
	config_db/2,			% DB, Predicate
	config_store/4.			% DB, Path, Value, Type

lasserta(Term) :-
	asserta(Term).
lretract(Term) :-
	retract(Term).
lretractall(Term) :-
	retractall(Term).

	
		 /*******************************
		 *	     REGISTER		*
		 *******************************/

register_config(Spec) :-
	strip_module(Spec, Module, Pred),
	(   config_db(Module, Pred)
	->  true
	;   lasserta(config_db(Module, Pred))
	).


		 /*******************************
		 *		QUERY		*
		 *******************************/

get_config_type(Key, Type) :-
	strip_module(Key, DB, Path),
	config_db(DB, Pred),
	call(DB:Pred, Path, Attributes),
	memberchk(type(Type), Attributes).


get_config(Key, Value) :-
	strip_module(Key, DB, Path),
	config_store(DB, Path, Value0, Type), !,
	config_term_to_object(Type, Value0, Value).
get_config(Key, Value) :-
	config_attribute(Key, default(Default)), !,
	(   config_attribute(Key, type(Type))
	->  strip_module(Key, DB, Path),
	    lasserta(config_store(DB, Path, Default, Type)),
	    config_term_to_object(Type, Default, Value)
	;   Value = Default
	).
	    

get_config_term(Key, Term, Type) :-
	strip_module(Key, DB, Path),
	config_store(DB, Path, Term, Type).


		 /*******************************
		 *	       MODIFY		*
		 *******************************/

set_config(Key, Value) :-
	strip_module(Key, DB, Path),
	set_config_(DB, Path, Value),
	set_modified(DB),
	broadcast(set_config(Key, Value)).

set_config_(DB, Path, Value) :-		% local version
	(   lretract(config_store(DB, Path, _, Type))
	->  true
	;   get_config_type(DB:Path, Type)
	),
	config_term_to_object(Type, TermValue, Value),
	lasserta(config_store(DB, Path, TermValue, Type)).

set_config_term(DB, Path, Term, Type) :- % loaded keys
	retractall(config_store(DB, Path, _, _)),
	asserta(config_store(DB, Path, Term, Type)),
	config_term_to_object(Type, Term, Value), % should we broadcast?
	broadcast(set_config(DB:Path, Value)).

set_config_(DB, Path, Value, Type) :-	% local version
	retractall(config_store(DB, Path, _, _)),
	asserta(config_store(DB, Path, Value, Type)).

add_config(Key, Value) :-
	strip_module(Key, DB, Path),
	(   lretract(config_store(DB, Path, Set0, Type)),
	    is_list(Set0)
	->  (   delete(Set0, Value, Set1)
	    ->	Set = [Value|Set1]
	    ;	Set = [Value|Set0]
	    )
	;   retractall(config_store(DB, Path, _, _)), % make sure
	    get_config_type(Key, Type),
	    Set = [Value]
	),
	lasserta(config_store(DB, Path, Set, Type)),
	set_modified(DB).

del_config(Key, Value) :-
	strip_module(Key, DB, Path),
	config_store(DB, Path, Set0, Type),
	delete(Set0, Value, Set),
	lretract(config_store(DB, Path, Set0, Type)), !,
	lasserta(config_store(DB, Path, Set, Type)),
	set_modified(DB).

set_modified(DB) :-
	config_store(DB, '$modified', true, _), !.
set_modified(DB) :-
	asserta(config_store(DB, '$modified', true, bool)).

clear_modified(DB) :-
	retractall(config_store(DB, '$modified', _, _)).


		 /*******************************
		 *	      META		*
		 *******************************/

%	config_attributes(+Key, -Attributes)
%
%	Fetch the (meta) attributes of the given config key.  The special
%	path `config' returns information on the config database itself.
%	The path of the key may be partly instantiated.

config_attributes(Key, Attributes) :-
	strip_module(Key, DB, Path),
	config_db(DB, Pred),
	call(DB:Pred, Path, Attributes).

config_attribute(Key, Attribute) :-
	var(Attribute), !,
	config_attributes(Key, Attributes),
	member(Attribute, Attributes).
config_attribute(Key, Attribute) :-
	config_attributes(Key, Attributes),
	memberchk(Attribute, Attributes), !.

current_config_path(Key) :-
	strip_module(Key, DB, Path),
	findall(P, config_path(DB, P), Ps0),
	list_to_set(Ps0, Ps),
	member(Path, Ps).

config_path(DB, Path) :-
	config_db(DB, Pred),
	call(DB:Pred, Path, Attributes),
	memberchk(type(_), Attributes).
	



		 /*******************************
		 *	       SAVE		*
		 *******************************/

save_file(Key, File) :-
	is_absolute_file_name(Key), !,
	File = Key.
save_file(Key, File) :-
	absolute_file_name(config(Key),
			   [ access(write),
			     extensions([cnf])
			   ], File), !.
save_file(Key, File) :-
	absolute_file_name(config(Key),
			   [ extensions([cnf])
			   ], File), !,
	file_directory_name(File, Dir),
	(   send(directory(Dir), exists)
	->  send(@pce, report, error, 'Cannot write config directory %s', Dir),
	    fail
	;   new(D, dialog('Create config directory')),
	    send(D, append,
		 label(comment, string('Create config directory %s?', Dir))),
	    send(D, append, button(ok, message(D, return, ok))),
	    send(D, append, button(cancel, message(D, return, cancel))),
	    get(D, confirm_centered, RVal),
	    send(D, destroy),
	    RVal == ok,
	    send(directory(Dir), make)
	).


save_config(Spec) :-
	strip_module(Spec, M, Key),
	(   var(Key)
	->  get_config(M:config/file, Key)
	;   true
	),
	save_file(Key, File),
	open(File, write, Fd),
	save_config_header(Fd, M),
	save_config_body(Fd, M),
	close(Fd).

save_config_header(Fd, M) :-
	get(@pce?date, value, Date),
	get(@pce, user, User),
	version(Version),
	format(Fd, '/*  XPCE configuration file for "~w"~n', [M]),
	format(Fd, '    Saved ~w by ~w~n', [Date, User]),
	format(Fd, '*/~n~n', []),
	format(Fd, 'configversion(~q).~n', [Version]),
	format(Fd, '[~q].~n~n', [M]),
	format(Fd, '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%~n', []),
	format(Fd, '% Option lines starting with a `%'' indicate      %~n',[]),
	format(Fd, '% the value is equal to the application default. %~n', []),
	format(Fd, '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%~n', []).
	
save_config_body(Fd, M) :-
	forall(current_config_path(M:Path),
	       save_config_key(Fd, M:Path)).

save_config_key(Fd, Key) :-
	config_attribute(Key, comment(Comment)),
	nl(Fd),
	(   is_list(Comment)
	->  format_comment(Comment, Fd)
	;   format_comment([Comment], Fd)
	),
	fail.
save_config_key(Fd, Key) :-
	get_config_term(Key, Value, _Type), !,
	strip_module(Key, _, Path),
	(   (   config_attribute(Key, default(Value0))
	    ->	Value = Value0
	    )
	->  format(Fd, '%~q = ~t~32|~q.~n', [Path, Value])
	;   format(Fd, '~q = ~t~32|~q.~n',  [Path, Value])
	).
save_config_key(_,_).

format_comment([], _).
format_comment([H|T], Fd) :-
	format(Fd, '/* ~w */~n', [H]),
	format_comment(T, Fd).

save_modified_configs :-
	config_db(DB, _Pred),
	get_config(DB:'$modified', true),
	clear_modified(DB),
	get_config(DB:config/file, Key),
	send(@pce, report, status, 'Saving config database %s', Key),
	save_config(DB:_DefaultFile),
	fail.
save_modified_configs.

:- initialization
   send(@pce, exit_message, message(@prolog, save_modified_configs)).

	
		 /*******************************
		 *	       LOAD		*
		 *******************************/

ensure_loaded_config(Spec) :-
	strip_module(Spec, M, _Key),
	config_store(M, _Path, _Value, _Type), !.
ensure_loaded_config(Spec) :-
	load_config(Spec).

load_file(Key, File) :-
	is_absolute_file_name(Key), !,
	File = Key.
load_file(Key, File) :-
	absolute_file_name(config(Key),
			   [ access(read),
			     extensions([cnf])
			   ], File).

load_key(_DB, Key) :-
	nonvar(Key), !.
load_key(DB, Key) :-
	get_config(DB:config/file, Key), !.


load_config(Spec) :-
	strip_module(Spec, M, Key),
	load_key(M, Key),
	load_file(Key, File), !,
	open(File, read, Fd),
	read_config_file(Fd, _SaveVersion, _SaveModule, Bindings),
	close(Fd),
	load_config_keys(M, Bindings),
	set_config_(M, config/file, File, file),
	clear_modified(M).
load_config(Spec) :-			% no config file, use defaults
	strip_module(Spec, M, Key),
	load_key(M, Key),
%	load_config_keys(M, []),
	set_config_(M, config/file, Key, file),
	clear_modified(M).		% or not, so we save first time?


read_config_file(Fd, SaveVersion, SaveModule, Bindings) :-
	read(Fd, configversion(SaveVersion)),
	read(Fd, [SaveModule]),
	read(Fd, Term),
	read_config_file(Term, Fd, Bindings).

read_config_file(end_of_file, _, []) :- !.
read_config_file(Binding, Fd, [Binding|T]) :-
	read(Fd, Term),
	read_config_file(Term, Fd, T).

load_config_keys(DB, Bindings) :-
	forall(current_config_path(DB:Path),
	       load_config_key(DB:Path, Bindings)).

load_config_key(Key, Bindings) :-
	strip_module(Key, DB, Path),
	config_attribute(Key, type(Type)),
	(   memberchk(Path=Value, Bindings)
	->  set_config_term(DB, Path, Value, Type)
	;   config_attribute(Key, default(Value))
	->  set_config_term(DB, Path, Value, Type)
	;   true
	).
	

		 /*******************************
		 *	       EDIT		*
		 *******************************/

edit_config(Spec) :-
	strip_module(Spec, M, Graphical),
	make_config_editor(M, Editor),
	(   object(Graphical),
	    send(Graphical, instance_of, visual),
	    get(Graphical, frame, Frame)
	->  send(Editor, transient_for, Frame),
	    send(Editor, modal, transient),
	    send(Editor, open_centered, Frame?area?center)
	;   send(Editor, open_centered)
	).
	    
make_config_editor(M, Editor) :-
	new(Editor, pce_config_editor(M)).


		 /*******************************
		 *	       TYPES		*
		 *******************************/

builtin_config_type(bool,		[ editor(config_bool_item),
					  term(map([@off=false, @on=true]))
					]).
builtin_config_type(font,		[ editor(font_item),
					  term([family, style, points]),
					  icon('16x16/font.xpm')
					]).
builtin_config_type(colour,		[ editor(colour_item),
					  term(if(@arg1?kind == named, name)),
					  term([@default, red, green, blue])
					]).
builtin_config_type(setof(colour),	[ editor(colour_palette_item),
					  icon('16x16/cpalette2.xpm')
					]).
builtin_config_type(image,		[ editor(image_item),
					  term(if(@arg1?name \== @nil, name)),
					  term(@arg1?file?absolute_path)
					]).
builtin_config_type(file,		[ editor(file_item)
					]).
builtin_config_type(directory,		[ editor(directory_item)
					]).
builtin_config_type({}(_),		[ editor(config_one_of_item)
					]).
builtin_config_type(_,			[ editor(config_generic_item)
					]).

register_config_type(TypeSpec, Attributes) :-
	strip_module(TypeSpec, Module, Type),
	(   config_type(Type, Module, Attributes)
	->  true
	;   lasserta(config_type(Type, Module, Attributes))
	).

current_config_type(TypeSpec, Attributes) :-
	strip_module(TypeSpec, Module, Type),
	(   config_type(Type, Module, Attributes)
	->  true
	;   config_type(Type, _, Attributes)
	).
current_config_type(TypeSpec, Attributes) :-
	builtin_config_type(TypeSpec, Attributes).

pce_object_type(setof(Type)) :- !,
	pce_object_type(Type).
pce_object_type(Type) :-
	current_config_type(Type, Attributes),
	memberchk(term(_), Attributes).


		 /*******************************
		 *	 TERM <-> OBJECT	*
		 *******************************/

config_term_to_object(Type, Term, Object) :-
	pce_object_type(Type), !,
	config_term_to_object(Term, Object).
config_term_to_object(_, Value, Value).
	

config_term_to_object(Term, Object) :-
	nonvar(Object), !,
	config_object_to_term(Object, Term).
config_term_to_object(Term, _Object) :-
	var(Term),
	fail.				% raise error!
config_term_to_object(List, Chain) :-
	is_list(List), !,
	maplist(config_term_to_object, List, Objects),
	chain_list(Chain, Objects).
config_term_to_object(Atomic, Atomic) :-
	atomic(Atomic), !.
config_term_to_object(Term+Attribute, Object) :- !,
	Attribute =.. [AttName, AttTerm],
	config_term_to_object(AttTerm, AttObject),
	config_term_to_object(Term, Object),
	send(Object, AttName, AttObject).
config_term_to_object(Term, Object) :-
	new(Object, Term).

%	Object --> Term

config_object_to_term(@off, false) :- !.
config_object_to_term(@on, true) :- !.
config_object_to_term(@Ref, @Ref) :-
	atom(Ref), !.			% global objects!
config_object_to_term(Chain, List) :-
	send(Chain, instance_of, chain), !,
	chain_list(Chain, List0),
	maplist(config_object_to_term, List0, List).
config_object_to_term(Obj, Term) :-
	object(Obj),
	get(Obj, class_name, ClassName),
	term_description(ClassName, Attributes, Condition),
	send(Condition, forward, Obj),
	config_attributes_to_term(Attributes, Obj, Term).
config_object_to_term(Obj, Term) :-
	object(Obj),
	get(Obj, class_name, ClassName),
	term_description(ClassName, Attributes),
	config_attributes_to_term(Attributes, Obj, Term).
config_object_to_term(V, V).

config_attributes_to_term(map(Mapping), Obj, Term) :- !,
	memberchk(Obj=Term, Mapping).
config_attributes_to_term(NewAtts+Att, Obj, Term+AttTerm) :- !,
	config_attributes_to_term(NewAtts, Obj, Term),
	prolog_value_argument(Obj, Att, AttTermVal),
	AttTerm =.. [Att, AttTermVal].
config_attributes_to_term(Attributes, Obj, Term) :-
	is_list(Attributes), !,
	get(Obj, class_name, ClassName),
	maplist(prolog_value_argument(Obj), Attributes, InitArgs),
	Term =.. [ClassName|InitArgs].
config_attributes_to_term(Attribute, Obj, Term) :-
	prolog_value_argument(Obj, Attribute, Term).

					% unconditional term descriptions
term_description(Type, TermDescription) :-
	current_config_type(Type, Attributes),
	member(term(TermDescription), Attributes),
	TermDescription \= if(_,_).
term_description(Type, TermDescription, Condition) :-
	current_config_type(Type, Attributes),
	member(term(if(Condition, TermDescription)), Attributes).

prolog_value_argument(Obj, Arg, ArgTerm) :-
	atom(Arg), !,
	get(Obj, Arg, V0),
	config_object_to_term(V0, ArgTerm).
prolog_value_argument(Obj, Arg, Value) :-
	functor(Arg, ?, _),
	get(Arg, '_forward', Obj, Value).
prolog_value_argument(_, Arg, Arg).

	
