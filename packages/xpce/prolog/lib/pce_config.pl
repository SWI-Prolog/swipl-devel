/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1997 University of Amsterdam. All rights reserved.
*/

:- module(pce_config,
	  [ register_config/2,		% +PredicateName, +Application
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
					% +Editor interface
	    config_attributes/2,	% ?Key, -Attributes
	    current_config_type/2	% +Type, -Attributes
	  ]).

:- meta_predicate
	register_config(:, +),
	register_config_type(:, +),
	current_config_type(:, +),
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
	config_db/3,			% DB, Predicate, Application
	config_store/3.			% DB, Path, Value

config_db(Module, Pred) :-
	config_db(Module, Pred, _App).

lasserta(Term) :-
	asserta(Term).
lretract(Term) :-
	retract(Term).
lretractall(Term) :-
	retractall(Term).

	
		 /*******************************
		 *	     REGISTER		*
		 *******************************/

register_config(Spec, App) :-
	strip_module(Spec, Module, Pred),
	(   config_db(Module, Pred, App)
	->  true
	;   lasserta(config_db(Module, Pred, App))
	).


		 /*******************************
		 *		QUERY		*
		 *******************************/

get_config(Key, Value) :-
	strip_module(Key, DB, Path),
	config_store(DB, Path, Value0), !,
	Value = Value0.


		 /*******************************
		 *	       MODIFY		*
		 *******************************/

set_config(Key, Value) :-
	strip_module(Key, DB, Path),
	set_config_(DB, Path, Value),
	set_modified(DB),
	broadcast(set_config(Key, Value)).

set_config_(DB, Path, Value) :-		% local version
	lretractall(config_store(DB, Path, _)),
	lasserta(config_store(DB, Path, Value)).

add_config(Key, Value) :-
	strip_module(Key, DB, Path),
	(   lretract(config_store(DB, Path, Set0)),
	    is_list(Set0)
	->  (   delete(Set0, Value, Set1)
	    ->	Set = [Value|Set1]
	    ;	Set = [Value|Set0]
	    )
	;   retractall(config_store(DB, Path, _)), % make sure
	    Set = [Value]
	),
	lasserta(config_store(DB, Path, Set)),
	set_modified(DB).

del_config(Key, Value) :-
	strip_module(Key, DB, Path),
	config_store(DB, Path, Set0),
	delete(Set0, Value, Set),
	lretract(config_store(DB, Path, Set0)), !,
	lasserta(config_store(DB, Path, Set)),
	set_modified(DB).

set_modified(DB) :-
	config_store(DB, '$modified', true), !.
set_modified(DB) :-
	asserta(config_store(DB, '$modified', true)).

clear_modified(DB) :-
	retractall(config_store(DB, '$modified', _)).


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
	config_db(DB, Pred, App),
	(   Path == config
	->  Attributes = [application(App)]
	;   Goal =.. [Pred, Path, Attributes],
	    DB:Goal
	).

comment(Key, Comment) :-
	config_attributes(Key, Attributes),
	memberchk(comment(Comment), Attributes).


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
	->  get_config(M:'$admin'/file, Key)
	;   true
	),
	save_file(Key, File),
	open(File, write, Fd),
	config_db(M, Pred),
	save_config_header(Fd, M),
	save_config_body(Fd, M, Pred),
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
	
save_config_body(Fd, M, Pred) :-
	Head =.. [Pred, Path, Attributes],
	forall(M:Head, save_config_key(Fd, M:Path, Attributes)).

save_config_key(Fd, _, Attributes) :-
	memberchk(comment(Comment), Attributes),
	format(Fd, '~n/* ~w */~n', [Comment]),
	fail.
save_config_key(Fd, Key, Attributes) :-
	get_config(Key, Value), !,
	strip_module(Key, _, Path),
	(   memberchk(default(Value), Attributes)
	->  format(Fd, '%~q = ~t~32|~q.~n', [Path, Value])
	;   format(Fd, '~q = ~t~32|~q.~n',  [Path, Value])
	).
save_config_key(_,_,_).

save_modified_configs :-
	config_db(DB, _Pred, _App),
	get_config(DB:'$modified', true),
	clear_modified(DB),
	get_config(DB:'$admin'/file, Key),
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
	get_config(M:'$admin'/file, _File), !.
ensure_loaded_config(Spec) :-
	load_config(Spec).

load_config(Spec) :-
	strip_module(Spec, M, Key),
	absolute_file_name(config(Key),
			   [ access(read),
			     extensions([cnf])
			   ], File), !,
	open(File, read, Fd),
	read_config_file(Fd, _SaveVersion, _SaveModule, Bindings),
	close(Fd),
	config_db(M, Pred),
	Head =.. [Pred, Path, Attributes],
	forall(M:Head,
	       pce_config:load_config_key(M:Path, Attributes, Bindings)),
	set_config_(M, '$admin'/file, File),
	clear_modified(M).
load_config(Spec) :-			% no config file, use defaults
	strip_module(Spec, M, Key),
	config_db(M, Pred),
	Head =.. [Pred, Path, Attributes],
	forall(M:Head,
	       pce_config:load_config_key(M:Path, Attributes, [])),
	set_config_(M, '$admin'/file, Key),
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

load_config_key(Key, Attributes, Bindings) :-
	Key = _:Path,
	(   memberchk(Path=Value, Bindings)
	->  set_config(Key, Value)
	;   memberchk(default(Value), Attributes)
	->  set_config(Key, Value)
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
