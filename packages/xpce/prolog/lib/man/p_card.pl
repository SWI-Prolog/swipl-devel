/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/


:- module(man_card, []).

:- use_module(library(pce)).
:- use_module(util).
:- require([ concat/3
	   , concat_atom/2
	   , file_directory_name/2
	   , forall/2
	   , member/2
	   ]).


		/********************************
		*            SPACES		*
		********************************/

:- pce_begin_class(man_space(name), object,
		   "Collection of man_modules").

:- pce_global(@man_space_table, new(hash_table)).


variable(name,		name,		both,
	 "Logical name of the space").
variable(directory,	directory,	both,
	 "Directory the saved modules reside in").
variable(modules,	hash_table,	get,
	 "Map module name onto module").
variable(modified,	bool,		both,
	 "Indicate some module has modified").

initialise(S, Name:name, Dir:directory) :->
        "Initialise from name and directory"::
	(   get(@man_space_table, member, Name, _)
	->  send(@display, inform, 'Space %s already exists', Name)
	;   send(S, slot, name,      Name),
	    send(S, slot, modified,  @off),
	    send(S, slot, directory, Dir),
	    send(S, slot, modules,   new(hash_table)),
	    send(@man_space_table, append, Name, S)
	).


lookup(_, Name:name, _Dir:[directory], S:man_space) :<-
	"Lookup existing manual space"::
	get(@man_space_table, member, Name, S).


module(S, ModuleName:name, Load:[bool], Module) :<-
	"Find named module (if loaded)"::
	(   get(S?modules, member, ModuleName, Module)
	->  true
	;   Load == @on,
	    send(S, load, ModuleName),
	    get(S?modules, member, ModuleName, Module)
	).


module_file(S, Module:name, File:file) :<-
	"Find file for storing module"::
	concat(Module, '.doc', FileName),
	(   concat('class/', ClassName, Module),
	    get(@pce, convert, ClassName, class, Class),
	    get(Class, creator, host),
	    get(Class, source, source_location(Path, _)),
	    file_directory_name(Path, SrcDir),
	    concat_atom([SrcDir, '/doc'], DocDirName),
	    new(DocDir, directory(DocDirName)),
	    send(DocDir, exists)
	->  true
	;   get(S, directory, DocDir)
	),
	get(DocDir, file, FileName, File).


save_some(S) :->
        "Save all modified buffers"::
	send(S?modules, for_all, message(@arg2, save_if_modified)),
	send(S, modified, @off).


save_all(S) :->
        "Save all modified buffers (modified or not)"::
	send(S?modules, for_all, message(@arg2, save)),
	send(S, modified, @off).


load(S, Module:name) :->
	"Load named module from file"::
	get(S, module_file, Module, File),
	(   send(File, exists)
	->  send(S, report, progress, 'Loading %s ...', File?base_name),
	    get(File, object, Mod),
	    send(S, report, done),
	    send(Mod, modified, @off),
	    send(S?modules, append, Module, Mod)
	).


ensure_loaded(S, Module:name) :->
	"Load named module if not yet done"::
	get(S, module, Module, @on, _).


load_all_modules(S) :->
	"Load all modules from the directory"::
	get(S?directory, files, '.*\.doc$', F1),
	get(S?directory, directory, class, ClassDir),
	get(ClassDir, files, '.*\.doc$', F2),
	send(F1, for_all, message(S, load_file, @arg1)),
	send(F2, for_all, message(S, load_file,
				  create(string, 'class/%s', @arg1))).


update_save_version(S) :->
	"Load and save all modules"::
	send(S, load_all_modules),
	send(S, save_all).


load_file(S, Name:name) :->
	"Load from a file name"::
	get(Name, delete_suffix, '.doc', Module),
	send(S, ensure_loaded, Module).


for_all_cards(S, Msg:code) :->
	"Run code on all loaded cards"::
	send(S?modules, for_all, message(@arg2, for_all_cards, Msg)).


delete_unreferenced(S) :->
	send(@classes, for_all, message(@arg2, realise)),
	send(S, load_all_modules),
	send(S, for_all_cards, message(@arg1, delete_unreferenced)).


fix_names(S) :->
	"Fix changed module-names"::
	send(S?modules, for_all,
	     if(@arg2?name \== @arg1,
		message(@arg2, rename, @arg1))).

:- pce_end_class.


		/********************************
		*            MODULES		*
		********************************/

:- pce_begin_class(man_module(name), object,
	 	   "Group of manual cards (man_card)").

variable(name,		name,	        get,	"Name of the module").
variable(space,		name,		none,	"Name of the related space").
variable(id_table,	hash_table,	get,	"Mapping CardId --> Card").
variable(modified,	bool,		get,	"Indicate has changed").
variable(current_id,	number,		both,	"Numeric id for next card").

initialise(M, Space:man_space, Name:name) :->
	"Create from space and name"::	      
	(   get(Space?modules, member, Name, _)
	->  send(@display, inform, 'Module %s already exists', Name)
	;   send(M, slot, name,     Name),
	    send(M, slot, id_table, new(hash_table)),
	    send(M, slot, space,    Space?name),
	    send(M, slot, modified, @off),
	    send(M, slot, current_id, number(1)),
	    send(Space?modules, append, Name, M)
	).


space(M, Space) :<-
	"Space this module belongs to"::
	get(@man_space_table, member, ?(M, slot, space), Space).


modified(M, Val:bool) :->
	"Set modified value"::
	send(M, slot, modified, Val),
	(   Val == @on
	->  send(M?space, modified, @on)
	;   true
	).


card(M, Id:'int|name', Card) :<-
	"Card from id"::
	get(M?id_table, member, Id, Card).


save_if_modified(M) :->
	"Save if modified is @on"::
	(   get(M, modified, @on),
	    \+ send(M?id_table, empty)
	->  send(M, save)
	;   true
	).


save(M) :->
	"Save in related file"::
	get(M, name, Name),
	get(M?space, module_file, Name, F),
	send(F, backup),
	send(M, report, progress, 'Saving %s ... ', F?base_name),
	send(M, save_in_file, F),
	send(M, report, done),
	send(M, modified, @off).


for_all_cards(M, Msg:code) :->
	"Run code on all cards of module"::
	send(M?id_table, for_all,
	     message(Msg, forward, @arg2)).


rename(M, Name:name) :->
	"Change name and relation-names"::
	get(M, name, OldName),
	send(M, report, progress, 'Renaming module %s --> %s', OldName, Name),
	send(M?space, for_all_cards,
	     message(@arg1, renamed_module, OldName, Name)),
	send(M, slot, name, Name),
	send(M, report, done).

:- pce_end_class.


		/********************************
		*             CARDS		*
		********************************/

:- pce_begin_class(man_card(module, name), object,
		   "Card of the online manual").

variable(identifier,	'int|name',	get,	"Unique identifier").
variable(module,	man_module,	get,	"Module I belong to").
variable(last_modified, date,		get,	"Last time a slot was").
variable(name,		name,		get,	"My name").
variable(summary,	string*,	get,	"Half-line summary").
variable(description,	string*,	get,	"Full description").
variable(see_also,	chain*,		none,	"`See Also' references").
variable(inherit,	chain*,		none,   "Inherit descriptions").


initialise(C, Mod:man_module, Name:[name], Id:[name]) :->
	"Initialise from module, name and identifier"::
	(   Id == @default
	->  get(Mod?current_id, value, Ident),
	    send(Mod?current_id, plus, 1)
	;   Ident = Id
	),
	send(C, slot, identifier,    Ident),
	send(C, slot, name,          Name),
	send(C, slot, module,        Mod),
	send(C, slot, last_modified, new(date)),
	send(Mod?id_table, append, C?identifier, C),
	send(Mod, modified, @on).


unlink(C) :->
	"Delete id from associated module"::
	send(C?module, modified, @on),
	send(C?module?id_table, delete, C?identifier).


space(C, Space) :<-
	"Space card resides in"::
	get(C?module, space, Space).


identifier(C, Id:name) :->
	"Set named identifier"::
	get(C, identifier, Old),
	get(C?module, id_table, Table),
	send(Table, append, Id, C),
	send(C, slot, identifier, Id),
	send(Table, delete, Old).


	/* SLOTS */

store(C, Slot:name, Value:any) :->
	"Store a slot value (normally a string)"::
	get(C, slot, Slot, OldValue),
	(   send(OldValue, equal, Value)
	->  true
	;   send(C?last_modified, current),
	    send(C?module, modified, @on),
	    send(C, slot, Slot, Value)
	).

fetch(C, Slot:name, Value:any) :<-
	"Read a slot value (possibly inherit)"::
	get(C, slot, Slot, Value),
	Value \== @nil.


inherited_fetch(C, Slot:name, Tuple:tuple) :<-
	"Read a slot value (possibly inherit)"::
	(   get(C, slot, Slot, Value),
	    Value \== @nil,
	    new(Tuple, tuple(C?object, Value))
	->  true
	;   get(C, related, inherit, Chain),
	    get(Chain, find, ?(@arg1, fetch, Slot), From),
	    get(From, fetch, Slot, Value),
	    Value \== @nil,
	    new(Tuple, tuple(From?object, Value))
	).


	/* RELATIONS */

rel_id(C, To:man_card, Id:'int|name') :<-
	"Relation id (internal/external)"::
	get(To, module, ToModule),
	(   get(C, module, ToModule)
	->  get(To, identifier, Id)
	;   get(ToModule, name, ToName),
	    get(To, identifier, ToId),
	    concat_atom([$, ToName, $, ToId], Id)
	).


expand_id(C, Id:'int|name', Card) :<-
	"Expand a relation id to a card"::
	(   atom(Id),
	    get(Id, scan, '$%[^$]$%s', vector(ModuleName, LocalId))
	->  get(C, space, Space),
	    get(Space, module, ModuleName, @on, Module),
	    get(Module, card, LocalId, Card)
	;   get(C?module, card, Id, Card)
	).


relate(C, Type:name, To:man_card) :->
	"Create typed relation to card"::
	get(C, slot, Type, Chain),
	(   Chain == @nil
	->  send(C, slot, Type, chain(?(C, rel_id, To)))
	;   send(Chain, add, ?(C, rel_id, To))
	),
	send(C?module, modified, @on).


move_relation_after(C, Type:name, To:man_card, Before:[man_card]) :->
	"Move relation to be before last argument or first"::
	get(C, slot, Type, Val), Val \== @nil,
	(   Before == @default
	->  send(Val, move_after, ?(C, rel_id, To))
	;   send(Val, move_after, ?(C, rel_id, To), ?(C, rel_id, Before))
	),
	send(C?module, modified, @on).


unrelate(C, Type:name, To:man_card) :->
	"Destroy typed relation to card"::
	get(C, slot, Type, Val),
	(   Val == @nil
	->  true
	;   send(Val, delete, ?(C, rel_id, To))
	).


related(C, Type:name, To:man_card) :->
	"Test if I'm related to card"::
	get(C, slot, Type, Val), Val \== @nil,
	send(Val, member, ?(C, rel_id, To)).


related(C, Type:name, Result) :<-
	"New chain with related cards"::
	get(C, slot, Type, Val), Val \== @nil,
	get(Val, map, new(?(C, expand_id, @arg1)), Result).


renamed_module(C, Old:name, New:name) :->
	"Scan (see-also, inherit) relations for module and rename"::
	forall(member(RelName, [see_also, inherit]),
	       renamed_module_relations(C, RelName, Old, New)).

renamed_module_relations(C, RelName, Old, New) :-
	get(C, slot, RelName, Chain),
	Chain \== @nil, !,
	send(Chain, for_all,
	     message(@prolog, renamed_module_relation,
		     Chain, @arg1, Old, New)).
renamed_module_relations(_, _, _, _).

renamed_module_relation(Ch, Id, Old, New) :-
	atom(Id),
	get(Id, scan, '$%[^$]$%s', vector(OldString, LocalIdString)),
	send(Old, equal, OldString), !,
	get(LocalIdString, value, LocalId),
	concat_atom(['$', New, '$', LocalId], NewId),
	send(Ch, replace, Id, NewId).
renamed_module_relation(_, _, _, _).


man_card(C, _Create:[bool], C) :<-
	"The card for a card is the card itself"::
	true.

object(C, C) :<-
	"For a general card, the object itself"::
	true.

has_source(_C) :->
	"Cards don't have source ..."::
	fail.

man_summary(C, S) :<-
	"General summary string"::
	new(S, string('%s\t%s\t%s', C?man_id, C?name, C?summary)),
	(   send(C, man_documented)
	->  send(S, append, ' (+)')
	;   true
	).

man_name(C, S) :<-
	"General name string"::
	new(S, string('%s \t%s', C?man_id, C?name)).

delete_unreferenced(C) :->
	"Delete if not referenced"::
	(   pce_catch_error(bad_return_value, get(C, object, _))
	->  true
	;   get(C, identifier, Id),
	    (	send(Id, sub, '.win_')	% windows-specific card
	    ->	true
	    ;   format(user_error, 'Deleting card ~w~n', [Id]),
		free(C)
	    )
	).

:- pce_end_class.

