/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2007, University of Amsterdam

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


:- module(rdf_edit,
	  [ rdfe_assert/3,		% Sub, Pred, Obj
	    rdfe_assert/4,		% Sub, Pred, Obj, PayLoad
	    rdfe_retractall/3,		% Sub, Pred, Obj
	    rdfe_retractall/4,		% Sub, Pred, Obj, PayLoad
	    rdfe_update/4,		% Sub, Pred, Obj, +Action
	    rdfe_update/5,		% Sub, Pred, Obj, +PayLoad, +Action
	    rdfe_load/1,		% +File
	    rdfe_load/2,		% +File, +Options
	    rdfe_delete/1,		% +Resource

	    rdfe_register_ns/2,		% +Id, +URI
	    rdfe_unregister_ns/2,	% +Id, +URI

	    rdfe_reset/0,		% clear everything

	    rdfe_transaction/1,		% :Goal
	    rdfe_transaction/2,		% :Goal, +Name
	    rdfe_transaction_member/2,	% +Transactions, -Action
	    rdfe_transaction_name/2,	% +Transactions, -Name
	    rdfe_set_transaction_name/1,% +Name

	    rdfe_set_watermark/1,	% +Name

	    rdfe_undo/0,		% 
	    rdfe_redo/0,
	    rdfe_can_undo/1,		% -TID
	    rdfe_can_redo/1,		% -TID

	    rdfe_set_file_property/2,	% +File, +Property
	    rdfe_get_file_property/2,	% ?File, ?Property

	    rdfe_is_modified/1,		% ?File
	    rdfe_clear_modified/1,	% +File

	    rdfe_open_journal/2,	% +File, +Mode
	    rdfe_close_journal/0,
	    rdfe_replay_journal/1,	% +File
	    rdfe_current_journal/1,	% -Path

	    rdfe_snapshot_file/1	% -File
	  ]).
:- use_module(rdf_db).
:- use_module(library(broadcast)).
:- use_module(library(lists)).
:- use_module(library(debug)).
:- use_module(library(url)).

:- meta_predicate
	rdfe_transaction(:),
	rdfe_transaction(:, +).

:- dynamic
	undo_log/5,			% TID, Action, Subj, Pred, Obj
	current_transaction/1,		% TID
	transaction_name/2,		% TID, Name
	undo_marker/2,			% Mode, TID
	journal/3,			% Path, Mode, Stream
	unmodified_md5/2,		% Path, MD5
	snapshot_file/1.		% File

/** <module> RDF edit layer
This library provides a number of functions on top of the rdf_db module:

    * Broadcast modifications
    * Provide undo/redo

@tbd	This library must be rewritten using rdf_monitor/3.  This allows
	using edit layer without having to choose between rdf_ and rdfe_
	predicates.

@see	rdf_persistency.pl provides reliable persistency, but without
	changes boardcasting and undo/redo.
*/

:- rdf_meta
	rdfe_assert(r,r,o),
	rdfe_assert(r,r,o,+),
	rdfe_retractall(r,r,o),
	rdfe_update(r,r,o,t),
	rdfe_delete(r),
	rdfe_transaction(:),
	rdfe_transaction(:, +).
	

		 /*******************************
		 *     BASIC EDIT OPERATIONS	*
		 *******************************/

rdfe_assert(Subject, Predicate, Object) :-
	rdfe_assert(Subject, Predicate, Object, user).

rdfe_assert(Subject, Predicate, Object, PayLoad) :-
	rdf_assert(Subject, Predicate, Object, PayLoad),
	rdfe_current_transaction(TID),
	assert_action(TID, assert(PayLoad), Subject, Predicate, Object),
	journal(assert(TID, Subject, Predicate, Object, PayLoad)).

rdfe_retractall(Subject, Predicate, Object) :-
	rdfe_retractall(Subject, Predicate, Object, _).

rdfe_retractall(Subject, Predicate, Object, PayLoad) :-
	rdfe_current_transaction(TID),
	(   rdf(Subject, Predicate, Object, PayLoad),
	    assert_action(TID, retract(PayLoad), Subject, Predicate, Object),
	    journal(retract(TID, Subject, Predicate, Object, PayLoad)),
	    fail
	;   true
	),
	rdf_retractall(Subject, Predicate, Object, PayLoad).
	
%%	rdfe_update(+Subject, +Predicate, +Object, +Action)
%	
%	Update an existing triple.  Possible actions are:
%	
%%		subject(+Subject)
%%		predicate(+Predicate)
%%		object(+Object)
%%		source(+Source)

rdfe_update(Subject, Predicate, Object, Action) :-
	rdfe_current_transaction(TID),
	rdf_update(Subject, Predicate, Object, Action),
	(   Action = object(New)
	->  assert_action(TID, object(Object), Subject, Predicate, New)
	;   Action = predicate(New)
	->  assert_action(TID, predicate(Predicate), Subject, New, Object)
	;   Action = subject(New)
	->  assert_action(TID, subject(Subject), New, Predicate, Object)
	;   Action = source(New)
	->  forall(rdf(Subject, Predicate, Object, PayLoad),
		   assert_action(TID, source(PayLoad, New),
				 Subject, Predicate, Object))
	),
	journal(update(TID, Subject, Predicate, Object, Action)).

rdfe_update(Subject, Predicate, Object, PayLoad, Action) :-
	rdfe_current_transaction(TID),
	rdf_update(Subject, Predicate, Object, PayLoad, Action),
	(   Action = source(New)
	->  assert_action(TID, source(PayLoad, New),
			  Subject, Predicate, Object)
	;   throw(tbd)			% source is used internally
	),
	journal(update(TID, Subject, Predicate, Object, PayLoad, Action)).

%%	rdfe_delete(+Subject)
%	
%	Delete a subject and all we know about it. This is a bit tricky.
%	If we are involved in transitive relations, should we re-joint
%	these in this module?

rdfe_delete(Subject) :-
	rdfe_transaction(delete(Subject)).

delete(Subject) :-
	rdfe_retractall(Subject, _, _),
	rdfe_retractall(_, Subject, _),
	rdfe_retractall(_, _, Subject).


		 /*******************************
		 *	   FILE HANDLING	*
		 *******************************/

%%	rdfe_load(+File) is det.
%%	rdfe_load(+File, +Options) is det.
%	
%	Load an RDF file and record this action including version information
%	to facilitate reliable reload.

rdfe_load(File) :-
	rdfe_load(File, []).


rdfe_load(File, Options) :-
	rdfe_current_transaction(TID),
	absolute_file_name(File,
			   [ access(read),
			     extensions([rdf,rdfs,owl,''])
			   ], Path),
	atom_concat('file://', Path, BaseURI),
	rdf_load(Path,
		 [ base_uri(BaseURI),
		   result(Action, Triples, MD5)
		 | Options
		 ]),
	(   Action == none		% load, reload, none
	->  true
	;   absolute_file_name('.', PWD),
	    size_file(Path, Size),
	    time_file(Path, Modified),
	    SecTime is round(Modified),
	    assert_action(TID, load_file(Path), -, -, -),
	    journal(rdf_load(TID,
			     Path,
			     [ pwd(PWD),
			       size(Size),
			       modified(SecTime),
			       triples(Triples),
			       md5(MD5),
			       from(File)
			     ])),
	    ensure_snapshot(Path)
	).


rdfe_unload(Path) :-
	rdfe_current_transaction(TID),
	rdf_unload(Path),
	assert_action(TID, unload_file(Path), -, -, -),
	journal(rdf_unload(TID, Path)).


%%	ensure_snapshot(+Path)
%	
%	Ensure we have a snapshot of Path if we are making a journal, so
%	we can always reload the snapshot to ensure exactly the same
%	state.

ensure_snapshot(Path) :-
	rdfe_current_journal(_),
	rdf_md5(Path, MD5),
	(   snapshot_file(Path, MD5,
			  [ access(read),
			    file_errors(fail)
			  ],
			  File)
	->  debug(snapshot, 'Existing snapshot for ~w on ~w', [Path, File])
	;   snapshot_file(Path, MD5,
			  [ access(write)
			  ],
			  File),
	    debug(snapshot, 'Saving snapshot for ~w to ~w', [Path, File]),
	    rdf_save_db(File, Path)
	),
	assert(snapshot_file(File)).
ensure_snapshot(_).


%%	load_snapshot(+Source, +Path)
%	
%	Load triples from the given snapshot   file. One of the troubles
%	is the time-stamp to avoid rdf_make/0   from reloading the file.
%	for the time being we use 1e12, which   is  a lot further in the
%	future than this system is going to live.

load_snapshot(Source, Path) :-
	statistics(cputime, T0),
	rdf_load_db(Path),
	statistics(cputime, T1),
	Time is T1 - T0,
	rdf_statistics(triples_by_file(Source, Triples)),
	rdf_md5(Source, MD5),
					% 1e10: modified far in the future
	assert(rdf_db:rdf_source(Source, 1e12, Triples, MD5)),
	print_message(informational,
		      rdf(loaded(Source, Triples, snapshot(Time)))),
	assert(snapshot_file(Path)).


%%	snapshot_file(+Path, +MD5, +Access, -File)
%	
%	Find existing snapsnot file or location to save a new one.

snapshot_file(Path, MD5, Options, SnapShot) :-
	file_base_name(Path, Base),
	concat_atom([Base, @, MD5], File),
	absolute_file_name(snapshot(File),
			   [ extensions([trp])
			   | Options
			   ],
			   SnapShot).


%%	rdfe_snapshot_file(-File)
%	
%	Enumerate the MD5 snapshot files required to restore the current
%	journal file. Using this  call  we   can  write  a  routine that
%	packages the journal file with all required snapshots to restore
%	the journal on another computer.

rdfe_snapshot_file(File) :-
	snapshot_file(File).


		 /*******************************
		 *	NAMESPACE HANDLING	*
		 *******************************/

:- dynamic
	system_ns/2.
:- volatile
	system_ns/2.

%%	rdfe_register_ns(Id, URI)
%	
%	Encapsulation of rdf_register_ns(Id, URI)

rdfe_register_ns(Id, URI) :-
	rdf_db:ns(Id, URI), !.
rdfe_register_ns(Id, URI) :-
	save_system_ns,
	rdfe_current_transaction(TID),
	rdf_register_ns(Id, URI),
	broadcast(rdf_ns(register(Id, URI))),
	assert_action(TID, ns(register(Id, URI)), -, -, -),
	journal(ns(TID, register(Id, URI))).

rdfe_unregister_ns(Id, URI) :-
	save_system_ns,
	rdfe_current_transaction(TID),
	retractall(rdf_db:ns(Id, URI)),
	broadcast(rdf_ns(unregister(Id, URI))),
	assert_action(TID, ns(unregister(Id, URI)), -, -, -),
	journal(ns(TID, unregister(Id, URI))).

%	rdfe_register_ns/0
%	
%	Reset namespaces to the state they where before usage of the
%	rdf_edit layer.

rdfe_reset_ns :-
	(   system_ns(_, _)
	->  retractall(rdf_db:ns(Id, URI)),
	    forall(system_ns(Id, URI), assert(rdb_db:ns(Id, URI)))
	;   true
	).

save_system_ns :-
	system_ns(_, _), !.		% already done
save_system_ns :-
	forall(rdf_db:ns(Id, URI), assert(system_ns(Id, URI))).


		 /*******************************
		 *	   TRANSACTIONS		*
		 *******************************/

%%	rdfe_transaction(:Goal)
%	
%	Run Goal, recording all modifications   as a single transaction.
%	If  Goal  raises  an  exception  or    fails,  all  changes  are
%	rolled-back.

rdfe_transaction(Goal) :-
	rdfe_transaction(Goal, []).
rdfe_transaction(Goal, Name) :-
	rdfe_begin_transaction(Name),
	(   catch(Goal, E, true)
	->  (   var(E)
	    ->	check_file_protection(Error),
		(   var(Error)
		->  rdfe_commit
		;   rdfe_rollback,
		    throw(Error)
		)
	    ;	rdfe_rollback,
		throw(E)
	    )
	;   rdfe_rollback,
	    fail
	).
	    
%%	rdfe_begin_transaction
%	
%	Start a transaction.  This is followed by either rdfe_end_transaction
%	or rdfe_rollback.  Transactions may be nested.

rdfe_begin_transaction(Name) :-
	current_transaction(TID), !,	% nested transaction
	append(TID, [1], TID2),
	asserta(current_transaction(TID2)),
	assert(transaction_name(TID2, Name)).
rdfe_begin_transaction(Name) :-		% toplevel transaction
	flag(rdf_edit_tid, TID, TID+1),
	asserta(current_transaction([TID])),
	assert(transaction_name(TID, Name)).

rdfe_current_transaction(TID) :-
	current_transaction(TID), !.
rdfe_current_transaction(_) :-
	throw(error(existence_error(rdf_transaction, _), _)).

rdfe_commit :-
	retract(current_transaction(TID)), !,
	retractall(undo_marker(_, _)),
	(   rdfe_transaction_member(TID, _)
	->  get_time(Time),		% transaction is not empty
	    journal(commit(TID, Time)),
	    (   TID = [Id]
	    ->  broadcast(rdf_transaction(Id))
	    ;   true
	    )
	;   true
	).

rdfe_rollback :-
	retract(current_transaction(TID)), !,
	journal(rollback(TID)),
	rollback(TID).

%%	rollback(+TID)
%	
%	This is the same as undo/1, but it must not record the undone
%	actions as rollbacks cannot be `redone'.  Somehow there should
%	be a cleaner way to distinguish between transactional operations
%	and plain operations.

rollback(TID) :-
	append(TID, _, Id),
	(   retract(undo_log(Id, Action, Subject, Predicate, Object)),
	    (	rollback(Action, Subject, Predicate, Object)
	    ->	fail
	    ;	print_message(error,
			      rdf_undo_failed(undo(Action, Subject,
						   Predicate, Object))),
		fail
	    )
	;   true
	).
	
rollback(assert(PayLoad), Subject, Predicate, Object) :- !,
	rdf_retractall(Subject, Predicate, Object, PayLoad).
rollback(retract(PayLoad), Subject, Predicate, Object) :- !,
	rdf_assert(Subject, Predicate, Object, PayLoad).
rollback(Action, Subject, Predicate, Object) :-
	action(Action), !,
	rdf_update(Subject, Predicate, Object, Action).


assert_action(TID, Action, Subject, Predicate, Object) :-
	asserta(undo_log(TID, Action, Subject, Predicate, Object)).

%%	undo(+TID)
%	
%	Undo a transaction as well as possible transactions nested into
%	it.

undo(TID) :-
	append(TID, _, Id),
	(   retract(undo_log(Id, Action, Subject, Predicate, Object)),
	    (	undo(Action, Subject, Predicate, Object)
	    ->	fail
	    ;	print_message(warning,
			      rdf_undo_failed(undo(Action, Subject,
						   Predicate, Object))),
		fail
	    )
	;   true
	).
	
undo(assert(PayLoad), Subject, Predicate, Object) :- !,
	rdfe_retractall(Subject, Predicate, Object, PayLoad).
undo(retract(PayLoad), Subject, Predicate, Object) :- !,
	rdfe_assert(Subject, Predicate, Object, PayLoad).
undo(source(Old, New), Subject, Predicate, Object) :- !,
	rdfe_update(Subject, Predicate, Object, Old, source(New)).
undo(ns(Action), -, -, -) :- !,
	(   Action = register(Id, URI)
	->  rdfe_unregister_ns(Id, URI)
	;   Action = unregister(Id, URI)
	->  rdfe_register_ns(Id, URI)
	).
undo(load_file(Path), -, -, -) :- !,
	rdfe_unload(Path).
undo(unload_file(Path), -, -, -) :- !,
	rdfe_load(Path).
undo(Action, Subject, Predicate, Object) :-
	action(Action), !,
	rdfe_update(Subject, Predicate, Object, Action).

action(subject(_)).
action(predicate(_)).
action(object(_)).

%%	rdfe_undo
%	
%	Undo a (toplevel) transaction. More calls do further undo. The
%	`Undone' actions are re-added to the undo log, so the user can
%	redo them.  Fails if there are no more undo/redo transactions.

rdfe_undo :-
	undo_marker(undo, TID), !,
	(   undo_previous(TID, UnDone)
	->  retractall(undo_marker(_, _)),
	    assert(undo_marker(undo, UnDone)),
	    broadcast(rdf_undo(undo, UnDone))
	;   fail			% start of undo log
	).
rdfe_undo :-
	retract(undo_marker(redo, _)), !,
	last_transaction(TID),
	undo_previous(TID, UnDone),
	assert(undo_marker(undo, UnDone)),
	broadcast(rdf_undo(undo, UnDone)).
rdfe_undo :-
	last_transaction(TID),
	undo_previous(TID, UnDone),
	assert(undo_marker(undo, UnDone)),
	broadcast(rdf_undo(undo, UnDone)).

find_previous_undo(-1, _) :- !,
	fail.
find_previous_undo(TID, TID) :-
	undo_log([TID|_], _, _, _, _), !.
find_previous_undo(TID0, TID) :-
	TID1 is TID0 - 1,
	find_previous_undo(TID1, TID).

undo_previous(TID, Undone) :-
	find_previous_undo(TID, Undone),
	rdfe_transaction(undo([Undone])).

last_transaction(TID) :-
	undo_log([TID|_], _, _, _, _), !.

%%	rdfe_redo
%	
%	Start a redo-session

rdfe_redo :-
	(   retract(undo_marker(undo, _))
	->  last_transaction(TID),
	    undo_previous(TID, UnDone),
	    assert(undo_marker(redo, UnDone))
	;   retract(undo_marker(redo, TID))
	->  undo_previous(TID, UnDone),
	    assert(undo_marker(redo, UnDone))
	;   true
	),
	broadcast(rdf_undo(redo, UnDone)).


%%	rdfe_can_redo(-TID) is semidet.
%%	rdfe_can_undo(-TID) is semidet.
%	
%	Check if we can undo and if so return the id of the transaction
%	that will be un/re-done.  A subsequent call to rdfe_transaction_name
%	can be used to give a hint in the UI.

rdfe_can_redo(Redo) :-
	undo_marker(undo, _), !,
	last_transaction(TID),
	find_previous_undo(TID, Redo).
rdfe_can_redo(Redo) :-
	undo_marker(redo, TID),
	find_previous_undo(TID, Redo).

rdfe_can_undo(Undo) :-			% continue undo
	undo_marker(undo, TID), !,
	find_previous_undo(TID, Undo).
rdfe_can_undo(Undo) :-			% start undo
	last_transaction(TID),
	find_previous_undo(TID, Undo).

%%	rdfe_transaction_name(+TID, -Name)
%	
%	Return name if the transaction is named.

rdfe_transaction_name(TID, Name) :-
	transaction_name(TID, Name),
	Name \== [].

%%	rdfe_set_transaction_name(+Name)
%	
%	Set name of the current transaction

rdfe_set_transaction_name(Name) :-
	current_transaction(TID), !,
	assert(transaction_name(TID, Name)).

%%	rdfe_transaction_member(+TID, -Action)
%	
%	Query actions inside a transaction to allow for quick update
%	of visualisers.

rdfe_transaction_member(TID, Member) :-
	(   integer(TID)
	->  Id = [TID|_]
	;   append(TID, _, Id)
	),
	undo_log(Id, Action, Subject, Predicate, Object),
	user_transaction_member(Action, Subject, Predicate, Object, Member).

user_transaction_member(assert(_), Subject, Predicate, Object,
			assert(Subject, Predicate, Object)) :- !.
user_transaction_member(retract(_), Subject, Predicate, Object,
			retract(Subject, Predicate, Object)) :- !.
user_transaction_member(load_file(Path), -, -, -,
			file(load(Path))) :- !.
user_transaction_member(unload_file(Path), -, -, -,
			file(unload(Path))) :- !.
user_transaction_member(Update, Subject, Predicate, Object,
			update(Subject, Predicate, Object, Update)).


		 /*******************************
		 *	     PROTECTION		*
		 *******************************/

:- dynamic
	rdf_source_permission/2,	% file, ro/rw
	rdf_current_default_file/2.	% file, all/fallback

%%	rdfe_set_file_property(+File, +Options)
%	
%	Set properties on the file.  Options is one of
%	
%		* access(ro/rw)
%		* default(all/fallback)

rdfe_set_file_property(File, access(Access)) :- !,
	to_url(File, URL),
	retractall(rdf_source_permission(URL, _)),
	assert(rdf_source_permission(URL, Access)),
	broadcast(rdf_file_property(URL, access(Access))).
rdfe_set_file_property(File, default(Type)) :-
	to_url(File, URL),
	rdfe_set_file_property(URL, access(rw)), % must be writeable
	retractall(rdf_current_default_file(_,_)),
	assert(rdf_current_default_file(URL, Type)),
	broadcast(rdf_file_property(URL, default(Type))).


%%	rdfe_get_file_property(+FileOrURL, ?Option).
%%	rdfe_get_file_property(-URL, ?Option).
%	
%	Fetch file properties set with rdfe_set_file_property/2.

rdfe_get_file_property(FileOrURL, access(Access)) :-
	(   ground(FileOrURL)
	->  to_url(FileOrURL, URL)
	;   rdf_source(_DB, URL),
	    FileOrURL = URL
	),
	(   rdf_source_permission(URL, Access0)
	->  Access0 = Access
	;   access_file(URL, write)
	->  assert(rdf_source_permission(URL, rw)),
	    Access = rw
	;   assert(rdf_source_permission(URL, ro)),
	    Access = ro
	).
rdfe_get_file_property(FileOrURL, default(Default)) :-
	ground(FileOrURL),
	to_url(FileOrURL, URL),
	(   rdf_current_default_file(URL, Default)
	->  true
	;   FileOrURL = user,
	    Default = fallback
	).
rdfe_get_file_property(URL, default(Default)) :-
	(   rdf_current_default_file(URL, Default)
	->  true
	;   URL = user,
	    Default = fallback
	).


%%	check_file_protection(-Error)
%	
%	Check modification of all protected files

check_file_protection(Error) :-
	(   rdfe_get_file_property(File, access(ro)),
	    rdfe_is_modified(File)
	->  Error = error(permission_error(modify, source, File), triple20)
	;   true
	).


%%	to_url(+Spec, -URL) is det.
%
%	Convert a specification into a URL.

to_url(URL, URL) :-
	atom(URL),
	sub_atom(URL, B, _, _, '://'),
	sub_atom(URL, 0, B, _, Protocol),
	url_protocol(Protocol), !.
to_url(File, URL) :-
	file_name_to_url(File, URL).


url_protocol(file).
url_protocol(http).
url_protocol(https).
url_protocol(ftp).
url_protocol(ftps).


		 /*******************************
		 *	     MODIFIED		*
		 *******************************/

%%	rdfe_is_modified(?Source)
%	
%	True if facts have been added, deleted or updated that have
%	Source as `payload'.

rdfe_is_modified(Source) :-
	rdf_source(DB, Source),
	rdf_md5(DB, MD5),
	(   unmodified_md5(Source, UnmodifiedMD5)
	->  true
	;   rdf_db:rdf_source(DB, Source, _Time, _Triples, UnmodifiedMD5)
	),
	UnmodifiedMD5 \== MD5.


rdfe_clear_modified :-
	forall(rdf_graph(File),
	       rdfe_clear_modified(File)).

%%	rdfe_clear_modified(+DB) is det.
%
%	Consider the current state of DB as _unmodified_.

rdfe_clear_modified(DB) :-
	atom(DB),
	retractall(unmodified_md5(DB, _)),
	rdf_md5(DB, MD5),
	(   rdf_db:rdf_source(DB, _File, _Time, _Triples, UnmodifiedMD5),
	    MD5 == UnmodifiedMD5
	->  true
	;   assert(unmodified_md5(DB, MD5))
	).


		 /*******************************
		 *	     WATERMARKS		*
		 *******************************/

%%	rdfe_set_watermark(Name)
%	
%	Create a watermark for undo and replay journal upto this point.
%	The rest of the logic needs to be written later.

rdfe_set_watermark(Name) :-
	rdfe_current_transaction(TID),
	assert_action(TID, watermark(Name), -, -, -),
	journal(watermark(TID, Name)).


		 /*******************************
		 *	       RESET		*
		 *******************************/

%%	rdfe_reset
%	
%	Clear database, undo, namespaces and journalling info.

rdfe_reset :-
	rdfe_reset_journal,
	rdfe_reset_ns,
	rdfe_reset_undo,
	rdf_reset_db,
	broadcast(rdf_reset).

%%	rdfe_reset_journal
%
%	If a journal is open, close it using rdfe_close_journal/0

rdfe_reset_journal :-
	(   rdfe_current_journal(_)
	->  rdfe_close_journal
	;   true
	).

rdfe_reset_undo :-
	retractall(undo_log(_,_,_,_,_)),
	retractall(current_transaction(_)),
	retractall(transaction_name(_,_)),
	retractall(undo_marker(_,_)),
	retractall(unmodified_md5(_, _)),
	retractall(snapshot_file(_)).

%	close possible open journal at exit.  Using a Prolog hook
%	guarantees closure, even for most crashes.

:- at_halt(rdfe_reset_journal).


		 /*******************************
		 *	    JOURNALLING		*
		 *******************************/

journal_version(1).

%%	rdfe_open_journal(+File, +Mode) is det.
%
%	Open a journal writing to File in Mode.  Mode is one of 
%	
%		* read
%		Open and replay the journal
%		
%		* write
%		Delete current journal and create a fresh one
%		
%		* append
%		Read and replay the existing journal and append new
%		modifications to the File.

rdfe_open_journal(_, _) :-		% already open
	journal(_, _, _), !.
rdfe_open_journal(File, read) :- !,
	absolute_file_name(File,
			   [ extensions([rdfj, '']),
			     access(read)
			   ],
			   Path),
	rdfe_replay_journal(Path),
	rdfe_clear_modified.
rdfe_open_journal(File, write) :- !,
	absolute_file_name(File,
			   [ extensions([rdfj, '']),
			     access(write)
			   ],
			   Path),
	open(Path, write, Stream, [close_on_abort(false)]),
	assert(journal(Path, write, Stream)),
	get_time(T),
	journal_open(start, T).
rdfe_open_journal(File, append) :-
	working_directory(CWD, CWD),
	absolute_file_name(File,
			   [ extensions([rdfj, '']),
			     relative_to(CWD),
			     access(write)
			   ],
			   Path),
	(   exists_file(Path)
	->  rdfe_replay_journal(Path),
	    rdfe_clear_modified,
	    get_time(T),
	    assert(journal(Path, append(T), []))
	;   rdfe_open_journal(Path, write)
	).


journal_open(Type, Time) :-
	journal_comment(Type, Time),
	SecTime is round(Time),
	journal_version(Version),
	Start =.. [ Type, [ time(SecTime),
			    version(Version)
			  ]
		  ],
	journal(Start),
	broadcast(rdf_journal(Start)).

journal_comment(start, Time) :-
	journal(_, _, Stream),
	convert_time(Time, String),
	format(Stream,
	       '/* Triple20 Journal File\n\n   \
	       Created: ~w\n   \
	       Triple20 by Jan Wielemaker <wielemak@science.uva.nl>\n\n   \
	       EDIT WITH CARE!\n\
	       */~n~n', [String]).
journal_comment(resume, Time) :-
	journal(_, _, Stream),
	convert_time(Time, String),
	format(Stream,
	       '\n\
	       /* Resumed: ~w\n\
	       */~n~n', [String]).

%%	rdfe_close_journal
%
%	Close  the  journal.  Automatically  called    from  at  program
%	termination from at_halt/1.

rdfe_close_journal :-
	get_time(T),
	SecTime is round(T),
	journal(end([ time(SecTime)
		    ])),
	retract(journal(_, Mode, Stream)),
	(   Mode = append(_)
	->  true
	;   close(Stream)
	).

%%	rdfe_current_journal(-Path)
%	
%	Query the currently open journal

rdfe_current_journal(Path) :-
	journal(Path, _Mode, _Stream).

journal(Term) :-
	journal(Path, append(T), _), !,
	(   Term = end(_)
	->  true
	;   open(Path, append, Stream, [close_on_abort(false)]),
	    retractall(journal(Path, _, _)),
	    assert(journal(Path, append, Stream)),
	    journal_open(resume, T),
	    journal(Term)
	).
journal(Term) :-
	(   journal(_, _, Stream)
	->  write_journal(Term, Stream),
	    flush_output(Stream)
	;   broadcast(rdf_no_journal(Term))
	).

write_journal(commit(TID, Time), Stream) :- !,
	format(Stream, 'commit(~q, ~2f).~n~n', [TID, Time]).
write_journal(Term, Stream) :-
	format(Stream, '~q.~n', [Term]).


%%	rdfe_replay_journal(+File)
%	
%	Replay a journal file. For now  this   is  our cheap way to deal
%	with save/load. Future versions may be  more clever when dealing
%	with the version information stored in the journal.

rdfe_replay_journal(File) :-
	absolute_file_name(File,
			   [ extensions([rdfj, '']),
			     access(read)
			   ],
			   Path),
	open(Path, read, Stream),
	replay(Stream),
	close(Stream).

replay(Stream) :-
	read(Stream, Term),
	replay(Term, Stream).

replay(end_of_file, _) :- !.
replay(start(_Attributes), Stream) :- !,
	read(Stream, Term),
	replay(Term, Stream).
replay(resume(_Attributes), Stream) :- !,
	read(Stream, Term),
	replay(Term, Stream).
replay(end(_Attributes), Stream) :- !,
	read(Stream, Term),
	replay(Term, Stream).
replay(Term0, Stream) :-
	replay_transaction(Term0, Stream),
	read(Stream, Term),
	replay(Term, Stream).

replay_transaction(Term0, Stream) :-
	collect_transaction(Term0, Stream, Transaction, Last),
	(   committed_transaction(Last)
	->  replay_actions(Transaction)
	;   true
	).

collect_transaction(End, _, [], End) :-
	ends_transaction(End), !.
collect_transaction(A, Stream, [A|T], End) :-
	read(Stream, Term),
	collect_transaction(Term, Stream, T, End).

committed_transaction(commit(_)).
committed_transaction(commit(_, _)).

ends_transaction(end_of_file).
ends_transaction(commit(_)).
ends_transaction(commit(_, _)).
ends_transaction(rollback(_)).
ends_transaction(end(_)).
ends_transaction(start(_)).

replay_actions([]).
replay_actions([H|T]) :-
	(   replay_action(H)
	->  replay_actions(T)
	;   print_message(warning,
			  rdf_replay_failed(H)),
	    (	debugging(journal)
	    ->	gtrace,
		replay_actions([H|T])
	    ;	replay_actions(T)
	    )
	).


%%	replay_action(+Action)
%	
%	Replay actions from the journal. Tricky is rdf_load/3. It should
%	reload the file in the state it  was   in  at  the moment it was
%	created. For now this has been hacked  for files that were empry
%	at the moment they where loaded (e.g. created from `new_file' in
%	our GUI prototype). How to solve this? We could warn if the file
%	appears changed, but this isn't really   easy  as copying and OS
%	differences makes it hard to decide on changes by length as well
%	as modification time. Alternatively we could   save the state in
%	seperate quick-load states.

replay_action(retract(_, Subject, Predicate, Object, PayLoad)) :-
	rdf_retractall(Subject, Predicate, Object, PayLoad).
replay_action(assert(_, Subject, Predicate, Object, PayLoad)) :-
	rdf_assert(Subject, Predicate, Object, PayLoad).
replay_action(update(_, Subject, Predicate, Object, Action)) :-
	rdf_update(Subject, Predicate, Object, Action).
replay_action(update(_, Subject, Predicate, Object, Payload, Action)) :-
	rdf_update(Subject, Predicate, Object, Payload, Action).
replay_action(rdf_load(_, File, Options)) :-
	memberchk(md5(MD5), Options),
	snapshot_file(File, MD5,
		      [ access(read),
			file_errors(fail)
		      ],
		      Path), !,
	debug(snapshot, 'Reloading snapshot ~w~n', [Path]),
	load_snapshot(File, Path).
replay_action(rdf_load(_, File, Options)) :-
	find_file(File, Options, Path),
	(   memberchk(triples(0), Options),
	    memberchk(modified(Modified), Options)
	->  rdf_retractall(_,_,_,Path:_),
	    retractall(rdf_db:rdf_source(Path, _, _, _)),	% TBD: move
	    rdf_md5(Path, MD5),
	    assert(rdf_db:rdf_source(Path, Modified, 0, MD5))
	;   rdf_load(Path)
	).
replay_action(rdf_unload(_, Source)) :-
	rdf_unload(Source).
replay_action(ns(_, register(ID, URI))) :- !,
	rdf_register_ns(ID, URI).
replay_action(ns(_, unregister(ID, URI))) :-
	retractall(rdf_db:ns(ID, URI)).
replay_action(watermark(_, _Name)) :-
	true.

find_file(File, _, File) :-
	exists_file(File), !.
find_file(File, Options, Path) :-
	memberchk(pwd(PWD), Options),
	make_path(File, PWD, Path),
	exists_file(Path), !.

%%	make_path(+File, +PWD, -Path)
%	
%	Return location of File relative to PWD, Parent of PWD, etc. (TBD)

make_path(File, PWD, Path) :-
	atom_concat(PWD, /, PWD2),
	atom_concat(PWD2, Path, File).


		 /*******************************
		 *	      MESSAGES		*
		 *******************************/

:- multifile
	prolog:message/3,
	user:message_hook/3.

%	Catch messages.

prolog:message(rdf_replay_failed(Term)) -->
	[ 'RDFDB: Replay of ~p failed'-[Term] ].
prolog:message(rdf_undo_failed(Term)) -->
	[ 'RDFDB: Undo of ~p failed'-[Term] ].
