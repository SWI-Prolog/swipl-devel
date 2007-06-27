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

:- module(rdf_persistency,
	  [ rdf_attach_db/2,		% +Directory, +Options
	    rdf_detach_db/0,		% +Detach current DB
	    rdf_current_db/1,		% -Directory
	    rdf_persistency/2,		% +DB, +Bool
	    rdf_flush_journals/1,	% +Options
	    rdf_journal_file/2,		% ?DB, ?JournalFile
	    rdf_db_to_file/2		% ?DB, ?FileBase
	  ]).
:- use_module(library('semweb/rdf_db')).
:- use_module(library(lists)).
:- use_module(library(url)).
:- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(library(thread)).

/** <module> RDF persistency plugin

This  module  provides  persistency   for    rdf_db.pl   based   on  the
rdf_monitor/2 predicate to  track  changes   to  the  repository.  Where
previous  versions  used  autosafe  of  the  whole  database  using  the
quick-load format of rdf_db, this version is  based on a quick-load file
per source (4th argument of rdf/4), and journalling for edit operations.

The result is safe, avoids frequent small   changes to large files which
makes synchronisation and backup expensive and avoids long disruption of
the server doing the autosafe. Only loading large files disrupts service
for some time.

The persistent backup of the database is  realised in a directory, using
a lock file to avoid corruption due to concurrent access. Each source is
represented by two files, the latest snapshot   and a journal. The state
is restored by loading  the  snapshot   and  replaying  the journal. The
predicate rdf_flush_journals/1 can be used to create fresh snapshots and
delete the journals.

@tbd if there is a complete `.new'   snapshot  and no journal, we should
move the .new to the plain snapshot name as a means of recovery.

@see	rdf_edit.pl
*/

:- volatile
	rdf_directory/1,
	rdf_option/1,
	source_journal_fd/2,
	db_file_base/2.
:- dynamic
	rdf_directory/1,		% Absolute path
	rdf_option/1,			% Defined options
	source_journal_fd/2,		% DB, JournalFD
	db_file_base/2.			% DB, FileBase

%%	rdf_attach_db(+Directory, +Options)
%	
%	Start persistent operations using Directory   as  place to store
%	files.   There are several cases:
%	
%		* Empty DB, existing directory
%		Load the DB from the existing directory
%		
%		* Full DB, empty directory
%		Create snapshots for all sources in directory
%		
%	Options:
%	
%		* concurrency(+Jobs)
%		Number of threads to use for loading the initial
%		database.  If not provided it is the number of CPUs
%		as optained from the flag =cpu_count=.
%
%		* max_open_journals(+Count)
%		Maximum number of journals kept open.  If not provided,
%		the default is 10.  See limit_fd_pool/0.

rdf_attach_db(DirSpec, Options) :-
	absolute_file_name(DirSpec,
			   Directory,
			   [ access(write),
			     file_type(directory),
			     file_errors(fail)
			   ]), !,
	(   rdf_directory(Directory)
	->  true			% update settings?
	;   rdf_detach_db,
	    mkdir(Directory),
	    assert(rdf_directory(Directory)),
	    assert_options(Options),
	    lock_db(Directory),
	    stop_monitor,		% make sure not to register load
	    load_db,
	    at_halt(rdf_detach_db),
	    start_monitor
	).
rdf_attach_db(DirSpec, Options) :-
	absolute_file_name(DirSpec,
			   Directory,
			   [ solutions(all)
			   ]),
	(   exists_directory(Directory)
	->  access_file(Directory, write)
	;   catch(make_directory(Directory), _, fail)
	), !,
	rdf_attach_db(Directory, Options).


assert_options([]).
assert_options([H|T]) :-
	(   option_type(H, Check)
	->  Check,
	    assert(rdf_option(H))
	;   domain_error(rdf_option, H)
	),
	assert_options(T).
	
option_type(concurrency(X),       must_be(X, positive_integer)).
option_type(max_open_journals(X), must_be(X, positive_integer)).


%%	rdf_detach_db
%
%	Detach from the current database. Normally  called at the end of
%	the program through at_halt/1.

rdf_detach_db :-
	debug(halt, 'Detaching database', []),
	stop_monitor,
	close_journals,
	(   retract(rdf_directory(Dir))
	->  debug(halt, 'DB Directory: ~w', [Dir]),
	    retractall(rdf_option(_)),
	    retractall(source_journal_fd(_,_)),
	    retractall(db_file_base(_,_)),
	    unlock_db(Dir)
	;   true
	).


%%	rdf_current_db(?Dir)
%	
%	True if Dir is the current RDF persistent database.

rdf_current_db(Directory) :-
	rdf_directory(Dir), !,
	Dir = Directory.


%%	rdf_flush_journals(+Options)
%	
%	Flush dirty journals.  Options:
%	
%		* min_size(+KB)
%		Only flush if journal is over KB in size.
%		TBD: sensible default size

rdf_flush_journals(Options) :-
	forall(rdf_source(DB),
	       rdf_flush_journal(DB, Options)).

rdf_flush_journal(DB, Options) :-
	db_files(DB, _SnapshotFile, JournalFile),
	db_file(JournalFile, File),
	(   \+ exists_file(File)
	->  true
	;   memberchk(min_size(KB), Options),
	    size_file(JournalFile, Size),
	    Size / 1024 < KB
	->  true
	;   create_db(DB)
	).

		 /*******************************
		 *	       LOAD		*
		 *******************************/

%%	load_db is det.
%
%	Reload database from the directory specified by rdf_directory/1.
%	First we find all names graphs using find_dbs/1 and then we load
%	them.

load_db :-
	rdf_directory(Dir),
	working_directory(Old, Dir),
	call_cleanup(find_dbs(DBs), working_directory(_, Old)),
	make_goals(DBs, Goals),
	concurrency(Jobs),
	concurrent(Jobs, Goals, []).

make_goals([], []).
make_goals([DB|T0], [load_source(DB)|T]) :-
	make_goals(T0, T).

%%	concurrency(-Jobs)
%
%	Number of jobs to run concurrently.

concurrency(Jobs) :-
	rdf_option(concurrency(Jobs)), !.
concurrency(Jobs) :-
	current_prolog_flag(cpu_count, Jobs),
	Jobs > 0, !.
concurrency(1).


%%	find_dbs(-DBs:list(atom)) is det.
%
%	DBs is a  list  of  database   (named  graph)  names,  sorted in
%	increasing file-size. Small files  are   loaded  first  as these
%	typically contain the schemas and we   want  to avoid re-hashing
%	large databases due to added rdfs:subPropertyOf triples.

find_dbs(DBs) :-
	expand_file_name(*, Files),
	dbs(Files, DBs0),
	sort(DBs0, DBs1),		% remove duplicates
	maplist(key_by_size, DBs1, DBK),
	keysort(DBK, DBK1),
	unkey(DBK1, DBs).
	
dbs([], []).
dbs([H0|T0], [H|T]) :-
	file_name_extension(Base, Ext, H0),
	db_extension(Ext), !,
	rdf_db_to_file(H, Base),
	dbs(T0, T).
dbs([_|T0], T) :-
	dbs(T0, T).

db_extension(trp).
db_extension(jrn).

key_by_size(DB, Size-DB) :-
	rdf_db_to_file(DB, Base),
	ext_size(Base, trp, S1),
	ext_size(Base, jrn, S2),
	Size is S1 + S2.

ext_size(Base, Ext, Size) :-
	file_name_extension(Base, Ext, File),
	(   exists_file(File)
	->  size_file(File, Size)
	;   Size = 0
	).

unkey([], []).
unkey([_-H|T0], [H|T]) :-
	unkey(T0, T).


%%	load_source(+DB) is det.
%	
%	Load triples and reload  journal   from  the  indicated snapshot
%	file.

load_source(DB) :-
	db_files(DB, SnapshotFile, JournalFile),
	rdf_retractall(_,_,_,DB),
	statistics(cputime, T0),
	print_message(informational, rdf(restore(source(DB)))),
	(   exists_db(SnapshotFile)
	->  print_message(informational, rdf(restore(snapshot(SnapshotFile)))),
	    db_file(SnapshotFile, AbsFile),
	    rdf_load_db(AbsFile)
	;   true
	),
	(   exists_db(JournalFile)
	->  print_message(informational, rdf(restore(journal(JournalFile)))),
	    load_journal(JournalFile, DB)
	;   true
	),
	statistics(cputime, T1),
	T is T1 - T0,
	(   rdf_statistics(triples_by_file(DB, Count))
	->  true
	;   Count = 0
	),
	print_message(informational, rdf(restore(done(DB, T, Count)))).
	
	
		 /*******************************
		 *	   LOAD JOURNAL		*
		 *******************************/
	
%%	load_journal(+File:atom, +DB:atom) is det.
%
%	Process transactions from the RDF journal File, adding the given
%	named graph.

load_journal(File, DB) :-
	open_db(File, read, In,
		[ encoding(utf8)
		]),
	call_cleanup((  read(In, T0),
			process_journal(T0, In, DB)
		     ),
		     close(In)).
	
process_journal(end_of_file, _, _) :- !.
process_journal(Term, In, DB) :-
	(   process_journal_term(Term, DB)
	->  true
	;   throw(error(type_error(journal_term, Term), _))
	),
	read(In, T2),
	process_journal(T2, In, DB).

process_journal_term(assert(S,P,O), DB) :-
	rdf_assert(S,P,O,DB).
process_journal_term(assert(S,P,O,Line), DB) :-
	rdf_assert(S,P,O,DB:Line).
process_journal_term(retract(S,P,O), DB) :-
	rdf_retractall(S,P,O,DB).
process_journal_term(retract(S,P,O,Line), DB) :-
	rdf_retractall(S,P,O,DB:Line).
process_journal_term(update(S,P,O,Action), DB) :-
	(   rdf_update(S,P,O,DB, Action)
	->  true
	;   print_message(warning, rdf(update_failed(S,P,O,Action)))
	).
process_journal_term(start(_), _).
process_journal_term(end(_), _).
process_journal_term(begin(_), _).
process_journal_term(end, _).


		 /*******************************
		 *	   CREATE JOURNAL	*
		 *******************************/

:- dynamic
	blocked_db/2,			% DB, Reason
	transaction_message/2,		% Id, Message
	transaction_db/2.		% Id, DB

%%	rdf_persistency(+DB, Bool)
%
%	Specify whether a database is persistent.  Switching to =false=
%       kills the persistent state.  Switching to =true= creates it.

rdf_persistency(DB, Bool) :-
	must_be(atom, DB),
	must_be(boolean, Bool),
	fail.
rdf_persistency(DB, false) :- !,
	(   blocked_db(DB, persistency)
	->  true
	;   assert(blocked_db(DB, persistency)),
	    delete_db(DB)
	).
rdf_persistency(DB, true) :-
	(   retract(blocked_db(DB, persistency))
	->  create_db(DB)
	;   true
	).


%%	start_monitor is det.
%%	stop_monitor is det.
%
%	Start/stop monitoring the RDF database   for  changes and update
%	the journal.

start_monitor :-
	rdf_monitor(monitor,
		    [ -assert(load)
		    ]).
stop_monitor :-
	rdf_monitor(monitor,
		    [ -all
		    ]).

monitor(Msg) :-
	debug(monitor, 'Monitor: ~p~n', [Msg]),
	fail.
monitor(assert(S,P,O,DB:Line)) :- !,
	\+ blocked_db(DB, _),
	journal_fd(DB, Fd),
	open_transaction(DB, Fd),
	format(Fd, '~q.~n', [assert(S,P,O,Line)]),
	sync_journal(DB, Fd).
monitor(assert(S,P,O,DB)) :-
	\+ blocked_db(DB, _),
	journal_fd(DB, Fd),
	open_transaction(DB, Fd),
	format(Fd, '~q.~n', [assert(S,P,O)]),
	sync_journal(DB, Fd).
monitor(retract(S,P,O,DB:Line)) :- !,
	\+ blocked_db(DB, _),
	journal_fd(DB, Fd),
	open_transaction(DB, Fd),
	format(Fd, '~q.~n', [retract(S,P,O,Line)]),
	sync_journal(DB, Fd).
monitor(retract(S,P,O,DB)) :-
	\+ blocked_db(DB, _),
	journal_fd(DB, Fd),
	open_transaction(DB, Fd),
	format(Fd, '~q.~n', [retract(S,P,O)]),
	sync_journal(DB, Fd).
monitor(update(S,P,O,DB:Line,Action)) :- !,
	\+ blocked_db(DB, _),
	(   Action = source(NewDB)
	->  monitor(assert(S,P,O,NewDB)),
	    monitor(retract(S,P,O,DB:Line))
	;   journal_fd(DB, Fd),
	    format(Fd, '~q.~n', [update(S,P,O,Action)]),
	    sync_journal(DB, Fd)
	).
monitor(update(S,P,O,DB,Action)) :-
	\+ blocked_db(DB, _),
	(   Action = source(NewDB)
	->  monitor(assert(S,P,O,NewDB)),
	    monitor(retract(S,P,O,DB))
	;   journal_fd(DB, Fd),
	    open_transaction(DB, Fd),
	    format(Fd, '~q.~n', [update(S,P,O,Action)]),
	    sync_journal(DB, Fd)
	).
monitor(load(BE, Id)) :-
	(   BE == begin
	->  push_state(Id)
	;   sync_state(Id)
	).
monitor(transaction(BE, Id)) :-
	monitor_transaction(Id, BE).

monitor_transaction(load_journal(DB), begin) :- !,
	assert(blocked_db(DB, journal)).
monitor_transaction(load_journal(DB), end) :- !,
	retractall(blocked_db(DB, journal)).

monitor_transaction(parse(URI), begin) :- !,
	(   blocked_db(URI, persistency)
	->  true
	;   assert(blocked_db(URI, parse))
	).
monitor_transaction(parse(URI), end) :- !,
	(   retract(blocked_db(URI, parse))
	->  create_db(URI)
	;   true
	).
monitor_transaction(unload(DB), begin) :- !,
	(   blocked_db(DB, persistency)
	->  true
	;   assert(blocked_db(DB, unload))
	).
monitor_transaction(unload(DB), end) :- !,
	(   retract(blocked_db(DB, unload))
	->  delete_db(DB)
	;   true
	).
monitor_transaction(log(Msg), begin) :- !,
	(   predicate_property(transaction_message(_,_),
			       number_of_clauses(N))
	->  true
	;   N = 0
	),
	asserta(transaction_message(N, Msg)).
monitor_transaction(log(_), end) :- !,
	retract(transaction_message(N, _)),
	(   retract(transaction_db(N, DB)),
	    journal_fd(DB, Fd),
	    format(Fd, 'end.~n', []),
	    sync_journal(DB, Fd),
	    fail
	;   true
	).
monitor_transaction(reset, begin) :-
	forall(rdf_source(DB),
	       monitor_transaction(unload(DB), begin)).
monitor_transaction(reset, end) :-
	forall(blocked_db(DB, unload),
	       monitor_transaction(unload(DB), end)).
	       
open_transaction(DB, Fd) :-
	transaction_message(N, Msg), !,
	(   transaction_db(N, DB)
	->  true
	;   assert(transaction_db(N, DB)),
	    format(Fd, 'begin(~q).~n', [Msg])
	).
open_transaction(_,_).

%	State  handling.  We  use   this    for   trapping   changes  by
%	rdf_load_db/1. In theory, loading such files  can add triples to
%	multiple sources. In practice this rarely   happens. We save the
%	current state and sync all  files   that  have changed. The only
%	drawback of this approach is that loaded files spreading triples
%	over multiple databases cause all these   databases  to be fully
%	synchronised. This shouldn't happen very often.

:- dynamic
	pre_load_state/2.

push_state(Id) :-
	get_state(State),
	asserta(pre_load_state(Id, State)).

get_state(State) :-
	findall(DB-MD5, (rdf_source(DB), rdf_md5(DB, MD5)), State0),
	keysort(State0, State).

sync_state(Id) :-
	retract(pre_load_state(Id, PreState)),
	get_state(AfterState),
	sync_state(AfterState, PreState).

sync_state([], _).
sync_state([DB-MD5|TA], Pre) :-
	(   memberchk(DB-MD5P, Pre),
	    MD5P == MD5
	->  true
	;   create_db(DB)
	),
	sync_state(TA, Pre).


		 /*******************************
		 *	   JOURNAL FILES	*
		 *******************************/

%%	journal_fd(+DB, -Stream) is det.
%	
%	Get an open stream to a journal. If the journal is not open, old
%	journals are closed to satisfy   the =max_open_journals= option.
%	Then the journal is opened in   =append= mode. Journal files are
%	always encoded as UTF-8 for  portability   as  well as to ensure
%	full coverage of Unicode.

journal_fd(DB, Fd) :-
	source_journal_fd(DB, Fd), !.
journal_fd(DB, Fd) :-
	with_mutex(rdf_journal_file,
		   journal_fd_(DB, Out)),
	Fd = Out.

journal_fd_(DB, Fd) :-
	source_journal_fd(DB, Fd), !.
journal_fd_(DB, Fd) :-
	limit_fd_pool,
	db_files(DB, _Snapshot, Journal),
	open_db(Journal, append, Fd,
		[ encoding(utf8),
		  close_on_abort(false)
		]),
	time_stamp(Now),
	format(Fd, '~q.~n', [start([time(Now)])]),
	assert(source_journal_fd(DB, Fd)).		% new one at the end

%%	limit_fd_pool is det.
%
%	Limit the number of  open   journals  to max_open_journals (10).
%	Note that calls  from  rdf_monitor/2   are  issued  in different
%	threads, but as they are part of write operations they are fully
%	synchronised.

limit_fd_pool :-
	predicate_property(source_journal_fd(_, _), number_of_clauses(N)), !,
	(   rdf_option(max_open_journals(Max))
	->  true
	;   Max = 10
	),
	Close is N - Max,
	forall(between(1, Close, _),
	       close_oldest_journal).
limit_fd_pool.

close_oldest_journal :-
	source_journal_fd(DB, _Fd), !,
	debug(rdf_persistency, 'Closing old journal for ~q', [DB]),
	close_journal(DB).
close_oldest_journal.
		       

%%	sync_journal(+DB, +Fd)
%	
%	Sync journal represented by database and   stream.  If the DB is
%	involved in a transaction there is   no point flushing until the
%	end of the transaction.

sync_journal(DB, _) :-
	transaction_db(_, DB), !.
sync_journal(_, Fd) :-
	flush_output(Fd).

%%	close_journal(+DB) is det.
%	
%	Close the journal associated with DB if it is open.

close_journal(DB) :-
	with_mutex(rdf_journal_file,
		   close_journal_(DB)).

close_journal_(DB) :-
	(   retract(source_journal_fd(DB, Fd))
	->  time_stamp(Now),
	    format(Fd, '~q.~n', [end([time(Now)])]),
	    close(Fd, [force(true)])
	;   true
	).

%	close_journals
%	
%	Close all open journals.

close_journals :-
	forall(source_journal_fd(DB, _),
	       catch(close_journal(DB), E,
		     print_message(error, E))).

%%	create_db(+DB)
%	
%	Create a saved version of DB in corresponding file, close and
%	delete journals.

create_db(DB) :-
	debug(rdf_persistency, 'Saving DB ~w', [DB]),
	db_abs_files(DB, Snapshot, Journal),
	atom_concat(Snapshot, '.new', NewSnapshot),
	(   catch(rdf_save_db(NewSnapshot, DB), _, fail)
	->  (   exists_file(Journal)
	    ->  delete_file(Journal)
	    ;   true
	    ),
	    rename_file(NewSnapshot, Snapshot),
	    debug(rdf_persistency, 'Saved DB ~w', [DB])
	;   catch(delete_file(NewSnapshot), _, true)
	).


%%	delete_db(+DB)
%	
%	Remove snapshot and journal file for DB.

delete_db(DB) :-
	db_abs_files(DB, Snapshot, Journal),
	(   exists_file(Journal)
	->  delete_file(Journal)
	;   true
	),
	(   exists_file(Snapshot)
	->  delete_file(Snapshot)
	;   true
	).


		 /*******************************
		 *	       LOCKING		*
		 *******************************/

%%	lock_db(+Dir)
%	
%	Lock the database  directory.  This  isn't   safe  as  the  file
%	operations are not  atomic.  Needs   re-thinking,  but  with the
%	normal server setting it should be ok.

lock_db(Dir) :-
	lockfile(Dir, File),
	exists_file(File), !,
	throw(error(permission_error(lock, Dir),
		    context(_, 'Database is in use'))).
lock_db(Dir) :-
	lockfile(Dir, File),
	open(File, write, Out),
	(   current_prolog_flag(pid, PID)
	->  true
	;   PID = 0			% TBD: Fix in Prolog
	),
	time_stamp(Now),
	format(Out, '/* RDF Database is in use */~n~n', []),
	format(Out, '~q.~n', [ locked([ time(Now),
					pid(PID)
				      ])
			     ]),
	close(Out),
	at_halt(unlock_db(Dir)).

unlock_db(Dir) :-
	lockfile(Dir, File),
	(   exists_file(File)
	->  delete_file(File)
	;   true
	).

		 /*******************************
		 *	     FILENAMES		*
		 *******************************/

lockfile(Dir, LockFile) :-
	concat_atom([Dir, /, lock], LockFile).

db_file(Base, File) :-
	rdf_directory(Dir),
	concat_atom([Dir, /, Base], File).

open_db(Base, Mode, Stream, Options) :-
	db_file(Base, File),
	open(File, Mode, Stream, Options).

exists_db(Base) :-
	db_file(Base, File),
	exists_file(File).

%%	db_files(+DB, -Snapshot, -Journal).
%%	db_files(-DB, +Snapshot, -Journal).
%%	db_files(-DB, -Snapshot, +Journal).
%
%	True if named graph DB is represented  by the files Snapshot and
%	Journal. The filenames are local   to the directory representing
%	the store.

db_files(DB, Snapshot, Journal) :-
	nonvar(DB), !,
	rdf_db_to_file(DB, Base),
	atom_concat(Base, '.trp', Snapshot),
	atom_concat(Base, '.jrn', Journal).
db_files(DB, Snapshot, Journal) :-
	nonvar(Snapshot), !,
	atom_concat(Base, '.trp', Snapshot),
	atom_concat(Base, '.jrn', Journal),
	rdf_db_to_file(DB, Base).
db_files(DB, Snapshot, Journal) :-
	nonvar(Journal), !,
	atom_concat(Base, '.jrn', Journal),
	atom_concat(Base, '.trp', Snapshot),
	rdf_db_to_file(DB, Base).

db_abs_files(DB, Snapshot, Journal) :-
	db_files(DB, Snapshot0, Journal0),
	db_file(Snapshot0, Snapshot),
	db_file(Journal0, Journal).

%%	rdf_journal_file(?DB, -File)
%	
%	Return the journal files of  the   current  server. Intended for
%	external modules for merging journals.

rdf_journal_file(DB, Journal) :-
	rdf_source(DB),
	db_abs_files(DB, _Snapshot, Journal),
	exists_file(Journal).


%%	rdf_db_to_file(+DB, -File) is det.
%%	rdf_db_to_file(-DB, +File) is det.
%	
%	Translate between database encoding (often an   file or URL) and
%	the name we store in the  directory.   We  keep  a cache for two
%	reasons. Speed, but much more important   is that the mapping of
%	raw --> encoded provided by  www_form_encode/2 is not guaranteed
%	to be unique by the W3C standards.

rdf_db_to_file(DB, File) :-
	db_file_base(DB, File), !.
rdf_db_to_file(DB, File) :-
	url_to_filename(DB, File),
	assert(db_file_base(DB, File)).

%%	url_to_filename(+URL, -FileName) is det.
%%	url_to_filename(-URL, +FileName) is det.
%	
%	Turn  a  valid  URL  into  a  filename.  Earlier  versions  used
%	www_form_encode/2, but this can produce  characters that are not
%	valid  in  filenames.  We  will  use    the   same  encoding  as
%	www_form_encode/2,  but  using  our  own    rules   for  allowed
%	characters. The only requirement is that   we avoid any filename
%	special character in use.  The   current  encoding  use US-ASCII
%	alnum characters, _ and %

url_to_filename(URL, FileName) :-
	atomic(URL), !,
	atom_codes(URL, Codes),
	phrase(url_encode(EncCodes), Codes),
	atom_codes(FileName, EncCodes).
url_to_filename(URL, FileName) :-
	www_form_encode(URL, FileName).

url_encode([0'+|T]) -->
	" ", !,
        url_encode(T).
url_encode([C|T]) -->
	alphanum(C), !,
	url_encode(T).
url_encode([C|T]) -->
	no_enc_extra(C), !,
	url_encode(T).
url_encode(Enc) -->
	(   "\r\n"
	;   "\n"
	), !,
	{ append("%0D%0A", T, Enc)
	},
	url_encode(T).
url_encode([]) -->
	[], !.
url_encode([0'%,D1,D2|T]) -->
	[C],
	{ Dv1 is (C>>4 /\ 0xf),
	  Dv2 is (C /\ 0xf),
	  code_type(D1, xdigit(Dv1)),
	  code_type(D2, xdigit(Dv2))
	},
	url_encode(T).

alphanum(C) -->
	[C],
	{ C < 128,			% US-ASCII
	  code_type(C, alnum)
	}.

no_enc_extra(0'_) --> "_".		%'


		 /*******************************
		 *		UTIL		*
		 *******************************/

%%	mkdir(+Directory)
%	
%	Create a directory if it does not already exist.

mkdir(Directory) :-
	exists_directory(Directory), !.
mkdir(Directory) :-
	make_directory(Directory).

%%	time_stamp(-Integer)
%	
%	Return time-stamp rounded to integer.

time_stamp(Int) :-
	get_time(Now),
	Int is round(Now).


		 /*******************************
		 *	      MESSAGES		*
		 *******************************/

:- multifile
	prolog:message/3.

prolog:message(rdf(restore(source(DB)))) -->
	{ file_base_name(DB, Base) },
	[ 'Restoring ~w ... '-[Base], flush ].
prolog:message(rdf(restore(snapshot(_)))) -->
	[ at_same_line, '(snapshot) '-[], flush ].
prolog:message(rdf(restore(journal(_)))) -->
	[ at_same_line, '(journal) '-[], flush ].
prolog:message(rdf(restore(done(_, Time, Count)))) -->
	[ at_same_line, '~D triples in ~2f sec.'-[Count, Time] ].
prolog:message(rdf(update_failed(S,P,O,Action))) -->
	[ 'Failed to update <~p ~p ~p> with ~p'-[S,P,O,Action] ].
