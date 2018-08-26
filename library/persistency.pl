/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2009-2017, VU University, Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(persistency,
          [ (persistent)/1,             % +Declarations
            current_persistent_predicate/1, % :PI

            db_attach/2,                % :File, +Options
            db_detach/0,
            db_attached/1,              % :File

            db_sync/1,                  % :What
            db_sync_all/1,              % +What

            op(1150, fx, (persistent))
          ]).
:- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(library(option)).
:- use_module(library(aggregate)).

:- predicate_options(db_attach/2, 2,
                     [ sync(oneof([close,flush,none]))
                     ]).

/** <module> Provide persistent dynamic predicates

This module provides simple persistent storage   for one or more dynamic
predicates. A database is always associated with a module. A module that
wishes to maintain a database must declare  the terms that can be placed
in the database using the directive persistent/1.

The persistent/1 expands each declaration into four predicates:

        * name(Arg, ...)
        * assert_name(Arg, ...)
        * retract_name(Arg, ...)
        * retractall_name(Arg, ...)

As mentioned, a database can  only  be   accessed  from  within a single
module. This limitation is on purpose,  forcing   the  user to provide a
proper API for accessing the shared persistent data.

Below is a simple example:

==
:- module(user_db,
          [ attach_user_db/1,           % +File
            current_user_role/2,        % ?User, ?Role
            add_user/2,                 % +User, +Role
            set_user_role/2             % +User, +Role
          ]).
:- use_module(library(persistency)).

:- persistent
        user_role(name:atom, role:oneof([user,administrator])).

attach_user_db(File) :-
        db_attach(File, []).

%%      current_user_role(+Name, -Role) is semidet.

current_user_role(Name, Role) :-
        with_mutex(user_db, user_role(Name, Role)).

add_user(Name, Role) :-
        assert_user_role(Name, Role).

set_user_role(Name, Role) :-
        user_role(Name, Role), !.
set_user_role(Name, Role) :-
        with_mutex(user_db,
                   (  retractall_user_role(Name, _),
                      assert_user_role(Name, Role))).
==

@tbd    Provide type safety while loading
@tbd    Thread safety must now be provided at the user-level. Can we
        provide generic thread safety?  Basically, this means that we
        must wrap all exported predicates.  That might better be done
        outside this library.
@tbd    Transaction management?
@tbd    Should assert_<name> only assert if the database does not
        contain a variant?
*/

:- meta_predicate
    db_attach(:, +),
    db_attached(:),
    db_sync(:),
    current_persistent_predicate(:).
:- module_transparent
    db_detach/0.


                 /*******************************
                 *              DB              *
                 *******************************/

:- dynamic
    db_file/5,                      % Module, File, Created, Modified, EndPos
    db_stream/2,                    % Module, Stream
    db_dirty/2,                     % Module, Deleted
    db_option/2.                    % Module, Name(Value)

:- volatile
    db_stream/2.

:- multifile
    (persistent)/3,                 % Module, Generic, Term
    prolog:generated_predicate/1.


                 /*******************************
                 *         DECLARATIONS         *
                 *******************************/

%!  persistent(+Spec)
%
%   Declare dynamic database terms. Declarations appear in a
%   directive and have the following format:
%
%   ==
%   :- persistent
%           <callable>,
%           <callable>,
%           ...
%   ==
%
%   Each specification is a callable term, following the conventions
%   of library(record), where each argument is of the form
%
%           name:type
%
%   Types are defined by library(error).

persistent(Spec) :-
    throw(error(context_error(nodirective, persistent(Spec)), _)).

compile_persistent(Var, _, _) -->
    { var(Var),
      !,
      instantiation_error(Var)
    }.
compile_persistent(M:Spec, _, LoadModule) -->
    !,
    compile_persistent(Spec, M, LoadModule).
compile_persistent((A,B), Module, LoadModule) -->
    !,
    compile_persistent(A, Module, LoadModule),
    compile_persistent(B, Module, LoadModule).
compile_persistent(Term, Module, LoadModule) -->
    { functor(Term, Name, Arity),           % Validates Term as callable
      functor(Generic, Name, Arity),
      qualify(Module, LoadModule, Name/Arity, Dynamic)
    },
    [ :- dynamic(Dynamic),

      persistency:persistent(Module, Generic, Term)
    ],
    assert_clause(asserta, Term, Module, LoadModule),
    assert_clause(assert,  Term, Module, LoadModule),
    retract_clause(Term, Module, LoadModule),
    retractall_clause(Term, Module, LoadModule).

assert_clause(Where, Term, Module, LoadModule) -->
    { functor(Term, Name, Arity),
      atomic_list_concat([Where,'_', Name], PredName),
      length(Args, Arity),
      Head =.. [PredName|Args],
      Assert =.. [Name|Args],
      type_checkers(Args, 1, Term, Check),
      atom_concat(db_, Where, DBActionName),
      DBAction =.. [DBActionName, Module:Assert],
      qualify(Module, LoadModule, Head, QHead),
      Clause = (QHead :- Check, persistency:DBAction)
    },
    [ Clause ].

type_checkers([], _, _, true).
type_checkers([A0|AL], I, Spec, Check) :-
    arg(I, Spec, ArgSpec),
    (   ArgSpec = _Name:Type,
        nonvar(Type),
        Type \== any
    ->  Check = (must_be(Type, A0),More)
    ;   More = Check
    ),
    I2 is I + 1,
    type_checkers(AL, I2, Spec, More).

retract_clause(Term, Module, LoadModule) -->
    { functor(Term, Name, Arity),
      atom_concat(retract_, Name, PredName),
      length(Args, Arity),
      Head =.. [PredName|Args],
      Retract =.. [Name|Args],
      qualify(Module, LoadModule, Head, QHead),
      Clause = (QHead :- persistency:db_retract(Module:Retract))
    },
    [ Clause ].

retractall_clause(Term, Module, LoadModule) -->
    { functor(Term, Name, Arity),
      atom_concat(retractall_, Name, PredName),
      length(Args, Arity),
      Head =.. [PredName|Args],
      Retract =.. [Name|Args],
      qualify(Module, LoadModule, Head, QHead),
      Clause = (QHead :- persistency:db_retractall(Module:Retract))
    },
    [ Clause ].

qualify(Module, Module, Head, Head) :- !.
qualify(Module, _LoadModule, Head, Module:Head).


:- multifile
    system:term_expansion/2.

system:term_expansion((:- persistent(Spec)), Clauses) :-
    prolog_load_context(module, Module),
    phrase(compile_persistent(Spec, Module, Module), Clauses).


%!  current_persistent_predicate(:PI) is nondet.
%
%   True if PI is a predicate that provides access to the persistent
%   database DB.

current_persistent_predicate(M:PName/Arity) :-
    persistency:persistent(M, Generic, _),
    functor(Generic, Name, Arity),
    (   Name = PName
    ;   atom_concat(assert_, Name, PName)
    ;   atom_concat(retract_, Name, PName)
    ;   atom_concat(retractall_, Name, PName)
    ).

prolog:generated_predicate(PI) :-
    current_persistent_predicate(PI).


                 /*******************************
                 *            ATTACH            *
                 *******************************/

%!  db_attach(:File, +Options)
%
%   Use File as persistent database for  the calling module. The calling
%   module must defined persistent/1  to   declare  the  database terms.
%   Defined options:
%
%     - sync(+Sync)
%       One of =close= (close journal after write), =flush=
%       (default, flush journal after write) or =none=
%       (handle as fully buffered stream).
%
%   If File is already attached  this   operation  may change the `sync`
%   behaviour.

db_attach(Module:File, Options) :-
    db_set_options(Module, Options),
    db_attach_file(Module, File).

db_set_options(Module, Options) :-
    option(sync(Sync), Options, flush),
    must_be(oneof([close,flush,none]), Sync),
    (   db_option(Module, sync(Sync))
    ->  true
    ;   retractall(db_option(Module, _)),
        assert(db_option(Module, sync(Sync)))
    ).

db_attach_file(Module, File) :-
    db_file(Module, Old, _, _, _),         % we already have a db
    !,
    (   Old == File
    ->  (   db_stream(Module, Stream)
        ->  sync(Module, Stream)
        ;   true
        )
    ;   permission_error(attach, db, File)
    ).
db_attach_file(Module, File) :-
    db_load(Module, File),
    !.
db_attach_file(Module, File) :-
    assert(db_file(Module, File, 0, 0, 0)).

db_load(Module, File) :-
    retractall(db_file(Module, _, _, _, _)),
    debug(db, 'Loading database ~w', [File]),
    catch(setup_call_cleanup(
              open(File, read, In, [encoding(utf8)]),
              load_db_end(In, Module, Created, EndPos),
              close(In)),
          error(existence_error(source_sink, File), _), fail),
    debug(db, 'Loaded ~w', [File]),
    time_file(File, Modified),
    assert(db_file(Module, File, Created, Modified, EndPos)).

db_load_incremental(Module, File) :-
    db_file(Module, File, Created, _, EndPos0),
    setup_call_cleanup(
        ( open(File, read, In, [encoding(utf8)]),
          read_action(In, created(Created0)),
          set_stream_position(In, EndPos0)
        ),
        ( Created0 == Created,
          debug(db, 'Incremental load from ~p', [EndPos0]),
          load_db_end(In, Module, _Created, EndPos)
        ),
        close(In)),
    debug(db, 'Updated ~w', [File]),
    time_file(File, Modified),
    retractall(db_file(Module, File, Created, _, _)),
    assert(db_file(Module, File, Created, Modified, EndPos)).

load_db_end(In, Module, Created, End) :-
    read_action(In, T0),
    (   T0 = created(Created)
    ->  read_action(In, T1)
    ;   T1 = T0,
        Created = 0
    ),
    load_db(T1, In, Module),
    stream_property(In, position(End)).

load_db(end_of_file, _, _) :- !.
load_db(assert(Term), In, Module) :-
    persistent(Module, Term, _Types),
    !,
    assert(Module:Term),
    read_action(In, T1),
    load_db(T1, In, Module).
load_db(asserta(Term), In, Module) :-
    persistent(Module, Term, _Types),
    !,
    asserta(Module:Term),
    read_action(In, T1),
    load_db(T1, In, Module).
load_db(retractall(Term, Count), In, Module) :-
    persistent(Module, Term, _Types),
    !,
    retractall(Module:Term),
    set_dirty(Module, Count),
    read_action(In, T1),
    load_db(T1, In, Module).
load_db(retract(Term), In, Module) :-
    persistent(Module, Term, _Types),
    !,
    (   retract(Module:Term)
    ->  set_dirty(Module, 1)
    ;   true
    ),
    read_action(In, T1),
    load_db(T1, In, Module).
load_db(Term, In, Module) :-
    print_message(error, illegal_term(Term)),
    read_action(In, T1),
    load_db(T1, In, Module).

db_clean(Module) :-
    retractall(db_dirty(Module, _)),
    (   persistent(Module, Term, _Types),
        retractall(Module:Term),
        fail
    ;   true
    ).

%!  db_size(+Module, -Terms) is det.
%
%   Terms is the total number of terms in the DB for Module.

db_size(Module, Total) :-
    aggregate_all(sum(Count), persistent_size(Module, Count), Total).

persistent_size(Module, Count) :-
    persistent(Module, Term, _Types),
    predicate_property(Module:Term, number_of_clauses(Count)).

%!  db_attached(:File) is semidet.
%
%   True if the context module attached to the persistent database File.

db_attached(Module:File) :-
    db_file(Module, File, _Created, _Modified, _EndPos).

%!  db_assert(:Term) is det.
%
%   Assert Term into the database  and   record  it for persistency.
%   Note that if the on-disk file  has   been  modified  it is first
%   reloaded.

:- public
    db_assert/1,
    db_asserta/1,
    db_retractall/1,
    db_retract/1.

db_assert(Module:Term) :-
    assert(Module:Term),
    persistent(Module, assert(Term)).

db_asserta(Module:Term) :-
    asserta(Module:Term),
    persistent(Module, asserta(Term)).

persistent(Module, Action) :-
    (   db_stream(Module, Stream)
    ->  true
    ;   db_file(Module, File, _Created, _Modified, _EndPos)
    ->  db_sync(Module, update),            % Is this correct?
        db_open_file(File, append, Stream),
        assert(db_stream(Module, Stream))
    ;   existence_error(db_file, Module)
    ),
    write_action(Stream, Action),
    sync(Module, Stream).

db_open_file(File, Mode, Stream) :-
    open(File, Mode, Stream,
         [ close_on_abort(false),
           encoding(utf8),
           lock(write)
         ]),
    (   size_file(File, 0)
    ->  get_time(Now),
        write_action(Stream, created(Now))
    ;   true
    ).


%!  db_detach is det.
%
%   Detach persistency from  the  calling   module  and  delete  all
%   persistent clauses from the Prolog database.  Note that the file
%   is not affected. After  this  operation   another  file  may  be
%   attached,  providing  it   satisfies    the   same   persistency
%   declaration.

db_detach :-
    context_module(Module),
    db_sync(Module:detach),
    db_clean(Module).


%!  sync(+Module, +Stream) is det.
%
%   Synchronise journal after a write.   Using  =close=, the journal
%   file is closed, making it easier   to  edit the file externally.
%   Using =flush= flushes the stream  but   does  not close it. This
%   provides better performance. Using  =none=,   the  stream is not
%   even flushed. This makes the journal   sensitive to crashes, but
%   much faster.

sync(Module, Stream) :-
    db_option(Module, sync(Sync)),
    (   Sync == close
    ->  db_sync(Module, close)
    ;   Sync == flush
    ->  flush_output(Stream)
    ;   true
    ).

read_action(Stream, Action) :-
    read_term(Stream, Action, [module(db)]).

write_action(Stream, Action) :-
    \+ \+ ( numbervars(Action, 0, _, [singletons(true)]),
            format(Stream, '~W.~n',
                   [ Action,
                     [ quoted(true),
                       numbervars(true),
                       module(db)
                     ]
                   ])
          ).

%!  db_retractall(:Term) is det.
%
%   Retract all matching facts and do the   same in the database. If
%   Term is unbound, persistent/1 from the   calling  module is used as
%   generator.

db_retractall(Module:Term) :-
    (   var(Term)
    ->  forall(persistent(Module, Term, _Types),
               db_retractall(Module:Term))
    ;   State = count(0),
        (   retract(Module:Term),
            arg(1, State, C0),
            C1 is C0+1,
            nb_setarg(1, State, C1),
            fail
        ;   arg(1, State, Count)
        ),
        (   Count > 0
        ->  set_dirty(Module, Count),
            persistent(Module, retractall(Term, Count))
        ;   true
        )
    ).


%!  db_retract(:Term) is nondet.
%
%   Retract terms from the database one-by-one.

db_retract(Module:Term) :-
    (   var(Term)
    ->  instantiation_error(Term)
    ;   retract(Module:Term),
        set_dirty(Module, 1),
        persistent(Module, retract(Term))
    ).


set_dirty(_, 0) :- !.
set_dirty(Module, Count) :-
    (   retract(db_dirty(Module, C0))
    ->  true
    ;   C0 = 0
    ),
    C1 is C0 + Count,
    assert(db_dirty(Module, C1)).

%!  db_sync(:What)
%
%   Synchronise database with the associated file.  What is one of:
%
%     * reload
%     Database is reloaded from file if the file was modified
%     since loaded.
%     * update
%     As `reload`, but use incremental loading if possible.
%     This allows for two processes to examine the same database
%     file, where one writes the database and the other periodycally
%     calls db_sync(update) to follow the modified data.
%     * gc
%     Database was re-written, deleting all retractall
%     statements.  This is the same as gc(50).
%     * gc(Percentage)
%     GC DB if the number of deleted terms is greater than the given
%     percentage of the total number of terms.
%     * gc(always)
%     GC DB without checking the percentage.
%     * close
%     Database stream was closed
%     * detach
%     Remove all registered persistency for the calling module
%     * nop
%     No-operation performed
%
%   With unbound What, db_sync/1 reloads  the   database  if  it was
%   modified on disk, gc it if it  is   dirty  and close it if it is
%   opened.

db_sync(Module:What) :-
    db_sync(Module, What).


db_sync(Module, reload) :-
    \+ db_stream(Module, _),                % not open
    db_file(Module, File, _Created, ModifiedWhenLoaded, _EndPos),
    catch(time_file(File, Modified), _, fail),
    Modified > ModifiedWhenLoaded,         % Externally modified
    !,
    debug(db, 'Database ~w was externally modified; reloading', [File]),
    !,
    (   catch(db_load_incremental(Module, File),
              E,
              ( print_message(warning, E), fail ))
    ->  true
    ;   db_clean(Module),
        db_load(Module, File)
    ).
db_sync(Module, gc) :-
    !,
    db_sync(Module, gc(50)).
db_sync(Module, gc(When)) :-
    db_dirty(Module, Dirty),
    (   When == always
    ->  true
    ;   db_size(Module, Total),
        (   Total > 0
        ->  Perc is (100*Dirty)/Total,
            Perc > When
        ;   Dirty > 0
        )
    ),
    !,
    db_sync(Module, close),
    db_file(Module, File, _, Modified, _),
    atom_concat(File, '.new', NewFile),
    debug(db, 'Database ~w is dirty; cleaning', [File]),
    get_time(Created),
    catch(setup_call_cleanup(
              db_open_file(NewFile, write, Out),
              (   persistent(Module, Term, _Types),
                  call(Module:Term),
                  write_action(Out, assert(Term)),
                  fail
              ;   stream_property(Out, position(EndPos))
              ),
              close(Out)),
          Error,
          ( catch(delete_file(NewFile),_,fail),
            throw(Error))),
    retractall(db_file(Module, File, _, Modified, _)),
    rename_file(NewFile, File),
    time_file(File, NewModified),
    assert(db_file(Module, File, Created, NewModified, EndPos)).
db_sync(Module, close) :-
    retract(db_stream(Module, Stream)),
    !,
    db_file(Module, File, Created, _, _),
    debug(db, 'Database ~w is open; closing', [File]),
    stream_property(Stream, position(EndPos)),
    close(Stream),
    time_file(File, Modified),
    retractall(db_file(Module, File, _, _, _)),
    assert(db_file(Module, File, Created, Modified, EndPos)).
db_sync(Module, Action) :-
    Action == detach,
    !,
    (   retract(db_stream(Module, Stream))
    ->  close(Stream)
    ;   true
    ),
    retractall(db_file(Module, _, _, _, _)),
    retractall(db_dirty(Module, _)),
    retractall(db_option(Module, _)).
db_sync(_, nop) :- !.
db_sync(_, _).


%!  db_sync_all(+What)
%
%   Sync all registered databases.

db_sync_all(What) :-
    must_be(oneof([reload,gc,gc(_),close]), What),
    forall(db_file(Module, _, _, _, _),
           db_sync(Module:What)).


                 /*******************************
                 *             CLOSE            *
                 *******************************/

close_dbs :-
    forall(retract(db_stream(_Module, Stream)),
           close(Stream)).

:- at_halt(close_dbs).
