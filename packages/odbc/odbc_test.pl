/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
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

					% use the local copy
:- asserta(user:file_search_path(foreign, '.')).

:- use_module(odbc).

:- dynamic
	parms/2,
	passwd/1.

parms(test, [ user(jan),
	      password(Pass)
	    ]) :-
	(   passwd(Pass)
	->  true
	;   getpass(Pass),			% my private library
	    assert(passwd(Pass))
	).
	

set_db(DSN, Options) :-
	retractall(parms(_,_)),
	assert(parms(DSN, Options)).

open_db :-
	parms(DSN, Options),
	odbc_connect(DSN, _,
		     [ alias(test),
		       open(once)
		     | Options
		     ]).

test :-
	forall(type(Type, _, _, _),
	       test_type(Type)).


		 /*******************************
		 *	     TYPE TESTS		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Test the type-conversion system. For each type   we create a small table
and read it back. If the type   can  be accessed with alternative Prolog
types we test this too.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%	type(SqlType, PlType=Values, [AltType=Map, ...], Options)
%	
%	Define a test-set.  The first argument is the SQL type to test.
%	PlType is the default Prolog type with a set of values.  AltType
%	is an alternative type that can be used to exchange values and
%	Map is a maplist/3 argument to convert the value-list into a
%	list of values of the alternative type.

type(integer,
     integer = [-1, 0, 42, '$null$' ],
     [ atom  = integer_to_atom,		% exchange as text
       float = integer_to_float		% exchange as float
     ],
     []).
type(float,
     float   = [-1.0, 0.0, 42.0, 3.2747, '$null$' ],
     [ 
     ],
     []).
type(decimal(10,2),
     atom = ['3.43', '4.50', '5.00', '$null$'],
     [
     ],
     []).
type(decimal(6,2),			% truncating test
     atom = ['1000.00'],
     [
     ],
     []).
type(decimal(14,2),			% truncating test
     atom = ['17.45'],
     [
     ],
     []).
type(numeric(10),
     integer = [-1, 0, 42, '$null$'],
     [
     ],
     []).
type(varchar(20),
     atom = [ 'foo',
	      '',
	      'this is a long text',
	      '$null$'
	    ],
     [ codes   = sql_atom_codes,
       string  = atom_to_string
     ],
     []).
type(varchar(10),				% can we access as integers?
     atom = [ '1', '2', '$null$'
	    ],
     [ integer = atom_to_integer
     ],
     []).
type(varchar(100),
     atom = [ foo,
	      '',
	      'This is a nice long string consisting of enough text',
	      '$null$'
	    ],
     [ codes   = sql_atom_codes
     ],
     []).
type(varchar(2000),				% can we access as integers?
     atom = [ 'This is a nice atom',
	      '',
	      Long,
	      '$null$'
	    ],
     [ 
     ],
     [ \+ dbms_name('MySQL')
     ]) :-
	findall(C, (between(1, 1500, X),
		    C is X mod 64 + "@"), LongCodes),
	atom_chars(Long, LongCodes).
type(varbinary(20),
     atom = [ foo,
	      '',
	      'With a \0\ character'
	    ],
     [
     ],
     []).
type(blob,			% mySql blob
     atom = [ foo,
	      '',
	      'With a \0\ character'
	    ],
     [
     ],
     [ odbc_type(longvarbinary),
       dbms_name('MySQL')		% MySQL specific test
     ]).
type(longblob,			% mySql blob
     atom = [ BIG
	    ],
     [
     ],
     [ odbc_type(longvarbinary),
       dbms_name('MySQL')		% MySQL specific test
     ]) :-
	read_file_to_codes('odbc.pdf', Codes, []),
	atom_codes(BIG, Codes).
type(date - 'Type date',
     date = [ date(1960,3,19) ],
     [
     ],
     [ \+ dbms_name('Microsoft SQL Server')
     ]).
type(date - 'Type date: NULL',
     date = [ '$null$' ],
     [
     ],
     [ \+ dbms_name('Microsoft SQL Server'),
       \+ dbms_name('MySQL')
     ]).
type(time - 'Type time',
     time = [ time(18,23,19) ],
     [
     ],
     [ \+ dbms_name('Microsoft SQL Server')
     ]).
type(time - 'Type time: NULL',
     time = [ '$null$' ],
     [
     ],
     [ \+ dbms_name('Microsoft SQL Server'),
       \+ dbms_name('MySQL')
     ]).
type(timestamp,				% MySQL uses POSIX stamps
     timestamp = [ timestamp(1990,5,18,18,23,19,0) ],
     [ integer = timestamp_to_integer
     ],
     [ dbms_name('MySQL')
     ]).

test_type(TypeSpec) :-
	open_db,
	type(TypeSpec, PlType=Values, AltAccess, Options),
	db_type(TypeSpec, Type, Comment),
	(   applicable(Options, Type)
	->  progress('~w:', [Comment]),
	    create_test_table(Type),
	    progress(' (w)', []),
	    (	memberchk(odbc_type(ODBCType), Options)
	    ->	true
	    ;	ODBCType = Type
	    ),
	    insert_values(test, ODBCType, PlType, Values),
	    progress(' (r)', []),
	    read_values(test, PlType, ReadValues),
	    compare_sets(Values, ReadValues),
	    read_test_alt_types(AltAccess, test, Values),
	    write_test_alt_types(AltAccess, Type, test),
	    progress(' (OK!)~n', [])
	;   true
	    % progress('Skipped ~w: not supported~n', [Comment])
	).

db_type(Type-Comment, Type, Comment).
db_type(Type,         Type, Comment) :-
	sformat(Comment, 'Type ~w', [Type]).

%	applicable(+Options, +Type)
%	
%	See whether we can run this test on this connection.

applicable([], _) :- !.
applicable([H|T], Type) :- !,
	applicable(H, Type),
	applicable(T, Type).
applicable(\+ Option, Type) :- !,
	\+ applicable(Option, Type).
applicable(dbms_name(DB), _) :- !,
	odbc_get_connection(test, dbms_name(DB)).
applicable(_, _).


%	read_test_alt_types([Type=Map, ...], Table, Values)
%	
%	Try to read the table using alternative Prolog types and check
%	the results.

read_test_alt_types([], _, _).
read_test_alt_types([Type=Map|T], Table, Values) :-
	read_test_alt_type(Type, Map, Table, Values),
	read_test_alt_types(T, Table, Values).

read_test_alt_type(Type, Map, Table, Values) :-
	progress(' r(~w)', Type),
	maplist(Map, Values, AltValues),
	read_values(Table, Type, ReadValues),
	compare_sets(AltValues, ReadValues).
	
%	write_test_alt_types([Type=Map, ...], Table, Values)
%

write_test_alt_types([], _, _).
write_test_alt_types([Type=Map|T], SqlType, Table) :-
	write_test_alt_type(Type, Map, SqlType, Table),
	write_test_alt_types(T, SqlType, Table).

write_test_alt_type(Type, Map, SqlType, Table) :-
	progress(' w(~w)', Type),
	type(SqlType, PlType=NativeValues, _, _),
	odbc_query(test, 'delete from ~w'-[Table]),
	maplist(Map, NativeValues, Values),
	insert_values(test, SqlType, Type, Values),
	read_values(test, PlType, ReadValues),
	compare_sets(NativeValues, ReadValues).

%	insert_values(+Table, +OdbcType, +PlType, +Values)
%	
%	Insert Prolog values into the table

insert_values(Table, SqlType, PlType, Values) :-
	open_db,
	odbc_prepare(test,
		     'insert into ~w (testval) values (?)'-[Table],
		     [ PlType>SqlType ],
		     Statement),
	forall(member(V, Values),
	       odbc_execute(Statement, [V])).

read_values(Table, PlType, Values) :-
	open_db,
	findall(Value,
		odbc_query(test,
			   'select (testval) from ~w'-[Table],
			   row(Value),
			   [ types([PlType])
			   ]),
		Values).

compare_sets(X, X) :- !.
compare_sets(X, Y) :-
	compare_elements(X, Y).

compare_elements([], []).
compare_elements([H|T0], [H|T1]) :- !,
	compare_elements(T0, T1).
compare_elements([H0|T0], [H1|T1]) :-
	(   float(H0),
	    float(H1)
	->  Diff is H0-H1,
	    format('~NERROR: ~q != ~q (diff=~f)~n', [H0, H1, Diff])
	;   format('~NERROR: ~q != ~q~n', [H0, H1])
	),
	compare_elements(T0, T1).


		 /*******************************
		 *	       MAPS		*
		 *******************************/

integer_to_atom('$null$', '$null$') :- !.
integer_to_atom(Int, Atom) :-
	(   number(Int)
	->  number_codes(Int, Codes),
	    atom_codes(Atom, Codes)
	;   atom_codes(Atom, Codes),
	    number_codes(Int, Codes)
	).

integer_to_float('$null$', '$null$') :- !.
integer_to_float(Int, Float) :-
	Float is float(Int).

atom_to_string('$null$', '$null$') :- !.
atom_to_string(Atom, String) :-
	string_to_atom(String, Atom).

atom_to_integer('$null$', '$null$') :- !.
atom_to_integer(Atom, Int) :-
	integer_to_atom(Int, Atom).

sql_atom_codes('$null$', '$null$') :- !.
sql_atom_codes(Atom, Codes) :-
	atom_codes(Atom, Codes).

timestamp_to_integer('$null$', '$null$') :- !.
timestamp_to_integer(timestamp(Y,M,D,H,Mn,S,0), Sec) :-
	get(date(S,Mn,H,D,M,Y), slot, date, Sec).

		 /*******************************
		 *	    SHOW SOURCE		*
		 *******************************/

list(Name) :-
	open_db,
	(   odbc_query(test, 'select * from ~w'-[Name], Row, [source(true)]),
	    writeln(Row),
	    fail
	;   true
	).

		 /*******************************
		 *	    CURSOR TEST		*
		 *******************************/

:- dynamic
	statement/2.

delete_statements :-
	forall(retract(statement(_, Statement)),
	       odbc_free_statement(Statement)).


mark(john,  6).
mark(bob,   7).
mark(chris, 5).
mark(mary,  7).

make_mark_table :-
	open_db,
	delete_statements,
	catch(odbc_query(test, 'drop table marks'), _, true),
	odbc_query(test, 'create table marks (name char(25), mark integer)'),
	odbc_prepare(test,
		     'insert into marks (name,mark) values (?,?)',
		     [ char(25),
		       integer
		     ],
		     Statement),
	forall(mark(Name, Mark),
	       odbc_execute(Statement, [Name,Mark])),
	odbc_free_statement(Statement).

db_mark(Name, Mark) :-
	(   statement(select_mark, Stmt)
	->  true
	;   odbc_prepare(test, 'select * from marks', [], Stmt),
	    assert(statement(select_mark, Stmt))
	),
	odbc_execute(Stmt, [], row(Name, Mark)).

same_mark(Name1, Name2) :-
	db_mark(Name1, Mark1),
	db_mark(Name2, Mark2),
	Mark1 == Mark2.

marks(L) :-
	open_db,
	odbc_query(test,
		   'select * from marks', L,
		   [findall(mark(X,Y), row(X,Y))]).

with_mark(Mark, L) :-			% doesn't work yet
	open_db,
	odbc_query(test,
		   'select * from marks', L,
		   [ findall(Name, row(Name, Mark))
		   ]).

tmark :-
	open_db,
	odbc_query(test, 'SELECT * from marks', row(X, 6)),
	write(X),
	fail.

prepfoo :-
	open_db,
	odbc_prepare(test,
		     'select name from foo where mark=?',
		     [default],
		     S,
		     []),
	writeln(S).


		 /*******************************
		 *	 GENERIC ACTIONS	*
		 *******************************/

create_test_table(Type) :-
	catch(odbc_query(test, 'drop table test'), _, true),
	odbc_query(test,
		   'create table ~w (testval ~w)'-[test, Type]).


		 /*******************************
		 *	   SPECIAL TESTS	*
		 *******************************/

test(decimal) :-
	create_test_table(decimal(14,2)),
	odbc_prepare(test,
		     'insert into test (testval) values (?)',
		     [ decimal(14,2) ],
		     Statement),
	odbc_execute(Statement, ['17.45'], affected(Affected)),
	progress('Affected ~w rows', [Affected]),
	odbc_query(test, 'select * from test', row('17.45')),
	progress(' OK!~n', []).


		 /*******************************
		 *	       FETCH		*
		 *******************************/

create_fetch_table :-
	open_db,
	create_test_table(integer),
	odbc_prepare(test,
		     'insert into test (testval) values (?)',
		     [ integer ],
		     Statement),
	forall(between(1, 100, X),
	       odbc_execute(Statement, [X])),
	odbc_free_statement(Statement).

fetch(Options) :-
	open_db,
	odbc_set_connection(test, cursor_type(static)),
	odbc_prepare(test,
		     'select (testval) from test',
		     [],
		     Statement,
		     [ fetch(fetch)
		     ]),
	odbc_execute(Statement, []),
	fetch(Statement, Options).

fetch(Statement, Options) :-
	odbc_fetch(Statement, Row, Options),
	(   Row == end_of_file
	->  true
	;   writeln(Row),
	    fetch(Statement, Options)
	).
	

		 /*******************************
		 *	       META		*
		 *******************************/

tcolumns :-
	open_db,
	make_mark_table,
	findall(X, odbc_table_column(test, marks, X), List),
	List = [name, mark].


		 /*******************************
		 *	     FEEDBACK		*
		 *******************************/

progress(Fmt, Args) :-
	format(Fmt, Args),
	flush_output.


		 /*******************************
		 *	       PORTRAY		*
		 *******************************/

user:portray(Long) :-
	atom(Long),
	atom_length(Long, Len),
	Len > 70,
	sub_atom(Long, 0, 20, _, Start),
	sub_atom(Long, _, 10, 0, End),
	format('\'~w...~w\'', [Start, End]).
