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

open_db :-
	odbc_connect('test', _,
		     [ alias(test),
		       user(jan),
		       password(geheim),
		       open(once)
		     ]).

test :-
	forall(type(Type, _, _),
	       test_type(Type)).


		 /*******************************
		 *	     TYPE TESTS		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Test the type-conversion system. For each type   we create a small table
and read it back. If the type   can  be accessed with alternative Prolog
types we test this too.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%	type(SqlType, PlType=Values, [AltType=Map, ...])
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
     ]).
type(double,
     float   = [-1.0, 0.0, 42.0, 3.2747, '$null$' ],
     [ 
     ]).
type(decimal(10,2),
     atom = ['3.43', '4.50', '5.00', '$null$'],
     [
     ]).
type(numeric(10),
     integer = [-1, 0, 42, '$null$'],
     [
     ]).
type(varchar(20),
     atom = [ 'foo',
	      '',
	      'this is a long text',
	      '$null$'
	    ],
     [ codes   = sql_atom_codes,
       string  = atom_to_string
     ]).
type(varchar(10),				% can we access as integers?
     atom = [ '1', '2', '$null$'
	    ],
     [ integer = atom_to_integer
     ]).
type(varchar(100),
     atom = [ foo,
	      '',
	      'This is a nice long string consisting of enough text',
	      '$null$'
	    ],
     [ codes   = sql_atom_codes
     ]).
type(binary(20),
     atom = [ foo,
	      '',
	      'With a \0\ character'
	    ],
     [
     ]).
type(date,
     date = [ date(1960,3,19), '$null$' ],
     [
     ]).
type(time,
     time = [ time(18,23,19), '$null$' ],
     [
     ]).
type(timestamp,				% MySQL uses POSIX stamps
     timestamp = [ timestamp(1990,5,18,18,23,19,0), '$null$' ],
     [ integer = timestamp_to_integer
     ]).

test_type(Type) :-
	open_db,
	type(Type, PlType=Values, AltAccess),
	Type =.. [ODBCName|_Args],
	(   odbc_type(test, ODBCName, name(_DbName))
	->  progress('Type ~w:', [Type]),
	    catch(odbc_query(test, 'drop table test'), _, true),
	    odbc_query(test,
		       'create table ~w (testval ~w)'-[test, Type]),
	    progress(' (w)', []),
	    insert_values(test, Type, PlType, Values),
	    progress(' (r)', []),
	    read_values(test, PlType, ReadValues),
	    compare_sets(Values, ReadValues),
	    read_test_alt_types(AltAccess, test, Values),
	    write_test_alt_types(AltAccess, Type, test),
	    progress(' (OK!)~n', [])
	;   progress('Skipped ~w: not supported~n', [Type])
	).

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
	type(SqlType, PlType=NativeValues, _),
	odbc_query(test, 'delete from ~w'-[Table]),
	maplist(Map, NativeValues, Values),
	insert_values(test, SqlType, Type, Values),
	read_values(test, PlType, ReadValues),
	compare_sets(NativeValues, ReadValues).

%	insert_values(+Table, +SqlType, +Values)
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
	odbc_query(test, 'drop table if exists marks'),
	odbc_query(test, 'create table marks (name char(25), mark integer)'),
	odbc_prepare(test,
		     'insert into marks (name,mark) values (?,?)',
		     [ char(25),
		       integer
		     ],
		     Statement),
	forall(mark(Name, Mark),
	       odbc_execute(Statement, [Name,Mark])),
	odbc_free_statement(Statement),
	odbc_prepare(test, 'select * from marks', [], SelectStatement),
	assert(statement(select_mark, SelectStatement)).
	
db_mark(Name, Mark) :-
	statement(select_mark, Stmt),
	odbc_execute(Stmt, [], row(Name, Mark)).

same_mark(Name1, Name2) :-
	db_mark(Name1, Mark1),
	db_mark(Name2, Mark2),
	Mark1 == Mark2.

		 /*******************************
		 *	     FEEDBACK		*
		 *******************************/

progress(Fmt, Args) :-
	format(Fmt, Args),
	flush_output.



