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


:- module(odbc,
	  [ odbc_connect/3,		% +DSN, -Conn, +Options
	    odbc_disconnect/1,		% +Conn
	    odbc_current_connection/2,	% ?Conn, -DSN
	    odbc_set_connection/2,	% +Conn, +Option
	    odbc_end_transaction/2,	% +Conn, +CommitRollback

	    odbc_query/4,		% +Conn, +SQL, -Row, +Options
	    odbc_query/3,		% +Conn, +SQL, -Row
	    odbc_query/2,		% +Conn, +SQL

	    odbc_prepare/4,		% +Conn, +SQL, +Parms, -Qid
	    odbc_prepare/5,		% +Conn, +SQL, +Parms, -Qid, +Options
	    odbc_execute/2,		% +Qid, +Parms
	    odbc_execute/3,		% +Qid, +Parms, -Row
	    odbc_clone_statement/2,	% +Statement, -Clone
	    odbc_free_statement/1,	% +Statement

					% DB Schema info
	    odbc_current_table/2,	% +Conn, -Table
	    odbc_current_table/3,	% +Conn, -Table, ?Facet
	    odbc_table_column/3,	% +Conn, ?Table, ?Column
	    odbc_table_column/4,	% +Conn, ?Table, ?Column, ?Facet
	    odbc_type/3,		% +Conn, ?Type, -Facet

	    odbc_statistics/1		% -Value

	  ]).

:- initialization
   load_foreign_library(odbc4pl).

%	odbc_query(+Connection, +SQL, -Row)
%	
%	Run query without options.

odbc_query(Connection, SQL, Row) :-
	odbc_query(Connection, SQL, Row, []).

%	odbc_query(+Connection, +SQL)
%	
%	Execute SQL-statement that does not produce a result

odbc_query(Connection, SQL) :-
	odbc_query(Connection, SQL, Row), !,
	(   var(Row)
	->  true
	;   print_message(warning, odbc(unexpected_result(Row)))
	).

odbc_execute(Statement, Parameters) :-
	odbc_execute(Statement, Parameters, Row), !,
	(   var(Row)
	->  true
	;   print_message(warning, odbc(unexpected_result(Row)))
	).

odbc_prepare(Connection, SQL, Parameters, Statement) :-
	odbc_prepare(Connection, SQL, Parameters, Statement, []).

		 /*******************************
		 *	    SCHEMA STUFF	*
		 *******************************/

%	odbc_current_table(-Table, -Facet)
%	
%	Enumerate the existing tables.

odbc_current_table(Connection, Table) :-
	odbc_tables(Connection, row(_Qualifier, _Owner, Table, 'TABLE', _Comment)).

odbc_current_table(Connection, Table, Facet) :-
	odbc_tables(Connection, Tuple),
	arg(3, Tuple, Table),
	table_facet(Facet, Connection, Tuple).

table_facet(qualifier(Qualifier), _, Tuple) :- arg(1, Tuple, Qualifier).
table_facet(owner(Owner), _, Tuple) :-         arg(2, Tuple, Owner).
table_facet(type(Type), _, Tuple) :-           arg(4, Tuple, Type).
table_facet(comment(Comment), _, Tuple) :-     arg(5, Tuple, Comment).
table_facet(arity(Arity), Connection, Tuple) :-
	arg(3, Tuple, Table),
	findall(C, odbc_table_column(Connection, Table, C), Cs),
	length(Cs, Arity).

%	odbc_table_column(+Connection, +Table, -Column)

odbc_table_column(Connection, Table, Column) :-
	(   var(Table)
	->  odbc_current_table(Connection, Table)
	;   true
	),
	odbc_column(Connection, Table, Tuple),
	arg(4, Tuple, Column).

%	odbc_table_column(+Connection, +Table, -Column, -Facet)

odbc_table_column(Connection, Table, Column, Facet) :-
	(   var(Table)
	->  odbc_current_table(Connection, Table)
	;   true
	),
	odbc_column(Connection, Table, Tuple),
	arg(4, Tuple, Column),
	column_facet(Facet, Tuple).

column_facet(table_qualifier(Q), T) :- arg(1, T, Q).
column_facet(table_owner(Q), T)	    :- arg(2, T, Q).
column_facet(table_name(Q), T)	    :- arg(3, T, Q).
%column_facet(column_name(Q), T)    :- arg(4, T, Q).
column_facet(data_type(Q), T)	    :- arg(5, T, Q).
column_facet(type_name(Q), T)	    :- arg(6, T, Q).
column_facet(precision(Q), T)	    :- arg(7, T, Q).
column_facet(length(Q), T)	    :- arg(8, T, Q).
column_facet(scale(Q), T)	    :- arg(9, T, Q).
column_facet(radix(Q), T)	    :- arg(10, T, Q).
column_facet(nullable(Q), T)	    :- arg(11, T, Q).
column_facet(remarks(Q), T)	    :- arg(12, T, Q).
column_facet(type(Type), T) :-
	arg(6, T, TypeName),
	sql_type(TypeName, T, Type).

sql_type(dec, T, Type) :- !,
	sql_type(decimal, T, Type).
sql_type(numeric, T, Type) :- !,
	sql_type(decimal, T, Type).
sql_type(decimal, T, Type) :- !,
	column_facet(precision(Len), T),
	(   column_facet(scale(D), T),
	    D \== 0
	->  Type = decimal(Len, D)
	;   Type = decimal(Len)
	).
sql_type(char, T, char(Len)) :- !,
	column_facet(length(Len), T).
sql_type(varchar, T, varchar(Len)) :- !,
	column_facet(length(Len), T).
sql_type(TypeName, _T, Type) :-
	downcase_atom(TypeName, Type).

%	odbc_type(+Connection, +TypeSpec, ?Facet).

odbc_type(Connection, TypeSpec, Facet) :-
	odbc_types(Connection, TypeSpec, Row),
	type_facet(Facet, Row).

type_facet(name(V), Row)	   :- arg(1, Row, V).
type_facet(data_type(V), Row)	   :- arg(2, Row, V).
type_facet(precision(V), Row)	   :- arg(3, Row, V).
type_facet(literal_prefix(V), Row) :- non_null_arg(4, Row, V).
type_facet(literal_suffix(V), Row) :- non_null_arg(5, Row, V).
type_facet(create_params(V), Row)  :- non_null_arg(6, Row, V).
type_facet(nullable(V), Row)	   :- arg(7, Row, I), nullable_arg(I, V).
type_facet(case_sensitive(V), Row) :- bool_arg(8, Row, V).
type_facet(searchable(V), Row)	   :- arg(9, Row, I), searchable_arg(I, V).
type_facet(unsigned(V), Row)	   :- bool_arg(10, Row, V).
type_facet(money(V), Row)	   :- bool_arg(11, Row, V).
type_facet(auto_increment(V), Row) :- bool_arg(12, Row, V).
type_facet(local_name(V), Row)	   :- non_null_arg(13, Row, V).
type_facet(minimum_scale(V), Row)  :- non_null_arg(14, Row, V).
type_facet(maximum_scale(V), Row)  :- non_null_arg(15, Row, V).

non_null_arg(Index, Row, V) :-
	arg(Index, Row, V),
	V \== '$null$'.
bool_arg(Index, Row, V) :-
	arg(Index, Row, I),
	int_to_bool(I, V).

int_to_bool(0, false).
int_to_bool(1, true).

nullable_arg(0, false).
nullable_arg(1, true).
nullable_arg(2, unknown).

searchable_arg(0, false).
searchable_arg(1, like_only).
searchable_arg(2, all_except_like).
searchable_arg(4, true).



		 /*******************************
		 *	     STATISTICS		*
		 *******************************/

odbc_statistics(Key) :-
	statistics_key(Key),
	'$odbc_statistics'(Key).
	
statistics_key(statements(_Created, _Freed)).


		 /*******************************
		 *	      MESSAGES		*
		 *******************************/

:- multifile
	prolog:message/3.

prolog:message(error(odbc(ODBCCode, _NativeCode, Comment), _)) -->
	[ 'ODBC: State ~w: ~w'-[ODBCCode, Comment] ].
prolog:message(error(context_error(Obj, Error, What), _)) -->
	[ 'Context error: ~w ~w: '-[What, Obj] ],
	context(Error).

context(in_use) -->
	[ 'object is in use' ].
