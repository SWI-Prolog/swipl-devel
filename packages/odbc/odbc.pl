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
	  [ odbc_connect/3,		% +DSN, -Con, +Options
	    odbc_disconnect/1,		% +Con
	    odbc_current_connection/2,	% ?Con, -DSN
	    odbc_set_connection/2,	% +Connection, +Option
	    odbc_end_transaction/2,	% +Connection, +CommitRollback

	    odbc_query/4,		% +Connection, +SQL, -Row, +Options
	    odbc_query/3,		% +Connection, +SQL, -Row
	    odbc_query/2,		% +Connection, +SQL

	    odbc_prepare/4,		% +DSN, +SQL, +Parms, -Qid
	    odbc_prepare/5,		% +DSN, +SQL, +Parms, -Qid, +Options
	    odbc_execute/2,		% +Qid, +Parms
	    odbc_execute/3,		% +Qid, +Parms, -Row
	    odbc_clone_statement/2,	% +Statement, -Clone
	    odbc_free_statement/1,	% +Statement

					% DB Schema info
	    odbc_current_table/2,	% +DSN, -Table
	    odbc_current_table/3,	% +DSN, -Table, ?Facet
	    odbc_table_column/3,	% +DSN, ?Table, ?Column
	    odbc_table_column/4,		% +DSN, ?Table, ?Column, ?Facet

	    odbc_statistics/1		% -Value

	  ]).

:- initialization
   load_foreign_library(odbc).

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

odbc_current_table(DSN, Table) :-
	odbc_database(DSN, row(_Qualifier, _Owner, Table, 'TABLE', _Comment)).

odbc_current_table(DSN, Table, Facet) :-
	odbc_database(DSN, Tuple),
	arg(3, Tuple, Table),
	table_facet(Facet, DSN, Tuple).

table_facet(qualifier(Qualifier), _, Tuple) :- arg(1, Tuple, Qualifier).
table_facet(owner(Owner), _, Tuple) :-         arg(2, Tuple, Owner).
table_facet(type(Type), _, Tuple) :-           arg(4, Tuple, Type).
table_facet(comment(Comment), _, Tuple) :-     arg(5, Tuple, Comment).
table_facet(arity(Arity), DSN, Tuple) :-
	arg(3, Tuple, Table),
	findall(C, odbc_table_column(DSN, Table, C), Cs),
	length(Cs, Arity).

%	odbc_table_column(+DSN, +Table, -Column)

odbc_table_column(DSN, Table, Column) :-
	(   var(Table)
	->  odbc_current_table(DSN, Table)
	;   true
	),
	odbc_column(DSN, Table, Tuple),
	arg(4, Tuple, Column).

%	odbc_table_column(+DSN, +Table, -Column, -Facet)

odbc_table_column(DSN, Table, Column, Facet) :-
	(   var(Table)
	->  odbc_current_table(DSN, Table)
	;   true
	),
	odbc_column(DSN, Table, Tuple),
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
