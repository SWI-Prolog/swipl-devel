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

:- module(table_util,
	  [ sort_table/2,		% +Handle, +OutputFile
	    verify_table_order/1	% +Handle
	  ]).
:- use_module(library(table)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Utility library for the table package.  Predicates:

	verify_table_order(+Table)
		If `table' is a handle to a defined table and the table
		contains a key-fields, check that the fields in the table
		are really sorted according to the order defined in the
		table.  Errors are reported.

	sort_table(+Table, +FileName)
		Read the records from the given table, sort them according
		to the ordering information on the key field and write the
		result to the given filename.  Note this may require a lot
		of memory.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


%	sort_table(+Table, +File)
%
%	Read the given table, sort it using the associated ordering and
%	write the result back to File.

sort_table(Table, File) :-
	open(File, write, OutFd),	% fail early :-)
	get_table_attribute(Table, key_field, Key), !,
	get_table_attribute(Table, field(Key), Term),
	get_table_attribute(Table, file, InFile),
	functor(Term, KeyName, _),
	arg(2, Term, Attributes),
	format('Sorting table "~w" ', [InFile]),
	(   memberchk(sorted(Order), Attributes)
	->  true
	;   memberchk(sorted, Attributes),
	    Order = exact
	),
	format('sorted(~w) on field "~w" ... ', [Order, KeyName]),
	flush,
	read_table(Table, KeyName, Fields),
	sort_fields(Order, Fields, SortedFields),
	write_table(SortedFields, Table, OutFd),
	close(OutFd),
	format('done.~n', []).

read_table(Table, KeyName, Fields) :-
	format('(reading) ... ', []), flush,
	read_table(Table, KeyName, 0, Fields).

read_table(Table, KeyName, From, [KeyValue-From|T]) :-
	read_field(Table, From, To, KeyName, KeyValue), !,
	read_table(Table, KeyName, To, T).
read_table(_, _, _, []).
	
sort_fields(Order, Fields, Sorted) :-
	length(Fields, N),
	format('(sorting ~D records) ... ', [N]), flush,
	sort_keyed_strings(Order, Fields, Sorted).

write_table(Records, Table, OutFd) :-
	format('(writing) ... ', []), flush,
	get_table_attribute(Table, record_separator, Sep),
	write_records(Records, Table, Sep, OutFd).

write_records([], _, _, _).
write_records([_-From|T], Table, Sep, OutFd) :-
	read_table_record_data(Table, From, _To, RecordData),
	format(OutFd, '~s~c', [RecordData, Sep]),
	write_records(T, Table, Sep, OutFd).


%	sort_keyed_strings(+Table, +List, -Sorted)
%
%	Sort a list of KeyName-Index pairs on their KeyName using the
%	given ordering table.

sort_keyed_strings(Table, List, Sorted) :-
	length(List, Length), 
	do_sort(Length, Table, List, _, Result), 
	Sorted = Result.

do_sort(2, Table, [X1, X2|L], L, R) :- !, 
	X1 = K1-_,
	X2 = K2-_,
	compare_strings(Table, K1, K2, Cmp),
	merge2(Cmp, X1, X2, R).
do_sort(1, _, [X|L], L, [X]) :- !.
do_sort(0, _, L, L, []) :- !.
do_sort(N, Table, L1, L3, R) :-
	N1 is N // 2, 
	N2 is N - N1, 
	do_sort(N1, Table, L1, L2, R1), 
	do_sort(N2, Table, L2, L3, R2), 
	do_merge(R1, R2, Table, R).

do_merge([], R, _, R) :- !.
do_merge(R, [], _, R) :- !.
do_merge(R1, R2, Table, [X|R]) :-
	R1 = [X1|R1a], 
	R2 = [X2|R2a], 
	X1 = K1-_,
	X2 = K2-_,
	(   compare_strings(Table, K1, K2, >)
	->  X = X2, do_merge(R1, R2a, Table, R)
	;   X = X1, do_merge(R1a, R2, Table, R)
	).

merge2(>, A, B, [B, A]) :- !.
merge2(_, A, B,	[A, B]).


		 /*******************************
		 *	       VERIFY		*
		 *******************************/

%	verify_table_order)(+Table)
%
%	Verify a sorted table is really sorted according to its documentation.

verify_table_order(Table) :-
	get_table_attribute(Table, key_field, Key), !,
	get_table_attribute(Table, field(Key), Term),
	get_table_attribute(Table, file, File),
	functor(Term, KeyName, _),
	arg(2, Term, Attributes),
	format('Checking "~w" ', [File]),
	(   memberchk(sorted(Order), Attributes)
	->  true
	;   memberchk(sorted, Attributes),
	    Order = exact
	),
	(   memberchk(unique, Attributes)
	->  Cmp = >,
	    format('uniquely ', [])
	;   Cmp = [>, =]
	),
	format('sorted(~w) on field "~w" ... ', [Order, KeyName]),
	flush,
	read_field(Table, 0, To, KeyName, KeyValue),
	verify_table(Table, To, KeyName, KeyValue, Order, Cmp),
	format('done.~n', []).

verify_table(Table, From, KeyName, PrevValue, Order, Cmp) :-
	read_field(Table, From, To, KeyName, KeyValue), !,
	(   compare_strings(Order, KeyValue, PrevValue, Rval),
	    ok_cmp(Rval, Cmp)
	->  verify_table(Table, To, KeyName, KeyValue, Order, Cmp)
	;   format('~N!! Order conflict: ~w < ~w~n', [KeyValue, PrevValue]),
	    verify_table(Table, To, KeyName, KeyValue, Order, Cmp)
	).
verify_table(_, _, _, _, _, _).

ok_cmp(Cmp, Cmp) :- !.
ok_cmp(Cmp, List) :-
	memberchk(Cmp, List).

read_field(Table, From, To, Field, Value) :-
	functor(Term, Field, 1),
	read_table_fields(Table, From, To,  [Term]),
	arg(1, Term, Value).
