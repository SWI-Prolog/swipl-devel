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

:- module(table,
	  [ new_table/4,		% from table.c
	    open_table/1,
	    close_table/1,
	    free_table/1,
	    table_window/3,
	    read_table_record/4,
	    read_table_record_data/4,
	    read_table_fields/4,
	    get_table_attribute/3,
	    table_previous_record/3,
	    table_start_of_record/4,
	    in_table/3,
	    new_order_table/2,		% from order.c
	    order_table_mapping/3,
	    compare_strings/4,
	    prefix_string/3,
	    prefix_string/4,
	    sub_string/3
	  ]).

:- initialization load_foreign_library(foreign(table)).
