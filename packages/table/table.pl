/*  $Id$

    Designed and implemented by Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1996 University of Amsterdam. All rights reserved.
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
