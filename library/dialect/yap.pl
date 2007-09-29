/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2007, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(yap,
	  [ 
	  ]).

/** <module> YAP Compatibility module

This module provides compatibility to YAP.  It must:

	* Implement system predicates available in YAP we do not yet or
	do not wish to support in SWI-Prolog.  Export these predicates.

	* Provide yap_<name>(...) predicates for predicates that exist
	both in YAP and SWI-Prolog and define goal_expansion/2 rules to
	map calls to these predicates to the yap_<name> version.
	Export these predicates.

	* Alter the library search path, placing dialect/yap *before*
	the system libraries.

@tbd	Fill it in!
@author Jan Wielemaker
*/

		 /*******************************
		 *	     EXPANSION		*
		 *******************************/

:- multifile
	user:goal_expansion/2,
	user:file_search_path/2,
	yap_expansion/2.
:- dynamic
	user:goal_expansion/2,
	user:file_search_path/2.

user:goal_expansion(In, Out) :-
	prolog_load_context(dialect, yap),
	yap_expansion(In, Out).

%%	yap_expansion(+In, +Out)
%
%	goal_expansion rules to emulate YAP behaviour in SWI-Prolog.


		 /*******************************
		 *	    LIBRARY SETUP	*
		 *******************************/

%%	push_yap_library
%
%	Pushes searching for  dialect/yap  in   front  of  every library
%	directory that contains such as sub-directory.

push_yap_library :-
	(   absolute_file_name(library(dialect/yap), Dir,
			       [ file_type(directory),
				 access(read),
				 solutions(all),
				 file_errors(fail)
			       ]),
	    asserta((user:file_search_path(library, Dir) :-
		    prolog_load_context(dialect, yap))),
	    fail
	;   true
	).

:- push_yap_library.
