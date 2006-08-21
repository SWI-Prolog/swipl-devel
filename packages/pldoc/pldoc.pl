/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2006, University of Amsterdam

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

:- module(pldoc,
	  [ doc_collect/1,		% +Bool

	    pldoc_loading/0		% True if we are loading
	  ]).
:- dynamic
	pldoc_loading/0.

pldoc_loading.

:- dynamic   user:file_search_path/2.
:- multifile user:file_search_path/2.

user:file_search_path(pldoc, library(pldoc)).

doc_collect(OnOff) :-
	set_prolog_flag(pldoc_collecting, OnOff).

:- doc_collect(true).

:- load_files([ pldoc(doc_process),
		pldoc(doc_register),
		pldoc(doc_modes),
		pldoc(doc_wiki),
		library(debug),
		library(option),
		library(lists),
		library(operators),
		library(prolog_source)
	      ],
	      [ silent(true),
		if(not_loaded)
	      ]).

		 /*******************************
		 *	  DOCUMENTATION		*
		 *******************************/

/** <module> Process source documentation

The pldoc module processes structured comments   in Prolog source files.
These  comments  can  be  saved   to    file.   During  development  the
documentation system can start a web-server to view the documentation of
loaded sources through your browser. The server   is defined in the file
doc_http.pl and started through doc_server/1.

During  development,  a  typical  scenario  is    to   first  start  the
documentation server and start  a   browser  at <http://localhost:4000>.
Note that by default the web-pages allow  for starting an editor only if
the connection comes from =localhost=.  See   doc_server/2  to realise a
different setup.

==
:- doc_server(4000).
:- [application].
==

@author  Jan Wielemaker
@license LGPL
@see	 doc_server/1, doc_server/2, doc_collect/1.
*/

%%	doc_collect(+Bool) is det.
%
%	Switch collecting comments true/false.   This autoload predicate
%	can be used to force loading  the   pldoc  library. In a typical
%	development setup loading pldoc  is   normally  triggered  using
%	doc_server/1.

%%	pldoc_loading is semidet.
%
%	True if we are loading the  PlDoc libraries. Required internally
%	to avoid undefined predicates  while   re-loading  and  document
%	itself.


		 /*******************************
		 *	     FINISH UP		*
		 *******************************/

:- retract(pldoc_loading),
   process_stored_comments.
