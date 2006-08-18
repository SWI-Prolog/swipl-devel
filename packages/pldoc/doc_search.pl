/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2006, University of Amsterdam

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

:- module(pldoc_search,
	  [ search_form/2,		% //
	    search_reply/3		% +Search, //
	  ]).
:- use_module(library('http/html_write')).
:- use_module(process).
:- use_module(html).

/** <module> Search form and reply

@tbd	* Summary results
	* Results per file
*/

%%	search_form//
%
%	Create a search button

search_form -->
	search_form('Search').

search_form(Title) -->
	html(form(action('/search'),
		  div([ Title,
			input([ name(for),
				size(30)
			      ], [])
		      ]))).

%%	search_reply(+Search) is det.
%
%	Generate a reply searching for Search

search_reply(For) -->
	{ findall(Obj, matching_object(For, Obj), Objs),
	  Objs \== []
	}, !,
	html([ \new_search,
	       h1(class(search), 'Search results')
	     | \objects(Objs, [])
	     ]).
search_reply(_For) -->
	html([ \new_search,
	       h1(class(search), 'No matches')
	     ]).

new_search -->
	html(div(class(navhdr),
		 [ div(style('float:right'),
		       [ \search_form('New search ')
		       ])
		 ])).

matching_object(Search, Obj) :-
	doc_comment(Obj, _Pos, Summary, _Comment),
	(   apropos_match(Search, Summary)
	->  true
	;   sub_term(S, Obj),
	    atom(S),
	    apropos_match(Search, S)
	).
	
apropos_match(Needle, Haystack) :-
	'$apropos_match'(Needle, Haystack).
