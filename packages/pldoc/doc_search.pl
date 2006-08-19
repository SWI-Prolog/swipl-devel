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
	    search_reply/4		% +Search, +Options, //
	  ]).
:- use_module(library('http/html_write')).
:- use_module(process).
:- use_module(html).
:- use_module(doc_index).

/** <module> Search form and reply

*/

%%	search_form//
%
%	Create a search button

search_form -->
	search_form('Search ').

search_form(Title) -->
	html(form(action('/search'),
		  div([ Title,
			input([ name(for),
				size(30)
			      ], [])
		      ]))).

%%	search_reply(+Search, +Options) is det.
%
%	Generate a reply searching for Search

search_reply(For, Options) -->
	{ search_doc(For, PerFile),
	  PerFile \== [],
	  option(resultFormat(Format), Options, summary)
	}, !,
	html([ \doc_links('', Options),
	       h1(class(search),
		  ['Search results for ', span(class(for), For)])
	     | \matches(Format, PerFile, Options)
	     ]).
search_reply(_For, _Options) -->
	html([ \new_search,
	       h1(class(search), 'No matches')
	     ]).

%%	matches(+Format, +PerFile, +Options)// is det
%
%	Display search matches according to Format.
%
%	@param PerFile List of File-Objects

matches(long, PerFile, Options) -->
	long_matches(PerFile, Options).
matches(summary, PerFile, Options) -->
	html(table(class(summary), \short_matches(PerFile, Options))).
			 
	
long_matches([], _) -->
	[].
long_matches([File-Objs|T], Options) -->
	file_header(File, Options),
	objects(Objs, Options),
	long_matches(T, Options).

short_matches([], _) -->
	[].
short_matches([File-Objs|T], Options) -->
	file_index_header(File, Options),
	object_summaries(Objs, Options),
	short_matches(T, Options).


%%	search_doc(+SearchString, -PerFile:list) is det.
%
%	Return matches of SearchString as File-ListOfObjects, sorted
%	by File and by Object.

search_doc(Search, PerFile) :-
	findall(Tuple, matching_object(Search, Tuple), Tuples0),
	keysort(Tuples0, Tuples),
	group_by_key(Tuples, PerFile).


%%	group_by_key(+KeyedList, -KeyedGroups) is det.
%
%	Translate a sorted Key-Value list into a list Key-Values, where
%	Values all share the same key.  Values is sorted.

group_by_key([], []).
group_by_key([K-H|T0], [K-VL|T]) :-
	collect_by_key(K, T0, VL0, T1),
	sort([H|VL0], VL),
	group_by_key(T1, T).

collect_by_key(K, [K-V|T0], [V|VT], T) :- !,
	collect_by_key(K, T0, VT, T).
collect_by_key(_, L, [], L).


%%	matching_object(+SearchString, -Object) is nondet.
%
%	Object matches SearchString.
%	
%	@param Object	Term of the form File-Item

matching_object(Search, File-Obj) :-
	doc_comment(Obj, File:_Line, Summary, _Comment),
	(   apropos_match(Search, Summary)
	->  true
	;   sub_term(S, Obj),
	    atom(S),
	    apropos_match(Search, S)
	).
	
apropos_match(Needle, Haystack) :-
	'$apropos_match'(Needle, Haystack).
