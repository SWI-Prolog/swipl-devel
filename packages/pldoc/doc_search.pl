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
	  [ search_form/3,		% +Options, //
	    search_reply/4		% +Search, +Options, //
	  ]).
:- use_module(library('http/html_write')).
:- use_module(library(occurs)).
:- use_module(library(option)).
:- use_module(doc_process).
:- use_module(doc_html).
:- use_module(doc_index).
:- include(hooks).

/** <module> Search form and reply

@tbd	Advanced search field

		* Limit to a directory
		* Whole-word search
		* *and*, *or*, *not*, etc advanced syntax
		* Specified fields: =name=, =summary=, =description=
*/

%%	search_form(+Options)//
%
%	Create  a  search  input  field.  The   input  field  points  to
%	=|/search?for=String|= on the current server.  Options:
%	
%		* title(Title)

search_form(Options) -->
	{ (   option(for(Value), Options)
	  ->  Extra = [value(Value)]
	  ;   Extra = []
	  ),
	  option(search_in(In), Options, all)
	},
	html(form(action('/search'),
		  [ div([ input([ name(for),
				  size(30)
				| Extra
				], []),
			  input([ type(submit),
				  value('Search')
				])
			]),
		    div(class('search-options'),
			[ \radio(in, all, 'All', In),
			  \radio(in, app, 'Application', In),
			  \radio(in, man, 'Manual', In)
			])
		  ])).

radio(Radio, Field, Label, In) -->
	{   Field == In
	->  Extra = [checked]
	;   Extra = []
	},
	html(input([ type(radio),
		     name(Radio),
		     value(Field)
		   | Extra
		   ],
		   [ Label
		   ])).

%%	search_reply(+For, +Options)// is det.
%
%	Generate a reply searching for For.  Options include
%	
%		* resultFormat(Format)
%		If =summary= (default), produce a summary-table.  If
%		=long=, produce full object descriptions.
%		
%		* search_in(In)
%		Determine which databases to search.  One of
%		=all=, =app=, =man=

search_reply(For, Options) -->
	{ search_doc(For, PerCategory, Options),
	  PerCategory \== [],
	  option(resultFormat(Format), Options, summary),
	  count_matches(PerCategory, Matches)
	}, !,
	html([ \doc_links('', [for(For)|Options]),
	       div(class('search-results'),
		   ['Search results for ', span(class(for), ['"', For, '"'])]),
	       div(class('search-counts'),
		   [ Matches, ' matches; ',
		     \count_by_category(PerCategory)
		   ])
	     | \matches(Format, PerCategory, Options)
	     ]).
search_reply(For, Options) -->
	html([ \doc_links('', [for(For)|Options]),
	       h1(class(search), 'No matches')
	     ]).

count_by_category([]) -->
	[].
count_by_category([Cat-PerFile|T]) -->
	{ count_category(PerFile, Count),
	  atom_concat(#, Cat, HREF)
	},
	html([ a(href(HREF), \category_title(Cat)),
	       ': ',
	       Count
	     ]),
	(   {T == []}
	->  []
	;   html(', '),
	    count_by_category(T)
	).

count_matches([], 0).
count_matches([_-Cat|T], Count) :-
	count_matches(T, Count0),
	count_category(Cat, N),
	Count is Count0 + N.

count_category([], 0).
count_category([_-Objs|T], Count) :-
	count_category(T, Count0),
	length(Objs, N),
	Count is Count0 + N.
		 
%%	matches(+Format, +PerCategory, +Options)// is det
%
%	Display search matches according to Format.
%
%	@param PerCategory List of File-Objects

matches(long, PerCategory, Options) -->
	long_matches_by_type(PerCategory, Options).
matches(summary, PerCategory, Options) -->
	html(table(class(summary),
		   \short_matches_by_type(PerCategory, Options))).
			 
	
long_matches_by_type([], _) -->
	[].
long_matches_by_type([Category-PerFile|T], Options) -->
	category_header(Category, Options),
	long_matches(PerFile, Options),
	long_matches_by_type(T, Options).


long_matches([], _) -->
	[].
long_matches([File-Objs|T], Options) -->
	file_header(File, Options),
	objects(Objs, Options),
	long_matches(T, Options).

category_header(Category, _Options) -->
	html(h1(class(category), \category_title(Category))).

short_matches_by_type([], _) -->
	[].
short_matches_by_type([Category-PerFile|T], Options) -->
	category_index_header(Category, Options),
	short_matches(PerFile, Options),
	short_matches_by_type(T, Options).

short_matches([], _) -->
	[].
short_matches([File-Objs|T], Options) -->
	file_index_header(File, Options),
	object_summaries(Objs, File, Options),
	short_matches(T, Options).


category_index_header(Category, _Options) -->
	html(tr(th([class(category), colspan(2)],
		   a(name(Category), \category_title(Category))))).

category_title(Category) -->
	{   prolog:doc_category(Category, _Order, Title)
	->  true
	;   Title = Category
	},
	html(Title).

%%	search_doc(+SearchString, -PerType:list, +Options) is det.
%
%	Return matches of SearchString  as   Type-PerFile  tuples, where
%	PerFile is a list Fule-ListOfObjects.

search_doc(Search, PerType, Options) :-
	findall(Tuples, matching_object(Search, Tuples, Options), Tuples0),
	keysort(Tuples0, Tuples),
	group_by_key(Tuples, PerCat0),
	key_sort_order(PerCat0, PerCat1),
	keysort(PerCat1, PerCat2),
	unkey(PerCat2, PerCat),
	group_by_file(PerCat, PerType).

key_sort_order([], []).
key_sort_order([Cat-ByCat|T0], [Order-(Cat-ByCat)|T]) :-
	(   prolog:doc_category(Cat, Order, _Title)
	->  true
	;   Order = 99
	),
	key_sort_order(T0, T).


%%	unkey(+Keyed, -Values) is det.
%
%	Remove keys added for keysort.

unkey([], []).
unkey([_-V|T0], [V|T]) :-
	unkey(T0, T).


group_by_file([], []).
group_by_file([Type-Tuples0|T0], [Type-ByFile|T]) :-
	keysort(Tuples0, Tuples),
	group_by_key(Tuples, ByFile),
	group_by_file(T0, T).


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


%%	matching_object(+SearchString, -Object, +Options) is nondet.
%
%	Object matches SearchString.  Options include
%	
%		* search_in(In)
%		One of =all=, =app=, =man=.
%	
%	@param Object	Term of the form File-Item
%	@tbd Deal with search syntax

matching_object(Search, Type-(Section-Obj), Options) :-
	option(search_in(In), Options, all),
	prolog:doc_object_summary(Obj, Type, Section, Summary),
	matching_category(In, Type),
	(   apropos_match(Search, Summary)
	->  true
	;   sub_term(S, Obj),
	    atom(S),
	    apropos_match(Search, S)
	).
	
matching_category(all, _).
matching_category(app, application).
matching_category(man, manual).

%%	object_summary(?Object, ?Category, ?Section, ?Summary) is nondet.
%
%	True  if  Object  is  summarised   by  Summary.  This  multifile
%	predicate can be extended  with   other  search  mechanisms. The
%	returned objects must be  handled   by  object_summaries//2  and
%	objects//2.
%	
%	@param Category	Atom describing the source.
%	@param Section  Reference to the context of Object.

prolog:doc_object_summary(Obj, Category, File, Summary) :-
	current_prolog_flag(home, SWI),
	doc_comment(Obj, File:_Line, Summary, _Comment),
	Obj \= _:module(_Title),		% HACK.  See ref_object//1
	(   sub_atom(File, 0, _, _, SWI)
	->  Category = library
	;   Category = application
	).
	

%%	doc_category(Name, SortOrder, Description) is nondet.
%
%	Describe the various  categories  of   search  results.  Used to
%	create the category headers  as  well   as  the  advanced search
%	dialog.
%	
%	@param SortOrder	Ranges 0..100.  Lower values come first 

prolog:doc_category(application, 20, 'Application').
prolog:doc_category(library,     80, 'System Libraries').


		 /*******************************
		 *	       UTIL		*
		 *******************************/

%%	apropos_match(+Needle, +Haystick) is semidet.
%
%	True if Needle can be found   as a case-insensitive substring in
%	Haystick.
%	
%	@tbd	Use public predicates for that.

apropos_match(Needle, Haystack) :-
	'$apropos_match'(Needle, Haystack).
