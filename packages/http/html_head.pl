/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2009, University of Amsterdam

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

:- module(html_head,
	  [ html_resource/2,		% +Resource, +Attributes
	    html_requires//1		% +Resource
	  ]).
:- use_module(library(http/html_write)).
:- use_module(library(http/mimetype)).
:- use_module(library(http/http_path)).
:- use_module(library(error)).
:- use_module(library(settings)).
:- use_module(library(lists)).
:- use_module(library(occurs)).
:- use_module(library(option)).
:- use_module(library(ordsets)).
:- use_module(library(assoc)).
:- use_module(library(ugraphs)).
:- use_module(library(broadcast)).
:- use_module(library(apply)).
:- use_module(library(debug)).


/** <module> Automatic inclusion of CSS and scripts links

This library allows for  abstract  declaration   of  available  CSS  and
Javascript resources and their dependencies using html_resource/2. Based
on these declarations, html generating code  can declare that it depends
on specific CSS or Javascript functionality,   after  which this library
ensures  that  the  proper  links   appear    in   the  HTML  head.  The
implementation is based on mail  system   implemented  by html_post/2 of
library html_write.pl.

Declarations come in two forms. First of all http locations are declared
using the http_path.pl library. Second,   html_resource/2 specifies HTML
resources to be used in the =head= and their dependencies. Resources are
currently limited to Javascript files (.js)  and style sheets (.css). It
is  trivial  to  add  support  for  other  material  in  the  head.  See
html_include//1.

For usage in HTML generation,  there   is  the DCG rule html_requires//1
that demands named resources  in  the   HTML  head.

---++ About resource ordering

All calls to html_requires//1 for the page are collected and duplicates
are removed.  Next, the following steps are taken:

    1. Add all dependencies to the set
    2. Replace multiple members by `aggregate' scripts or css files.
       see use_agregates/4.
    3. Order all resources by demanding that their dependencies
       preceede the resource itself.  Note that the ordering of
       resources in the dependency list is *ignored*.  This implies
       that if the order matters the dependency list must be split
       and only the primary dependency must be added.

---++ Debugging dependencies

Use ?- debug(html(script)). to  see  the   requested  and  final  set of
resources. All declared resources  are   in  html_resource/3. The edit/1
command recognises the names of HTML resources.

---++ Predicates

@tbd	Possibly we should add img//2 to include images from symbolic
	path notation.
@tbd	It would be nice if the HTTP file server could use our location
	declarations.
*/

:- dynamic
	html_resource/3.		% Resource, Source, Properties
:- multifile
	html_resource/3.

%%	html_resource(+About, +Properties) is det.
%
%	Register an HTML head resource.  About   is  either an atom that
%	specifies an HTTP location or  a   term  Alias(Sub).  This works
%	similar to absolute_file_name/2.  See   http:location_path/2  for
%	details.  Recognised properties are:
%
%		* requires(+Requirements)
%		Other required script and css files.  If this is a plain
%		file name, it is interpreted relative to the declared
%		resource.  Requirements can be a list, which is equivalent
%		to multiple requires properties.
%
%		* virtual(+Bool)
%		If =true= (default =false=), do not include About itself,
%		but only its dependencies.  This allows for defining an
%		alias for one or more resources.
%
%		* aggregate(+List)
%		States that About is an aggregate of the resources in
%		List.
%
%	Registering the same About multiple times extends the properties
%	defined  for  About.  In  particular,  this  allows  for  adding
%	additional dependencies to a (virtual) resource.

html_resource(About, Properties) :-
	assert_resource(About, -, Properties).

assert_resource(About, Location, Properties) :-
	assert(html_resource(About, Location, Properties)),
	clean_cache(About, Properties).

system:term_expansion((:-html_resource(About, Properties)),
		      html_head:html_resource(About, File:Line, Properties)) :-
	source_location(File, Line),
	clean_cache(About, Properties).

clean_cache(_About, Properties) :-
	clean_same_about_cache,
	(   memberchk(aggregate(_), Properties)
	->  clean_aggregate_cache
	;   true
	).


%%	html_requires(+ResourceOrList)// is det.
%
%	Include ResourceOrList and all dependencies  derived from it and
%	add them to the  HTML  =head=   using  html_post/2.  The  actual
%	dependencies are computed  during  the   HTML  output  phase  by
%	html_insert_resource//1.

html_requires(Required) -->
	html_post(head, 'html required'(Required)).

:- multifile
	html_write:html_head_expansion/2.

html_write:html_head_expansion(In, Out) :-
	require_commands(In, Required, Rest),
	Required \== [], !,
	flatten(Required, Plain),
	Out = [ html_head:(\html_insert_resource(Plain))
	      | Rest
	      ].

require_commands([], [], []).
require_commands([_:('html required'(Required))|T0], [Required|TR], R) :- !,
	require_commands(T0, TR, R).
require_commands([R|T0], TR, [R|T]) :- !,
	require_commands(T0, TR, T).


%%	html_insert_resource(+ResourceOrList)// is det.
%
%	Actually   include   HTML   head   resources.   Called   through
%	html_post//2   from   html_requires//1   after     rewrite    by
%	html_head_expansion/2. We are guaranteed we   will  only get one
%	call that is passed a flat   list  of requested requirements. We
%	have three jobs:
%
%	    1. Figure out all indirect requirements
%	    2. See whether we can use any `aggregate' resources
%	    3. Put required resources before their requiree.

html_insert_resource(Required) -->
	{ requirements(Required, Paths),
	  debug(html(script), 'Requirements: ~q~nFinal: ~q', [Required, Paths])
	},
	html_include(Paths).

requirements(Required, Paths) :-
	phrase(requires(Required), List),
	sort(List, Paths0),		% remove duplicates
	use_agregates(Paths0, Paths1, AggregatedBy),
	order_html_resources(Paths1, AggregatedBy, Paths).

%%	use_agregates(+Paths, -Aggregated, -AggregatedBy) is det.
%
%	Try to replace sets of  resources   by  an  `aggregate', a large
%	javascript or css file that  combines   the  content of multiple
%	small  ones  to  reduce  the  number   of  files  that  must  be
%	transferred to the server. The current rule says that aggregates
%	are used if at least half of the members are used.

use_agregates(Paths, Aggregated, AggregatedBy) :-
	empty_assoc(AggregatedBy0),
	use_agregates(Paths, Aggregated, AggregatedBy0, AggregatedBy).

use_agregates(Paths, Aggregated, AggregatedBy0, AggregatedBy) :-
	current_aggregate(Aggregate, Parts, Size),
	ord_subtract(Paths, Parts, NotCovered),
	length(Paths, Len0),
	length(NotCovered, Len1),
	Covered is Len0-Len1,
	Covered >= Size/2, !,
	ord_add_element(NotCovered, Aggregate, NewPaths),
	add_aggregated_by(Parts, AggregatedBy0, Aggregate, AggregatedBy1),
	use_agregates(NewPaths, Aggregated, AggregatedBy1, AggregatedBy).
use_agregates(Paths, Paths, AggregatedBy, AggregatedBy).

add_aggregated_by([], Assoc, _, Assoc).
add_aggregated_by([H|T], Assoc0, V, Assoc) :-
	put_assoc(H, Assoc0, V, Assoc1),
	add_aggregated_by(T, Assoc1, V, Assoc).


:- dynamic
	aggregate_cache_filled/0,
	aggregate_cache/3.
:- volatile
	aggregate_cache_filled/0,
	aggregate_cache/3.

clean_aggregate_cache :-
	retractall(aggregate_cache_filled).

%%	current_aggregate(-Aggregate, -Parts, -Size) is nondet.
%
%	True if Aggregate is a defined   aggregate  with Size Parts. All
%	parts are canonical absolute HTTP locations  and Parts is sorted
%	to allow for processing using ordered set predicates.

current_aggregate(Path, Parts, Size) :-
	aggregate_cache_filled, !,
	aggregate_cache(Path, Parts, Size).
current_aggregate(Path, Parts, Size) :-
	retractall(aggregate_cache(_,_, _)),
	forall(uncached_aggregate(Path, Parts, Size),
	       assert(aggregate_cache(Path, Parts, Size))),
	assert(aggregate_cache_filled),
	aggregate_cache(Path, Parts, Size).

uncached_aggregate(Path, APartsS, Size) :-
	html_resource(Aggregate, _, Properties),
	memberchk(aggregate(Parts), Properties),
	http_absolute_location(Aggregate, Path, []),
	absolute_paths(Parts, Path, AParts),
	sort(AParts, APartsS),
	length(APartsS, Size).

absolute_paths([], _, []).
absolute_paths([H0|T0], Base, [H|T]) :-
	http_absolute_location(H0, H, [relative_to(Base)]),
	absolute_paths(T0, Base, T).


%%	requires(+Spec)// is det.
%%	requires(+Spec, +Base)// is det.
%
%	True if Files is the set of  files   that  need to be loaded for
%	Spec. Note that Spec normally appears in  Files, but this is not
%	necessary (i.e. virtual resources  or   the  usage  of aggregate
%	resources).

requires(Spec) -->
	requires(Spec, /).

requires([], _) --> !,
	[].
requires([H|T], Base) --> !,
	requires(H, Base),
	requires(T, Base).
requires(Spec, Base) -->
	requires(Spec, Base, true).

requires(Spec, Base, Virtual) -->
	{ res_properties(Spec, Properties),
	  http_absolute_location(Spec, File, [relative_to(Base)])
	},
	(   { option(virtual(true), Properties)
	    ; Virtual == false
	    }
	->  []
	;   [File]
	),
	requires_from_properties(Properties, File).


requires_from_properties([], _) -->
	[].
requires_from_properties([H|T], Base) -->
	requires_from_property(H, Base),
	requires_from_properties(T, Base).

requires_from_property(requires(What), Base) --> !,
	requires(What, Base).
requires_from_property(_, _) -->
	[].


%%	order_html_resources(+Requirements, +AggregatedBy, -Ordered) is det.
%
%	Establish a proper order for the   collected (sorted and unique)
%	list of Requirements.

order_html_resources(Requirements, AggregatedBy, Ordered) :-
	requirements_graph(Requirements, AggregatedBy, Graph),
	(   top_sort(Graph, Ordered)
	->  true
	;   connect_graph(Graph, Start, Connected),
	    top_sort(Connected, Ordered0),
	    Ordered0 = [Start|Ordered]
	).

%%	requirements_graph(+Requirements, +AggregatedBy, -Graph) is det.
%
%	Produce an S-graph (see library(ugraphs))   that  represents the
%	dependencies  in  the  list  of  Requirements.  Edges  run  from
%	required to requirer.

requirements_graph(Requirements, AggregatedBy, Graph) :-
	phrase(prerequisites(Requirements, AggregatedBy, Vertices, []), Edges),
	vertices_edges_to_ugraph(Vertices, Edges, Graph).

prerequisites([], _, Vs, Vs) -->
	[].
prerequisites([R|T], AggregatedBy, Vs, Vt) -->
	prerequisites_for(R, AggregatedBy, Vs, Vt0),
	prerequisites(T, AggregatedBy, Vt0, Vt).

prerequisites_for(R, AggregatedBy, Vs, Vt) -->
	{ phrase(requires(R, /, false), Req) },
	(   {Req == []}
	->  {Vs = [R|Vt]}
	;   req_edges(Req, AggregatedBy, R),
	    {Vs = Vt}
	).

req_edges([], _, _) -->
	[].
req_edges([H|T], AggregatedBy, R) -->
	(   { get_assoc(H, AggregatedBy, Aggregate) }
	->  [Aggregate-R]
	;   [H-R]
	),
	req_edges(T, AggregatedBy, R).


%%	connect_graph(+Graph, -Start, -Connected) is det.
%
%	Turn Graph into a connected graph   by putting a shared starting
%	point before all vertices.

connect_graph([], 0, []) :- !.
connect_graph(Graph, Start, [Start-Vertices|Graph]) :-
	vertices(Graph, Vertices),
	Vertices = [First|_],
	before(First, Start).

%%	before(+Term, -Before) is det.
%
%	Unify Before to a term that comes   before  Term in the standard
%	order of terms.
%
%	@error instantiation_error if Term is unbound.

before(X, _) :-
	var(X), !,
	instantiation_error(X).
before(Number, Start) :-
	number(Number), !,
	Start is Number - 1.
before(_, 0).


%%	res_properties(+Spec, -Properties) is det.
%
%	True if Properties is the set of defined properties on Spec.

res_properties(Spec, Properties) :-
	findall(P, res_property(Spec, P), Properties0),
	list_to_set(Properties0, Properties).

res_property(Spec, Property) :-
	same_about(Spec, About),
	html_resource(About, _, Properties),
	member(Property, Properties).

:- dynamic
	same_about_cache/2.
:- volatile
	same_about_cache/2.

clean_same_about_cache :-
	retractall(same_about_cache(_,_)).

same_about(Spec, About) :-
	same_about_cache(Spec, Same), !,
	member(About, Same).
same_about(Spec, About) :-
	findall(A, uncached_same_about(Spec, A), List),
	assert(same_about_cache(Spec, List)),
	member(About, List).

uncached_same_about(Spec, About) :-
	html_resource(About, _, _),
	same_resource(Spec, About).


%%	same_resource(+R1, +R2) is semidet.
%
%	True if R1 an R2 represent  the   same  resource.  R1 and R2 are
%	resource specifications are defined by http_absolute_location/3.

same_resource(R, R) :- !.
same_resource(R1, R2) :-
	resource_base_name(R1, B),
	resource_base_name(R2, B),
	http_absolute_location(R1, Path, []),
	http_absolute_location(R2, Path, []).

:- dynamic
	base_cache/2.
:- volatile
	base_cache/2.

resource_base_name(Spec, Base) :-
	(   base_cache(Spec, Base0)
	->  Base = Base0
	;   uncached_resource_base_name(Spec, Base0),
	    assert(base_cache(Spec, Base0)),
	    Base = Base0
	).

uncached_resource_base_name(Atom, Base) :-
	atomic(Atom), !,
	file_base_name(Atom, Base).
uncached_resource_base_name(Compound, Base) :-
	arg(1, Compound, Base0),
	file_base_name(Base0, Base).

%%	html_include(+PathOrList)// is det.
%
%	Include to HTML resources  that  must   be  in  the  HTML <head>
%	element. Currently onlu supports  =|.js|=   and  =|.css|= files.
%	Extend this to support more  header   material.  Do not use this
%	predicate directly. html_requires//1 is the  public interface to
%	include HTML resources.
%
%	@param	HTTP location or list of these.

html_include([]) --> !.
html_include([H|T]) --> !,
	html_include(H),
	html_include(T).
html_include(Path) -->
	{ file_mime_type(Path, Mime) }, !,
	html_include(Mime, Path).

html_include(text/css, Path) --> !,
	html(link([ rel(stylesheet),
		    type('text/css'),
		    href(Path)
		  ], [])).
html_include(text/javascript, Path) --> !,
	html(script([ type('text/javascript'),
		      src(Path)
		    ], [])).
html_include(Mime, Path) -->
	{ print_message(warning, html_include(dont_know, Mime, Path))
	}.


		 /*******************************
		 *	  CACHE CLEANUP		*
		 *******************************/

:- multifile
	user:message_hook/3.
:- dynamic
	user:message_hook/3.

user:message_hook(make(done(Reload)), _Level, _Lines) :-
	Reload \== [],
	clean_same_about_cache,
	clean_aggregate_cache,
	fail.


		 /*******************************
		 *	       EDIT		*
		 *******************************/

% Allow edit(Location) to edit the :- html_resource declaration.
:- multifile
	prolog_edit:locate/3.

prolog_edit:locate(Path, html_resource(Spec), [file(File), line(Line)]) :-
	atom(Path),
	html_resource(Spec, File:Line, _Properties),
	sub_term(Path, Spec).
