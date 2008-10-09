/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2008, University of Amsterdam

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

:- module(http_path,
	  [ http_absolute_location/3	% +Spec, -Path, +Options
	  ]).
:- use_module(library(lists)).
:- use_module(library(error)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(option)).
:- use_module(library(settings)).
:- use_module(library(broadcast)).


/** <module> Abstract specification of HTTP server locations

This module provides an abstract specification  of HTTP server locations
that is inspired on absolute_file_name/3. The   specification is done by
adding rules to the  dynamic   multifile  predicate http:location/3. The
speficiation is very similar to   user:file_search_path/2,  but takes an
additional argument with options. Currently only one option is defined:

    * priority(+Integer)
    If two rules match, take the one with highest priority.  Using
    priorities is needed because we want to be able to overrule 
    paths, but we do not want to become dependent on clause ordering.

Here is an example that binds =|/login|=  to login/1. The user can reuse
this application while moving all locations  using   a  new rule for the
admin location with the option =|[priority(10)]|=.

==
:- multifile http:location/3.
:- dynamic   http:location/3.

http:location(admin, /, []).

:- http_handler(admin(login), login, []).

login(Request) :-
	...
==

@tbd	Make this module replace the http:prefix option.
@tbd	Remove hard-wired support for prefix().
*/

:- multifile
	http:location/3.		% Alias, Expansion, Options
:- dynamic
	http:location/3.		% Alias, Expansion, Options

http:location(root, Root, []) :-
	(   catch(setting(http:prefix, Prefix), _, fail),
	    Prefix \== ''
	->  Root = Prefix
	;   Root = (/)
	).

%%	http_absolute_location(+Spec, -Path, +Options) is det.
%
%	Path is the HTTP location for the abstract specification Spec.
%	Options:
%	
%	    * relative_to(Base)
%	    Path is made relative to Base.  Default is to generate
%	    absolute URLs.

:- dynamic
	location_cache/3.

http_absolute_location(Spec, Path, Options) :-
	must_be(ground, Spec),
	option(relative_to(Base), Options, /),
	absolute_location(Spec, Base, Path, Options).

absolute_location(Spec, Base, Path, _Options) :-
	location_cache(Spec, Base, Cache), !,
	Path = Cache.
absolute_location(Spec, Base, Path, Options) :-
	expand_location(Spec, Base, L, Options),
	assert(location_cache(Spec, Base, L)),
	Path = L.

expand_location(Spec, Base, Path, _Options) :-
	atomic(Spec), !,
	relative_to(Base, Spec, Path).
expand_location(Spec, _Base, Path, Options) :-
	Spec =.. [Alias, Sub],
	http_location_path(Alias, Parent),
	absolute_location(Parent, /, ParentLocation, Options),
	phrase(path_list(Sub), List),
	concat_atom(List, /, SubAtom),
	(   ParentLocation == ''
	->  Path = SubAtom
	;   sub_atom(ParentLocation, _, _, 0, /)
	->  atom_concat(ParentLocation, SubAtom, Path)
	;   concat_atom([ParentLocation, SubAtom], /, Path)
	).


%%	http_location_path(+Alias, -Expansion) is det.
%
%	Expansion is the expanded HTTP location for Alias. As we have no
%	condition search, we demand a single  expansion for an alias. An
%	ambiguous alias results in a printed   warning.  A lacking alias
%	results in an exception.
%	
%	@error	existence_error(http_alias, Alias)

http_location_path(Alias, Path) :-
	findall(P-L, http_location_path(Alias, L, P), Pairs),
	keysort(Pairs, Sorted0),
	reverse(Sorted0, Result),
	(   Result = [_-One]
	->  Path = One
	;   Result == []
	->  existence_error(http_location, Spec)
	;   Result = [P-Best,P2-_|_],
	    P \== P2
	->  Path = Best
	;   Result = [_-First|_],
	    pairs_values(Result, Paths),
	    print_message(warning, http(ambiguous_location(Spec, Paths))),
	    Path = First
	).


%%	http_location_path(+Alias, -Path, -Priority) is nondet.
%
%	@tbd	prefix(Path) is discouraged; use root(Path)

http_location_path(Alias, Path, Priority) :-
	http:location(Alias, Path, Options),
	option(priority(Priority), Options, 0).
http_location_path(prefix, Path, 0) :-
	(   catch(setting(http:prefix, Prefix), _, fail),
	    Prefix \== ''
	->  (	sub_atom(Prefix, 0, _, _, /)
	    ->  Path = Prefix
	    ;	atom_concat(/, Prefix, Path)
	    )
	;   Path = /
	).


%%	relative_to(+Base, +Path, -AbsPath) is det.
%
%	AbsPath is an absolute URL location created from Base and Path.
%	The result is cleaned

relative_to(/, Path, Path) :- !.
relative_to(_Base, Path, Path) :-
	sub_atom(Path, 0, _, _, /), !.
relative_to(Base, Local, Path) :-
	path_segments(Base, BaseSegments),
	append(BaseDir, [_], BaseSegments) ->
	path_segments(Local, LocalSegments),
	append(BaseDir, LocalSegments, Segments0),
	clean_segments(Segments0, Segments),
	path_segments(Path, Segments).
	
path_segments(Path, Segments) :-
	concat_atom(Segments, /, Path).

%%	clean_segments(+SegmentsIn, -SegmentsOut) is det.
%
%	Clean a path represented  as  a   segment  list,  removing empty
%	segments and resolving .. based on syntax.

clean_segments([''|T0], [''|T]) :- !,
	exclude(empty_segment, T0, T1),
	clean_parent_segments(T1, T).
clean_segments(T0, T) :-
	exclude(empty_segment, T0, T1),
	clean_parent_segments(T1, T).

clean_parent_segments([], []).
clean_parent_segments([..|T0], T) :- !,
	clean_parent_segments(T0, T).
clean_parent_segments([_,..|T0], T) :- !,
	clean_parent_segments(T0, T).
clean_parent_segments([H|T0], [H|T]) :-
	clean_parent_segments(T0, T).

empty_segment('').
empty_segment('.').


%%	path_list(+Spec, -List) is det.
%
%	Translate seg1/seg2/... into [seg1,seg2,...].
%	
%	@error	instantiation_error
%	@error	type_error(atomic, X)

path_list(Var) -->
	{ var(Var), !,
	  instantiation_error(Var)
	}.
path_list(A/B) --> !,
	path_list(A),
	path_list(B).
path_list(.) --> !,
	[].
path_list(A) -->
	{ must_be(atomic, A) },
	[A].


		 /*******************************
		 *	      MESSAGES		*
		 *******************************/

:- multifile
	prolog:message/3.

prolog:message(http(ambiguous_location(Spec, Paths))) -->
	[ 'http_absolute_location/2: ambiguous specification: ~q: ~p'-[Spec, Paths]
	].


		 /*******************************
		 *	  CACHE CLEANUP		*
		 *******************************/

clean_location_cache :-
	retractall(location_cache(_,_,_)).

:- listen(settings(changed(http:prefix, _, _)),
	  clean_location_cache).

:- multifile
	user:message_hook/3.
:- dynamic
	user:message_hook/3.

user:message_hook(make(done(Reload)), _Level, _Lines) :-
	Reload \== [],
	clean_location_cache,
	fail.
