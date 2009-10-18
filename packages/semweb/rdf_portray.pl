/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2007-2009, University of Amsterdam

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


:- module(rdf_portray,
	  [ rdf_portray_as/1,		% +Style
	    rdf_portray_lang/1		% +Lang
	  ]).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(error)).

/** <module> Portray RDF resources

This module defines  rules  for  user:portray/1   to  help  tracing  and
debugging  RDF  resources  by  printing   them    in   a   more  concise
representation and optionally adding comment  from   the  label field to
help the user interpreting the URL. The main predicates are:

	* rdf_portray_as/1 defines the overall style
	* rdf_portray_lang/1 selects languages for extracting label comments

@tbd	Define alternate predicate to use for providing a comment
@tbd	Use rdf:type if there is no meaningful label?
@tbd	Smarter guess whether or not the local identifier might be
	meaningful to the user without a comment.  I.e. does it look
	`word-like'?
*/

:- dynamic
	style/1,
	lang/1.

%%	rdf_portray_as(+Style) is det.
%
%	Set the style used to portray resources.  Style is one of:
%
%		* ns:id
%		Write as NS:ID, compatible with what can be handed to
%		the rdf predicates.  This is the default.
%
%		* writeq
%		Use quoted write of the full resource.
%
%		* ns:label
%		Write namespace followed by the label.  This format
%		cannot be handed to rdf/3 and friends, but can be
%		useful if resource-names are meaningless identifiers.
%
%		* ns:id=label
%		This combines ns:id with ns:label, providing both human
%		readable output and output that can be pasted into the
%		commandline.

rdf_portray_as(Style) :-
	must_be(oneof([writeq, ns:id, ns:label, ns:id=label]), Style),
	retractall(style(_)),
	assert(style(Style)).

%%	rdf_portray_lang(+Lang) is det.
%
%	If Lang is a list, set the list or preferred languages. If it is
%	a  single  atom,  push  this  language  as  the  most  preferred
%	language.

rdf_portray_lang(Lang) :-
	(   is_list(Lang)
	->  must_be(list(atom), Lang),
	    retractall(lang(_)),
	    forall(member(L,Lang), assert(lang(L)))
	;   must_be(atom, Lang),
	    asserta(lang(Lang))
	).

try_lang(L) :- lang(L).
try_lang(_).


:- multifile
	user:portray/1.

user:portray(URL) :-
	atom(URL),
	sub_atom(URL, 0, _, _, 'http://'), !,
	(   style(Style)
	->  true
	;   Style = ns:id
	),
	portray_url(Style, URL).
user:portray(URL) :-
	atom(URL),
	atom_concat('__file://', URL2, URL),
	sub_atom(URL2, S, _, A, #),
	sub_atom(URL2, _, A, 0, Local),
	sub_atom(URL2, 0, S, _, Path),
	file_base_name(Path, Base),
	format('__~w#~w', [Base, Local]).

portray_url(writeq, URL) :-
	writeq(URL).
portray_url(ns:id, URL) :-
	(   rdf_global_id(NS:Id, URL)
	->  writeq(NS:Id)
	;   writeq(URL)
	).
portray_url(ns:id=label, URL) :-
	(   rdf_global_id(NS:Id, URL)
	->  Value = NS:Id
	;   Value = URL
	),
	(   Id \== '',
	    (   (   try_lang(Lang),
		    rdf_has(URL, rdfs:label, literal(lang(Lang, Label)))
		->  nonvar(Lang),
		    \+ label_is_id(Label, Id)
		)
	    ->  format('~q/*"~w"@~w*/', [Value, Label, Lang])
	    ;   rdf_has(URL, rdfs:label, literal(type(Type, Label))),
		nonvar(Type),
		\+ label_is_id(Label, Id)
	    ->  (   rdf_global_id(TNS:TId, Type)
		->	TVal = TNS:TId
		;	TVal = Type
		),
		format('~q/*"~w"^^~w*/', [Value, Label, TVal])
	    ;   rdf_has(URL, rdfs:label, literal(Label)),
		atom(Label),
		Label \== Id
	    ->  format('~q/*"~w"*/', [Value, Label])
	    )
	->  true
	;   writeq(Value)
	).
portray_url(ns:label, URL) :-
	rdfs_ns_label(URL, Label),
	write(Label).

label_is_id(_, Var) :-
	var(Var), !, fail.
label_is_id(Label, Label) :- !.
label_is_id(L0, L1) :-
	downcase_atom(L0, Lwr),
	downcase_atom(L1, Lwr).

