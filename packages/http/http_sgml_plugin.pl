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

:- module(http_sgml_plugin, []).
:- use_module(http_client).
:- use_module(library(memfile)).
:- use_module(library(sgml)).

:- multifile
	http_client:http_convert_data/4.

:- multifile
	markup_type/2.			% +MimeType, -ParseOptions

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module provides a plugin for the   HTTP  client to handle xml, html
and sgml files using  the   SWI-Prolog  sgml-parser  from library(sgml).
Using this library avoids unnecessary copying of data as the sgml-parser
reads directly from the stream that established the HTTP connection.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

http_client:http_convert_data(In, Fields, Data, Options) :-
	memberchk(content_type(Type), Fields),
	(   markup_type(Type, ParseOptions)
	->  true
	;   major_type(Type, Major),
	    default_markup_type(Major, ParseOptions)
	),
	merge_options([ max_errors(-1),
			syntax_errors(quiet)
		      | ParseOptions
		      ], Options, Merged),
	markup_options(Fields, Merged, MarkupOptions),
	load_structure(stream(In), Data, MarkupOptions).


major_type(Type0, Type) :-
	sub_atom(Type0, B, _, _, ;), !,
	sub_atom(Type0, 0, B, _, Major),
	atom_codes(Major, Codes),
	delete_white(Codes, NonWhite),
	atom_codes(Type, NonWhite).
major_type(Type, Type).

delete_white([], []).
delete_white([H|_], []) :-
	code_type(H, white), !.
delete_white([H|T0], [H|T]) :-
	delete_white(T0, T).


%	default_markup_type(+MimeType, -ParseOptions)
%	
%	State that the HTTP contents should be parsed with
%	load_structure/3 using the returned options. This predicate may
%	be hooked using the multifile predicate markup_type/2.

default_markup_type('text/xml',
	    [ dialect(xmlns)
	    ]).
default_markup_type('text/html',
	    [ dtd(DTD),
	      dialect(sgml),
	      shorttag(false)
	    ]) :-
	dtd(html, DTD).
default_markup_type('text/x-sgml',
	    [ dialect(sgml)
	    ]).

markup_options(Fields, Opt0, Options) :-
	(   memberchk(content_length(Bytes), Fields)
	->  Options = [content_length(Bytes)|Opt0]
	;   Options = Opt0
	).

%	merge_options(+Defaults, +GivenOptions, -Options)
%	
%	If an option is not in GivenOptions, use the one from
%	Defaults.

merge_options([], Options, Options).
merge_options([H|T], Options0, Options) :-
	functor(H, Name, Arity),
	functor(H0, Name, Arity),
	memberchk(H0, Options0), !,
	merge_options(T, Options0, Options).
merge_options([H|T], Options0, Options) :-
	merge_options(T, [H|Options0], Options).
