/*  $Id$

    Part of SWI-Prolog SGML/XML parser

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: LGPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2000 SWI, University of Amsterdam. All rights reserved.
*/

:- module(uri,
	  [ canonical_uri/3		% +URI, +BaseURI, -CanonicalURI
	  ]).

%	canonical_uri(+URI, +BaseURI, -CanonicalURI)
%
%	Turn URI into a global URI.  Rather crude as it stands.

canonical_uri(URI, _, CanonicalURI) :-
	is_global_uri(URI), !,
	CanonicalURI = URI.
canonical_uri(URI, BaseURI, CanonicalURI) :-
	sub_atom(URI, 0, _, _, #), !,
	atom_concat(BaseURI, URI, CanonicalURI).
canonical_uri(URI, BaseURI, CanonicalURI) :-
	file_directory_name(BaseURI, BaseDir),
	concat_atom([BaseDir, URI], /, CanonicalURI).

%	We assume a URI is global if is starts with an identifier
%	followed by a colon (:).

is_global_uri(URI) :-
	sub_atom(URI, P, _, _, :),
	sub_atom(URI, 0, P, _, Protocol),
	atom_codes(Protocol, Codes),
	id_chars(Codes).

id_chars([]).
id_chars([H|T]) :-
	code_type(H, alnum),
	id_chars(T).
