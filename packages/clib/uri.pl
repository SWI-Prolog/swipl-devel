/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2009, VU University Amsterdam

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

:- module(uri,
	  [ uri_components/2,		% ?URI, ?Components
	    uri_data/3,			% ?Field, +Components, ?Data

	    uri_normalized/2,		% +URI, -NormalizedURI
	    uri_normalized_iri/2,	% +URI, -NormalizedIRI
	    uri_normalized/3,		% +URI, +Base, -NormalizedURI
	    uri_normalized_iri/3,	% +URI, +Base, -NormalizedIRI
	    uri_resolve/3,		% +URI, +Base, -AbsURI
	    uri_is_global/1,		% +URI
	    uri_query_components/2,	% ?QueryString, ?NameValueList
	    uri_authority_components/2,	% ?Authority, ?Components
	    uri_authority_data/3,	% ?Field, ?Components, ?Data
					% Encoding
	    uri_encoded/3,		% +Component, ?Value, ?Encoded
	    uri_iri/2			% ?URI, ?IRI
	  ]).
:- use_foreign_library(foreign(uri)).

/** <module> Process URIs

This  library  provides   high-performance    C-based   primitives   for
manipulating URIs. We decided for a  C-based implementation for the much
better performance on raw character  manipulation. Notably, URI handling
primitives are used in  time-critical  parts   of  RDF  processing. This
implementation is based on RFC-3986:

	http://labs.apache.org/webarch/uri/rfc/rfc3986.html

The URI processing in this library is  rather liberal. That is, we break
URIs according to the rules, but we  do not validate that the components
are valid. Also, percent-decoding for IRIs   is  liberal. It first tries
UTF-8; then ISO-Latin-1 and finally accepts %-characters verbatim.

Earlier experience has shown that strict   enforcement of the URI syntax
results in many errors that  are   accepted  by  many other web-document
processing tools.
*/

%%	uri_components(+URI, -Components) is det.
%%	uri_components(-URI, +Components) is det.
%
%	Break a URI  into  its  5   basic  components  according  to the
%	RFC-3986 regular expression:
%
%	    ==
%	    ^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\?([^#]*))?(#(.*))?
%	     12            3  4          5       6  7        8 9
%	    ==
%
%	@param Components is a term uri_components(Scheme, Authority,
%	Path, Search, Fragment).  See uri_data/5 for accessing this
%	structure.

%%	uri_data(?Field, +Components, ?Data) is semidet.
%
%	Provide access the uri_component structure.  Defined field-names
%	are: =scheme=, =authority=, =path=, =search= and =fragment=

uri_data(scheme,    uri_component(S, _, _, _, _), S).
uri_data(authority, uri_component(_, A, _, _, _), A).
uri_data(path,	    uri_component(_, _, P, _, _), P).
uri_data(search,    uri_component(_, _, _, S, _), S).
uri_data(fragment,  uri_component(_, _, _, _, F), F).

%%	uri_normalized(+URI, -NormalizedURI) is det.
%
%	NormalizedURI is the normalized form   of  URI. Normalization is
%	syntactic and involves the following steps:
%
%	    * 6.2.2.1. Case Normalization
%	    * 6.2.2.2. Percent-Encoding Normalization
%	    * 6.2.2.3. Path Segment Normalization
%	    * 6.2.3. Scheme-Based Normalization

%%	uri_normalized_iri(+URI, -NormalizedIRI) is det.
%
%	As uri_normalized/2, but percent-encoding is translated into IRI
%	Unicode characters. The translation  is   liberal:  valid  UTF-8
%	sequences  of  %-encoded  bytes  are    mapped  to  the  Unicode
%	character. Other %XX-sequences are mapped   to the corresponding
%	ISO-Latin-1 character and sole % characters are left untouched.
%
%	@see uri_iri/2.


%%	uri_is_global(+URI) is semidet.
%
%	True if URI has a scheme. The semantics  is the same as the code
%	below, but the implementation is more   efficient as it does not
%	need to parse the  other  components,   nor  needs  to  bind the
%	scheme.
%
%	==
%	uri_is_global(URI) :-
%		uri_components(URI, Components),
%		uri_data(Components, scheme, Scheme),
%		nonvar(Scheme).
%	==

%%	uri_resolve(+URI, +Base, -GlobalURI) is det.
%
%	Resolve a possibly local URI relative   to Base. This implements
%	http://labs.apache.org/webarch/uri/rfc/rfc3986.html#relative-transform

%%	uri_normalized(+URI, +Base, -NormalizedGlobalURI) is det.
%
%	NormalizedGlobalURI is the normalized global version of URI.
%	Behaves as if defined by:
%
%	==
%	uri_normalized(URI, Base, NormalizedGlobalURI) :-
%		uri_resolve(URI, Base, GlobalURI),
%		uri_normalized(GlobalURI, NormalizedGlobalURI).
%	==

%%	uri_normalized_iri(+URI, +Base, -NormalizedGlobalIRI) is det.
%
%	NormalizedGlobalIRI is the normalized global IRI of URI. Behaves
%	as if defined by:
%
%	==
%	uri_normalized(URI, Base, NormalizedGlobalIRI) :-
%		uri_resolve(URI, Base, GlobalURI),
%		uri_normalized_iri(GlobalURI, NormalizedGlobalIRI).
%	==

%%	uri_query_components(+String, -Query) is det.
%%	uri_query_components(-String, +Query) is det.
%
%	Perform encoding and decoding of an URI query string. Query is a
%	list of fully decoded (Unicode) Name=Value pairs. In mode (-,+),
%	query elements of the forms Name(Value)  and Name-Value are also
%	accepted to enhance interoperability with   the option and pairs
%	libraries.  E.g.
%
%	==
%	?- uri_query_components(QS, [a=b, c('d+w'), n-'VU Amsterdam']).
%	QS = 'a=b&c=d%2Bw&n=VU%20Amsterdam'.
%
%	?- uri_query_components('a=b&c=d%2Bw&n=VU%20Amsterdam', Q).
%	Q = [a=b, c='d+w', n='VU Amsterdam'].
%	==


%%	uri_authority_components(+Authority, -Components) is det.
%%	uri_authority_components(-Authority, +Components) is det.
%
%	Break-down the authority component of a   URI. The fields of the
%	structure Components can be accessed using uri_authority_data/3.

%%	uri_authority_data(+Field, ?Components, ?Data) is semidet.
%
%	Provide access the uri_authority  structure. Defined field-names
%	are: =user=, =password=, =host= and =port=

uri_authority_data(user,     uri_authority(U, _, _, _), U).
uri_authority_data(password, uri_authority(_, P, _, _), P).
uri_authority_data(host,     uri_authority(_, _, H, _), H).
uri_authority_data(port,     uri_authority(_, _, _, P), P).


%%	uri_encoded(+Component, +Value, -Encoded) is det.
%%	uri_encoded(+Component, -Value, +Encoded) is det.
%
%	Encoded  is  the  URI   encoding    for   Value.  When  encoding
%	(Value->Encoded), Component specifies the   URI  component where
%	the value is used. It  is   one  of =query_value=, =fragment= or
%	=path=.  Besides  alphanumerical  characters,    the   following
%	characters are passed verbatim (the  set   is  split  in logical
%	groups according to RFC3986).
%
%	    $ query_value, fragment :
%	    "-._~" | "!$'()*,;" | ":@" | "/?"
%	    $ path :
%	    "-._~" | "!$&'()*,;=" | ":@" | "/"


%%	uri_iri(+URI, -IRI) is det.
%%	uri_iri(-URI, +IRI) is det.
%
%	Convert between a URI, encoded in US-ASCII and an IRI. An IRI is
%	a fully expanded  Unicode  string.   Unicode  strings  are first
%	encoded into UTF-8, after which %-encoding takes place.
%
%	@error syntax_error(Culprit) in mode (+,-) if URI is not a
%	legally percent-encoded UTF-8 string.
