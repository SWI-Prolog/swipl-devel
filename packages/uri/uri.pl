/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
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

:- module(uri,
	  [ encode_uri/2,		% +Text, -Atom
	    encode_uri_component/2,	% +Text, -Atom
	    encode_uri/3,		% +Text, -Codes, ?Tail
	    encode_uri_component/3,	% +Text, -Codes, ?Tail
	    decode_uri/2,		% +Text, -Atom
	    decode_uri_component/2,	% +Text, -Atom

	    parse_uri/3,		% +Text, -Parts, +Options
	    is_absolute_uri/1,		% +Text
	    uri_iri/3			% +URI, -IRI, +Options
	  ]).
:- use_module(library(shlib)).

:- initialization
   load_foreign_library(foreign(uri)).

/** <module> URI/URL handling library

@see	RFC3986 -- http://gbiv.com/protocols/uri/rfc/rfc3986.html
@see	UriParser -- http://uriparser.sourceforge.net/
*/

%%	escape_uri(+In, -Out) is det.
%%	escape_uri_component(+In, -Out) is det.

%%	escape_uri(+In, -Out:codes, ?Tail:codes) is det.
%%	escape_uri_component(+In, -Out:codes, ?Tail:codes) is det.

%%	decode_uri(+In, -Out) is det.
%%	decode_uri_component(+In, -Out) is det.

%%	parse_uri(+URI, -Parts, +Options) is det.
%
%	Parse a URI into its components.  Parts is a term uri/7:
%	
%	    1. Schema (atom)
%	    2. userInfo (atom)
%	    3. hostname (atom)
%	    4. port (integer)
%	    5. path (atom)
%	    6. query (atom)
%	    7. fragment (atom)
%
%	Options include:
%	
%	    * normalize(+Bool)
%	    Normalise the URI
%	    
%	    * base(+BaseURI)
%	    Decompose, relative to Base.

%%	uri_iri(+URI, -IRI, +Options) is det.
