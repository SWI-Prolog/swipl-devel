/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2004, University of Amsterdam

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

:- module(xsdp_type,
	  [ xsdp_type/1,		% ?Type
	    xsdp_subtype_of/2,		% ?Type, ?Super
	    xsdp_convert/3		% +Type, +Content, -Value
	  ]).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This  modules  provides  support  for  the  primitive  XML-Schema  (XSD)
datatypes. It defines the type hierarch  which allows for reasoning over
types as well as xsd_convert/3  to  convert   XML  content  to a natural
Prolog representation of the XSD type.

Based on the W3C definitions at

	http://www.w3.org/TR/xmlschema-2/#built-in-datatypes

The current implementation is incomplete and only  there to test the API
and its integration with rdf:dataType=Type handling in the RDF parser.

The extra 'p' in the module  prefix  (xsdp_*)   is  used  to allow for a
module xsd_*, providing full  user-defined  XSD   types  on  top of this
module.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

ns('http://www.w3.org/2001/XMLSchema#').


		 /*******************************
		 *    PRIMITIVE TYPE HIERARCHY	*
		 *******************************/

%	xsdp_type(?Type)
%	
%	Test/generate the names for the XML schema primitive types

xsdp_type(Type) :-
	subtype_of(Type, _).

%	xsdp_subtype_of(?Type, ?Super)
%	
%	True if Type is a (transitive) subtype of Super.

xsdp_subtype_of(Type, Type).
xsdp_subtype_of(Type, Super) :-
	(   nonvar(Type)
	->  subtype_of(Type, Super0),
	    Super0 \== (-),
	    xsdp_subtype_of(Type, Super)
	;   subtype_of(Sub0, Super),
	    xsdp_subtype_of(Type, Sub0)
	).

subtype_of(anyType,	       -).
subtype_of(anySimpleType,      anyType).
					% string hierarchy
subtype_of(string,	       anySimpleType).
subtype_of(normalizedString,   string).
subtype_of(token,	       normalizedString).
subtype_of(language,	       token).
subtype_of('Name',	       token).
subtype_of('NCName',	       'Name').
subtype_of('NMTOKEN',	       token).
subtype_of('NMTOKENS',	       'NMTOKEN').
subtype_of('ID',	       'NCName').
subtype_of('IDREF',	       'NCName').
subtype_of('IDREFS',	       'IDREF').
subtype_of('ENTITY',	       'NCName').
subtype_of('ENTITIES',	       'ENTITY').
					% numeric hierarchy
subtype_of(decimal,	       anySimpleType).
subtype_of(integer,	       decimal).
subtype_of(nonPositiveInteger, integer).
subtype_of(negativeInteger,    nonPositiveInteger).
subtype_of(long,	       integer).
subtype_of(int,		       long).
subtype_of(short,	       int).
subtype_of(byte,	       short).
subtype_of(nonNegativeInteger, integer).
subtype_of(unsignedLong,       nonNegativeInteger).
subtype_of(positiveInteger,    nonNegativeInteger).
subtype_of(unsignedInt,	       unsignedLong).
subtype_of(unsignedShort,      unsignedInt).
subtype_of(unsignedByte,       unsignedShort).
					% other simple types
subtype_of(duration,	       anySimpleType).
subtype_of(dateTime,	       anySimpleType).
subtype_of(time,	       anySimpleType).
subtype_of(date,	       anySimpleType).
subtype_of(gYearMonth,	       anySimpleType).
subtype_of(gYear,	       anySimpleType).
subtype_of(gMonthDay,	       anySimpleType).
subtype_of(gDay,	       anySimpleType).
subtype_of(gMonth,	       anySimpleType).
subtype_of(boolean,	       anySimpleType).
subtype_of(base64Binary,       anySimpleType).
subtype_of(hexBinary,	       anySimpleType).
subtype_of(float,	       anySimpleType).
subtype_of(double,	       anySimpleType).
subtype_of(anyURI,	       anySimpleType).
subtype_of('QName',	       anySimpleType).
subtype_of('NOTATION',	       anySimpleType).

term_expansion(integer_types, Clauses) :-
	findall(integer_type(Type), xsdp_subtype_of(Type, integer), Clauses).
term_expansion(xsd_local_ids, Clauses) :-
	ns(NS),
	findall(xsd_local_id(URI, Type),
		(   xsdp_type(Type),
		    atom_concat(NS, Type, URI)
		),
		Clauses).

integer_types.
xsd_local_ids.

%	xsdp_convert(+Type, +Content, -Value)
%	
%	Convert the content model Content to an  object of the given XSD
%	type and return the Prolog value in Value.

xsdp_convert(URI, Content, Value) :-
	(   xsd_local_id(URI, Type)
	->  convert(Type, Content, Value)
	;   convert(URI, Content, Value)
	).

convert(anyType, Term, Term) :- !.
convert(anySimpleType, [Simple], Simple) :- !.
					% strings
convert(string, [String], String) :- !.
					% numbers
convert(IntType, [Text], Integer) :-
	integer_type(IntType), !,
	atom_number(Text, Integer),
	(   integer(Integer),
	    validate_int_domain(IntType, Integer)
	->  true
	;   throw(error(domain_error(Text, IntType), _))
	).
convert(float, [Text], Float) :- !,
	atom_number(Text, Number),
	Float is float(Number).
convert(double, [Text], Float) :- !,
	atom_number(Text, Number),
	Float is float(Number).
convert(_Any, [X], X) :- !.		% TBD: provide for more types
convert(_Any, X, X).

validate_int_domain(integer, _).
validate_int_domain(int, _).
validate_int_domain(long, _).
validate_int_domain(nonPositiveInteger, I) :- \+ I > 0.
validate_int_domain(negativeInteger, I) :-    I < 0.
validate_int_domain(short, I) :-	      between(-32768, 32767, I).
validate_int_domain(byte,  I) :-	      between(-128, 127, I).
validate_int_domain(nonNegativeInteger, I) :- \+ I < 0.
validate_int_domain(unsignedLong,       I) :- I >= 0.
validate_int_domain(positiveInteger,	I) :- I > 0.
validate_int_domain(unsignedInt,	I) :- I >= 0.
validate_int_domain(unsignedShort,	I) :- between(0, 65535, I).
validate_int_domain(unsignedByte,	I) :- between(0, 255, I).
