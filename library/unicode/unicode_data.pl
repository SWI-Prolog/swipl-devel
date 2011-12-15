/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@science.uva.nl
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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(unicode_data,
	  [ unicode_property/2		% ?Code, ?Property
	  ]).
:- use_module(library(table)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module provides access to the   UNICODE datafile distributed by the
unicode organisation (http://www.unicode.org). This  file describes many
aspects for all defined UNICODE  code   positions,  such  as their name,
type, etc.  The meaning of the fields is defined here:

	http://www.unicode.org/Public/UNIDATA/UCD.html#UCD_File_Format

This library uses the table package for accessing structured files. This
maps the file in memory and performs  binary search. This is not blindly
fast and this library should therefore   not be used for computationally
intensive tasks. In such cases it  can   be  used  to generate tables in
Prolog or even to create a dedicated C datastructure.

The file UnicodeData.txt itself is not part   of the library and must be
obtained and installed seperately. This is because of its size (close to
1MB). Increasing the footprint of the environment with 1MB is too much.

The UCD file must be  named  UnicodeData.txt   and  placed  in  the same
directory  as  this  file  or  in    the   search  path  'unicode'  (see
file_search_path/2).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%	unicode_property(?Code, ?Property)
%
%	Logical predicate relating code  points   to  properties.  It is
%	optimised for asking a single  property   of  a  known code, but
%	works with any instantiation.

unicode_property(Code, Property) :-
	table(Handle),
	property(Property),
	in_table(Handle, [code(Code), Property], _),
	\+ arg(1, Property, '').

property(name(_)).
property(general_category(_)).
property(canonical_combining_class(_)).
property(bidi_class(_)).
property(decomposition_type(_)).
property(numeric_type_1(_)).
property(numeric_type_2(_)).
property(numeric_type_3(_)).
property(bidi_mirrored(_)).
property(unicode_1_name(_)).
property(iso_comment(_)).
property(simple_uppercase_mapping(_)).
property(simple_lowercase_mapping(_)).
property(simple_titlecase_mapping(_)).

:- dynamic
	handle/1.
:- volatile
	handle/1.

:- multifile
	user:file_search_path/2.
:- dynamic
	user:file_search_path/2.

:- (   user:file_search_path(unicode, _)
   ->  true
   ;   prolog_load_context(directory, Dir),
       assert(user:file_search_path(unicode, Dir))
   ).

table(Handle) :-
	handle(Handle), !.
table(Handle) :-
	absolute_file_name(unicode('UnicodeData.txt'),
			   Path,
			   [ access(read)
			   ]),
	new_table(Path,
		  [ code(hexadecimal, [sorted, unique]),
		    name(atom, [downcase]),		    		% 1
		    general_category(atom), 				% 2
		    canonical_combining_class(integer),			% 3
		    bidi_class(atom, [downcase]),			% 4
		    decomposition_type(atom),				% 5
		    numeric_type_1(integer, [syntax]),			% 6
		    numeric_type_2(integer, [syntax]),			% 7
		    numeric_type_3(integer, [syntax]),			% 8
		    bidi_mirrored(atom, [downcase]),			% 9
		    unicode_1_name(atom, [downcase]),			% 10
		    iso_comment(atom),					% 11
		    simple_uppercase_mapping(hexadecimal, [syntax]),	% 12
		    simple_lowercase_mapping(hexadecimal, [syntax]),	% 13
		    simple_titlecase_mapping(hexadecimal, [syntax])	% 14
		  ],
		  [ field_separator(0';)
		  ],
		  Handle),
	assert(handle(Handle)).

