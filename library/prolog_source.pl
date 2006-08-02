/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2005, University of Amsterdam

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

:- module(prolog_source,
	  [ prolog_open_source/2,	% +Source, -Stream
	    prolog_canonical_source/2	% +Spec, -Id
	  ]).

/** <module> Examine Prolog source-files

The modile prolog_source.pl provides predicates to  open, close and read
terms from Prolog source-files. This may  seem   easy,  but  there are a
couple of problems that must be taken care of.

	* Source files may start with #!, supporting PrologScript
	* Embeded operators declarations must be taken into account
	* Style-check options must be taken into account
	* Operators and style-check options may be implied by directives
	* On behalf of the development environment we also wish to
	  parse PceEmacs buffers

This module concentrates these issues  in   a  single  library. Intended
users of the library are:

	$ prolog_xref.pl : The Prolog cross-referencer
	$ PceEmacs :	   Emacs syntax-colouring
	$ PlDoc :	   The documentation framework
*/

%	prolog_open_source(+CanonicalId:atomic, -Stream:stream) is det.
%	
%	Open source with given canonical id (see canonical_source/2) and
%	remove the #! line if any.

prolog_open_source(Src, Fd) :-
	(   prolog:xref_open_source(Src, Fd)
	->  true
	;   open(Src, read, Fd)
	),
	(   peek_char(Fd, #)		% Deal with #! script
	->  skip(Fd, 10)
	;   true
	).


%%	prolog_canonical_source(+SourceSpec:ground, -Id:atomic) is det.
%	
%	Given a user-specification of a source,   generate  a unique and
%	indexable  identifier  for   it.   For    files   we   use   the
%	prolog_canonical absolute filename.

prolog_canonical_source(Src, Id) :-		% Call hook
	prolog:xref_source_identifier(Src, Id), !.
prolog_canonical_source(User, user) :-
	User == user, !.
prolog_canonical_source(Source, Src) :-
	absolute_file_name(Source,
			   [ file_type(prolog),
			     access(read),
			     file_errors(fail)
			   ],
			   Src), !.
prolog_canonical_source(Source, Src) :-
	var(Source), !,
	Src = Source.
