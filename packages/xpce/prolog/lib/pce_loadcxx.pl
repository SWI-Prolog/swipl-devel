/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
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

:- module(pce_load_cxx,
	  [ pce_load_cxx/1		% +File
	  ]).
:- use_module(library(pce)).
:- require([ absolute_file_name/3
	   , feature/2
	   , open_dll/2
	   , open_shared_object/2
	   ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
pce_load_cxx(+Spec)
    Load a Unix shared object or Windows DLL or somilar object, containing
    XPCE/C++ code.  This call deals with OS and Prolog incompatibilities. 
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

pce_load_cxx(File) :-
	current_prolog_flag(open_shared_object, true), !,
	absolute_file_name(File,
			   [ extensions([so]),
			     access(read)
			   ],
			   Path),
	send(@pce, succeed),		% ensure XPCE is loaded
	open_shared_object(Path, _Handle).
