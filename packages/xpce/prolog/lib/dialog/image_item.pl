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

:- module(dia_image_item, []).
:- use_module(library(pce)).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Library defined class `image_item', which defines a text item that can
be used to specify an image using automatic completion.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(image_item, text_item).

:- pce_global(@image_path_regex, new(regex('[^:]+'))).

initialise(I, Name:[name], Default:[any|function], Msg:[code]*) :->
	"Initialise the value-set"::
	send(I, send_super, initialise, Name, Default, Msg),
	send(I, type, image),
	get(class(image), class_variable_value, path, Path),
	new(ValueSet, chain),
	send(ValueSet, lock_object, @on),
	send(@image_path_regex, for_all, Path,
	     and(assign(new(Dir, var),
			create(directory,
			       ?(@arg1, register_value, @arg2, 0))),
		 if(message(Dir, exists),
		    message(Dir, scan, ValueSet, ValueSet, '.*\\.bm$')))),
	Object = ?(@pce, object_from_reference, @arg1),
	send(@pce, for_name_reference,
	     if(message(Object, '_instance_of', image),
		message(ValueSet, append, create(string, '@%s', @arg1)))),
	send(ValueSet, sort),
	send(ValueSet, unique),
	send(I, value_set, ValueSet),
	send(ValueSet, lock_object, @off).

:- pce_end_class.
