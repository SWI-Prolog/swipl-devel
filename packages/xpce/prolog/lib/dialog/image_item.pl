/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
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
