/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

:- module(pce_image,
	  [ pce_image_directory/1
	  ]).
:- use_module(library(pce)).
:- require([ absolute_file_name/3
	   , concat_atom/2
	   , is_absolute_file_name/1
	   ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Prepend the given  directory  to  the   image  search-path.  Useful  for
applications defining a private image directory.

Typically one would put all images required   by an application in *.xpm
files in a directory called bitmaps. The   load  file of the application
makes a call

	:- pce_image_directory(bitmaps).

after which the images from the  directory   may  be accessed using -for
example-:

	send(Box, fill_pattern, image('my_image.bm')).

See also the ImageViewer demo  tool  to   get  an  overview of available
images in a directory.

Images and program resources
----------------------------

pce_image_directory/1   prepends   the   given     directory    to   the
file_search_path/2 declarations using the alias   `image'. Especially if
the application should (eventually)  be  turned   into  a  runtime,  the
following skeleton for using images as program resources is adviced:

resource(cute,	image, image('cute.xpm')).

	...
	new(I, image(resource(cute)),
	...
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

pce_image_directory(Dir) :-
	(   atom(Dir)
	->  (   \+ is_absolute_file_name(Dir),
	        prolog_load_context(directory, Cwd)
	    ->	concat_atom([Cwd, /, Dir], DirPath)
	    ;	DirPath = Dir
	    ),
	    asserta(user:file_search_path(image, DirPath))
	;   asserta(user:file_search_path(image, Dir)),
	    absolute_file_name(Dir,
			       [ file_type(directory),
				 access(read)
			       ], DirPath)
	),
	get(class(image), class_variable, path, PathVar),
	get(PathVar, value, Path),
	send(PathVar, value, string('%s:%s', DirPath, Path)).
