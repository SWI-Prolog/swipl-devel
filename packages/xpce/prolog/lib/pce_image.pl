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
:- require([ concat_atom/2
	   , prolog_load_context/2
	   ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Prepend the given directory  to  the   image  search-path.   Useful  for
applications defining a private image directory.

Typically one would put all images required   by  an application in *.bm
files in a directory called bitmaps.  The   load file of the application
makes a call

	:- pce_image_directory(bitmaps).

after which the images from the  directory   may  be accessed using -for
example-:

	send(Box, fill_pattern, image('my_image.bm')).

See also the ImageViewer demo  tool  to   get  an  overview of available
images in a directory.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

pce_image_directory(Dir) :-
	(   atom(Dir),
	    prolog_load_context(directory, Cwd)
	->  concat_atom([Cwd, /, Dir], DirPath)
	;   absolute_file_name(Dir, DirPath)
	),
	get(class(image), resource, path, PathResource),
	get(PathResource, value, Path),
	send(PathResource, value, string('%s:%s', DirPath, Path)).
