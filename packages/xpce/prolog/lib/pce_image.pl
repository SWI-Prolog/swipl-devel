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
	(   compiling,
	    get(@display, open, @off)	% don't force the display
					% when compiling
	->  true
	;   get(class(image), class_variable, path, PathVar),
	    get(PathVar, value, Path),
	    send(PathVar, value, string('%s:%s', DirPath, Path))
	).
