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

:- module(url_image, []).
:- use_module(library(pce)).

:- pce_autoload(http_client, library(http_client)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Simple class to provide URL-based  images.   Currently  deals  only with
fetching images using the file and http protocols.

Fetched images are stored in a table.   Creating a url_image for the 2nd
time will return the previously loaded image.

TBD: Proper handling of errors.
TBD: What to do on a reload?
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(url_image, image,
		   "Image whose source comes from a URL").

variable(url,	name,	get,  "Source of the image").
variable(cache, bool,   get, "Image is cached").

initialise(I, URL:url=name, Cache:cache=[bool]) :->
	"Create image from URL data"::
	send_super(I, initialise),
	send(I, load, URL, Cache).

:- pce_global(@url_image_table, new(hash_table)).

lookup(_, URL:name, Cache:[bool], I:url_image) :<-
	"Lookup from image table"::
	Cache \== @off,
	get(@url_image_table, member, URL, I).

unlink(I) :->
	get(I, url, URL),
	(   get(I, cache, @on)
	->  send(@url_image_table, delete, URL)
	;   true
	),
	send_super(I, unlink).

free(I) :->
	"Only free if not referenced"::
	(   get(I, references, 1)	% 1 from hash-table
	->  send_super(I, free)
	).

:- pce_group(file).

load(I, URL:name, Cache:[bool]) :->
	"load from URL data"::
	send(I, slot, url, URL),
	(   new(Re, regex('file:\\(.*\\)', @off)),
	    send(Re, match, URL)
	->  get(Re, register_value, URL, 1, name, FileName),
	    send_super(I, load, FileName)
	;   send(URL, prefix, 'http:', @on)
	->  new(HC, http_client(URL)),
	    new(TB, text_buffer),
	    send(HC, fetch_data, TB),
	    send_super(I, load, TB),
	    free(TB),
	    free(HC)
	),
	(   Cache == @off
	->  send(I, slot, cache, @off)
	;   send(@url_image_table, append, URL, I),
	    send(I, slot, cache, @on)
	).

:- pce_end_class.

