/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
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

:- module(mimetype,
	  [ file_mime_type/2		% +Path, -Type
	  ]).

%	file_mime_type(+FileName, -MimeType)
%
%	Simple library to guess the mime-type from the extension of a file.
%	As various applications need to do this type of inferencing it seems
%	worthwhile to place this functionality in an extensible library.
%
%	Please add clauses to mime:mime_extension/2 to add your own types.

file_mime_type(File, MimeType) :-
	file_name_extension(_, Ext, File),
	(   current_prolog_flag(windows, true)
	->  downcase_atom(Ext, Lower),
	    mime_extension(Lower, MimeType)
	;   mime_extension(Ext, M0)
	->  MimeType = M0
	;   downcase_atom(Ext, Lower),
	    mime_extension(Lower, MimeType)
	).

:- multifile
	mime:mime_extension/2.

mime_extension(Ext, Mime) :-
	mime:mime_extension(Ext, Mime), !.
					% plain text
mime_extension(txt,  text/plain).
					% markup
mime_extension(htm,  text/html).
mime_extension(html, text/html).
mime_extension(sgml, text/'x-sgml').
mime_extension(sgm,  text/'x-sgml').
mime_extension(xml,  text/xml).
					% semantic web stuff
mime_extension(rdf,  application/'rdf+xml').
mime_extension(rdfs, application/'rdf+xml').
mime_extension(owl,  application/'rdf+xml').
					% Prolog source
mime_extension(pl,   application/'x-prolog').
					% Packaged formats
mime_extension(gz,   application/'x-gzip').
mime_extension(zip,  application/zip).
mime_extension(tgz,  application/'x-gtar').
					% Some document formats
mime_extension(pdf,  application/pdf).
mime_extension(doc,  application/msword).
					% Some image formats
mime_extension(jpg,  image/jpeg).
mime_extension(jpeg, image/jpeg).
mime_extension(gif,  image/gif).
mime_extension(png,  image/png).
mime_extension(tif,  image/tiff).
mime_extension(tiff, image/tiff).
mime_extension(xpm,  image/'x-xpixmap').
