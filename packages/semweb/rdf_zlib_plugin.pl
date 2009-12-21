/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2006, University of Amsterdam

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


:- module(rdf_zlib_plugin, []).
:- use_module(library(zlib)).
:- use_module(library(semweb/rdf_db)).

/** <module> RDF compressed-data plugin

This  module  connects   library(zlib)    to   library(rdf_db),  causing
rdf_load/2 to seemlessly load .gz files.
*/

:- multifile
	rdf_db:rdf_open_decode/4,
	rdf_db:rdf_storage_encoding/2.

rdf_db:rdf_open_decode(gzip, File, Stream, close(Stream)) :-
	gzopen(File, read, Stream).

rdf_db:rdf_storage_encoding(gz, gzip).
