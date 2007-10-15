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
:- use_module(library('semweb/rdf_db')).

/** <module> RDF Zip Plugin

This  module  connects   library(zlib)    to   library(rdf_db),  causing
rdf_load/2 to seemlessly load .gz files.
*/

:- multifile
	rdf_db:rdf_open_hook/3,
	rdf_db:rdf_input_info_hook/3.

rdf_db:rdf_open_hook(file(GZFile), Stream, gzip(Format)) :-
	file_name_extension(File, gz, GZFile), !,
	file_name_extension(_, Ext, File),
	rdf_db:rdf_file_type(Ext, Format),
	gzopen(GZFile, read, Stream, [type(binary)]).
rdf_db:rdf_open_hook(Source, Stream, gzip(Format)) :-
	rdf_db:rdf_input_open(Source, Stream0, Format),
	zopen(Stream0, Stream, []).

rdf_db:rdf_input_info_hook(file(GZFile), Modified, gzip(Format)) :-
	file_name_extension(File, gz, GZFile), !,
	file_name_extension(_, Ext, File),
	rdf_db:rdf_file_type(Ext, Format),
	time_file(GZFile, Modified).
