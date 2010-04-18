/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2009, VU University Amsterdam

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

:- module(http_dirindex,
	  [ http_reply_dirindex/3	% +PhysicalDir, +Options, +Request
	  ]).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_server_files)).
:- use_module(library(http/html_head)).
:- use_module(library(apply)).
:- use_module(library(option)).

/** <module> HTTP directory listings

This module provides a simple API to   generate  an index for a physical
directory. The index can be customised   by  overruling the dirindex.css
CSS file and by defining  additional  rules   for  icons  using the hook
http:file_extension_icon/2.

@tbd	Provide more options (sorting, selecting columns, hiding files)
*/

%%	http_reply_dirindex(+DirSpec, +Options, +Request) is det.
%
%	Provide a directory listing for Request, assuming it is an index
%	for the physical directrory Dir. If   the  request-path does not
%	end with /, first return a moved (301 Moved Permanently) reply.
%
%	The  calling  conventions  allows  for    direct   calling  from
%	http_handler/3.

http_reply_dirindex(DirSpec, Options, Request) :-
	http_safe_file(DirSpec, Options),
	absolute_file_name(DirSpec, Dir,
			   [ file_type(directory),
			     access(read)
			   ]),
	memberchk(path(Path), Request),
	(   atom_concat(PlainPath, /, Path),
	    merge_options(Options,
			  [ title(['Index of ', PlainPath]) ],
			  Options1)
	->  dir_index(Dir, Options1)
	;   atom_concat(Path, /, NewLocation),
	    throw(http_reply(moved(NewLocation)))
	).

dir_index(Dir, Options) :-
	directory_members(Dir, SubDirs, Files),
	option(title(Title), Options, Dir),
	reply_html_page(title(Title),
			[ \html_requires(http_dirindex),
			  h1(Title),
			  table(class(dirindex),
				[ \dirindex_title,
				  \back
				| \dirmembers(SubDirs, Files)
				])
			]).

directory_members(Dir, Dirs, Files) :-
	atom_concat(Dir, '/*', Pattern),
	expand_file_name(Pattern, Matches),
	partition(exists_directory, Matches, Dirs, Files).

dirindex_title -->
	html(tr(class(dirindex_header),
		[ th(class(icon),     ''),
		  th(class(name),     'Name'),
		  th(class(modified), 'Last modified'),
		  th(class(size),     'Size')
		])).

back -->
	html(tr([ \icon_cell('back.png', '[UP]'),
		  \name_cell(.., 'Up'),
		  td(class(modified), -),
		  td(class(size),     -)
		])).

dirmembers(Dirs, Files) -->
	dir_rows(Dirs, odd, End),
	file_rows(Files, End, _).

dir_rows([], OE, OE) --> [].
dir_rows([H|T], OE0, OE) -->
	dir_row(H, OE0),
	{ oe(OE0, OE1) },
	dir_rows(T, OE1, OE).

file_rows([], OE, OE) --> [].
file_rows([H|T], OE0, OE) -->
	file_row(H, OE0),
	{oe(OE0, OE1)},
	file_rows(T, OE1, OE).

oe(odd, even).
oe(even, odd).

dir_row(Dir, OE) -->
	{ file_base_name(Dir, Name)
	},
	html(tr(class(OE),
		[ \icon_cell('folder.png', '[DIR]'),
		  \name_cell(Name, Name),
		  \modified_cell(Dir),
		  td(class(size), -)
		])).


file_row(File, OE) -->
	{ file_base_name(File, Name),
	  file_name_extension(_, Ext, Name),
	  file_type_icon(Ext, IconName)
	},
	html(tr(class(OE),
		[ \icon_cell(IconName, '[FILE]'),
		  \name_cell(Name, Name),
		  \modified_cell(File),
		  td(class(size), \size(File))
		])).

icon_cell(IconName, Alt) -->
	{ http_absolute_location(icons(IconName), Icon, [])
	},
	html(td(class(icon), img([src(Icon), alt(Alt)]))).


name_cell(Ref, Name) -->
	html(td(class(name), a(href(Ref), Name))).


modified_cell(Name) -->
	{ time_file(Name, Stamp),
	  format_time(string(Date), '%+', Stamp)
	},
	html(td(class(modified), Date)).

size(Name) -->
	{ size_file(Name, Size)
	},
	html('~D'-[Size]).

%%	file_type_icon(+Extension, -Icon) is det.
%
%	Determine the icon that is used  to   show  a  file of the given
%	extension. This predicate can  be   hooked  using  the multifile
%	http:file_extension_icon/2 hook with the same signature. Icon is
%	the  plain  name  of  an  image    file   that  appears  in  the
%	file-search-path =icons=.

file_type_icon(Ext, Icon) :-
	http:file_extension_icon(Ext, Icon), !.
file_type_icon(_, 'generic.png').

:- multifile
	http:file_extension_icon/2.

http:file_extension_icon(pdf, 'layout.png').
http:file_extension_icon(c,   'c.png').
http:file_extension_icon(gz,  'compressed.png').
http:file_extension_icon(tgz, 'compressed.png').
http:file_extension_icon(zip, 'compressed.png').


		 /*******************************
		 *	      RESOURCES		*
		 *******************************/

:- html_resource(http_dirindex,
		 [ virtual(true),
		   requires([ css('dirindex.css')
			    ])
		 ]).
