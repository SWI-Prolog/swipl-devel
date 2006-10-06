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

:- module(pldoc_files,
	  [ doc_save/2			% +File, +Options
	  ]).
:- use_module(doc_html).
:- use_module(doc_index).
:- use_module(library(option)).
:- use_module(library(lists)).

/** <module> Create stand-alone documentation files

Create stand-alone documentation from a  bundle of source-files. Typical
use of the PlDoc package is to run   it as a web-server from the project
in progress, providing search and guaranteed consistency with the loaded
version. Creating stand-alone files as  provided   by  this  file can be
useful for printing or distribution.

@tbd	Handle CSS files
@tbd	Suppress search header
@tbd	Fix filenames (.pl --> .html)
@tdb	Fix predicate references
*/

%%	doc_save(+FileOrDir, +Options)
%
%	Save documentation for FileOrDir to file(s).  Options include
%	
%		* format(+Format)
%		Currently only supports =html=.
%		
%		* directory(+Dir)
%		Save output to the given directory.  Default is to save
%		the documentation files in the same directory as the
%		sources.
%		
%		* index_file(+Base)
%		Filename for directory indices.  Default is =index=.
%		
%		* if(Condition)
%		What to do with files in a directory.  =loaded= (default)
%		only documents files loaded into the Prolog image.  =true=
%		documents all files.
%		
%		* recursive(+Bool)
%		If =true=, recurse into subdirectories.

doc_save(FileOrDir, Options) :-
	absolute_file_name(FileOrDir, File,
			   [ file_type(prolog),
			     file_errors(fail),
			     access(read)
			   ]), !,
	(   document_file(File, DocFile, Options)
	->  open(DocFile, write, Out, [encoding(utf8)]),
	    doc_for_file(File, Out, Options)
	;   true
	).
doc_save(FileOrDir, Options) :-
	absolute_file_name(FileOrDir, Dir,
			   [ file_type(directory),
			     file_errors(fail),
			     access(read)
			   ]), !,
	(   document_file(Dir, DocFile, Options)
	->  open(DocFile, write, Out, [encoding(utf8)]),
	    doc_for_dir(Dir, Out, Options),
	    forall(prolog_file_in_dir(Dir, File, Options),
		   doc_save(File, Options))
	;   true
	).

	
			   
%%	document_file(+File, -DocFile, +Options) is semidet.
%
%	DocFile is the file into which to write the documentation for
%	File.

document_file(File, DocFile, Options) :-
	(   option(if(loaded), Options, loaded)
	->  (   source_file(File)
	    ->	true
	    ;	exists_directory(File),
		source_file(SrcFile),
		sub_atom(SrcFile, 0, _, _, File)
	    ->	true
	    )
	;   true
	),
	option(format(Format), Options, html),
	doc_extension(Format, Ext),
	(   exists_directory(File)
	->  option(index_file(Index), Options, index),
	    concat_atom([File, /, Index, '.', Ext], DocFile0)
	;   file_name_extension(Base, _, File),
	    file_name_extension(Base, Ext, DocFile0)
	),
	(   option(directory(Dir0), Options),
	    ensure_slash(Dir0, Dir)
	->  working_directory(PWD, PWD),
	    atom_concat(PWD, Local, DocFile0),
	    atom_concat(Dir, Local, DocFile),
	    file_directory_name(DocFile, DocDir),
	    ensure_dir(DocDir, Options)
	;   DocFile = DocFile0
	).


%%	doc_extension(+Format, -Extension) is det.

doc_extension(html, html).
doc_extension(latex, tex).


%%	ensure_slash(+DirName, -WithSlash) is det.
%
%	Ensure WithSlash ends with a /.

ensure_slash(DirName, WithSlash) :-
	(   sub_atom(DirName, _, _, 0, /)
	->  WithSlash = DirName
	;   atom_concat(DirName, /, WithSlash)
	).

			
%%	ensure_dir(+Directory, +Options) is det.
%
%	Create Directory as mkdir -p.  May generate file errors.

ensure_dir(Directory, _Options) :-
	exists_directory(Directory), !.
ensure_dir(Directory, Options) :-
	file_directory_name(Directory, Parent),
	Parent \== Directory,
	ensure_dir(Parent, Options),
	make_directory(Directory).


%%	prolog_file_in_dir(+Dir, -File, +Options) is nondet.
%
%	File is a file in Dir that must be documented.  Options:
%	
%		* recursive(+Bool)
%		If =true=, also generate subdirectories

prolog_file_in_dir(Dir, File, Options) :-
	(   option(if(loaded), Options, loaded)
	->  source_file(File),
	    file_directory_name(File, Dir)
	;   prolog_file_type(Ext, prolog),
	    concat_atom([Dir, '/*.', Ext], Pattern),
	    expand_file_name(Pattern, Files),
	    member(File, Files)
	),
	file_base_name(File, Base),
	\+ blocked(Base).
prolog_file_in_dir(Dir, SubDir, Options) :-
	option(recursive(true), Options, false),
	atom_concat(Dir, '/*', Pattern),
	expand_file_name(Pattern, Matches),
	member(SubDir, Matches),
	exists_directory(SubDir).
	    
%%	blocked(+File) is semidet.
%
%	True if File is blocked from documentation.

blocked('.plrc').
blocked('INDEX.pl').
