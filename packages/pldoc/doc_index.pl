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

:- module(pldoc_index,
	  [ doc_for_dir/3,		% +Dir, +Out, +Options
	    dir_index/4			% +Dir, +Options, //
	  ]).
:- use_module(process).
:- use_module(html).
:- use_module(wiki).
:- use_module(library('http/html_write')).

/** <module> Create indexes
*/

%%	doc_for_dir(+Dir, +Out, +Options) is det.
%
%	Write documentation for all files in Dir to Out.

doc_for_dir(DirSpec, Out, Options) :-
	absolute_file_name(DirSpec,
			   [ file_type(directory),
			     access(read)
			   ],
			   Dir),
	file_base_name(Dir, Base),
	Title = Base,
	doc_page_dom(Title, \dir_index(Dir, Options), DOM),
	phrase(html(DOM), Tokens),
	print_html_head(Out),
	print_html(Out, Tokens).


%%	dir_index(+Dir, +Options)//
%
%	Create an index for all Prolog files appearing in Dir.
%	
%	@tbd	Include page skeleton with directory description

dir_index(Dir, Options) -->
	{ dir_source_files(Dir, Files, Options),
	  atom_concat(Dir, '/index.html', File),
	  b_setval(pldoc_file, File)	% for predref
	},
	html([ \dir_header(Dir, Options),
	       table(class(summary),
		     \file_indices(Files, [directory(Dir)|Options]))
	     ]).

%%	dir_source_files(+Dir, -Files, +Options) is det
%
%	Create a list of source-files to be documented as part of Dir.

dir_source_files(DirSpec, Files, _Options) :-
	absolute_file_name(DirSpec, Dir,
			   [ file_type(directory),
			     access(read)
			   ]),
	findall(F, source_file_in_dir(Dir, F), Files).

source_file_in_dir(Dir, File) :-
	source_file(File),
	sub_atom(File, 0, _, _, Dir).

%%	dir_header(+Dir, +Options)// is det.
%
%	Create header for directory

dir_header(_Dir, _Options) -->
	[].

%%	file_indices(+Files, +Options)// is det.

file_indices([], _) -->
	[].
file_indices([H|T], Options) -->
	file_index(H, Options),
	file_indices(T, Options).
	
%%	file_index(+File, +Options)// is det.

file_index(File, Options) -->
	{ Pos = File:_Line,
	  findall(doc(Obj,Pos,Summary),
		  doc_comment(Obj, Pos, Summary, _), Objs0),
	  module_info(File, ModuleOptions, Options),
	  doc_hide_private(Objs0, Objs1, ModuleOptions),
	  sort(Objs1, Objs)
	},
	html([ \file_index_header(File, Options)
	     | \object_summaries(Objs, ModuleOptions)
	     ]).

file_index_header(File, Options) -->
	{ (   option(directory(Dir), Options),
	      atom_concat(Dir, Local0, File),
	      atom_concat(/, Local, Local0)
	  ->  true
	  ;   file_base_name(File, Local)
	  )
	},
	html(tr(th([colspan(2), class(file)],
		   [ div(style('float:right'),
			 [ \edit_button(File,
					[ button_height(16)|Options
					])
			 ]),
		     Local
		   ]))).


object_summaries([], _) -->
	[].
object_summaries([H|T], Options) -->
	object_summary(H, Options),
	object_summaries(T, Options).

%%	object_summary(+Object, +Options)// is det
%
%	@tdb	wiki-faces on Summary

object_summary(doc(Obj, _Pos, Summary), _Options) -->
	{ pi(Obj, Name, Arity),
	  wiki_string_to_dom(Summary, [], DOM0),
	  strip_leading_par(DOM0, DOM)
	}, !,
	html(tr([ td(\predref(Name/Arity)),
		  td(class(summary), DOM)
		])).
object_summary(_, _) -->
	[].

pi(_:Name/Arity, Name, Arity) :- !.
pi(Name/Arity, Name, Arity).


	       
	
