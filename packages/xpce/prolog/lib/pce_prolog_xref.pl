/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (C): 2006, University of Amsterdam

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

:- module(pce_prolog_xref,
	  [ xref_source/1,		% +Source
	    xref_called/3,		% ?Source, ?Callable, ?By
	    xref_defined/3,		% ?Source. ?Callable, -How
	    xref_definition_line/2,	% +How, -Line
	    xref_exported/2,		% ?Source, ?Callable
	    xref_module/2,		% ?Source, ?Module
	    xref_op/2,			% ?Source, ?Op
	    xref_clean/1,		% +Source
	    xref_current_source/1,	% ?Source
	    xref_done/2,		% +Source, -Time
	    xref_built_in/1,		% ?Callable
	    xref_expand/2,		% +Term, -Expanded
	    xref_source_file/3,		% +Spec, -Path, +Source
	    xref_public_list/4,		% +Path, -Export, +Src
	    xref_meta/2,		% +Goal, -Called
	    xref_hook/1,		% ?Callable
					% XPCE class references
	    xref_used_class/2,		% ?Source, ?ClassName
	    xref_defined_class/3	% ?Source, ?ClassName, -How
	  ]).
:- use_module(library(pce)).
:- use_module(library(prolog_xref)).

:- multifile
	prolog:xref_source_identifier/2,	% +Source, -Id
	prolog:xref_source_directory/2,		% +Source, -Dir
	prolog:xref_open_source/2.		% +SourceId, -Stream

%	prolog:xref_source_identifier(+Object, -Ref)
%	
%	The  cross-referencer  runs  faster  if   the  reference  is  an
%	indexable term. Therefore we strip the XPCE @ from the object.

prolog:xref_source_identifier(Object, Ref) :-
	object(Object), !,
	Object = @Ref.
prolog:xref_source_identifier(Ref, Ref) :-
	integer(Ref), !.

%	prolog:xref_source_directory(+Source, -Dir)
%	
%	Find the directory of a PceEmacs buffer to resolve relative paths.

prolog:xref_source_directory(SourceId, Dir) :-
	integer(SourceId),
	Obj = @SourceId,
	object(Obj),
	catch(get(Obj?file, absolute_path, Path), _, fail),
	file_directory_name(Path, Dir).

%	prolog:xref_open_source(+Source, -Stream)
%	
%	Open the PceEmacs as a Prolog stream.

prolog:xref_open_source(SourceId, Stream) :-
	integer(SourceId),
	Obj = @SourceId,
	object(Obj),
	pce_open(Obj, read, Stream).
