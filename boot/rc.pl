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

:- module($rc,
	  [ open_resource/3,		% +Name, ?Class, -Stream
	    open_resource/4,		% +Name, ?Class, +RW, -Stream
	    current_resource/3		% :Name, ?Class, ?File
	  ]).

:- dynamic
	user:resource/3.
:- multifile
	user:resource/3.
	
:- module_transparent
	open_resource/3,
	open_resource/4.

%	open_resource(:Name, ?Class, -Handle)

open_resource(Name, Class, Handle) :-
	open_resource(Name, Class, read, Handle).

open_resource(Name, Class, RW, Handle) :-
	$strip_module(Name, Module, RcName),
	(   catch(Module:resource(RcName, Class, FileSpec),
		  error(existence_error(procedure, Module:resource/3), _),
		  user:resource(RcName, Class, FileSpec))
	->  absolute_file_name(FileSpec, File),
	    open(File, RW, Handle, [type(binary)])
	;   $rc_handle(RC),
	    tag_rc_name(Module, RcName, TaggedName),
	    $rc_open(RC, TaggedName, Class, RW, Handle)
	).

tag_rc_name(user, RcName, RcName) :- !.
tag_rc_name(Module, RcName, TaggedName) :-
	concat_atom([Module, ':', RcName], TaggedName).
tag_rc_name(_, RcName, RcName).

%	current_resource(Name, Class, File)
%
%	List all currently defined resources.  Should eventually deal with
%	resources that are already part of the state.

:- module_transparent
	current_resource/3.

current_resource(Name, Class, File) :-
	(   atom(Name)
	;   var(Name)
	), !,
	catch(resource(Name, Class, File), _, fail).
current_resource(M:Name, Class, File) :-
	current_module(M),
	catch(M:resource(Name, Class, File), _, fail).
