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


:- module(pce_autoload,
	[ pce_autoload/2
	, pce_autoload_all/0
	]).

:- use_module(pce_boot(pce_principal),
	      [ get/3,
		send/2
	      ]).
:- use_module(pce_boot(pce_realise),
	      [ pce_realise_class/1,
		pce_prolog_class/1
	      ]).
:- require([ is_absolute_file_name/1
	   , concat_atom/2
	   , absolute_file_name/3
	   , get/3
	   ]).

:- dynamic
	autoload/2.

%	pce_autoload(+ClassName, +FileSpec)
%
%	States class `ClassName' can be created by loading the Prolog
%	file `FileSpec'.  This will actually be done if either the class
%	is actually needed by PCE or pce_autoload_all/0 is called.

pce_autoload(Class, PathAlias) :-	% trap library(), demo(), contrib(), ..
	functor(PathAlias, _, 1), !,
	retractall(autoload(Class, _)),
	assert(autoload(Class, PathAlias)).
pce_autoload(Class, Abs) :-
	is_absolute_file_name(Abs), !,
	absolute_file_name(Abs, Canonical),
	retractall(autoload(Class, _)),
	assert(autoload(Class, Canonical)).
pce_autoload(Class, Local) :-
	prolog_load_context(directory, Dir),
	concat_atom([Dir, /, Local], File),
	pce_host:property(file_extensions(Exts)),
	absolute_file_name(File,
			   [ extensions(Exts),
			     access(exist)
			   ], Abs),
	retractall(autoload(Class, _)),
	assert(autoload(Class, Abs)).

%	pce_autoload_all/0
%
%	Load all    classes  declared  using   the    pce_autoload/2
%	directive.  Useful for debugging purposes.

pce_autoload_all :-
	autoload(Class, File),
	\+ get(@classes, member, Class, _),
	\+ pce_prolog_class(Class),
	ensure_loaded(user:File),
	fail.
pce_autoload_all.


register_handler :-
   send(@pce?exception_handlers, append(
	attribute(undefined_class,
		  message(@prolog, call, trap_autoload, @arg1)))).

:- initialization
	register_handler.

pce_ifhostproperty(prolog(swi),
		   (:- '$hide'(trap_autoload, 1)),
		   (notrace(G) :- G)).

trap_autoload(Class) :-
	notrace(do_trap_autoload(Class)).

do_trap_autoload(Class) :-
	pce_realise_class(Class), !.
do_trap_autoload(Class) :-
	autoload(Class, File),
	load_files(user:File,
		   [ autoload(true)
		   ]),
	pce_realise_class(Class).
