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


:- module(pce_manual,
	  [ manpce/0,
	    manpce/1
	  ]).
:- use_module(library(pce)).
:- consult(
	[ 'man/util'			% Common utilities
	, 'man/p_card'			% General card infra-structure
	, 'man/p_data'			% Manual specific infra-structure
	, 'man/v_manual'		% Top level window
	]).
:- require([ pce_warn/1
	   , pce_to_method/2
	   ]).

:- pce_autoload(man_class_browser,	library('man/v_class')).
:- pce_autoload(man_editor,		library('man/v_editor')).
:- pce_autoload(man_card_editor,	library('man/v_card')).
:- pce_autoload(man_summary_browser,	library('man/v_summary')).
:- pce_autoload(man_class_hierarchy,	library('man/v_hierarchy')).
:- pce_autoload(man_search_tool,	library('man/v_search')).
:- pce_autoload(man_index_manager,	library('man/man_index')).
:- pce_autoload(man_topic_browser,	library('man/v_topic')).
:- pce_autoload(man_module_browser,	library('man/v_module')).
:- pce_autoload(man_statistics,		library('man/v_statistics')).
:- pce_autoload(isp_frame,		library('man/v_inspector')).
:- pce_autoload(vis_frame,		library('man/v_visual')).
:- pce_autoload(man_instance_browser,	library('man/v_instance')).
:- pce_autoload(man_global,		library('man/v_global')).
:- pce_autoload(man_object_browser,	library('man/v_global')).
:- pce_autoload(man_error_browser,	library('man/v_error')).
:- pce_autoload(man_group_browser,	library('man/v_group')).

:- pce_global(@manual, new(man_manual)).

manpce :-
	send(@manual, expose).


manpce(Spec) :-
	(   method(Spec, Object)
	->  send(@manual, manual, Object)
	;   pce_warn(pce(no_help(Spec))),
	    fail
	).

method(Spec, Method) :-
	object(Spec),
	send(Spec, '_instance_of', var), !,
	Spec = @Ref,
	new(Method, man_global(Ref)).
method(Spec, Method) :-
	pce_to_method(Spec, Method), !.
method(Atom, Method) :-
	atom(Atom),
	catch(term_to_atom(Spec, Atom), _, fail),
	pce_to_method(Spec, Method).



