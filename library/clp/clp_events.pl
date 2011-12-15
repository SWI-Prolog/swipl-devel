/*  $Id$

    Part of SWI-Prolog

    Author:        Tom Schrijvers
    E-mail:        tom.schrijvers@cs.kuleuven.ac.be
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2005, K.U.Leuven

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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Module for managing constraint solver events.
%
% Author: 	Tom Schrijvers
% E-mail: 	tom.schrijvers@cs.kuleuven.ac.be
% Copyright:	2005, K.U.Leuven
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:-module(clp_events,
	[
		notify/2,
		subscribe/4,
		unsubscribe/2
	]).

notify(V,NMod) :-
	( get_attr(V,clp_events,List) ->
		notify_list(List,NMod)
	;
		true
	).

subscribe(V,NMod,SMod,Goal) :-
	( get_attr(V,clp_events,List) ->
		put_attr(V,clp_events,[entry(NMod,SMod,Goal)|List])
	;
	        put_attr(V,clp_events,[entry(NMod,SMod,Goal)])
	).

unsubscribe(V,SMod) :-
	( get_attr(V,clp_events,List) ->
		unsubscribe_list(List,SMod,NList),
		put_attr(V,clp_events,NList)
	;
		true
	).

notify_list([],_).
notify_list([entry(Mod,_,Goal)|Rest],NMod) :-
	( Mod == NMod ->
		call(Goal)
	;
		true
	),
	notify_list(Rest,NMod).

unsubscribe_list([],_,_).
unsubscribe_list([Entry|Rest],SMod,List) :-
	Entry = entry(_,Mod,_),
	( Mod == SMod ->
		List = Rest
	;
		List = [Entry|Tail],
		unsubscribe_list(Rest,SMod,Tail)
	).

attr_unify_hook(_,_).
