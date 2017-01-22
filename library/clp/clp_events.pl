/*  Part of SWI-Prolog

    Author:        Tom Schrijvers
    E-mail:        tom.schrijvers@cs.kuleuven.ac.be
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2005-2011, K.U.Leuven
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
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
