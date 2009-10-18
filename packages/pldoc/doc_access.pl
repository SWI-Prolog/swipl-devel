/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2008, University of Amsterdam

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

:- module(doc_access,
	  [ host_access_options/2	% +AllOptions, -NoAccessOptions
	  ]).
:- use_module(library(http/http_hook)).
:- use_module(library(http/dcg_basics)).

:- dynamic
	can_edit/1,
	allow_from/1,
	deny_from/1.


%	http:authenticate(+Type, +Request, -Extra) is semidet.
%
%	PlDoc specific access control. The access   control  is based on
%	these options that are passed from starting PlDoc. If PlDoc runs
%	on  top  of   an   external    dispatch   loop,   the  predicate
%	host_access_options/2 can be used to specify access rights.
%
%	    * allow(+IP)
%	    * deny(+IP)
%	    * edit(+Bool)

http:authenticate(pldoc(read), Request, []) :- !,
	memberchk(peer(Peer), Request),
	allowed_peer(Peer).
http:authenticate(pldoc(edit), Request, []) :- !,
	(   can_edit(false)
	->  fail
	;   (   memberchk(x_forwarded_for(Forwarded), Request),
	        primary_forwarded_host(Forwarded, IPAtom),
		parse_ip(IPAtom, Peer)
	    ->  true
	    ;   memberchk(peer(Peer), Request)
	    ),
	    match_peer(localhost, +, Peer)
	).


%%	host_access_options(+AllOptions, -NoAuthOptions) is det.
%
%	Filter the authorization options from   AllOptions,  leaving the
%	remaining options in NoAuthOptions.

host_access_options([], []).
host_access_options([H|T0], T) :-
	host_access_option(H), !,
	host_access_options(T0, T).
host_access_options([H|T0], [H|T]) :-
	host_access_options(T0, T).

host_access_option(allow(From)) :-
	assert(allow_from(From)).
host_access_option(deny(From)) :-
	assert(deny_from(From)).
host_access_option(edit(Bool)) :-
	assert(can_edit(Bool)).

%%	match_peer(:RuleSet, +PlusMin, +Peer) is semidet.
%
%	True if Peer is covered by the   ruleset RuleSet. Peer is a term
%	ip(A,B,C,D). RuleSet is a predicate with   one  argument that is
%	either  a  partial  ip  term,  a    hostname  or  a  domainname.
%	Domainnames start with a '.'.
%
%	@param PlusMin	Positive/negative test.  If IP->Host fails, a
%		       	positive test fails, while a negative succeeds.
%			I.e. deny('.com') succeeds for unknown IP
%			addresses.

match_peer(Spec, _, Peer) :-
	call(Spec, Peer), !.
match_peer(Spec, PM, Peer) :-
	(   call(Spec, HOrDom), atom(HOrDom)
	->  (   catch(tcp_host_to_address(Host, Peer), E, true),
	        var(E)
	    ->	call(Spec, HostOrDomain),
		atom(HostOrDomain),
		(   sub_atom(HostOrDomain, 0, _, _, '.')
		->  sub_atom(Host, _, _, 0, HostOrDomain)
		;   HostOrDomain == Host
		)
	    ;   PM == (+)
	    ->	!, fail
	    ;	true
	    )
	).

%%	allowed_peer(+Peer) is semidet.
%
%	True if Peer is allowed according to the rules.

allowed_peer(Peer) :-
	match_peer(deny_from, -, Peer), !,
	match_peer(allow_from, +, Peer).
allowed_peer(Peer) :-
	allow_from(_), !,
	match_peer(allow_from, +, Peer).
allowed_peer(_).


:- dynamic
	can_edit/1.

%%	primary_forwarded_host(+Spec, -Host) is det.
%
%	x_forwarded host contains multiple hosts seperated   by  ', ' if
%	there are multiple proxy servers in   between.  The first one is
%	the one the user's browser knows about.

primary_forwarded_host(Spec, Host) :-
	sub_atom(Spec, B, _, _, ','), !,
	sub_atom(Spec, 0, B, _, Host).
primary_forwarded_host(Host, Host).


localhost(ip(127,0,0,1)).
localhost(localhost).

parse_ip(Atom, IP) :-
	atom_codes(Atom, Codes),
	phrase(ip(IP), Codes).

%%	ip(?IP)// is semidet.
%
%	Parses A.B.C.D into ip(A,B,C,D)

ip(ip(A,B,C,D)) -->
	integer(A), ".", integer(B), ".", integer(C), ".", integer(D).

