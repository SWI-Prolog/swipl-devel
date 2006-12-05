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

:- module(swi_option,
	  [ option/2,			% +Term, +List
	    option/3,			% +Term, +List, +Default
	    select_option/3,		% +Term, +Options, -RestOpts
	    select_option/4		% +Term, +Options, -RestOpts, +Default
	  ]).

%%	option(?Option, +OptionList, +Default)
%
%	Get  an  option  from  a  OptionList.  OptionList  can  use  the
%	Name=Value as well as the Name(Value) convention.
%	
%	@param Option	Term of the form Name(?Value).

option(Opt, Options, Default) :-	% make option processing stead-fast
	arg(1, Opt, OptVal),
	nonvar(OptVal), !,
	functor(Opt, OptName, 1),
	functor(Gen, OptName, 1),
	option(Gen, Options, Default),
	Opt = Gen.
option(Opt, Options, _) :-
	get_option(Opt, Options), !.
option(Opt, _, Default) :-
	arg(1, Opt, Default).

%%	option(?Option, +OptionList)
%
%	Get  an  option  from  a  OptionList.  OptionList  can  use  the
%	Name=Value as well as the Name(Value) convention. Fails silently
%	if the option does not appear in OptionList.
%	
%	@param Option	Term of the form Name(?Value).

option(Opt, Options) :-			% make option processing stead-fast
	arg(1, Opt, OptVal),
	nonvar(OptVal), !,
	functor(Opt, OptName, 1),
	functor(Gen, OptName, 1),
	option(Gen, Options),
	Opt = Gen.
option(Opt, Options) :-
	get_option(Opt, Options), !.


get_option(Opt, Options) :-
	memberchk(Opt, Options), !.
get_option(Opt, Options) :-
	functor(Opt, OptName, 1),
	arg(1, Opt, OptVal),
	memberchk(OptName=OptVal, Options), !.


%%	select_option(?Option, +Options, -RestOptions) is semidet.
%
%	As option/2, removing  the  matching   option  from  Options and
%	unifying the remaining options with RestOptions.

select_option(Opt, Options0, Options) :-	% stead-fast
	arg(1, Opt, OptVal),
	nonvar(OptVal), !,
	functor(Opt, OptName, 1),
	functor(Gen, OptName, 1),
	select_option(Gen, Options0, Options),
	Opt = Gen.
select_option(Opt, Options0, Options) :-
	get_option(Opt, Options0, Options), !.


get_option(Opt, Options0, Options) :-
	select(Opt, Options0, Options), !.
get_option(Opt, Options0, Options) :-
	functor(Opt, OptName, 1),
	arg(1, Opt, OptVal),
	select(OptName=OptVal, Options0, Options), !.

%%	select_option(?Option, +Options, -RestOptions, +Default) is det.
%
%	As select_option/3, but if Option is   not in Options, its value
%	is unified with Default and RestOptions with Options.

select_option(Option, Options, RestOptions, _Default) :-
	select_option(Option, Options, RestOptions), !.
select_option(Option, Options, Options, Default) :-
	arg(1, Option, Default).
