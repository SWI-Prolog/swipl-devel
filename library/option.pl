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
	    select_option/4,		% +Term, +Options, -RestOpts, +Default
	    merge_options/3,		% +New, +Old, -Merged
	    meta_options/3		% :IsMeta, :OptionsIn, -OptionsOut
	  ]).
:- use_module(library(lists)).

/** <module> Option list processing

The library(option) provides some utilities for processing option lists.
Option lists are commonly used  as   an  alternative for many arguments.
Examples built-in predicates are  open/4   and  write_term/3. Naming the
arguments results in more readable code  and   the  list nature makes it
easy to extend the list of options accepted by a predicate. Option lists
come in two styles, both of which are handled by this library.

	$ Name(Value) :
	This is the preferred style.

	$ Name = Value :
	This is often used, but deprecated.

Processing options inside time critical code   (loops) can cause serious
overhead. One possibility is to define   a  record using library(record)
and initialise this using make_<record>/2. In addition to providing good
performance, this also provides type-checking and central declaration of
defaults.

==
:- record atts(width:integer=100, shape:oneof([box,circle])=box).

process(Data, Options) :-
	make_atts(Options, Attributes),
	action(Data, Attributes).

action(Data, Attributes) :-
	atts_shape(Attributes, Shape),
	...
==

@tbd	We should consider putting many options in an assoc or record
	with appropriate preprocessing to achieve better performance.
@tbd	We should provide some standard to to automatic type-checking
	on option lists.
@see	library(record)
*/

%%	option(?Option, +OptionList, +Default)
%
%	Get  an  option  from  a  OptionList.  OptionList  can  use  the
%	Name=Value as well as the Name(Value) convention.
%	
%	@param Option	Term of the form Name(?Value).

option(Opt, Options, Default) :-	% make option processing stead-fast
	arg(1, Opt, OptVal),
	ground(OptVal), !,
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
%	Get and remove option from an option list. As option/2, removing
%	the matching option from  Options   and  unifying  the remaining
%	options with RestOptions.

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
%	Get and remove option with   default  value. As select_option/3,
%	but if Option is not  in  Options,   its  value  is unified with
%	Default and RestOptions with Options.

select_option(Option, Options, RestOptions, _Default) :-
	select_option(Option, Options, RestOptions), !.
select_option(Option, Options, Options, Default) :-
	arg(1, Option, Default).


%%	merge_options(+New, +Old, -Merged) is det.
%
%	Merge two option lists. Merged is a sorted list of options using
%	the canonical format Name(Value) holding   all  options from New
%	and Old, after removing conflicting options from Old.

merge_options(New, Old, Merged) :-
	canonise_options(New, NCanonical),
	canonise_options(Old, OCanonical),
	sort(NCanonical, NSorted),
	sort(OCanonical, OSorted),
	ord_merge(NSorted, OSorted, Merged).

ord_merge([], L, L) :- !.
ord_merge(L, [], L) :- !.
ord_merge([NO|TN], [OO|TO], Merged) :-
	functor(NO, NName, 1),
	functor(OO, OName, 1),
	compare(Diff, NName, OName),
	ord_merge(Diff, NO, NName, OO, OName, TN, TO, Merged).

ord_merge(=, NO, _, _, _, TN, TO, [NO|T]) :-
	ord_merge(TN, TO, T).
ord_merge(<, NO, _, OO, OName, TN, TO, [NO|T]) :-
	(   TN = [H|TN2]
	->  functor(H, NName, 1),
	    compare(Diff, NName, OName),
	    ord_merge(Diff, H, NName, OO, OName, TN2, TO, T)
	;   T = [OO|TO]
	).
ord_merge(>, NO, NName, OO, _, TN, TO, [OO|T]) :-
	(   TO = [H|TO2]
	->  functor(H, OName, 1),
	    compare(Diff, NName, OName),
	    ord_merge(Diff, NO, NName, H, OName, TN, TO2, T)
	;   T = [NO|TN]
	).


%%	canonise_options(+OptionsIn, -OptionsOut) is det.
%
%	Rewrite option list from possible Name=Value to Name(Value)

canonise_options(In, Out) :-
	memberchk(_=_, In), !,		% speedup a bit if already ok.
	canonise_options2(In, Out).
canonise_options(Options, Options).

canonise_options2([], []).
canonise_options2([Name=Value|T0], [H|T]) :- !,
	H =.. [Name,Value],
	canonise_options2(T0, T).
canonise_options2([H|T0], [H|T]) :- !,
	canonise_options2(T0, T).


%%	meta_options(+IsMeta, :Options0, -Options) is det.
%
%	Perform meta-expansion on options that are module-sensitive.
%	Whether an option name is module sensitive is determined by
%	calling call(IsMeta, Name).  Here is an example:
%	
%	==
%		meta_options(is_meta, OptionsIn, Options),
%		...
%	
%	is_meta(callback).
%	==

:- module_transparent
	meta_options/3.

meta_options(IsMeta, Options0, Options) :-
	strip_module(IsMeta, IMC, IM),
	strip_module(Options0, Context, Options1),
	meta_options(Options1, IMC:IM, Context, Options).

meta_options([], _, _, []).
meta_options([H0|T0], IM, Context, [H|T]) :-
	meta_option(H0, IM, Context, H),
	meta_options(T0, IM, Context, T).

meta_option(Name=V0, IM, Context, Name=M:V) :-
	call(IM, Name), !,
	strip_module(Context:V0, M, V).
meta_option(O0, IM, Context, O) :-
	compound(O0),
	O0 =.. [Name,V0],
	call(IM, Name), !,
	strip_module(Context:V0, M, V),
	O =.. [Name,M:V].
meta_option(O, _, _, O).

