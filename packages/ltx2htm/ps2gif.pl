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

:- module(ps2gif,
	  [ ps2gif/2,			% +In, +Out
	    ps2gif/3			% +In, +Out, +Options
	  ]).

option(gs,	gs).
option(res,	72).
option(device,	ppmraw).
option(tmp,	Tmp) :-
	tmp_file(ps2gif, Tmp).

ps2gif(In, Out) :-
	ps2gif(In, Out, []).

ps2gif(In, Out, Options) :-
	get_option(Options, tmp(Tmp)),
	get_option(Options, res(Res0)),
	absolute_file_name(In, [ access(read),
				 extensions([ps, eps]),
				 file_errors(fail)
			       ],
			   InFile),
	get_ps_parameters(InFile, EPS, bb(X1,Y1,X2,Y2)),
	(   get_option(Options, width(W))
	->  ScaleX is W/((X2-X1)/72)
	;   ScaleX is 1
	),
	(   get_option(Options, height(H))
	->  ScaleY is H/((Y2-Y1)/72)
	;   ScaleY is 1
	),
	ResX is Res0 * ScaleX,
	ResY is Res0 * ScaleY,
	(   ResX =:= ResY
	->  Res = ResX
	;   sformat(Res, '~wx~w', [ResX, ResY])
	),
	BBX is -X1,
	BBY is -Y1,
	BBW0 = X2 - X1,
	BBH0 = Y2 - Y1,
	BBW is round(BBW0 * ResX / 72),
	BBH is round(BBH0 * ResY / 72),
	gs_command([size(BBW,BBH),tmp(Tmp),res(Res)|Options], Cmd),
	telling(Old), tell(pipe(Cmd)),
	format('~w ~w translate ', [BBX, BBY]),
	format('(~w) run ', InFile),
	(   EPS == eps
	->  format('showpage ')
	;   true
	),
	format('quit~n'),
	told, tell(Old),
	(   exists_file(Tmp)
	->  ppm2gif(Tmp, Out, Options),
	    delete_file(Tmp)
	;   EPS == ps,
	    format(user_error,
		   'No output from ~w, Trying again with showpage~n',
		   [InFile]),
	    telling(Old), tell(pipe(Cmd)),
	    format('~w ~w translate ', [BBX, BBY]),
	    format('(~w) run ', InFile),
	    format('showpage '),
	    format('quit~n'),
	    told, tell(Old),
	    ppm2gif(Tmp, Out, Options)
	).

ppm2gif(Tmp, Out, Options) :-
	(   get_option(Options, margin(B))
	->  aformat(Cmd,
		    'pnmcrop < ~w | pnmmargin ~w | pnmmargin -black 1 | ppmquant 192 | ppmtogif > ~w',
		    [Tmp, B, Out])
	;   aformat(Cmd, 'pnmcrop < ~w | ppmquant 192 | ppmtogif > ~w',
		    [Tmp, Out])
	),
	shell(Cmd).

gs_command(Options, Cmd) :-
	get_option(Options, gs(GS)),
	get_option(Options, res(Res)),
	get_option(Options, device(Dev)),
	get_option(Options, tmp(Tmp)),
	(   get_option(Options, size(W, H))
	->  sformat(SCmd, '-g~wx~w', [W, H])
	;   SCmd = ''
	),
	aformat(Cmd,
		'~w -q -dNOPAUSE -sDEVICE=~w ~w -r~w -sOutputFile=~w',
		[GS, Dev, SCmd, Res, Tmp]).
	
	
get_option(List, Term) :-
	memberchk(Term, List), !.
get_option(_, Term) :-
	functor(Term, Name, _),
	option(Name, Def), !,
	arg(1, Term, Def).
	
aformat(Atom, Fmt, Args) :-
	sformat(Str, Fmt, Args),
	string_to_atom(Str, Atom).
