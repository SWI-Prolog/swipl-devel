/*  $Id$

    Designed and implemented by Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1996 University of Amsterdam. All rights reserved.
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
				 extensions([ps, eps])
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
		    'pnmcrop < ~w | pnmmargin ~w | pnmmargin -black 1 | ppmtogif > ~w',
		    [Tmp, B, Out])
	;   aformat(Cmd, '~w < ~w | ~w > ~w',
		    [pnmcrop, Tmp, ppmtogif, Out])
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
