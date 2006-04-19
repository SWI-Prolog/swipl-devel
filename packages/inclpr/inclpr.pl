% Author:	 Leslie De Koninck
% E-mail:	 Leslie.DeKoninck@cs.kuleuven.be
% Copyright (C): 2005, K.U.Leuven

:- module(inclpr,
    [
	change_incremental/1,
	incremental/1,
	change_standard_domain/1,
	standard_domain/1,
	{}/1,
	solve/0,
	get_domain/2
    ]).
:- use_module(inclpr/inclpr_core,
    [
	change_incremental/1,
	incremental/1,
	change_standard_domain/1,
	standard_domain/1,
	{}/1,
	solve/0,
	get_domain/2
    ]).