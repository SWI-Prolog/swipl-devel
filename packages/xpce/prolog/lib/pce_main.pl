/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1995 University of Amsterdam. All rights reserved.
*/

:- module(pce_main,
	  [ pce_loop/2,
	    pce_loop/1,
	    pce_main_loop/1
	  ]).

:- meta_predicate
	pce_loop(:),
	pce_loop(:, +),
	pce_main_loop(:).

:- use_module(library(pce)).	    
:- require([ append/3
	   , apply/2
	   , chain_list/2
	   , ignore/1
	   , unix/1
	   ]).

%	pce_main_loop(+Goal)
%	
%	Simple XPCE runtime toplevel loop.  This goal extracts the command
%	line arguments, calls `apply(Goal, CmdLineArgs)' and waits for all
%	frames created by this call to be invisible.  Then it will halt/0.

pce_main_loop(Goal) :-
	unix(argv(Argv)),
	application_flags(Argv, ApplArgv),
	pce_loop(Goal, ApplArgv),
	halt.

%	pce_loop(+Goal, [+ArgList])
%
%	Runs `Goal', finds all toplevel frames created and then dispatches
%	events untill the last frame is destroyed.

pce_loop(Goal) :-
	pce_loop(Goal, []).
pce_loop(Goal, Argv) :-
	get(@display?frames, find_all, @arg1?kind == toplevel, FramesOld),
	call(Goal, Argv),
	get(@display?frames, find_all, @arg1?kind == toplevel, FramesNew),
	get(FramesNew, copy, FrameChain),
	send(FrameChain, subtract, FramesOld),
	chain_list(FrameChain, Frames),
	dispatch_till_all_gone(Frames).

application_flags(Argv, Appl) :-
	append(_, ['--'|Appl], Argv), !.
application_flags(Argv, Appl) :-
	append(_, ['-x', _State|Appl], Argv), !.
application_flags([_|Appl], Appl).


dispatch_till_all_gone([]) :- !.
dispatch_till_all_gone(Frames) :-
	ignore(send(@display, dispatch)),
	existing_frames(Frames, Existing),
	dispatch_till_all_gone(Existing).

existing_frames([], []).
existing_frames([H|T0], [H|T]) :-
	object(H),
	send(H, instance_of, frame),
	get(H, status, Status),
	Status \== unmapped, !,
	existing_frames(T0, T).
existing_frames([_|T0], T) :-
	existing_frames(T0, T).
