%
% %Z% %M% %I% %E% %Q%
%
% Copyright (C) 1992, 1993 by Christian Schlichtherle.
%
% For distribution conditions please read the
% GNU General Public License version 2.
%
%
% Rubik's Cube:
%
% The algorithm of this program is based on Josef Trajber's book
% 'Der Wuerfel (Rubiks Cube); Loesungswege; math. Grundlagen; Varianten fuer
% Supertueftler', Falken-Verlag, 1981.
%
% There are two different data structures handled by this program:
% 1. The cube is represented as a prolog term
%
%      cube(+Field1, ...+Field54)
%
%    where FieldX is the X-th field on the cube.
%      The ordinate X of a field can easily be seen when you take a cube and
%    fold it out into a 2-dimensional cross. The fields on this cross are
%    numbered in canonical order, i.e. from left to right, top down.
%      Any full instantiated term can be used as a value of a field since the
%    terms are only used to compare them to the terms of the centerstones.
% 2. Draws to the cube are notated as a Prolog term like this:
%
%      S/N
%    where S is one of:
%
%      l: left side.
%      f: front side.
%      r: right side.
%      b: back side.
%      u: upside.
%      d: downside.
%
%    and N is the number of clockwise 90 degree turns applying to this side:
%
%      -1: one turn counterclockwise (i.e. three clockwise turns).
%       0: no op.
%       1: one turn.
%       2: two turns (either directions).
%
%    To know how a sidename is attached to a side of the cube you should know
%    that the heart of the cross is called the front side. All other sidenames
%    are then obvious.
%
% INTERNALS:
%
% Important note: Rotating the cube clockwise around the front surface
% (turn_cube/3 does this for you) is a permutation of the sides adjacent to the
% front side. The permutation is (l u r d) (<-- This is the cyclic notation of
% the permutation).
% Thus, if you have a list of draws applying to a one times 90 degree clockwise
% turned cube and you want to know which draws would do the same job on the
% unturned cube, take the side of the cube to turn from each draw and apply
% the permutation backwards! The number and direction of each turn stays the
% same!
% Thus, the draw list [r/1,f/ -1,l/2] becomes [u/1,f/ -1,d/2]
% (f and b stay the same because they are not permuted and are thus not
% mentioned in the cycle).
% In general: If the cube was turned n times,
% you have to apply the permutation n times backwards.
% Thus, if the cube was three times turned clockwise,
% the draw r/ -1 becomes d/ -1.
% Note that this is the same as if the cube has been turned counterclockwise
% one time (thus applying the permutation one times forward).
%

:-module(rubik,[ordered/1,
		ordered/7,
		adjust/2,
		draw/3,
		draws/3,
		opt_draws/2,
		perm_turn_cube/2,
		turn_cube/2,
		perm_tilt_cube/2,
		tilt_cube/2,
		random_draws/1,
		disordered/1,
		portray/1]).
:-use_module(library(pce)).
:-use_module(maplist).
:- require([ append/3
	   , member/2
	   , random/3
	   ]).

:- op(0, fx, l).

% This is for printing Rubik's Cube:
% I splitted the predicate up so SB-Prolog can work with it.
portray([]):-!.
portray([H|T]):-
	portray(H),write(','),portray(T).
portray(S/N):-
	portray(S),write('/'),portray(N).
portray(
    cube(U1,U2,U3,
         U4,U5,U6,
         U7,U8,U9,
L1,L2,L3,F1,F2,F3,R1,R2,R3,
L4,L5,L6,F4,F5,F6,R4,R5,R6,
L7,L8,L9,F7,F8,F9,R7,R8,R9,
         D1,D2,D3,
         D4,D5,D6,
         D7,D8,D9,
         B1,B2,B3,
         B4,B5,B6,
         B7,B8,B9)
  ):-
	!,
	portray(cube_up(U1,U2,U3,
			U4,U5,U6,
			U7,U8,U9)),
	portray(cube_mid(L1,L2,L3,F1,F2,F3,R1,R2,R3,
			 L4,L5,L6,F4,F5,F6,R4,R5,R6,
			 L7,L8,L9,F7,F8,F9,R7,R8,R9)),
	portray(cube_down(D1,D2,D3,
			  D4,D5,D6,
			  D7,D8,D9,
			  B1,B2,B3,
			  B4,B5,B6,
			  B7,B8,B9)).
portray(
	cube_up(U1,U2,U3,
		U4,U5,U6,
		U7,U8,U9)
	):-
	tab(6),portray(U1),write(' '),portray(U2),write(' '),portray(U3),nl,
	tab(6),portray(U4),write(' '),portray(U5),write(' '),portray(U6),nl,
	tab(6),portray(U7),write(' '),portray(U8),write(' '),portray(U9),nl.
portray(
      cube_mid(L1,L2,L3,F1,F2,F3,R1,R2,R3,
	       L4,L5,L6,F4,F5,F6,R4,R5,R6,
	       L7,L8,L9,F7,F8,F9,R7,R8,R9)
      ):-
	tab(0),portray(L1),write(' '),portray(L2),write(' '),portray(L3),write(' '),
	portray(F1),write(' '),portray(F2),write(' '),portray(F3),write(' '),
	portray(R1),write(' '),portray(R2),write(' '),portray(R3),nl,
	tab(0),portray(L4),write(' '),portray(L5),write(' '),portray(L6),write(' '),
	portray(F4),write(' '),portray(F5),write(' '),portray(F6),write(' '),
	portray(R4),write(' '),portray(R5),write(' '),portray(R6),nl,
	tab(0),portray(L7),write(' '),portray(L8),write(' '),portray(L9),write(' '),
	portray(F7),write(' '),portray(F8),write(' '),portray(F9),write(' '),
	portray(R7),write(' '),portray(R8),write(' '),portray(R9),nl.
portray(
	cube_down(D1,D2,D3,
		  D4,D5,D6,
		  D7,D8,D9,
		  B1,B2,B3,
		  B4,B5,B6,
		  B7,B8,B9)
	):-
	tab(6),portray(D1),write(' '),portray(D2),write(' '),portray(D3),nl,
	tab(6),portray(D4),write(' '),portray(D5),write(' '),portray(D6),nl,
	tab(6),portray(D7),write(' '),portray(D8),write(' '),portray(D9),nl,
	tab(6),portray(B1),write(' '),portray(B2),write(' '),portray(B3),nl,
	tab(6),portray(B4),write(' '),portray(B5),write(' '),portray(B6),nl,
	tab(6),portray(B7),write(' '),portray(B8),write(' '),portray(B9),nl.
portray(X):-atomic(X),!,write(X).

% This will assign the symbols to the cube's fields:
ordered(C):-
	ordered('B','R','G','O','W','Y',C).

% This is the cube in ordered state:
ordered(U,L,F,R,D,B,
 cube(U,U,U,
      U,U,U,
      U,U,U,
L,L,L,F,F,F,R,R,R,
L,L,L,F,F,F,R,R,R,
L,L,L,F,F,F,R,R,R,
      D,D,D,
      D,D,D,
      D,D,D,
      B,B,B,
      B,B,B,
      B,B,B)
  ).

% Adjust Rubik's Cube.
adjust(C1,L6):-
	adjust1(C1,L1,C2),
	adjust2(C2,L2,C3),
	adjust3(C3,L3,_),
	append(L2,L3,L4),
	append(L1,L4,L5),
	opt_draws(L5,L6).

%
% The adjustXXX/3 predicates have the general form
%   adjustXXX(+CubeBefore,-DrawList,-CubeAfter)
% with:
% CubeBefore : The disordered cube before adjustment.
% DrawList   : The list of draws to adjust the cube.
% CubeAfter  : The cube after adjustment.
%
% Some of these predicates have only two arguments (CubeBefore and DrawList).
% In this case CubeAfter is always the fully ordered cube.
%

% Adjust the first layer. Assume nothing about the look of the cube.
adjust1(C1,L3,C3):-
	adjust1sides(C1,L1,C2),
	adjust1corners(C2,L2,C3),
	append(L1,L2,L3).

%
% Adjust the side stones of the first layer
% (i.e. the front surface) of the cube.
% Assume nothing about the look of the cube.
%

adjust1sides(C,[],C):-
C = cube( _, _, _,
          _, U, _,
          _, U, _,
 _, _, _, _, F, _, _, _, _,
 _, L, L, F, F, F, R, R, _,
 _, _, _, _, F, _, _, _, _,
          _, D, _,
          _, D, _,
          _, _, _,
          _, _, _,
          _, _, _,
          _, _, _).
adjust1sides(C1,L3,C3):-
	meta_match_pattern1sides(C1,L1,C2),
	adjust1sides(C2,L2,C3),
	append(L1,L2,L3).

% First try only constructive draws...
meta_match_pattern1sides(C1,L2,C3):-
	turn_cube(C1,N,C2),
	match_cst_pattern1sides(C2,L1),
 n_maplist(N,perm_turn_cube,L2,L1),
	draws(C1,L2,C3).
% then try only destructive draws.
meta_match_pattern1sides(C1,L2,C3):-
	turn_cube(C1,N,C2),
	match_dst_pattern1sides(C2,L1),
 n_maplist(N,perm_turn_cube,L2,L1),
	draws(C1,L2,C3).

% First try only constructive moves...
match_cst_pattern1sides(
    cube( _, _, _,
          F, _, _,
          _, _, _,
 _, L, _, _, _, _, _, _, _,
 _, L, _, _, F, _, _, _, _,
 _, _, _, _, _, _, _, _, _,
          _, _, _,
          _, _, _,
          _, _, _,
          _, _, _,
          _, _, _,
          _, _, _),
	  [l/1]
      ).
match_cst_pattern1sides(
    cube( _, _, _,
          _, _, F,
          _, _, _,
 _, _, _, _, _, _, _, R, _,
 _, _, _, _, F, _, _, R, _,
 _, _, _, _, _, _, _, _, _,
          _, _, _,
          _, _, _,
          _, _, _,
          _, _, _,
          _, _, _,
          _, _, _),
	  [r/ -1]
      ).
match_cst_pattern1sides(
    cube( _, _, _,
          F, _, _,
          _, _, _,
 _, R, _, _, _, _, _, _, _,
 _, _, _, _, F, _, _, R, _,
 _, _, _, _, _, _, _, _, _,
          _, _, _,
          _, _, _,
          _, _, _,
          _, _, _,
          _, _, _,
          _, _, _),
	  [f/2,l/1,f/2]
      ).
match_cst_pattern1sides(
    cube( _, _, _,
          _, _, F,
          _, _, _,
 _, _, _, _, _, _, _, L, _,
 _, L, _, _, F, _, _, _, _,
 _, _, _, _, _, _, _, _, _,
          _, _, _,
          _, _, _,
          _, _, _,
          _, _, _,
          _, _, _,
          _, _, _),
	  [f/2,r/ -1,f/2]
      ).
match_cst_pattern1sides(C1,[b/Turns|DrawList]):-
	draw(C1,b/Turns,C2),
	match_pattern1sides_turned_backside(C2,DrawList).

% if no constructive move matches, then try destructive moves.
match_dst_pattern1sides(
    cube( _, _, _,
          _, _, _,
          _, _, _,
 _, _, _, _, _, _, _, _, _,
 _, _, _, _, F, _, F, _, _,
 _, _, _, _, _, _, _, _, _,
          _, _, _,
          _, _, _,
          _, _, _,
          _, _, _,
          _, _, _,
          _, _, _),
	  [r/2]
      ).
match_dst_pattern1sides(
    cube( _, _, _,
          _, _, _,
          _, _, _,
 _, _, _, _, _, _, _, _, _,
 _, _, _, _, F, F, X, R, _,
 _, _, _, _, _, _, _, _, _,
          _, _, _,
          _, _, _,
          _, _, _,
          _, _, _,
          _, _, _,
          _, _, _),
	  [r/2]
      ):-
	X\==R.
match_dst_pattern1sides(
    cube( _, _, _,
          F, _, _,
          _, _, _,
 _, X, _, _, _, _, _, _, _,
 _, L, _, _, F, _, _, _, _,
 _, _, _, _, _, _, _, _, _,
          _, _, _,
          _, _, _,
          _, _, _,
          _, _, _,
          _, _, _,
          _, _, _),
	  [l/ -1,b/1,l/1]
      ):-
	X\==L.
match_dst_pattern1sides(
    cube( _, _, _,
          _, _, F,
          _, _, _,
 _, _, _, _, _, _, _, X, _,
 _, _, _, _, F, _, _, R, _,
 _, _, _, _, _, _, _, _, _,
          _, _, _,
          _, _, _,
          _, _, _,
          _, _, _,
          _, _, _,
          _, _, _),
	  [r/1,b/1,r/ -1]
      ):-
	X\==R.

% Draws with the backside rotated: All of these are constructive.
match_pattern1sides_turned_backside(
    cube( _, _, _,
          _, _, _,
          _, _, _,
 _, _, _, _, _, _, _, _, _,
 _, _, _, _, F, _, _, _, _,
 _, _, _, _, _, _, _, _, _,
          _, _, _,
          _, D, _,
          _, D, _,
          _, F, _,
          _, _, _,
          _, _, _),
	  [d/2]
      ).
match_pattern1sides_turned_backside(
    cube( _, _, _,
          _, _, _,
          _, _, _,
 _, _, _, _, _, _, _, _, _,
 _, _, _, _, F, _, _, _, F,
 _, _, _, _, _, _, _, _, _,
          _, _, _,
          _, D, _,
          _, _, _,
          _, _, _,
          _, _, D,
          _, _, _),
	  [r/1,d/ -1,r/ -1]
      ).
match_pattern1sides_turned_backside(
    cube( _, _, _,
          _, _, _,
          _, _, _,
 _, _, _, _, _, _, _, _, _,
 F, _, _, _, F, _, _, _, _,
 _, _, _, _, _, _, _, _, _,
          _, _, _,
          _, D, _,
          _, _, _,
          _, _, _,
          D, _, _,
          _, _, _),
	  [l/ -1,d/1,l/1]
      ).

%
% Adjust the corner stones of the first layer.
% Assume that the side stones of the first layer are already adjusted.
%

% The corner stones are correct in this case:
adjust1corners(C,[],C):-
C = cube( _, _, _,
          _, U, _,
          U, _, U,
 _, _, L, _, _, _, R, _, _,
 _, L, _, _, _, _, _, R, _,
 _, _, L, _, _, _, R, _, _,
          D, _, D,
          _, D, _,
          _, _, _,
          _, _, _,
          _, _, _,
          _, _, _).
adjust1corners(C1,L3,C3):-
	meta_match_pattern1corners(C1,L1,C2),
	adjust1corners(C2,L2,C3),
	append(L1,L2,L3).

% First try only constructive draws...
meta_match_pattern1corners(C1,L4,C4):-
	turn_cube(C1,N,C2),
	draw(C2,b/Turns,C3),
	match_cst_pattern1corners(C3,DrawList),
 n_maplist(N,perm_turn_cube,L4,[b/Turns|DrawList]),
	draws(C1,L4,C4).
% then try only destructive draws.
meta_match_pattern1corners(C1,L4,C4):-
	turn_cube(C1,N,C2),
	draw(C2,b/Turns,C3),
	match_dst_pattern1corners(C3,DrawList),
 n_maplist(N,perm_turn_cube,L4,[b/Turns|DrawList]),
	draws(C1,L4,C4).

% These are constructive draws.
match_cst_pattern1corners(
    cube( _, _, _,
          _, _, _,
          _, _, _,
 _, _, _, _, _, _, _, _, _,
 _, _, _, _, F, _, _, R, _,
 _, _, _, _, _, _, _, _, F,
          _, _, _,
          _, _, _,
          _, _, _,
          _, _, R,
          _, _, _,
          _, _, _),
	  [r/ -1,b/ -1,r/1]
	).
match_cst_pattern1corners(
    cube( _, _, _,
          _, _, _,
          _, _, _,
 _, _, _, _, _, _, _, _, _,
 _, _, _, _, F, _, _, R, _,
 _, _, _, _, _, _, _, _, R,
          _, _, _,
          _, _, _,
          _, _, F,
          _, _, _,
          _, _, _,
          _, _, _),
	  [d/1,b/1,d/ -1]
	).
match_cst_pattern1corners(
    cube( _, _, _,
          _, _, _,
          _, _, _,
 _, _, _, _, _, _, _, _, _,
 _, L, _, _, F, _, _, _, _,
 _, _, _, _, _, _, _, _, F,
          _, _, _,
          _, _, _,
          _, _, L,
          _, _, _,
          _, _, _,
          _, _, _),
	  [l/1,b/ -1,l/ -1]
	).
match_cst_pattern1corners(
    cube( _, _, _,
          _, _, _,
          _, _, _,
 _, _, _, _, _, _, _, _, _,
 _, _, _, _, F, _, _, R, _,
 F, _, _, _, _, _, _, _, _,
          _, _, _,
          _, _, _,
          R, _, _,
          _, _, _,
          _, _, _,
          _, _, _),
	  [r/ -1,b/1,r/1]
	).

% These are destructive draws.
match_dst_pattern1corners(
    cube( _, _, _,
          _, _, _,
          _, _, _,
 _, _, _, _, _, _, _, _, _,
 _, _, _, _, F, _, _, R, _,
 _, _, _, _, _, _, _, _, _,
          _, _, _,
          _, _, _,
          _, _, R,
          _, _, F,
          _, _, _,
          _, _, _),
	  [r/ -1,b/2,r/1]
	).
match_dst_pattern1corners(
    cube( _, _, _,
          _, _, _,
          _, _, _,
 _, _, _, _, _, _, _, _, _,
 _, _, _, _, F, _, _, _, _,
 _, _, _, _, _, F, _, _, _,
          _, _, X,
          _, D, _,
          _, _, _,
          _, _, _,
          _, _, _,
          _, _, _),
	  [r/ -1,b/ -1,r/1]
	):-
	X\==D.
match_dst_pattern1corners(
    cube( _, _, _,
          _, _, _,
          _, _, _,
 _, _, _, _, _, _, _, _, _,
 _, _, _, _, F, _, _, _, _,
 _, _, _, _, _, _, X, _, _,
          _, _, Y,
          _, _, _,
          _, _, _,
          _, _, _,
          _, _, _,
          _, _, _),
	  [r/ -1,b/ -1,r/1]
	):-
	X==F;
	Y==F.

%
% Adjust the second layer. Assume that the first layer is already adjusted.
%
adjust2(C1,L,C2):-
	adjust2sides(C1,L,C2).

%
% Adjust the side stones of the second layer of the cube.
% This assumes that the first layer is already adjusted.
%

adjust2sides(C,[],C):-
C = cube( _, _, _,
          U, U, U,
          _, _, _,
 _, L, _, _, _, _, _, R, _,
 _, L, _, _, _, _, _, R, _,
 _, L, _, _, _, _, _, R, _,
          _, _, _,
          D, D, D,
          _, _, _,
          _, _, _,
          _, _, _,
          _, _, _).
adjust2sides(C1,L3,C3):-
	meta_match_pattern2sides(C1,L1,C2),
	adjust2sides(C2,L2,C3),
	append(L1,L2,L3).

meta_match_pattern2sides(C1,L2,C3):-
	turn_cube(C1,N,C2),
	match_cst_pattern2sides(C2,L1),
 n_maplist(N,perm_turn_cube,L2,L1),
	draws(C1,L2,C3).
meta_match_pattern2sides(C1,L2,C3):-
	turn_cube(C1,N,C2),
	match_dst_pattern2sides(C2,L1),
 n_maplist(N,perm_turn_cube,L2,L1),
	draws(C1,L2,C3).

% These are constructive draws.
match_cst_pattern2sides(
    cube( _, _, _,
          _, _, _,
          _, _, _,
 _, _, _, _, _, _, _, _, _,
 _, L, _, _, _, _, _, R, _,
 _, R, _, _, _, _, _, L, _,
          _, _, _,
          D, D, D,
          _, _, _,
          _, _, _,
          _, _, _,
          _, _, _),
	  [r/2,b/2,r/2,b/2,r/2]
      ).
match_cst_pattern2sides(C1,[b/Turns|DrawList]):-
	draw(C1,b/Turns,C2),
	match_pattern2sides_turned_backside(C2,DrawList).

% These are destructive draws.
match_dst_pattern2sides(
    cube( _, _, _,
          _, _, _,
          _, _, _,
 _, _, _, _, _, _, _, _, _,
 _, _, _, _, _, _, _, R, _,
 _, _, _, _, _, _, _, X, _,
          _, _, _,
          _, D, Y,
          _, _, _,
          _, _, _,
          _, _, _,
          _, _, _),
	  [r/ -1,b/ -1,r/1,b/1,d/1,b/1,d/ -1]
      ):-
	X\==R;
	Y\==D.

% These are constructive draws.
match_pattern2sides_turned_backside(
    cube( _, _, _,
          _, _, _,
          _, _, _,
 _, _, _, _, _, _, _, _, _,
 D, _, _, _, _, _, _, R, _,
 _, _, _, _, _, _, _, _, _,
          _, _, _,
          _, D, _,
          _, _, _,
          _, _, _,
          R, _, _,
          _, _, _),
	  [r/ -1,b/ -1,r/1,b/1,d/1,b/1,d/ -1]
      ).
match_pattern2sides_turned_backside(
    cube( _, R, _,
          _, _, _,
          _, _, _,
 _, _, _, _, _, _, _, _, _,
 _, _, _, _, _, _, _, R, _,
 _, _, _, _, _, _, _, _, _,
          _, _, _,
          _, D, _,
          _, _, _,
          _, _, _,
          _, _, _,
          _, D, _),
	  [d/1,b/ -1,d/ -1,b/ -1,r/ -1,b/1,r/1]
      ).

% Adjust the third layer. Assume that the second layer is already adjusted.
adjust3(C1,L5,C4):-
	adjust3draw_corners(C1,L1,C2),
	adjust3turn_corners(C2,L2,C3),
	append(L1,L2,L3),
	adjust3sides(C3,L4,C4),
	append(L3,L4,L5).

%
% Draw the corner stones of the third layer of the cube to their places.
% This assumes that the second layer is already adjusted.
% This needs some explanation:
% 1. Find the two adjacent corner stones which have two colors in common
%    (the one of the backside and one other).
%    Such two stones must always exist!
% 2. Turn the backside of the cube so that these two stones are at the correct
%    side.
% 3. Now there are four possible cases:
%    - These stones and the other two stones are at their correct place,
%      i.e. all corner stones are at their correct place.
%    - These stones are at their right place,
%      but the other two stones are exchanged.
%    - These stones are exchanged,
%      but the other two stones are at their correct place.
%    - Both these stones and the other two stones are exchanged.
%    In each case, use a specific list of draws which will draw the corner
%    stones to their correct place.
%

adjust3draw_corners(Cube,ResList,ResCube):-
	turn_cube(Cube,N,Cube1), % rotate the cube and focus on the left side.
	draw(Cube1,b/Turns,Cube2), % rotate the backside of the cube.
Cube2 
  = cube(A1, _,C1,
          _, U, _,
          _, _, _,
A2, _, _, _, _, _, _, _,C2,
 _, L, _, _, _, _, _, _, _,
B1, _, _, _, _, _, _, _, _,
          _, _, _,
          _, D, _,
         B2, _, _,
         B3, _, _,
          _, _, _,
         A3, _,C3),
	 % check if the two left cornerstones are at their right place.
	   member(L,[A1,A2,A3]),
	   member(L,[B1,B2,B3]),
         % filter the color that is not in common
	   findall(Color,(member(Color,[A1,A2,A3]),
			  member(Color,[U,D])),
		   [UpLeftColor]),
	   findall(Color,(member(Color,[C1,C2,C3]),
			  member(Color,[U,D])),
		   [UpRightColor]),
	 % determine the drawlist which corrects the corners
	   match_pattern3draw_corners(UpLeftColor,U,UpRightColor,DrawList),
  n_maplist(N,perm_turn_cube,ResList,[b/Turns|DrawList]),
	 draws(Cube,ResList,ResCube).

% Select a drawlist to draw the corner stones of the cube to their right
% places.
match_pattern3draw_corners(
	UpColor,
	UpColor,
	UpColor,
	[]
    ):-!.
match_pattern3draw_corners(
	_,
	UpColor,
	UpColor,
	[d/ -1,r/1,d/1,b/1,d/ -1,b/ -1,r/ -1,b/1,d/1]
    ):-!.
match_pattern3draw_corners(
	UpColor,
	UpColor,
	_,
	[u/ -1,l/1,u/1,b/1,u/ -1,b/ -1,l/ -1,b/1,u/1]
    ):-!.
match_pattern3draw_corners(
	_,
	_,
	_,
	[r/1,b/1,d/1,b/ -1,d/ -1,r/ -1]
    ).

%
% Turn the corner stones of the third layer of the cube.
% This assumes that the second layer is already adjusted and all corner
% stones are already at the right place.
%

adjust3turn_corners(C,[],C):-
C = cube( U, _, U,
          _, _, _,
          _, _, _,
 L, _, _, _, _, _, _, _, R,
 _, _, _, _, _, _, _, _, _,
 L, _, _, _, _, _, _, _, R,
          _, _, _,
          _, _, _,
          D, _, D,
          _, _, _,
          _, _, _,
          _, _, _).
adjust3turn_corners(Cube,ResList,ResCube):-
	turn_cube(Cube,N,Cube1),
	match_pattern3turn_corners(Cube1,List1),
 n_maplist(N,perm_turn_cube,ResList,List1),
	draws(Cube,ResList,ResCube).

match_pattern3turn_corners(
    cube( L, _, _,
          _, _, _,
          _, _, _,
 _, _, _, _, _, _, _, _, U,
 _, _, _, _, _, _, _, _, _,
 _, _, _, _, _, _, _, _, D,
          _, _, _,
          _, _, _,
          L, _, _,
          D, _, R,
          _, _, _,
          U, _, R),
	  [d/1,b/1,l/1,b/ -1,l/ -1,b/1,l/1,b/ -1,l/ -1,d/ -1]
      ).
match_pattern3turn_corners(
    cube( _, _, U,
          _, _, _,
          _, _, _,
 U, _, _, _, _, _, _, _, R,
 _, _, _, _, _, _, _, _, _,
 _, _, _, _, _, _, _, _, D,
          _, _, _,
          _, _, _,
          L, _, _,
          D, _, R,
          _, _, _,
          L, _, _),
	  [l/ -1,b/ -1,l/1,b/ -1,l/ -1,b/2,l/1,b/2]
      ).
match_pattern3turn_corners(
    cube( L, _, U,
          _, _, _,
          _, _, _,
 _, _, _, _, _, _, _, _, R,
 _, _, _, _, _, _, _, _, _,
 D, _, _, _, _, _, _, _, _,
          _, _, _,
          _, _, _,
          _, _, R,
          L, _, D,
          _, _, _,
          U, _, _),
	  [b/2,l/ -1,b/2,l/1,b/1,l/ -1,b/1,l/1]
      ).
match_pattern3turn_corners(
    cube( L, _, R,
          _, _, _,
          _, _, _,
 _, _, _, _, _, _, _, _, _,
 _, _, _, _, _, _, _, _, _,
 L, _, _, _, _, _, _, _, R,
          _, _, _,
          _, _, _,
          D, _, D,
          _, _, _,
          _, _, _,
          U, _, U),
	  [d/ -1,b/ -1,d/1,b/ -1,d/ -1,b/2,d/1,l/ -1,b/2,l/1,b/1,l/ -1,b/1,l/1]
      ).
match_pattern3turn_corners(
    cube( _, _, _,
          _, _, _,
          _, _, _,
 U, _, _, _, _, _, _, _, U,
 _, _, _, _, _, _, _, _, _,
 L, _, _, _, _, _, _, _, R,
          _, _, _,
          _, _, _,
          D, _, D,
          _, _, _,
          _, _, _,
          L, _, R),
	  [l/ -1,b/ -1,l/1,b/ -1,l/ -1,b/2,l/1,d/ -1,b/2,d/1,b/1,d/ -1,b/1,d/1]
      ).
match_pattern3turn_corners(
    cube( U, _, _,
          _, _, _,
          _, _, _,
 L, _, _, _, _, _, _, _, U,
 _, _, _, _, _, _, _, _, _,
 _, _, _, _, _, _, _, _, R,
          _, _, _,
          _, _, _,
          L, _, D,
          D, _, _,
          _, _, _,
          _, _, R),
	  [l/ -1,b/ -1,l/1,b/ -1,l/ -1,b/2,l/1,r/ -1,b/2,r/1,b/1,r/ -1,b/1,r/1]
      ).
match_pattern3turn_corners(
    cube( L, _, R,
          _, _, _,
          _, _, _,
 _, _, _, _, _, _, _, _, _,
 _, _, _, _, _, _, _, _, _,
 _, _, _, _, _, _, _, _, _,
          _, _, _,
          _, _, _,
          L, _, R,
          D, _, D,
          _, _, _,
          U, _, U),
        [l/ -1,b/ -1,l/1,b/ -1,l/ -1,b/1,l/1,b/ -1,l/ -1,b/2,l/1]
      ).

%
% Adjust the side stones of the third layer of the cube.
% This assumes that the corner stones of the third layer are already
% adjusted.
%

% In this case the side stones are already adjusted.
adjust3sides(C,[],C):-
C = cube( _, U, _,
          _, U, _,
          _, _, _,
 _, _, _, _, _, _, _, _, _,
 L, L, _, _, _, _, _, R, R,
 _, _, _, _, _, _, _, _, _,
          _, _, _,
          _, D, _,
          _, D, _,
          _, _, _,
          _, _, _,
          _, _, _).
adjust3sides(Cube,ResList,ResCube):-
	meta_match_pattern3sides(Cube,List1,Cube1),
	adjust3sides(Cube1,List2,ResCube),
	append(List1,List2,ResList).

meta_match_pattern3sides(Cube,ResList,ResCube):-
	turn_cube(Cube,N,Cube1),
	match_pattern3sides(Cube1,List1),
 n_maplist(N,perm_turn_cube,ResList,List1),
	draws(Cube,ResList,ResCube).

match_pattern3sides(
    cube( _, _, _,
          _, U, _,
          _, _, _,
 _, _, _, _, _, _, _, _, _,
A1, _, _, _, _, _, _, _, _,
 _, _, _, _, _, _, _, _, _,
          _, _, _,
          _, _, _,
          _, _, _,
          _, _, _,
         A2, B, _,
          _, _, _),
	  [l/2,f/ -1,d/2,l/1,r/ -1,b/2,l/ -1,r/1,f/1,l/2]
      ):-
	turn_side([A1,A2],[U,B]).
match_pattern3sides(
    cube( _, _, _,
          _, U, _,
          _, _, _,
 _, _, _, _, _, _, _, _, _,
 _, _, _, _, _, _, _, _, _,
 _, _, _, _, _, _, _, _, _,
          _, _, _,
          _, _, _,
          _,A1, _,
          _,A2, _,
          _, B, _,
          _, _, _),
	  [r/2,f/ -1,u/2,r/1,l/ -1,b/2,r/ -1,l/1,f/1,r/2]
      ):-
	turn_side([A1,A2],[U,B]).
match_pattern3sides(
    cube( _, _, _,
          _, U, _,
          _, _, _,
 _, _, _, _, _, _, _, _, _,
 _, _, _, _, _, _, _, _,A1,
 _, _, _, _, _, _, _, _, _,
          _, _, _,
          _, _, _,
          _, _, _,
          _, _, _,
          _, B,A2,
          _, _, _),
	  [r/2,f/ -1,r/1,l/ -1,b/2,r/ -1,l/1,u/2,f/1,r/2]
      ):-
	turn_side([A1,A2],[U,B]).
match_pattern3sides(
    cube( _, _, _,
          _, _, _,
          _, _, _,
 _, _, _, _, _, _, _, _, _,
 _, L, _, _, _, _, _, _, _,
 _, _, _, _, _, _, _, _, _,
          _, _, _,
          _, D, _,
          _, _, _,
          _, D, _,
          L, _, _,
          _, _, _),
	  [l/ -1,b/2,l/2,b/1,l/ -1,b/ -1,l/ -1,b/2,r/1,d/1,l/1,d/ -1,r/ -1]
      ).
match_pattern3sides(
    cube( _, _, _,
          _, _, _,
          _, _, _,
 _, _, _, _, _, _, _, _, _,
 _, L, _, _, _, _, _, R, _,
 _, _, _, _, _, _, _, _, _,
          _, _, _,
          _, _, _,
          _, _, _,
          _, _, _,
          L, _, R,
          _, _, _),
	  [r/1,d/1,l/ -1,b/2,l/2,b/1,l/ -1,b/ -1,l/ -1,
	   b/2,r/1,d/1,l/1,d/ -1,r/ -1,d/ -1,r/ -1]
      ).

% These predicates optimise a draw list.
opt_draws([],[]).
opt_draws([_/0|List],ResList):-
	!,
	opt_draws(List,ResList).
opt_draws([M1/C1|List1],ResList):-
	opt_draws(List1,[M2/C2|List3]),
	!,
	C3 is C1+C2,
	opt_two_draws(M1/C1,M2/C2,C3,List2),
	append(List2,List3,ResList).
opt_draws(List,List).

opt_two_draws(M /_ ,M /_ ,-2,[M / 2]      ):-!.
opt_two_draws(M /_ ,M /_ , 0,[]           ):-!.
opt_two_draws(M /_ ,M /_ , 3,[M / -1]     ):-!.
opt_two_draws(M /_ ,M /_ , 4,[]           ):-!.
opt_two_draws(M /_ ,M /_ , C,[M / C]      ):-!.
opt_two_draws(M1/C1,M2/C2, _,[M1/C1,M2/C2]).

disordered(Cube):-
	ordered(Ordered),
	random_draws(Draws),
	draws(Ordered,Draws,Cube).

% random_draws(-DrawList).
random_draws(DrawList):-
 random(0, 6, Draws),
	random_draws(Draws,DrawList).

random_draws(0,[]):-!.
random_draws(Draws,[Draw|DrawList]):-
	random_draw(Draw),
	NewDraws is Draws - 1,
	random_draws(NewDraws,DrawList).

% random_draw(-Draw).
random_draw(M/N):-
 random(0, 6, X),
	int2draw(X,M),
 random(0,3,Y),
	(Y =:= 0 -> N is -1; N is Y).

int2draw(0,u).
int2draw(1,l).
int2draw(2,f).
int2draw(3,r).
int2draw(4,d).
int2draw(5,b).

%
% Do a list of draws.
% Note: It is possible to specify an empty list of draws with this predicate,
% whereas you cannot specify 'no draw' with the draw/3 predicate.
%
draws(C,[],C).
draws(C1,[M|T],C3):-
	draw(C1,M,C2),
	draws(C2,T,C3).

% These are the legal draws.
% For speed reasons, all single clockwise draws are given *explicitly* here
% instead of computing them from applying one single draw to a several times
% turned and tilted cube.
% Note: Order is important here. Zero clockwise turns must be the last
% solution found!
draw(
    cube(U1,U2,U3,
         U4,U5,U6,
         U7,U8,U9,
L1,L2,L3,F1,F2,F3,R1,R2,R3,
L4,L5,L6,F4,F5,F6,R4,R5,R6,
L7,L8,L9,F7,F8,F9,R7,R8,R9,
         D1,D2,D3,
         D4,D5,D6,
         D7,D8,D9,
         B1,B2,B3,
         B4,B5,B6,
         B7,B8,B9),
	 l/1,
    cube(B1,U2,U3,
         B4,U5,U6,
         B7,U8,U9,
L7,L4,L1,U1,F2,F3,R1,R2,R3,
L8,L5,L2,U4,F5,F6,R4,R5,R6,
L9,L6,L3,U7,F8,F9,R7,R8,R9,
         F1,D2,D3,
         F4,D5,D6,
         F7,D8,D9,
         D1,B2,B3,
         D4,B5,B6,
         D7,B8,B9)
     ).
draw(
    cube(U1,U2,U3,
         U4,U5,U6,
         U7,U8,U9,
L1,L2,L3,F1,F2,F3,R1,R2,R3,
L4,L5,L6,F4,F5,F6,R4,R5,R6,
L7,L8,L9,F7,F8,F9,R7,R8,R9,
         D1,D2,D3,
         D4,D5,D6,
         D7,D8,D9,
         B1,B2,B3,
         B4,B5,B6,
         B7,B8,B9),
	 f/1,
    cube(U1,U2,U3,
         U4,U5,U6,
         L9,L6,L3,
L1,L2,D1,F7,F4,F1,U7,R2,R3,
L4,L5,D2,F8,F5,F2,U8,R5,R6,
L7,L8,D3,F9,F6,F3,U9,R8,R9,
         R7,R4,R1,
         D4,D5,D6,
         D7,D8,D9,
         B1,B2,B3,
         B4,B5,B6,
         B7,B8,B9)
     ).
draw(
    cube(U1,U2,U3,
         U4,U5,U6,
         U7,U8,U9,
L1,L2,L3,F1,F2,F3,R1,R2,R3,
L4,L5,L6,F4,F5,F6,R4,R5,R6,
L7,L8,L9,F7,F8,F9,R7,R8,R9,
         D1,D2,D3,
         D4,D5,D6,
         D7,D8,D9,
         B1,B2,B3,
         B4,B5,B6,
         B7,B8,B9),
	 r/1,
    cube(U1,U2,F3,
         U4,U5,F6,
         U7,U8,F9,
L1,L2,L3,F1,F2,D3,R7,R4,R1,
L4,L5,L6,F4,F5,D6,R8,R5,R2,
L7,L8,L9,F7,F8,D9,R9,R6,R3,
         D1,D2,B3,
         D4,D5,B6,
         D7,D8,B9,
         B1,B2,U3,
         B4,B5,U6,
         B7,B8,U9)
     ).
draw(
    cube(U1,U2,U3,
         U4,U5,U6,
         U7,U8,U9,
L1,L2,L3,F1,F2,F3,R1,R2,R3,
L4,L5,L6,F4,F5,F6,R4,R5,R6,
L7,L8,L9,F7,F8,F9,R7,R8,R9,
         D1,D2,D3,
         D4,D5,D6,
         D7,D8,D9,
         B1,B2,B3,
         B4,B5,B6,
         B7,B8,B9),
	 b/1,
    cube(R3,R6,R9,
         U4,U5,U6,
         U7,U8,U9,
U3,L2,L3,F1,F2,F3,R1,R2,D9,
U2,L5,L6,F4,F5,F6,R4,R5,D8,
U1,L8,L9,F7,F8,F9,R7,R8,D7,
         D1,D2,D3,
         D4,D5,D6,
         L1,L4,L7,
         B7,B4,B1,
         B8,B5,B2,
         B9,B6,B3)
     ).
draw(
    cube(U1,U2,U3,
         U4,U5,U6,
         U7,U8,U9,
L1,L2,L3,F1,F2,F3,R1,R2,R3,
L4,L5,L6,F4,F5,F6,R4,R5,R6,
L7,L8,L9,F7,F8,F9,R7,R8,R9,
         D1,D2,D3,
         D4,D5,D6,
         D7,D8,D9,
         B1,B2,B3,
         B4,B5,B6,
         B7,B8,B9),
	 u/1,
    cube(U7,U4,U1,
         U8,U5,U2,
         U9,U6,U3,
F1,F2,F3,R1,R2,R3,B9,B8,B7,
L4,L5,L6,F4,F5,F6,R4,R5,R6,
L7,L8,L9,F7,F8,F9,R7,R8,R9,
         D1,D2,D3,
         D4,D5,D6,
         D7,D8,D9,
         B1,B2,B3,
         B4,B5,B6,
         L3,L2,L1)
     ).
draw(
    cube(U1,U2,U3,
         U4,U5,U6,
         U7,U8,U9,
L1,L2,L3,F1,F2,F3,R1,R2,R3,
L4,L5,L6,F4,F5,F6,R4,R5,R6,
L7,L8,L9,F7,F8,F9,R7,R8,R9,
         D1,D2,D3,
         D4,D5,D6,
         D7,D8,D9,
         B1,B2,B3,
         B4,B5,B6,
         B7,B8,B9),
	 d/1,
    cube(U1,U2,U3,
         U4,U5,U6,
         U7,U8,U9,
L1,L2,L3,F1,F2,F3,R1,R2,R3,
L4,L5,L6,F4,F5,F6,R4,R5,R6,
B3,B2,B1,L7,L8,L9,F7,F8,F9,
         D7,D4,D1,
         D8,D5,D2,
         D9,D6,D3,
         R9,R8,R7,
         B4,B5,B6,
         B7,B8,B9)
     ).
draw(C1,M/2,C3):-
	draw(C1,M/1,C2),
	draw(C2,M/1,C3).
draw(C1,M/ -1,C2):-
	draw(C2,M/1,C1).
draw(C,_/0,C).

%
% These predicates are used to correct a list of draws applied to a turned
% cube (see intro).
%

% This is the draw permutation for clockwise cube rotations around the
% front/back axis.
perm_turn_cube(d/N,l/N).
perm_turn_cube(l/N,u/N).
perm_turn_cube(u/N,r/N).
perm_turn_cube(r/N,d/N).
perm_turn_cube(f/N,f/N).
perm_turn_cube(b/N,b/N).

% This is the draw permutation for cube tilts from the front to the upside.
perm_tilt_cube(u/N,b/N).
perm_tilt_cube(f/N,u/N).
perm_tilt_cube(d/N,f/N).
perm_tilt_cube(b/N,d/N).
perm_tilt_cube(l/N,l/N).
perm_tilt_cube(r/N,r/N).

% This predicate will rotate the cube clockwise around its front and
% back sides as the rotation axis.
% The notation is turn_cube(?C1,+N,?C2),
% where C1 is the original cube and C2 is the turned cube.
% N indicates how many times C2 was turned 90 degree clockwise.
% Note: Order is important here. Zero turns must be the last solution found!
turn_cube(C1,1,C2):-
	turn_cube(C1,C2).
turn_cube(C1,2,C3):-
	turn_cube(C1,C2),
	turn_cube(C2,C3).
turn_cube(C1,3,C2):-
	turn_cube(C2,C1).
turn_cube(C1,0,C1).

% turn_cube(?C1, ?C2) turns the cube C1 to be cube C2
% where the front and back side make up the rotation axis.
turn_cube(
    cube(U1,U2,U3,
         U4,U5,U6,
         U7,U8,U9,
L1,L2,L3,F1,F2,F3,R1,R2,R3,
L4,L5,L6,F4,F5,F6,R4,R5,R6,
L7,L8,L9,F7,F8,F9,R7,R8,R9,
         D1,D2,D3,
         D4,D5,D6,
         D7,D8,D9,
         B1,B2,B3,
         B4,B5,B6,
         B7,B8,B9),
    cube(L7,L4,L1,
         L8,L5,L2,
         L9,L6,L3,
D7,D4,D1,F7,F4,F1,U7,U4,U1,
D8,D5,D2,F8,F5,F2,U8,U5,U2,
D9,D6,D3,F9,F6,F3,U9,U6,U3,
         R7,R4,R1,
         R8,R5,R2,
         R9,R6,R3,
         B3,B6,B9,
         B2,B5,B8,
         B1,B4,B7)
     ).

% tilt_cube(?UnTiltCube,?TiltCube) turns the front side to the upside of the
% cube.
tilt_cube(
    cube(U1,U2,U3,
         U4,U5,U6,
         U7,U8,U9,
L1,L2,L3,F1,F2,F3,R1,R2,R3,
L4,L5,L6,F4,F5,F6,R4,R5,R6,
L7,L8,L9,F7,F8,F9,R7,R8,R9,
         D1,D2,D3,
         D4,D5,D6,
         D7,D8,D9,
         B1,B2,B3,
         B4,B5,B6,
         B7,B8,B9),
    cube(F1,F2,F3,
         F4,F5,F6,
         F7,F8,F9,
L3,L6,L9,D1,D2,D3,R7,R4,R1,
L2,L5,L8,D4,D5,D6,R8,R5,R2,
L1,L4,L7,D7,D8,D9,R9,R6,R3,
         B1,B2,B3,
         B4,B5,B6,
         B7,B8,B9,
         U1,U2,U3,
         U4,U5,U6,
         U7,U8,U9)
	).

% Specify what a turned corner stone may look like
turn_corner([M1,M2,M3],[M1,M2,M3]).
turn_corner([M3,M1,M2],[M1,M2,M3]).
turn_corner([M2,M3,M1],[M1,M2,M3]).

% Specify what a turned side stone may look like:
turn_side([M1,M2],[M1,M2]).
turn_side([M2,M1],[M1,M2]).

end_of_file.
