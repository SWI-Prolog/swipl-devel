/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

:- module(draw_align,
	  [ align_graphical/2
	  , adjust_graphical/2
	  , align_path_point/3
	  ]).

:- use_module(library(pce)).

:- require([ ignore/1
	   , chain_list/2
	   , forall/2
	   , last/2
	   , member/2
	   , send_list/2
	   , append/3
	   , get_chain/3
	   , maplist/3
	   ]).

pce_ifhostproperty(prolog(quintus),
(:- require([ pow/3,		% Needed for handling Quintus expanded math
	      sqrt/2
	   ]))).

verbose(off).
%verbose(on).

satisfies(threshold,    H) :- H < 50.
satisfies(displacement, D) :- abs(D) < 15.

handicap(D, R, N, Handicap) :-
	satisfies(displacement, D),
	Handicap is abs(D) * N * 100 / (abs(R)+20),
	satisfies(threshold, Handicap).


abs(A, B) :-
	A > 0, !,
	B is A.
abs(A, B) :-
	B is -A.

		/********************************
		*           POSITIONS		*
		********************************/

%	align_graphicals(+Gr, +Grs)
%
%	Align Gr with Grs
%	

align_graphical(Gr, Grs) :-
	align_requests(Gr, Grs, Requests),
    portray_requests('Align', Requests),
	combine_request(Requests, Request),
    portray_requests('Combined', [Request]),
	offset_request(Request, Xdiff, Ydiff),
	ignore(Xdiff=0), ignore(Ydiff=0),
	CX is -Xdiff, CY is -Ydiff,
	send(Gr, relative_move, point(CX, CY)).


xy_request(request(G1, G2, _TX, _TY, _HX, _HY, _DX, _DY)) :-
	G1 == G2.
x_request(request(GX, _GY, _TX, _TY, _HX, _HY, _DX, _DY)) :- nonvar(GX).
y_request(request(_GX, GY, _TX, _TY, _HX, _HY, _DX, _DY)) :- nonvar(GY).
offset_request(request(_GX, _GY, _TX, _TY, _HX, _HY, DX, DY), DX, DY).
handicap_request(request(_GX, _GY, _TX, _TY, HX, HY, _DX, _DY), H) :-
	(   nonvar(HX)
	->  (   nonvar(HY)
	    ->  H is (HX+HY)/2
	    ;   H = HX
	    )
	;   H = HY
	).
describe_request(request(GX, GY, TX, TY, _HX, _HY, _DX, _DY), D) :-
	(   GX==GY
	->  (   TX == TY
	    ->  D = GX:TX
	    ;   D = GX:TX+TY
	    )
	;   nonvar(GX)
	->  (   nonvar(GY)
	    ->  D = GX:TX+GY:TY
	    ;   D = GX:TX
	    )
	;   D = GY:TY
	).


combine_request(R0, Request) :-
	split_requests(R0, RXY1, RX1, RY1),
	merge_requests(RX1, RY1, RXY2, NRX, NRY),
	append(RXY2, RXY1, RXY),
	(   best_request(RXY, Request)
	->  true
	;   ignore(best_request(NRX, Request)),
	    ignore(best_request(NRY, Request)),
	    nonvar(Request)
	).
	
	
split_requests([], [], [], []).
split_requests([R|T], RXY, RX, RY) :-
	(   xy_request(R)
	->  RXY = [R|TRXY],
	    split_requests(T, TRXY, RX, RY)
	;   x_request(R)
	->  RX = [R|TRX],
	    split_requests(T, RXY, TRX, RY)
	;   RY = [R|TRY],
	    split_requests(T, RXY, RX, TRY)
	).

merge_requests([], RY, [], [], RY).
merge_requests(RX, [], [], RX, []).
merge_requests(RX, RY, [], RX, RY).		  % TBD

best_request([R|T], Best) :-
	best_request(T, R, Best).

best_request([], B, B).
best_request([R|T], S, B) :-
	handicap_request(R, HR),
	handicap_request(S, HS),
	HR < HS, !,
	best_request(T, R, B).
best_request([_|T], S, B) :-
	best_request(T, S, B).


align_requests(Gr, Grs, Requests) :-
	findall(Request, (member(Gr2, Grs), alignment(Gr, Gr2, Request)),
		Requests).


%	alignment(+Gr1, +Gr2, +How, -Request)
%
%	Determine how well some alignment is feasible.

alignment(Gr1, Gr2, Request) :-
	(   get(Gr1, distance_x, Gr2, DX),
	    satisfies(displacement, DX)
	->  (   get(Gr1, distance_y, Gr2, DY),
	        satisfies(displacement, DY)
	    ->  alignment(_, Gr1, Gr2, Request)
	    ;   alignment(align_x, Gr1, Gr2, Request)
	    )
	;   get(Gr1, distance_y, Gr2, DY),
	    satisfies(displacement, DY),
	    alignment(align_y, Gr1, Gr2, Request)
	).


alignment(What, Gr1, Gr2, Request) :-
	get(Gr1, connections, Gr2, Chain),
	chain_list(Chain, Connections),
	member(Connection, Connections),
	get(Connection, width, W),
	get(Connection, height, H),
	(   W > H
	->  What = align_y,
            handicap(H, W, 1, Handicap),
	    get(Connection, start_y, SY),
	    get(Connection, end_y, EY),
	    (   get(Connection, from, Gr1)
	    ->  DiffY is SY - EY
	    ;   DiffY is EY - SY
	    ),
	    Request = request(_, Gr2, _, connection, _, Handicap, _, DiffY)
	;   What = align_x,
	    handicap(W, H, 1, Handicap),
	    get(Connection, start_x, SX),
	    get(Connection, end_x, EX),
	    (   get(Connection, from, Gr1)
	    ->  DiffX is SX - EX
	    ;   DiffX is EX - SX
	    ),
	    Request = request(Gr2, _, connection, _, Handicap, _, DiffX, _)
	).
alignment(handle, Gr1, Gr2, Request) :-
	setof(T, close_handle(Gr1, Gr2, T), [t(D,DX,DY)|_]),
	max_attribute([Gr1?width, Gr1?height, Gr2?width, Gr2?height], Max),
	handicap(D, Max, 1, Hc),
	Request = request(Gr2, Gr2, handle, handle, Hc, Hc, DX, DY).
alignment(center, Gr1, Gr2, Request) :-
	get(Gr1, center, C1),
	get(Gr2, center, C2),
	get(C1, distance, C2, Displacement),
	satisfies(displacement, Displacement),
	max_attribute([Gr1?width, Gr1?height, Gr2?width, Gr2?height], Max),
	handicap(Displacement, Max, 1, Handicap),
	get(C1, difference, C2, point(DX, DY)),
	send_list([C1, C2], done),
	Request = request(Gr2, Gr2, center, center, Handicap, Handicap, DX,DY).
alignment(align_x, Gr1, Gr2, Request) :-
	x_displacement(Gr1, Gr2, How, Displacement),
	satisfies(displacement, Displacement),
	max_attribute([Gr1?width, Gr2?width], Max),
	get(Gr1, distance, Gr2, Dist),
	Correct is (Dist+100) / 50,
	handicap(Displacement, Max, Correct, Handicap),
	Request = request(Gr2, _, How, _, Handicap, _, Displacement, _).
alignment(align_y, Gr1, Gr2, Request) :-
	y_displacement(Gr1, Gr2, How, Displacement),
	satisfies(displacement, Displacement),
	max_attribute([Gr1?height, Gr2?height], Max),
	get(Gr1, distance, Gr2, Dist),
	Correct is (Dist+100) / 50,
	handicap(Displacement, Max, Correct, Handicap),
	Request = request(_, Gr2, _, How, _, Handicap, _, Displacement).

	
x_displacement(Gr1, Gr2, center_x, Displacement) :-
	get(Gr1, center_x, C1),
	get(Gr2, center_x, C2),
	Displacement is C1 - C2.
x_displacement(Gr1, Gr2, left, Displacement) :-
	get(Gr1, left_side, L1),
	get(Gr2, left_side, L2),
	Displacement is L1 - L2.
x_displacement(Gr1, Gr2, right, Displacement) :-
	get(Gr1, right_side, R1),
	get(Gr2, right_side, R2),
	Displacement is R1 - R2.
x_displacement(Gr1, Gr2, left_of, Displacement) :-
	get(Gr1, left_side, L1),
	get(Gr2, right_side, R2),
	Displacement is L1 - R2.
x_displacement(Gr1, Gr2, right_of, Displacement) :-
	get(Gr1, right_side, R1),
	get(Gr2, left_side, L2),
	Displacement is R1 - L2.

	
y_displacement(Gr1, Gr2, center_y, Displacement) :-
	get(Gr1?center, y, C1),
	get(Gr2?center, y, C2),
	Displacement is C1 - C2.
y_displacement(Gr1, Gr2, top, Displacement) :-
	get(Gr1, top_side, T1),
	get(Gr2, top_side, T2),
	Displacement is T1 - T2.
y_displacement(Gr1, Gr2, bottom, Displacement) :-
	get(Gr1, bottom_side, B1),
	get(Gr2, bottom_side, B2),
	Displacement is B1 - B2.
y_displacement(Gr1, Gr2, below, Displacement) :-
	get(Gr1, top_side, V1),
	get(Gr2, bottom_side, V2),
	Displacement is V1 - V2.
y_displacement(Gr1, Gr2, above, Displacement) :-
	get(Gr1, bottom_side, V1),
	get(Gr2, top_side, V2),
	Displacement is V1 - V2.
	

max_attribute([Obj?Att], Value) :- !,
	get(Obj, Att, Value).
max_attribute([Obj?Att|Rest], Value) :- !,
	get(Obj, Att, V0),
	max_attribute(Rest, V1),
	Value is max(V0, V1).

close_handle(Gr1, Gr2, t(D, DX, DY)) :-
	get(Gr1, common_device, Gr2, Dev),
	get(Gr1, handles, CH1),
	chain_list(CH1, L1),
	member(H1, L1),
	get(H1, kind, Kind),
	get(H1, position, Gr1, Dev, Pos1),
	get(Gr2, handles, Pos1, Kind, 15, CH2),
	chain_list(CH2, L2),
	member(H2, L2),
	get(H2, position, Gr2, Dev, Pos2),
	get(Pos1, distance, Pos2, D),
	object(Pos2, point(X2, Y2)),
	object(Pos1, point(X1, Y1)),
	DX is X1 - X2,
	DY is Y1 - Y2.
	
		/********************************
		*        SIZE ADJUSTMENT	*
		********************************/

%	adjust_graphical(+Gr, +Grs)
%	Adjust the size of Gr to fit a size close to one of the graphical
%	in the list Grs.

adjust_graphical(Gr, Grs) :-
	adjust_requests(Gr, Grs, Requests),
    portray_requests('Adjust', Requests),
	combine_request(Requests, Request),
    portray_requests('Combined', [Request]),
	offset_request(Request, SW, SH),
	ignore(SW = @default), ignore(SH = @default),
	send(Gr, set, @default, @default, SW, SH).


adjust_requests(Gr, Grs, Requests) :-
	findall(Request, (member(Gr2, [@nil|Grs]),
			  adjust_request(Gr, Gr2, Request)), Requests).

adjust_request(Gr, Gr2, Request) :-
	(   w_adjust_dimension(Gr, Gr2, W2, HW)
	->  (   h_adjust_dimension(Gr, Gr2, H2, HH)
	    ->  Request = request(Gr2, Gr2, size, size, HW, HH, W2, H2)
	    ;   Request = request(Gr2, _, width, _, HW, _, W2, _)
	    )
	;   h_adjust_dimension(Gr, Gr2, H2, HH),
	    Request = request(_, Gr2, _, height, _, HH, _, H2)
	).
	

w_adjust_dimension(Line, @nil, 0, 0) :- !,	% almost vertical line
	send(Line, instance_of, line),
	get(Line, start, Start),
	get(Line, end, End),
	get(End, minus, Start, point(DX, DY)),
	abs(DX) * 10 < abs(DY).
w_adjust_dimension(Gr, Gr2, W2, Ha) :-
	get(Gr, width, W),
	get(Gr2, width, W2),
	DW is W - W2,
	handicap(DW, W, 1, H0),
	adjust_correct_handicap(connected(w), Gr, Gr2, H0, H1),
	adjust_correct_handicap(center_y,     Gr, Gr2, H1, H2),
	adjust_correct_handicap(class,        Gr, Gr2, H2, H3),
	adjust_correct_handicap(distance,     Gr, Gr2, H3, Ha).

	
h_adjust_dimension(Line, @nil, 0, 0) :- !,
	send(Line, instance_of, line),
	get(Line, start, Start),
	get(Line, end, End),
	get(End, minus, Start, point(DX, DY)),
	abs(DY) * 10 < abs(DX).
h_adjust_dimension(Gr, Gr2, H2, Ha) :-
	get(Gr, height, H),
	get(Gr2, height, H2),
	DH is H - H2,
	handicap(DH, H, 1, Ha0),
	adjust_correct_handicap(connected(h), Gr, Gr2, Ha0, Ha1),
	adjust_correct_handicap(center_x,     Gr, Gr2, Ha1, Ha2),
	adjust_correct_handicap(class,        Gr, Gr2, Ha2, Ha3),
	adjust_correct_handicap(distance,     Gr, Gr2, Ha3, Ha).
	
adjust_correct_handicap(connected(WH), Gr1, Gr2, H0, H) :-
	get(Gr1, connections, Gr2, Chain), !,
	chain_list(Chain, Cs),
	adjust_connections_correction(Cs, WH, Factor),
	H is H0 / Factor.
adjust_correct_handicap(class, Gr1, Gr2, H0, H) :-
	send(Gr1, same_class, Gr2), !,
	H is H0 * 0.6.
adjust_correct_handicap(distance, Gr1, Gr2, H0, H) :- !,
	get(Gr1, distance, Gr2, D),
	H is H0 * (D+20)/50.
adjust_correct_handicap(S, Gr1, Gr2, H0, H) :-
	(S == center_x ; S == center_y),
	get(Gr1, S, C1),
	get(Gr2, S, C2),
	DC is C1 - C2,
	abs(DC, ADC),
	ADC < 10, !,
	H is H0 * (ADC+5)/10.
adjust_correct_handicap(_, _, _, H, H).

adjust_connections_correction([], _, 1).
adjust_connections_correction([C|T], WH, F) :-
	adjust_connections_correction(T, WH, F0),
	(   WH == w
	->  get(C, end_y, V1),
	    get(C, start_y, V2)
	;   get(C, end_x, V1),
	    get(C, start_x, V2)
	),
	DV is V1 - V2,
	abs(DV, ADV),
	(   ADV < 10
	->  F is F0 * (ADV+5)/10
	;   F = F0
	).


		/********************************
		*            PORTRAY		*
		********************************/

%portray_requests(_, _) :- !.		  % comment-out for debugging
portray_requests(_, _) :-
	verbose(off), !.
portray_requests(Label, List) :-
	format('~w:~n', [Label]),
	forall(member(R, List), portray_request(R)).


portray_request(R) :-
	handicap_request(R, H),
	offset_request(R, X, Y),
	describe_request(R, D),
	\+ \+ (ignore(X = -), ignore(Y = -),
	       format('~t~8|~2f: ~p (~w,~w)~n', [H, D, X, Y])).
	

		 /*******************************
		 *	 PATH/LINE POINTS	*
		 *******************************/

align_path_point(Path, PR, P) :-
	findall(PC, candidate_path_point(Path, PR, PC), PCS),
	closest_point(PCS, PR, P), !.
align_path_point(_, P, P).

closest_point([P0|T], PR, P) :-
	distance_points(P0, PR, D),
	closest_point(T, PR, D, P0, P).

closest_point([], _, _, P, P).
closest_point([P0|T], PR, D0, _, P) :-
	distance_points(P0, PR, D1),
	D1 < D0, !,
	closest_point(T, PR, D1, P0, P).
closest_point([_|T], PR, D0, P0, P) :-
	closest_point(T, PR, D0, P0, P).


candidate_path_point(Path, point(X0,Y0), point(X,Y)) :-
	get_chain(Path, points, PointObjects),
	maplist(object, PointObjects, Points),
	(   (   last(point(LX,LY), Points),
		VX0 is X0-LX,
		VY0 is Y0-LY,
		align_path_vector(Points, v(VX0,VY0), v(VX,VY))
	    ->  X is LX+VX,
		Y is LY+VY
	    )
	;   align_to_path_point(Points, point(X0,Y0), point(X,Y))
	;   align_mid(Points, point(X0,Y0), point(X,Y))
	).

align_path_vector(Points, V0, V) :-
	align_path_vector(Points, V0, 1000, v, V),
	V \== v.
align_path_vector(_, v(DX, DY), v(0, DY)) :-
	abs(DX)*10 < abs(DY), !.
align_path_vector(_, v(DX, DY), v(DX, 0)) :-
	abs(DY)*10 < abs(DX), !.
	
align_path_vector([],  _, _, V, V) :- !.
align_path_vector([_], _, _, V, V) :- !.
align_path_vector([P1,P2|T], V0, H, _, V) :-
	make_vector(P1, P2, Va),
	diff_vector(Va, V0, Vb, D),
	D < H, !,
	align_path_vector([P2|T], V0, D, Vb, V).
align_path_vector([_|T], V0, H, V1, V) :-
	align_path_vector(T, V0, H, V1, V).
	

make_vector(point(X0,Y0), point(X1,Y1), v(DX, DY)) :-
	DX is X1-X0,
	DY is Y1-Y0.

msign(1).
msign(-1).

diff_vector(v(VX0, VY0), v(VX1, VY1), v(VX, VY), H) :-
	msign(MX),
	msign(MY),
	VX is VX0*MX,
	VY is VY0*MY,
	D is sqrt((VX1-VX)*(VX1-VX) + (VY1-VY)*(VY1-VY)),
	satisfies(displacement, D),
	L0 is sqrt(VX0*VX0 + VY0*VY0),
	H is D/max(1,L0).

%	align_to_path_point(+Points, +P0, -P)
%
%	Try to align to one of the existing points.

align_to_path_point(Points, P0, P) :-
	align_to_path_point(Points, P0, 1000, p, P),
	P \== p.

align_to_path_point([], _, _, P, P).
align_to_path_point([P0|T], PR, D, _, P) :-
	distance_points(P0, PR, H),
	satisfies(displacement, H),
	H < D, !,
	align_to_path_point(T, PR, H, P0, P).
align_to_path_point([_|T], PR, D, P0, P) :-
	align_to_path_point(T, PR, D, P0, P).
	
distance_points(point(X0,Y0), point(X1,Y1), D) :-
	D is sqrt((X1-X0)*(X1-X0) + (Y1-Y0)*(Y1-Y0)).


align_mid(Points, PR, P) :-
	findall(MP, mid_point(Points, PR, MP), MPS),
	sort(MPS, [_-P]).

mid_point(Points, point(X, Y), D-point(X, MY)) :-
	member(point(PX, PY1), Points),
	member(point(PX, PY2), Points),
	abs(PY1-PY2) > 10,
	MY is (PY1+PY2+1)//2,
	D is abs(MY-Y),
	satisfies(displacement, D).
mid_point(Points, point(X, Y), D-point(MX, Y)) :-
	member(point(PX1, PY), Points),
	member(point(PX2, PY), Points),
	abs(PX1-PX2) > 10,
	MX is (PX1+PX2+1)//2,
	D is abs(MX-X),
	satisfies(displacement, D).
