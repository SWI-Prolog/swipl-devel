%
% %Z% %M% %I% %E% %Q%
%
% Copyright (C) 1993 by Christian Schlichtherle.
%
% For distribution conditions please read the
% GNU General Public License version 2.
%
%
% GUI for playing (and solving) Rubik's Cube using XPCE/SWI-Prolog.
% Invoke rubik_pce/0.

:-module(rubikpce,[rubikpce/0]).
:-use_module(library(pce)).
:- require([ send_list/3
	   ]).


% First read the Prolog predicates for solving Rubik's Cube. These predicates
% do the real work (i.e. order Rubik's Cube) and are defined so that they
% can be used alone without XPCE. This gives you the ability to use these
% predicates with any ordinary Prolog system (hopefully).
:-use_module(rubik).
:-use_module(maplist).

:-dynamic mousepos/2,drawlist/2,steplist/2,do_pause/1.

% This neat little trick gives us the ability to store a cube as an object
% level attribute of a frame.
:-pce_begin_class(cube,vector,
		  "Instances of this class represent Rubik's Cube.").
:-pce_end_class.

rubikpce:-
	new(Frame,frame('Rubik''s Cube')),
	make_pathlist(Frame,FrontWindow,_),
	make_boxlist(Frame,CrossWindow),
	send(FrontWindow,left,CrossWindow),
	send(Frame,append,new(Dialog,dialog)),
	send(CrossWindow,above,Dialog),
	send(Dialog,append,button(disorder,message(@prolog,disorder,@receiver?frame))),
	send(Dialog,append,button(adjust,message(@prolog,adjust,@receiver?frame))),
	send(Dialog,append,button(quit,message(@prolog,quit,@receiver?frame))),
	send(Dialog,append,button(step,message(@prolog,step,@receiver?frame)),next_row),
	send(Dialog,append,button(play,message(@prolog,play,@receiver?frame))),
	send(Dialog,append,button(pause,message(@prolog,pause,@receiver?frame))),
	send(Dialog,append,button(stop,message(@prolog,stop,@receiver?frame))),
	ordered(blue,red,green,orange,gray,yellow,Cube),
	send_cube(Frame,Cube),
	refresh(Frame),
	send(FrontWindow,open).

disorder(Frame):-
	% Ensure that no sequence is running.
          (get(Frame,attribute,timer,Timer) -> get(Timer,status,idle); true),
	%load_sequence(Frame,[f/1]),
	%load_sequence(Frame,[l/ -1,r/1,f/ -1,b/1,l/1,r/ -1,f/1,b/ -1,l/ -1,r/1,f/ -1,b/1]),
	%load_sequence(Frame,[r/2,l/2,u/2,r/2,l/2,d/2]),
	%load_sequence(Frame,[r/2,d/2,r/2,d/2,r/2,d/2]),
	%load_sequence(Frame,[r/ -1,l/1,f/2,r/1,l/ -1,u/2]),
	%load_sequence(Frame,[r/ -1,d/1,r/1,f/1,d/1,f/ -1,u/1,f/1,d/ -1,f/ -1,r/ -1,d/ -1,r/1,u/ -1]),
	%load_sequence(Frame,[r/1,l/ -1,u/ -1,d/1,f/1,b/ -1,r/1,l/ -1]),
	%load_sequence(Frame,[r/ -1,l/ -1,f/ -1,b/ -1,r/ -1,l/ -1,f/ -1,b/ -1,r/ -1,l/ -1,f/ -1,b/ -1]),
	%load_sequence(Frame,[r/1,l/1,u/2,r/ -1,l/ -1,f/ -1,b/ -1,d/2,f/1,b/1]),
	%load_sequence(Frame,[r/1,l/1,f/1,b/1,u/1,d/1,r/1,l/1,f/1,b/1,u/1,d/1]),
	%load_sequence(Frame,[r/1,l/1,u/2,r/ -1,l/ -1,f/1,b/1,u/2,f/ -1,b/ -1]),
	random_draws(DrawList),load_sequence(Frame,DrawList),
	play(Frame).

adjust(Frame):-
	% Ensure that no sequence is running.
          (get(Frame,attribute,timer,Timer)->get(Timer,status,idle);true),
	get_cube(Frame,Disordered),
	time(adjust(Disordered,Draws)),
	load_sequence(Frame,Draws),
	play(Frame).

time(Goal):-
      Goal.

step(Frame):-
	pause(Frame),
	start_timer(Frame).

play(Frame):-
	retractall(do_pause(Frame)),
	start_timer(Frame).

pause(Frame):-
	retractall(do_pause(Frame)),
	assert(do_pause(Frame)).

stop(Frame):-
	discard_sequence(Frame).

quit(Frame):-
	discard_sequence(Frame),
	retractall(steplist(Frame,_)),
	retractall(mousepos(Frame,_)),
	retractall(do_pause(Frame)),
	free(Frame).

load_sequence(Frame,Draws):-
	retractall(drawlist(Frame,_)),
	assert(drawlist(Frame,Draws)).

discard_sequence(Frame):-
	retractall(drawlist(Frame,_)).

start_timer(Frame):-
	get(Frame,attribute,timer,Timer),!,
	send(Timer,start).
start_timer(Frame):-
	new(Timer,timer(0.15,message(@prolog,timer_action,Frame))),
	send(Frame,attribute,attribute(timer,Timer)),
	send(Timer,start).

stop_timer(Frame):-
	get(Frame,attribute,timer,Timer),
	send(Timer,stop).

timer_action(Frame):-
	retract(steplist(Frame,[Step|Steps])),!,
	(Steps==[]->
	  (
	    (retract(do_pause(Frame))->stop_timer(Frame);true),
	    send_cube(Frame,Step),
	    refresh(Frame)
	  );(
	    assert(steplist(Frame,Steps)),
	    display_cube(Frame,Step)
	  )
	).
timer_action(Frame):-
	retract(drawlist(Frame,[Draw|Draws])),!,
	get_cube(Frame,Cube),
	assert(drawlist(Frame,Draws)),
	singlesteps(Cube,Draw,StepList),
	assert(steplist(Frame,StepList)),
	timer_action(Frame).
timer_action(Frame):-
	retractall(drawlist(Frame,_)),
	stop_timer(Frame).

% make_pathlist(+Frame,-FrontWindow,-BackWindow).
make_pathlist(Frame,FrontWindow,BackWindow):-
	new(Size,size(6*20,6*20)),
	send(Frame,append,new(FrontWindow,window(cube_frontside,Size))),
	send(Frame,append,new(BackWindow,window(cube_backside,Size))),
	send(FrontWindow,above,BackWindow),
	send(FrontWindow,display,new(FrontDevice,device)),
	send(BackWindow,display,new(BackDevice,device)),
	send(FrontDevice,position,point(3.5*20,2.5*20)),
	send(BackDevice,position,point(3.5*20,3.5*20)),
	new(Vector,vector),
	send(Frame,attribute,attribute(pathvector,Vector)),
	path_position(FrontDevice,BackDevice,PosList),
	box_position(BoxPosList),
	make_paths(PosList,BoxPosList,Vector,1).

% This predicate is used to compute the graphical position of each path.
% in the list.
path_position(DF,DB,L):-
	path_position(DF,DB,u,l,f,r,d,b,L).
path_position(DF,DB,U,L,F,R,D,B,
	                        [(DF,2,2,U),(DF,1,2,U),(DF,0,2,U),
	                         (DF,2,1,U),(DF,1,1,U),(DF,0,1,U),
	                         (DF,2,0,U),(DF,1,0,U),(DF,0,0,U),
(DB,2,0,L),(DB,2,1,L),(DB,2,2,L),(DF,0,2,F),(DF,0,1,F),(DF,0,0,F),(DF,0,0,R),(DF,1,0,R),(DF,2,0,R),
(DB,1,0,L),(DB,1,1,L),(DB,1,2,L),(DF,1,2,F),(DF,1,1,F),(DF,1,0,F),(DF,0,1,R),(DF,1,1,R),(DF,2,1,R),
(DB,0,0,L),(DB,0,1,L),(DB,0,2,L),(DF,2,2,F),(DF,2,1,F),(DF,2,0,F),(DF,0,2,R),(DF,1,2,R),(DF,2,2,R),
	                         (DB,2,0,D),(DB,2,1,D),(DB,2,2,D),
	                         (DB,1,0,D),(DB,1,1,D),(DB,1,2,D),
	                         (DB,0,0,D),(DB,0,1,D),(DB,0,2,D),
	                         (DB,0,0,B),(DB,1,0,B),(DB,2,0,B),
	                         (DB,0,1,B),(DB,1,1,B),(DB,2,1,B),
	                         (DB,0,2,B),(DB,1,2,B),(DB,2,2,B)]
			     ).

make_paths([],[],_,_).
make_paths([(Device,X,Y,Side)|PosList],[(Row,Col)|BoxPosList],Vector,I):-
	compute_pathpoints(X,Y,Side,Point1,Point2,Point3,Point4),
	send(Device,display,new(Path,path)),
	send(Path,closed,@on),
	send_list(Path,append,[Point1,Point2,Point3,Point4]),
	send(Path,fill_pattern,@black_image),
	send(Path,recogniser,@draw_field_gesture),
	send(Path,attribute,attribute(row,Row)),
	send(Path,attribute,attribute(col,Col)),
	send(Vector,element,I,Path),
	II is I+1,
	make_paths(PosList,BoxPosList,Vector,II).

compute_pathpoints(X,Y,Side,point(OX1,OY1),point(OX2,OY2),point(OX3,OY3),point(OX4,OY4)):-
	path_shape(Side,(IX1,IY1),(IX2,IY2),(IX3,IY3),(IX4,IY4)),
      sgn(IX2, IX2SGN),
      sgn(IX4, IX4SGN),
      sgn(IY4, IY4SGN),
      sgn(IY2, IY2SGN),
      PX is Y * (IX2 + IX2SGN) + X * (IX4 + IX4SGN),
      PY is X * (IY4 + IY4SGN) + Y * (IY2 + IY2SGN),
	OX1 is PX+IX1,OY1 is PY+IY1,
	OX2 is PX+IX2,OY2 is PY+IY2,
	OX3 is PX+IX3,OY3 is PY+IY3,
	OX4 is PX+IX4,OY4 is PY+IY4.

% path_shape(?Side,?Point1,?Point2,?Point3,?Point4) defines the shape of the
% paths. The points are listed counterclockwise. The first point is always
% (0,0).
path_shape(u,(0,0),( 11,-11),( -9,-11),(-20,  0)).
path_shape(l,(0,0),( 11, 11),( 11, -9),(  0,-20)).
path_shape(f,(0,0),(-20,  0),(-20, 20),(  0, 20)).
path_shape(r,(0,0),(  0, 20),( 11,  9),( 11,-11)).
path_shape(d,(0,0),(-20,  0),( -9, 11),( 11, 11)).
path_shape(b,(0,0),(  0,-20),(-20,-20),(-20,  0)).

sgn(0,0).
sgn(X,Y):-
	(X >= 0 -> Y is 1; Y is -1).

make_boxlist(Frame,Window):-
	send(Frame,append,new(Window,window(cube_cross,size(10*20,13*20)))),
	send(Window,display,new(Device,device)),
	send(Device,move,point(10,10)),
	new(Vector,vector),
	send(Frame,attribute,attribute(boxvector,Vector)),
	box_position(PosList),
	% This does not yet work because window<-area does not return the
	% visible area of the window.
        %new(_,constraint(Window,Device,spatial(xref=x+w/2,yref=y+h/2,xref=x+w/2,yref=y+h/2))),
	make_boxes(Device,PosList,Vector,1).

% This predicate is used to compute the graphical position of each box
% in the list.
box_position(
	            [( 0,3),( 0,4),( 0,5),
	             ( 1,3),( 1,4),( 1,5),
	             ( 2,3),( 2,4),( 2,5),
( 3,0),( 3,1),( 3,2),( 3,3),( 3,4),( 3,5),( 3,6),( 3,7),( 3,8),
( 4,0),( 4,1),( 4,2),( 4,3),( 4,4),( 4,5),( 4,6),( 4,7),( 4,8),
( 5,0),( 5,1),( 5,2),( 5,3),( 5,4),( 5,5),( 5,6),( 5,7),( 5,8),
	             ( 6,3),( 6,4),( 6,5),
	             ( 7,3),( 7,4),( 7,5),
	             ( 8,3),( 8,4),( 8,5),
	             ( 9,3),( 9,4),( 9,5),
	             (10,3),(10,4),(10,5),
	             (11,3),(11,4),(11,5)
	      ]).

make_boxes(_,[],_,_).
make_boxes(Device,[(Y,X)|PosList],Vector,I):-
	send(Device,display,new(Box,box(20,20)),point(X*20,Y*20)),
	send(Box,radius,3),
	send(Box,fill_pattern,@black_image),
	send(Box,recogniser,@draw_field_gesture),
	send_list(Box,attribute,[attribute(row,Y),attribute(col,X)]),
	send(Vector,element,I,Box),
	II is I+1,
	make_boxes(Device,PosList,Vector,II).

% refresh(+Frame) will redisplay the cube associated with Frame on the
% display.
%
% To alter the cube associated with the frame and refresh the display use
%
%   send_cube(Frame,Cube),refresh(Frame).
refresh(Frame):-
	get(Frame,attribute,cube,CubeVector),
	send(?(Frame,attribute,pathvector),for_all,
	     message(@arg1,colour,?(CubeVector,element,@arg2))),
	send(?(Frame,attribute,boxvector),for_all,
	     message(@arg1,colour,?(CubeVector,element,@arg2))).

% display_cube(+Frame,+CubeTerm) will display CubeTerm on Frame. However, it
% will not alter the cube associated with Frame. To do this, use send_cube/2.
%
% To alter the cube associated with the frame and refresh the display use
%
%   send_cube(Frame,Cube),refresh(Frame).
display_cube(Frame,CubeTerm):-
	new(CubeVector,CubeTerm),
	send(?(Frame,attribute,pathvector),for_all,
	     message(@arg1,colour,?(CubeVector,element,@arg2))),
	send(?(Frame,attribute,boxvector),for_all,
	     message(@arg1,colour,?(CubeVector,element,@arg2))).

% singlesteps describes the single steps needed to make draw (f,1).
% This predicate is needed for draw animation.
singlesteps(
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
	 [
    cube(U1,U2,U3,
         U4,U5,U6,
         L3,U7,U8,
L1,L2,L6,F1,F2,F3,U9,R2,R3,
L4,L5,L9,F4,F5,F6,R1,R5,R6,
L7,L8,D1,F7,F8,F9,R4,R8,R9,
         D2,D3,R7,
         D4,D5,D6,
         D7,D8,D9,
         B1,B2,B3,
         B4,B5,B6,
         B7,B8,B9),
    cube(U1,U2,U3,
         U4,U5,U6,
         L6,L3,U7,
L1,L2,L9,F4,F1,F2,U8,R2,R3,
L4,L5,D1,F7,F5,F3,U9,R5,R6,
L7,L8,D2,F8,F9,F6,R1,R8,R9,
         D3,R7,R4,
         D4,D5,D6,
         D7,D8,D9,
         B1,B2,B3,
         B4,B5,B6,
         B7,B8,B9),
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
     ]).
singlesteps(C1,b/1,[S1,S2,S3]):-
	compute_singlesteps(C1,[tilt_cube(C1,C2),tilt_cube(C2,_)],[S1,S2,S3]).
singlesteps(C1,u/1,[S1,S2,S3]):-
	compute_singlesteps(C1,[tilt_cube(_,C1)],[S1,S2,S3]).
singlesteps(C1,d/1,[S1,S2,S3]):-
	compute_singlesteps(C1,[tilt_cube(C1,_)],[S1,S2,S3]).
singlesteps(C1,l/1,[S1,S2,S3]):-
	compute_singlesteps(C1,[tilt_cube(C1,C2),turn_cube(C2,C3),tilt_cube(_,C3)],[S1,S2,S3]).
singlesteps(C1,r/1,[S1,S2,S3]):-
	compute_singlesteps(C1,[tilt_cube(C1,C2),turn_cube(C2,C3),tilt_cube(C3,_)],[S1,S2,S3]).
singlesteps(C1,M/ -1,[S1,S2,C2]):-
	draw(C1,M/ -1,C2),
	singlesteps(C2,M/1,[S2,S1,C1]).
singlesteps(C1,M/2,[S1,S2,S3,S4,S5,S6]):-
	singlesteps(C1,M/1,[S1,S2,S3]),
	singlesteps(S3,M/1,[S4,S5,S6]).

compute_singlesteps(Cube,[],[S1,S2,S3]):-
	singlesteps(Cube,f/1,[S1,S2,S3]).
compute_singlesteps(C1,[Command|Commands],[S1,S2,S3]):-
	Command=..[Functor,C1,C2],
	Command,
	compute_singlesteps(C2,Commands,[S4,S5,S6]),
	T1=..[Functor,S1,S4],T1,
	T2=..[Functor,S2,S5],T2,
	T3=..[Functor,S3,S6],T3.
compute_singlesteps(C1,[Command|Commands],[S1,S2,S3]):-
	Command=..[Functor,C2,C1],
	Command,
	compute_singlesteps(C2,Commands,[S4,S5,S6]),
	T1=..[Functor,S4,S1],T1,
	T2=..[Functor,S5,S2],T2,
	T3=..[Functor,S6,S3],T3.

:- pce_global(@draw_field_gesture,make_draw_field_gesture).
:- pce_global(@hand2_cursor,new(cursor(hand2))).

make_draw_field_gesture(G):-
	new(G,gesture),
	send(G,send_method,send_method(verify,vector(event),message(@prolog,verify_draw,@arg1))),
	send(G,send_method,send_method(initiate,vector(event),message(@prolog,initiate_draw,G))),
	send(G,send_method,send_method(drag,vector(event),message(@prolog,drag_draw,@arg1))),
	send(G,send_method,send_method(terminate,vector(event),message(@prolog,terminate_draw,@arg1))),
	send(G,status,active).

verify_draw(PCEEvent):-
	get(PCEEvent,receiver,PCEDevice),
	get(PCEDevice,frame,PCEFrame),
	get(PCEDevice,attribute,row,FromRow),
	get(PCEDevice,attribute,col,FromCol),
	retractall(mousepos(PCEFrame,_)),
	assert(mousepos(PCEFrame,FromRow/FromCol)).

initiate_draw(PCEGesture):-
	send(PCEGesture,cursor,@hand2_cursor).

drag_draw(Event):-
	get(Event,receiver,FromField),
	get(FromField,frame,Frame),
	get(?(FromField?device,pointed_objects,Event),head,ToField),
	get(ToField,attribute,row,ToRow),
	get(ToField,attribute,col,ToCol),
	mousepos(Frame,FromRow/FromCol),
	get_cube(Frame,Cube),!,
	pos2draw(FromRow/FromCol,ToRow/ToCol,Draw,Steps),
	compute_drag(Frame,ToRow/ToCol,Cube,Draw,Steps,NewCube),
	display_cube(Frame,NewCube).

% There is a bug in here. If the user makes a drag so that more than three
% singlesteps have to be made AT ONCE and the number of the singlesteps is not
% a multiple of three and the user releases the mouse button before reaching
% the next multiple of three, the cube will jump back to the initial position
% instead of to the next multiple of three.
% You may find this a bit complicated - I agree.
% You may even never notice this bug - that's why I don't fix it.
compute_drag(_,_,Cube,_,0,Cube):-!.
compute_drag(_,_,Cube,Draw,1,NewCube):-
	!,singlesteps(Cube,Draw,[NewCube|_]).
compute_drag(_,_,Cube,Draw,2,NewCube):-
	!,singlesteps(Cube,Draw,[_,NewCube|_]).
compute_drag(Frame,ToRow/ToCol,Cube,Draw,3,NewCube):-
	!,draw(Cube,Draw,NewCube),
	send_cube(Frame,NewCube),
	asserta(mousepos(Frame,ToRow/ToCol)).
compute_drag(Frame,Pos,Cube,Draw,Steps,NewCube):-
	draw(Cube,Draw,HelpCube),
	HelpSteps is Steps - 3,
	compute_drag(Frame,Pos,HelpCube,Draw,HelpSteps,NewCube).

terminate_draw(Event):-
	get(Event?receiver,frame,Frame),
	retractall(mousepos(Frame,_)),
        refresh(Frame).

% pos2draw(+From,+To,-Draw,-StepCount).
pos2draw(From,To,Draw,StepCount):-
	tilt_pos(From,M,TiltFrom),
	tilt_pos(To,M,TiltTo),
	turn_pos(TiltFrom,N,TiltRotFrom),
	turn_pos(TiltTo,N,TiltRotTo),
	simple_pos2draw(TiltRotFrom,TiltRotTo,TiltRotDraw,StepCount),
      n_maplist(N,perm_turn_cube,[TiltDraw],[TiltRotDraw]),
      n_maplist(M,perm_tilt_cube,[Draw],[TiltDraw]).

simple_pos2draw(3/FromCol,3/ToCol,u/1,Steps):-
	FromCol >= ToCol,!,
	Steps is FromCol - ToCol.
simple_pos2draw(3/FromCol,3/ToCol,u/ -1,Steps):-
	Steps is ToCol - FromCol.

% turn_pos(+Pos,+N,?RotPos) will rotate the cube from 0 to 3 times clockwise.
turn_pos(Pos,0,Pos).
turn_pos(Pos,1,RotPos):-
	turn_pos(Pos,RotPos).
turn_pos(Pos,2,RotPos):-
	turn_pos(Pos,Help),
	turn_pos(Help,RotPos).
turn_pos(Pos,3,RotPos):-
	turn_pos(Pos,Help1),
	turn_pos(Help1,Help2),
	turn_pos(Help2,RotPos).

% turn_pos(+Pos,?RotPos). Order is important here!
turn_pos(Row1/Col1,Row2/Col2):- % upside -> right
	Row1 < 3,!,
	Row2 is Col1,
	Col2 is 8 - Row1.
turn_pos(Row1/Col1,Row2/Col2):- % left -> upside
	Col1 < 3,!,
	Row2 is Col1,
	Col2 is 8 - Row1.
turn_pos(Row1/Col1,Row2/Col2):- % right -> downside
	Col1 > 5,!,
	Row2 is Col1,
	Col2 is 8 - Row1.
turn_pos(Row1/Col1,Row2/Col2):- % back -> back
	Row1 > 8,!,
	Row2 is 14 - Col1,
	Col2 is Row1 - 6.
turn_pos(Row1/Col1,Row2/Col2):- % down -> left
	Row1 > 5,!,
	Row2 is Col1,
	Col2 is 8 - Row1.
turn_pos(Row1/Col1,Row2/Col2):- % front -> front
	Row2 is Col1,
	Col2 is 8 - Row1.

% tilt_pos(+Pos,+N,?TiltPos) will tilt the cube M times from the front to the upside.
tilt_pos(Pos,0,Pos).
tilt_pos(Pos,1,TiltPos):-
	tilt_pos(Pos,TiltPos).
tilt_pos(Pos,2,TiltPos):-
	tilt_pos(Pos,Help),
	tilt_pos(Help,TiltPos).
tilt_pos(Pos,3,TiltPos):-
	tilt_pos(Pos,Help1),
	tilt_pos(Help1,Help2),
	tilt_pos(Help2,TiltPos).

% tilt_pos(+Pos,?TiltPos) will tilt the cube from the front to the upside.
tilt_pos(Row1/Col,Row2/Col):- % up -> back
	Row1 < 3,!,
	Row2 is 9 + Row1.
tilt_pos(Row1/Col1,Row2/Col2):- % left -> left
	Col1 < 3,!,
	Row2 is 5 - Col1,
	Col2 is Row1 - 3.
tilt_pos(Row1/Col1,Row2/Col2):- % right -> right
	Col1 > 5,!,
	Row2 is Col1 - 3,
	Col2 is 11 - Row1.
tilt_pos(Row1/Col,Row2/Col):- % back -> down, down -> front, front -> up
	Row2 is Row1 - 3.

% These "methods" should be really implemented as such one day:

% send_cube(+Frame,+Cube) associates cube with the frame. However, it does not
% display the cube.
%
% To alter the cube associated with the frame and refresh the display use
%
%   send_cube(Frame,Cube),refresh(Frame).
send_cube(Frame,Cube):-
	send(Frame,attribute,attribute(cube,Cube)).

get_cube(Frame,Cube):-
	Cube=cube(_,_,_,
		  _,_,_,
		  _,_,_,
	    _,_,_,_,_,_,_,_,_,
	    _,_,_,_,_,_,_,_,_,
	    _,_,_,_,_,_,_,_,_,
		  _,_,_,
		  _,_,_,
		  _,_,_,
		  _,_,_,
		  _,_,_,
		  _,_,_),
	get(Frame,attribute,cube,Cube).

end_of_file.
