:- use_module(library('R')).
:- use_module(library(lists)).     % member/2.

rs_1 :-
     r_open,
     r_in( x <- c(10.4, 5.6, 3.1, 6.4, 21.7) ),
     ( r_out( print(x), Lines ), r_lines_print( Lines ), fail; true ),
     r_print( x ),
     r_in( x ),
     r_in( (y <- c(6,5,4,3,2,1); y) ), % The extra paranthesis are only
                                       % needed for Yap.
     r_in( Z <- c(10.4, 5.6, 3.1, 6.4, 21.7) ),
     write( z(Z) ), nl,
     r_close.

rs_2 :-
     r_open( [alias(mamonaku)] ),
     ( current_r_session(Alias),write(session(Alias)),nl, fail; true ),
     r_in( mamonaku, x <- c(10.4, 5.6, 3.1, 6.4, 21.7) ),
     r_print( mamonaku, x ),
     r_close( mamonaku ).

rs_3 :-
     r_open,
     ( r_history(A,B), write(h(A,B)), nl, fail; true ),
     r_in( x <- c(10.4, 5.6, 3.1, 6.4, 21.7) ),
     ( r_out( print(x), Lines ), r_lines_print( Lines ), fail; true ),
     r_print( x ),
     ( r_history(C,D), write(h(C,D)), nl, fail; true ),
     r_close.

rs_4 :-
     r_open,
     r_in( i <- 0:14 ),
     r_print( i ),
     r_in( I <- i ),
     write( 'I'(I) ), nl,
     r_in( x <- i/10 ),
     r_in( y <- c(176.547,194.2552,218.5462,441.3706,795.786,1190.8606,1321.0128,1326.4694,1437.3068,1364.6906,1343.768,1513.7298,1553.8264,1554.1748,1549.399) ),
     r_print( (integrate(splinefun(x,y), 0.2, 0.4)) ),
     r_close.

rs_5 :-
     r_open( [at_r_halt(restart)] ),
     r_in( x <- c(10.4, 5.6, 3.1, 6.4, 21.7) ),
     r_print( x ),
          % at this stage my slave dies due to the fact an error is encountered
          % y is an unknown object
     r_print( y ),
     r_print( x ),
          % also fails as restart doesnot replay the history. compare with reinstate
     r_close.
     
rs_6 :-
     r_open( [at_r_halt(reinstate)] ),
     r_in( x <- c(10.4, 5.6, 3.1, 6.4, 21.7) ),
     r_print( y ),
          % here slave dies
          % and r_session tries to restart it and replay all commands.
     r_print( x ),
          % succeeds
     r_close.

rs_7 :- 
     r_open( [copy(both,both)] ),
     r_in( x <- c(10.4, 5.6, 3.1, 6.4, 21.7) ),
     r_print( x ),
     r_print( y ),
     r_close.

rs_8 :- 
     r_open( [copy(out_only,out)] ),
     r_in( x <- c(10.4, 5.6, 3.1, 6.4, 21.7) ),
     r_print( x ),
     r_print( y ),
     r_close.

/* Private tests. */

/* change scibsn7 to a host in your domain before running the following. */
rs_9 :-
     r_open( [ssh(scibsn7)] ),
     r_in( I <- 0:14 ),
     write( 'I'(I) ), nl,
     r_close.

rs_10 :-
     r_open( [ssh(scibsn9)] ),
     r_in( I <- 0:14 ),
     write( 'I'(I) ), nl,
     r_close.

rs_all :-
       member(Wh,[rs_1,rs_2,rs_3,rs_4,rs_5,rs_6,rs_7,rs_8,rs_9,rs_10]),
       write( doing-Wh ), nl,
       call(Wh),
       nl, nl,
       fail.
rs_all :-
     write( done ), nl.

/*
% You can replace any of the above r_open/0,1, with one of the following
r_open( [with(restore)] ).
     % do not use --no-restore on the R flags
r_open( [copy(copied,both)] ).
     % copy both input and output to file copied
r_open( [at_r_halt(restart),alias(mamunaku),copy(copied_in,in)] ).
     % copy input to file copied_in
r_open( [at_r_halt(restart),alias(mamunaku),copy(copy_out,out)] ).
     % copy output to file copied_out
     */
