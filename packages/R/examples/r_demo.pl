% :- use_module(library('R')).
:- use_module(library(lists)).     % member/2.
:- use_module(library(readutil)).     % read_line_to_codes/2.

:- nl, nl.
:- ( (current_prolog_flag(windows,true),\+ r_bin(test)) ->
       write( 'As this session runs under MS Windows you need to first register' ),nl,
       write( 'the location of the R executalbe using r_bin/1 before you can' ), nl,
       write( 'the demos.' ),
       nl, nl
       ;
       true
   ).

:- write( 'Demo predicates for R (r_session) package.' ), nl.
:- write( 'See r_demo_1/0,...,r_demo_10/0.' ), nl.
:- write( 'Also r_demo/0 for r_demo_1/0,...,r_demo_7/0 which are the main demos.' ), nl.
:- write( 'r_demo_all/0 and r_demo_clauses/0 for r_demo_1,...,r_demo_10.' ), nl.
:- write( 'which include demos for some non-basic features.' ), nl.
:- write( 'You need to look at the sources before running r_demo_8,9 and 10.' ).
:- nl, nl.

r_demo :-
     nl, nl,
     Rdemos = [r_demo_1,r_demo_2,r_demo_3,r_demo_4,r_demo_5,r_demo_6,r_demo_7],
     r_demo( Rdemos, false ).

r_demo_all:-
     nl, nl,
     Rdemos = [r_demo_1,r_demo_2,r_demo_3,r_demo_4,r_demo_5,r_demo_6,r_demo_7,r_demo_8,r_demo_9,r_demo_10],
     r_demo( Rdemos, false ).

r_demo_clauses :-
     nl, nl,
     Rdemos = [r_demo_1,r_demo_2,r_demo_3,r_demo_4,r_demo_5,r_demo_6,r_demo_7,r_demo_8,r_demo_9,r_demo_10],
     r_demo( Rdemos, true ).
     
r_demo( Rdemos, Clauses ) :-
     member(Wh, Rdemos ),
     write( doing-Wh ), nl,
     ( Clauses == true ->
          write( 'Clauses: ' ), nl,
          findall( Wh-Body, (clause(Wh,Body),
                          portray_clause((Wh:-Body)), nl), _ )
          ;
          true
     ),
     ( call(Wh) -> 
          true
          ; 
          write( 'Demo ended with failure.' ), nl
     ),
     nl, nl,
     fail.
r_demo( _Rdemos, _ ) :-
     write( done ), nl.

r_demo_1 :-
     write( 'Demo: basic vector interactions.' ), nl, nl,
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

r_demo_2 :-
     write( 'Demo: plots (screen and postscript).' ), nl, nl,
     r_open,
     r_in( y <- rnorm(50) ),
     r_print( y ),
     r_in( Y <- y ),
     write( y(Y) ), nl,
     r_in( x <- rnorm(y) ),
     r_print( x ),
     r_in( X <- x ),
     write( x(X) ), nl,
     r_in( x11(width=5,height=3.5) ),
     r_in( plot(x,y)),
     write( 'Press Return to continue...' ), nl,
     current_prolog_flag( version, V ),
     ( integer(V) -> User = current_input  % SWI Prolog
                   ; User = user ),
     read_line_to_codes( User, _ ),
     r_in( 'dev.off()' ),
     r_in( 'postscript(file="x_vs_y.eps")' ),
     r_in( plot(x,y)),
     r_in( 'dev.off()' ),
     r_close,
     !, % Swi leaves a backtracking point at read_line_to_codes/2
     write( 'Check that file x_vs_y.eps has been created.' ), nl.

r_demo_3 :-
     write( 'Demo: aliases.' ), nl, nl,
     r_open( [alias(mamonaku)] ),
     ( current_r_session(Alias),write(session(Alias)),nl, fail; true ),
     r_in( mamonaku, x <- c(10.4, 5.6, 3.1, 6.4, 21.7) ),
     r_print( mamonaku, x ),
     r_close( mamonaku ).

r_demo_4 :-
     write( 'Demo: history.' ), nl, nl,
     r_open,
     ( r_history(A,B), write(history(A,B)), nl, fail; true ),
     r_in( x <- c(10.4, 5.6, 3.1, 6.4, 21.7) ),
     ( r_out( print(x), Lines ), r_lines_print( Lines ), fail; true ),
     r_print( x ),
     ( r_history(C,D), write(h(C,D)), nl, fail; true ),
     r_close.

r_demo_5 :-
     write( 'Demo: calls to R functions.' ), nl, nl,
     r_open,
     r_in( i <- 0:14 ),
     r_print( i ),
     r_in( I <- i ),
     write( 'I'(I) ), nl,
     r_in( x <- i/10 ),
     r_in( y <- c(176.547,194.2552,218.5462,441.3706,795.786,1190.8606,1321.0128,1326.4694,1437.3068,1364.6906,1343.768,1513.7298,1553.8264,1554.1748,1549.399) ),
     r_print( (integrate(splinefun(x,y), 0.2, 0.4)) ),
     r_close.

r_demo_6 :-
     write( 'Demo: copying output and error on to file.' ), nl, nl,
     r_open( [copy('rec_both.txt',both)] ),
     r_in( x <- c(10.4, 5.6, 3.1, 6.4, 21.7) ),
     r_print( x ),
     write( 'Check that file rec_both.txt has been created.' ), nl,
     r_close.

r_demo_7 :-
     write( 'Demo: error on R.' ), nl, nl,
     r_open( [at_r_halt(restart)] ),
     r_in( x <- c(10.4, 5.6, 3.1, 6.4, 21.7) ),
     r_print( x ),
     r_print( y ),
     r_print( x ),
     r_close.

%%% Cut-off

r_demo_8 :-
     write( 'Demo: reinstate on halt.' ), nl,
     write( 'This is no longer valid.' ), nl, nl,
     r_open( [at_r_halt(reinstate)] ),
     r_in( x <- c(10.4, 5.6, 3.1, 6.4, 21.7) ),
     r_print( y ),
          % here slave dies
          % and r_session tries to restar_demo it and replay all commands.
     r_print( x ),
          % succeeds
     r_close.

/* change 192.168.0.* to a host in your domain before running the following. */
r_demo_9 :-
     write( 'Demo: ssh on a machine with R on a different location.' ), nl, nl,
     r_open( [ssh('192.168.0.3')] ),
     r_in( I <- 0:14 ),
     write( 'I'(I) ), nl,
     r_close.

r_demo_10 :-
     write( 'Demo: ssh on a machine with explicit set of the remote R location.' ),
     nl, nl,
     r_bin( '/usr/local/users/nicos/local/bin/R' ),
     r_open( [ssh('192.168.0.3')] ),
     r_in( I <- 0:14 ),
     write( 'I'(I) ), nl,
     r_close.

/*
% You can replace any of the above r_open/0,1, with one of the following
r_open( [with(restore)] ).
     % do not use --no-restore on the R flags
r_open( [copy(copied,both)] ).
     % copy both input and output to file copied
r_open( [at_r_halt(restar_demo),alias(mamunaku),copy(copied_in,in)] ).
     % copy input to file copied_in
r_open( [at_r_halt(restar_demo),alias(mamunaku),copy(copy_out,out)] ).
     % copy output to file copied_out
     */
