:- module(test,
	[
	    test/0
	]).
%:- asserta(user:file_search_path(library, '.')).
:- use_module(library(inclpr),
	[
	    {}/1,
	    change_standard_domain/1,
	    standard_domain/1,
	    get_domain/2,
	    solve/0,
	    change_incremental/1,
	    incremental/1
	]).

test :- 
	catch(test1,_,(writeln('Unexpected exception'),halt(1))),
	halt(0).

test1 :- 
	change_standard_domain(i(-100,100)),
	(   \+ standard_domain(i(-100,100))
	->  writeln('Standard domain assertion error'),
	    halt(1)
	;   true
	),
	(   {X^2=Y}
	->  (   get_domain(X,i(LX,RX)),
		get_domain(Y,i(LY,RY))
	    ->  (   LX > -10.1, LX =< 10,
		    RX < 10.1, RX >= 10,
		    LY >= -0.1, LY =< 0,
		    RY =:= 100
		->  (   X=Y
		    ->  (   get_domain(X,i(L,R))
			->  (   L > -0.1, L =< 0,
			        R >= 1, R < 1.1
			    ->  true
			    ;   writeln('Unification domain pruning error'),
				halt(1)
			    )
			;   writeln('Unification domain error'),
			    halt(1)
			)
		    ;   writeln('Unification error'),
			halt(1)
		    )
		;   writeln('Domain pruning error'),
		    halt(1)
		)
	    ;   writeln('Domain error'),
	    halt(1)
	    )
	;   writeln('Basic constraint failure')    
	).
