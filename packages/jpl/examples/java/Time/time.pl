% a simple database for Time.java

traverse( []).

traverse( [H|T]) :-
	traverse( H),
	traverse( T).


noop( _X).


noop_nobind( _X, _Y).


noop_bind( X, X).

