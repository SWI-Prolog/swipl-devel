% a simple database for Test.java

p( a).

p( f(a)).

p( a-b).

p( _X).


p( a, a).

p( a, b).


q( f(X,X)).

q( f(_X,_Y)).


r( f(X,X), X).

r( f(X,X), _Y).


s( X, f(X,X)).

s( _Y, f(X,X)).


tuple( t(a,b,c,d,e)).


t :-
	throw( 'this is an error message').


display( X) :-
	write( X).

