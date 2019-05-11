/*=====================================================================*/
/* NOTE: Make sure that this file is compiled with "spec_off" option.  */
/*=====================================================================*/

%--------------------------------------------------------------

test :- test_s2, fail.
test :- test_pq, fail.
test :- test_pXqX, fail.
test :- test_p0Xq0X.

%---------------------------------------------------------------

:- table s/2.

s(X, dallas) :- tnot(s(X, phoenix)).
s(lasVegas, phoenix) :- s(lasVegas, dallas).

test_s2 :-
	( s(X,Y), writeln(s(X,Y)), fail ; true ).

%--------------------------------------------------------

:- table p/0, q/0.

p :- tnot(q).
p.
q :- tnot(p). 

test_pq :-
	p, fail.
test_pq :-
	( p -> writeln('p is true') ; writeln('p is false') ),
	( q -> writeln('q is true') ; writeln('q is false') ).

%--------------------------------------------------------

% Program (from Ross JACM) that flounders but is
% left-to-right modularly stratified.

:- table p/1, q/1, t/2.

t(a,a).
p(X) :- t(X,X), tnot(q(X)).
q(X) :- t(X,Y), tnot(t(Y,Z)), p(Z).

test_pXqX :-
	( p(X), writeln(p(X)), fail ; q(X), writeln(q(X)), fail ; true ).

%--------------------------------------------------------

% Program (from Ross JACM) that is not left-to-right
% modularly stratified but does not flounder.  Model = {t0(a,a),p0(a)}

:- table p0/1, q0/1, t0/2.

t0(a,a).
p0(X) :- t0(X,X), tnot(q0(X)).
q0(X) :- t0(X,Y), p0(Z), tnot(t0(Y,Z)).

test_p0Xq0X :-
	( p0(X), writeln(p0(X)), fail ; q0(X), writeln(q0(X)), fail ; true ).

%--------------------------------------------------------

