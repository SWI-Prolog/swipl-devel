
:- table(boss/2).

:- hilog manage.
:- hilog p.
:- table(apply/3).

tc(R)(X,Y) :- R(X,Y).
tc(R)(X,Y) :- tc(R)(X,Z),R(Z,Y).

boss(X,Y) :- manage(X,Y).
boss(X,Y) :- boss(X,Z),manage(Z,Y).

manage(bill,mary).
manage(mary, sue).
manage(sue, john).

p(a,b).
p(b,c).
