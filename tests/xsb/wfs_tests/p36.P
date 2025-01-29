query(p36,noise(1),[noise(0),loaded(0),shoots(0),noise(1),loaded(1),shoots(1)],[loaded(0),noise(1),loaded(1),shoots(1)],[]).
%-----------------------------------------------------------

:- table loaded / 1.
:- table noise / 1.
:- table shoots / 1.

noise(1) :- loaded(1), shoots(1).
loaded(1) :- succ(0,1), loaded(0), tnot(shoots(0)).
succ(0,1).
loaded(0).
shoots(1).
