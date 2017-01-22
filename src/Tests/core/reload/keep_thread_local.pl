:- thread_local p/1.

p1 :-
	asserta(p(a)),
	p(_).
