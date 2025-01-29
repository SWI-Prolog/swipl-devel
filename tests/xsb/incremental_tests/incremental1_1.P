:- import incr_assert/1,incr_retractall/1,incr_retract/1 from increval.

test_seq:- test1,fail.
test_seq:- test2.

% direct dependency
test1:- p(X,Y),incr_writeln(first_p(X,Y)),fail.
test1:- incr_assert(q(d,4)),fail.
test1:- p(X,Y),incr_writeln(second_p(X,Y)),fail.
test1:- incr_retractall(q(a,_X)),fail.
test1:- p(X,Y),incr_writeln(third_p(X,Y)),fail.
test1:- incr_retract(q(b,_X)),fail.
test1:- get_residual(p(X,Y),_),incr_writeln(fourth_p(X,Y)),fail.
test1:- p(_X,_Y),fail.
test1:- get_residual(p(X,Y),_),incr_writeln(fifth_p(X,Y)),fail.
test1:- incr_assert(q(e,2)),fail.
test1:- get_residual(p(X,Y),_),incr_writeln(sixth_p(X,Y)),fail.
test1:- p(_X,_Y),fail.
test1:- get_residual(p(X,Y),_),incr_writeln(seventh_p(X,Y)),fail.
test1.

% transitive dependency
test2:- n(X,Y),incr_writeln(first_n(X,Y)),fail.
test2:- incr_assert(q(f,4)),fail.
test2:- n(X,Y),incr_writeln(second_n(X,Y)),fail.
test2:- incr_retractall(q(d,_X)),fail.
test2:- n(X,Y),incr_writeln(third_n(X,Y)),fail.
test2:- incr_retract(q(e,_X)),fail.
test2:- get_residual(n(X,Y),_),incr_writeln(fourth_n(X,Y)),fail.
test2:- n(_X,_Y),fail.
test2:- get_residual(n(X,Y),_),incr_writeln(fifth_n(X,Y)),fail.
test2:- incr_assert(q(g,2)),fail.
test2:- get_residual(n(X,Y),_),incr_writeln(sixth_n(X,Y)),fail.
test2:- n(_X,_Y),fail.
test2:- get_residual(n(X,Y),_),incr_writeln(seventh_n(X,Y)),fail.
test2.

incr_writeln(Term):- write(incr(Term)),writeln('.').


:- table p/2 as incremental.

p(X,Y) :- q(X,Y),Y =< 5.

:- table n/2 as incremental.
n(X,Y) :- p(X,Y),Y < 5.

%:- use_incremental_dynamic q/2.
%:- dynamic q/2.
:- dynamic q/2 as incremental.
q(a,1).
q(b,3).
q(c,5).
q(d,7).


end_of_file.
