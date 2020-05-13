%------------------------------------------------------------------------
% Program that caused a bus error.
%
% Model T = {p}, F = {q}, U = {r}.
%------------------------------------------------------------------------

:- dynamic rule/2.

:- table interp_g/1.

interp([]).
interp([tnot(G)|Gs]) :- tnot(interp_g(G)),interp(Gs).
interp([G|Gs]) :- interp_g(G),interp(Gs).

interp_g(G) :- rule(G,B),interp(B).

%-------------------------------------------------------------------------------

test :- new_program, query(Goal), interp_g(Goal), fail.
/*
test :- ( interp_g(p) ->
		( tnot(interp_g(p)) -> writeln('i(p) is undefined')
		; writeln('i(p) is true (OK)') )
	; writeln('i(p) is false') ),
        ( interp_g(q) ->
		( tnot(interp_g(q)) -> writeln('i(q) is undefined')
		; writeln('i(q) is true') )
	; writeln('i(q) is false (OK)') ),
	( interp_g(r) ->
		( tnot(interp_g(r)) -> writeln('i(r) is undefined (OK)')
		; writeln('i(r) is true') )
	; writeln('i(r) is false') ), fail.
 */
test :- writeln('Program interp16 finished execution...').

%-------------------------------------------------------------------------------

new_program :- cleanup, assert(query(r)),
	assert(rule(p,[tnot(q)])),
	assert(rule(p,[tnot(q),r,s])),
	assert(rule(q,[tnot(p),q])),
	assert(rule(r,[tnot(p),tnot(r),q])),
	assert(rule(r,[tnot(q),tnot(r),p])).

cleanup :- retractall(query(_)), retractall(rule(_,_)), abolish_all_tables.
