:- dynamic query/5.

:- import query/5 from usermod.
:- import str_cat/3 from machine.
:- import numbervars/1 from num_vars.
:- import member/2 from basics.

?- load_dyn('wfsexs.P').

gen(P) :-
	telling(X), str_cat(P, '.P', File), tell(File), gen1(P), told, tell(X).

gen1(P) :-
	query(P, Q, Subgoals, Trues, Undefineds),
	write_query(query(P, Q, Subgoals, Trues, Undefineds)),
	write_table_decls(Subgoals), write_clauses(P),
	fail.
gen1(_).

write_query(Query) :-
	write(Query), writeln('.'),
	writeln('%-----------------------------------------------------------'),
	nl, fail.	% undo numbervars.
write_query(_).

write_table_decls(Subgoals) :- 
	findall(P/A, (member(S, Subgoals), functor(S, P, A)), PredList),
	sort(PredList, PredSet), member(Pred, PredSet),
	write(':- table '), write(Pred), writeln('.'), fail.
write_table_decls(_) :- nl.

write_clauses(P) :-
	pgm(P, Rule), numbervars(Rule),
	Rule = rule(Head, BodyLiterals),
	write(Head), write_body(BodyLiterals), writeln('.'),
	fail.
write_clauses(_).

write_body([]).
write_body([L|Ls]) :-
	write(' :- '), write_literal(L), write_body1(Ls).

write_body1([]).
write_body1([L|Ls]) :-
	write(', '), write_literal(L), write_body1(Ls).

write_literal(L) :-
	( L = (\+ A) -> write(tnot(A)) ; write(L) ).
