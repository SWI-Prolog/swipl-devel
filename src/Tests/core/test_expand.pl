:- module(test_expand,
	  [ test_expand/0
	  ]).
:- use_module(library(plunit)).
:- set_base_module(user).

test_expand :-
	run_tests([ expand
		  ]).

		 /*******************************
		 *	       RULES		*
		 *******************************/

:- multifile
	user:term_expansion/2,
	user:goal_expansion/2.
:- dynamic
	user:term_expansion/2,
	user:goal_expansion/2.

user:term_expansion(b2c, final_c).

term_expansion(a2b, b2c).
term_expansion(make_dcg, (a --> "")).

goal_expansion(g_b, g_c).


		 /*******************************
		 *	     PROGRAMS		*
		 *******************************/

:- if(false).				% nothing compiles if false
no_a --> [].
:- endif.

make_dcg.				% expand into DCG
a2b.					% chained expansion

e_not :-
	not(g_b).


		 /*******************************
		 *	       TESTS		*
		 *******************************/

:- begin_tests(expand).

test(cond_dcg, true) :-
	\+ current_predicate(no_a//0).
test(make_dcg, B == true) :-
	clause(a(_,_), B).
test(chained_term_expansion, B == true) :-
	clause(final_c, B).
test(meta_arg, B == not(g_c)) :-
	clause(e_not, B).

:- end_tests(expand).
