:- module(prolog_vm,
	  [ vm_list/1
	  ]).
:- use_module(library(prolog_clause)).
:- use_module(library(lists)).

/** <module> SWI-Prolog Virtual Machine utilities

This is an internal developers  module   to  manage  the virtual machine
instructions.
*/

:- meta_predicate
	vm_list(:).

%%	vm_list(:Spec) is det.
%
%	Lists  the  definition  of  the   predicates  matching  Spec  to
%	=current_output=. Spec is also allowed to be a clause-reference.

vm_list(_:Ref) :-
	integer(Ref), !,
	(   nth_clause(_Head, N, Ref),
	    format('~40c~nclause ~d (~w):~n~40c~n', [0'-, N, Ref, 0'-]),
	    vm_list_clause(Ref),
	    fail
	;   true
	).
vm_list(Spec) :-
	'$find_predicate'(Spec, List),
	(   member(PI, List),
	    pi_to_head(PI, Head),
	    unify_args(Head, Spec),
	    predicate_name(Head, Name),
	    format('~72c~n~w~n~72c~n', [0'=, Name, 0'=]),
	    (	'$fetch_vm'(Head, 0, _, _)
	    ->	vm_list_clause(Head)
	    ;	format('    (No supervisor)~n')
	    ),
	    (   nth_clause(Head, N, Ref),
		clause(MHead, _, Ref),
		same_head(Head, MHead),
		format('~40c~nclause ~d (~w):~n~40c~n', [0'-, N, Ref, 0'-]),
		vm_list_clause(Ref),
		fail
	    ;   true
	    ),
	    fail
	;   true
	).

pi_to_head(M:PI, M:Head) :- !,
	pi_to_head(PI, Head).
pi_to_head(Name/Arity, Head) :-
	functor(Head, Name, Arity).

vm_list_clause(Clause) :-
	vm_list_clause(Clause, 0).

vm_list_clause(Clause, PC) :-
	'$fetch_vm'(Clause, PC, NextPC, VMI), !,
	format('~t~d~4| ~q~n', [PC, VMI]),
	vm_list_clause(Clause, NextPC).
vm_list_clause(_, _).

%	Unify the arguments of the specification with the given term,
%	so we can partially instantate the head.

unify_args(_, _/_) :- !.		% Name/arity spec
unify_args(X, X) :- !.
unify_args(_:X, X) :- !.
unify_args(_, _).

same_head(X, X) :- !.
same_head(H1, H2) :-
	strip_module(H1, _, H),
	strip_module(H2, _, H).
