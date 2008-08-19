:- module(prolog_vm,
	  [ vm_list/1
	  ]).
:- use_module(library(prolog_clause)).
:- use_module(library(lists)).

/** <module> SWI-Prolog Virtual Machine utilities

This is an internal developers  module   to  manage  the virtual machine
instructions.
*/

:- module_transparent
	vm_list/1.

%%	vm_list(:Spec) is det.
%
%	Lists  the  definition  of  the   predicates  matching  Spec  to
%	=current_output=.

vm_list(Spec) :-
	'$find_predicate'(Spec, List),
	(   member(Head, List),
	    predicate_name(Head, Name),
	    format('~72c~n~w~n~72c~n', [0'=, Name, 0'=]),
	    (   nth_clause(Head, N, Ref),
		format('~40c~nclause ~d (~d):~n~40c~n', [0'-, N, Ref, 0'-]),
		vm_list_clause(Ref),
		fail
	    ;   true
	    ),
	    fail
	;   true
	).

vm_list_clause(Clause) :-
	vm_list_clause(Clause, 0).

vm_list_clause(Clause, PC) :-
	'$fetch_vm'(Clause, PC, NextPC, VMI), !,
	format('~t~d~4| ~q~n', [PC, VMI]),
	vm_list_clause(Clause, NextPC).
vm_list_clause(_, _).
