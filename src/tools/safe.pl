:- module(shift,
	  [ safe/3,			% +Type, +What, +Func
	    (volatile)/2,		% +Function, +File
	    stop/2
	  ]).

safe(predicate, _, '$attvars_after_choicepoint/2').
safe(predicate, _, 'succ/2').
safe(predicate, _, 'unify_with_occurs_check/2').

safe(function, _, unify_vmi).
safe(function, _, vm_compile_instruction).
safe(function, _, unify_ptrs).
safe(function, _, unifyAtomic).
safe(function, _, unify_with_occurs_check).
safe(function, _, var_or_integer).
safe(function, _, globalMPZ).
safe(function, _, put_number__LD).

volatile('PL_halt', _).
volatile('sysError', _).
volatile('fatalError', _).

stop('PL_warning', _).
stop('warning', _).
stop('PL_throw', _).
stop('outOfStack', _).
stop('abortProlog', _).
