:- module(shift,
	  [ shift_safe/2
	  ]).

shift_safe('$attvars_after_choicepoint/2', predicate).
shift_safe('succ/2', predicate).
shift_safe('unify_with_occurs_check/2', predicate).

shift_safe(unify_vmi, function).
shift_safe(vm_compile_instruction, function).
shift_safe(unify_ptrs, function).
shift_safe(unifyAtomic, function).
shift_safe(unify_with_occurs_check, function).
shift_safe(var_or_integer, function).
shift_safe(globalMPZ, function).
shift_safe(put_number__LD, function).
