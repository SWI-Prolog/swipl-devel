:- export output_table/2.

:- import write_canonical_lettervar/1 from xsb_writ.
:- import numbervars/3 from num_vars.
:- import get_calls/3, get_returns/2 from tables.

%==============================================================================

output_table(File,Term) :- tell(File), 'output table'(Term), told.

'output table'(Term) :-
	get_calls(Term, C, R), get_returns(C,R),
	numbervars(Term,0,_), write_canonical_lettervar(Term), nl, fail.
'output table'(_).

%==============================================================================
