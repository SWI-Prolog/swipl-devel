:- use_module(library(jpl)).

% jpl_table_demo :-
%	displays the names and values of all current Prolog flags
%	in a new JTable (within a new JScrollPane, within a new JFrame)

jpl_table_demo :-
	findall(
		Ar,
		(	current_prolog_flag( N, V),						% assume atom(N)
			term_to_atom( V, Va),
			jpl_list_to_array( [N,Va], Ar)					% or jpl_new( '[Ljava.lang.String;', [N,Va], Ar)
		),
		Ars
	),
	jpl_list_to_array( Ars, Ac),							% or jpl_new( '[[Ljava.lang.String;', Ars, Ac)
	jpl_list_to_array( [name,value], Ah),
	jpl_new( 'javax.swing.JFrame', ['current_prolog_flag'], F),
	jpl_call( F, getContentPane, [], CP),
	jpl_new( 'javax.swing.JTable', [Ac,Ah], T),
	jpl_new( 'javax.swing.JScrollPane', [T], SP),
	jpl_call( CP, add, [SP,'Center'], _),
	jpl_call( F, setSize, [600,400], _),
	jpl_call( F, setVisible, [@(true)], _).


% this directive runs the above demo

:- jpl_table_demo.

