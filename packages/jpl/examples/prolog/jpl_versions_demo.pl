:- use_module(library(jpl)).

jpl_versions_demo :-
	jpl_call( 'jpl.JPL', version_string, [], Vj),
	jpl_c_lib_version( Vc),
	jpl_pl_lib_version( Vp),

	nl,
	write( 'prolog library version: '), write( Vp), nl,
	write( '  java library version: '), write( Vj), nl,
	write( '     c library version: '), write( Vc), nl,
	(	Vp == Vj,
		Vj == Vc
	->	write( 'BINGO! you appear to have the same version of each library installed'), nl
	;	write( 'WHOOPS! you appear not to have the same version of each library installed'), nl
	),
	nl.


% this directive runs the above demo

:- jpl_versions_demo.

