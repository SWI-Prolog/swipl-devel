% shoes a JColorChooser dialog, on top of a (necessary) JFrame,
% and awaits OK/Cancel click

:- use_module(library(jpl)).

jpl_colour_choose_demo :-
	jpl_new( 'javax.swing.JFrame', ['frame with dialog'], F),
	jpl_call( F, setLocation, [400,300], _),
	jpl_call( F, setSize, [400,300], _),
	jpl_call( F, setVisible, [@(true)], _),
	jpl_call( F, toFront, [], _),
	jpl_call( F, getContentPane, [], CP),
	jpl_get( 'java.awt.Color', pink, Pink),
	jpl_call( 'javax.swing.JColorChooser', showDialog, [CP,'pick a colo(u)r',Pink], C),
	jpl_call( F, dispose, [], _),
	(	C == @(null)
	->	write( 'you cancelled')
	;	write( 'you chose '), write( C)
	),
	nl.


% this directive runs the above demo

:- jpl_colour_choose_demo.

