:- use_module(library(jpl)).

% shows a JOptionPane dialog, on top of a (necessary) new JFrame,
% and awaits text entry and OK/Cancel button click

jpl_text_entry_demo :-
	jpl_new( 'javax.swing.JFrame', ['frame with dialog'], F),
	jpl_call( F, setLocation, [400,300], _),
	jpl_call( F, setSize, [400,300], _),
	jpl_call( F, setVisible, [@(true)], _),
	jpl_call( F, toFront, [], _),
	jpl_call( 'javax.swing.JOptionPane', showInputDialog, [F,'type your name'], N),
	jpl_call( F, dispose, [], _), 
	(	N == @(null)
	->	write( 'you cancelled')
	;	write( 'you typed '), write( N)
	),
	nl.


% this directive runs the above demo

:- jpl_text_entry_demo.

