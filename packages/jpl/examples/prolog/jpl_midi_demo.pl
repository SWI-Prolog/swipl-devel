:- use_module(library(jpl)).

jpl_midi_demo :-
	jpl_midi_demo( 20).	% play 20 random notes


jpl_midi_demo( N) :-
	jpl_call( 'javax.sound.midi.MidiSystem', getSynthesizer, [], Synth),
	jpl_call( Synth, open, [], _),
	jpl_call( Synth, getChannels, [], Channels),
	jpl_get( Channels, 0, Channel0),	% i.e. Channel0 = Channels[0]
	jpl_midi_demo( N, Channel0),
	jpl_call( Synth, close, [], _).


jpl_midi_demo( N, Channel) :-
	(	N @> 0
	->	Note is 50+random(50),	% see MIDI docs for pitch relationship
		Velocity is 100,		% arbitrary value > 0
		jpl_call(  Channel, noteOn, [Note,Velocity], _),
		sleep( 0.5),	% play note for approx 0.5 seconds
		jpl_call(  Channel, noteOff, [Note], _),
		Nx is N-1,
		jpl_midi_demo( Nx, Channel)	% play remaining notes
	;	true
	).


% this directive runs the above demo

:- jpl_midi_demo.

