universe :-
	send(new(P, picture), open),
	send(P, display, new(PB, parbox), point(25, 0)),
	send(P, resize_message,
	     message(PB, line_width, @arg2?width-50)),

	send_list(PB,
		  [ cdata('About the Universe', @bold_style),
		    append(@section_skip),
		    append(@br),
		    cdata('The universe is ', @normal_style),
		    cdata('big', @bold_style),
		    cdata('!')
		  ]).
		  
:- pce_global(@bold_style,   new(style(font := bold))).
:- pce_global(@normal_style, new(style(font := normal))).
:- pce_global(@br,	     new(hbox(rubber := rubber(linebreak := force)))).
:- pce_global(@parskip,	     new(hbox(ascent := 10))).
:- pce_global(@section_skip, new(hbox(descent := 20))).

	     
