/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (C): 1985-2002, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(pce_progress,
	  [ progress_checklist/3	% :Goal, +List, +Title
	  ]).
:- meta_predicate(progress_checklist(:,+,+)).

:- use_module(library(pce)).
:- use_module(library(lists)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module defines primitives for progress-bars.  Classes:

	* progress_bar
	Creates a progress bar with given length and scale (=100% value).
	User must supply the current value using ->value.  The latter
	forces a display synchronisation.

	* listening_progress_bar
	Similar to class progress_bar, but designed to interact with
	procedures using the broadcasting service to send the current
	value.  The advantage of this approach is that the computing
	routine need not know whether there is a progress bar associated
	to it.  The `Id' argument denotes the progress bar.  The cumputing
	routine must use the following broadcast messages:

		broadcast(progress(Id, start(Scale)))
		broadcast(progress(Id, at(Here)))

	The first one indicates the start and defines the length of the
	scale (a number).  The second must be send repeatetly to update
	the value.  When using a background thread, use

		in_pce_thread(broadcast(progress(Id, at(Here))))

	* progress_dialog
	Dialog to act as placeholder for one or more progress-bars.  Of
	course they can also be placed in a custom dialog.

EXAMPLE:

	new(B, listening_progress_bar(progress, read_file)),
	new(D, progress_dialog('Processing file', B)),
	send(D, open),
	process_file(File),
	send(D, destroy).

process_file(File) :-
	size_file(File, Size),
	broadcast(progress(read_file, start(Size))),
	open(File, read, In),
	....
	character_count(In, Here),
	broadcast(progress(read_file, at(Here)),
	....

BUGS:

There is some code dealing with aborting  the slow task. This however is
not completed. I do not yet have a  clear view on how to implement that.
One option is to  raise  an  exception   on  the  next  update, but this
wouldn't work using the background thread id.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(progress_bar, label_box,
		   "Show progress to the user").

variable(scale,	 'int|real',      get, "100% value").
variable(value,  'int|real' := 0, get, "Current value").
variable(length, int,             get, "Width of the slider-part").

class_variable(length,	   int,	   200).
class_variable(bar_colour, colour, mediumblue).

initialise(PB, Name:name=[name],
	   Scale:scale=[int|real], Length:length='[0..]') :->
	send_super(PB, initialise, Name),
	default(Scale, 100, TheScale),
	(   Length \== @default
	->  send(PB, slot, length, Length)
	;   true
	),
	send(PB, scale, TheScale),
	send(PB, solid, @on),
	(   Name == @default
	->  send(PB, show_label, @off),
	    send(PB, reference, point(0,PB?height))
	;   true
	).

bar(PB, Bar:box) :<-
	"Get access to the box realising the bar"::
	get(PB, member, figure, Fig1),
	get(Fig1, member, figure, Fig2),
	get(Fig2, member, box, Bar).

scale(PB, Scale:[int|real]) :->
	"Set the scale"::
	send(PB, slot, scale, Scale),
	send(PB, clear),
	get(PB, bar_colour, C),
	send(PB, append, new(F, figure)),
	get(PB, label_font, LF),
	get(LF, descent, Descent),
	send(F, attribute, reference, point(0, 20-Descent-2)),
	send(F, elevation, elevation(@nil, -1, white, background := C)),
	get(PB, length, Len),
	Height = 20,
	send(F, display, graphical(0,0,Len,Height)),
	send(F, display, new(F2, figure)),
	send(F2, border, 1),
	send(F2, background, C),
	send(F2, elevation, elevation(@nil, 1)),
	send(F2, display, new(B, box(0,Height-4)), point(2,2)),
	send(B, pen, 0),
	send(B, fill_pattern, C).

value(BP, Val:[int|real]) :->
	"Set progress"::
	(   get(BP, value, Val)
	->  true
	;   send(BP, slot, value, Val),
	    get(BP, bar, Box),
	    get(BP, length, Len),
	    get(BP, scale, Scale),
	    PixVal is round((Len-4)*(Val/Scale)),
	    send(Box, width, PixVal)
	),
	send(BP, synchronise).		% Flush?

open(BP) :->
	"Open on behalf of a client"::
	new(D, progress_dialog(BP, BP?name)),
	send(D, open).

:- pce_end_class(progress_bar).


:- pce_begin_class(listening_progress_bar, progress_bar,
		   "Progress bar listening to broadcast messages").

initialise(PB, Name:name=[name],
	   Id:prolog, Length:length='[0..]') :->
	send_super(PB, initialise, Name),
	(   Length \== @default
	->  send(PB, slot, length, Length)
	;   true
	),
	send(PB, scale, 100),
	send(PB, solid, @on),
	(   Name == @default
	->  send(PB, show_label, @off),
	    send(PB, reference, point(0,PB?height))
	;   true
	),
	listen(PB, progress(Id, Message),
	       send(PB, progress, Message)).

unlink(PB) :->
	unlisten(PB),
	send_super(PB, unlink).

id(PB, Id:prolog) :->
	"Prepare to listen to another channal"::
	unlisten(PB),
	listen(PB, progress(Id, Message),
	       send(PB, progress, Message)),
	send(PB, value, 0).

progress(PB, Message:prolog) :->
	(   Message = start(Scale)
	->  send(PB, scale, Scale)
	;   Message = at(Here)
	->  send(PB, value, Here)
	;   true
	).

:- pce_end_class(listening_progress_bar).



:- pce_begin_class(progress_dialog, dialog,
		   "Dialog for monitoring progress").

variable(can_abort, bool := @off, both, "Can user abort progress?").

initialise(PD, Title:name, PBS:progress_bar ...) :->
	send_super(PD, initialise, Title?label_name),
	(   PBS = [One]
	->  send(One, show_label, @off),
	    send(PD, append, One)
	;   forall(member(PB, PBS), send(PD, append, PB))
	),
	send(PD, done_message, message(PD, abort)),
	(   CanAbort == @on
	->  send(PD, can_abort, CanAbort)
	;   true
	).

open(PD) :->
	"Make transient for current frame"::
	send(PD, update_can_abort),
	(   send(@event, instance_of, event),
	    get(@event?window, frame, Frame)
	->  send(PD, transient_for, Frame),
	    send(PD, open_centered, Frame?area?center)
	;   send(PD, open_centered)
	),
	send(PD, wait).

resize(PD) :->
	send(PD, layout, PD?area?size).

update_can_abort(PD) :->
	(   get(PD, can_abort, @on)
	->  (   get(PD, member, abort, B)
	    ->	true
	    ;	send(PD, append, new(B, button(abort))),
		send(B, alignment, right)
	    )
	;   (   get(PD, member, abort, B)
	    ->  free(B)
	    ;	true
	    )
	).

abort(PD) :->
	(   get(PD, can_abort, @on)
	->  send(PD, destroy)
	;   send(PD, report, warning, 'Cannot abort')
	).

:- pce_end_class(progress_dialog).


		 /*******************************
		 *  HIGH-LEVEL PROLOG INTERFACE	*
		 *******************************/

%%	progress_checklist(:Goal, +List, +Options)
%	
%	As checklist/2, but show a progress-bar while processing the
%	elements of the list.

progress_checklist(Goal, List, Options) :-
	strip_module(Goal, M, G),
	do_progress_checklist(M:G, List, Options).

do_progress_checklist(Goal, List, Options) :-
	length(List, Len),
	UpdateStep is max(1, Len//100),
	option(title(Title), Options, progress),
	option(abort(Abort), Options, @off),
	new(PD, progress_dialog(Title,
				new(PB, progress_bar(Title, Len)))),
	send(PD, can_abort, Abort),
	send(PD, open),
	(   catch(pchecklist(List, Goal, 0, UpdateStep, PB),
		  E, true)
	->  premove(PB),
	    (	nonvar(E)
	    ->	(   E = error(existence_error(pce(object), PB), _Context)
		->  throw(aborted)
		;   throw(E)
		)
	    ;	true
	    )
	;   premove(PB),
	    fail
	).
		
option(Term, List, _Default) :-
	memberchk(Term, List), !.
option(Term, _, Default) :-
	arg(1, Term, Default).

premove(PB) :-
	object(PB), !,
	send(PB?frame, destroy).
premove(_).


pchecklist([], _, _, _, _).
pchecklist([Elem|Tail], Goal, N, Step, PB) :-
	call(Goal, Elem), 
	NN is N + 1,
	(   0 =:= NN mod Step
	->  send(PB, value, NN)
	;   true
	),
	pchecklist(Tail, Goal, NN, Step, PB).


