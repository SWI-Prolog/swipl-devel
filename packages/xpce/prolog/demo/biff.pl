/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/


:- module(pce_mail,
	  [ biff/0
	  ]).

:- use_module(library(pce)).

biff :-
	send(new(biff), open).


:- pce_begin_class(biff, picture).

resource(mail_dir,	directory,	'"/usr/spool/mail"').
resource(interval,	int,		30, "Polling interval").
resource(position,	point,		'point(0,0)', "Location on the screen").

variable(in_box,	file,		get,
	 "Name of incoming mail-file").
variable(in_box_modified, date,		both,
	 "Last modification time of in_box").
variable(in_box_size,	int,		both,
	 "Size of inbox").
variable(timer,		timer*,		get,
	 "Associated timeer").

initialise(B) :->
	send(B, send_super, initialise, xbiff, size(52, 52)),
	send(B, scrollbars, none),
	send(B, border, 0),
	get(B, resource_value, mail_dir, Dir),
	get(@pce, user, UserName),
	send(B, slot, in_box, string('%s/%s', Dir?path, UserName)),
	send(B, in_box_modified, new(date)),
	send(B, display, new(F, figure)),
	send(F, display, new(NM, bitmap(flagup))),
	send(F, display, new(M, bitmap(flagdown))),
	send(F, recogniser, click_gesture(left, '', single,
					  message(B, status, nomail))),
	send(NM, name, nomail),
	send(M, name, mail),
	send(M, inverted, @on),
	send(B, slot, timer, timer(?(B, resource_value, interval),
				   message(B, update))).

open(B, Pos:[point]) :->
	"Open and start the timer"::
	default(Pos, ?(B, resource_value, position), P),
	send(B, send_super, open, P),
	send(B, status, nomail),
	send(B?timer, start).


unlink(B) :->
	"Remove timer before unlinking"::
	send(B, slot, timer, @nil),
	send(B, send_super, unlink).


update(B) :->
	"Update the status of the biff window"::
	(   get(B, status, nomail),
	    send(B?in_box, exists),
	    get(B?in_box, time, Date),
	    send(Date, after, B?in_box_modified),
	    send(B, in_box_modified, Date),
	    get(B?in_box, size, Size),
	    get(B, in_box_size, OldSize),
	    send(B, in_box_size, Size),
	    Size > OldSize
	->  send(B, status, mail)
	;   true
	).


status(B, Status:name) :->
	"Change status of mail"::
	send(B?figure_member, status, Status),
	(   Status == nomail
	->  send(B, in_box_size, B?in_box?size),
	    send(B, in_box_modified, B?in_box?time)
	;   true
	).

status(B, Status) :<-
	get(B?figure_member, status, Status).

:- pce_end_class.
