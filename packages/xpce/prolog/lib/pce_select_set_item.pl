:- module(pce_select_set_item, []).
:- use_module(library(pce)).
:- use_module(library(pce_util)).

resource(back,    image, image('16x16/back.xpm')).
resource(forward, image, image('16x16/forward.xpm')).

:- pce_begin_class(select_set_item, dialog_group,
		   "Select objects from a set").

initialise(SSI, Name:name, Width:[int], Height:[int]) :->
	send_super(SSI, initialise, Name, group),
	send(SSI, attribute, hor_stretch, 100),
	send(SSI, attribute, ver_stretch, 100),
	send(SSI, attribute, ver_shrink, 100),
	send(SSI, display, new(LB, list_browser(@default, Width, Height))),
	send(LB, name, left),
	send(LB, multiple_selection, @on),
	send(SSI, display, new(RB, list_browser(@default, Width, Height))),
	send(RB, name, right),
	send(RB, multiple_selection, @on),
	send(SSI, display, new(Back, button(back))),
	send(Back, label, image(resource(back))),
	send(SSI, display, new(Forw, button(forward))),
	send(Forw, label, image(resource(forward))),
	get(LB?area, size, size(BW, BH)),
	send(SSI, size, size(BW*2+50, BH)),
	send(SSI, layout_dialog).

layout_dialog(SSI) :->
	get(SSI, size, size(TW, TH)),
	(   get(SSI, border, size(BW, BH))
	->  true
	;   get(SSI, gap, size(BW, BH))
	),
	get(SSI, member, left, LB),
	get(SSI, member, right, RB),
	get(SSI, member, forward, B1),
	get(SSI, member, back, B2),
	BrowserSep = 50,
	BrowserWidth is (TW-2*BW-BrowserSep)//2,
	BrowserHeigth is (TH-2*BH),
	send(LB, do_set,
	     BW, BH,
	     BrowserWidth, BrowserHeigth),
	send(RB, do_set,
	     BW+BrowserWidth+BrowserSep, BH,
	     BrowserWidth, BrowserHeigth),
	get(B1, size, size(B1W, B1H)),
	ButtonW is BW + BrowserWidth + (BrowserSep-B1W)//2,
	B1Y is BH + (BrowserHeigth-2*B1H-15)/2,
	B2Y is B1Y + B1H + 15,
	send(B1, set, ButtonW, B1Y),
	send(B2, set, ButtonW, B2Y).

forward(SSI) :->
	"Move objects from left to right"::
	get(SSI, member, left, LB),
	get(SSI, member, right, RB),
	get(LB, selection, Chain),
	send(Chain, for_all, message(RB, append, @arg1)),
	send(RB, sort).

back(SSI) :->
	"Move objects from right to left"::
	get(SSI, member, left, LB),
	get(SSI, member, right, RB),
	get(RB, selection, Chain),
	send(Chain, for_all, message(LB, append, @arg1)),
	send(LB, sort).

append(SSI, DI:dict_item) :->
	"Append item to the set to select from"::
	get(SSI, member, left, LB),
	send(LB, append, DI).

clear(SSI) :->
	"Clear left browser"::
	get(SSI, member, left, LB),
	send(LB, clear).

selection(SSI, Items:chain) :<-
	"Get currently selected items"::
	get(SSI, member, right, RB),
	get(RB, members, Items).

:- pce_end_class(select_set_item).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
test :-
	D = @d,
	free(D),
	new(D, dialog),
	send(D, append, new(S, select_set_item(files))),
	expand_file_name('*.train', Files),
	send_list(S, append, Files),
	send(D, resize_message, message(D, layout, D?size)),
	send(D, append, button(ok,
			       message(@prolog, portray_object,
				       S?selection))),
	send(D, append, button(cancel, message(D, destroy))),
	send(D, open).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */



