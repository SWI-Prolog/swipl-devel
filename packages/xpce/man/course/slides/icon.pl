:- pce_begin_class(icon, device).

initialise(I, Image:image, Label:name) :->
	"Create from image and label"::
	send(I, send_super, initialise),
	send(I, display, bitmap(Image)),
	send(I, display, text(Label, center)),
	send(I, reposition).

reposition(I) :->
	get(I, member, bitmap, Bitmap),
	get(I, member, text, Text),
	get(Bitmap, center_x, CX),
	get(Bitmap, bottom_side, BS),
	get(Text, width, W),
	TX is CX - W//2,
	send(Text, set, TX, BS).

label(I, Text:name) :->
	"Change the label"::
	get(I, member, text, Text),
	send(Text, string, Text).

:- pce_end_class.
