/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

:- module(pce_imageviewer, [image_viewer/0]).
:- use_module(library(pce)).
:- require([ chain_list/2
	   , concat/3
	   , concat_atom/2
	   , send_list/3
	   , shell/1
	   ]).

image_viewer :-
	new(P, picture),
	send(P, scrollbars, vertical),
	send(P, background, when(@colour_display,
				 colour(grey80),
				 @grey12_image)),
	send(P, format, format(horizontal, 800, @off)),
	send(P, resize_message,
	     message(P, format, width, @arg2?width)),
	send(new(D, dialog), below, P),
	send(P?frame, label, 'Image Viewer'),
	get(@pce, home, Home),
	concat(Home, '/bitmaps', DefDir),
	send(D, append, new(Dir, text_item(directory, DefDir))),
	send(D, append, new(File, text_item(file_pattern, regex('\.bm$')))),
	send(D, append, button(apply,
			       message(@prolog, view, P,
				       Dir?selection, File?selection))),
	send(D, append, button(quit,
			       message(P, destroy))),
	send(D, default_button, apply),
	send(P, open).


view(P, Dir, Pattern) :-
	new(D, directory(Dir)),
	get(D, files, Pattern, Files),
	(   send(Files, empty)
	->  send(@display, inform, 'No matching images')
	;   send(P, clear),
	    send(P?frame, label, string('Images from %s/%s',
					D?path, Pattern?pattern)),
	    chain_list(Files, List),
	    show(P, List, D)
	).


:- pce_global(@image_recogniser, make_image_recogniser).

make_image_recogniser(R) :-
	new(R, handler_group),
	send(R, append, click_gesture(left, '', single,
				      message(@receiver, inverted,
					      @receiver?inverted?negate))),
	send(R, append, popup_gesture(new(P, popup))),
	new(Bitmap, @event?receiver),
	new(BitmapName, Bitmap?file?name),
	send_list(P, append,
		  [ menu_item(run_bitmap_editor,
			      message(@prolog, edit_image, Bitmap),
			      @default, @on)
		  , menu_item(reload,
			      message(@prolog, reload, Bitmap),
			      @default, @off)
		  , menu_item(resize,
			      message(@prolog, resize, Bitmap),
			      @default, @off)
		  , menu_item(convert_to_x11,
			      message(@prolog, convert, Bitmap),
			      @default, @off)
		  , menu_item(remove,
			      and(message(@display, confirm,
					  'Remove %s', BitmapName),
				  message(@prolog, remove, Bitmap)),
			      @default, @on)
%		  , menu_item(rename,
%			      message(@prolog, rename, Bitmap),
%			      @default, @on)
		  ]).


convert(Bitmap) :-
	get(Bitmap, file, File),
	send(File, backup),
	send(Bitmap?image, save, File).


remove(Bitmap) :-
	get(Bitmap, file, File),
	send(File, remove),
	send(Bitmap, free).


reload(Bitmap) :-
	send(Bitmap?image, load),
	send(Bitmap, redraw).


edit_image(Bitmap) :-
	get(Bitmap?file, name, File),
	concat_atom(['bitmap ', File, ' &'], Cmd),
	shell(Cmd).


rename(Bitmap) :-
	get(Bitmap, file, _File).


resize(Bitmap) :-
	get(Bitmap, file, File),
	get(File, name, FileName),
	new(D, dialog(string('Resize image %s', FileName))),
	send(D, append, label(image, Bitmap)),
	send(D, append, new(W, text_item(width, Bitmap?width))),
	send(D, append, new(H, text_item(height, Bitmap?height))),
	send(D, append, button(ok,
			       and(message(@prolog, resize,
					   Bitmap, W?selection, H?selection),
				   message(D, destroy)))),
	send(D, append, button(cancel,
			       message(D, destroy))),
	send(D, default_button, ok),
	send(D, open).

resize(Bitmap, W, H) :-
	get(Bitmap, file, File),
	new(I2, image(@nil, W, H)),
	send(I2, draw_in, Bitmap, point(0,0)),
	send(Bitmap, image, I2),
	send(File, backup),
	send(I2, save, File),
	send(I2, load, File).

:- pce_global(@image_viewer_icon_spatial,
	      new(spatial(xref=x+w/2, yref=y+h+5,
			  xref=x+w/2, yref=y))).

show(_, [], _) :- !.
show(P, [F|R], Dir) :-
	get(Dir, file, F, File),
	new(B, bitmap(File?name)),	% the bitmap
	send(B, recogniser, @image_recogniser),

	new(F2, figure),		% elevate it from the background
	send(F2, border, 3),
	send(F2, elevation, 1),
	send(F2, display, B),

	new(D, device),			% put together with label
	send(D, display, F2),
	send(D, display, new(T, text(F, center))),
	send(@image_viewer_icon_spatial, forwards, F2, T),

	send(P, display, D),		% display (<-format positions)
	show(P, R, Dir).
show(P, [_|R], Dir) :-
	show(P, R, Dir).
