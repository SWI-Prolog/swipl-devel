/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1997 University of Amsterdam. All rights reserved.
*/

:- module(draw_config,
	  [
	  ]).
:- use_module(library(pce)).
:- use_module(library(pce_config)).
:- require([ absolute_file_name/3
	   , broadcast_request/1
	   , concat/3
	   , file_directory_name/2
	   ]).

:- pce_global(@draw_default_arrow, new(arrow)).

config(Path, Attributes) :-
	broadcast_request(config(draw_config:Path, Attributes)).

config(config/file,
       [ default('PceDraw')
       ]).
config(config/application,
       [ default('PceDraw')
       ]).

config(edit/auto_align,
       [ type(bool),
	 comment(['Startup default for `Settings/Auto Align'': ',
	          'Automatically align new and modified objects']),
	 default(true),
	 icon('16x16/valign.xpm')
       ]).
config(resources/default_font,
       [ type(font),
	 comment('Default font for text objects'),
	 default(Default)
       ]) :-
	(   get(@pce, convert, normal, font, Normal),
	    get(Normal, family, win)
	->  Default = font(helvetica, roman, 12)
	;   Default = normal
	).
config(resources/colour_palette,
       [ type(setof(colour)),
	 comment('Set of colours available in attribute viewer'),
	 default([red, green, blue, yellow, black, white])
       ]).
config(resources/fill_palette,
       [ type(setof(image)),
	 comment('Images used for filling'),
	 default([ @white_image,
		   @grey12_image,
		   @grey25_image,
		   @grey50_image,
		   @grey75_image,
		   @black_image
		 ])
       ]).
config(resources/arrows,
       [ type(setof(arrow)),
	 comment('Arrow heads for lines'),
	 default([ @draw_default_arrow
		 ])
       ]).
config(print/printer,
       [ type(name),
	 comment('Name of the default printer.  May be $VARIABLE'),
	 default(DefPrinter)
       ]) :-
	\+ get(@pce, operating_system, win32),
	(   get(@pce, environment_variable, 'PRINTER', _DefPrinter)
	->  DefPrinter = '$PRINTER'
	;   DefPrinter = 'PostScript'
	).
config(print/print_command,
       [ type(name),
	 comment(['Command to send PostScript file to printer.  ',
		  '%p refers to the printer, %f to the (temporary) ',
		  'PostScript file']),
	 default('lpr -P%p %f')
       ]) :-
	\+ get(@pce, operating_system, win32).
config(file/save_prototypes,
       [ type(bool),
	 comment('Save user prototypes with drawing.'),
	 default(true)
       ]).
config(file/save_postscript_on_save,
       [ type(bool),
	 comment(['Automatically save drawing as PostScript after ',
		  'saving as PceDraw .pd file'
		 ]),
	 default(false)
       ]).
config(file/save_metafile_on_save,
       [ type(bool),
	 comment(['Automatically save drawing as Windows metafile after ',
		  'saving as PceDraw .pd file'
		 ]),
	 default(false)
       ]) :-
	get(@pce, operating_system, win32).
config(file/postscript_file_extension,
       [ type(name),
	 comment(['Extension for PostScript files.  ',
		  'PceDraw generated PostScript satisfies the rules ',
		  'for `Encapsulated PostScript''']),
	 default('eps')
       ]).
config(file/meta_file_format,
       [ type({aldus,wmf,emf}),
	 comment(['WMF is Windows 3.1 metafile format.  ALDUS is WMF with',
		  'additional information. EMF is Windows 95 and NT',
		  'metafile format.  Despite this, most',
		  'Windows 95 and NT applications read/write only the',
		  'ALDUS format.'
		 ]),
	 default(aldus)
       ]) :-
	get(@pce, operating_system, win32).
config(history/recent_files,
       [ type(setof(file)),
	 comment('Recently visited files'),
	 editable(false)
       ]).
config(history/geometry/main_window,
       [ type(geometry),
	 comment('(X-)geometry specification of main window'),
	 editable(false),
	 default('600x600')
       ]).
config(history/geometry/attributes,
       [ type(point),
	 comment('X,Y offset of attribute editor relative to main window'),
	 editable(false)
       ]).

:- register_config_type(arrow,		[ editor(arrow_item),
					  term([ length,
						 wing,
						 style,
						 fill_pattern
					       ]+pen+colour),
					  icon('16x16/arrows.bm')
					]).
:- register_config_type(setof(arrow),	[ editor(arrow_set_item),
					  icon('16x16/arrows.bm')
					]).
:- register_config_type(setof(image),	[ editor(image_set_item),
					  icon('16x16/fillpattern.bm')
					]).

:- register_config(config).

		 /*******************************
		 *	      CLASSES		*
		 *******************************/

:- pce_autoload(arrow_item, 	   library(pce_arrow_item)).
:- pce_autoload(image_item,	   library(pce_image_item)).
:- pce_autoload(set_item,   	   library(pce_set_item)).
:- pce_autoload(graphical_browser, library(pce_graphical_browser)).

:- pce_begin_class(arrow_set_item, set_item,
		   "Editor for a set of arrows").

initialise(ASI, Name:[name], Default:[chain], Msg:[code]*) :->
	new(B, graphical_browser(@default, @default,
				 ?(@prolog, make_arrow_line, @arg1),
				 @arg1?second_arrow)),
	send(B, single_column, @on),
	send(ASI, send_super, initialise, new(arrow_item), Name, Msg, B),
	(   Default \== @default
	->  send(ASI, selection, Default)
	;   true
	).

make_arrow_line(Arrow, Line) :-
	new(Line, line(0, 0, 50, 0)),
	send(Line, second_arrow, Arrow).

:- pce_end_class.


:- pce_begin_class(image_set_item, set_item,
		   "Editor for a set of images").

initialise(ASI, Name:[name], Default:[chain], Msg:[code]*) :->
	new(B, graphical_browser(@default, @default,
				 create(bitmap, @arg1),
				 @arg1?image)),
	send(ASI, send_super, initialise, new(II, image_item), Name, Msg, B),
	(   absolute_file_name(pce('bitmaps/patterns'),
			       [ file_type(directory),
				 access(read)
			       ], PatternDir)
	->  send(II, directory, PatternDir)
	;   true
	),
	(   Default \== @default
	->  send(ASI, selection, Default)
	;   true
	).

selection(ASI, Sel:chain) :<-
	get(ASI, get_super, selection, Sel0),
	get(Sel0, map, ?(@prolog, map_selected_image, @arg1), Sel).

map_selected_image(@Named, @Named) :-
	atom(Named), !.
map_selected_image(Image, Named) :-
	get(Image, name, @nil),
	get(Image, file, File),
	get(Image, resource_value, path, SearchPath),
	local_name(SearchPath, File, Name), !,
	new(Named, image(Name)).
map_selected_image(Image, Image).

local_name(SearchPath, File, Name) :-
	get(File, absolute_path, FileName),
	base_name(FileName, Name),
	new(F, file(Name)),
	pce_catch_error(cannot_find_file, send(F, find, SearchPath, read)),
	send(F, same, File), !.

base_name(FileName, Name) :-
	file_directory_name(FileName, Dir),
	base_name(FileName, Dir, Name).

base_name(FileName, Dir0, Local) :-
	get(Dir0, ensure_suffix, /, Dir),
	concat(Dir, Local, FileName).
base_name(_, /, _) :- !,
	fail.
base_name(_, '', _) :- !,
	fail.
base_name(FileName, Dir, Local) :-
	file_directory_name(Dir, Super),
	base_name(FileName, Super, Local). 

:- pce_end_class.

