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

		 /*******************************
		 *	CHECK ENVIRONMENT	*
		 *******************************/

has_metafile :-
	get(@pce, convert, win_metafile, class, _).
has_printer_class :-
	get(@pce, convert, win_printer, class, _).

		 /*******************************
		 *	   CONFIG STUFF		*
		 *******************************/

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
	\+ has_printer_class,
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
	\+ has_printer_class.
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
	has_metafile.
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
		  'metafile format.  Use EMF whenever possible as its',
		  'scaling behaviour is much better'
		 ]),
	 default(emf)
       ]) :-
	has_metafile.
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

resource(arrows,	image, image('16x16/arrows.bm')).
resource(fillpattern,	image, image('16x16/fillpattern.bm')).

:- register_config_type(arrow,		[ editor(arrow_item),
					  term([ length,
						 wing,
						 style,
						 fill_pattern
					       ]+pen+colour),
					  icon(arrows)
					]).
:- register_config_type(setof(arrow),	[ editor(arrow_set_item),
					  icon(arrows)
					]).
:- register_config_type(setof(image),	[ editor(image_set_item),
					  icon(fillpattern)
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
				 access(read),
				 file_errors(fail)
			       ], PatternDir)
	->  send(II, directory, PatternDir)
	;   true
	),
	(   Default \== @default
	->  good_patterns(Default, Patterns),
	    send(ASI, selection, Patterns)
	;   true
	).

good_patterns(ChainIn, ChainOut) :-
	chain_list(ChainIn, ListIn),
	realise_patterns(ListIn, ListOut),
	chain_list(ChainOut, ListOut).

realise_patterns([], []).
realise_patterns([Image|T0], [Image|T]) :-
	object(Image),
	send(Image, instance_of, image), !,
	realise_patterns(T0, T).
realise_patterns([Name|T0], [Image|T]) :-
	pce_catch_error(_Error, new(Image, image(Name))), !,
	realise_patterns(T0, T).
realise_patterns([_|T0], T) :-
	realise_patterns(T0, T).

selection(ASI, Sel:chain) :<-
	get(ASI, get_super, selection, Sel0),
	get(Sel0, map, ?(@prolog, map_selected_image, @arg1), Sel).

map_selected_image(@Named, @Named) :-
	atom(Named), !.
map_selected_image(Image, Named) :-
	get(Image, name, @nil),
	get(Image, file, File),
	get(Image, path, SearchPath),
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

