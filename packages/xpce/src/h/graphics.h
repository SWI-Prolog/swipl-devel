/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (C): 1985-2002, University of Amsterdam

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

#ifndef _PCE_GRA_INCLUDED
#define _PCE_GRA_INCLUDED

		 /*******************************
		 *     STRETCHABLE OBJECTS	*
		 *******************************/

typedef struct
{ int	ideal;				/* ideal size */
  int   minimum;			/* minimum size */
  int	maximum;			/* maximum size */
  int	stretch;			/* stretch handicap */
  int	shrink;				/* shrink handicap */
  int	size;				/* resulting size */
} stretch, *Stretch;

		 /*******************************
		 *	    TABLES, ETC.	*
		 *******************************/

#include <h/layout.h>

#define MAX_WRAP_LINES	100		/* line-wraps in text-objects */

typedef struct update_area *UpdateArea;	/* Window changes data  */
typedef struct colour_context *ColourContext; /* for selection/inactive */

#define ABSTRACT_GRAPHICAL \
  ABSTRACT_VISUAL \
  Device     	device;			/* device on which displayed */ \
  Area	     	area;			/* area (bounding box) */ \
  Bool       	displayed;		/* is graphical object displayed? */ \
  Int        	pen;			/* pen thickness for this object */ \
  Name	     	texture;		/* dash pattern for lines */ \
  Any		colour;			/* colour of the graphical */ \
  Chain		handles;		/* handles connected to graphical */ \
  Chain		connections;		/* Available connections */ \
  Name		name;			/* name to find it */ \
  Bool		selected;		/* is graphical selected? */ \
  Bool		inverted;		/* inverted image */ \
  Bool		active;			/* (de)activated */ \
  CursorObj	cursor;			/* cursor of the graphical */ \
  LayoutInterface layout_interface;	/* Interface to layout manager */ \
  Any	     	request_compute;	/* graphical needs recomputed */

#define ABSTRACT_DEVICE \
  ABSTRACT_GRAPHICAL \
  Int	        level;			/* Level from root */ \
  Point	        offset;			/* Origin offset */ \
  Area	        clip_area;		/* Visible subarea */ \
  Chain	        graphicals;		/* Displayed graphicals */ \
  Chain	        pointed;		/* Graphicals on last event */ \
  LayoutManager layout_manager;		/* Manage graphicals layout */ \
  Format        format;			/* Row/Column formatting */ \
  Bool	        badFormat;		/* Formats needs to be recomputed */ \
  Bool	        badBoundingBox;		/* BoundingBox needs recomputed */ \
  Chain	        recompute;		/* Graphicals requesting recompute */

#define ABSTRACT_FIGURE \
  ABSTRACT_DEVICE \
  Name		status;			/* Which members are displayed? */ \
  Image		background;		/* Pattern for background */ \
  Int		border;			/* border around graphicals */ \
  Int		radius;			/* radius of outline */ \
  Elevation	elevation;		/* elevation of outline */

#define ABSTRACT_DIALOGITEM \
  ABSTRACT_GRAPHICAL \
  Any	     label;			/* Label of the item */ \
  FontObj    label_font;		/* Font used for the label */ \
  Int	     label_width;		/* Width of the label */ \
  Name	     label_format;		/* Alignment of label in box */ \
  Any	     background;		/* Colour or Image for background */ \
  Name	     status;			/* inactive, focus, preview, execute*/\
  Code	     message;			/* Execution message */ \
  PopupObj   popup;			/* Popup associated with item */ \
  Name	     look;			/* Look and feel switch */ \
  Bool	     auto_align;		/* Item is automatically aligned */ \
  Point	     reference;			/* Reference point of item */ \
  DialogItem above;			/* Item above me */ \
  DialogItem below;			/* Item below me */ \
  DialogItem right;			/* Item right of me */ \
  DialogItem left;			/* Item left of me */ \
  Name	     alignment;			/* Align in the column? */ \
  Bool	     auto_label_align;		/* Automatically align label? */ \
  Bool	     auto_value_align;		/* Automatically align value? */ \
  Name	     accelerator;		/* Associated accelerator */


#define ABSTRACT_JOINT \
  ABSTRACT_GRAPHICAL \
  Graphical	first_arrow;		/* arrow on start point */ \
  Graphical	second_arrow;		/* arrow on end point */

#define ABSTRACT_LINE \
  ABSTRACT_JOINT \
  Int		start_x;		/* X-start point */ \
  Int		start_y;		/* Y-start point */ \
  Int		end_x;			/* X-end point */ \
  Int		end_y;			/* Y-end point */


		/********************************
		*         DEVICE CLASSES	*
		********************************/

NewClass(graphical)
  ABSTRACT_GRAPHICAL
End;
  
NewClass(device)
  ABSTRACT_DEVICE
End;

NewClass(figure)
  ABSTRACT_FIGURE
End;

#define ABSTRACT_DIALOG_GROUP \
  ABSTRACT_DEVICE \
  Any		label;			/* Displayed textual-label */ \
  FontObj	label_font;		/* Font used for label */ \
  Name		label_format;		/* Alignment of the label */ \
  Elevation	elevation;		/* Elevation for the box */ \
  Int		radius;			/* corners rounding radius */ \
  Size		size;			/* Size of the drawing area */ \
  Size		gap;			/* Layout gap for items (dialog) */ \
  Size		border;			/* Border around graphicals */ \
  Bool		auto_align;		/* Align in dialog window? */ \
  Name		alignment;		/* Row alignment */

NewClass(dialog_group)
  ABSTRACT_DIALOG_GROUP
End;  

NewClass(tab)
  ABSTRACT_DIALOG_GROUP
  Size		label_size;		/* Size of the label-box */
  Int		label_offset;		/* X-Offset of the label-box */
  Name		status;			/* {on_top, hidden} */
End;

NewClass(label_box)
  ABSTRACT_DIALOG_GROUP
  Int		label_width;		/* Width of box holding the label */
  Bool		auto_label_align;	/* Automatically align label */
  Code		message;		/* associated message */
  Any		default_value;		/* default */
  Bool		modified;		/* item has been modified */
End;

NewClass(tab_stack)
  ABSTRACT_DEVICE
End;

		/********************************
		*           RELATIONS		*
		********************************/

NewClass(format)
  Name		direction;	/* horizontal, vertical */
  Int		width;		/* Width/Height */
  Bool		columns;	/* W/H in columns (@on)/pixels (@off) */
  Int		column_sep;	/* separation between columns */
  Int		row_sep;	/* separation between rows */
  Vector	adjustment;	/* left/center/right vector for comlumn mode */
End;

NewClass(tileobj)
  Int	     	idealWidth;		/* Desired width of the tile */
  Int	     	idealHeight;		/* Idem for height */
  Int	     	horStretch;		/* Horizontal stretchability */
  Int	     	horShrink;		/* Horizontal shrinkability */
  Int	     	verStretch;		/* Vertical stretchability */
  Int	     	verShrink;		/* Vertical shrinkability */
  Bool	     	canResize;		/* Can be resized by user? */
  TileAdjuster	adjuster;		/* Object that resizes me */
  Int	     	border;			/* Border between subtiles */
  Name       	orientation;		/* none, horizontal, vertical */
  Chain	     	members;		/* subtiles */
  TileObj    	super;			/* Super tile */
  Any	     	object;			/* Object managed */
  Area	     	area;			/* Current area of the tile */
  Bool	     	enforced;		/* Enfore layout */
End;

NewClass(tree)
  ABSTRACT_FIGURE
  Node		root;			/* the real root root node */
  Node		displayRoot;		/* node displayed as root */
  Bool		auto_layout;		/* Enforce automatic layout? */
  Int		levelGap;		/* distance parent/son */
  Int		neighbourGap;		/* distance sons */
  Int		linkGap;		/* distance link and graphical */
  Name		direction;		/* NAME_horizonal/vertical/list */
  Link		link;			/* link  between son/parent */
  Handle	parentHandle;		/* connection parent handle */
  Handle	sonHandle;		/* connection son handle */
  Chain		rootHandlers;		/* handlers for root node */
  Chain		leafHandlers;		/* handlers for leaf nodes */
  Chain		nodeHandlers;		/* handlers for other nodes */
  Chain		collapsedHandlers;	/* handlers for collapsed nodes */
End;

NewClass(connection)
  ABSTRACT_LINE
  Link		link;			/* link description of connection */
  Graphical	from;			/* `from' side graphical */
  Graphical	to;			/* `to' side graphical */
  Name		from_handle;		/* Handle-name `from' */
  Name		to_handle;		/* Handle-name `to' */
  Bool		fixed_from;		/* From is fixed */
  Bool		fixed_to;		/* To is fixed */
End;

NewClass(link)
  Line		line;			/* line to connect objects with */
  Name		from;			/* handle kind 'from' */
  Name		to;			/* handle kind 'to' */
  Class		connection_class;	/* Class to create connection from */
End;

		/********************************
		*      PRIMITIVE GRAPHICALS	*
		********************************/

NewClass(arc)
  ABSTRACT_JOINT
  Point		position;		/* Center of the arc */
  Size		size;			/* width and height */
  Real		start_angle;		/* Start angle */
  Real		size_angle;		/* Size angle */
  Name		close;			/* {none,chord,pie_slice} */
  Image		fill_pattern;		/* Filled with this pattern */
End;

NewClass(circle)
  ABSTRACT_GRAPHICAL
  Image		fill_pattern;		/* image to fill the circle */
End;

NewClass(ellipse)
  ABSTRACT_GRAPHICAL
  Int		shadow;			/* shadow displayed around ellipse */
  Image		fill_pattern;		/* fill pattern  */
End;

NewClass(bezier)
  ABSTRACT_JOINT
  Point	start;
  Point end;
  Point control1;			/* Quadratic Bezier Curve */
  Point control2;			/* Cubic Bezier Curve */
End;

NewClass(box)
  ABSTRACT_GRAPHICAL
  Int        radius;			/* if displayed as a rounded box */
  Int	     shadow;			/* shadow displayed around box */
  Image	     fill_pattern;		/* fill box with this */
  Point	     fill_offset;		/* Offset for filling */
End;

NewClass(arrow)
  ABSTRACT_GRAPHICAL
  Point      tip;		/* tip of the arrow head */
  Point      reference;		/* reference point for the arrow */
  Int        length;		/* length of the arrow head */
  Int        wing;		/* wing (width) of the arrow head */
  Image	     fill_pattern;	/* bitmap used to fill the arrow head */
  Name       style;		/* open or closed arrow */
  Point      left;
  Point      right;		/* together with tip make up the arrow head */
End;

NewClass(textobj)
  ABSTRACT_GRAPHICAL
  CharArray  string;		/* the string itself */
  FontObj    font;		/* font */
  Name       format;		/* NAME_left, NAME_center, NAME_right */
  Int	     margin;		/* Right margin (pixels) */
  Bool	     underline;		/* Underline the text? */
  Point	     position;		/* reference position of text */
  Int        caret;		/* current insertion/deletion point */
  Any	     show_caret;	/* show the caret (default OFF) */
  Any	     background;	/* Background of text */
  Int	     border;		/* Border around actual text */
  Name	     wrap;		/* Clip to width */
  Int	     x_offset;		/* Shift in X-direction (length > 0) */
  Int	     x_caret;		/* Caret X, relative to graphical */
  Int	     y_caret;		/* Caret Y, relative to graphical */
  Int	     selection;		/* Represented selection */
End;

NewClass(joint)
  ABSTRACT_JOINT
End;

NewClass(line)
  ABSTRACT_LINE
End;

NewClass(path)
  ABSTRACT_JOINT
  Point	     offset;			/* offset of the points */
  Name	     kind;			/* poly_line, smooth_line */
  Int	     radius;			/* Round corners of poly-line */
  Int	     intervals;			/* Number of iteration intervals */
  Chain	     points;			/* Points of the poly-line */
  Image	     fill_pattern;		/* Fill the path with this pattern */
  Image	     mark;			/* Mark used for points */
  Bool	     closed;			/* Line from end back to start */
  Chain	     interpolation;		/* interpolated points */
End;


		/********************************
		*          MENU CLASSES		*
		********************************/

NewClass(dialog_item)
  ABSTRACT_DIALOGITEM
End;

NewClass(label)
  ABSTRACT_DIALOGITEM
  FontObj    font;			/* Font of the text */
  Int	     length;			/* Length in characters */
  Int	     width;			/* Width of field in pixels */
  Any	     selection;			/* Currently displayed value */
  Int	     border;			/* additional space */
  Elevation  elevation;			/* Elevation of the area */
End;

#define GTK_BUTTON_MARGIN 5		/* Lowered region of default button */

NewClass(button)
  ABSTRACT_DIALOGITEM
  Int	     radius;			/* Rounding radius */
  Int	     shadow;			/* shadow around button */
  Image	     popup_image;		/* Image to indicate popup */
  Bool	     default_button;		/* Button is the default button */
  Bool	     show_focus_border;		/* Show wide border around focus */
End;

#define ABSTRACT_TEXTITEM \
  ABSTRACT_DIALOGITEM \
  Any	     selection;			/* Current selection */ \
  Any	     default_value;		/* The default (initial) value */ \
  StringObj  print_name;		/* Print-name of selection */ \
  Type	     type;			/* Type of the value */ \
  Any	     value_set;			/* Set of possible values */ \
  Name	     advance;			/* Clear value after return? */ \
  Int	     length;			/* Length in x's */ \
  FontObj    value_font;		/* Font for entry-field */ \
  Bool	     show_label;		/* Show the label */ \
  TextObj    value_text;		/* Displayed text value */ \
  Bool	     editable;			/* TextItem is editable */ \
  Int	     value_width;		/* Width of value-field in pixels */ \
  Int	     hor_stretch;		/* Horizontal stretchability */ \
  Name	     style;			/* normal, combo_box */

NewClass(textitem)
  ABSTRACT_TEXTITEM
End;

NewClass(slider)
  ABSTRACT_DIALOGITEM
  Any	     selection;			/* Current value (selection) */
  Any	     default_value;		/* Default valur (or function) */
  Any	     displayed_value;		/* Currently displayed value */
  FontObj    value_font;		/* Font of the value */
  Bool	     show_label;		/* Display the label */
  Bool	     show_value;		/* Display the value numerical */
  Name	     format;			/* format for the value */
  Any	     low;
  Any	     high;			/* Low and high values */
  Int	     width;			/* Width in pixels */
  Bool	     drag;			/* Dragging gives messages */
End;

#define ABSTRACT_MENU \
  ABSTRACT_DIALOGITEM \
  Any	     selection;			/* Current selection */ \
  Chain	     members;			/* Chain of menu-items */ \
  Any	     default_value;		/* Function or default value */ \
  Name	     kind;			/* Kind of menu */ \
  MenuItem   preview;			/* Item in preview state */ \
  Name	     preview_feedback;		/* Feedback given for this */ \
  Name	     feedback;			/* Visual feedback */ \
  Bool	     multiple_selection;	/* radio-button? */ \
  Bool       show_label;		/* Show label of menu? */ \
  FontObj    value_font;		/* Font of the value */ \
  Int	     value_width;		/* Minimum width for a value */ \
  Name	     layout;			/* Horizontal or vertical */ \
  Int	     columns;			/* Number of columns */ \
  Name	     format;			/* {left,center,right} */ \
  Name	     vertical_format;		/* {top,center,bottom} */ \
  Size	     gap;			/* Space between items */ \
  Int	     border;			/* Space around item */ \
  Image	     on_image;			/* Image if selected == @on */ \
  Image	     off_image;			/* Image if selected == @off */ \
  Image	     popup_image;		/* Image if popup != @nil */ \
  FontObj    accelerator_font;		/* Font for accelerators */ \
  Int	     margin;			/* Margin at the left/right */ \
  Int	     left_offset;		/* Space box and item-image */ \
  Int	     right_offset;		/* Same at right side */ \
  Point	     item_offset;		/* Offset of first item */ \
  Size	     item_size;			/* Size of each item */ \
  Area	     label_area;		/* Area for the label */
  
#define PULLRIGHT_GAP 3			/* gap between item and => in popup */

NewClass(menu)
  ABSTRACT_MENU
End;

NewClass(popupobj)
  ABSTRACT_MENU
  Any	     context;			/* Invoking context */
  Code	     update_message;		/* Message sent to allow for update */
  PopupObj   pullright;			/* Currently shown pullright menu */
  Any	     selected_item;		/* What has been selected? */
  Name	     button;			/* Invoking button */
  Name	     default_item;		/* Initial previewed item */
  Bool	     show_current;		/* Show current selection? */
End;

NewClass(menu_bar)
  ABSTRACT_DIALOGITEM
  Chain	     members;			/* The popups */
  Name	     format;			/* format of labels in their box */
  PopupObj   current;			/* Currently visible popup */
  Name	     button;			/* Button that activated me */
  Chain	     buttons;			/* Chain of buttons */
  Int	     gap;			/* distance between buttons */
  Int	     radius;			/* radius for the buttons */
End;

NewClass(pen)
  Int		thickness;		/* drawing pen thickness */
  Name		texture;		/* dash-pattern */
  Any		colour;			/* look */
End;

NewClass(node)
  ABSTRACT_VISUAL
  Graphical	image;			/* image displayed as the node */
  Tree		tree;			/* tree in which node resides */
  Int		level;			/* distance from the root */
  Chain		sons;			/* chain of son nodes */
  Chain		parents;		/* chain of parent nodes */
  Bool		collapsed;		/* greyed out and collapsed */
  Bool		displayed;		/* Non-visible node */
  Int		sons_size;		/* total height of sub-tree */
  Int		my_size;		/* size of my graphical */
  Name		computed;		/* height has been computed */
End;

NewClass(scrollbar)
  ABSTRACT_GRAPHICAL
  Code		message;		/* Message executed */
  Graphical	object;			/* Scrolling this graphical */
  Chain		placement;		/* Relative placement */
  Int		distance;		/* Distance to <-object */
  Name		status;			/* {inactive, active, running} */
  Name		orientation;		/* {horizontal, vertical} */
  Int		view;			/* length of visual part of object */
  Int		start;			/* start of visual part of object */
  Int		length;			/* Total length of object */
  Int		bubble_start;		/* Start of bubble in pixels */
  Int		bubble_length;		/* Length of bubble in pixels */
  Name		look;			/* NAME_mac or NAME_sun or NAME_x */
  Bool		drag;			/* Issue dragging updates? */
  Int		amount;			/* Amount to scroll */
  Name		direction;		/* Direction in which to scroll */
  Name		unit;			/* Unit to scroll */
  Int		offset;			/* offset of down from bubble-start */
  Bool		auto_hide;		/* if @on, hide automatically */
End;  

#define SCROLL_PAGE_PROM 900		/* Scroll one page */

NewClass(menu_item)
  ABSTRACT_VISUAL
  Menu	     menu;			/* Menu I'm part of */
  Any	     value;			/* Value represented */
  Code	     message;			/* Message send */
  Any        label;			/* Label of the item */
  FontObj    font;			/* Font of the label */
  Colour     colour;			/* Colour to use */
  Colour     background;		/* Colour for the background */
  Bool	     selected;			/* Currently selected */
  Bool	     active;			/* Can be selected? */
  Code	     condition;			/* Determines <->active */
  Bool	     end_group;			/* Ends logical group of items */
  PopupObj   popup;			/* Popup (sub-popups and menu-bars) */
  Name	     accelerator;		/* activate on this key */
End;

		/********************************
		*            GESTURES		*
		********************************/

NewClass(popup_gesture)
  ABSTRACT_GESTURE
  PopupObj	popup;			/* Popup shown */
  PopupObj	current;		/* Currently visible popup */
  Any		context;		/* Execution context parameter */
  Int		max_drag_distance;	/* Cancel after dragging this far */
End;

#define ABSTRACT_MOVEGESTURE \
  ABSTRACT_GESTURE \
  Point		offset;			/* Offset of down to graphical */

NewClass(move_gesture)
  ABSTRACT_MOVEGESTURE
End;

NewClass(move_outline_gesture)
  ABSTRACT_MOVEGESTURE
  Box		outline;		/* Outline dragged */
End;

#define ABSTRACT_RESIZEGESTURE \
  ABSTRACT_GESTURE \
  Name		h_mode;			/* Horizontal resize mode */ \
  Name		v_mode;			/* Vertical resize mode */ \
  Size		min_size;		/* Minimum size */ \
  Size		max_size;		/* Maximum size */


NewClass(resize_gesture)
  ABSTRACT_RESIZEGESTURE
End;

NewClass(resize_outline_gesture)
  ABSTRACT_RESIZEGESTURE
  Box		outline;		/* Outline dragged */
  ResizeGesture outline_gesture;	/* Recoigniser of the outline */
End;

NewClass(click_gesture)
  ABSTRACT_GESTURE
  Name		multiclick;		/* {single,double,tripple} */
  Point		down_position;		/* Position of the ->down event */
  Code		execute_message;	/* Message sent on up inside area */
  Code		preview_message;	/* Message sent on down */
  Code		cancel_message;		/* Message sent on up outside area */
  CursorObj	execute_cursor;		/* Cursor while executing */
  Int		max_drag_distance;	/* Cancel after dragging this far */
End;

NewClass(connect_gesture)
  ABSTRACT_GESTURE
  Device	device;			/* Device used for feedback */
  Line		line;			/* Line for feedback (cf. outline) */
  Image		mark;			/* Bitmap used to mark handles */
  Link		link;			/* Link to realise connection */
  Chain		from_indicators;	/* Indicator bitmaps at from side */
  Chain		to_indicators;		/* Indicator bitmaps at to side */
  Name		from_handle;		/* Handle at from side */
  Name		to_handle;		/* Handle at to side */
  Graphical	to;			/* Graphical at to side */
End;

NewClass(edit_text_gesture)
  ABSTRACT_GESTURE
  Int		selection_origin;	/* Start of the selection */
End;



		/********************************
		*          BROWSERS		*
		********************************/

NewClass(list_browser)
  ABSTRACT_DEVICE
  Dict		dict;			/* dict in which items reside */
  TextImage	image;			/* The text area */
  ScrollBar	scroll_bar;		/* The scrollbar */
  TextObj	label_text;		/* Text to display the label */
  Name		status;			/* active/inactive */
  KeyBinding	key_binding;		/* Keybinding table */
  Any		selection;		/* DictItem (Chain) of selected */
  Style		selection_style;	/* Style object for selection */
  Bool		multiple_selection;	/* Multiple selections (def: OFF) */
  Code		select_message;		/* Message on left button click */
  Code		open_message;		/* Message on left-double click */
  Code		cancel_message;		/* Send on drag with `up' outside */
  PopupObj	popup;			/* Associated popup */
  FontObj	font;			/* Font for text */
  Sheet		styles;			/* Name --> style mapping */
  Size		size;			/* Size in characters */
  Int		start;			/* Index of first item shown */
  Int		search_origin;		/* Origin of incremental search */
  Int		search_hit;		/* Current item found */
  StringObj	search_string;		/* Incremental search string */
  Int		caret;			/* Index of `caret' item */
  Int		selection_origin;	/* Origin of select-range */
					/* Start private data */
  Cell		start_cell;		/* Cell corresponding to <-start */
End;


		 /*******************************
		 *	       WINDOWS		*
		 *******************************/

#define ABSTRACT_WINDOW \
  ABSTRACT_DEVICE \
  FrameObj	frame;			/* Frame we are member of */ \
  PceWindow	decoration;		/* Window holding decorations */ \
  Area		bounding_box;		/* Union of graphicals */ \
  TileObj	tile;			/* Area managing tile */ \
  Code		resize_message;		/* Message send after a resize */ \
  CursorObj	displayed_cursor;	/* Currently displayed cursor */ \
  Bool		input_focus;		/* Window has the input focus */ \
  Graphical	keyboard_focus;		/* Graphical in focus of keyboard */ \
  Graphical	focus;			/* Graphical in focus */ \
  Recogniser	focus_recogniser;	/* Recorniser in focus */ \
  CursorObj	focus_cursor;		/* Cursor during focus */ \
  Name		focus_button;		/* Button that initiated focus */ \
  EventObj	focus_event;		/* Event that grabbed the focus */ \
  Point		scroll_offset;		/* Amount scrolled */ \
  PopupObj	popup;			/* Associated popup */ \
  EventObj	current_event;		/* Currently processed event */ \
  Bool		sensitive;		/* Sensitive to events */ \
  Any		background;		/* Background colour of the window */ \
  Bool		has_pointer;		/* We own the pointer */ \
  Any		selection_feedback;	/* Feedback for selection */ \
  Bool		buffered_update;	/* Buffered update? */ \
					/* Alien stuff */ \
  UpdateArea	changes_data;		/* Recorded changes */ \
  WsRef		ws_ref;			/* Window system reference */

NewClass(colour_map)
  Name		name;			/* name of the map */
  Vector	colours;		/* colours in the map */
  Bool		read_only;		/* colourmap cannot be changed */
  WsRef		ws_ref;			/* Window system reference */
End;

#define ABSTRACT_IMAGE \
  ABSTRACT_VISUAL \
  Name		name;			/* Name of the image */ \
  Name		kind;			/* {pixmap,bitmap} */ \
  SourceSink	file;			/* Resolved file */ \
  Name		access;			/* {read,both} */ \
  Colour	background;		/* Background-colour (pixmap) */ \
  Colour	foreground;		/* Foreground-colour (pixmap) */ \
  Int		depth;			/* Bits/pixel */ \
  Size		size;			/* Size of the image */ \
  DisplayObj	display;		/* Display of read-write's */ \
  BitmapObj	bitmap;			/* Bitmap for read-write's */ \
  Point		hot_spot;		/* Indication of hot-spot */ \
  Image		mask;			/* Masking image */ \
  WsRef		ws_ref;			/* Window system reference */

NewClass(image)
  ABSTRACT_IMAGE
End;

NewClass(pixmapobj)
  ABSTRACT_IMAGE
End;

NewClass(bitmapobj)
  ABSTRACT_GRAPHICAL
  Image		image;			/* Image of the bitmap */
  Bool		transparent;		/* Just stipple pattern */
End;


NewClass(cursorobj)
  Name		name;			/* Name of the cursor */
  Int		font_id;		/* Identifier in X-cursor-font */
  Image		image;			/* User-defined image */
  Image		mask;			/* User-defined mask */
  Point		hot_spot;		/* User-defined hot_spot */
  Colour	foreground;		/* User-defined foreground */
  Colour	background;		/* User-defined background */
End;


NewClass(colour)
  Name		name;			/* Name of the colour (red, ...) */
  Name		kind;			/* `named' or `rgb' */
  Int		red;			/* Red intensity */
  Int		green;			/* Green intensity */
  Int		blue;			/* Blue intensity */
End;


NewClass(elevation)
  Name		name;			/* logical name */
  Int		height;			/* Height of the top */
  Any		colour;			/* Colour of the top */
  Any		background;		/* Colour when down */
  Any		relief;			/* Relief colour/pixmap */
  Any		shadow;			/* Shadow colour/pixmap */
  Name		kind;			/* How elevation is painted */
End;


NewClass(fontobj)
  Name		family;			/* Font family */
  Name		style;			/* Style */
  Int		points;			/* Point size */
  Int		ex;			/* Width of an x in this font */
  Name          x_name;			/* X-Name of the font */
  Bool		fixed_width;		/* Is this a fixed width font? */
  Bool		b16;			/* Font is 16-bit font */
  Name		postscript_font;	/* Name of PostScript font */
  Int		postscript_size;	/* Size in PostScript */
End;


NewClass(displayobj)
  ABSTRACT_VISUAL
  Size		size;			/* size of the display in pixels */
  Name		address;		/* Display address specification */
  HashTable	font_table;		/* Table holding font-mappings */
  Chain		frames;			/* Created frames on this display */
  Chain		inspect_handlers;	/* Event-handlers for inspector */
  Colour	foreground;		/* Window default foreground */
  Colour	background;		/* Window default background */
  ColourMap	colour_map;		/* Default colour_map for frames */
  Bool		quick_and_dirty;	/* Use quick_and_dirty drawing */
  Image		cache;			/* Graphics cache */
  Name		window_manager;		/* name of the window manager */
  DisplayManager display_manager;	/* Global display manager */
  Int		busy_locks;		/* Lock count of ->busy_cursor */
  WsRef		ws_ref;			/* Window System Reference */
End;
  

NewClass(display_manager)
  ABSTRACT_VISUAL
  Chain		members;		/* Available displays */
  Chain		current;		/* Stack for current display */
  Bool		test_queue;		/* Test queue during redraw */
End;
  

NewClass(application)
  ABSTRACT_VISUAL
  Name		name;			/* name of the application */
  Chain		members;		/* its member frames */
  Name		kind;			/* {user,service} */
  FrameObj	modal;			/* Modal frame */
End;


NewClass(frameobj)
  ABSTRACT_VISUAL
  Name		name;			/* Name of the frame */
  Name		label;			/* Label of the frame */
  Name		icon_label;		/* Label of the icon */
  Image		icon_image;		/* Image of the icon */
  Point		icon_position;		/* Position of the icon */
  Application	application;		/* Application it belongs too */
  DisplayObj	display;		/* Display it is displayed on */
  Int		border;			/* Border width */
  Any		background;		/* Frames background */
  ColourMap	colour_map;		/* Attached colourmap */
  Area		area;			/* Area of the frame */
  Name		geometry;		/* X-Window geometry spec */
  Chain		members;		/* Windows displayed */
  Name		kind;			/* Kind of frame */
  FrameObj	transient_for;		/* Sub frame of who? */
  Chain		transients;		/* Sub frames */
  Name		modal;			/* Modal operation */
  Any		return_value;		/* ->return saved value */
  Bool		input_focus;		/* @on: focus for keyboard events */
  Name		status;			/* {unmapped,iconic,open} */
  Bool		can_delete;		/* User can delete the frame */
  Bool		can_resize;		/* User can resize the frame */
  Bool		confirm_done;		/* User must confirm delete */
  Bool		fitting;		/* We are running ->fit */
  Sheet		wm_protocols;		/* WM protocols understood */
  Bool		wm_protocols_attached;	/* Have the protocols been attached */
					/* start private data */
  WsRef		ws_ref;			/* Window-System reference */
End;


NewClass(eventobj)
  Any		window;			/* Original window */
  Any		receiver;		/* Receiver of the event */
  Any		id;			/* Event identifier */
  Int		buttons;		/* Bit mask of button positions */
  Int		x;			/* X coordinate relative to window */
  Int		y;			/* Y coordinate relative to window */
  Point		position;		/* Computed Position */
  unsigned long time;			/* Time of event in milliseconds */
End;


NewClass(event_nodeobj)
  Name		value;			/* value of the event_node */
  EventNodeObj	parent;			/* Super event_node */
  Chain		sons;			/* Sub event_nodes */
End;


NewClass(event_treeobj)
  EventNodeObj	root;			/* Root of the event_tree */
  HashTable	table;			/* Value -> EventNodeObj */
End;


NewClass(windowobj)
  ABSTRACT_WINDOW
End;

NewClass(window_decorator)
  ABSTRACT_WINDOW
  PceWindow	window;			/* Client (decorated) window */
  ScrollBar	horizontal_scrollbar;	/* Horizontal scrollbar */
  ScrollBar	vertical_scrollbar;	/* Vertical scrollbar */
  TextObj	label_text;		/* Text object to display label */
End;

NewClass(picture)
  ABSTRACT_WINDOW
End;

NewClass(tile_adjuster)
  ABSTRACT_WINDOW
  TileObj	client;			/* Tile I adjust */
  Name		orientation;		/* Direction of resize */
  Int		down_offset;		/* Offset when processing */
End;

NewClass(dialog)
  ABSTRACT_WINDOW
  Size		gap;			/* X-Y gap between items */
  Size		border;			/* border around objects */
  Name		size_given;		/* Size given by user: no recompute */
End;

NewClass(view)
  ABSTRACT_WINDOW
  Editor	editor;			/* Editor implementing view */
End;

NewClass(browser)
  ABSTRACT_WINDOW
  ListBrowser	list_browser;		/* ListBrowser implementing browser */
End;

NewClass(timer)
  Real		interval;		/* interval as a real value */
  Code		message;		/* message when timer fires */
  Name		status;			/* {idle,interval,once} */
  Bool		service;		/* Operating in service mode? */
  WsRef		ws_ref;			/* Window System Reference */
End;


		 /*******************************
		 *	      EVENTS		*
		 *******************************/

#define BUTTON_mask		(0x0ff)
#define BUTTON_control		(0x001)
#define BUTTON_shift		(0x002)
#define BUTTON_meta		(0x004)
#define BUTTON_ms_left		(0x008)
#define BUTTON_ms_middle	(0x010)
#define BUTTON_ms_right		(0x020)
#define BUTTON_ms_button4	(0x040)
#define BUTTON_ms_button5	(0x080)
					/* buttons bit mask */
#define CLICK_TYPE_mask		(0x700)
#define CLICK_TYPE_single	(0x100)
#define CLICK_TYPE_double	(0x200)
#define CLICK_TYPE_triple	(0x400)

		/********************************
		*      X-WINDOW REFERENCES	*
		********************************/

struct xref
{ Any		object;			/* Object (Cursor, Font, ...) */
  DisplayObj    display;		/* Display for which to get ref */
  void *	xref;			/* X-window reference */
  Xref		next;			/* Next entry of table */
};


		/********************************
		*            COLOURS		*
		********************************/

#define BRIGHT	toInt(65535)		/* full on */
#define NoPixel (1L << 30)		/* no pixel value */

		/********************************
		*            CHANGES		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This macro is to be called to change a graphical object.  It will pass
the appropriate  changes to its  device, so the  device can figure out
the optimal redisplay behaviour.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


#define CHANGING_GRAPHICAL(gr, code) \
  { Int _x, _y, _w, _h;					\
    Device _device;					\
							\
    _x = (gr)->area->x; _y = (gr)->area->y;		\
    _w = (gr)->area->w; _h = (gr)->area->h;		\
    _device = (gr)->device;				\
							\
    code;						\
    							\
    if ( (_x != (gr)->area->x || _y != (gr)->area->y ||	\
          _w != (gr)->area->w || _h != (gr)->area->h)&& \
	 _device == (gr)->device)			\
      changedAreaGraphical((gr), _x, _y, _w, _h);	\
  }

		/********************************
		*            DRAWING		*
		********************************/

#define DRAW_3D_DOWN	0x1		/* r_3d_rectangular_polygon() flags */
#define DRAW_3D_CLOSED	0x2
#define DRAW_3D_FILLED	0x4

typedef struct
{ int x, y;
} ipoint, *IPoint;


typedef struct
{ int x, y, w, h;
} iarea, *IArea;


typedef struct
{ int x1, y1, x2, y2;
} isegment, *ISegment;


struct update_area
{ iarea		area;			/* Concerned area */
  int		clear;			/* needs to be cleared */
  int		deleted;		/* area is deleted */
  int		size;			/* width * height */
  UpdateArea    next;			/* next in chain */
};

struct colour_context
{ Any		foreground;		/* saved foreground */
  Any		background;		/* saved background */
  int		lock;			/* lock nesting count */
};

typedef struct
{ Int	x;
  Int	y;
  Int 	w;
  Int	h;
} device_draw_context, *DeviceDrawContext;

typedef struct
{ int   x;				/* fill-offset-x */
  int   y;				/* fill-offset-y */
} fill_state;


		/********************************
		*           OVERLAP		*
		********************************/

#define OverlapArea(ax, ay, aw, ah, bx, by, bw, bh) \
	    (!((by > ay+ah || by+bh < ay || bx > ax+aw || bx+bw < ax)))


		/********************************
		*           GLOBALS		*
		********************************/

#ifndef GLOBAL
#define GLOBAL extern
#endif

GLOBAL Image BLACK_IMAGE;
GLOBAL Image WHITE_IMAGE;
GLOBAL Image GREY12_IMAGE;
GLOBAL Image GREY25_IMAGE;
GLOBAL Image GREY50_IMAGE;
GLOBAL Image GREY75_IMAGE;

GLOBAL Colour BLACK_COLOUR;
GLOBAL Colour WHITE_COLOUR;

GLOBAL Image CYCLE_IMAGE;		/* image of a cycle */
GLOBAL Image MARK_IMAGE;		/* images for toggle and marked */
GLOBAL Image NOMARK_IMAGE;
GLOBAL Image MS_MARK_IMAGE;		/* MS images for toggle and marked */
GLOBAL Image MS_NOMARK_IMAGE;
GLOBAL Image PULLRIGHT_IMAGE;		/* Popup menu pullright marker */
GLOBAL Image MARK_HANDLE_IMAGE;		/* connect_gesture */
GLOBAL Image NULL_IMAGE;		/* empty image */
GLOBAL Image INT_ITEM_IMAGE;
GLOBAL Image SCROLL_UP_IMAGE;
GLOBAL Image SCROLL_DOWN_IMAGE;
GLOBAL Image SCROLL_LEFT_IMAGE;
GLOBAL Image SCROLL_RIGHT_IMAGE;
GLOBAL Image EXCLAMATION_IMAGE;

GLOBAL Modifier MODIFIER_shift;		/* Demands `shift-is-down' */
GLOBAL Modifier MODIFIER_control;	/* Demands `control-is-down' */
GLOBAL Modifier MODIFIER_allup;		/* Demands all modifiers up */

GLOBAL PopupGesture GESTURE_popup;	/* Displays a popup menu */
GLOBAL ClickGesture GESTURE_button;	/* Gesture for handling buttons */
GLOBAL Recogniser   GESTURE_wheelMouse;	/* Wheelmouse translation */

GLOBAL Chain ChangedWindows;		/* Windows that have changed */

GLOBAL  int XrefsResolved;		/* succesful getXrefObject()'s */
GLOBAL	HashTable ColourTable;		/* ColourName --> Colour */
GLOBAL  HashTable CursorTable;		/* CursorName --> Cursor */
GLOBAL	HashTable FontTable;		/* FontName --> Font */
GLOBAL	HashTable ImageTable;		/* ImageName --> Image */
GLOBAL  HashTable WindowTable;		/* X-Window --> PceWindow|FrameObj */
GLOBAL  Class ClassImage;		/* @image_class */

GLOBAL  EventTreeObj  EventTree;	/* @event_tree */

		 /*******************************
		 *     MENU LAYER CONSTANTS	*
		 *******************************/

#define TEXTFIELD_EDITABLE	0x01	/* if editable field */
#define TEXTFIELD_COMBO		0x02	/* if combo-box displayed */
#define TEXTFIELD_COMBO_DOWN	0x04	/* after combo-box has been clicked */
#define TEXTFIELD_STEPPER	0x08	/* up/down stepper */
#define TEXTFIELD_INCREMENT	0x10	/* stepper in `increment' mode */
#define TEXTFIELD_DECREMENT	0x20	/* stepper in `increment' mode */

#define LABEL_INACTIVE		0x1	/* str_label() flags */

#define CHECKBOX_SELECTED	0x1	/* item is selected */
#define CHECKBOX_ACTIVE		0x2	/* item is active */
#define CHECKBOX_MULTIPLE	0x4	/* more than one may be selected */
		 
#define MBX_INFORM		0x1	/* infomational message */
#define MBX_CONFIRM		0x2	/* confirm action */
#define MBX_ERROR		0x4	/* error message */

#define MBX_NOTHANDLED		0x0	/* not handled by virtual controller */
#define MBX_OK			0x1	/* user confirmed */
#define MBX_CANCEL		0x2	/* user canceled */


		/********************************
		*          PROTOTYPES		*
		********************************/

#include	"wst.h"
#include	"../gra/proto.h"
#include	"../win/proto.h"
#include	"../evt/proto.h"

#endif /*_PCE_GRA_INCLUDED*/


