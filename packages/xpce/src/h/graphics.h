/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_GRA_INCLUDED
#define _PCE_GRA_INCLUDED

#define MAX_WRAP_LINES	100		/* line-wraps in text-objects */

typedef struct ipoint *IPoint;		/* integer-point */
typedef struct iarea  *IArea;		/* integer-area */
typedef struct update_area *UpdateArea;	/* Window changes data  */

#include "wst.h"

#define ABSTRACT_GRAPHICAL \
  ABSTRACT_VISUAL \
  Device     device;			/* device on which displayed */ \
  Area	     area;			/* area (bounding box) */ \
  Bool       displayed;			/* is graphical object displayed? */ \
  Int        pen;			/* pen thickness for this object */ \
  Name	     texture;			/* dash pattern for lines */ \
  Any        colour;			/* colour of the graphical */ \
  Chain	     handles;			/* handles connected to graphical */ \
  Chain	     connections;		/* Available connections */ \
  Name       name;			/* name to find it */ \
  Bool	     selected;			/* is graphical selected? */ \
  Bool	     inverted;			/* inverted image */ \
  Bool	     active;			/* (de)activated */ \
  CursorObj  cursor;			/* cursor of the graphical */ \
  Any	     request_compute;		/* graphical needs recomputed */

#define ABSTRACT_DEVICE \
  ABSTRACT_GRAPHICAL \
  Int	     level;			/* Level from root */ \
  Point	     offset;			/* Origin offset */ \
  Area	     clip_area;			/* Visible subarea */ \
  Chain	     graphicals;		/* Displayed graphicals */ \
  Chain	     pointed;			/* Graphicals on last event */ \
  Format     format;			/* Row/Column formatting */ \
  Bool	     badFormat;			/* Formats needs to be recomputed */ \
  Bool	     badBoundingBox;		/* BoundingBox needs recomputed */ \
  Chain	     recompute;			/* Graphicals requesting recompute */

#define ABSTRACT_FIGURE \
  ABSTRACT_DEVICE \
  Name		status;			/* Which members are displayed? */ \
  Image		background;		/* Pattern for background */ \
  Int		border;			/* border around graphicals */ \
  Int		radius;			/* radius of outline */ \
  Elevation	elevation;		/* elevation of outline */

#define ABSTRACT_DIALOGITEM \
  ABSTRACT_GRAPHICAL \
  Name	     label;			/* Label of the item */ \
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
  Bool	     auto_value_align;		/* Automatically align value? */


#define ABSTRACT_JOINT \
  ABSTRACT_GRAPHICAL \
  Arrow     first_arrow;		/* arrow on start point */ \
  Arrow     second_arrow;		/* arrow on end point */

#define ABSTRACT_LINE \
  ABSTRACT_JOINT


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
  Int	     idealWidth;		/* Desired width of the tile */
  Int	     idealHeight;		/* Idem for height */
  Int	     horStretch;		/* Horizontal stretchability */
  Int	     horShrink;			/* Horizontal shrinkability */
  Int	     verStretch;		/* Vertical stretchability */
  Int	     verShrink;			/* Vertical shrinkability */
  Int	     border;			/* Border between subtiles */
  Name       orientation;		/* none, horizontal, vertical */
  Chain	     members;			/* subtiles */
  TileObj    super;			/* Super tile */
  Any	     object;			/* Object managed */
  Area	     area;			/* Current area of the tile */
  Bool	     enforced;			/* Enfore layout */
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

NewClass(box)
  ABSTRACT_GRAPHICAL
  Int        radius;			/* if displayed as a rounded box */
  Int	     shadow;			/* shadow displayed around box */
  Image	     fill_pattern;		/* fill box with this */
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
  Point	     position;		/* reference position of text */
  Int        caret;		/* current insertion/deletion point */
  Any	     show_caret;	/* show the caret (default OFF) */
  Any	     background;	/* Background of text */
  Int	     border;		/* Border around actual text */
  Name	     wrap;		/* Clip to width */
  Int	     x_offset;		/* Shift in X-direction (length > 0) */
  Int	     x_caret;		/* Caret X, relative to graphical */
  Int	     y_caret;		/* Caret Y, relative to graphical */
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
  Any	     selection;			/* Currently displayed value */
  Int	     border;			/* additional space */
End;

NewClass(button)
  ABSTRACT_DIALOGITEM
  Int	     radius;			/* Rounding radius */
  Int	     shadow;			/* shadow around button */
  FontObj    label_font;		/* Font of the button */
  Name	     accelerator;		/* activate on this key */
  Image	     popup_image;		/* Image to indicate popup */
End;

NewClass(textitem)
  ABSTRACT_DIALOGITEM
  Any	     selection;			/* Current selection */
  Any	     default_value;		/* The default (initial) value */
  StringObj  print_name;		/* Print-name of selection */
  Type	     type;			/* Type of the value */
  Any	     value_set;			/* Set of possible values */
  Name	     advance;			/* Clear value after return? */
  Int	     length;			/* Length in x's */
  FontObj    value_font;		/* Font for entry-field */
  FontObj    label_font;		/* Font of the label */
  Bool	     show_label;		/* Show the label */
  TextObj    value_text;		/* Displayed text value */
  Bool	     editable;			/* TextItem is editable */
End;

NewClass(slider)
  ABSTRACT_DIALOGITEM
  Any	     selection;			/* Current value (selection) */
  Any	     default_value;		/* Default valur (or function) */
  Any	     displayed_value;		/* Currently displayed value */
  FontObj    label_font;		/* Font of the label */
  FontObj    value_font;		/* Font of the value */
  Bool	     show_label;		/* Display the label */
  Bool	     show_value;		/* Display the value numerical */
  Int	     low;
  Int	     high;			/* Low and high values */
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
  FontObj    label_font;		/* Font of the label */ \
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
End;

NewClass(menu_bar)
  ABSTRACT_DIALOGITEM
  Chain	     members;			/* The popups */
  Name	     format;			/* format of labels in their box */
  FontObj    label_font;		/* Font to set the labels */
  PopupObj   current;			/* Currently visible popup */
  Name	     button;			/* Button that activated me */
  Chain	     buttons;			/* Chain of buttons */
  Int	     gap;			/* distance between buttons */
  Int	     radius;			/* radius for the buttons */
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
End;  

NewClass(menu_item)
  ABSTRACT_VISUAL
  Menu	     menu;			/* Menu I'm part of */
  Any	     value;			/* Value represented */
  Code	     message;			/* Message send */
  Any        label;			/* Label of the item */
  FontObj    font;			/* Font of the label */
  Colour     colour;			/* Colour to use */
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
  Code		select_middle_message;	/* Message on middle button click */
  Code		open_message;		/* Message on left-double click */
  PopupObj	popup;			/* Associated popup */
  FontObj	font;			/* Font for text */
  Sheet		styles;			/* Name --> style mapping */
  Size		size;			/* Size in characters */
  Int		start;			/* Index of first item shown */
  Int		search_origin;		/* Origin of incremental search */
  Int		search_hit;		/* Current item found */
  StringObj	search_string;		/* Incremental search string */
					/* Start private data */
  Cell		start_cell;		/* Cell corresponding to <-first */
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
					/* Alien stuff */ \
  UpdateArea	changes_data;		/* Recorded changes */ \
  WsRef		ws_ref;			/* Window system reference */

#define ABSTRACT_IMAGE \
  ABSTRACT_VISUAL \
  Name		name;			/* Name of the image */ \
  Name		kind;			/* {pixmap,bitmap} */ \
  FileObj	file;			/* Resolved file */ \
  Name		access;			/* {read,both} */ \
  Colour	background;		/* Background-colour (pixmap) */ \
  Colour	foreground;		/* Foreground-colour (pixmap) */ \
  Int		depth;			/* Bits/pixel */ \
  Size		size;			/* Size of the image */ \
  DisplayObj	display;		/* Display of read-write's */ \
  BitmapObj	bitmap;			/* Bitmap for read-write's */ \
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
  Name		resource_class;		/* Resource class name */
  Chain		frames;			/* Created frames on this display */
  Chain		inspect_handlers;	/* Event-handlers for inspector */
  Colour	foreground;		/* Window default foreground */
  Colour	background;		/* Window default background */
  Bool		quick_and_dirty;	/* Use quick_and_dirty drawing */
  Image		cache;			/* Graphics cache */
  Name		window_manager;		/* name of the window manager */
  DisplayManager display_manager;	/* Global display manager */
  WsRef		ws_ref;			/* Window System Reference */
End;
  

NewClass(display_manager)
  ABSTRACT_VISUAL
  Chain		members;		/* Available displays */
  Chain		current;		/* Stack for current display */
End;
  

NewClass(frameobj)
  ABSTRACT_VISUAL
  Name		label;			/* Label of the frame */
  Name		icon_label;		/* Label of the icon */
  Image		icon_image;		/* Image of the icon */
  Point		icon_position;		/* Position of the icon */
  DisplayObj	display;		/* Display it is displayed on */
  Int		border;			/* Border width */
  Any		background;		/* Frames background */
  Area		area;			/* Area of the frame */
  Name		geometry;		/* X-Window geometry spec */
  Chain		members;		/* Windows displayed */
  Bool		destroying;		/* Destroying the tree */
  Name		kind;			/* Kind of frame */
  FrameObj	transient_for;		/* Sub frame of who? */
  Chain		transients;		/* Sub frames */
  Any		return_value;		/* ->return saved value */
  Bool		input_focus;		/* @on: focus for keyboard events */
  Name		status;			/* {unmapped,iconic,open} */
  Bool		can_delete;		/* User can delete the frame */
  Bool		confirm_done;		/* User must confirm delete */
  Sheet		wm_protocols;		/* WM protocols understood */
  Bool		wm_protocols_attached;	/* Have the protocols been attached */
					/* start private data */
  WsRef		ws_ref;			/* Window-System reference */
End;


NewClass(eventobj)
  PceWindow	window;			/* Original window */
  Graphical	receiver;		/* Receiver of the event */
  Any		id;			/* Event identifier */
  Int		buttons;		/* Bit mask of button positions */
  Int		x;			/* X coordinate relative to window */
  Int		y;			/* Y coordinate relative to window */
  Point		position;		/* Computed Position */
  ulong		time;			/* Time of event in milliseconds */
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

NewClass(dialog)
  ABSTRACT_WINDOW
  Size		gap;			/* X-Y gap between items */
  Bool		size_given;		/* Size given by user: no recompute */
End;

NewClass(view)
  ABSTRACT_WINDOW
  Editor	editor;			/* Editor implementing view */
End;

NewClass(browser)
  ABSTRACT_WINDOW
  ListBrowser	list_browser;		/* ListBrowser implementing browser */
End;

NewClass(resource)
  Name		name;			/* Name of this resource */
  Name		r_class;		/* Class of this resource */
  Type		r_type;			/* Type of this resource */
  StringObj	r_default;		/* Default value */
  Any		context;		/* Context object we belong to */
  Any		value;			/* Value of the resource */
  StringObj	summary;		/* Short documentation */
End;

NewClass(timer)
  Real		interval;		/* interval as a real value */
  Code		message;		/* message when timer fires */
  Name		status;			/* {idle,interval,once} */
  WsRef		ws_ref;			/* Window System Reference */
End;

		 /*******************************
		 *	      EVENTS		*
		 *******************************/

#define BUTTON_mask		(0x3f)
#define BUTTON_control		(0x1)
#define BUTTON_shift		(0x2)
#define BUTTON_meta		(0x4)
#define BUTTON_ms_left		(0x8)
#define BUTTON_ms_middle	(0x10)
#define BUTTON_ms_right		(0x20)

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
the optiomal redisplay behaviour.

The device is maintained to ensure the  ->frozen @on and ->frozen @off
pair is sent to the same device.  If  the graphical is moved  from one
device to  another, displayed or deleted from  its device, the display
or erase behaviour of graphicals will take care of the move operation.
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

struct ipoint
{ int x, y;
};

struct iarea
{ int x, y, w, h;
};

struct update_area
{ struct iarea	area;			/* Concerned area */
  int		clear;			/* needs to be cleared */
  int		deleted;		/* area is deleted */
  UpdateArea    next;			/* next in chain */
};





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
GLOBAL Image MARK_IMAGE;		/* images for toggle and choice */
GLOBAL Image NOMARK_IMAGE;
GLOBAL Image PULLRIGHT_IMAGE;		/* Popup menu pullright marker */
GLOBAL Image MARK_HANDLE_IMAGE;		/* connect_gesture */

GLOBAL Modifier MODIFIER_shift;		/* Demands `shift-is-down' */
GLOBAL Modifier MODIFIER_allup;		/* Demands all modifiers up */

GLOBAL PopupGesture GESTURE_popup;	/* Displays a popup menu */
GLOBAL ClickGesture GESTURE_button;	/* Gesture for handling buttons */

GLOBAL Chain ChangedWindows;		/* Windows that have changed */

GLOBAL  int XrefsResolved;		/* succesful getXrefObject()'s */
GLOBAL	HashTable ColourTable;		/* ColourName --> Colour */
GLOBAL  HashTable CursorTable;		/* CursorName --> Cursor */
GLOBAL	HashTable FontTable;		/* FontName --> Font */
GLOBAL	HashTable ImageTable;		/* ImageName --> Image */
GLOBAL  HashTable WindowTable;		/* X-Window --> PceWindow|FrameObj */
GLOBAL  Class ClassImage;		/* @image_class */


		/********************************
		*          PROTOTYPES		*
		********************************/

#include	"../gra/proto.h"
#include	"../win/proto.h"
#include	"../evt/proto.h"

#endif /*_PCE_GRA_INCLUDED*/


