/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_TXT_INCLUDED
#define _PCE_TXT_INCLUDED

#include <h/graphics.h>

		/********************************
		*          TEXT CLASSES		*
		********************************/


#define TXT_X_MARGIN 5		/* Margin between text and box of textimage */
#define TXT_Y_MARGIN 2

#define TXT_UNDERLINED	0x1	/* underlined character */
#define TXT_HIGHLIGHTED	0x2	/* highlighted character (inverse video) */
#define TXT_GREYED	0x4	/* greyed character */
#define TXT_BOLDEN	0x8	/* bold character */
#define TXT_CLOSED	0x10	/* invisible fragment */

NewClass(text_buffer)
  Fragment	first_fragment;		/* first fragment */
  Fragment	last_fragment;		/* last fragment */
  Chain		editors;		/* editors associated buffer */
  Bool		modified;		/* has textbuffer been modified? */
  Int		undo_buffer_size;	/* Size of the undo-buffer */
  SyntaxTable	syntax;			/* Syntax description */
					/* start private data */
  int		changed_start;		/* start of changed region */
  int		changed_end;		/* end of changed region */
  int		gap_start;		/* first location of the gap */
  int		gap_end;		/* last location of the gap */
  int		size;			/* # characters in buffer */
  int		allocated;		/* allocated size */
  UndoBuffer	undo_buffer;		/* Undo log */
  string	buffer;			/* Actual buffer (with gap) */
End;

#define tb_buffer8  buffer.text_union.text8
#define tb_buffer16 buffer.text_union.text16

NewClass(fragment)
  TextBuffer	textbuffer;	/* text buffer fragment is associated with */
  Fragment	next;		/* next fragment */
  Fragment  	prev;		/* previous fragment */
  Name	 	style;		/* style of fragment (indirect via editor) */
  long   	start;		/* start of fragment */
  long  	length;		/* length of fragment (> 0) */
End;

NewClass(style)
  FontObj	font;			/* font of fragment */
  Colour	colour;			/* colour of fragment */
  Image		icon;			/* margin marker */
  long		attributes;		/* style attributes */
End;


typedef struct fragment_cache *FragmentCache;

NewClass(editor)
  ABSTRACT_DEVICE			/* abstract super class device */
  TextBuffer	text_buffer;		/* Buffer editor operates on */
  TextImage	image;			/* The text area */
  ScrollBar	scroll_bar;		/* The scrollbar */
  TextMargin	margin;			/* The margin */
  TextCursor	text_cursor;		/* The cursor */
  FontObj	font;			/* editors default font */
  Size		size;			/* Size in characters */
  Int		caret;			/* position of the caret */
  Int		mark;			/* position of the marker */
  Int		tab_distance;		/* distance between tabs */
  Fragment	selected_fragment;	/* Currently selected fragment */
  Style		selected_fragment_style; /* style of selected_fragment */
  Sheet		styles;			/* Style-name --> Style-object */
  KeyBinding	bindings;		/* Key bindings */
  Name		focus_function;		/* Function in focus */
  Bool		fill_mode;		/* Auto fill */
  Bool		exact_case;		/* Search and replace do exact case */
  Name		kill_location;		/* Processing kill sequences */
  Name		search_direction;	/* direction of the search */
  StringObj	search_string;		/* Target of search */
  Int		search_origin;		/* Incremental search started here */
  Int		search_base;		/* Currently searching from here */
  Name		selection_unit;		/* Selection unit (char, word, line) */
  Name		selection_origin;	/* Original start of selection */
  Bool		editable;		/* Text may be changed by user */
  Code		error_message;		/* Forward error messages */
  Code		modified_message;	/* Forward <->modified changed */
  Int		left_margin;		/* Left margin indentation */
  Int		right_margin;		/* Right margin */
  Int		indent_increment; 	/* Steps taken by region in/undent1 */
  Bool		auto_newline;		/* Auto newline on ->append */
  FileObj	file;			/* Name of file or NIL */
  Name		dabbrev_target;		/* Base of the dabbrev expansion */
  Chain		dabbrev_reject;		/* Hits rejected by dabbrev */
  Int		dabbrev_pos;		/* Current search position */
  Int		dabbrev_origin;		/* Start of dabbrev word */
					/* Private data */
  long		internal_mark;		/* Internally used mark */
  long		selection_start;	/* Start of selection */
  long		selection_end;		/* End of selection */
  FragmentCache fragment_cache;		/* Cache to compute frament overlap */
End;

NewClass(text_cursor)
  ABSTRACT_GRAPHICAL			/* Abstract class graphical */
  Name		style;			/* Block, arrow, bitmap */
  Image		image;			/* If there is an image; this is it */
  Point		hot_spot;		/* Hot spot of the bitmap image */
End;

NewClass(text_margin)
  ABSTRACT_GRAPHICAL			/* Abstract class graphical */
  Editor	editor;			/* Editor we are associated with */
  Size		gap;			/* X and Y distance between icons */
End;


		/********************************
		*            TEXTIMAGE		*
		********************************/

#define EOB	(-1)			/* end-of-buffer */

#define TEXT_SCAN_FOR	0
#define TEXT_SKIP_OVER	1

typedef struct text_screen	* TextScreen;
typedef struct text_char	* TextChar;
typedef struct text_line	* TextLine;

typedef void (*SeekFunction) P((Any, long));
typedef long (*ScanFunction) P((Any, long, int, int, int, int *));
typedef long (*FetchFunction) P((Any, TextChar));

struct text_char
{ int		c;			/* The character at this position */
  FontObj	font;			/* Font of this character */
  Colour	colour;			/* Colour of this character */
  short		x;			/* X-position in line (pixels) */
  unsigned char attributes;		/* Its attributes */
};

struct text_line
{ long		start;			/* Start index (relative) */
  long		end;			/* Last index (relative) */
  short		y;			/* Y-position in pixels */
  short		h;			/* Heigth in pixels */
  short		w;			/* Width of displayed text */
  short		base;			/* Baseline (relative to y) */
  short		length;			/* Number of characters displayed */
  short		allocated;		/* Size of chars array */
  int		changed;		/* Line has been changed? */
  int		ends_because;		/* END_WRAP; END_EOF; END_NL */
  TextChar	chars;			/* Character descriptions */
};

struct text_screen
{ short		skip;			/* Skip this many screen lines */
  short		length;			/* Number of lines displayed */
  short		allocated;		/* Allocated entries of the array */
  TextLine	lines;			/* The actual line structure */
};


NewClass(text_image)			/* TBD: subclass of bitmap? */
  ABSTRACT_GRAPHICAL
  Any   	text;			/* Text we are operation on */
  Any		background;		/* Background of text */
  Int		start;			/* Start offset */
  Int		end;			/* First non-visible character */
  Name		wrap;			/* Wrap mode in effect */
  Int		tab_distance;		/* Tab distance in pixels */
  Vector	tab_stops;		/* Vector of tab-stops (pixels) */
  Bool		eof_in_window;		/* EOF is in the window */
					/* start private data */
  int		w;			/* Used width in pixels */
  int		h;			/* Used height in pixels */
  int		change_start;		/* Start of changes */
  int		change_end;		/* End of changes */
  int		inserted;       	/* Number of chars inserted/deleted */
  SeekFunction  seek;			/* Seek to position */
  ScanFunction	scan;			/* Scan for character type */
  FetchFunction fetch;			/* Function to fetch characters */
  TextScreen	map;			/* Describes the text object */
End;

#endif /* _PCE_TXT_INCLUDED */
