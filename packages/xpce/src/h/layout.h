/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1997 University of Amsterdam. All rights reserved.
*/

#ifndef LAYOUT_H_INCLUDED
#define LAYOUT_H_INCLUDED
#include <h/graphics.h>

GLOBAL Class ClassLayoutManager;
GLOBAL Class ClassLayoutInterface;
GLOBAL Class ClassTable;
GLOBAL Class ClassTableSlice;
GLOBAL Class ClassTableRow;
GLOBAL Class ClassTableColumn;
GLOBAL Class ClassTableCell;

typedef struct layout_manager	*LayoutManager;
typedef struct layout_interface *LayoutInterface;
typedef struct table		*Table;
typedef struct table_slice	*TableSlice;
typedef struct table_row	*TableRow;
typedef struct table_column	*TableColumn;
typedef struct table_cell	*TableCell;

#define ABSTRACT_LAYOUT_MANAGER \
    Device	device;			/* Device managed */ \
    Any		request_compute;	/* Layout needs recomputed? */

#define ABSTRACT_LAYOUT_INTERFACE \
    LayoutManager layout_manager;	/* The layout manager */ \
    Graphical	  image;		/* Graphical managed */

NewClass(layout_manager)
    ABSTRACT_LAYOUT_MANAGER
End;

NewClass(layout_interface)
    ABSTRACT_LAYOUT_INTERFACE
End;

NewClass(table)
    ABSTRACT_LAYOUT_MANAGER
    Vector	rows;			/* Vector of holding the rows */
    Vector	columns;		/* Vector holding column info */
    Int		border;			/* border around cells */
    Name	frame;			/* Parts of the frame painted */
    Name	rules;			/* Which rules are painted */
    Size	cell_padding;		/* default padding around cells */
    Size	cell_spacing;		/* Space between cells */
    Point	current;		/* Current X-Y location */
					/* internal stuff */
    Area	area;			/* Total occupied area */
    Bool	changed;		/* layout-changing action */
End;

NewClass(table_cell)
    ABSTRACT_LAYOUT_INTERFACE
    Int		column;			/* X-location in table */
    Int		row;			/* Y-location in table */
    Name	halign;			/* left, right, center, ref, stretch */
    Name	valign;			/* top, bottom, center, ref, stretch */
    Int		col_span;		/* number of columns spanned */
    Int		row_span;		/* number of rows spanned */
    Size	cell_padding;		/* Padding for this cell (default) */
    Bool	selected;		/* Cell is selected */ \
    Any		background;		/* Background colour in */
End;

#define ABSTRACT_TABLE_SLICE \
    ABSTRACT_VECTOR			/* vector attributes */ \
    Table	table;			/* Table I belong to */ \
    Any		background;		/* Background colour in */ \
    Bool	selected;		/* Default <-selected for cells */ \
    Name	alignment;		/* halign,valign */ \
    Bool	end_group;		/* Slice ends a (row/column) group */ \
    Int		index;			/* nth row/column */ \
    Bool	fixed;			/* Width/reference is fixed */ \
    Int		width;			/* width/height of the row/column */ \
    Int		reference;		/* position of reference-aligned */ \
    Int		position;		/* Offset of row/column */

NewClass(table_slice)
    ABSTRACT_TABLE_SLICE
End;

NewClass(table_row)
    ABSTRACT_TABLE_SLICE
End;

NewClass(table_column)
    ABSTRACT_TABLE_SLICE
End;

typedef struct _table_cell_dimensions
{ int	x;				/* X-location */
  int	y;				/* Y-location */
  int	w;				/* Width of the cell (outer) */
  int	h;				/* heigth */
  int	rx;				/* reference-X */
  int	ry;				/* reference-Y */
  int	px;				/* H-padding */
  int	py;				/* V-padding */
} table_cell_dimensions, *TableCellDimensions;


#if 0
NewClass(geometry_preferences)		/* Steps? */
    Int		width;			/* (ideal) width */
    Int		height;			/* (ideal) width */
    Int		hor_stretch;		/* Horizontal stretchability */
    Int		hor_shrink;		/* Horizontal shrinkability */
    Int		ver_stretch;		/* Verticak stretchability */
    Int		ver_shrink;		/* Verticak shrinkability */
End;
#endif

#include <fmt/proto.h>			/* function prototypes */

#endif /*LAYOUT_H_INCLUDED*/
