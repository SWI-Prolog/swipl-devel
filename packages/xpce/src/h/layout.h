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
#ifndef BOXES_H_INCLUDED
typedef struct rubber		*Rubber;
#endif

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
    Int		width;			/* Total width (or @default) */
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
    Rubber	hrubber;		/* Horizonal stretchability */
    Rubber	vrubber;		/* Vertical stretchability */
    Int		col_span;		/* number of columns spanned */
    Int		row_span;		/* number of rows spanned */
    Size	cell_padding;		/* Padding for this cell (default) */
    Bool	selected;		/* Cell is selected */ \
    Any		background;		/* Background colour in */
    Image	note_mark;		/* Mark for (foot-) notes */
End;

#define ABSTRACT_TABLE_SLICE \
    ABSTRACT_VECTOR			/* vector attributes */ \
    Table	table;			/* Table I belong to */ \
    Any		background;		/* Background colour in */ \
    Bool	selected;		/* Default <-selected of cells */ \
    Name	alignment;		/* halign,valign */ \
    Bool	end_group;		/* Slice ends a (row/column) group */ \
    Name	name;			/* Name of the slice */ \
    Int		index;			/* nth row/column */ \
    Bool	fixed;			/* Width/reference is fixed */ \
    Int		width;			/* width/height of the row/column */ \
    Int		reference;		/* position of reference-aligned */ \
    Int		position;		/* Offset of row/column */ \
    Rubber	rubber;			/* Stretch/Shrinkability */ \
    Bool	displayed;		/* @off: slice is hidden */

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

#include <fmt/proto.h>			/* function prototypes */

#endif /*LAYOUT_H_INCLUDED*/
