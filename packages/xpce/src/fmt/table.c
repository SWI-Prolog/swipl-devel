/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

#include <box/boxes.h>			/* to exploit class rubber */

static void	advance_table(Table tab);
static status	placeCellsTable(Table tab);
static void	frame_border(Table tab, int *tb, int *rb, int *bb, int *lb);

static status
initialiseTable(Table tab)
{ initialiseLayoutManager(tab);

  assign(tab, rows,    newObject(ClassVector, EAV));
  assign(tab, columns, newObject(ClassVector, EAV));
  assign(tab, current, newObject(ClassPoint, ONE, ONE, EAV));
  assign(tab, area,    newObject(ClassArea, EAV));
  assign(tab, changed, OFF);
  assign(tab, width,   DEFAULT);

  obtainClassVariablesObject(tab);

  succeed;
}

		 /*******************************
		 *	    ITERATIONS		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
for_cells_table(Table, CellVar, CellCode, EndRowCode)
    Iterates over the contents of the table, calling CellCode for each
    cell and EndRowCode after each row.

    The `int' variables row and col contain the current row/column number.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define for_cells_table(tab, c, code, endrowcode) \
	{ int _nrows = valInt((tab)->rows->size); \
	  int _roff = valInt((tab)->rows->offset)+1; \
	  int row; \
	  for(row = _roff; row < _nrows+_roff; row++) \
	  { TableRow _trow = (tab)->rows->elements[row-_roff]; \
 \
	    if ( notNil(_trow) ) \
	    { int _ncols = valInt(_trow->size); \
	      int _coff = valInt(_trow->offset)+1; \
	      int col; \
	      for(col = _coff; col < _ncols+_coff; col++) \
	      { TableCell c = _trow->elements[col-_coff]; \
 \
		if ( notNil(c) && \
		     valInt(c->column) == col && valInt(c->row) == row ) \
		{ code; \
		} \
	      } \
	    } \
 	    endrowcode; \
	  } \
	}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
NOTE: This method  may  only  be  used   after  all  columns  have  been
established! RedrawRulesTable() is ok as  this   is  done after compute,
which establishes all column objects.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define for_displayed_cells_table(tab, c, code, endrowcode) \
	{ int _nrows = valInt((tab)->rows->size); \
	  int _roff = valInt((tab)->rows->offset)+1; \
	  int _cvoff = valInt((tab)->columns->offset)+1; \
	  int row; \
	  for(row = _roff; row < _nrows+_roff; row++) \
	  { TableRow _trow = (tab)->rows->elements[row-_roff]; \
 \
	    if ( notNil(_trow) && _trow->displayed == ON ) \
	    { int _ncols = valInt(_trow->size); \
	      int _coff = valInt(_trow->offset)+1; \
	      int col; \
	      for(col = _coff; col < _ncols+_coff; col++) \
	      { TableColumn _tcol = (tab)->columns->elements[col-_cvoff]; \
		if ( notNil(_tcol) && _tcol->displayed == ON ) \
		{ TableCell c = _trow->elements[col-_coff]; \
 \
		  if ( notNil(c) && \
		       valInt(c->column) == col && valInt(c->row) == row ) \
		  { code; \
		  } \
		} \
	      } \
	    } \
 	    endrowcode; \
	  } \
	}


#define for_rows_table(tab, r, code) \
	{ int _nrows = valInt((tab)->rows->size); \
	  int _roff = valInt((tab)->rows->offset)+1; \
	  int crow; \
	  for(crow = _roff; crow < _nrows+_roff; crow++) \
	  { TableRow r = (tab)->rows->elements[crow-_roff]; \
	    code; \
	  } \
	}

#define for_cols_table(tab, c, code) \
	{ int _ncols = valInt((tab)->columns->size); \
	  int _coff = valInt((tab)->columns->offset)+1; \
	  int ccol; \
	  for(ccol = _roff; ccol < _ncols+_coff; ccol++) \
	  { TableColumn c = (tab)->columns->elements[ccol-_coff]; \
	    code; \
	  } \
	}


static inline Table
table_of_cell(TableCell cell)
{ Table tab = (Table)cell->layout_manager;

  if ( isNil(tab) )
    fail;
  
  answer(tab);
}

		 /*******************************
		 *	       PARTS		*
		 *******************************/

TableRow
getRowTable(Table tab, Any y, Bool create)
{ if ( isInteger(y) )
  { TableRow row = getElementVector(tab->rows, y);

    if ( isNil(row) )
      row = FAIL;
  
    if ( !row && create == ON )
    { elementVector(tab->rows, y, (row=newObject(ClassTableRow, EAV)));
      assign(row, table, tab);
      assign(row, index, y);
    }

    return row;
  }

  answer(findNamedSlice(tab->rows, y));
}


TableColumn
getColumnTable(Table tab, Any x, Bool create)
{ if ( isInteger(x) )
  { TableColumn col = getElementVector(tab->columns, x);

    if ( isNil(col) )
      col = FAIL;
  
    if ( !col && create == ON )
    { elementVector(tab->columns, x, (col=newObject(ClassTableColumn, EAV)));
      assign(col, table, tab);
      assign(col, index, x);
    }
  
    return col;
  }

  answer(findNamedSlice(tab->columns, x));
}


static TableCell
getCellTable(Table tab, Any x, Any y)
{ TableRow row = getRowTable(tab, y, OFF);

  if ( row && notNil(row) )
  { TableCell cell = NULL;

    if ( isInteger(x) )
    { cell = getElementVector((Vector)row, x);
    } else
    { TableColumn col = getColumnTable(tab, x, OFF);
      if ( col )
	cell = getElementVector((Vector)row, col->index);
    }

    if ( cell && notNil(cell) )
      answer(cell);
  }

  fail;
}

		 /*******************************
		 *	       BUILDING		*
		 *******************************/

void
table_row_range(Table tab, int *ymin, int *ymax)
{ Vector rows = tab->rows;

  *ymin = valInt(getLowIndexVector(rows));
  *ymax = valInt(getHighIndexVector(rows));
}


void
table_column_range(Table tab, int *xmin, int *xmax)
{ Vector rows = tab->rows;
  int low=0, high=0;
  int y, ymin, ymax;
  int first = TRUE;

  table_row_range(tab, &ymin, &ymax);

  for(y=ymin; y<=ymax; y++)
  { TableRow row = getElementVector(rows, toInt(y));

    if ( row && notNil(row) )
    { int l = valInt(getLowIndexVector((Vector)row));
      int h = valInt(getHighIndexVector((Vector)row));

      if ( first )
      { low   = l;
	high  = h;
	first = FALSE;
      } else
      { low   = min(low, l);
	high  = max(high, h);
      }
    }
  }

  *xmin = low;
  *xmax = high;
}


static Tuple
getColumnRangeTable(Table tab)
{ int cmin, cmax;

  table_column_range(tab, &cmin, &cmax);
  
  answer(answerObject(ClassTuple, toInt(cmin), toInt(cmax), EAV));
}


static Tuple
getRowRangeTable(Table tab)
{ int rmin, rmax;

  table_row_range(tab, &rmin, &rmax);
  
  answer(answerObject(ClassTuple, toInt(rmin), toInt(rmax), EAV));
}


static status
appendTable(Table tab, TableCell cell, Int x, Int y)
{ int cspan = valInt(cell->col_span);
  int rspan = valInt(cell->row_span);
  int dy;

  if ( isDefault(x) )
    x = tab->current->x;
  if ( isDefault(y) )
    y = tab->current->y;

  if ( notNil(tab->device) && notNil(cell->image) )
    send(tab->device, NAME_display, cell->image, EAV);

  assign(cell, layout_manager, tab);
  assign(cell, column, x);
  assign(cell, row, y);

  for(dy=0; dy<rspan; dy++)
  { Int cy = toInt(valInt(y)+dy);
    TableRow row = getRowTable(tab, cy, ON);
    int dx;

    for(dx=0; dx<cspan; dx++)
    { Int cx = toInt(valInt(x)+dx);

      cellTableRow(row, cx, cell);
    }
  }

  advance_table(tab);
  requestComputeLayoutManager((LayoutManager)tab, DEFAULT);

  return changedTable(tab);
}


static status
nextRowTable(Table tab, Bool end_group)
{ if ( end_group == ON )
  { TableRow r = getRowTable(tab, tab->current->y, ON);

    send(r, NAME_endGroup, ON, EAV);
  }

  assign(tab->current, x, ONE);
  assign(tab->current, y, add(tab->current->y, ONE));
  advance_table(tab);

  succeed;
}


/*  advance_table() skips over spanned cells to find the first free
    location (starting from the current) in the table
*/

static void
advance_table(Table tab)
{ Point c = tab->current;
  TableRow row = getRowTable(tab, c->y, ON);

  if ( row )
  { int cx = valInt(c->x);

    while( getCellTableRow(row, toInt(cx)) )
      cx++;

    assign(c, x, toInt(cx));
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Big question is what to do with the   graphicals.  The ideal is to leave
this to the incremental  garbage  collector,   but  graphicals  use many
cyclic structures, which are  not  handled   correctly  by  the  garbage
collector.  For now we will destroy them if they are not locked.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
 
static status
removeCellImageTable(Table tab, TableCell cell, Bool keep)
{ Graphical gr = cell->image;

  if ( notNil(gr) )
  { DeviceGraphical(gr, NIL);
    if ( keep != ON && !onFlag(gr, F_PROTECTED|F_LOCKED|F_FREED) )
      qadSendv(gr, NAME_destroy, 0, NULL);
  }

  succeed;
}


static status
deleteCellTable(Table tab, TableCell cell, Bool keep)
{ if ( cell->layout_manager == (LayoutManager)tab )
  { int tx = valInt(cell->column) + valInt(cell->col_span);
    int ty = valInt(cell->row) + valInt(cell->row_span);
    int x, y;

    removeCellImageTable(tab, cell, keep);
  
    for(y=valInt(cell->row); y<ty; y++)
    { TableRow row = getRowTable(tab, toInt(y), OFF);
      
      if ( row )
      { for(x=valInt(cell->column); x<tx; x++)
	  elementVector((Vector)row, toInt(x), NIL);
      }
    }

    assign(cell, layout_manager, NIL);

    changedTable(tab);
    requestComputeLayoutManager((LayoutManager)tab, DEFAULT);
  }

  succeed;
}


		 /*******************************
		 *   INSERT/DELETE ROW/COLUMN	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Delete a row/column. Shift the  cells,   updating  their X/Y coordinate.
Cells spanning the deleted row/column span   a row/column less after the
deletion, so they span the same columns.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static status
deleteRowTable(Table tab, TableRow row, Bool keep)
{ int i, rown = valInt(row->index);
  int rmin, rmax;

  table_row_range(tab, &rmin, &rmax);
					/* deal with cells spanned over */
					/* this row */
  for_vector_i(row, TableCell cell, i,
	       { if ( notNil(cell) && i == valInt(cell->column) )
		 { if ( cell->row_span != ONE )
		   { if ( cell->row == row->index )
		       assign(cell, row, inc(cell->row));
		     assign(cell, row_span, dec(cell->row_span));
		   } else if ( cell->row == row->index &&
			       notNil(cell->image) )
		   { removeCellImageTable(tab, cell, keep);
		   }
		   freeObject(cell);
		 }
	       });

  assign(row, table, NIL);		/* detach to avoid recursion! */

					/* delete the row itself */
  for(i=rown; i <= rmax; i++)
  { TableRow r2;

    if ( (r2=getRowTable(tab, toInt(i+1), OFF)) )
    { indexTableRow(r2, toInt(i));
      elementVector(tab->rows, toInt(i), r2);
    } else
      elementVector(tab->rows, toInt(i), NIL);
  }

					/* decrement the row-range */
  rangeVector(tab->rows, DEFAULT, toInt(rmax-1));

  changedTable(tab);
  return requestComputeLayoutManager((LayoutManager)tab, DEFAULT);
}


static status
deleteColumnTable(Table tab, TableColumn col, Bool keep)
{ int coln = valInt(col->index);
  int y, rmin, rmax;
  int x, cmax;

  table_row_range(tab, &rmin, &rmax);
  cmax = valInt(getHighIndexVector(tab->columns));

					/* update the rows */
  for(y=rmin; y<=rmax; y++)
  { TableRow row = getRowTable(tab, toInt(y), OFF);

    if ( row )
    { int xmax = valInt(getHighIndexVector((Vector)row));
      TableCell cell;

      if ( (cell = getCellTableRow(row, col->index)) )
      { if ( cell->row == toInt(y) )
	{ if ( cell->col_span != ONE )
	  { if ( cell->column == col->index )
	      assign(cell, column, inc(cell->column));
	    assign(cell, col_span, dec(cell->col_span));
	  } else if ( cell->column == col->index && notNil(cell->image) )
	  { if ( !isFreeingObj(col) )
	      elementVector((Vector)col, toInt(y), cell);

	    removeCellImageTable(tab, cell, keep);
	  }
	}
      }

      for(x=coln; x<=xmax; x++)
      { if ( (cell = getCellTableRow(row, toInt(x+1))) )
	{ if ( cell->column == toInt(x+1) )
	    assign(cell, column, toInt(x));
	  elementVector((Vector)row, toInt(x), cell);
	} else
	  elementVector((Vector)row, toInt(x), NIL);
      }

      if ( xmax >= coln )
	rangeVector((Vector)row, DEFAULT, toInt(xmax-1));
    }
  }

  assign(col, table, NIL);		/* detach to avoid recursion! */

					/* remove from tab->columns */
  for(x=coln; x<cmax; x++)
  { TableColumn c2;

    if ( (c2=getElementVector(tab->columns, toInt(x+1))) )
    { assign(c2, index, toInt(x));
      elementVector(tab->columns, toInt(x), c2);
    } else
      elementVector(tab->columns, toInt(x), NIL);
  }
  rangeVector(tab->columns, DEFAULT, toInt(cmax-1));

  changedTable(tab);
  return requestComputeLayoutManager((LayoutManager)tab, DEFAULT);
}


static status
deleteTable(Table tab, Any obj, Bool keep)
{ if ( instanceOfObject(obj, ClassTableCell) )
    return deleteCellTable(tab, obj, keep);
  if ( instanceOfObject(obj, ClassTableRow) )
    return deleteRowTable(tab, obj, keep);
  if ( instanceOfObject(obj, ClassTableColumn) )
    return deleteColumnTable(tab, obj, keep);

  fail;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This one uses a different algorithm if the   full table is deleted, as a
partial delete requires a lot of work to deal properly with spanning.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static status
deleteRowsTable(Table tab, Int from, Int to, Bool keep)
{ int y, rmin, rmax, f, t;

  table_row_range(tab, &rmin, &rmax);
  if ( isDefault(from) )
  { f = rmin;
  } else
  { f = valInt(from);
    f = max(rmin, f);
  }
  if ( isDefault(to) )
  { t = rmax;
  } else
  { t = valInt(to);
    t = min(rmax, t);
  }

  if ( f == rmin && t == rmax )		/* full delete */
  { for(y=rmin; y<=rmax; y++)
    { TableRow r = getRowTable(tab, toInt(y), OFF);
  
      if ( r )
      { for_vector_i(r, TableCell cell, i,
		 { if ( i == valInt(cell->column) &&
			cell->row == r->index &&
			notNil(cell->image) )
		   { removeCellImageTable(tab, cell, keep);
		     freeObject(cell);
		   }
		 });
	assign(r, table, NIL);
	freeObject(r);
      }
    }  
  
    clearVector(tab->rows);
    setPoint(tab->current, ONE, ONE);
  
    changedTable(tab);

    return requestComputeLayoutManager((LayoutManager)tab, DEFAULT);
  } else				/* partial delete */
  { for(y=t; y>=f; y--)
    { TableRow r = getRowTable(tab, toInt(y), OFF);

      if ( r )
	deleteRowTable(tab, r, keep);
    }

    return setPoint(tab->current, ONE, toInt(f));
  }
}


static status
insertRowTable(Table tab, Int at, TableRow newrow)
{ int y, ymin, ymax;
  int here = valInt(at);
  TableRow movedrow;

  table_row_range(tab, &ymin, &ymax);
  for(y=ymax; y>=here; y--)
  { TableRow r2;

    if ( (r2 = getRowTable(tab, toInt(y), OFF)) )
    { indexTableRow(r2, toInt(y+1));
      elementVector(tab->rows, toInt(y+1), r2);
    } else
      elementVector(tab->rows, toInt(y+1), NIL);
  }

  elementVector(tab->rows, at, NIL);
  if ( isDefault(newrow) )
    newrow = get(tab, NAME_row, at, ON, EAV);
  else
  { elementVector(tab->rows, at, newrow);
    assign(newrow, table, tab);
    assign(newrow, index, at);
    indexTableRow(newrow, at);
    
    for_vector(newrow, TableCell cell,
	       if ( notNil(cell) )
	       { assign(cell, layout_manager, tab);
		 assign(cell, row, at);

		 if ( notNil(tab->device) && notNil(cell->image) &&
		      cell->image->device != tab->device )
		   send(tab->device, NAME_display, cell->image, EAV);
	       });
  }

  if ( (movedrow = getRowTable(tab, toInt(here+1), OFF)) )
  { for_vector_i(movedrow, TableCell cell, i,
		 if ( cell->row_span != ONE &&
		      cell->column == toInt(i) &&
		      valInt(cell->row) < here )
		 { int x;
  
		   assign(cell, row_span, inc(cell->row_span));
  
		   for(x = i; x < i + valInt(cell->col_span); x++)
		     cellTableRow(newrow, toInt(x), cell);
		 });
  }

  changedTable(tab);
  return requestComputeLayoutManager((LayoutManager)tab, DEFAULT);
}


static status
insertColumnTable(Table tab, Int at, TableColumn new)
{ int y, ymin, ymax;
  int here = valInt(at);
  int x, cmax;

  table_row_range(tab, &ymin, &ymax);
  cmax = valInt(getHighIndexVector(tab->columns));

					/* make room in row structures */
  for(y=ymin; y<=ymax; y++)
  { TableRow row = getRowTable(tab, toInt(y), OFF);

    if ( row )
    { int x, xmax = valInt(getHighIndexVector((Vector)row));

      for(x=xmax; x>=here; x--)
      { TableCell cell = getCellTableRow(row, toInt(x));

	if ( cell )
	{ if ( cell->column == toInt(x) && cell->row == toInt(y) )
	    assign(cell, column, toInt(x+1));
	  elementVector((Vector)row, toInt(x+1), cell);
	} else
	  elementVector((Vector)row, toInt(x+1), NIL);
      }

      elementVector((Vector)row, at, NIL);
    }
  }

					/* update tab<-columns */
  for(x=cmax; x>=here; x--)
  { TableColumn col = getElementVector(tab->columns, toInt(x));

    if ( col )
      assign(col, index, toInt(x+1));
    else
      col = NIL;

    elementVector(tab->columns, toInt(x+1), col);
  }

					/* fix spanned cells */
  for(y=ymin; y<=ymax; y++)
  { TableRow row = getRowTable(tab, toInt(y), OFF);

    if ( row )
    { TableCell cell = getCellTableRow(row, toInt(here+1));

      if ( cell &&
	   cell->col_span != ONE &&
	   cell->row == toInt(y) &&
	   valInt(cell->column) < here )
      { int y2;

	assign(cell, col_span, inc(cell->col_span));

	for(y2 = y; y2 < y + valInt(cell->row_span); y2++)
	{ TableRow r2 = getRowTable(tab, toInt(y2), ON);

	  DEBUG(NAME_colSpan,
		Cprintf("Copying spanned cell to %s %d\n", pp(at), y2));

	  cellTableRow(r2, at, cell);
	}
      }
    }
  }

					/* make room and insert a new column */
  elementVector(tab->columns, at, NIL);
  if ( isDefault(new) )
  { getColumnTable(tab, at, ON);
  } else
  { elementVector(tab->columns, at, new);
    assign(new, table, tab);
    assign(new, index, at);

    for_vector_i(new, TableCell cell, i,
		 if ( notNil(cell) )
		 { appendTable(tab, cell, at, toInt(i));
		   elementVector((Vector)new, toInt(i), NIL);
		 });
    clearVector((Vector)new);
  }


  changedTable(tab);
  return requestComputeLayoutManager((LayoutManager)tab, DEFAULT);
}

		 /*******************************
		 *	      SORTING		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Sort a range of rows in the table:

	(1) Sort the underlying row-structures.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static status
sortRowsTable(Table tab, Code cmp, Int from, Int to)
{ int y, ymin, ymax;
  Vector rows = tab->rows;

  table_row_range(tab, &ymin, &ymax);		/* deteymine row-range */
  if ( notDefault(from) )
    ymin = max(ymin, valInt(from));
  if ( notDefault(to) )
    ymax = min(ymax, valInt(to));
  if ( ymax <= ymin )
    succeed;

  for(y=ymin; y<=ymax; y++)		/* check row-spanned references */
  { TableRow r = getRowTable(tab, toInt(y), OFF);

    if ( r )
    { for_vector(r, TableCell cell,
		 { if ( notNil(cell) && cell->row != r->index )
		   { errorPce(tab, NAME_spannedRow, cell);
		   }
		 });
    }
  }

  send(rows, NAME_sort, cmp, toInt(ymin), toInt(ymax), EAV);
  
  for(y=ymin; y<=ymax; y++)		/* correct row-offsets */
  { TableRow r = getRowTable(tab, toInt(y), OFF);

    if ( r )
    { assign(r, index, toInt(y));

      for_vector(r, TableCell cell,
		 { if ( notNil(cell) )
		   { assign(cell, row, r->index);
		   }
		 });
    }
  }
  
  changedTable(tab);
  return requestComputeLayoutManager((LayoutManager)tab, DEFAULT);
}


		 /*******************************
		 *	     SELECTION		*
		 *******************************/

static status
clearSelectionTable(Table tab)
{ for_cells_table(tab, cell,
		  if ( cell->selected == ON )
		  { send(cell, NAME_selected, OFF, EAV);
		  }, ;);

  succeed;
}


static status
selectTable(Table tab, Any selection)
{ if ( instanceOfObject(selection, ClassChain) )
  { Chain ch = selection;
    Cell cell;
    status rval = SUCCEED;

    for_cell(cell, ch)
    { if ( !selectTable(tab, cell->value) )
	rval = FAIL;
    }

    return rval;
  } else if ( isNil(selection) )
  { succeed;
  } else
    return send(selection, NAME_selected, ON, EAV);
}


static status
selectionTable(Table tab, Any selection)
{ clearSelectionTable(tab);

  return selectTable(tab, selection);
}


static Chain
getSelectionTable(Table tab)
{ Chain rval = FAIL;

  for_cells_table(tab, cell,
		  { if ( cell->selected == ON )
		    { if ( !rval )
			rval = answerObject(ClassChain, cell, EAV);
		      else
			appendChain(rval, cell);
		    }
		  }, ;);

  return rval;
}


		 /*******************************
		 *	       EVENT		*
		 *******************************/

Any
getCellFromPositionTable(Table tab, Any pos, Bool onborder)
{ Point pt;
  int x, y;
  int tx, ty;				/* X/Y tolerance */

  if ( instanceOfObject(pos, ClassPoint) )
    pt = pos;
  else if ( notNil(tab->device) )
    pt = getPositionEvent(pos, tab->device);
  else
    fail;

  ComputeLayoutManager(tab);		/* make sure it is up-to-date */
  
  x = valInt(pt->x);
  y = valInt(pt->y);

  if ( onborder == ON )
  { tx = valInt(tab->cell_spacing->w);
    ty = valInt(tab->cell_spacing->h);

    if ( tx > 0 ) tx = (tx+1)/2;
    if ( ty > 0 ) ty = (ty+1)/2;
  } else
    tx = ty = 0;

  for_rows_table(tab, row,
		 { int ry = valInt(row->position);
		   int rh = valInt(row->width);

		   if ( ry-ty < y && ry+rh+ty >= y )
		   { for_cols_table(tab, col,
				    { int cx = valInt(col->position);
				      int cw = valInt(col->width);

				      if ( cx-tx < x && cx+cw+tx >= x )
				      { TableCell c;
  
					c = getCellTableRow(row, col->index);
					if ( c )
					  answer(c);
					else
					  answer(answerObject(ClassPoint,
							      col->index,
							      row->index,
							      EAV));
				      }
				    });
		   }
		 });

  fail;
}


#undef swapint
#define swapint(a, b) { int _tmp = (b); (b)=(a); (a)=(_tmp); }

static Chain
getCellsInRegionTable(Table tab, Area reg)
{ int minx = valInt(reg->x);
  int maxx = minx + valInt(reg->w);
  int miny = valInt(reg->y);
  int maxy = miny + valInt(reg->h);
  int y;
  Chain rval = answerObject(ClassChain, EAV);

  if ( maxx < minx )
    swapint(maxx, minx);
  if ( maxy < miny )
    swapint(maxy, miny);

  for(y=miny; y<maxy; y++)
  { TableRow row = getRowTable(tab, toInt(y), OFF);

    if ( row )
    { int x;

      for(x = minx; x<maxx; x++)
      { TableCell cell = getCellTableRow(row, toInt(x));

	if ( cell && cell->column == toInt(x) && cell->row == toInt(y) )
	  appendChain(rval, cell);
      }
    }
  } 

  answer(rval);
}


static int
userResizeSliceTable(Table tab, TableSlice slice, Int size)
{ if ( instanceOfObject(slice, ClassTableColumn) )
  { int xmin, xmax;

    table_column_range(tab, &xmin, &xmax);
    if ( valInt(slice->index) >= xmax )
    { int tabw = valInt(size) + valInt(slice->position);

      send(tab, NAME_width, toInt(tabw), EAV);
    } else
    { int i;

      for(i=xmin; i<=xmax; i++)
      { TableColumn col = getColumnTable(tab, toInt(i), OFF);

	if ( col )
	  assign(col, fixed, i <= valInt(slice->index) ? ON : OFF);
      }

      send(slice, NAME_width, size, EAV);
    }
  } else
  { int ymin, ymax;

    table_row_range(tab, &ymin, &ymax);
    if ( valInt(slice->index) >= ymax )
    { int tabh = valInt(size) + valInt(slice->position);

      send(tab, NAME_height, toInt(tabh), EAV);
    } else
      send(slice, NAME_height, size, EAV);
  }

  succeed;
}


		 /*******************************
		 *	       LAYOUT		*
		 *******************************/

status
changedTable(Table tab)
{ assign(tab, changed, ON);

  if ( notNil(tab->device) )
  { Area a = tab->area;

    changedImageGraphical(tab->device, a->x, a->y, a->w, a->h);
  }

  succeed;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Computing the table layout.  This  process   consists  of  the following
stages:

	# Determine row/column-dimensions
	In the first phase, the dimensions of the individual rows and
	columns is computed, *only* considering non-spanned cells.

	# Resolve spanning cells
	This two considers rows and columns in seperate and similar
	stages.  We first collect all spanning cells.  Then, we sort
	them to increasing number of spanned rows/columns.  Then we
	resolve each of the cells in turn, only making columns/cells
	wider.  Currently, the space is spread equally over the
	affected rows/columns.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
span(TableCell cell, Name which)
{ return valInt(which == NAME_colSpan ? cell->col_span : cell->row_span);
}


static void
addSpannedCell(Chain *rval, TableCell tc, Name which)
{ if ( ! *rval )
    *rval = answerObject(ClassChain, tc, EAV);
  else
  { Cell cell;
    int myspan = span(tc, which);

    for_cell(cell, *rval)
    { if ( span(cell->value, which) > myspan )
      {	insertBeforeChain(*rval, tc, cell->value);
	return;
      }
    }
    appendChain(*rval, tc);
  }
}


/*  Return a chain holding all col/row spanning cells in ascending order
    `which' is one of NAME_rowSpan or NAME_colSpan
*/

static Chain
getSpannedCellsTable(Table tab, Name which)
{ Chain rval = FAIL;

  for_cells_table(tab, c,
		    if ( span(c, which) > 1 )
		      addSpannedCell(&rval, c, which);, ;)

  answer(rval);
}


static void
slice_stretchability(TableSlice slice, stretch *s)
{ if ( notNil(slice->rubber) )
  { Rubber r = slice->rubber;

    s->ideal   = isDefault(r->natural) ? valInt(slice->width)
				       : valInt(r->natural);
    s->minimum = isNil(r->minimum) ? 0 : valInt(r->minimum);
    s->maximum = isNil(r->maximum) ? INT_MAX : valInt(r->maximum);
    s->stretch = valInt(r->stretch);
    s->shrink  = valInt(r->shrink);
  } else
  { s->ideal   = valInt(slice->width);
    s->minimum = s->ideal;
    s->maximum = INT_MAX;
    s->stretch = 100;
    s->shrink  = 0;
  }

  if ( slice->fixed == ON )		/* how to interact with <-rubber */
    s->stretch = s->shrink = 0;
}


void
cell_stretchability(TableCell cell, Name which, stretch *s)
{ Rubber r;

  if ( notNil(cell->image) )
  { int px, py;

    table_cell_padding(cell, &px, &py);

    if ( which == NAME_column )
      s->ideal = valInt(cell->image->area->w) + 2*px;
    else
      s->ideal = valInt(cell->image->area->h) + 2*py;
      
    s->minimum = s->ideal;
    s->maximum = INT_MAX;
    s->stretch = 100;
    s->shrink  = 0;
  }

  r = (which == NAME_column ? cell->hrubber : cell->vrubber);

  if ( notNil(r) )
  { if ( notDefault(r->natural) )
      s->ideal = valInt(r->natural);
    if ( notDefault(r->minimum) )
      s->minimum = valInt(r->minimum);
    if ( notDefault(r->maximum) )
      s->maximum = valInt(r->maximum);
    s->stretch = valInt(r->stretch);
    s->shrink  = valInt(r->shrink);
  }
}


static void
stretch_table_slices(Table tab, Vector v, /* table and <-rows or <-columns */
		     int from, int span, /* range */
		     stretch *into,	/* desired total width */
		     int spacing,	/* desired distance between slices */
		     int always)	/* do not negotiate */
{ int i, to = from+span;
  stretch *stretches = alloca(span * sizeof(stretch));
  int ngaps, nslices = 0;
  stretch tmp[2];
  stretch joined;
  int width;
  
  for(i=from; i<to; i++)
  { TableSlice slice = getElementVector(v, toInt(i));

    if ( slice && notNil(slice) && slice->displayed == ON )
    { Stretch s = &stretches[i-from];
      slice_stretchability(slice, s);
      if ( always )
      { s->stretch = max(1, s->stretch);
	s->shrink  = max(1, s->shrink);
      }
      nslices++;
    }
  }

  if ( nslices == 0 )
    return;

  DEBUG(NAME_table,
	Cprintf("%s: Stretching %d slices from %d into %d+%d-%d\n",
		pp(tab), nslices, from,
		into->ideal, into->stretch, into->shrink));

  sum_stretches(stretches, nslices, &tmp[0]);
  tmp[1] = *into;
  join_stretches(tmp, 2, &joined);

  DEBUG(NAME_table,
	Cprintf("Summed = %d+%d-%d, joined = %d+%d-%d\n",
		tmp[0].ideal, tmp[0].stretch, tmp[0].shrink,
		joined.ideal, joined.stretch, joined.shrink));

  ngaps = nslices-1;
  width = joined.ideal - ngaps*spacing;

  { distribute_stretches(stretches, nslices, width);

    for(i=from; i<to; i++)
    { TableSlice slice = getElementVector(v, toInt(i));

      if ( slice && notNil(slice) && slice->displayed == ON )
      { Any av[2];
	Name sel;

	av[0] = slice;
	av[1] = toInt(stretches[i-from].size);

	if ( instanceOfObject(slice, ClassTableColumn) )
	  sel = NAME_stretchedColumn;
	else
	  sel = NAME_stretchedRow;

	qadSendv(tab, sel, 2, av);
      }
    }
  }
}


static status
stretchedSliceTable(Table tab, TableSlice slice, Int width)
{ assign(slice, width, width);

  succeed;
} 


static void
stretchColsSpannedCell(TableCell cell)
{ if ( notNil(cell->image) )
  { int x    = valInt(cell->column);
    int span = valInt(cell->col_span);
    Table tab = (Table)cell->layout_manager;
    int colspacing = valInt(tab->cell_spacing->w);
    stretch s;

    cell_stretchability(cell, NAME_column, &s);

    stretch_table_slices(tab, tab->columns, x, span, &s, colspacing, FALSE);
  }
}


static void
stretchRowsSpannedCell(TableCell cell)
{ if ( notNil(cell->image) )
  { int y    = valInt(cell->row);
    int span = valInt(cell->row_span);
    Table tab = (Table)cell->layout_manager;
    int rowspacing = valInt(tab->cell_spacing->w);
    stretch s;

    cell_stretchability(cell, NAME_row, &s);

    stretch_table_slices(tab, tab->rows, y, span, &s, rowspacing, FALSE);
  }
}


static status
computeRowsTable(Table tab)
{ int y, ymin, ymax;
  int cy;
  Chain spanned;
  int rowspacing = valInt(tab->cell_spacing->h);
  int nrows = 0;
  int tborder, bborder;

  frame_border(tab, &tborder, NULL, &bborder, NULL);
  table_row_range(tab, &ymin, &ymax);

  for(y=ymin; y<=ymax; y++)
  { TableRow row = getRowTable(tab, toInt(y), OFF);

    if ( row && row->fixed != ON )
      send(row, NAME_compute, EAV);
  }

  if ( (spanned = getSpannedCellsTable(tab, NAME_rowSpan)) )
  { Cell cell;

    for_cell(cell, spanned)
      stretchRowsSpannedCell(cell->value);
    freeObject(spanned);
  }

  for(cy=tborder+max(0,rowspacing), y=ymin; y<=ymax; y++)
  { TableRow row = getRowTable(tab, toInt(y), OFF);

    if ( row && row->width != ZERO && row->displayed == ON )
    { if ( cy != valInt(row->position) )
      { changedTable(tab);
	assign(row, position, toInt(cy));
      }

      cy += valInt(row->width) + rowspacing;
      nrows++;
    }
  }
  cy += bborder;
  if ( rowspacing < 0 )
    cy -= rowspacing;

  if ( cy != valInt(tab->area->h) )
  { changedTable(tab);
    assign(tab->area, h, toInt(cy));
  }

  succeed;
}


static status
computeColsTable(Table tab)
{ int x, xmin, xmax;
  int cx;
  Chain spanned;
  int colspacing = valInt(tab->cell_spacing->w);
  int ncols = 0;
  int lborder, rborder;

  frame_border(tab, NULL, &rborder, NULL, &lborder);
  table_column_range(tab, &xmin, &xmax);

  for(x=xmin; x<=xmax; x++)
  { TableColumn col = getColumnTable(tab, toInt(x), ON);

    if ( col && col->fixed != ON )
      send(col, NAME_compute, EAV);
  }

  if ( notDefault(tab->width) )
  { int wc = valInt(tab->width) - lborder - rborder - 2*colspacing;
    stretch s;

    s.ideal   = wc;
    s.minimum = wc;
    s.maximum = wc;
    s.stretch = 0;
    s.shrink  = 0;

    stretch_table_slices(tab,
			 tab->columns,	/* column vector */
			 xmin,		/* first column */
			 xmax-xmin+1,	/* last column */
			 &s,		/* width to distribute */
			 colspacing,	/* distance between columns */
			 TRUE);		/* force alignment */
  }

  if ( (spanned = getSpannedCellsTable(tab, NAME_colSpan)) &&
       isDefault(tab->width) )		/* TBD: When to negotiate? */
  { Cell cell;

    for_cell(cell, spanned)
      stretchColsSpannedCell(cell->value);
    freeObject(spanned);
  }

  for(cx=lborder+max(0,colspacing), x=xmin; x<=xmax; x++)
  { TableColumn col = getColumnTable(tab, toInt(x), ON);

    if ( col->displayed == ON )
    { if ( cx != valInt(col->position) )
      { changedTable(tab);
	assign(col, position, toInt(cx));
      }
      ncols++;				/* real test for empty columns? */
      cx += valInt(col->width) + colspacing;
    }
  }
  cx += rborder;
  if ( colspacing < 0 )
    cx -= colspacing;

  if ( cx != valInt(tab->area->w) )
  { changedTable(tab);
    assign(tab->area, w, toInt(cx));
  }

  succeed;
}


static status
computeTable(Table tab)
{ if ( notNil(tab->request_compute) )
  { struct area a = *tab->area;

    assign(tab, request_compute, NAME_computing);

    computeColsTable(tab);
    computeRowsTable(tab);

    if ( tab->changed == ON )
    { Device dev = tab->device;
  
      if ( notNil(dev) && tab->border != ZERO )
      { unionNormalisedArea(&a, tab->area);
	DEBUG(NAME_table, Cprintf("Changed %d %d %d %d\n",
				    valInt(a.x), valInt(a.y),
				    valInt(a.w), valInt(a.h)));
	changedImageGraphical(dev, a.x, a.y, a.w, a.h);
      }
  
      assign(tab, changed, OFF);
    }

    placeCellsTable(tab);

    assign(tab, request_compute, NIL);
  }

  succeed;
}


static status
computeBoundingBoxTable(Table tab)
{ if ( notNil(tab->device) )
    copyArea(tab->device->area, tab->area);

  succeed;
}


static status
placeCellsTable(Table tab)
{ int ymin = valInt(getLowIndexVector(tab->rows));
  int ymax = valInt(getHighIndexVector(tab->rows));
  int xmin = valInt(getLowIndexVector(tab->columns));
  int xmax = valInt(getHighIndexVector(tab->columns));
  int y;

  for(y=ymin; y<=ymax; y++)
  { TableRow row = getRowTable(tab, toInt(y), OFF);

    if ( row )
    { int x;

      for(x=xmin; x<=xmax; x++)
      { TableCell cell  = getCellTableRow(row, toInt(x));
	TableColumn col = getColumnTable(tab, toInt(x), OFF);

	if ( cell && cell->column == col->index && cell->row == row->index )
	{ if ( row->displayed == ON && col->displayed == ON )
	    placeImageTableCell(cell);
	  else
	  { if ( notNil(cell->image) && notNil(cell->image->device) )
	    { Any av[1];

	      av[0] = NIL;
	      qadSendv(cell->image, NAME_device, 1, av);
	    }
	  }
	}
      }    
    }
  }

  succeed;
}


		 /*******************************
		 *	      REDRAW		*
		 *******************************/

static void
r_hline(int x, int y, int w, int b)
{ if ( b == 1 )
  { r_thickness(1);
    r_line(x, y, x+w-b, y);
  } else if ( b > 1 )
  { r_thickness(0);
    r_box(x, y, w, b, 0, BLACK_IMAGE);
  }
}


static void
r_vline(int x, int y, int h, int b)
{ if ( b == 1 )
  { r_thickness(1);
    r_line(x, y, x, y+h);
  } else if ( b > 1 )
  { r_thickness(0);
    r_box(x, y, b, h, 0, BLACK_IMAGE);
  }
}


static void
frame_border(Table tab, int *tb, int *rb, int *bb, int *lb)
{ int border = valInt(tab->border);
  int t=0, r=0, b=0, l=0;

  if ( tab->frame == NAME_box )
  { t = r = b = l = border;
  } else if ( tab->frame == NAME_above )
  { t = border;
  } else if ( tab->frame == NAME_below )
  { b = border;
  } else if ( tab->frame == NAME_hsides )
  { t = b = border;
  } else if ( tab->frame == NAME_vsides )
  { r = l = border;
  }

  if ( tb ) *tb = t;
  if ( rb ) *rb = r;
  if ( bb ) *bb = b;
  if ( lb ) *lb = l;
}


static status
RedrawFrameTable(Table tab, Area a)
{ if ( tab->border != ZERO )
  { Area a = tab->area;
    int x = valInt(a->x);
    int y = valInt(a->y);
    int w = valInt(a->w);
    int h = valInt(a->h);
    int b = valInt(tab->border);

    r_dash(NAME_none);
    r_thickness(b);

    if ( tab->frame == NAME_box )
    { r_box(x, y, w, h, 0, NIL);
    } else if ( tab->frame == NAME_above )
    { r_hline(x, y, w, b);		/* deal with thick lines */
    } else if ( tab->frame == NAME_below )
    { r_hline(x, y+h-b, w, b);
    } else if ( tab->frame == NAME_hsides )
    { r_hline(x, y, w, b);
      r_hline(x, y+h-b, w, b);
    } else if ( tab->frame == NAME_vsides )
    { r_vline(x, y, h, b);
      r_vline(x+w-b, y, h, b);
    }
  }

  succeed;
}


static Any
getBackgroundTableCell(TableCell cell)
{ Table tab;

  if ( notDefault(cell->background) )
    answer(cell->background);

  if ( (tab=table_of_cell(cell) ) )
  { TableRow row;
    TableColumn col;

    if ( (row = getRowTable(tab, cell->row, OFF)) &&
	 notDefault(row->background) )
      answer(row->background);
    if ( (col = getColumnTable(tab, cell->column, OFF)) &&
	 notDefault(col->background) )
      answer(row->background);
  }

  fail;
}


static Bool
getSelectedTableCell(TableCell cell)
{ Table tab;

  if ( cell->selected == ON )
    answer(ON);

  if ( (tab=table_of_cell(cell) ) )
  { TableRow row;
    TableColumn col;

    if ( (row = getRowTable(tab, cell->row, OFF)) &&
	 row->selected == ON )
      answer(ON);
    if ( (col = getColumnTable(tab, cell->column, OFF)) &&
	 col->selected == ON )
      answer(ON);
  }

  answer(OFF);
}


#define NOSIDES	  0
#define BOX_SIDES 0xf			/* top, right, bottom, left */
#define VSIDES    0x5
#define HSIDES    0xa
#define RSIDES    0x4
#define BSIDES    0x2
#define RBSIDES   0x6

static void
RedrawRulesTableCell(TableCell cell, Name style, int b)
{ Table tab = (Table)cell->layout_manager;
  table_cell_dimensions d;
  int sides = NOSIDES;
  Any bg;

  dims_table_cell(cell, &d);
  if ( (bg=getBackgroundTableCell(cell)) )
    r_fill(d.x, d.y, d.w, d.h, bg);

  if ( getSelectedTableCell(cell) == ON )
  { r_thickness(b+1);
    r_box(d.x, d.y, d.w, d.h, 0, NIL);
  }
  if ( notNil(cell->note_mark) )
  { int mw = valInt(cell->note_mark->size->w);
    int mh = valInt(cell->note_mark->size->h);

    if ( mw > d.w ) mw = d.w;		/* clip the image */
    if ( mh > d.h ) mh = d.h;
      
    DEBUG(NAME_noteMark,
	  Cprintf("%s: note_mark %s at %d,%d, size %dx%d\n",
		  pp(cell), pp(cell->note_mark), d.x+d.w-mw, d.y, mw, mh));
    r_image(cell->note_mark, 0, 0, d.x+d.w-mw, d.y, mw, mh, ON);
  }

  { if ( style == NAME_all )
    { if ( cell->selected != ON )	/* already done */
	sides = BOX_SIDES;
    } else if ( style == NAME_cols )
    { sides = RSIDES;
    } else if ( style == NAME_rows )
    { sides = BSIDES;
    } else if ( style == NAME_groups )
    { Table tab	= (Table)cell->layout_manager;
      int rown	      = valInt(cell->row) + valInt(cell->row_span) - 1;
      int coln        = valInt(cell->column) + valInt(cell->col_span) - 1;
      TableRow row    = getRowTable(tab, toInt(rown), ON);
      TableColumn col = getColumnTable(tab, toInt(coln), ON);

      if ( row->end_group == ON )
      { if ( col->end_group == ON )
	  sides = RBSIDES;
	else
	  sides = BSIDES;
      } else
      { if ( col->end_group == ON )
	  sides = RSIDES;
      }
    }
  }
  
  if ( sides != BOX_SIDES && sides != NOSIDES )
  { if ( sides & BSIDES )
    { int rmin, rmax;

      table_row_range(tab, &rmin, &rmax);
      if ( valInt(cell->row) + valInt(cell->row_span) > rmax )
	sides &= ~BSIDES;
    }
    if ( sides & RSIDES )
    { int cmin, cmax;

      table_column_range(tab, &cmin, &cmax);
      if ( valInt(cell->column) + valInt(cell->col_span) > cmax )
	sides &= ~RSIDES;
    }
  }

  switch(sides)
  { case BOX_SIDES:
      r_thickness(b);
      r_box(d.x, d.y, d.w, d.h, 0, NIL);
      break;
    case HSIDES:
      r_hline(d.x, d.y, d.w, b);
      r_hline(d.x, d.y+d.h-b, d.w, b);
      break;
    case VSIDES:
      r_vline(d.x, d.y, d.h, b);
      r_vline(d.x+d.w-b, d.y, d.h, b);
      break;
    case RSIDES:
    { int rowspacing = valInt(tab->cell_spacing->h);
      int colspacing = valInt(tab->cell_spacing->w);

      r_vline(d.x+d.w+(colspacing-b)/2,
	      d.y-(rowspacing+1)/2,
	      d.h+rowspacing,
	      b);
      break;
    }
    case BSIDES:
    { int rowspacing = valInt(tab->cell_spacing->h);
      int colspacing = valInt(tab->cell_spacing->w);
      
      r_hline(d.x-colspacing/2,
	      d.y+d.h+(rowspacing-b)/2,
	      d.w+colspacing,
	      b);
      break;
    }
    case RBSIDES:
    { int rowspacing = valInt(tab->cell_spacing->h);
      int colspacing = valInt(tab->cell_spacing->w);

      r_vline(d.x+d.w+(colspacing-b)/2,
	      d.y-(rowspacing+1)/2,
	      d.h+rowspacing,
	      b);
      r_hline(d.x-colspacing/2,
	      d.y+d.h+(rowspacing-b)/2,
	      d.w+colspacing,
	      b);
      break;
    }
  }
}


static status 
RedrawRulesTable(Table tab, Area a)
{ int b = valInt(tab->border);

  r_dash(NAME_none);
  r_thickness(b);

  for_displayed_cells_table(tab, cell,
			    RedrawRulesTableCell(cell, tab->rules, b), ;);

  succeed;
}


static status
RedrawAreaTable(Table tab, Area a)
{ if ( overlapArea(a, tab->area) )
  { RedrawFrameTable(tab, a);
    RedrawRulesTable(tab, a);
  }

  succeed;
}
		      
		 /*******************************
		 *	    ATTRIBUTES		*
		 *******************************/

static status
assignTable(Table tab, Name slot, Any value, int compute)
{ Class class = classOfObject(tab);
  Variable var;

  if ( (var = getInstanceVariableClass(class, (Any) slot)) )
  { if ( getGetVariable(var, tab) != value )
    { setSlotInstance(tab, var, value);
      changedTable(tab);
      if ( compute )
	requestComputeLayoutManager((LayoutManager)tab, DEFAULT);
    }

    succeed;
  }

  fail;
}


static status
borderTable(Table tab, Int border)
{ return assignTable(tab, NAME_border, border, TRUE);
}


static status
frameTable(Table tab, Name frame)
{ return assignTable(tab, NAME_frame, frame, TRUE);
}


static status
rulesTable(Table tab, Name rules)
{ return assignTable(tab, NAME_rules, rules, FALSE);
}


static status
widthTable(Table tab, Int width)
{ return assignTable(tab, NAME_width, width, TRUE);
}


static status
cellPaddingTable(Table tab, Any padding)
{ if ( isInteger(padding) )
    padding = answerObject(ClassSize, padding, padding, EAV);

  return assignTable(tab, NAME_cellPadding, padding, TRUE);
}


static status
cellSpacingTable(Table tab, Any spacing)
{ if ( isInteger(spacing) )
    spacing = answerObject(ClassSize, spacing, spacing, EAV);

  return assignTable(tab, NAME_cellSpacing, spacing, TRUE);
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_append[] =
	{ "cell=table_cell", "x=[int]", "y=[int]" };
static char *T_xy[] =
	{ "x=int|name", "y=int|name" };
static char *T_row[] =
	{ "y=int|name", "create=[bool]" };
static char *T_col[] =
	{ "x=int|name", "create=[bool]" };
static char *T_sort[] =
	{ "compare=code", "from=[int]", "to=[int]" };
static char *T_deleteRows[] =
	{ "from=[int]", "to=[int]", "keep=[bool]" };
static char *T_insert_column[] =
	{ "at=int", "new=table_column" };
static char T_frame[] = "{void,above,below,hsides,vsides,box}";
static char T_rules[] = "{none,groups,rows,cols,all}";
static char *T_stretchedColumn[] =
	{ "column=table_column", "width=int" };
static char *T_stretchedRow[] =
	{ "column=table_row", "height=int" };
static char *T_resizeSlice[] =
	{ "slice=table_slice", "size=int" };
static char *T_getCell[] =
	{ "at=point|event", "allow_border=[bool]" };
static char *T_delete[] =
	{ "what=table_cell|table_row|table_column",
	  "keep=[bool]"
	};
static char *T_insertRow[] =
	{ "at=int", "row=[table_row]" };

/* Instance Variables */

static vardecl var_table[] =
{ IV(NAME_rows, "vector", IV_GET,
     NAME_contents, "Vector holding the rows"),
  IV(NAME_columns, "vector", IV_GET,
     NAME_contents, "Vector holding column info"),
  SV(NAME_border, "0..", IV_GET|IV_STORE, borderTable,
     NAME_appearance, "Thickness of border-lines"),
  SV(NAME_frame, T_frame, IV_GET|IV_STORE, frameTable,
     NAME_appearance, "Parts of the frame painted"),
  SV(NAME_rules, T_rules, IV_GET|IV_STORE, rulesTable,
     NAME_appearance, "Rules painted"),
  IV(NAME_cellPadding, "size", IV_GET,
     NAME_appearance, "Area around contents of the cell"),
  IV(NAME_cellSpacing, "size", IV_GET,
     NAME_appearance, "Space between the cells"),
  IV(NAME_current, "point", IV_BOTH,
     NAME_caret, "Current inserting/append location"),
  SV(NAME_width, "[0..]", IV_GET|IV_STORE, widthTable,
     NAME_appearance, "Total width of the table"),
  IV(NAME_area, "area", IV_GET,
     NAME_internal, "Occupied area"),
  IV(NAME_changed, "bool", IV_NONE,
     NAME_internal, "A change has been made affecting the layout")
};

  
/* Send Methods */

static senddecl send_table[] =
{ SM(NAME_initialise, 0, NULL, initialiseTable,
     DEFAULT, "Create a table"),

					/* building */
  SM(NAME_append, 3, T_append, appendTable,
     NAME_cell, "Append a cell"),
  SM(NAME_nextRow, 1, "end_group=[bool]", nextRowTable,
     NAME_cell, "Start next row (group)"),
  SM(NAME_insertRow, 2, T_insertRow, insertRowTable,
     NAME_cell, "Insert row at the specified index"),
  SM(NAME_insertColumn, 2, T_insert_column, insertColumnTable,
     NAME_cell, "Insert new column at the specified index"),
  SM(NAME_delete, 2, T_delete, deleteTable,
     NAME_cell, "Delete a cell, row or column"),
  SM(NAME_deleteRows, 3, T_deleteRows, deleteRowsTable,
     NAME_cell, "Delete a all rows"),
  SM(NAME_sortRows, 3, T_sort, sortRowsTable,
     NAME_order, "Sort rows in indicated range"),
					/* attributes */
  SM(NAME_cellPadding, 1, "int|size", cellPaddingTable,
     DEFAULT, NULL),
  SM(NAME_cellSpacing, 1, "int|size", cellSpacingTable,
     DEFAULT, NULL),
					/* layout */
  SM(NAME_compute, 0, NULL, computeTable,
     NAME_compute, "Compute column and row dimensions"),
  SM(NAME_computeBoundingBox, 0, NULL, computeBoundingBoxTable,
     NAME_compute, "Compute the bounding box"),
  SM(NAME_placeCells, 0, NULL, placeCellsTable,
     NAME_compute, "Place all cells"),
  SM(NAME_RedrawArea, 1, "area", RedrawAreaTable,
     NAME_redraw, "Draw lines"),

  SM(NAME_selection, 1, "table_cell|chain*", selectionTable,
     NAME_selection, "Specify the selection"),

  SM(NAME_stretchedColumn, 2, T_stretchedColumn, stretchedSliceTable,
     NAME_compute, "Column has been stretched to this width"),
  SM(NAME_stretchedRow, 2, T_stretchedRow, stretchedSliceTable,
     NAME_compute, "Row has been stretched to this width"),

  SM(NAME_userResizeSlice, 2, T_resizeSlice, userResizeSliceTable,
     NAME_event, "User request to resize row/column")
};

/* Get Methods */

static getdecl get_table[] =
{ GM(NAME_column,  2, "table_column", T_col, getColumnTable,
     NAME_contents, "Column at specified X"),
  GM(NAME_row,  2, "table_row", T_row, getRowTable,
     NAME_contents, "Row at specified Y"),
  GM(NAME_cell, 2, "table_cell", T_xy, getCellTable,
     NAME_contents, "Contents at specified location"),

  GM(NAME_selection, 0, "chain", NULL, getSelectionTable,
     NAME_selection, "Chain holding selected cells"),
  GM(NAME_cellFromPosition, 2, "table_cell|point", T_getCell,
     getCellFromPositionTable,
     NAME_find, "Translate coordinate to cell or point"),
  GM(NAME_cellsInRegion, 1, "chain", "area",
     getCellsInRegionTable,
     NAME_find, "Find all cells in a row/column region"),

  GM(NAME_columnRange, 0, "tuple", NULL, getColumnRangeTable,
     NAME_dimension, "Lowest/Highest defined column index"),
  GM(NAME_rowRange, 0, "tuple", NULL, getRowRangeTable,
     NAME_dimension, "Lowest/Highest defined row index")
};

/* Resources */

static classvardecl rc_table[] =
{ RC(NAME_border, "0..", "1",
     "Default thickness of lines around cells"),
  RC(NAME_frame, T_frame, "void",
     "Parts of the frame painted"),
  RC(NAME_rules, T_rules, "none",
     "Rules painted"),
  RC(NAME_cellPadding, "size", "size(5,2)",
     "Default space around cell contents"),
  RC(NAME_cellSpacing, "size", "size(1,1)",
     "Distance between the cells")
};

/* Class Declaration */

/*
static Name table_termnames[] = { NAME_halign };
*/

ClassDecl(table_decls,
          var_table, send_table, get_table, rc_table,
          0, NULL,
          "$Rev$");

status
makeClassTable(Class class)
{ return declareClass(class, &table_decls);
}

