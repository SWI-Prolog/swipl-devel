/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1999 University of Amsterdam. All rights reserved.
*/


#include "boxes.h"

		/********************************
		*            CREATE		*
		********************************/

static status
initialiseParBox(ParBox pb, Int width, Name alignment, Chain content)
{ initialiseDevice((Device) pb);

  if ( isDefault(width) )
    width = getClassVariableValueObject(pb, NAME_lineWidth);
  if ( isDefault(alignment) )
    alignment = getClassVariableValueObject(pb, NAME_alignment);

  if ( isDefault(content) )
    assign(pb, content, newObject(ClassChain, 0));
  else
  { Cell cell;

    for_cell(cell, content)
    { if ( !instanceOfObject(cell->value, ClassHBox) )
	return errorPce(cell->value, NAME_unexpectedType,
			nameToType(NAME_hbox));
    }

    assign(pb, content, content);
  }

  assign(pb, alignment,  alignment);
  assign(pb, line_width, width);

  succeed;
}

		 /*******************************
		 *	      CONTENT		*
		 *******************************/

static status
appendParBox(ParBox pb, HBox hb)
{ appendChain(pb->content, hb);

  if ( instanceOfObject(hb, ClassGrBox) )
  { GrBox grb = (GrBox) hb;

    deviceGraphical(grb->graphical, (Device)pb);
    DisplayedGraphical(grb->graphical, ON);
  }

  return requestComputeGraphical(pb, DEFAULT);
}

		 /*******************************
		 *	  LOW-LEVEL DATA	*
		 *******************************/

#define MAXHBOXES 512			/* max per line */

typedef struct _parcell
{ HBox		box;			/* box displayed here */
  int		x;			/* Relative X-position */
  int		w;			/* Width (stretch!) */
} parcell;


typedef struct _parline
{ int		x;			/* X, relative to device */
  int		y;			/* Y, relative to device */
  int		w;			/* Total width of the line */
  int		minx;			/* left side */
  int		nat_width;		/* Natural width */
  int		ascent;			/* Total ascent of the line */
  int		descent;		/* Total descent of the line */
  int		size;			/* # hboxes contained */
  int		graphicals;		/* # graphicals on line */
  int		end_of_par;		/* Last line?  */
  int		rlevel;			/* Highest rubber-level */
  parcell	hbox[MAXHBOXES];	/* array of cells */
} parline;

static Cell	fill_line(Cell cell, parline *line);
static void	justify_line(parline *line, Name alignment);


		/********************************
		*             REDRAW		*
		********************************/

static void
drawHBox(HBox hb, int x, int y, int w)
{ if ( instanceOfObject(hb, ClassTBox) )
  { drawTBox((TBox)hb, x, y, w);
  }
}


static status
RedrawAreaParBox(ParBox pb, Area a)
{ int w = valInt(pb->line_width);
  int y = 0;
  device_draw_context ctx;
  parline l;

  if ( EnterRedrawAreaDevice((Device)pb, a, &ctx) )
  { Cell cell;
    int ay = valInt(a->y);		/* start of redraw area */
    int zy = ay + valInt(a->h);		/* end of it */

					/* DEBUGGING */
    DEBUG(NAME_parbox,
	  r_fill(0, 0, valInt(pb->area->w), valInt(pb->area->h),
		 newObject(ClassColour, CtoName("light_blue"), 0)));

    for_cell(cell, pb->graphicals)
    { Graphical gr = cell->value;

      if ( gr->displayed == ON && overlapArea(a, gr->area) )
	RedrawArea(gr, a);
    }


    cell = pb->content->head;
    while(notNil(cell) && y < zy)
    { parcell *pc;
      int i;
  
      l.x = 0;
      l.w = w;
      l.size = MAXHBOXES;
      cell = fill_line(cell, &l);

      if ( y+l.ascent+l.descent < valInt(a->y) )
      { y += l.ascent+l.descent;	/* above display */
	continue;
      }
      if ( l.size > MAXHBOXES )	/* prevent crash on overflow */
	l.size = MAXHBOXES;
      justify_line(&l, pb->alignment);
      
      y += l.ascent;			/* the baseline */
      
      for(i=0, pc = l.hbox; i<l.size; i++, pc++)
	drawHBox(pc->box, pc->x, y, pc->w);
  
      y += l.descent;
    }

    ExitRedrawAreaDevice((Device)pb, a, &ctx);
  }

  return RedrawAreaGraphical(pb, a);
}


		 /*******************************
		 *	     COMPUTE		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Fill a line, start using hboxes from the given cell. Returns cell of the
next hbox or NIL  if  the  contents   is  complete.  Flags  is  used for
refinement of the behaviour.

At entry, x and w must be  filled.   size  should be set to to allocated
size. If size is too small, the other fields are filled nevertheless, so
this can be used for dimension testing.

y is left untouched, the other fields are filled by this routine.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
justify_line(parline *line, Name alignment)
{ int i;
  parcell *c;

  if ( line->end_of_par && alignment == NAME_justify )
    alignment = NAME_left;
  if ( line->rlevel >= 3 )		/* line contains hfill */
    alignment = NAME_justify;

  if ( alignment == NAME_right )
  { int shift = line->w - line->nat_width;

    for( i=0, c = line->hbox; i++ < line->size; c++ )
      c->x += shift;
  } else if ( alignment == NAME_center )
  { int shift = (line->w - line->nat_width)/2;

    for( i=0, c = line->hbox; i++ < line->size; c++ )
      c->x += shift;
  } else if ( alignment == NAME_justify )
  { stretch *stretches = alloca(sizeof(stretch) * line->size);
    stretch *sp = stretches;
    int dw = line->w - line->nat_width;		/* width to distribute */
    int cx = line->x;

    for( i=0, c = line->hbox; i++ < line->size; c++ )
    { HBox hb = c->box;

      if ( notNil(hb->rubber) && valInt(hb->rubber->level) == line->rlevel )
      { sp->ideal   = valInt(hb->width);
	sp->stretch = valInt(hb->rubber->stretch);
	sp->shrink  = valInt(hb->rubber->shrink);

	dw += sp->ideal;
	sp++;
      }
    }

    distribute_stretches(stretches, sp-stretches, dw);
    sp = stretches;

    for( i=0, c = line->hbox; i++ < line->size; c++ )
    { HBox hb = c->box;

      if ( notNil(hb->rubber) && valInt(hb->rubber->level) == line->rlevel )
	c->w = sp++->size;
      
      c->x = cx;
      cx += c->w;
    }
  } 
}


static void
compute_line(parline *line)
{ parcell *pc	 = line->hbox;
  parcell *epc	 = &pc[line->size];
  int cx	 = line->x;
  int ascent	 = 0;
  int descent	 = 0;
  int graphicals = 0;
  int rlevel	 = 0;
  int minx	 = cx;
  int maxx	 = cx;

  for( pc = line->hbox; pc < epc; pc++ )
  { HBox hb = pc->box;
    int  bw = valInt(hb->width);

    ascent  = max(ascent, valInt(hb->ascent));
    descent = max(descent, valInt(hb->descent));
    pc->x = cx;
    pc->w = bw;
    cx += bw;
    minx = min(minx, cx);
    maxx = max(maxx, cx);
    if ( instanceOfObject(hb, ClassGrBox) )
      graphicals++;
    if ( notNil(hb->rubber) )
      rlevel = max(rlevel, valInt(hb->rubber->level));
  }

  line->ascent	   = ascent;
  line->descent	   = descent;
  line->minx	   = minx;
  line->nat_width  = maxx;
  line->graphicals = graphicals;
  line->rlevel	   = rlevel;
}


static Cell
fill_line(Cell cell, parline *line)
{ int cx = line->x;
  int ex = cx + line->w;
  int n  = 0;				/* # hboxes */
  int  last_break =	 0;
  Cell last_break_cell = cell; 

  for( ; notNil(cell); cell = cell->next )
  { HBox hb = cell->value;
    int  bw = valInt(hb->width);

    if ( notNil(hb->rubber) && notNil(hb->rubber->linebreak) )
    { if ( cx+bw > ex )
      { if ( last_break )
	{ n    = last_break;
	  cell = last_break_cell;
	}
	break;
      }
      if ( hb->rubber->linebreak == NAME_force )
	break;

      last_break      = n;
      last_break_cell = cell;
    }

    if ( n < line->size )
      line->hbox[n].box = hb;

    cx += bw;
    n++;
  }

  if ( notNil(cell) )
    cell = cell->next;

  line->size       = n;
  line->end_of_par = isNil(cell);
  compute_line(line);

  return cell;
}


static void
compute_ascent_descent_line(parline *l)
{ int ascent  = 0;
  int descent = 0;
  int i;
  parcell *pc;
  
  for(i=0, pc = l->hbox; i < l->size; i++, pc++)
  { HBox hb = pc->box;

    ascent  = max(ascent,  valInt(hb->ascent));
    descent = max(descent, valInt(hb->descent));
  }

  l->ascent  = ascent;
  l->descent = descent;
}


static status
PlaceGrBox(ParBox pb, GrBox grb, Int x, Int y, Int w)
{ Graphical gr = grb->graphical;

  DEBUG(NAME_parbox,
	Cprintf("Placing %s (grbox %s) on %s at %d,%d (width = %d)\n",
		pp(gr), pp(grb), pp(pb), valInt(x), valInt(y), valInt(w)));

  if ( gr->area->x != x || gr->area->y != y || gr->area->w != w )
  { setGraphical(gr, x, y, w, DEFAULT);
    if ( computeAscentDescentGrBox(grb) )
    { DEBUG(NAME_parbox, Cprintf("    --> Size changed\n"));
      fail;				/* modified */
    }
  }

  succeed;
}


static status
computeParBox(ParBox pb)
{ if ( notNil(pb->request_compute) )
  { int w = valInt(pb->line_width);
    int y = 0;
    int mw = w;
    int lm = 0;				/* left margin */
    int ax, aw;				/* area x/w */
    Cell cell = pb->content->head;

    computeGraphicalsDevice((Device)pb); /* TBD: BB of graphicals */

    while(notNil(cell))
    { parline l;

      l.x = 0;
      l.w = w;
      l.size = MAXHBOXES;
      cell = fill_line(cell, &l);

      if ( l.graphicals )
      { parcell *pc;
	int i;
	int maxloop = 3;

	if ( l.size > MAXHBOXES )	/* prevent crash on overflow */
	  l.size = MAXHBOXES;

	while(--maxloop >= 0)
	{ int modified = FALSE;
	  int gr = 0;

	  justify_line(&l, pb->alignment);

	  for(i=0, pc = l.hbox; i<l.size; i++, pc++)
	  { if ( instanceOfObject(pc->box, ClassGrBox) )
	    { GrBox grb = (GrBox)pc->box;

	      if ( !PlaceGrBox(pb, grb,
			       toInt(pc->x),
			       toInt(y + l.ascent - valInt(grb->ascent)),
			       toInt(pc->w)) )
		modified = TRUE;
	      if ( ++gr == l.graphicals )
		break;			/* we had them all */
	    }
	  }

          if ( modified )
	    compute_ascent_descent_line(&l);
	  else
	    break;
	}
      }

      y += l.ascent + l.descent;	/* + skip? */
      mw = max(mw, l.nat_width);	/* things that don't fit */
      lm = min(lm, l.minx);
    }
    
    ax = valInt(pb->offset->x) + lm;
    aw = mw-lm;				/* valInt(pb->offset->x) + mw - ax */

    if ( toInt(y)  != pb->area->h ||
	 toInt(aw) != pb->area->w ||
	 toInt(ax) != pb->area->x )
    { CHANGING_GRAPHICAL(pb,
      { assign(pb->area, h, toInt(y));
	assign(pb->area, w, toInt(aw));
	assign(pb->area, x, toInt(ax));
	changedEntireImageGraphical(pb);
      });
    }

    assign(pb, request_compute, NIL);
  }

  succeed;
}


		 /*******************************
		 *	     GEOMETRY		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Interpret ->width from the offset
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
requestGeometryParBox(ParBox pb, Int x, Int y, Int w, Int h)
{ Int av[4];
  int lm = valInt(pb->area->x) - valInt(pb->offset->x);

  if ( isDefault(x) )
    av[0] = x;
  else
    av[0] = toInt(valInt(x) + lm);
  if ( isDefault(w) )
    av[2] = w;
  else
    av[2] = toInt(valInt(w) - lm);
  av[1] = y;
  av[3] = h;


  return qadSendv(pb, NAME_geometry, 4, av);
}


static status
geometryParBox(ParBox pb, Int x, Int y, Int w, Int h)
{ Area a = pb->area;
  Point o = pb->offset;

  if ( isDefault(x) ) x = a->x;
  if ( isDefault(y) ) y = a->y;
  if ( isDefault(w) ) w = a->w;

  if ( x != a->x || y != a->y || w != a->w )
  { Int dx = sub(x, a->x);
    Int dy = sub(y, a->y);

    CHANGING_GRAPHICAL(pb,
		       assign(o, x, add(o->x, dx));
		       assign(o, y, add(o->y, dy));

		       assign(a, w, w);
		       assign(a, x, x);
		       assign(a, y, y);
		       assign(pb, line_width,
			      toInt(valInt(a->x)+valInt(a->w)-valInt(o->x)));
		       assign(pb, request_compute, DEFAULT);
		       computeParBox(pb));

    updateConnectionsDevice((Device) pb, sub(pb->level, ONE));
  }

  succeed;
}


static status
alignmentParBox(ParBox pb, Name alignment)
{ return assignGraphical(pb, NAME_alignment, alignment);
}


static status
lineWidthParBox(ParBox pb, Int w)
{ return assignGraphical(pb, NAME_lineWidth, w);
}



		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_initialise[] = 
	{ "width=[int]",
	  "alignment=[{left,center,right,justify}]",
	  "content=[chain]"
	};
static char *T_geometry[] =
        { "x=[int]", "y=[int]", "width=[int]", "height=[int]" };

/* Instance Variables */

static vardecl var_parbox[] =
{ SV(NAME_lineWidth, "0..", IV_GET|IV_STORE,
     lineWidthParBox, NAME_area, "Maximum width of a textline"),
  IV(NAME_content, "chain", IV_GET, 
     NAME_content, "Contained hbox objects"),
  SV(NAME_alignment, "{left,right,center,justify}", IV_GET|IV_STORE,
     alignmentParBox, NAME_layout, "Alignment of text in box")
};

/* Send Methods */

static senddecl send_parbox[] =
{ SM(NAME_initialise, 3, T_initialise, initialiseParBox,
     DEFAULT, "Create parbox from width and content"),
  SM(NAME_compute, 0, NULL, computeParBox,
     DEFAULT, "Compute heigth"),
  SM(NAME_requestGeometry, 4, T_geometry, requestGeometryParBox,
     DEFAULT, "Change parbox width"),
  SM(NAME_geometry, 4, T_geometry, geometryParBox,
     DEFAULT, "Change parbox width"),
  SM(NAME_append, 1, "hbox", appendParBox,
     NAME_content, "Append a hbox")
};

/* Get Methods */

#define get_parbox NULL
/*
static getdecl get_parbox[] =
{ 
};
*/

/* Resources */

static classvardecl rc_parbox[] =
{ RC(NAME_lineWidth, NULL, "500",  NULL),
  RC(NAME_alignment, NULL, "left", NULL)
};

/* Class Declaration */

static Name parbox_termnames[] = { NAME_width, NAME_alignment };

ClassDecl(parbox_decls,
          var_parbox, send_parbox, get_parbox, rc_parbox,
          2, parbox_termnames,
          "$Rev$");


status
makeClassParBox(Class class)
{ declareClass(class, &parbox_decls);

  setRedrawFunctionClass(class, RedrawAreaParBox);
  solidClass(class, ON);

  succeed;
}


