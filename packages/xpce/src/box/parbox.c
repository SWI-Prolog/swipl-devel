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

  assign(pb, alignment, alignment);
  assign(pb->area, w, width);

  succeed;
}

		 /*******************************
		 *	      CONTENT		*
		 *******************************/

static status
appendParBox(ParBox pb, HBox hb)
{ appendChain(pb->content, hb);

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
{ int w = valInt(pb->area->w);
  int y = 0;
  device_draw_context ctx;
  parline l;

  if ( EnterRedrawAreaDevice((Device)pb, a, &ctx) )
  { Cell cell;
    int ay = valInt(a->y);		/* start of redraw area */
    int zy = ay + valInt(a->h);		/* end of it */

					/* DEBUGGING */
    r_fill(0, 0, valInt(pb->area->w), valInt(pb->area->h),
	  newObject(ClassColour, CtoName("light_blue"), 0));

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


static Cell
fill_line(Cell cell, parline *line)
{ int cx = line->x;
  int ex = cx + line->w;
  int n  = 0;				/* # hboxes */
  int ascent = 0;
  int descent = 0;
  int graphicals = 0;
  int rlevel = 0;

  int  last_break_w =	       0;
  int  last_break =	       0;
  Cell last_break_cell =       cell; 
  int  last_break_ascent =     0;
  int  last_break_descent =    0;
  int  last_break_graphicals = 0;
  int  last_break_rlevel =     0;

  for( ; cx < ex && notNil(cell); cell = cell->next )
  { HBox hb = cell->value;
    int  bw = valInt(hb->width);

    if ( last_break > 0 && cx+bw > ex )
      break;

    if ( notNil(hb->rubber) )
    { if ( notNil(hb->rubber->linebreak) )
      { last_break	      = n;
	last_break_cell	      = cell;
	last_break_ascent     = ascent;
	last_break_descent    = descent;
	last_break_w          = cx;
	last_break_graphicals = graphicals;
	last_break_rlevel     = rlevel;
  
	if ( hb->rubber->linebreak == NAME_force )
	  break;
      }

      if ( valInt(hb->rubber->level) > rlevel )
      { rlevel = valInt(hb->rubber->level);
      }
    }

    if ( instanceOfObject(hb, ClassGrBox) )
      graphicals++;

    if ( n < line->size )
    { line->hbox[n].box = hb;
      line->hbox[n].x   = cx;
      line->hbox[n].w   = bw;
    }

    cx += bw;
    n++;

    ascent  = max(ascent,  valInt(hb->ascent));
    descent = max(descent, valInt(hb->descent));
  }

  if ( notNil(cell) )			/* we use a break */
  { cell	     = last_break_cell->next;
    line->ascent     = last_break_ascent;
    line->descent    = last_break_descent;
    line->nat_width  = last_break_w;
    line->size       = last_break;
    line->graphicals = last_break_graphicals;
    line->rlevel     = last_break_rlevel;
    line->end_of_par = FALSE;
  } else
  { line->nat_width  = cx;
    line->ascent     = ascent;
    line->descent    = descent;
    line->size       = n;
    line->graphicals = graphicals;
    line->rlevel     = rlevel;
    line->end_of_par = TRUE;
  }

  if ( line->nat_width > line->w )
  { DEBUG(NAME_parbox,
	  Cprintf("Overful hbox (aw = %d, w = %d)\n",
		  line->nat_width, line->w));
    line->w = line->nat_width;
  }

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

  if ( gr->device != (Device) pb )
    send(pb, NAME_display, gr, 0);

  succeed;
}


static status
computeParBox(ParBox pb)
{ if ( notNil(pb->request_compute) )
  { int w = valInt(pb->area->w);
    int y = 0;
    int mw = w;
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
      mw = max(mw, l.w);		/* things that don't fit */
    }
    
    if ( toInt(y) != pb->area->h || mw != w )
    { Int od[4];

      od[0] = pb->area->x;
      od[1] = pb->area->y;
      od[2] = pb->area->w;
      od[3] = pb->area->h;

      CHANGING_GRAPHICAL(pb,
      { assign(pb->area, h, toInt(y));
	assign(pb->area, w, toInt(mw));
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

static int
geometryParBox(ParBox pb, Int x, Int y, Int w, Int h)
{ if ( notDefault(w) && w != pb->area->w )
  { CHANGING_GRAPHICAL(pb,
		       assign(pb->area, w, w);
		       assign(pb, request_compute, DEFAULT);
		       computeParBox(pb));
  }

  return geometryDevice((Device)pb, x, y, DEFAULT, DEFAULT);
}


static status
alignmentParBox(ParBox pb, Name alignment)
{ return assignGraphical(pb, NAME_alignment, alignment);
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
{ IV(NAME_content, "chain", IV_GET, 
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
{ RC(NAME_alignment, NULL, "left", "Default alignment of text"),
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


