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

  obtainClassVariablesObject(pb);

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

  if ( notDefault(alignment) ) assign(pb, alignment,  alignment);
  if ( notDefault(width) )     assign(pb, line_width, width);

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

#define MAXHBOXES	512		/* max per line */
#define MAXPENDINGGR	10		/* aligned graphicals pending */
#define GR_SEP		5		/* distance from aligned graphical */

#define PC_GRAPHICAL	0x01		/* contains a graphics */
#define PC_ALIGNED_GR	0x02		/* left/right aligned graphical */
#define PC_PLACED	0x04		/* we already placed the graphical */
#define PC_GRMASK	(PC_GRAPHICAL|PC_ALIGNED_GR)

typedef struct _parcell
{ HBox		box;			/* box displayed here */
  int		x;			/* Relative X-position */
  int		w;			/* Width (stretch!) */
  int		flags;			/* PC_* flags */
} parcell;


typedef struct _parline
{ int		x;			/* X, relative to device */
  int		y;			/* Y, relative to device */
  int		w;			/* Total width of the line */
  int		minx;			/* left side */
  int		maxx;		/* Natural width */
  int		ascent;			/* Total ascent of the line */
  int		descent;		/* Total descent of the line */
  int		size;			/* # hboxes contained */
  int		graphicals;		/* # graphicals on line */
  int		shape_graphicals;	/* # left/right aligned graphicals */
  int		end_of_par;		/* Last line?  */
  int		rlevel;			/* Highest rubber-level */
  parcell	hbox[MAXHBOXES];	/* array of cells */
} parline;

typedef struct
{ int	start_y;
  int	end_y;
  int   x;
} shape_cell;


typedef struct
{ ParBox     parbox;			/* Box we are working on */
  int	     line_width;		/* full width of the line */
  int	     ln;			/* # in left-queue */
  int	     rn;			/* # in right queue */
  shape_cell left[MAXPENDINGGR];
  shape_cell right[MAXPENDINGGR];
} parshape;

static Cell	fill_line(Cell cell, parline *line, parshape *shape);
static void	justify_line(parline *line, Name alignment);
static void	init_shape(parshape *s, ParBox pb, int w);
static void	push_shape_graphicals(parline *l, parshape *s);
static void	PlaceAlignedGr(GrBox grb,
			       parline *line, parshape *shape, int below);


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
  parshape shape;

  init_shape(&shape, pb, w);
  DEBUG(NAME_parbox,
	{ Area a2 = pb->area;

	  r_fill(valInt(a2->x), valInt(a2->y), valInt(a2->w), valInt(a2->h),
	       newObject(ClassColour, CtoName("light_blue"), 0));
	});


  if ( EnterRedrawAreaDevice((Device)pb, a, &ctx) )
  { Cell cell;
    int ay = valInt(a->y);		/* start of redraw area */
    int zy = ay + valInt(a->h);		/* end of it */

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
      l.y = y;
      l.w = w;
      l.size = MAXHBOXES;
      cell = fill_line(cell, &l, &shape);
      if ( l.shape_graphicals )
	push_shape_graphicals(&l, &shape);

      if ( y+l.ascent+l.descent < valInt(a->y) )
      { y += l.ascent+l.descent;	/* above display */
	continue;
      }
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
		 *	LOCATIONS AND EVENTS	*
		 *******************************/

static HBox
getBoxFromEventParBox(ParBox pb, EventObj ev)
{ Int X, Y;

  if ( get_xy_event(ev, pb, OFF, &X, &Y) )
  { int ex = valInt(X);
    int ey = valInt(Y);
    int w = valInt(pb->line_width);
    int y = 0;
    parline l;
    parshape shape;
    Cell cell;

    init_shape(&shape, pb, w);

    cell = pb->content->head;
    while( notNil(cell) )
    { parcell *pc;
      int i;
  
      l.x = 0;
      l.y = y;
      l.w = w;
      l.size = MAXHBOXES;
      cell = fill_line(cell, &l, &shape);
      if ( l.shape_graphicals )
	push_shape_graphicals(&l, &shape);

      if ( y+l.ascent+l.descent < ey )
      { y += l.ascent+l.descent;	/* before event */
	continue;
      }
      justify_line(&l, pb->alignment);
      
      for(i=0, pc = l.hbox; i<l.size; i++, pc++)
      { if ( ex > pc->x && ex <= pc->x + pc->w )
	  answer(pc->box);
      }

      fail;
    }
  }

  fail;
}

		 /*******************************
		 *	 PARAGRAPH-SHAPE	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void current_margins(parshape *s, int y, int h, int *lm, int *lw)
    If we want to place a line at y of height h, compute the left-side and
    width of the line we can place there.  Assume `s' is cleaned upto y.

void clean_margins(parshape *s, int y)
    Delete any `old' margin declarations.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
init_shape(parshape *s, ParBox pb, int w)
{ s->line_width = w;
  s->parbox     = pb;
  s->ln = s->rn = 0;
}


static void
current_margins(parshape *s, int y, int *lm, int *lw)
{ int l	= 0;
  int r = s->line_width;
  int i;

  for(i=0; i<s->ln; i++)
  { if ( !(s->left[i].start_y > y) )
      l = max(l, s->left[i].x);
  }
  for(i=0; i<s->rn; i++)
  { if ( !(s->right[i].start_y > y) )
    { r = min(r, s->right[i].x);
    }
  }

  *lm = l;
  *lw = r-l;
}


static void
clean_margins(parshape *s, int y)
{ while( s->ln > 0 && s->left[0].end_y < y )
  { s->ln--;
    memmove(&s->left[0], &s->left[1], s->ln*sizeof(shape_cell));
  }
  while( s->rn > 0 && s->right[0].end_y < y )
  { s->rn--;
    memmove(&s->right[0], &s->right[1], s->rn*sizeof(shape_cell));
  }
}


static void
add_left_margin(parshape *s, int y, int h, int x)
{ int i;

  DEBUG(NAME_parbox, Cprintf("add_left_margin(%d %d %d)\n", y, h, x));
  for(i=0; i<s->ln && s->left[i].end_y < y+h; i++)
    ;
  if ( s->ln > i )
    memmove(&s->left[s->ln+1], &s->left[s->ln], (s->ln-i)*sizeof(shape_cell));
  s->left[i].start_y = y;
  s->left[i].end_y   = y+h;
  s->left[i].x       = x + GR_SEP;
  s->ln++;
}


static void
add_right_margin(parshape *s, int y, int h, int x)
{ int i;

  for(i=0; i<s->rn && s->right[i].end_y < y+h; i++)
    ;
  if ( s->rn > i )
    memmove(&s->right[s->rn+1], &s->right[s->rn],
	    (s->rn-i)*sizeof(shape_cell));
  s->right[i].start_y = y;
  s->right[i].end_y   = y+h;
  s->right[i].x       = x - GR_SEP;
  s->rn++;
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
  { int shift = line->w - line->maxx;

    for( i=0, c = line->hbox; i++ < line->size; c++ )
      c->x += shift;
  } else if ( alignment == NAME_center )
  { int shift = (line->w - line->maxx)/2;

    for( i=0, c = line->hbox; i++ < line->size; c++ )
      c->x += shift;
  } else if ( alignment == NAME_justify )
  { stretch *stretches = alloca(sizeof(stretch) * line->size);
    stretch *sp = stretches;
    int dw = line->x + line->w - line->maxx; /* width to distribute */
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
      if ( !(c->flags & PC_ALIGNED_GR) )
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
  int rlevel	 = 0;
  int minx	 = cx;
  int maxx	 = cx;

  line->graphicals = 0;

  for( pc = line->hbox; pc < epc; pc++ )
  { HBox hb = pc->box;

    pc->x = cx;

    if ( !(pc->flags & PC_ALIGNED_GR) )
    { ascent  = max(ascent,  valInt(hb->ascent));
      descent = max(descent, valInt(hb->descent));
      cx += pc->w;
      minx = min(minx, cx);
      maxx = max(maxx, cx);
      if ( notNil(hb->rubber) )
	rlevel = max(rlevel, valInt(hb->rubber->level));
    }

    if ( pc->flags & PC_GRAPHICAL )
    { if ( pc->flags & PC_ALIGNED_GR )
	line->shape_graphicals++;
      else
	line->graphicals++;
    }
  }

  line->ascent	   = ascent;
  line->descent	   = descent;
  line->minx	   = minx;
  line->maxx  = maxx;
  line->rlevel	   = rlevel;
}


static Cell
fill_line(Cell cell, parline *line, parshape *shape)
{ int cx, ex;
  Cell last_break_cell = cell; 
  parcell *last_break = NULL;
  parcell *pc = line->hbox, *epc = pc+line->size;
  int blank = TRUE;			/* only emitted blank space */

  clean_margins(shape, line->y);
  current_margins(shape, line->y, &line->x, &line->w);
  cx = line->x;
  ex = cx + line->w;

  for( ; notNil(cell) && pc < epc; cell = cell->next, pc++ )
  { HBox hb = cell->value;
    int  bw = valInt(hb->width);

    if ( notNil(hb->rubber) && notNil(hb->rubber->linebreak) )
    { if ( cx+bw > ex )
      { if ( last_break )
	{ pc   = last_break;
	  cell = last_break_cell;
	}
	line->end_of_par = FALSE;
	break;
      }
      if ( hb->rubber->linebreak == NAME_force )
      { line->end_of_par = TRUE;
	break;
      }

      last_break      = pc;
      last_break_cell = cell;
    }

    pc->box   = hb;
    pc->w     = bw;
    pc->flags = 0;

    if ( instanceOfObject(hb, ClassGrBox) )
    { GrBox grb = (GrBox)hb;

      if ( notNil(grb->graphical->request_compute) )
      { ComputeGraphical(grb->graphical);
	computeGrBox(grb);
      }

      pc->flags |= PC_GRAPHICAL;
      if ( notNil(grb->alignment) )
      { pc->flags |= PC_ALIGNED_GR;

	if ( blank )
	{ int lx;

	  pc->flags |= PC_PLACED;
	  PlaceAlignedGr(grb, line, shape, FALSE);
	  current_margins(shape, line->y, &lx, &line->w);
	  cx += lx - line->x;
	  ex = cx + line->w;
	  DEBUG(NAME_parbox,
		Cprintf("Placed %s; line %d to %d\n",
			pp(grb->graphical), cx, ex));
	  line->x = lx;
	}
      }
    }

    if ( !(pc->flags & PC_ALIGNED_GR) )
    { if ( !(hb->width == ZERO ||
	     (hb->ascent == ZERO && hb->descent == ZERO)) )
	blank = FALSE;
      cx += pc->w;
    }
  }

  if ( notNil(cell) )
    cell = cell->next;
  else
    line->end_of_par = TRUE;

  line->size       = pc-line->hbox;
  compute_line(line);

  return cell;
}


#if 0
static Cell
place_graphicals(Cell cell, parline *line, parshape *shape)
{ int cx = line->x;
  int cy = line->y;

  for(; notNil(cell); cell = cell->next)
  { if ( instanceOfObject(cell->value, ClassGrBox) )
    { GrBox grb = cell->value;
      
      if ( isNil(grb->alignment) )
      { return cell;
      } else
      { PlaceAlignedGr(grb, line, shape, FALSE);
      }
    } else if ( classOfObject(cell->value) == ClassHBox )
    { HBox hb = cell->value;

      cx += valInt(hb->width);
      cy += valInt(hb->ascent);
    } else
      return cell;
  }
}
#endif


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
push_shape_graphicals()
    Used by RedrawAreaParBox() to add already placed graphicals to the
    margin-shape.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
push_shape_graphicals(parline *l, parshape *s)
{ parcell *pc = l->hbox, *epc = pc+l->size;

  for( ; pc < epc; pc++ )
  { if ( (pc->flags & PC_ALIGNED_GR) && !(pc->flags & PC_PLACED) )
    { GrBox grb = (GrBox)pc->box;
      int h = valInt(grb->ascent)+valInt(grb->descent);
      int y = l->y + l->ascent + l->descent;
      int grw = valInt(grb->width);

      if ( grb->alignment == NAME_left )
      { add_left_margin(s, y, h, grw);
      } else
      { int grx = l->w-pc->w;

	add_right_margin(s, y, h, grx);
      }
      if ( --l->shape_graphicals <= 0 )
	break;
    }
  }
}


static void				/* debugging help */
print_line(parline *l)
{ parcell *pc = l->hbox, *epc = pc+l->size;

  for(; pc < epc; pc++)
  { if ( instanceOfObject(pc->box, ClassTBox) )
    { TBox tb = (TBox) pc->box;

      Cprintf("[%s] ", strName(tb->text));
    } else if ( instanceOfObject(pc->box, ClassGrBox) )
    { GrBox grb = (GrBox) pc->box;
      Cprintf("%s ", pp(grb->graphical));
    } else
    { Cprintf("|%d+%d-%d|",
	      valInt(pc->box->width),
	      valInt(pc->box->ascent),
	      valInt(pc->box->descent));
    }
  }

  Cprintf("\n");
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


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Place left/right aligned graphical.  If `below' is TRUE, place it below the
current line.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
PlaceAlignedGr(GrBox grb, parline *line, parshape *shape, int below)
{ Int grw = grb->width;
  int y = line->y;

  if ( below )
    y += line->ascent + line->descent;

  DEBUG(NAME_parbox, Cprintf("PLacing %s (y=%d)\n", pp(grb), y));

  if ( grb->alignment == NAME_left )
  { PlaceGrBox(shape->parbox, grb, toInt(line->x), toInt(y), grw);
    add_left_margin(shape,
		    y,
		    valInt(grb->ascent)+valInt(grb->descent),
		    valInt(grw));
  } else
  { int grx = line->x + line->w - valInt(grw);

    PlaceGrBox(shape->parbox, grb, toInt(grx), toInt(y), grw);
    add_right_margin(shape,
		     y,
		     valInt(grb->ascent)+valInt(grb->descent),
		     grx);
  } 
}


static status
computeParBox(ParBox pb)
{ if ( notNil(pb->request_compute) )
  { int w = valInt(pb->line_width);
    int y = 0;
    int mw = (pb->auto_crop == ON ? 0 : w);
    int lm = 0;				/* left margin */
    int ax, aw;				/* area x/w */
    Cell cell = pb->content->head;
    parshape shape;
    int lineno = 0;

    init_shape(&shape, pb, w);

    while(notNil(cell))
    { parline l;

      l.x = 0;
      l.y = y;
      l.w = w;
      l.size = MAXHBOXES;
      cell = fill_line(cell, &l, &shape);
      lineno++;

      DEBUG(NAME_parbox,
	    if ( l.maxx > l.x + l.w )
	    { Cprintf("%s: Overfull line %d\n", pp(pb), lineno);
	      print_line(&l);
	    });

      if ( l.graphicals )
      { parcell *pc;
	int i;
	int maxloop = 3;

	while(--maxloop >= 0)
	{ int modified = FALSE;
	  int gr = 0;

	  justify_line(&l, pb->alignment);

	  for(i=0, pc = l.hbox; i<l.size; i++, pc++)
	  { if ( (pc->flags & PC_GRMASK) == PC_GRAPHICAL ) 
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
      mw = max(mw, l.maxx);		/* things that don't fit */
      lm = min(lm, l.minx);

      if ( l.shape_graphicals )
      { parcell *pc = l.hbox, *epc = pc+l.size;

	for( ; pc < epc; pc++ )
	{ if ( (pc->flags & PC_ALIGNED_GR) && !(pc->flags & PC_PLACED) )
	  { GrBox grb = (GrBox)pc->box;

	    PlaceAlignedGr(grb, &l, &shape, TRUE);
	  }
	}
      }
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

static status
autoCropParBox(ParBox pb, Bool crop)
{ return assignGraphical(pb, NAME_autoCrop, crop);
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
     alignmentParBox, NAME_layout, "Alignment of text in box"),
  SV(NAME_autoCrop, "bool", IV_GET|IV_STORE,
     autoCropParBox, NAME_layout, "If @on, make <-area fit content")
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

static getdecl get_parbox[] =
{ GM(NAME_boxFromEvent, 1, "hbox", "event", getBoxFromEventParBox,
     NAME_event, "Find hbox from event")
};


/* Resources */

static classvardecl rc_parbox[] =
{ RC(NAME_lineWidth, NULL, "500",  NULL),
  RC(NAME_alignment, NULL, "left", NULL),
  RC(NAME_autoCrop,  NULL, "@off", NULL)
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


