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

#include <h/kernel.h>
#include <h/graphics.h>

static status	layoutTile(TileObj t, Int ax, Int ay, Int aw, Int ah);
static status	computeTile(TileObj t);

#define Max(a, b)	(valInt(a) > valInt(b) ? (a) : (b))
#define Min(a, b)	(valInt(a) < valInt(b) ? (a) : (b))
#define MAX_TILE_MEMBERS 200
#define INFINITE toInt(PCE_MAX_INT)


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A tile manages a set of  non-overlapping areas.  It  is used to manage
the areas of the windows in a frame.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

		/********************************
		*         CREATE/DESTROY	*
		********************************/

static status
initialiseTile(TileObj t, Any object, Int w, Int h)
{ if ( notNil(object) )
  { if ( isDefault(w) )
      w = get(object, NAME_width, EAV);
    if ( isDefault(h) )
      h = get(object, NAME_height, EAV);
  } 

  assign(t, enforced,	 OFF);		/* building stage */
  assign(t, idealWidth,  w);		/* ideal size */
  assign(t, idealHeight, h);
  assign(t, horStretch,  toInt(100));	/* stretchabilities */
  assign(t, horShrink,   toInt(100));
  assign(t, verStretch,  toInt(100));
  assign(t, verShrink,   toInt(100));
  assign(t, canResize,   DEFAULT);
  assign(t, orientation, NAME_none);
  assign(t, members, NIL);		/* subtiles */
  assign(t, super,   NIL);		/* super-tile */
  assign(t, object,  object);		/* managed object */
					/* Actual area */
  assign(t, area, newObject(ClassArea, ZERO, ZERO, w, h, EAV));

  return obtainClassVariablesObject(t);
}


static status
unlinkTile(TileObj t)
{ if ( notNil(t->members) )
  { clearChain(t->members);
    assign(t, members, NIL);
  }

  succeed;
}

		/********************************
		*         UNRELATE TILE		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A member of this tile has been deleted.  If there is only one member
left, remove this tile for the hierarchy.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static status
cleanTile(TileObj t)
{ if ( t->members->size == ONE )
  { TileObj child = getHeadChain(t->members);
      
    if ( notNil(t->super) )
    { TileObj super = t->super;

      replaceChain(super->members, t, child);
      assign(child, super, super);
    } else
    { assign(child, super, NIL);
      freeObject(t);
    }

    computeTile(getRootTile(child));
  } else
    computeTile(t);

  succeed;
}


status
unrelateTile(TileObj t)
{ if ( notNil(t->super) )
  { TileObj super = t->super;

    deleteChain(t->super->members, t);
    assign(t, super, NIL);
    cleanTile(super);
  }

  succeed;
}

		/********************************
		*    CREATING TILE HIERARCHY	*
		********************************/

static TileObj
toTile(Any obj)
{ if ( instanceOfObject(obj, ClassTile) )
    return (TileObj) obj;

  return answerObject(ClassTile, obj, EAV);
}


static status
computeTile(TileObj t)
{ Int w   = ZERO;
  Int h   = ZERO;
  Int vsh=ZERO, vst=ZERO, hsh=ZERO, hst=ZERO;

  DEBUG(NAME_tile, Cprintf("computeTile(%s) --> ", pp(t)));

  if ( t->orientation == NAME_horizontal )
  { Cell cell;

    hsh = ZERO;
    hst = ZERO;
    vsh = INFINITE;
    vst = INFINITE;
    
    for_cell(cell, t->members)
    { TileObj t2 = cell->value;
      
      w = add(w, t2->idealWidth);
      h = Max(h, t2->idealHeight);
      hsh = Max(hsh, t2->horShrink);
      hst = Max(hst, t2->horStretch);
      vsh = Min(vsh, t2->verShrink);
      vst = Min(vst, t2->verStretch);
      w = add(w, t->border);
    }
    
    assign(t, idealWidth,  w);
    assign(t, horShrink,   hsh);
    assign(t, horStretch,  hst);
    assign(t, idealHeight, h);
    assign(t, verShrink,   vsh);
    assign(t, verStretch,  vst);
  } else if ( t->orientation == NAME_vertical )
  { Cell cell;

    vsh = ZERO;
    vst = ZERO;
    hsh = INFINITE;
    hst = INFINITE;
    
    for_cell(cell, t->members)
    { TileObj t2 = cell->value;
      
      w = Max(w, t2->idealWidth);
      h = add(h, t2->idealHeight);
      hsh = Min(hsh, t2->horShrink);
      hst = Min(hst, t2->horStretch);
      vsh = Max(vsh, t2->verShrink);
      vst = Max(vst, t2->verStretch);
      h = add(h, t->border);
    }
    
    assign(t, idealWidth,  w);
    assign(t, horShrink,   hsh);
    assign(t, horStretch,  hst);
    assign(t, idealHeight, h);
    assign(t, verShrink,   vsh);
    assign(t, verStretch,  vst);
  }
  
  DEBUG(NAME_tile,
	if ( t->orientation == NAME_horizontal ||
	     t->orientation == NAME_vertical )
	  Cprintf("%s, %dx%d, -%dx+%d, -%dy+%d\n",
		  pp(t->orientation),
		  valInt(w), valInt(h),
		  valInt(hsh), valInt(hst),
		  valInt(vsh), valInt(vst));
	else
	  Cprintf("\n"));

  succeed;
}


static status
nonDelegatingLeftRightTile(TileObj t, TileObj t2, Name where)
{ TileObj super;

  t = getRootTile(t);

  if ( notNil(t2->super) && t2->super->orientation == NAME_horizontal )
  { super = t2->super;

    if ( where == NAME_right )
      insertAfterChain(super->members, t, t2);
    else
      insertBeforeChain(super->members, t, t2);

    assign(t, super, super);
  } else
  { Chain ch;

    super = newObject(ClassTile, NIL, ZERO, ZERO, EAV);
    
    if ( where == NAME_right )
      ch = newObject(ClassChain, t2, t, EAV);
    else
      ch = newObject(ClassChain, t, t2, EAV);

    assign(super, orientation, NAME_horizontal);
    assign(super, members, ch);
    assign(super->area, x, t->area->x);
    assign(super->area, y, t->area->y);
    if ( notNil(t2->super) )
    { replaceChain(t2->super->members, t2, super);
      assign(super, super, t2->super);
    }
    assign(t2, super, super);
    assign(t,  super, super);
    assign(super, enforced, t2->enforced);
  }

  return computeTile(super);
}


static status
leftTile(TileObj t, Any obj, Bool delegate)
{ TileObj t2 = toTile(obj);
  TileObj super;

  if ( delegate == OFF )
    return nonDelegatingLeftRightTile(t, t2, NAME_left);

					/* One already has a super tile */
  if ( notNil(t->super)  &&
       (t->super->orientation  == NAME_vertical ||
	notNil(t->super->super)) )
    return leftTile(t->super, t2, ON);
  if ( notNil(t2->super) &&
       (t2->super->orientation == NAME_vertical ||
	notNil(t2->super->super)) )
    return leftTile(t, t2->super, ON);
					/* both left-to-right */
  if ( notNil(t->super) && notNil(t2->super) )
    return leftTile(t->super, t2->super, ON);

  if ( notNil(t->super) )
  { super = t->super;
    appendChain(super->members, t2);
  } else if ( notNil(t2->super) )
  { super = t2->super;
    prependChain(super->members, t);
  } else
  { super = newObject(ClassTile, NIL, ZERO, ZERO, EAV);
    assign(super, orientation, NAME_horizontal);
    assign(super, members, newObject(ClassChain, t, t2, EAV));
    assign(super->area, x, t->area->x);
    assign(super->area, y, t->area->y);
  }

  assign(t,  super, super);
  assign(t2, super, super);
  computeTile(super);

  succeed;
}


static status
rightTile(TileObj t, Any obj, Bool delegate)
{ if ( delegate == OFF )
    return nonDelegatingLeftRightTile(t, toTile(obj), NAME_right);

  return leftTile(toTile(obj), t, ON);
}

					/* can be merged with left version */
static status
nonDelegatingAboveBelowTile(TileObj t, TileObj t2, Name where)
{ TileObj super;

  t = getRootTile(t);

  if ( notNil(t2->super) && t2->super->orientation == NAME_vertical )
  { super = t2->super;

    if ( where == NAME_below )
      insertAfterChain(super->members, t, t2);
    else
      insertBeforeChain(super->members, t, t2);

    assign(t, super, super);
  } else
  { Chain ch;

    super = newObject(ClassTile, NIL, ZERO, ZERO, EAV);
    
    if ( where == NAME_below )
      ch = newObject(ClassChain, t2, t, EAV);
    else
      ch = newObject(ClassChain, t, t2, EAV);

    assign(super, orientation, NAME_vertical);
    assign(super, members, ch);
    assign(super->area, x, t->area->x);
    assign(super->area, y, t->area->y);
    if ( notNil(t2->super) )
    { replaceChain(t2->super->members, t2, super);
      assign(super, super, t2->super);
    }
    assign(t2, super, super);
    assign(t,  super, super);
    assign(super, enforced, t2->enforced);
  }

  return computeTile(super);
}


static status
aboveTile(TileObj t, Any obj, Bool delegate)
{ TileObj t2 = toTile(obj);
  TileObj super;

  if ( delegate == OFF )
    return nonDelegatingAboveBelowTile(t, t2, NAME_above);

					/* One already has a super tile */
  if ( notNil(t->super)  &&
       (t->super->orientation  == NAME_horizontal ||
	notNil(t->super->super)) )
    return aboveTile(t->super, t2, ON);
  if ( notNil(t2->super) &&
       (t2->super->orientation == NAME_horizontal ||
	notNil(t2->super->super)) )
    return aboveTile(t, t2->super, ON);

  if ( notNil(t->super) && notNil(t2->super) )
    return aboveTile(t->super, t2->super, ON);

  if ( notNil(t->super) )
  { super = t->super;
    appendChain(super->members, t2);
  } else if ( notNil(t2->super) )
  { super = t2->super;
    prependChain(super->members, t);
  } else
  { super = newObject(ClassTile, NIL, ZERO, ZERO, EAV);
    assign(super, orientation, NAME_vertical);
    assign(super, members, newObject(ClassChain, t, t2, EAV));
    assign(super->area, x, t->area->x);
    assign(super->area, y, t->area->y);
  }

  assign(t,  super, super);
  assign(t2, super, super);

  computeTile(super);

  succeed;
}


static status
belowTile(TileObj t, Any obj, Bool delegate)
{ if ( delegate == OFF )
    return nonDelegatingAboveBelowTile(t, toTile(obj), NAME_below);

  return aboveTile(toTile(obj), t, ON);
}


		/********************************
		*        LAYOUT MANAGEMENT	*
		********************************/


TileObj
getRootTile(TileObj t)
{ while(notNil(t->super))
    t = t->super;

  answer(t);
}


status
distribute_stretches(stretch *s, int n, int w)
{ int ok = FALSE;
  int i;
  int maxloop = n;
  
  if ( w <= 0 )
  { for(i=0; i<n; i++)
      s[i].size = 0;

    succeed;
  }

  while( !ok && maxloop-- > 0 )
  { int total_ideal = 0, total_stretch = 0, total_shrink = 0;
    int grow, growed;
    int is_pos;
    Stretch sp;
    
    for(i = 0; i < n; i++)
    { total_ideal   += s[i].ideal;
      total_stretch += s[i].stretch;
      total_shrink  += s[i].shrink;
      DEBUG(NAME_tile, Cprintf("%-2d %-3d <- %-3d -> %-3d\n",
			       i, s[i].shrink, s[i].ideal, s[i].stretch));
    }

    grow = w - total_ideal;

    if ( grow < 0 && total_shrink == 0 )
    { for(is_pos = 0, i = 0; i < n; i++)
        if ( s[i].ideal > 0 || s[i].shrink > 0 )
	  is_pos++;
    } else
      is_pos = n;

    DEBUG(NAME_tile, Cprintf("grow = %d, is_pos = %d\n", grow, is_pos));

    for(growed = 0, i = 0; i < n; i++)
    { int grow_this;

      if ( grow >= 0 )
      { grow_this = (total_stretch==0 ? grow / n
				      : (grow * s[i].stretch) / total_stretch);
      } else
      {	if ( s[i].ideal == 0 && s[i].shrink == 0 )
	  grow_this = 0;
	else
	  grow_this = (total_shrink ==0 ? grow / is_pos
				        : (grow * s[i].shrink) / total_shrink);
      }
      s[i].size = s[i].ideal + grow_this;
      growed += grow_this;
    }

    if ( grow != growed )
    { int do_grow = (grow > 0);
      int stretchables;
      int per_stretchable;
      int stretchall;
      int m;

      DEBUG(NAME_tile, Cprintf("Show grow %d, done %d\n", grow, growed));

      if ( grow < 0 )			/* normalise */
      { grow = -grow;
	growed = -growed;
      }

      for(i=0, stretchables=0; i < n; i++)
      { if ( (do_grow ? s[i].stretch : s[i].shrink) > 0)
	  stretchables++;
      }
					/* No one wants, so all have to */
      if ( stretchables == 0 )
      { stretchables = is_pos;
	stretchall = FALSE;
      } else
      { stretchall = TRUE;
      }
					/* distribute outside --> inside */
      per_stretchable = (grow - growed + stretchables - 1) / stretchables;

      for( i=0, m=n; growed < grow && m-- > 0; i++ )
      { int j = (i%2 ? i : n - i - 1);

	if ( stretchall ||
	     (do_grow ? s[j].stretch : s[i].shrink) > 0 )
	{ int to_grow = (grow - growed < per_stretchable ? grow - growed
							 : per_stretchable);
	  
					/* don't make negative */
	  if ( !do_grow	&& to_grow > s[j].size )
	    to_grow = s[j].size;

	  s[j].size += (do_grow ? to_grow : -to_grow);
	  growed += to_grow;
	}
      }
    }
  
    ok = TRUE;
    for(sp=s, i = 0; i < n; i++, sp++)	/* Accept min and max */
    { if ( sp->size < sp->minimum )
      { sp->ideal = sp->minimum;
        sp->shrink = 0;
	DEBUG(NAME_tile, Cprintf("%d is too small; setting to %d\n",
				 i, sp->minimum));
	ok = FALSE;
      } else if ( sp->size > sp->maximum )
      { sp->ideal = sp->maximum;
	sp->stretch = 0;
	DEBUG(NAME_tile, Cprintf("%d is too large; setting to %d\n",
				 i, sp->maximum));
	ok = FALSE;
      }
    }
  }

  succeed;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Combine stretches in their `natural' direction.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
sum_stretches(stretch *sp, int len, stretch *r)
{ r->ideal   = 0;
  r->minimum = 0;
  r->maximum = 0;
  r->shrink  = 0;
  r->stretch = 0;

  for( ; len-- > 0; sp++)
  { r->shrink  = max(r->shrink, sp->shrink);
    r->stretch = max(r->stretch, sp->stretch);
    r->ideal   += sp->ideal;
    r->minimum += sp->minimum;
    if ( r->maximum < INT_MAX )
    { r->maximum += sp->maximum;
      if ( r->maximum > INT_MAX || r->maximum < 0 )
	r->maximum = INT_MAX;
    }
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Join a sequence of stretches that  have  to   be  forced  to be the same
width. Minimum and maximum are easy. The ideal is deduced by starting at
the average and then using the applicable   stretch or schrink to arrive
at a new weighted average. This process is iterated for at most 4 times.

The shrink and stretchability of the join  should be a weighted average,
where low values have more weight.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
join_stretches(stretch *stretches, int len, stretch *r)
{ stretch *sp;
  int i;
  int avg;
  int maxloop;

  r->minimum = 0;
  r->maximum = INT_MAX;

  DEBUG(NAME_stretch, Cprintf("Joining %d stretches\n", len));

  for(sp=stretches, i=len ; i-- > 0; sp++)
  { r->minimum = max(r->minimum, sp->minimum);
    r->maximum = min(r->maximum, sp->maximum);
    DEBUG(NAME_stretch, Cprintf("\t%d %d..%d <-%d ->%d\n",
				sp->ideal,
				sp->minimum, sp->maximum,
				sp->shrink, sp->stretch));
  }
    
  for(avg=0, sp=stretches, i=len; i-- > 0; sp++)
    avg += sp->ideal;
  avg /= len;

  for(maxloop = 4; maxloop-- > 0;)
  { int wavg = 0;
    int tw = 0;

    for(sp=stretches, i=len; i-- > 0; sp++)
    { int w0;

      w0 = (sp->ideal < avg ? sp->stretch : sp->shrink);

      if ( w0 != 0 )
	w0 = max(1000/w0, 1);
      else
	w0 = 100000;

      wavg += sp->ideal*w0;
      tw   += w0;
    }
    wavg = (wavg+tw/2)/tw;
    if ( wavg == avg )
      break;
    avg = wavg;
  }

  r->ideal = avg;

  { int stretchavg = 0, shrinkavg = 0;
    int twstretch = 0, twshrink = 0;

    for(sp=stretches, i=len; i-- > 0; sp++)
    { int w0;

      w0 = (sp->stretch ? max(1000/sp->stretch, 1) : 100000);
      stretchavg += sp->stretch*w0;
      twstretch += w0;
  
      w0 = (sp->shrink ? max(1000/sp->shrink, 1) : 100000);
      shrinkavg += sp->shrink*w0;
      twshrink += w0;
    }
    
    r->shrink  = (stretchavg+twstretch/2)/twstretch;
    r->stretch = (shrinkavg+twshrink/2)/twshrink;
  }

  DEBUG(NAME_stretch, Cprintf("--> %d %d..%d <-%d ->%d\n",
			      r->ideal,
			      r->minimum, r->maximum,
			      r->shrink, r->stretch));
}



status
setTile(TileObj t, Int x, Int y, Int w, Int h)
{ TileObj super;

  DEBUG(NAME_tile,
	Cprintf("setTile(%s, %s, %s, %s, %s) ",
		pp(t), pp(x), pp(y), pp(w), pp(h));
	Cprintf("enforced = %s\n", pp(t->enforced)));

  if ( notDefault(w) && valInt(w) < valInt(t->border)) w = t->border;
  if ( notDefault(h) && valInt(h) < valInt(t->border)) h = t->border;

  if ( notDefault(w) )
  { assign(t, idealWidth, w);

    if ( t->enforced == ON && notNil(t->super) )
    { Cell cell;
      int before = TRUE;
      int hs = 0, hg = 0;

      for_cell(cell, t->super->members)
      { TileObj t2 = cell->value;

	if ( before )
	{ assign(t2, horStretch, ZERO);
	  assign(t2, horShrink,  ZERO);
	  if ( t2 == t )
	    before = FALSE;
	} else
	{ hs += valInt(t2->horShrink);
	  hg += valInt(t2->horStretch);
	}
      }
      if ( hs == 0 || hg == 0 )
      { before = TRUE;

	for_cell(cell, t->super->members)
	{ TileObj t2 = cell->value;

	  if ( before )
	  { if ( t2 == t )
	      before = FALSE;
	  } else
	  { if ( hs == 0 )
	      assign(t2, horShrink, ONE);
	    if ( hg == 0 )
	      assign(t2, horStretch, ONE);
	  }
	}
      }
    }
  }
  if ( notDefault(h) )
  { assign(t, idealHeight, h);

    if ( t->enforced == ON && notNil(t->super) )
    { Cell cell;
      int before = TRUE;
      int vs = 0, vg = 0;

      for_cell(cell, t->super->members)
      { TileObj t2 = cell->value;

	if ( before )
	{ assign(t2, verStretch, ZERO);
	  assign(t2, verShrink,  ZERO);
	  if ( t2 == t )
	    before = FALSE;
	} else
	{ vs += valInt(t2->verShrink);
	  vg += valInt(t2->verStretch);
	}
      }
      if ( vs == 0 || vg == 0 )
      { before = TRUE;

	for_cell(cell, t->super->members)
	{ TileObj t2 = cell->value;

	  if ( before )
	  { if ( t2 == t )
	      before = FALSE;
	  } else
	  { if ( vs == 0 )
	      assign(t2, verShrink, ONE);
	    if ( vg == 0 )
	      assign(t2, verStretch, ONE);
	  }
	}
      }
    }
  }

  if ( t->enforced != ON )
  { for(super = t->super; notNil(super); super = super->super)
      computeTile(super);
  } else
  { if ( notNil(t->super) )
      layoutTile(getRootTile(t), DEFAULT, DEFAULT, DEFAULT, DEFAULT);
    else
      layoutTile(t, x, y, w, h);
  }

  succeed;
}


static status
unenforceTile(TileObj t)
{ assign(t, enforced, OFF);

  if ( notNil(t->members) )
  { Cell cell;

    for_cell(cell, t->members)
      unenforceTile(cell->value);
  }

  succeed;
}


status
enforceTile(TileObj t, Bool val)
{ if ( val == OFF )
  { unenforceTile(t);
    computeTile(t);
  } else
  { if ( t->enforced == OFF )
    { assign(t, enforced, ON);
    
      layoutTile(t, DEFAULT, DEFAULT, t->idealWidth, t->idealHeight);
    }
  }

  succeed;
}


static status
layoutTile(TileObj t, Int ax, Int ay, Int aw, Int ah)
{ int border = valInt(t->border);
  int borders = (isNil(t->members) ? 0 : valInt(getSizeChain(t->members))-1);
  int x, y, w, h;

  assign(t, enforced, ON);

  if ( notDefault(aw) && valInt(aw) < 0 ) aw = ZERO;
  if ( notDefault(ah) && valInt(ah) < 0 ) ah = ZERO;
 
  setArea(t->area, ax, ay, aw, ah);
  x = valInt(t->area->x);
  y = valInt(t->area->y);
  w = valInt(t->area->w);
  h = valInt(t->area->h);

  if ( isNil(t->super) )
  { x += border;
    y += border;
    w -= border*2;
    h -= border*2;
  }

  updateAdjusterPositionTile(t);

  if ( t->orientation == NAME_none )
    return send(t->object, NAME_doSet,
		toInt(x), toInt(y), toInt(w), toInt(h), EAV);

  DEBUG(NAME_tile, Cprintf("enter: layoutTile(%s) (%s)\n",
			   pp(t), pp(t->orientation)));
  if ( t->orientation == NAME_horizontal )
  { stretch s[MAX_TILE_MEMBERS];
    Stretch sp;
    Cell cell;
    
    sp = s;
    for_cell(cell, t->members)
    { TileObj t2 = cell->value;
      
      sp->minimum = 0;
      sp->maximum = INT_MAX;
      sp->ideal   = valInt(t2->idealWidth);
      sp->stretch = valInt(t2->horStretch);
      sp->shrink  = valInt(t2->horShrink);
      sp++;
    }
    
    distribute_stretches(s, sp-s, w - border*borders);
    
    sp = s;
    for_cell(cell, t->members)
    { TileObj t2 = cell->value;
      
      layoutTile(t2, toInt(x), toInt(y), toInt(sp->size), toInt(h));
      x += sp->size + border;
      sp++;
    }
  } else /*if ( t->orientation == NAME_vertical )*/
  { stretch s[MAX_TILE_MEMBERS];
    Stretch sp;
    Cell cell;
    
    sp = s;
    for_cell(cell, t->members)
    { TileObj t2 = cell->value;
      
      sp->minimum = 0;
      sp->maximum = INT_MAX;
      sp->ideal   = valInt(t2->idealHeight);
      sp->stretch = valInt(t2->verStretch);
      sp->shrink  = valInt(t2->verShrink);
      sp++;
    }
    
    distribute_stretches(s, sp-s, h - border*borders);
    
    sp = s;
    for_cell(cell, t->members)
    { TileObj t2 = cell->value;
      
      layoutTile(t2, toInt(x), toInt(y), toInt(w), toInt(sp->size));
      y += sp->size + border;
      sp++;
    }
  }
    
  DEBUG(NAME_tile, Cprintf("exit: layoutTile(%s)\n", pp(t)));

  succeed;
}


static status
areaTile(TileObj t, Area a)
{ return setTile(t, a->x, a->y, a->w, a->h);
}


static status
positionTile(TileObj t, Point p)
{ return setTile(t, p->x, p->y, DEFAULT, DEFAULT);
}


static status
sizeTile(TileObj t, Size s)
{ return setTile(t, DEFAULT, DEFAULT, s->w, s->h);
}


static status
xTile(TileObj t, Int x)
{ return setTile(t, x, DEFAULT, DEFAULT, DEFAULT);
}


static status
yTile(TileObj t, Int y)
{ return setTile(t, DEFAULT, y, DEFAULT, DEFAULT);
}


static status
widthTile(TileObj t, Int w)
{ return setTile(t, DEFAULT, DEFAULT, w, DEFAULT);
}


static status
heightTile(TileObj t, Int h)
{ return setTile(t, DEFAULT, DEFAULT, DEFAULT, h);
}


static status
cornerTile(TileObj t, Point pos)
{ return setTile(t, DEFAULT, DEFAULT, sub(pos->x, t->area->x),
		      		      sub(pos->y, t->area->y));
}


static status
centerTile(TileObj t, Point pos)
{ return setTile(t, dif(pos->x, t->area->w),
		    dif(pos->y, t->area->h),
		    DEFAULT, DEFAULT);
}


		 /*******************************
		 *	  SET OPERATIONS	*
		 *******************************/

static status
forAllTile(TileObj t, Code msg)
{ if ( notNil(t->object) )
    TRY(forwardCodev(msg, 1, &t->object));

  if ( notNil(t->members) )
  { TileObj st;

    for_chain(t->members, st, TRY(forAllTile(st, msg)));
  }

  succeed;
}

		 /*******************************
		 *	  RESIZE SUPPORT	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Find the tile in a tile-stack that  is above/left-of the separation line
in which `pos' lies.  This is used by resizeTileEventFrame() to find the
tile to resize.

A tile is considered only a candidate for  resizing if it can be resized
and there is at least one tile below/right of it that can be resized.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static status
ICanResizeTile(TileObj t, Name dir)
{ if ( dir == NAME_horizontal )
  { if ( t->horShrink != ZERO || t->horStretch != ZERO )
      succeed;
  } else
  { if ( t->verShrink != ZERO || t->verStretch != ZERO )
      succeed;
  }

  fail;
}


Bool
getCanResizeTile(TileObj t)
{ if ( isDefault(t->canResize) )
  { if ( notNil(t->super) )
    { if ( ICanResizeTile(t, t->super->orientation) )
      { Cell cell;
	int before = TRUE;
	
	for_cell(cell, t->super->members)
	{ TileObj t2 = cell->value;
	  
	  if ( before )
	  { if ( t == t2 )
	      before = FALSE;
	  } else
	  { if ( ICanResizeTile(t2, t->super->orientation) )
	    { assign(t, canResize, ON);
	      goto out;
	    }
	  }
	}
      }
    }

    assign(t, canResize, OFF);
  }

out:
  answer(t->canResize);
}


static int
adjust_pos(Int H, int bh)
{ int h  = valInt(H);
  int i1 = h*3/4;
  int i2 = h - 30;
  int i  = max(i1, i2);
  
  return i;
}

status
updateAdjusterPositionTile(TileObj t)
{ TileAdjuster adj;

  if ( notNil((adj = t->adjuster)) )
  { int cx, cy, bw, bh;
    int border;

    if ( notNil(t->super) )
      border = valInt(t->super->border);
    else
      border = 0;

    bw = valInt(adj->area->w);
    bh = valInt(adj->area->h);

    if ( adj->orientation == NAME_horizontal )
    { cx = valInt(t->area->x) + valInt(t->area->w) + border/2;
      cy = valInt(t->area->y) + adjust_pos(t->area->h, bh);
    } else
    { cy = valInt(t->area->y) + valInt(t->area->h) + border/2;
      cx = valInt(t->area->x) + adjust_pos(t->area->w, bw);
    }

    send(adj, NAME_geometry, toInt(cx-bw/2), toInt(cy-bh/2), EAV);
  }

  succeed;
}


TileObj
getSubTileToResizeTile(TileObj t, Point pos)
{ if ( pointInArea(t->area, pos) && notNil(t->members) )
  { Cell cell;
    
    DEBUG(NAME_tile, Cprintf("getSubTileToResizeTile() at %s, %s: ",
			     pp(pos->x), pp(pos->y)));


					/* in the area of a sub-tile */
    for_cell(cell, t->members)
    { TileObj t2 = cell->value;
      
      if ( pointInArea(t2->area, pos) && notNil(t2->members) )
      { TileObj t3 = getSubTileToResizeTile(t2, pos);

	if ( t3 )
	  answer(t3);
      }
    }

    for_cell(cell, t->members)
    { TileObj t2 = cell->value;
      TileObj t3;

      if ( notNil(cell->next) )
	t3 = cell->next->value;
      else
	break;

					/* +/- 1: be a bit relaxed */
      if ( t->orientation == NAME_horizontal )
      { if ( valInt(pos->x) >= valInt(t2->area->x) + valInt(t2->area->w) - 1 &&
	     valInt(pos->x) <= valInt(t3->area->x) + 1 )
	{ if ( getCanResizeTile(t2) == ON )
	  { DEBUG(NAME_tile, Cprintf("%s\n", pp(t2)));
	    answer(t2);
	  } else
	    goto no;
	}
      } else
      { if ( valInt(pos->y) >= valInt(t2->area->y) + valInt(t2->area->h) - 1 &&
	     valInt(pos->y) <= valInt(t3->area->y) + 1 )
	{ if ( getCanResizeTile(t2) == ON )
	  { DEBUG(NAME_tile, Cprintf("%s\n", pp(t2)));
	    answer(t2);
	  } else
	    goto no;
	}
      }
    }
  }

no:
  DEBUG(NAME_tile, Cprintf("NONE\n"));
  fail;
}

		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_initialise[] =
        { "object=object*", "width=[int]", "height=[int]" };
static char *T_xADintD_yADintD_widthADintD_heightADintD[] =
        { "x=[int]", "y=[int]", "width=[int]", "height=[int]" };
static char *T_associate[] =
	{ "object", "delegate=[bool]" };

/* Instance Variables */

static vardecl var_tile[] =
{ IV(NAME_idealWidth, "int", IV_BOTH,
     NAME_dimension, "Desired width of the tile"),
  IV(NAME_idealHeight, "int", IV_BOTH,
     NAME_dimension, "Desired height of the tile"),
  IV(NAME_horStretch, "int", IV_BOTH,
     NAME_resize, "Encouragement to get wider"),
  IV(NAME_horShrink, "int", IV_BOTH,
     NAME_resize, "Encouragement to get smaller"),
  IV(NAME_verStretch, "int", IV_BOTH,
     NAME_resize, "Encouragement to get higher"),
  IV(NAME_verShrink, "int", IV_BOTH,
     NAME_resize, "Encouragement to get lower"),
  IV(NAME_canResize, "[bool]", IV_SEND,
     NAME_resize, "Can be resized by user?"),
  IV(NAME_adjuster, "tile_adjuster*", IV_GET,
     NAME_resize, "Window to adjust my size"),
  IV(NAME_border, "int", IV_BOTH,
     NAME_appearance, "Distance between areas"),
  IV(NAME_orientation, "{none,horizontal,vertical}", IV_GET,
     NAME_layout, "Direction of adjacent sub-tiles"),
  IV(NAME_members, "chain*", IV_GET,
     NAME_organisation, "Managed tiles (subtiles)"),
  IV(NAME_super, "tile*", IV_GET,
     NAME_organisation, "Tile that manages me"),
  IV(NAME_object, "object*", IV_GET,
     NAME_client, "Object managed"),
  SV(NAME_area, "area", IV_GET|IV_STORE, areaTile,
     NAME_dimension, "Area of the object"),
  IV(NAME_enforced, "bool", IV_GET,
     NAME_layout, "If @on, the tile's layout will be enforced")
};

/* Send Methods */

static senddecl send_tile[] =
{ SM(NAME_initialise, 3, T_initialise, initialiseTile,
     DEFAULT, "Create from object, width and height"),
  SM(NAME_unlink, 0, NULL, unlinkTile,
     DEFAULT, "Unlink sub and super-tiles"),
  SM(NAME_center, 1, "point", centerTile,
     NAME_dimension, "Set center by moving tile"),
  SM(NAME_corner, 1, "point", cornerTile,
     NAME_dimension, "Set point opposite to origin"),
  SM(NAME_height, 1, "int", heightTile,
     NAME_dimension, "Set H of tile"),
  SM(NAME_position, 1, "point", positionTile,
     NAME_dimension, "Set XY of the tile"),
  SM(NAME_set, 4, T_xADintD_yADintD_widthADintD_heightADintD, setTile,
     NAME_dimension, "Set XYWH of entire tile"),
  SM(NAME_size, 1, "size", sizeTile,
     NAME_dimension, "Set WH of the tile"),
  SM(NAME_width, 1, "int", widthTile,
     NAME_dimension, "Set W of tile"),
  SM(NAME_x, 1, "int", xTile,
     NAME_dimension, "Set X of the tile"),
  SM(NAME_y, 1, "int", yTile,
     NAME_dimension, "Set Y of the tile"),
  SM(NAME_forAll, 1, "code", forAllTile,
     NAME_iterate, "Iterate over all <-object's"),
  SM(NAME_above, 2, T_associate, aboveTile,
     NAME_layout, "Place a tile above me"),
  SM(NAME_below, 2, T_associate, belowTile,
     NAME_layout, "Place a tile below me"),
  SM(NAME_enforce, 1, "[bool]", enforceTile,
     NAME_layout, "Enforce the tile layout"),
  SM(NAME_layout, 4, T_xADintD_yADintD_widthADintD_heightADintD, layoutTile,
     NAME_layout, "Compute subtile layout and adjust objects"),
  SM(NAME_left, 2, T_associate, leftTile,
     NAME_layout, "Place a tile to my left"),
  SM(NAME_right, 2, T_associate, rightTile,
     NAME_layout, "Place a tile to my right"),
  SM(NAME_compute, 0, NULL, computeTile,
     NAME_update, "Compute ideal sizes from sub-tiles"),
  SM(NAME_updateAdjusterPosition, 0, NULL, updateAdjusterPositionTile,
     NAME_resize, "Update ->geometry of <-adjuster")
};

/* Get Methods */

static getdecl get_tile[] =
{ GM(NAME_root, 0, "tile", NULL, getRootTile,
     NAME_organisation, "Root of the tile-hierarchy"),
  GM(NAME_subTileToResize, 1, "tile", "point", getSubTileToResizeTile,
     NAME_event, "Tile above or left-of gap at point"), 
  GM(NAME_canResize, 0, "bool", NULL, getCanResizeTile,
     NAME_resize, NULL)
};

/* Resources */

static classvardecl rc_tile[] =
{ RC(NAME_border, "int", "3",
     "Border between subtiles")
};

/* Class Declaration */

static Name tile_termnames[] =
	{ NAME_object, NAME_idealWidth, NAME_idealHeight };

ClassDecl(tile_decls,
          var_tile, send_tile, get_tile, rc_tile,
          1, tile_termnames,
          "$Rev$");


status
makeClassTile(Class class)
{ return declareClass(class, &tile_decls);
}

