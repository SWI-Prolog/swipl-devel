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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This file will contain window-system independent  3-d routines, based on
the real low-level routines in  xdraw.c   and  msdraw.c.  Various things
should be abstracted from these modules and  moved into this module. For
now, it contains only the new primitives.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
r_3d_rectangular_polygon(int n, IPoint pts, Elevation e, int flags)
	Draws a 3-d polygon.  The current implementation only deals
	with horizontal and vertical lines.  Might be abstracted
	further.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define D_RIGHT	0
#define D_UP	1
#define D_LEFT	2
#define D_DOWN  3

static int
direction(ISegment s)
{ if ( s->x1 == s->x2 )
  { if ( s->y2 > s->y1 )
      return D_DOWN;
    else
      return D_UP;
  } else
  { if ( s->x2 > s->x1 )
      return D_RIGHT;
    else
      return D_LEFT;
  }
}


void
r_3d_rectangular_polygon(int n, IPoint pts, Elevation e, int flags)
{ int h = valInt(e->height);
  int up = !(flags & DRAW_3D_DOWN);
  
  if ( h < 0 )
  { up = !up;
    h = -h;
  }

  if ( h )
  { ISegment dark  = (ISegment)alloca(sizeof(isegment) * n * h);
    ISegment light = (ISegment)alloca(sizeof(isegment) * n * h);
    int ndark = 0, nlight = 0;
    int m;
    IPoint p1, p2;

    for(m=0; m<h; m++)
    { int i;

      for(p1 = pts, p2 = p1+1, i=0; i<n; i++, p1++, p2++)
      { isegment s;
  
	if ( i == n-1 )
	  p2 = pts;			/* closing line */

	s.x1 = p1->x;
	s.y1 = p1->y;
	s.x2 = p2->x;
	s.y2 = p2->y;
  
	switch(direction(&s))
	{ case D_RIGHT:
	    p1->y++;
	    p2->y++;
	    goto addlight;
	  case D_UP:
	    p1->x++;
	    p2->x++;
	  addlight:
	    if ( i < n-1 || (flags & DRAW_3D_CLOSED) )
	    { if ( up )
		light[nlight++] = s;
	      else
		dark[ndark++] = s;
	    }
	    break;
	  case D_DOWN:
	    s.x1--;			/* draw at inside */
	    s.x2--;
	    p1->x--;
	    p2->x--;
	    goto adddark;
	  case D_LEFT:
	  default:
	    s.y1--;			/* draw at inside */
	    s.y2--;
	    p1->y--;
	    p2->y--;
	  adddark:
	    if ( i < n-1 || (flags & DRAW_3D_CLOSED) )
	    { if ( up )
		dark[ndark++] = s;
	      else
		light[nlight++] = s;
	    }
	}
      }
    }
    
    r_3d_segments(nlight, light, e, TRUE);
    r_3d_segments(ndark,  dark,  e, FALSE);
  }
}
