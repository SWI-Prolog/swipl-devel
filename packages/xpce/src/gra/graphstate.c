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

typedef struct graphics_state *GraphicsState;

struct graphics_state
{ int		level;
  int		thickness;
  Name		texture;
  Any		foreground;
  Any   	background;
  GraphicsState savedstate;		/* previous state */
};

static GraphicsState statelist;
long   statechange;

void
g_save()
{ GraphicsState gs = alloc(sizeof(struct graphics_state));

  gs->level	 = (statelist ? statelist->level+1 : 1);
#ifdef __WIN32__
  gs->thickness  = context.thickness;
  gs->texture    = context.texture;
  gs->foreground = context.colour;
  gs->background = context.background;
#else
  gs->thickness  = context.gcs->pen;
  gs->texture    = context.gcs->dash;
  gs->foreground = context.gcs->colour;
  gs->background = context.gcs->background;
#endif

  gs->savedstate = statelist;
  statelist = gs;
}


void
g_restore()
{ GraphicsState gs = statelist;

  if ( !gs )
  { errorPce(NAME_gRestore, NAME_nestMisMatch);
    return;
  }
  
  r_thickness(gs->thickness);
  r_dash(gs->texture);
  r_colour(gs->foreground);
  r_background(gs->background);
  
  statelist = gs->savedstate;
  unalloc(sizeof(struct graphics_state), gs);
}


int
g_level()
{ return statelist ? 0 : statelist->level;
}
