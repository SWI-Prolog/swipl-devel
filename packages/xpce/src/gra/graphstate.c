/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1995 University of Amsterdam. All rights reserved.
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
