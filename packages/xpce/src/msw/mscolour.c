/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

#include "include.h"
#include <h/unix.h>

static HashTable ColourNames;		/* name --> rgb (packed in Int) */


static HashTable
LoadColourNames()
{ if ( !ColourNames )
  { FileObj f = answerObject(ClassFile, CtoName("$PCEHOME/lib/rgb.txt"), 0);
    ColourNames = globalObject(NAME_colourNames, ClassHashTable, 0);
    
    if ( send(f, NAME_open, NAME_read, 0) )
    { char line[256];
      int r, g, b;
      char name[80];

      while( fgets(line, sizeof(line), f->fd) )
      { switch( sscanf(line, "%d%d%d%[^\n]", &r, &g, &b, name) )
	{ case 4:
	  { char *s;
	    char *e;
	    COLORREF rgb;
	    Name cname;

	    for(s=name; *s && *s <= ' '; s++)
	      ;
	    for(e = s + strlen(s); e > s && e[-1] <= ' '; e--)
	      ;
	    *e = EOS;
	    if ( isupper(*s) )
	      *s = tolower(*s);
	    for(e=s; *e; e++)
	      if ( *e == ' ' )
		*e = '_';
	    cname = CtoKeyword(s);
	    rgb = RGB(r, g, b);
	    appendHashTable(ColourNames, cname, toInt(rgb));
	    DEBUG(NAME_colour, Cprintf("%s --> 0x%lx\n",
				       pp(cname), (long) rgb)); 
	    break;
	  }
	}
      }

      send(f, NAME_close, 0);
    }
  }

  return ColourNames;
}



status
ws_colour_name(DisplayObj d, Name name)
{ if ( getMemberHashTable(LoadColourNames(), name) )
    succeed;

  fail;
}


status
ws_create_colour(Colour c, DisplayObj d)
{ Int rgb;

  if ( c->kind == NAME_named )
  { if ( (rgb = getMemberHashTable(LoadColourNames(), c->name)) )
    { COLORREF RGB = (COLORREF) valInt(rgb);
      int r = GetRValue(RGB);
      int g = GetGValue(RGB);
      int b = GetBValue(RGB);

      r = 256*r + r;
      g = 256*g + g;
      b = 256*b + b;

      assign(c, red,   toInt(r));
      assign(c, green, toInt(g));
      assign(c, blue,  toInt(b));

#if 0
      if ( r == g && r == b )		/* grey */
	RGB |= EXACT_COLOUR_MASK;	/* Avoid GetNearestColor() */
#endif

      return registerXrefObject(c, d, (void *)RGB);
    }
  } else
    return registerXrefObject(c, d, (void *)RGB(valInt(c->red)/256,
						valInt(c->green)/256,
						valInt(c->blue)/256));

  fail;
}


void
ws_uncreate_colour(Colour c, DisplayObj d)
{ unregisterXrefObject(c, d);
}


Colour
ws_pixel_to_colour(DisplayObj d, ulong pixel)
{ fail;
}


struct system_colour
{ char *name;
  int  id;
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Windows system colors as obtained from GetSysColor()
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static struct system_colour window_colours[] =
{ { "sys_scrollbar_background",	COLOR_BTNFACE },
  { "sys_dialog_background",	COLOR_MENU },
  { "sys_dialog_foreground",	COLOR_MENUTEXT },
  { "sys_window_background",	COLOR_WINDOW },
  { "sys_window_foreground",	COLOR_WINDOWTEXT },
  { "sys_relief",		COLOR_BTNFACE },
  { "sys_shadow",		COLOR_BTNSHADOW },
  { "sys_inactive",		COLOR_GRAYTEXT },

  { "win_activeborder",		COLOR_ACTIVEBORDER },
  { "win_activecaption",	COLOR_ACTIVECAPTION },
  { "win_appworkspace",		COLOR_APPWORKSPACE },
  { "win_background",		COLOR_BACKGROUND },
  { "win_btnface",		COLOR_BTNFACE },
  { "win_btnshadow",		COLOR_BTNSHADOW },
  { "win_btntext",		COLOR_BTNTEXT },
  { "win_captiontext",		COLOR_CAPTIONTEXT },
  { "win_graytext",		COLOR_GRAYTEXT },
  { "win_highlight",		COLOR_HIGHLIGHT },
  { "win_highlighttext",	COLOR_HIGHLIGHTTEXT },
  { "win_inactiveborder",	COLOR_INACTIVEBORDER },
  { "win_inactivecaption",	COLOR_INACTIVECAPTION },
  { "win_inactivecaptiontext",	COLOR_INACTIVECAPTIONTEXT },
  { "win_menu",			COLOR_MENU },
  { "win_menutext",		COLOR_MENUTEXT },
  { "win_scrollbar",		COLOR_SCROLLBAR },
/*{ "win_shadow",		COLOR_SHADOW },	*/
  { "win_window",		COLOR_WINDOW },
  { "win_windowframe",		COLOR_WINDOWFRAME },
  { "win_windowtext",		COLOR_WINDOWTEXT },

  { NULL, 			0 }
};


void
ws_system_colours(DisplayObj d)
{ struct system_colour *sc = window_colours;

  for( ; sc->name; sc++ )
  { Name ref = CtoKeyword(sc->name);
    DWORD rgb = GetSysColor(sc->id);
    int r = GetRValue(rgb);
    int g = GetGValue(rgb);
    int b = GetBValue(rgb);
    Colour c;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This is odd, these report the same as their window!?
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

    if ( sc->id == COLOR_SCROLLBAR || sc->id == COLOR_BTNFACE )
    { DWORD fg = GetSysColor(COLOR_MENU);

      if ( rgb == fg )
      { r = (r+256)/2;
	g = (g+256)/2;
	b = (b+256)/2;
	rgb = RGB(r, g, b);
      }
    }

    DEBUG(NAME_win, Cprintf("Colour %s = %d %d %d\n", sc->name, r, g, b));
    
    r = r*256 + r;
    g = g*256 + g;
    b = b*256 + b;

    rgb |= EXACT_COLOUR_MASK;	/* Avoid GetNearestColor() */

    if ( (c = newObject(ClassColour, ref, toInt(r), toInt(g), toInt(b), 0)) )
    { lockObject(c, ON);
      registerXrefObject(c, d, (void *)rgb);
    }
  }
}
