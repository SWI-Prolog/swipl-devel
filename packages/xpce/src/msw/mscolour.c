/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

#define USE_CONVERTED_COLOURS 1;

#include "include.h"
#ifdef USE_CONVERTED_COLOURS
#include "xcolours.c"
#else
#include <h/unix.h>
#endif

static HashTable ColourNames;		/* name --> rgb (packed in Int) */
static HashTable X11ColourNames;	/* rgb --> X11-name */

static status	ws_alloc_colour(ColourMap cm, Colour c);
static void	ws_unalloc_colour(ColourMap cm, Colour c);
static void	ws_system_colours(DisplayObj d);

#ifdef USE_CONVERTED_COLOURS

static HashTable
LoadColourNames()
{ if ( !ColourNames )
  { xcolourdef *cd;

    ColourNames = globalObject(NAME_colourNames, ClassHashTable, 0);
    
    for(cd = x11_colours; cd->name; cd++)
    { COLORREF rgb = RGB(cd->red, cd->green, cd->blue);
      char buf[100];
      const char *s = cd->name;
      char *q = buf;

      for( ; *s; s++ )
      { if ( *s == ' ' )
	  *q++ = '_';
	else
	  *q++ = tolower(*s);
      }
      *q = '\0';
      appendHashTable(ColourNames, CtoKeyword(buf), toInt(rgb));
    }

    ws_system_colours(CurrentDisplay(NIL));
  }

  return ColourNames;
}


#else /*USE_CONVERTED_COLOURS*/

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
	    for(e=s; *e; e++)
	    { if ( isupper(*e) )
		*e = tolower(*e);
	      else if ( *e == ' ' )
		*e = '_';
	    }
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

#endif /*USE_CONVERTED_COLOURS*/

#if O_XPM
#ifdef USE_CONVERTED_COLOURS

static HashTable
LoadX11ColourNames()
{ if ( !X11ColourNames )
  { xcolourdef *cd;

    X11ColourNames = globalObject(CtoName("_x11_colour_names"),
				  ClassHashTable, 0);

    for(cd = x11_colours; cd->name; cd++)
    { COLORREF rgb = RGB(cd->red, cd->green, cd->blue);

      appendHashTable(X11ColourNames, toInt(rgb), CtoName(cd->name));
    }
  }

  return X11ColourNames;
}

#else /*USE_CONVERTED_COLOURS*/

static HashTable
LoadX11ColourNames()
{ if ( !X11ColourNames )
  { FileObj f = answerObject(ClassFile, CtoName("$PCEHOME/lib/rgb.txt"), 0);
    X11ColourNames = globalObject(CtoName("_x11_colour_names"),
				  ClassHashTable, 0);
    
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

	    for(s=name; *s && *s <= ' '; s++)
	      ;
	    for(e = s + strlen(s); e > s && e[-1] <= ' '; e--)
	      ;
	    *e = EOS;
	    rgb = RGB(r, g, b);
	    appendHashTable(X11ColourNames, toInt(rgb), CtoName(s));
	    break;
	  }
	}
      }

      send(f, NAME_close, 0);
    }
  }

  return X11ColourNames;
}

#endif /*USE_CONVERTED_COLOURS*/

int
xpmGetRGBfromName(char *inname, int *r, int *g, int *b)
{ char name[256];
  char *q = name, *s = inname;
  Name cname;
  Int pcergb;

  for( ;*s; s++, q++)
  { if ( isupper(*s) )
      *q = tolower(*s);
    else if ( *s == ' ' )
      *q = '_';
    else
      *q = *s;
  }
  *q = EOS;

  cname = CtoKeyword(name);
  if ( (pcergb = getMemberHashTable(LoadColourNames(), cname)) )
  { COLORREF rgb = valInt(pcergb);

    *r = GetRValue(rgb);
    *g = GetGValue(rgb);
    *b = GetBValue(rgb);
  } else
  { *r = 255;				/* nothing there, return red */
    *g = *b = 0;
  }

  return 1;				/* we have it */
}


int
xpmReadRgbNames(char *rgb_fname, void *rgbn)
{ return valInt(LoadX11ColourNames()->size);
}


void
xpmFreeRgbNames(void *rgbn, int rgbn_max)
{
}


char *
xpmGetRgbName(void *rgbn, int rgbn_max, int red, int green, int blue)
{ Int rgb = toInt(RGB(red, green, blue));
  HashTable ht = LoadX11ColourNames();
  Name name;

  if ( rgb == ZERO )
    return "black";			/* otherwise these come as gray0 */
  if ( rgb == toInt(RGB(255,255,255)) )
    return "white";			/* and gray100 */

  if ( (name = getMemberHashTable(ht, rgb)) )
    return strName(name);

  return NULL;
}

#endif /*O_XPM*/


static Name
canonical_colour_name(Name in)
{ char *s = strName(in);
  char buf[100];
  int left = sizeof(buf);
  char *q = buf;
  int changed = 0;
  
  for( ; *s && --left > 0; s++, q++ )
  { if ( *s == ' ' )
    { *q = '_';
      changed++;
    } else if ( isupper(*s) )
    { *q = tolower(*s);
      changed++;
    } else
      *q = *s;
  }
  
  if ( left && changed )
  { *q = EOS;
    return CtoKeyword(buf);
  }

  return in;
}


status
ws_colour_name(DisplayObj d, Name name)
{ HashTable ht = LoadColourNames();

  if ( getMemberHashTable(ht, name) ||
       getMemberHashTable(ht, canonical_colour_name(name)) )
    succeed;

  fail;
}


status
ws_create_colour(Colour c, DisplayObj d)
{ Int Rgb;

  if ( c->kind == NAME_named )
  { HashTable ht = LoadColourNames();

    if ( (Rgb = getMemberHashTable(ht, c->name)) ||
	 (Rgb = getMemberHashTable(ht, canonical_colour_name(c->name))) )
    { COLORREF rgb = (COLORREF) valInt(Rgb);
      int r = GetRValue(rgb) * 257;
      int g = GetGValue(rgb) * 257;
      int b = GetBValue(rgb) * 257;

      assign(c, red,   toInt(r));
      assign(c, green, toInt(g));
      assign(c, blue,  toInt(b));

      registerXrefObject(c, d, (void *)rgb);
    } else
      fail;
  } else
  { COLORREF rgb = RGB(valInt(c->red)/256,
		       valInt(c->green)/256,
		       valInt(c->blue)/256);

    registerXrefObject(c, d, (void *)rgb);
  }

  if ( notNil(d->colour_map) && notDefault(d->colour_map) )
    ws_alloc_colour(d->colour_map, c);

  succeed;
}


void
ws_uncreate_colour(Colour c, DisplayObj d)
{ if ( isDefault(d) )
    d = CurrentDisplay(NIL);

  if ( notNil(d->colour_map) && notDefault(d->colour_map) )
    ws_unalloc_colour(d->colour_map, c);

  unregisterXrefObject(c, d);
}


Colour
ws_pixel_to_colour(DisplayObj d, unsigned long pixel)
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


Colour
ws_system_colour(DisplayObj d, const char *name, COLORREF rgb)
{ Name ref = CtoKeyword(name);
  int r = GetRValue(rgb);
  int g = GetGValue(rgb);
  int b = GetBValue(rgb);
  Colour c;

  r = r*256 + r;
  g = g*256 + g;
  b = b*256 + b;

  if ( (c = newObject(ClassColour, ref, toInt(r), toInt(g), toInt(b), 0)) )
  { lockObject(c, ON);
    registerXrefObject(c, d, (void *)rgb);
  }

  return c;
}


static void
ws_system_colours(DisplayObj d)
{ struct system_colour *sc = window_colours;

  for( ; sc->name; sc++ )
  { DWORD rgb = GetSysColor(sc->id);

    ws_system_colour(d, sc->name, rgb);
  }
}


		 /*******************************
		 *	     COLOURMAPS		*
		 *******************************/

#undef offset
#define offset(s, f) ((int)&((s *)NULL)->f)

static HPALETTE
CreateCCPalette(int size)
{ int le = size * size * size;
  LOGPALETTE *lp = pceMalloc(offset(LOGPALETTE, palPalEntry[le]));
  int i, r, g, b;
  PALETTEENTRY *pe = &lp->palPalEntry[0];
  BYTE *intensity = alloca(size * sizeof(BYTE));
  HPALETTE hpal;

  lp->palVersion    = 0x300;
  lp->palNumEntries = le;

  for(i=0; i<size; i++)
    intensity[i] = (255*i)/(size-1);	/* gamma correction? */

  for(r=0; r<size; r++)
  { for(g = 0; g<size; g++)
    { for(b = 0; b<size; b++)
      { pe->peRed   = intensity[r];
	pe->peGreen = intensity[g];
	pe->peBlue  = intensity[b];
	pe->peFlags = 0;
	pe++;
      }
    }
  }

  if ( !(hpal = CreatePalette(lp)) )
    Cprintf("Failed to create color cube with %d entries\n", le);

  return hpal;
}


static void
ws_open_colourmap(ColourMap cm)
{ if ( !cm->ws_ref && notNil(cm->colours) )
  { int size = valInt(cm->colours->size);
    LOGPALETTE *lp = pceMalloc(offset(LOGPALETTE, palPalEntry[size]));
    PALETTEENTRY *pe = &lp->palPalEntry[0];
    HPALETTE hpal;
    int n, nc = 0;
    DisplayObj d = CurrentDisplay(NIL);

    for(n=0; n<size; n++)
    { Colour c = cm->colours->elements[n];

      if ( isName(c) && (c = checkType(c, TypeColour, NIL)) )
	elementVector(cm->colours, toInt(n+1), c);

      if ( instanceOfObject(c, ClassColour) )
      { if ( c->kind == NAME_named )
	  ws_create_colour(c, d);

	pe->peRed   = valInt(c->red)   >> 8;
	pe->peGreen = valInt(c->green) >> 8;
	pe->peBlue  = valInt(c->blue)  >> 8;
	pe->peFlags = 0;

	pe++;
	nc++;
      } else
	Cprintf("%s is not a colour\n", pp(c));
    }
    
    lp->palVersion    = 0x300;
    lp->palNumEntries = nc;

    DEBUG(NAME_colourMap, Cprintf("Created %s with %d colours\n", pp(cm), nc));

    if ( !(hpal = CreatePalette(lp)) )
      Cprintf("%s: failed to create logpalette\n", pp(cm));

    setPaletteColourMap(cm, hpal);
  }
}


void
setPaletteColourMap(ColourMap cm, HPALETTE hpal)
{ cm->ws_ref = hpal;
}


HPALETTE
getPaletteColourMap(ColourMap cm)
{ if ( !cm->ws_ref )
    ws_open_colourmap(cm);

  return cm->ws_ref;
}


void
ws_colour_cube(ColourMap cm, int size)
{ HPALETTE hpal;
  
  if ( size < 1 )			/* nonsence value */
    size = 2;
  if ( size > 6 )			/* too large */
    size = 6;

  hpal = CreateCCPalette(size);

  setPaletteColourMap(cm, hpal);
}


static Int
XIntensity(BYTE wi)
{ unsigned i = (unsigned)wi;

  i = (i << 8) + i;

  return toInt(i);
}


void
ws_colour_map_colours(ColourMap cm)
{ if ( isNil(cm->colours) )
  { int entries;
    PALETTEENTRY *lpe;
    int n;

    if ( cm->name == NAME_system )	/* system palette */
    { HDC hdc = GetDC(NULL);

      entries = GetSystemPaletteEntries(hdc, 0, 1<<16, NULL);
      lpe = alloca(entries * sizeof(PALETTEENTRY));
      GetSystemPaletteEntries(hdc, 0, entries, lpe);

      ReleaseDC(NULL, hdc);
    } else				/* normal palette */
    { HPALETTE hpal;

      if ( cm->name == NAME_static ||
	   cm->name == NAME_pce )
	hpal = GetStockObject(DEFAULT_PALETTE);
      else
	hpal = getPaletteColourMap(cm);

      if ( !hpal )
	return;

      entries = GetPaletteEntries(hpal, 0, 1<<16, NULL);
      lpe = alloca(entries * sizeof(PALETTEENTRY));
      GetPaletteEntries(hpal, 0, entries, lpe);
    }
  
    assign(cm, colours, newObject(ClassVector, 0));
    elementVector(cm->colours, ONE, NIL);
    elementVector(cm->colours, toInt(entries), NIL);
    for(n=0; n<entries; n++)
    { elementVector(cm->colours, toInt(n+1),
		    newObject(ClassColour, DEFAULT,
			      XIntensity(lpe[n].peRed),
			      XIntensity(lpe[n].peGreen),
			      XIntensity(lpe[n].peBlue), 0));
    }
  }
}


status
ws_create_colour_map(ColourMap cm, DisplayObj d)
{ fail;
}


status
ws_uncreate_colour_map(ColourMap cm, DisplayObj d)
{ fail;
}


		 /*******************************
		 *EXPERIMENTAL COLOUR ALLOCATION*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Allocate a colour in the XPCE central  colourmap. At the moment, the map
is filled, and colours are nog changed afterwards. It would be better to
collect all colours and now and then   build an optimal palette from the
defined colours. How does one do that? 

Also, all device dependant (read: based  on this colourmap) images would
need to be updated (or all images should be stored as DIB images).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static status
ws_alloc_colour(ColourMap cm, Colour c)
{ Int i;

  if ( isNil(cm->colours) )
    ws_colour_map_colours(cm);

  if ( getIndexVector(cm->colours, c) )
    succeed;		/* already there (basically for my own colours) */

  if ( (i=getIndexVector(cm->colours, NIL)) )
    elementVector(cm->colours, i, c);
  else
    appendVector(cm->colours, 1, (Any *)&c);
  delRefObj(c);

  if ( cm->ws_ref )
  { HPALETTE hpal = (HPALETTE) cm->ws_ref;
    PALETTEENTRY peentry;
    PALETTEENTRY *pe = &peentry;
    int ne = valInt(cm->colours->size);

    pe->peRed   = valInt(c->red)   >> 8;
    pe->peGreen = valInt(c->green) >> 8;
    pe->peBlue  = valInt(c->blue)  >> 8;
    pe->peFlags = 0;
      
    if ( i )
    { if ( SetPaletteEntries(hpal, valInt(i)-1, 1, pe) != 1 )
      { DEBUG(NAME_colour,
	      Cprintf("Failed to set %s at index %d of %s\n",
		      pp(c), valInt(i)-1, pp(cm)));
	fail;
      }
      DEBUG(NAME_colour,
	    Cprintf("Reused colour entry %d for %s\n", valInt(i)-1, pp(c)));
    } else
    { if ( ResizePalette(hpal, ne) )
      { if ( SetPaletteEntries(hpal, ne-1, 1, pe) != 1 )
	{ DEBUG(NAME_colour,
		Cprintf("Failed to add %s to %s\n", pp(c), pp(cm)));
	  fail;
	}
      } else
      { DEBUG(NAME_colour,
	      Cprintf("Failed to extend %s\n", pp(cm)));
	fail;
      }
    }
  }

  succeed;
}


static void
ws_unalloc_colour(ColourMap cm, Colour c)
{ if ( notNil(cm->colours) )
  { Int i;

    if ( (i = getIndexVector(cm->colours, c)) )
    { addRefObj(c);
      elementVector(cm->colours, i, NIL);
    }
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Check  whether  the  display   supports    colourmaps.   Got  this  from
http://www.compuphase.com/palette.htm
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

status
ws_has_colourmap(DisplayObj d)
{ HDC hdc    = GetDC(NULL);
  int rc     = GetDeviceCaps(hdc,RASTERCAPS);
  int bits   = GetDeviceCaps(hdc,BITSPIXEL);
  int planes = GetDeviceCaps(hdc,PLANES);
  ReleaseDC(NULL, hdc);
  bits *= planes;       /* bits == flat number of bits per pixel */
  /* The only hardware modes that use a palette are modes
   * with 2, 4, 16 and 256 colours.
   * Microsoft Windows supports palette operations only
   * on displays with 256 colours. That is, in 16-colour
   * mode, you cannot use the palette.
   * So you only need to check for the RC_PALETTE bit if
   * the number of bits per pixel is 8.
   */
  return (rc & RC_PALETTE) != 0 && bits == 8;
}




