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
	    DEBUG(NAME_colour, printf("%s --> 0x%lx\n",
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
      return registerXrefObject(c, d, (void *)valInt(rgb));
  } else
    return registerXrefObject(c, d, (void *)RGB(valInt(c->red),
						valInt(c->green),
						valInt(c->blue)));

  fail;
}


Colour
ws_pixel_to_colour(DisplayObj d, ulong pixel)
{ fail;
}
