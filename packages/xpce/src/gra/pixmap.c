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
#include <h/unix.h>
#include <h/graphics.h>

static status
initialisePixmap(PixmapObj pm, Any from, Colour fg, Colour bg, Int w, Int h)
{ if ( isNil(from) )
  { initialiseImage((Image) pm, NIL, w, h, NAME_pixmap);
    if ( notDefault(fg) )
      assign(pm, foreground, fg);
    if ( notDefault(bg) )
      assign(pm, background, bg);

    succeed;
  }

  if ( instanceOfObject(from, ClassImage) )
  { Image i = from;

    initialiseImage((Image) pm, NIL, i->size->w, i->size->h, NAME_pixmap);
    if ( notDefault(fg) )
      assign(pm, foreground, fg);
    if ( notDefault(bg) )
      assign(pm, background, bg);

    TRY(send(pm, NAME_copy, i, EAV));

    newObject(ClassHyper, from, pm, NAME_pixmap, NAME_image, EAV);
    succeed;
  }

  if ( instanceOfObject(from, ClassFile) )
  { FileObj f = from;

    assign(pm, name,	   f->name);
    assign(pm, background, fg);
    assign(pm, foreground, bg);
    assign(pm, kind,	   NAME_pixmap);
    assign(pm, file,	   f);
    assign(pm, access,	   NAME_read);
    assign(pm, depth,	   DEFAULT);
    assign(pm, size,	   newObject(ClassSize,	EAV));
    ws_init_image((Image) pm);
    TRY(loadImage((Image) pm, DEFAULT, DEFAULT));
    protectObject(pm);
    appendHashTable(ImageTable, f->name, pm);

    succeed;
  }

  fail;
}


static PixmapObj
getLookupPixmap(Any receiver, Image i, Colour fg, Colour bg)
{ Chain ch;

  if ( (ch = getAllHypersObject(i, OFF)) )
  { Cell cell;

    for_cell(cell, ch)
    { Hyper h = cell->value;

      if ( h->from == i && h->forward_name == NAME_pixmap )
      { PixmapObj pm = h->to;

	if ( instanceOfObject(pm, ClassPixmap) &&
	     (isDefault(fg) || pm->foreground == fg) &&
	     (isDefault(bg) || pm->background == bg) )
	  answer(pm);
      }
    }
  }

  fail;
}


static PixmapObj
getConvertPixmap(Class class, Any obj)
{ PixmapObj pm;

  if ( (pm = getLookupPixmap(class, obj, DEFAULT, DEFAULT)) )
    answer(pm);

  if ( (pm = getConvertObject(class, obj)) )
  { if ( instanceOfObject(pm, ClassPixmap) )
      answer(pm);

    obj = pm;
  }

  if ( instanceOfObject(obj, ClassBitmap) )
  { pm = (PixmapObj)((BitmapObj)obj)->image;

    if ( instanceOfObject(pm, ClassPixmap) )
      answer(pm);
  }
  
  if ( instanceOfObject(obj, ClassGraphical) )
  { Graphical gr = obj;
    
    ComputeGraphical(gr);
    if ( (pm = newObject(ClassPixmap, NIL,
			  DEFAULT, DEFAULT, /* fg, bg */
			  gr->area->w, gr->area->h,
			  EAV)) )
    { send(pm, NAME_drawIn, gr, answerObject(ClassPoint, EAV), EAV);
      answer(pm);
    }
  }

  return answerObject(ClassPixmap, obj, EAV);
}


static Any
getSourcePixmap(PixmapObj pm)
{ Image src;

  if ( notNil(pm->file) )
    answer(pm->file);
  if ( (src = getHyperedObject(pm, NAME_image, DEFAULT)) )
    answer(src);

  answer(NIL);
}


#ifdef WIN32_GRAPHICS
Colour
getReplacementColourPixmap(PixmapObj pm)
{ Colour c;
  Image i;
  Int grey;

  if ( (c = getAttributeObject(pm, NAME_replacementColour)) )
    answer(c);
  if ( (i = getSourcePixmap(pm)) &&
       instanceOfObject(i, ClassImage) &&
       (grey = getAttributeObject(i, NAME_postscriptGrey)) )
  { char buf[100];
    sprintf(buf, "grey%d", 100-(int)valInt(grey));
    c = newObject(ClassColour, CtoName(buf), EAV);
  } else
    c = BLACK_COLOUR;
    
  errorPce(pm, NAME_replacedByColour, c);
  attributeObject(pm, NAME_replacementColour, c);

  answer(c);
}
#endif /*WIN32_GRAPHICS*/


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_fill[] =
        { "image|colour", "[area]" };
static char *T_initialise[] =
        { "source=[image|file]*", "foreground=[colour]",
	  "background=[colour]", "width=[int]", "height=[int]" };
static char *T_lookup[] =
        { "source=image", "foreground=[colour]", "background=[colour]" };

/* Instance Variables */

#define var_pixmap NULL
/*
vardecl var_pixmap[] =
{ 
};
*/

/* Send Methods */

static senddecl send_pixmap[] =
{ SM(NAME_initialise, 5, T_initialise, initialisePixmap,
     DEFAULT, "Create image of <-kind pixmap"),
  SM(NAME_fill, 2, T_fill, fillImage,
     NAME_edit, "Fill rectangular area of image with pattern")
};

/* Get Methods */

static getdecl get_pixmap[] =
{ GM(NAME_convert, 1, "pixmap", "name|image|graphical|file", getConvertPixmap,
     NAME_oms, "Convert @name, image, graphical or file-data"),
  GM(NAME_lookup, 3, "pixmap", T_lookup, getLookupPixmap,
     NAME_oms, "Lookup already made conversion"),
  GM(NAME_source, 0, "image|file*", NULL, getSourcePixmap,
     NAME_term, "Determine source for term representation")
};

/* Resources */

static classvardecl rc_pixmap[] =
{ RC(NAME_background, "colour", "white",
     "Default background colour"),
  RC(NAME_foreground, "colour", "black",
     "Default foreground colour")
};

/* Class Declaration */

static Name pixmap_termnames[] =
	{ NAME_source, NAME_foreground, NAME_background };

ClassDecl(pixmap_decls,
          var_pixmap, send_pixmap, get_pixmap, rc_pixmap,
          3, pixmap_termnames,
          "$Rev$");


status
makeClassPixmap(Class class)
{ return declareClass(class, &pixmap_decls);
}

