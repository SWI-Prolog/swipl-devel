/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>
#include <h/unix.h>

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

    TRY(send(pm, NAME_copy, i, 0));

    newObject(ClassHyper, from, pm, NAME_pixmap, NAME_image, 0);
    succeed;
  }

  if ( instanceOfObject(from, ClassFile) )
  { FileObj f = from;

    assign(pm, name,       f->name);
    assign(pm, background, fg);
    assign(pm, foreground, bg);
    ws_init_image((Image) pm);
    assign(pm, kind,	  NAME_pixmap);
    assign(pm, file,	  f);
    assign(pm, access, NAME_read);
    assign(pm, depth,  DEFAULT);
    assign(pm, size,	  newObject(ClassSize, 0));
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
getConvertPixmap(Any receiver, Image i)
{ PixmapObj pm;

  if ( !(pm = getLookupPixmap(receiver, i, DEFAULT, DEFAULT)) )
    pm = answerObject(ClassPixmap, i, 0);

  answer(pm);
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


#ifdef __WINDOWS__
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
    sprintf(buf, "grey%d", 100-valInt(grey));
    c = newObject(ClassColour, CtoName(buf), 0);
  } else
    c = BLACK_COLOUR;
    
  errorPce(pm, NAME_replacedColour, c);
  attributeObject(pm, NAME_replacementColour, c);

  answer(c);
}
#endif /*__WINDOWS__*/


status
makeClassPixmap(Class class)
{ sourceClass(class, makeClassPixmap, __FILE__, "$Revision$");

  termClass(class, "pixmap",
	    3, NAME_source, NAME_foreground, NAME_background);

  sendMethod(class, NAME_initialise, DEFAULT, 5,
	     "source=[image|file]*",
	     "foreground=[colour]", "background=[colour]",
	     "width=[int]", "height=[int]",
	     "Create image of <-kind pixmap",
	     initialisePixmap);

  getMethod(class, NAME_lookup, NAME_oms, "pixmap", 3,
	     "source=image", "foreground=[colour]", "background=[colour]",
	     "Lookup already made conversion",
	     getLookupPixmap);
  getMethod(class, NAME_convert, NAME_oms, "pixmap", 1, "source=image",
	     "Lookup already made conversion",
	     getConvertPixmap);
  getMethod(class, NAME_source, NAME_term, "image|file*", 0,
	     "Determine source for term representation",
	     getSourcePixmap);

  attach_resource(class, "foreground", "colour", "black",
		  "Default foreground colour");
  attach_resource(class, "background", "colour", "white",
		  "Default background colour");

  succeed;
}

