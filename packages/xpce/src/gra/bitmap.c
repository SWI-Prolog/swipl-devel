/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>
#include <h/unix.h>

static status	imageBitmap(BitmapObj bm, Image image);
static status	transparentBitmap(BitmapObj bm, Bool transparent);

static status
initialiseBitmap(BitmapObj b, Image image, Bool transparent)
{ if ( isDefault(image) )
    TRY(image = newObject(ClassImage, NIL, 0));
  
  initialiseGraphical(b, ZERO, ZERO, image->size->w, image->size->h);

  assign(b, pen, ZERO);
  assign(b, transparent, OFF);
  assign(b, image, image);
  if ( image->access == NAME_both && isNil(image->bitmap) )
    assign(image, bitmap, b);
  if ( transparent == ON )
    transparentBitmap(b, ON);

  succeed;
}


static status
unlinkBitmap(BitmapObj bm)
{ if ( notNil(bm->image) && bm->image->bitmap == bm )
    assign(bm->image, bitmap, NIL);

  return unlinkGraphical((Graphical) bm);
}


static BitmapObj
getConvertBitmap(Class class, Name name)
{ Image im;

  if ( (im = getConvertImage(ClassImage, (Any) name)) != FAIL )
    answer(answerObject(ClassBitmap, im, 0));

  fail;
}


static status
RedrawAreaBitmap(BitmapObj b, Area a)
{ int x, y, w, h;

  initialiseDeviceGraphical(b, &x, &y, &w, &h);
  if ( notNil(b->image) )
    r_image(b->image, 0, 0, x, y, w, h, b->transparent);

  if ( b->pen != ZERO )
  { r_thickness(valInt(b->pen));
    r_dash(b->texture);
    r_box(x, y, w, h, 0, NIL);
  }

  return RedrawAreaGraphical(b, a);
}


static status
geometryBitmap(BitmapObj b, Int x, Int y, Int w, Int h)
{ return geometryGraphical(b, x, y, DEFAULT, DEFAULT);
}


static BitmapObj
getCopyBitmap(BitmapObj bm)
{ BitmapObj copy = answerObject(ClassBitmap, 0);

  copyGraphical(copy, bm);
  imageBitmap(copy, bm->image);

  answer(copy);
}


static status
imageBitmap(BitmapObj bm, Image image)
{ if ( bm->image != image )
  { CHANGING_GRAPHICAL(bm,
      addRefObj(bm);			/* avoid drop-out */
      assign(bm, image, image);
      sizeArea(bm->area, image->size);
      if ( image->access == NAME_both && isNil(image->bitmap) )
	assign(image, bitmap, bm);
      delRefObj(bm);
      changedEntireImageGraphical(bm));
  }

  succeed;
}


static status
transparentBitmap(BitmapObj bm, Bool transparent)
{ CHANGING_GRAPHICAL(bm,
		     assign(bm, transparent, transparent);
		     if ( transparent == OFF )
		       setFlag(bm, F_SOLID);
		     else
		       clearFlag(bm, F_SOLID);
		     changedEntireImageGraphical(bm));

  succeed;
}


static status
redrawBitmap(BitmapObj bm, Area a)
{ CHANGING_GRAPHICAL(bm, sizeArea(bm->area, bm->image->size));

  return redrawGraphical((Graphical) bm, DEFAULT);
}


static status
loadBitmap(BitmapObj bm, FileObj file, CharArray path)
{ Image image;
  
  if ( isDefault(path) )
    TRY( path = getResourceValueClass(ClassImage, NAME_path));

  TRY(findFile(file, path, NAME_read));

  TRY(image = newObject(ClassImage, file->name, 0));

  return imageBitmap(bm, image);
}


		/********************************
		*     BACKWARD COMPATIBILITY    *
		********************************/


static status
storeBitmap(BitmapObj bm, FileObj file)
{ return storeSlotsObject(bm, file);
}


static status
loadFdBitmap(BitmapObj bm, FILE *fd, ClassDef def)
{ TRY(loadSlotsObject(bm, fd, def));

  if ( restoreVersion < 7 )
  { if ( restoreVersion == 1 )
    { Image image = newObject(ClassImage, 0);

      ws_load_old_image(image, fd);
      assign(bm, image, image);
    } else if ( restoreVersion <= 5 )
    { assign(bm, image, newObject(ClassImage, 0));
      assign(bm, pen, ZERO);
      assign(bm, request_compute, NIL);

      switch( getc(fd) )
      { case 'O':				/* no image */
	  setSize(bm->image->size, ZERO, ZERO);
	  break;
	case 'X':
	  loadXImage(bm->image, fd);
      }
    }

    if ( isNil(bm->texture) )
      assign(bm, texture, NAME_none);
    if ( isNil(bm->colour) )
      assign(bm, colour, DEFAULT);
    if ( isNil(bm->inverted) )
      assign(bm, inverted, OFF);
    if ( isNil(bm->transparent) )
      assign(bm, transparent, OFF);
  }

  succeed;
}


static Chain
getContainsBitmap(BitmapObj bm)
{ answer(answerObject(ClassChain, bm->image, 0));
}


status
makeClassBitmap(Class class)
{ sourceClass(class, makeClassBitmap, __FILE__, "$Revision$");

  localClass(class, NAME_image, NAME_appearance, "image", NAME_get,
	     "The pixel collection managed");
  localClass(class, NAME_transparent, NAME_appearance, "bool", NAME_get,
	     "When @on, 0-pixels are not painted");

  solidClass(class, ON);
  termClass(class, "bitmap", 1, NAME_image);
  setRedrawFunctionClass(class, RedrawAreaBitmap);
  setLoadStoreFunctionClass(class, loadFdBitmap, storeBitmap);
  cloneStyleVariableClass(class, NAME_image, NAME_reference);

  storeMethod(class, NAME_image, imageBitmap);
  storeMethod(class, NAME_transparent, transparentBitmap);
  delegateClass(class, NAME_image);

  sendMethod(class, NAME_initialise, DEFAULT, 2,
	     "image=[image]", "transparent=[bool]",
	     "Create from image",
	     initialiseBitmap);
  sendMethod(class, NAME_unlink, DEFAULT, 0,
	     "Unlink from <-image",
	     unlinkBitmap);
  sendMethod(class, NAME_geometry, DEFAULT, 4,
	     "x=[int]", "y=[int]", "width=[int]", "height=[int]",
	     "Bitmaps can only be moved",
	     geometryBitmap);
  sendMethod(class, NAME_DrawPostScript, NAME_postscript, 0,
	     "Create PostScript",
	     drawPostScriptBitmap);
  sendMethod(class, NAME_load, NAME_file, 2, "file", "path=[char_array]",
	     "Load file (in path) into bitmap",
	     loadBitmap);
  sendMethod(class, NAME_redraw, NAME_change, 1, "[area]",
	     "Update size and repaint indicated area",
	     redrawBitmap);
  
  getMethod(class, NAME_convert, DEFAULT, "bitmap", 1, "name",
	    "Convert image-names",
	    getConvertBitmap);
  getMethod(class, NAME_copy, NAME_copy, "bitmap", 0,
	    "Make a copy that shares the image",
	    getCopyBitmap);
  getMethod(class, NAME_contains, DEFAULT, "chain", 0,
	    "New chain with <-image",
	    getContainsBitmap);

  succeed;
}

