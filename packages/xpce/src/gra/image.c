/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>
#include <h/unix.h>


		/********************************
		*         CREATE/DESTROY	*
		********************************/

status
initialiseImage(Image image, Name name, Int w, Int h, Name kind)
{ if ( isDefault(name) )
    name = (Name) NIL;

  assign(image, name,       name);
  assign(image, background, DEFAULT);
  assign(image, foreground, DEFAULT);
  ws_init_image(image);

  if ( isNil(name) || notDefault(w) || notDefault(h) || notDefault(kind) )
  { if ( isDefault(w) )    w = toInt(16);
    if ( isDefault(h) )    h = toInt(16);
    if ( isDefault(kind) ) kind = NAME_bitmap;

    assign(image, kind,   kind);
    assign(image, file,   NIL);
    assign(image, access, NAME_both);
    assign(image, depth,  kind == NAME_bitmap ? ONE : (Int) DEFAULT);
    assign(image, size,	  newObject(ClassSize, w, h, 0));
  } else
  { assign(image, kind,	  NAME_bitmap);
    assign(image, file,	  newObject(ClassFile, getExternalName(name), 0));
    assign(image, access, NAME_read);
    assign(image, depth,  ONE);
    assign(image, size,	  newObject(ClassSize, 0));
    TRY(loadImage(image, DEFAULT, DEFAULT));
    protectObject(image);
    if ( notNil(name) )
      appendHashTable(ImageTable, name, image);
  }

  succeed;
}


static Image
getLookupImage(Class class, Name name)
{ answer(getMemberHashTable(ImageTable, name));
}


static status
unlinkImage(Image image)
{ XcloseImage(image, DEFAULT);
  ws_destroy_image(image);

  if ( notNil(image->bitmap) && image->bitmap->image == image )
    assign(image->bitmap, image, NIL);

  if ( notNil(image->name) )
    deleteHashTable(ImageTable, image->name);

  succeed;
}


static BitmapObj
getContainedInImage(Image image)
{ if ( notNil(image->bitmap) )
    return image->bitmap;

  fail;
}

		/********************************
		*           CONVERSION		*
		********************************/

Image
getConvertImage(Class class, Any obj)
{ Image image;
  Name name;

  if ( (image = getConvertObject(class, obj)) )
  { if ( instanceOfObject(image, ClassImage) )
      answer(image);

    obj = image;
  }

  if ( instanceOfObject(obj, ClassBitmap) )
    answer(((BitmapObj)obj)->image);

  TRY(name = checkType(obj, TypeName, class));

  if ( (image = getMemberHashTable(ImageTable, name)) )
    answer(image);

  answer(answerObject(ClassImage, name, 0));
}


		/********************************
		*            CHANGES		*
		********************************/

	/* TBD: function! */
#define CHANGING_IMAGE(img, code) \
  { BitmapObj _b = (img)->bitmap; \
 \
    code; \
 \
    if ( notNil(_b) ) \
    { Size _s = (img)->size; \
      Area _a = _b->area; \
 \
      if ( _s->w != _a->w || _s->h != _a->h ) \
      { Int _w = _a->w, _h = _a->h; \
 \
	assign(_a, w, _s->w); \
	assign(_a, h, _s->h); \
	changedAreaGraphical(_b, _a->x, _a->y, _w, _h); \
      } \
    } \
  }


		/********************************
		*           LOAD/STORE		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Store/load images to/form file. Format:

<image>	::= <pce-slots>
	    'X' <image> | 'O'
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static status
storeImage(Image image, FileObj file)
{ TRY( storeSlotsObject(image, file) );

  if ( isNil(image->file) )
    return ws_store_image(image, file);
  else
  { putc('O', file->fd);
    succeed;
  }
}


static status
loadFdImage(Image image, FILE *fd, ClassDef def)
{ TRY( loadSlotsObject(image, fd, def) );
  ws_init_image(image);

					/* convert old path-representation */
  if ( notNil(image->file) &&
       str_fetch(&image->file->name->data, 0) == '/' &&
       getBaseNameFile(image->file) == image->name )
  { assign(image->file, path, image->file->name);
    assign(image->file, name, image->name);
  }

  switch( getc(fd) )
  { case 'O':				/* no image */
      break;
    case 'X':
      return loadXImage(image, fd);
  }

  succeed;
}


		/********************************
		*          XOPEN/XCLOSE		*
		********************************/


status
XopenImage(Image image, DisplayObj d)
{ return ws_open_image(image, d);
}


status
XcloseImage(Image image, DisplayObj d)
{ ws_close_image(image, d);

  succeed;
}

		/********************************
		*         FILE OPERATIONS	*
		********************************/

status
loadImage(Image image, FileObj file, CharArray path)
{ status rval;

  if ( notDefault(file) )
    assign(image, file, file);
    
  if ( isNil(image->file) )
    fail;
  if ( isDefault(path) )
    TRY(path = getResourceValueObject(image, NAME_path));

  TRY( findFile(image->file, path, NAME_read) );

  CHANGING_IMAGE(image,
		 (rval = ws_load_image_file(image)));

  return rval;
}


static status
saveImage(Image image, FileObj file)
{ if ( isDefault(file) )
    file = image->file;

  if ( isNil(file) )
    return errorPce(image, NAME_noFile);

  return ws_save_image_file(image, file);
}


		/********************************
		*        EDIT OPERATIONS	*
		********************************/

static status
verifyAccessImage(Image image, Name sel)
{ if ( image->access != NAME_both )
    return errorPce(image, NAME_readOnly);

  if ( isNil(image->display) )
    assign(image, display, CurrentDisplay(image));

  openDisplay(image->display);

  succeed;
}


static status
changedImageImage(Image image, Int x, Int y, Int w, Int h)
{ if ( notNil(image->bitmap))
    return changedImageGraphical(image->bitmap, x, y, w, h);

  succeed;
}


static status
changedEntireImageImage(Image image)
{ if ( notNil(image->bitmap))
    return changedImageGraphical(image->bitmap, ZERO, ZERO,
				 image->size->w, image->size->h);

  succeed;
}


static status
inImage(Image image, Int x, Int y)
{ if ( valInt(x) >= 0 && valInt(y) >= 0 &&
       valInt(x) < valInt(image->size->w) &&
       valInt(y) < valInt(image->size->h) )
    succeed;

  fail;
}


static status
clearImage(Image image)
{ TRY( verifyAccessImage(image, NAME_clear) );

  CHANGING_IMAGE(image,
		 if ( image->size->w != ZERO && image->size->h != ZERO &&
		      notNil(image->display) &&
		      getExistingXrefObject(image, image->display) != NULL )
	         { int w = valInt(image->size->w);
		   int h = valInt(image->size->h);

		   d_image(image, 0, 0, w, h);
		   d_modify();
		   r_clear(0, 0, w, h);
		   d_done();
		   changedEntireImageImage(image);
		 });

  succeed;
}


status
resizeImage(Image image, Int w, Int h)
{ TRY( verifyAccessImage(image, NAME_resize) );

  CHANGING_IMAGE(image,
		 ws_resize_image(image, w, h));

  succeed;
}


static status
copyImage(Image image, Image i2)
{ Int w = i2->size->w;
  Int h = i2->size->h;

  TRY(verifyAccessImage(image, NAME_copy));

  CHANGING_IMAGE(image,
    TRY(resizeImage(image, w, h));

    d_image(image, 0, 0, valInt(w), valInt(h));
    d_modify();
    r_image(i2, 0, 0, 0, 0, valInt(w), valInt(h), OFF);
    d_done();
    changedEntireImageImage(image););


  succeed;
}


static status
drawInImage(Image image, Graphical gr, Point pos)
{ Int oldx, oldy;
  Device dev;

  TRY(verifyAccessImage(image, NAME_drawIn));

  if ( notDefault(pos) )
  { oldx = gr->area->x;
    oldy = gr->area->y;
    dev = gr->device;
    gr->device = NIL;
    setGraphical(gr, pos->x, pos->y, DEFAULT, DEFAULT);
  } else
  { oldx = oldy = DEFAULT;
    dev = NIL;				/* keep compiler happy */
  }

  ComputeGraphical(gr);

  CHANGING_IMAGE(image,
    d_image(image, 0, 0, valInt(image->size->w), valInt(image->size->h));
    d_modify();
    RedrawArea(gr, gr->area);
    d_done();
    changedEntireImageImage(image););

  if ( notDefault(oldx) )
  { setGraphical(gr, oldx, oldy, DEFAULT, DEFAULT);
    gr->device = dev;
  }

  succeed;
}

		/********************************
		*            FILLING		*
		********************************/

static status
fillImage(Image image, Image pattern, Area area)
{ int x, y, w, h;

  TRY(verifyAccessImage(image, NAME_fill));

  if ( isDefault(area) )
  { x = y = 0;
    w = valInt(image->size->w);
    h = valInt(image->size->h);
  } else
  { x = valInt(area->x);
    y = valInt(area->y);
    w = valInt(area->w);
    h = valInt(area->h);

    NormaliseArea(x, y, w, h);
    if ( x < 0 ) w += x, x = 0;
    if ( y < 0 ) h += y, y = 0;
    if ( x+w > valInt(image->size->w) ) w = valInt(image->size->w) - x;
    if ( y+h > valInt(image->size->h) ) h = valInt(image->size->h) - y;
  }

  if ( w > 0 && h > 0 )
  { CHANGING_IMAGE(image,
	  d_image(image, 0, 0, valInt(image->size->w), valInt(image->size->h));
	  d_modify();
	  r_fill(x, y, w, h, pattern);
	  d_done();
	  changedEntireImageImage(image));
  }

  succeed;
}


		/********************************
		*           PIXELS		*
		********************************/


static status
pixelImage(Image image, Int X, Int Y, Any obj)
{ int x = valInt(X);
  int y = valInt(Y);

  TRY( verifyAccessImage(image, NAME_pixel) );

  if ( inImage(image, X, Y) )
  { if ( image->kind == NAME_bitmap )
    { Bool val = checkType(obj, TypeBool, image);

      if ( !val )
	return errorTypeMismatch(image,
				 getMethodFromFunction((Any) pixelImage), 3,
				 TypeBool);

      CHANGING_IMAGE(image,
	  d_image(image, 0, 0, valInt(image->size->w), valInt(image->size->h));
	  d_modify();
	  r_pixel(x, y, val);
	  d_done();
	  changedImageImage(image, X, Y, ONE, ONE));
    } else
    { Colour val = checkType(obj, TypeColour, image);

      if ( !val )
	return errorTypeMismatch(image,
				 getMethodFromFunction((Any) pixelImage), 3,
				 TypeColour);

      CHANGING_IMAGE(image,
	  d_image(image, 0, 0, valInt(image->size->w), valInt(image->size->h));
	  d_modify();
	  r_pixel(x, y, val);
	  d_done();
	  changedImageImage(image, X, Y, ONE, ONE));
    }

    succeed;
  }
  
  fail;
}


static status
setPixelImage(Image image, Int x, Int y)
{ if ( image->kind == NAME_bitmap )
    return pixelImage(image, x, y, ON);
  else
    return pixelImage(image, x, y, image->foreground);
}


static status
clearPixelImage(Image image, Int x, Int y)
{ if ( image->kind == NAME_bitmap )
    return pixelImage(image, x, y, OFF);
  else
    return pixelImage(image, x, y, image->background);
}


static status
invertPixelImage(Image image, Int x, Int y)
{ TRY(verifyAccessImage(image, NAME_invertPixel));

  if ( inImage(image, x, y) )
  { CHANGING_IMAGE(image, 
	d_image(image, 0, 0, valInt(image->size->w), valInt(image->size->h));
	d_modify();
	r_complement_pixel(valInt(x), valInt(y));
	d_done();
	changedImageImage(image, x, y, ONE, ONE));
  }
  succeed;
}


static status
invertImage(Image image)
{ TRY(verifyAccessImage(image, NAME_invert));

  CHANGING_IMAGE(image, 
	d_image(image, 0, 0, valInt(image->size->w), valInt(image->size->h));
	d_modify();
	r_complement(0, 0, valInt(image->size->w), valInt(image->size->h));
	d_done();
	changedEntireImageImage(image));

  succeed;
}


static Any
getPixelImage(Image image, Int x, Int y)
{ if ( inImage(image, x, y) )
  { ulong pixel;
    d_image(image, 0, 0, valInt(image->size->w), valInt(image->size->h));
    pixel = r_get_pixel(valInt(x), valInt(y));
    d_done();

    DEBUG(NAME_image, printf("pixel = %ld\n", pixel));
    if ( pixel == NoPixel )
      fail;
    if ( image->kind == NAME_bitmap )
      answer((pixel == 0 ? OFF : ON));
    
    answer(ws_pixel_to_colour(image->display, pixel));
  }
  
  fail;
}

		/********************************
		*      LOGICAL OPERATIONS	*
		********************************/

static status
opImage(Image image, Image i2, Name op, Point pos)
{ int x, y;

  TRY(verifyAccessImage(image, op));
  if ( notDefault(pos) )
  { x = valInt(pos->x);
    y = valInt(pos->y);
  } else
    x = y = 0;

  CHANGING_IMAGE(image, 
	d_image(image, x, y, valInt(image->size->w), valInt(image->size->h));
	d_modify();
	r_op_image(i2, 0, 0, x, y, valInt(i2->size->w), valInt(i2->size->h),
		   op);
	d_done();
	changedEntireImageImage(image));

  succeed;
}


static status
orImage(Image image, Image i2, Point pos)
{ return opImage(image, i2, NAME_or, pos);
}


static status
andImage(Image image, Image i2, Point pos)
{ return opImage(image, i2, NAME_and, pos);
}


static status
xorImage(Image image, Image i2, Point pos)
{ return opImage(image, i2, NAME_xor, pos);
}


		/********************************
		*        GET SUB IMAGES		*
		********************************/

static Image
getClipImage(Image image, Area area)
{ Int x, y, w, h;
  Image i2;

  if ( isDefault(area) )
  { x = y = ZERO;
    w = image->size->w;
    h = image->size->h;
  } else
  { x = area->x; y = area->y;
    w = area->w; h = area->h;
  }

  i2 = answerObject(ClassImage, NIL, w, h, image->kind, 0);
  CHANGING_IMAGE(i2,
    d_image(i2, 0, 0, valInt(w), valInt(h));
    d_modify();
    r_image(image, valInt(x), valInt(y), 0, 0, valInt(w), valInt(h), OFF);
    d_done();
    changedEntireImageImage(i2););

  answer(i2);
}

		/********************************
		*           POSTSCRIPT		*
		********************************/

static Area
getBoundingBoxImage(Image image)
{ answer(answerObject(ClassArea,
		      ZERO, ZERO, image->size->w, image->size->h, 0));
}


		/********************************
		*       PREDEFINED IMAGES	*
		********************************/

#include "bitmaps/cycle_bm"
#include "bitmaps/mark_bm"
#include "bitmaps/nomark_bm"
#include "bitmaps/pullright_bm"
#include "bitmaps/mark_handle_bm"

static Image
stdImage(Name name, Image *global, char *bits, int w, int h)
{ Image image = globalObject(name, ClassImage, name, toInt(w), toInt(h), 0);
  
  assign(image, access, NAME_read);
  ws_create_image_from_x11_data(image, (unsigned char *)bits, w, h);
  *global = image;

  return image;
}


#include "bitmaps/white_bm"
#include "bitmaps/grey12_bm"
#include "bitmaps/grey25_bm"
#include "bitmaps/grey50_bm"
#include "bitmaps/grey75_bm"
#include "bitmaps/black_bm"

static void
greyImage(Name name, int grey, Image *global,
	  char *bits, int w, int h)
{ Image image;

  image = stdImage(name, global, bits, w, h);

  attach_attribute(image, NAME_postscriptGrey, toInt(grey));
}


static void
standardImages(void)
{ greyImage(NAME_whiteImage,  0,  &WHITE_IMAGE,
	     white_bm_bits, white_bm_width, white_bm_height);
  greyImage(NAME_grey12Image, 12, &GREY12_IMAGE,
	     grey12_bm_bits, grey12_bm_width, grey12_bm_height);
  greyImage(NAME_grey25Image, 25, &GREY25_IMAGE,
	     grey25_bm_bits, grey25_bm_width, grey25_bm_height);
  greyImage(NAME_grey50Image, 50, &GREY50_IMAGE,
	     grey50_bm_bits, grey50_bm_width, grey50_bm_height);
  greyImage(NAME_grey75Image, 75, &GREY75_IMAGE,
	     grey75_bm_bits, grey75_bm_width, grey75_bm_height);
  greyImage(NAME_blackImage, 100, &BLACK_IMAGE,
	     black_bm_bits, black_bm_width, black_bm_height);

  stdImage(NAME_cycleImage, &CYCLE_IMAGE,
	     cycle_bm_bits, cycle_bm_width, cycle_bm_height);
  stdImage(NAME_markImage, &MARK_IMAGE,
	     mark_bm_bits, mark_bm_width, mark_bm_height);
  stdImage(NAME_nomarkImage, &NOMARK_IMAGE,
	     nomark_bm_bits, nomark_bm_width, nomark_bm_height);
  stdImage(NAME_pullRightImage, &PULLRIGHT_IMAGE,
	     pullright_bm_bits, pullright_bm_width, pullright_bm_height);
  stdImage(NAME_markHandleImage, &MARK_HANDLE_IMAGE,
	    mark_handle_bm_bits, mark_handle_bm_width, mark_handle_bm_height);
}


status
makeClassImage(Class class)
{ sourceClass(class, makeClassImage, __FILE__, "$Revision$");

  localClass(class, NAME_name, NAME_name, "name*", NAME_get,
	     "Name of the image");
  localClass(class, NAME_kind, NAME_colour, "{bitmap,pixmap}", NAME_get,
	     "`bitmap' (0 and 1's) or `pixmap' (coloured)");
  localClass(class, NAME_file, NAME_file, "file*", NAME_get,
	     "File from which to load");
  localClass(class, NAME_access, NAME_permission, "{read,both}", NAME_get,
	     "One of {read, both}");
  localClass(class, NAME_background, NAME_colour, "[colour|pixmap]", NAME_both,
	     "Colour of background (pixmap)");
  localClass(class, NAME_foreground, NAME_colour, "[colour|pixmap]", NAME_both,
	     "Colour of foreground (pixmap)");
  localClass(class, NAME_depth, NAME_colour, "[int]", NAME_get,
	     "Number of bits/pixel");
  localClass(class, NAME_size, NAME_dimension, "size", NAME_get,	
	     "Size of the image in pixels");
  localClass(class, NAME_display, NAME_organisation, "display*", NAME_get,
	     "X-Display this image belongs to");
  localClass(class, NAME_bitmap, NAME_organisation, "bitmap*", NAME_get,
	     "Access both and displayed on this bitmap");
  localClass(class, NAME_wsRef, NAME_storage, "alien:WsRef", NAME_none,
	     "Window System Reference");

  termClass(class, "image", 1, NAME_name);
  saveStyleClass(class, NAME_external);
  setLoadStoreFunctionClass(class, loadFdImage, storeImage);
  cloneStyleClass(class, NAME_none);	/* just copy reference */

  sendMethod(class, NAME_initialise, DEFAULT, 4,
	     "name=[name]*", "width=[int]", "height=[int]",
	     "kind=[{bitmap,pixmap}]",
	     "Create from name, [width, height, kind]",
	     initialiseImage);
  sendMethod(class, NAME_unlink, DEFAULT, 0,
	     "Destroy private memeory and X-resources",
	     unlinkImage);
  sendMethod(class, NAME_Xopen, NAME_x, 1, "display",
	     "Open X-image",
	     XopenImage);
  sendMethod(class, NAME_Xclose, NAME_x, 1, "display",
	     "Closedown resources at server",
	     XcloseImage);
  sendMethod(class, NAME_clear, NAME_edit, 0,
	     "Clear all pixels to 0 or <-background",
	     clearImage);
  sendMethod(class, NAME_resize, NAME_dimension, 2, "width=int", "height=int",
	     "Resize image to width, height",
	     resizeImage);
  sendMethod(class, NAME_copy, NAME_copy, 1, "from=image",
	     "Copy contents of argument in image",
	     copyImage);
  sendMethod(class, NAME_drawIn, NAME_copy, 2, "graphical", "at=[point]",
	     "Paint graphical in image [at point]",
	     drawInImage);
  sendMethod(class, NAME_save, NAME_file, 1, "in=[file]",
	     "Save bits in standard X11 format",
	     saveImage);
  sendMethod(class, NAME_fill, NAME_edit, 2, "image", "[area]",
	     "Fill rectangular area of image with pattern",
	     fillImage);
  sendMethod(class, NAME_clearPixel, NAME_pixel, 2, "x=int", "y=int",
	     "Clear pixel at x-y (to 0 or background)",
	     clearPixelImage);
  sendMethod(class, NAME_setPixel, NAME_pixel, 2, "x=int", "y=int",
	     "Set pixel at x-y (to 1 or foreground)",
	     setPixelImage);
  sendMethod(class, NAME_pixel, NAME_pixel, 3,
	     "x=int", "y=int", "value=bool|colour",
	     "Set pixel at x-y to bool or colour",
	     pixelImage);
  sendMethod(class, NAME_invert, NAME_edit, 0,
	     "Invert all pixels in image",
	     invertImage);
  sendMethod(class, NAME_invertPixel, NAME_pixel, 2, "x=int", "y=int",
	     "Invert pixel at x-y",
	     invertPixelImage);
  sendMethod(class, NAME_or, NAME_edit, 2, "image", "at=[point]",
	     "Bitwise or with argument",
	     orImage);
  sendMethod(class, NAME_and, NAME_edit, 2, "image", "at=[point]",
	     "Bitwise and with argument",
	     andImage);
  sendMethod(class, NAME_xor, NAME_edit, 2, "image", "at=[point]",
	     "Bitwise xor with argument",
	     xorImage);
  sendMethod(class, NAME_DrawPostScript, NAME_postscript, 0,
	     "Create PostScript",
	     drawPostScriptImage);
  sendMethod(class, NAME_load, NAME_file, 2,
	     "from=[file]", "path=[char_array]",
	     "Load image from file (searching in path)",
	     loadImage);

  getMethod(class, NAME_convert, DEFAULT, "image", 1, "bitmap|name",
	    "Convert bitmap or (file-)name",
	    getConvertImage);
  getMethod(class, NAME_lookup, NAME_oms, "image", 1, "name",
	    "Lookup in @images table",
	    getLookupImage);
  getMethod(class, NAME_clip, NAME_copy, "image", 1, "[area]",
	    "Get a subimage",
	    getClipImage);
  getMethod(class, NAME_pixel, NAME_pixel, "value=bool|colour", 2,
	    "x=int", "y=int",
	    "Get 0-1 (image) or colour for x-y",
	    getPixelImage);
  getMethod(class, NAME_boundingBox, NAME_postscript, "area", 0,
	    "BoundingBox for PostScript generation",
	    getBoundingBoxImage);
  getMethod(class, NAME_postscript, NAME_postscript, "string", 2,
	    "landscape=[bool]", "maximum_area=[area]",
	    "New string holding PostScript description",
	    getPostscriptObject);
  getMethod(class, NAME_containedIn, DEFAULT, "bitmap", 0,
	    "Equivalent to <-bitmap if ot @nil",
	    getContainedInImage);

  attach_resource(class, "path", "string",
		  "\".:bitmaps:~/lib/bitmaps:$PCEHOME/bitmaps:" /* concat */
		  "/usr/include/X11/bitmaps\"",
		  "Search path for loading images");

  ImageTable = globalObject(NAME_images, ClassHashTable, toInt(32), 0);
  standardImages();

  succeed;
}

