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

#include "include.h"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Actually, this module does both medium-level   JPEG and GIF writing. The
low-level routines are in the img   directory. These routines are called
by msimage.c, which in  turn  implements   the  OS  specific  version of
gra/image.c implementing class image.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef __CYGWIN32__
#define XMD_H
#endif

#ifdef HAVE_LIBJPEG
/*#define boolean jpeg_boolean*/
#undef GLOBAL				/* conflict */
#include <jpeglib.h>
#include <jerror.h>
#include <setjmp.h>


extern void jpeg_iostream_dest(j_compress_ptr cinfo, IOSTREAM *outfile);
extern void jpeg_iostream_src(j_decompress_ptr cinfo, IOSTREAM* infile);
extern void attach_dib_image(Image image, BITMAPINFO *bmi, BYTE *bits);

int
write_jpeg_file(IOSTREAM *fd, Image image, HBITMAP bm)
{ BITMAP bitmap;
  int width, height, depth;
  int y;
  HDC hdc;
  HBITMAP obm;
  struct jpeg_compress_struct cinfo;
  struct jpeg_error_mgr jerr;
  JSAMPLE *row;
  DisplayObj d = image->display;
  HPALETTE ohpal=0, hpal;

  if ( !GetObject(bm, sizeof(BITMAP), &bitmap) )
  { Cprintf("write_jpeg_file(): GetObject() failed\n");
    return -1;
  }

  if ( isNil(d) )
    d = CurrentDisplay(image);
  if ( instanceOfObject(d->colour_map, ClassColourMap) )
    hpal = getPaletteColourMap(d->colour_map);
  else
    hpal = NULL;

  width  = bitmap.bmWidth;
  height = bitmap.bmHeight;
  depth  = bitmap.bmPlanes * bitmap.bmBitsPixel;

  hdc = CreateCompatibleDC(NULL);
  if ( hpal )
  { ohpal = SelectPalette(hdc, hpal, FALSE);
    RealizePalette(hdc);
  }
  obm = ZSelectObject(hdc, bm);

  row = pceMalloc(sizeof(JSAMPLE)*3*width);

  cinfo.err = jpeg_std_error(&jerr);
  jpeg_create_compress(&cinfo);
  jpeg_iostream_dest(&cinfo, fd);

  cinfo.image_width = width;
  cinfo.image_height = height;
  cinfo.input_components = 3;
  cinfo.in_color_space = JCS_RGB;
  jpeg_set_defaults(&cinfo);

  jpeg_start_compress(&cinfo, TRUE);

  for(y=0; y<height; y++)
  { int x;
    JSAMPLE *s = row;

    for(x=0; x<width; x++)
    { COLORREF c = GetPixel(hdc, x, y);

      *s++ = GetRValue(c);
      *s++ = GetGValue(c);
      *s++ = GetBValue(c);
      DEBUG(NAME_jpeg, Cprintf("#%02x%02x%02x", s[-3], s[-2], s[-1]));
    }
    DEBUG(NAME_jpeg, Cprintf("\n"));

    jpeg_write_scanlines(&cinfo, &row, 1);
  }

  pceFree(row);
  jpeg_finish_compress(&cinfo);
  jpeg_destroy_compress(&cinfo);

  ZSelectObject(hdc, obm);
  if ( ohpal )
    SelectPalette(hdc, ohpal, FALSE);
  DeleteDC(hdc);

  return 0;
}

		 /*******************************
		 *	   READING JPEG		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
status read_jpeg_file(IOSTREAM *fd, Image image)

Reads JPEG from a stream and attaches a   DIB  to the image. If an error
occurs this routine ensures the file-pointer is not moved, so we can try
other image-formats.

On colour-mapped displays, this routine  passes   the  colour-map of the
display to the JPEG library to reach at an optimal rendered image.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

struct jpeg_colour_map
{ int	   size;
  int 	   allocated;
  JSAMPLE *colours[3];
  RGBQUAD *dib_colours;
};

static JpegColourMap
alloc_jpeg_cmap(int size)
{ JpegColourMap map = alloc(sizeof(*map));

  map->allocated   = size;
  map->size        = 0;
  map->colours[0]  = alloc(sizeof(JSAMPLE)*size);
  map->colours[1]  = alloc(sizeof(JSAMPLE)*size);
  map->colours[2]  = alloc(sizeof(JSAMPLE)*size);
  map->dib_colours = alloc(sizeof(RGBQUAD)*size);

  return map;
}

void
free_jpeg_cmap(JpegColourMap map)
{ unalloc(sizeof(JSAMPLE)*map->allocated, map->colours[0]);
  unalloc(sizeof(JSAMPLE)*map->allocated, map->colours[1]);
  unalloc(sizeof(JSAMPLE)*map->allocated, map->colours[2]);
  unalloc(sizeof(RGBQUAD)*map->allocated, map->dib_colours);

  unalloc(sizeof(*map), map);
}


JpegColourMap
jpeg_cmap_from_colour_map(ColourMap cm, DisplayObj d)
{ WsCmdata data = getWsCmdata(cm);
  Vector colours;

  if ( data->jpeg_cmap )
    return data->jpeg_cmap;

  if ( (colours = get(cm, NAME_colours, EAV)) )
  { int ncolors = valInt(colours->size);
    int i=0;
    Colour e;
    JpegColourMap map = alloc_jpeg_cmap(ncolors);
    
    for_vector(colours, e,
	       { if ( notNil(e) )
		 { int r; int g; int b;
		   if ( isDefault(e->red) )
		     getXrefObject(e, d); 	/* Open the colour */

		   r = valInt(e->red)>>8;
		   g = valInt(e->green)>>8;
		   b = valInt(e->blue)>>8;

		   map->colours[0][i] = r;
		   map->colours[1][i] = g;
		   map->colours[2][i] = b;

		   map->dib_colours[i].rgbRed   = r;
		   map->dib_colours[i].rgbGreen = g;
		   map->dib_colours[i].rgbBlue  = b;
		     
		   i++;
		 }
	       });

    map->size = i;
    data->jpeg_cmap = map;

    return map;
  }

  return NULL;
}


		 /*******************************
		 *	      ERRORS		*
		 *******************************/

struct my_jpeg_error_mgr
{ struct jpeg_error_mgr	jerr;
  jmp_buf 		jmp_context;
};


static void
my_exit(j_common_ptr cl)
{ struct jpeg_decompress_struct *cinfo = (struct jpeg_decompress_struct *)cl;
  struct my_jpeg_error_mgr *jerr = (struct my_jpeg_error_mgr *)cinfo->err;

  longjmp(jerr->jmp_context, 1);
}


status
read_jpeg_file(IOSTREAM *fd, Image image)
{ struct jpeg_decompress_struct cinfo;
  struct my_jpeg_error_mgr jerr;
  long here = Stell(fd);
  long row_stride;
  int width, height, bwidth, image_size;
  JSAMPLE **buff;
  BYTE *data;
  BITMAPINFO *dib = NULL;
  BITMAPINFOHEADER *header;
  DisplayObj d = image->display;
  int outline;
  JpegColourMap cmap = NULL;

  cinfo.err = jpeg_std_error((struct jpeg_error_mgr *)&jerr);
  if ( setjmp(jerr.jmp_context) )
  { switch(jerr.jerr.msg_code)
    { case JERR_OUT_OF_MEMORY:
	return sysPce("Not enough memory");
      case JERR_NO_SOI:
	break;				/* invalid */
      default:
      DEBUG(NAME_image,
	    { char buf[1024];

	      (*jerr.jerr.format_message)((j_common_ptr)&cinfo, buf);
	      Cprintf("JPEG: %s\n", buf);
	    });
        break;				/* also invalid */
    }

    jpeg_destroy_decompress(&cinfo);

    Sseek(fd, here, SEEK_SET);
    fail;
  }
  jerr.jerr.error_exit = my_exit;

  jpeg_create_decompress(&cinfo);
  jpeg_iostream_src(&cinfo, fd);

  jpeg_save_markers(&cinfo, JPEG_COM, 0xffff);
  jpeg_read_header(&cinfo, TRUE);

					/* colourmap handling */
  if ( cinfo.output_components == 3 )
  { if ( isNil(d) )
      d = CurrentDisplay(image);
    openDisplay(d);

    if ( ws_depth_display(d) < 16 &&
	 instanceOfObject(d->colour_map, ClassColourMap) &&
	 (cmap = jpeg_cmap_from_colour_map(d->colour_map, d)) )
    { dib = pceMalloc(sizeof(dib->bmiHeader)+cmap->size*sizeof(RGBQUAD));

      header = &dib->bmiHeader;
      memset(header, 0, sizeof(*header));
      memcpy(&dib->bmiColors[0], cmap->dib_colours,
	     cmap->size*sizeof(RGBQUAD));
      header->biBitCount = 8;
      header->biClrUsed  = cmap->size;
    
      cinfo.colormap = cmap->colours;
      cinfo.actual_number_of_colors = cmap->size;
      cinfo.quantize_colors = TRUE;
    }
  }

  jpeg_start_decompress(&cinfo);
  row_stride = cinfo.output_width * cinfo.output_components;
  buff = (*cinfo.mem->alloc_sarray)((j_common_ptr)&cinfo,
				    JPOOL_IMAGE, row_stride, 1);

  width  = cinfo.image_width;
  height = cinfo.image_height;
  if ( cmap )
    bwidth = width;
  else
    bwidth = ((width*3+3)&0xfffc);	/* why is this? */
  image_size = bwidth*height;
  
  data = pceMalloc(image_size);

  for(outline = height-1;
      cinfo.output_scanline < cinfo.output_height;
      outline--)
  { int i;
    BYTE *src, *dest;

    dest = data + bwidth*outline;
  
    jpeg_read_scanlines(&cinfo, buff, 1);
    i = width;
    src = buff[0];

    if ( cmap )				/* colour-mapped */
    { while(i--)
      { *dest++ = *src++;
      }
    } else
    { switch( cinfo.output_components )
      { case 1:				/* grayscale JPEG */
	  while(i--)
	  { *dest++ = src[0];
	    *dest++ = src[0];
	    *dest++ = src[0];
	    src++;
	  }
	  break;
	case 3:				/* RGB JPEG */
	  while(i--)
	  { *dest++ = src[2];
	    *dest++ = src[1];
	    *dest++ = src[0];
	    src += 3;
	  }
	  break;
	default:			/* We don't have this */
	  Sseek(fd, here, SEEK_SET);
	  Cprintf("JPeg with %d output_components??\n");
	  fail;
      }

      memset(dest, 0, bwidth - width*3);
    }
  }

  if ( cinfo.marker_list )
  { jpeg_saved_marker_ptr m;
    Chain ch;

    attributeObject(image, NAME_comment, (ch=newObject(ClassChain, EAV)));

    for(m = cinfo.marker_list; m; m = m->next )
    { if ( m->marker == JPEG_COM )
      { string s;

	str_inithdr(&s, ENC_ASCII);
	s.size = m->data_length;
	s.s_text8 = m->data;

	appendChain(ch, StringToString(&s));
      }
    }
  }

  jpeg_finish_decompress(&cinfo);
  jpeg_destroy_decompress(&cinfo);

  if ( !dib )
  { dib = pceMalloc(sizeof(*dib));
    header = &dib->bmiHeader;
    memset(dib, 0, sizeof(*dib));
    header->biBitCount	= 24;
  }
  header->biSize        = sizeof(BITMAPINFOHEADER);
  header->biWidth       = width;
  header->biHeight      = height;
  header->biPlanes      = 1;
  header->biCompression = BI_RGB;
  header->biSizeImage   = image_size;

  attach_dib_image(image, dib, data);
  
  succeed;
}

#endif /*HAVE_LIBJPEG*/



		 /*******************************
		 *	 GIF (SHOULD MOVE)	*
		 *******************************/

#ifdef O_GIFWRITE
#include <img/gifwrite.h>
typedef unsigned char GSAMPLE;
static GSAMPLE *mask_bits(HBITMAP mask); /* forwards */

#define ROUND(p, n)		((((p) + (n) - 1) & ~((n) - 1)))

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Old versions used GetPixel(). Now it   uses  GetDIBits(), which probably
gives a big performance boost. Unfortutanely   though we need packed RGB
and GetDIBits() returns a word-aligned array of RGB triples Billy nicely
orders as BGR. Hence the shifting and swapping ...

Note that the height field  of  the   structure  is  set to the negative
height, the bits are  extracted  top-to-bottom   rather  than  MS native
bottom-to-top.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
write_gif_file(IOSTREAM *fd, Image image, HBITMAP bm, HBITMAP mask)
{ int width = valInt(image->size->w);
  int height = valInt(image->size->h);
  int rval, sl;
  HDC hdc;
  GSAMPLE *data, *maskdata = NULL;
  DisplayObj d = image->display;
  HPALETTE ohpal=0, hpal;
  BITMAPINFO info;

  if ( isNil(d) )
    d = CurrentDisplay(image);
  if ( instanceOfObject(d->colour_map, ClassColourMap) )
    hpal = getPaletteColourMap(d->colour_map);
  else
    hpal = NULL;

  hdc = CreateCompatibleDC(NULL);
  if ( hpal )
  { ohpal = SelectPalette(hdc, hpal, FALSE);
    RealizePalette(hdc);
  }

  memset(&info, 0, sizeof(info));
  info.bmiHeader.biSize = sizeof(info);
  info.bmiHeader.biWidth = width;
  info.bmiHeader.biHeight = -height;	/* work top-down */
  info.bmiHeader.biPlanes = 1;
  info.bmiHeader.biBitCount = 24;
  info.bmiHeader.biCompression = BI_RGB;

  if ( !(sl = GetDIBits(hdc, bm,
			0, height,
			NULL,
			&info,
			DIB_RGB_COLORS)) )
  { Cprintf("%s: GetDIBits() returned %d", pp(image), sl);
    return FALSE;
  } else
  { DEBUG(NAME_image,
	  { Cprintf("%s: GetDIBits() returned %d; ", pp(image), sl);
	    Cprintf("Image = %dx%dx%d (%ld bytes)\n",
		    info.bmiHeader.biWidth,
		    info.bmiHeader.biHeight,
		    info.bmiHeader.biBitCount,
		    info.bmiHeader.biSizeImage);
	  });

    data = pceMalloc(info.bmiHeader.biSizeImage);
    height = -info.bmiHeader.biHeight;
    width  = info.bmiHeader.biWidth;

    if ( !GetDIBits(hdc, bm,
		    0, height,
		    data,
		    &info,
		    DIB_RGB_COLORS) )
    { Cprintf("%s: GetDIBits() failed to get bits\n", pp(image));
      return FALSE;
    }

    if ( width*3 % sizeof(DWORD) )
    { int outlensl = width*3;
      int inlensl  = ROUND(outlensl, sizeof(DWORD));
      int y;

      for(y=0; y<height; y++)
      { GSAMPLE *p = data+y*outlensl;
	int x;

	memcpy(p, data+y*inlensl, outlensl);
					/* swap blue/red */
	for(x=0; x<width; x++, p+=3)
	{ GSAMPLE tmp = p[0];
	  p[0] = p[2];
	  p[2] = tmp;
	}
      }
    }
  }

  if ( mask )
    maskdata = mask_bits(mask);

  rval = gifwrite_rgb(fd, data, maskdata, width, height);
  pceFree(data);
  if ( maskdata )
    pceFree(maskdata);

  if ( ohpal )
    SelectPalette(hdc, ohpal, FALSE);
  DeleteDC(hdc);

  return rval;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
mask_bits() returns a bitmask for  gifwrite_rgb().   This  mask  is 1 at
transparent pixels and scanlines  are   byte-aligned.  MS  scanlines are
word-aligned.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static GSAMPLE *
mask_bits(HBITMAP mask)
{ HDC hdc = CreateCompatibleDC(NULL);
  GSAMPLE *data = NULL;
  BITMAPINFO *info = alloca(sizeof(BITMAPINFOHEADER)+2*sizeof(RGBQUAD));
  BITMAP bitmap;
  int outlensl, inlensl;		/* scanline lengths */
  int width, height, y;

  if ( !GetObject(mask, sizeof(BITMAP), &bitmap) )
  { Cprintf("mask_bits(): GetObject() failed\n");
    goto out;
  }
  /*Cprintf("Mask is %dx%d\n", bitmap.bmWidth, bitmap.bmHeight);*/

  memset(info, 0, sizeof(*info));
  info->bmiHeader.biSize = sizeof(*info);
  info->bmiHeader.biWidth = bitmap.bmWidth;
  info->bmiHeader.biHeight = -bitmap.bmHeight;
  info->bmiHeader.biPlanes = 1;
  info->bmiHeader.biBitCount = 1;
  info->bmiHeader.biCompression = BI_RGB;
					/* get info */
  if ( !GetDIBits(hdc, mask,
		  0, bitmap.bmHeight,
		  NULL,
		  info,
		  DIB_RGB_COLORS) )
  { Cprintf("%d: mask_bits(): GetDIBits() failed\n", __LINE__);
    goto out;
  }
  /*Cprintf("Mask: %d bytes\n", info->bmiHeader.biSizeImage);*/

					/* get the bits */
  data = pceMalloc(info->bmiHeader.biSizeImage);
  height = -info->bmiHeader.biHeight;
  width  = info->bmiHeader.biWidth;

  if ( !GetDIBits(hdc, mask,
		  0, height,
		  data,
		  info,
		  DIB_RGB_COLORS) )
  { Cprintf("Mask: GetDIBits() failed to get mask bits\n");
    return FALSE;
  }

  inlensl  = ROUND(width, sizeof(DWORD)*8)/8;
  outlensl = (width+7)/8;

  for(y=0; y<height; y++)
  { GSAMPLE *in  = data + y * inlensl;
    GSAMPLE *out = data + y * outlensl;

    memcpy(out, in, outlensl);

#if 0
    { int x;
      int m = 0x80;
      GSAMPLE *p = data + y * outlensl;

      for(x=0; x<info->bmiHeader.biWidth; x++)
      { Cprintf("%c", p[0]&m ? '.' : '*');
	m>>=1;
	if ( !m )
	{ m = 0x80;
	  p++;
	}
      }
      Cprintf("\n");
    }
#endif
  }

out:
  DeleteDC(hdc);

  return data;
}



#endif /*HAVE_LIBJPEG*/
