/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (note this is different from the rest of XPCE):
    This is free to use and modify provided proper credit is given
*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This file reads a .GIF image file. It is used by giftoxpm.c to create an
XpmImage  structure  from  a  .GIF  file.    After   that,  we  can  use
XpmCreateImagefromImage()  to  do  the  tricky  things  such  as  colour
allocation.  This route is probably a little slower than the direct one,
but acceptable and a lot less code.

The code in this file  is   based  on gifread.cpp by chrisdl@pagesz.net,
from  whom  I  cannot  find  his  real   name.  He  based  his  code  on
``Programming for Graphics Files'' by John Levine.  I'm grateful for his
contribution.

http://www.pagesz.net/~chrisdl/software/jpegfile.htm

This reads GIF 87a and 89a.

I converted the code to plain ANSI-C   and changed various Windows idiom
to standard C for easier integration in  XPCE and modified the interface
to reuse the Xpm package to do the tricky work.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#include <h/kernel.h>
#include <fcntl.h>
#include "gif.h"

#define INTERLACE	0x40
#define LOCALCOLORMAP   0x80
#define BitSet(byte,bit) (((byte) & (bit))==(bit))
#define ReadOK(file,buffer,len) (Sfread(buffer,1,(len),file)==(len))
#define LM_to_uint(a,b)	(((b)<<8)|(a))

typedef short int		code_int;	/* was int */
typedef long int		count_int;
typedef unsigned char pixval;

static int 
ReadColorMap(IOSTREAM *fd, int number,
	     GIFAllocColorTable at, GIFAllocColor ac, void *closure);
static int DoExtension(IOSTREAM *fd,int label);
static int GetDataBlock(IOSTREAM *fd, UCHAR *buf);
static int GetCode (IOSTREAM *fd, int code_size, int flag);
static int LZWReadByte (IOSTREAM *fd,int flag, int  input_code_size);

static int ReadImage(IOSTREAM *fd,
		     PIXEL *bigMemBuf,
		     int width, int height,
		     int interlace);


struct
{ unsigned int Width;
  unsigned int Height;
  unsigned int BitPixel;
  unsigned int ColorResolution;
  unsigned int BackGround;
  unsigned int AspectRatio;
} GifScreen;

struct
{ int transparent;
  int delayTime;
  int inputFlag;
  int disposal;
} Gif89 = { -1, -1, -1, 0 };

char *GIFErrorText;

const char *
GIFError()
{ return GIFErrorText ? GIFErrorText : "No Error";
}

static void
setGifError(const char *fmt)
{ if ( GIFErrorText )
    pceFree(GIFErrorText);

  if ( (GIFErrorText = pceMalloc(strlen(fmt)+1)) )
    strcpy(GIFErrorText, fmt);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
GIFReadFD(IOSTREAM *fd,
	  PIXEL **data, int *width, int *height,
	  GIFAllocColorTable at, GIFAllocColor ac, void *closure)
	Read GIF image from the given IO/stream
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
GIFReadFD(IOSTREAM *fd,
	  PIXEL **data, int *width, int *height,
	  GIFAllocColorTable at, GIFAllocColor ac, void *closure)
{
  UCHAR buf[16];
  UCHAR c;
  int useGlobalColormap;
  int bitPixel;
  char version[4];
  int w = 0;
  int h = 0;
  PIXEL *bigBuf;
  int rval;

  /* read GIF file header */
  if (!ReadOK(fd, buf, 6))
  { setGifError("Error reading GIF Magic");
    return GIF_INVALID;
  }
  /* need the string "GIF" in the header */
  if (strncmp((char *) buf, "GIF", 3) != 0)
  { setGifError("not a valid .GIF file");
    return GIF_INVALID;
  }
  strncpy(version, (char *) (buf + 3), 3);
  version[3] = '\0';

  /* only handle v 87a and 89a */
  if ((strcmp(version, "87a") != 0) && (strcmp(version, "89a") != 0))
  { setGifError("Error, Bad GIF Version number");
    return GIF_INVALID;
  }
  /* screen description */
  if (!ReadOK(fd, buf, 7))
  { setGifError("failed to GIF read screen descriptor. Giving up");
    return GIF_INVALID;
  }
  GifScreen.Width = LM_to_uint((UCHAR) buf[0], (UCHAR) buf[1]);
  GifScreen.Height = LM_to_uint((UCHAR) buf[2], (UCHAR) buf[3]);
  GifScreen.BitPixel = 2 << ((UCHAR) buf[4] & 0x07);
  GifScreen.ColorResolution = ((((UCHAR) buf[4] & 0x70) >> 3) + 1);
  GifScreen.BackGround = (UCHAR) buf[5];	/* background color... */

  GifScreen.AspectRatio = (UCHAR) buf[6];


  /* read colormaps */
  if ( BitSet((UCHAR) buf[4], LOCALCOLORMAP) )
  { if ( (rval=ReadColorMap(fd, GifScreen.BitPixel, at, ac, closure))
	 							!= GIF_OK )
    { setGifError("Error reading GIF colormap");
      return rval;
    }
  }
  /* non-square pixels, so what?   */
  if ((GifScreen.AspectRatio != 0) && (GifScreen.AspectRatio != 49))
  {
    setGifError("Non-square pixels in GIF image.  Ignoring that fact ...");
  }
  /* there can be multiple images in a GIF file... uh? */
  /* what the hell do we do with multiple images? */
  /* so, we'll be interested in just the first image, cause we're lazy */

  for (;;)
  { long bufsize;

    /* read a byte; */
    if (!ReadOK(fd, &c, 1))
    {
      setGifError("Unexpected EOF in GIF.  Giving up");
      return GIF_INVALID;
    }
    /* image terminator */
    if (c == ';')
    {
    }
    if (c == '!')
    { if (!ReadOK(fd, &c, 1))
      { setGifError("Error on extension read.  Giving up");
	return GIF_INVALID;
      }
      DoExtension(fd, c);
      continue;
    }
    if (c != ',')
    {
      /* Ignoring c */
      continue;
    }
    /* read image header */
    if (!ReadOK(fd, buf, 9))
    {
      setGifError("Error on dimension read.  Giving up");
      return GIF_INVALID;
    }
    useGlobalColormap = !BitSet((UCHAR) buf[8], LOCALCOLORMAP);

    bitPixel = 1 << (((UCHAR) buf[8] & 0x07) + 1);

    /* let's see if we have enough mem to continue? */

    if ((int) buf[5] > 4)
    {
      /*AfxMessageBox("This GIF file claims to be > 2000 bytes wide!",MB_OK | MB_ICONINFORMATION); */
    }
    if ((int) buf[7] > 4)
    {
      /*AfxMessageBox("This GIF file claims to be > 2000 bytes high!",MB_OK | MB_ICONINFORMATION); */
    }
    w = LM_to_uint((UCHAR) buf[4], (UCHAR) buf[5]);
    h = LM_to_uint((UCHAR) buf[6], (UCHAR) buf[7]);

    if ((w < 0) || (h < 0))
    { setGifError("Negative image dimensions!  Giving up");
      return GIF_INVALID;
    }
    bufsize = (long) w *(long) h;
    bigBuf  = (PIXEL *)pceMalloc(bufsize * sizeof(PIXEL));

    if (bigBuf == NULL)
    {
      setGifError("Out of Memory in GIFRead");
      return GIF_NOMEM;
    }
    if (!useGlobalColormap)
    { if ( (rval=ReadColorMap(fd, bitPixel, at, ac, closure)) != GIF_OK )
      {	setGifError("Error reading GIF colormap. Giving up");
	pceFree(bigBuf);
	return rval;
      }
      /*read image */
      if ( (rval=ReadImage(fd, bigBuf, w, h,
			   BitSet((UCHAR) buf[8], INTERLACE))) != GIF_OK )
      { setGifError("Error reading GIF file.  LocalColorMap. Giving up");
	pceFree(bigBuf);
	return rval;
      }
    } else
    { if ( (rval=ReadImage(fd, bigBuf, w, h,
			   BitSet((UCHAR) buf[8], INTERLACE))) != GIF_OK )
      { setGifError("Error reading GIF file.  GIFScreen Colormap.  Giving up");
	pceFree(bigBuf);
	return rval;
      }
    }
    break;
  }

  *width = w;
  *height = h;
  *data = bigBuf;

  return GIF_OK;
}


static int 
ReadColorMap(IOSTREAM *fd, int number,
	     GIFAllocColorTable at, GIFAllocColor ac, void *closure)
{ int i;
  UCHAR rgb[3];
  int rval;

  if ( (rval=(*at)(number, closure)) != GIF_OK )
    return rval;
  
  for (i = 0; i < number; ++i)
  { if (!ReadOK(fd, rgb, sizeof(rgb)))
    { return GIF_INVALID;
    }
    
    if ( (rval=(*ac)(i, rgb[0], rgb[1], rgb[2], closure)) != GIF_OK )
      return rval;
  }

  return GIF_OK;
}


static int 
DoExtension(IOSTREAM * fd, int label)
{
  static char buf[256];
  char *str;

  switch (label)
  {
  case 0x01:
    str = "Plain Text Ext";
    break;
  case 0xff:
    str = "Appl ext";
    break;
  case 0xfe:
    str = "Comment Ext";
    while (GetDataBlock(fd, (UCHAR *) buf) != 0)
    {
      /*AfxMessageBox(buf, MB_OK | MB_ICONINFORMATION); */
    }
    return FALSE;
    break;
  case 0XF9:
    str = "Graphic Ctrl Ext";
    (void) GetDataBlock(fd, (UCHAR *) buf);
    Gif89.disposal = (buf[0] >> 2) & 0x7;
    Gif89.inputFlag = (buf[0] >> 1) & 0x1;
    Gif89.delayTime = LM_to_uint(buf[1], buf[2]);
    if ((buf[0] & 0x1) != 0)
      Gif89.transparent = buf[3];

    while (GetDataBlock(fd, (UCHAR *) buf) != 0) ;
    return FALSE;
    break;
  default:
    str = buf;
    sprintf(buf, "UNKNOWN (0x%02x)", label);
    break;
  }

  while (GetDataBlock(fd, (UCHAR *) buf) != 0) ;

  return FALSE;
}

static int ZeroDataBlock = FALSE;

static int 
GetDataBlock(IOSTREAM * fd, UCHAR * buf)
{
  UCHAR count;

  if (!ReadOK(fd, &count, 1))
  {
    /*GIFErrorText="Error in GIF DataBlock Size"; */
    return -1;
  }
  ZeroDataBlock = count == 0;

  if ((count != 0) && (!ReadOK(fd, buf, count)))
  {
    /*GIFErrorText="Error reading GIF datablock"; */
    return -1;
  }
  return count;
}

static int 
GetCode(IOSTREAM * fd, int code_size, int flag)
{
  static UCHAR buf[280];
  static int curbit, lastbit, done, last_byte;
  int i, j, ret;
  UCHAR count;

  if (flag)
  {
    curbit = 0;
    lastbit = 0;
    done = FALSE;
    return 0;
  }
  if ((curbit + code_size) >= lastbit)
  {
    if (done)
    {
      if (curbit >= lastbit)
      {
	/*GIFErrorText="Ran off the end of my bits"; */
	return 0;
      }
      return -1;
    }
    buf[0] = buf[last_byte - 2];
    buf[1] = buf[last_byte - 1];

    if ((count = GetDataBlock(fd, &buf[2])) == 0)
      done = TRUE;

    last_byte = 2 + count;

    curbit = (curbit - lastbit) + 16;

    lastbit = (2 + count) * 8;
  }
  ret = 0;
  for (i = curbit, j = 0; j < code_size; ++i, ++j)
    ret |= ((buf[i / 8] & (1 << (i % 8))) != 0) << j;

  curbit += code_size;

  return ret;
}

static int 
LZWReadByte(IOSTREAM * fd, int flag, int input_code_size)
{
  static int fresh = FALSE;
  int code, incode;
  static int code_size, set_code_size;
  static int max_code, max_code_size;
  static int firstcode, oldcode;
  static int clear_code, end_code;

  static unsigned short next[1 << MAX_LZW_BITS];
  static UCHAR vals[1 << MAX_LZW_BITS];
  static UCHAR stack[1 << (MAX_LZW_BITS + 1)];
  static UCHAR *sp;

  register int i;

  if (flag)
  {
    set_code_size = input_code_size;
    code_size = set_code_size + 1;
    clear_code = 1 << set_code_size;
    end_code = clear_code + 1;
    max_code = clear_code + 2;
    max_code_size = 2 * clear_code;

    GetCode(fd, 0, TRUE);

    fresh = TRUE;

    for (i = 0; i < clear_code; ++i)
    {
      next[i] = 0;
      vals[i] = i;
    }

    for (; i < (1 << MAX_LZW_BITS); ++i)
      next[i] = vals[0] = 0;

    sp = stack;

    return 0;
  } else if (fresh)
  {
    fresh = FALSE;
    do
    {
      firstcode = oldcode = GetCode(fd, code_size, FALSE);
    }
    while (firstcode == clear_code);
    return firstcode;
  }
  if (sp > stack)
    return *--sp;

  while ((code = GetCode(fd, code_size, FALSE)) >= 0)
  {
    if (code == clear_code)
    {
      for (i = 0; i < clear_code; ++i)
      {
	next[i] = 0;
	vals[i] = i;
      }
      for (; i < (1 << MAX_LZW_BITS); ++i)
	next[i] = vals[i] = 0;
      code_size = set_code_size + 1;
      max_code_size = 2 * clear_code;
      max_code = clear_code + 2;
      sp = stack;
      firstcode = oldcode = GetCode(fd, code_size, FALSE);
      return firstcode;
    } else if (code == end_code)
    {
      int count;
      UCHAR buf[260];

      if (ZeroDataBlock)
	return -2;

      while ((count = GetDataBlock(fd, buf)) > 0) ;

      if (count != 0)
	/*AfxMessageBox("Missing EOD in GIF data stream (common occurrence)",MB_OK); */
	return -2;
    }
    incode = code;

    if (code >= max_code)
    {
      *sp++ = firstcode;
      code = oldcode;
    }
    while (code >= clear_code)
    {
      *sp++ = vals[code];
      if (code == (int) next[code])
      {
	/*GIFErrorText="Circular table entry, big GIF Error!"; */
	return -1;
      }
      code = next[code];
    }

    *sp++ = firstcode = vals[code];

    if ((code = max_code) < (1 << MAX_LZW_BITS))
    {
      next[code] = oldcode;
      vals[code] = firstcode;
      ++max_code;
      if ((max_code >= max_code_size) &&
	  (max_code_size < (1 << MAX_LZW_BITS)))
      {
	max_code_size *= 2;
	++code_size;
      }
    }
    oldcode = incode;

    if (sp > stack)
      return *--sp;
  }
  return code;
}


static int
ReadImage(IOSTREAM *fd,
	  PIXEL *bigMemBuf,
	  int width, int height,
	  int interlace)
{
  UCHAR c;
  int color;
  int xpos = 0, ypos = 0, pass = 0;
  long curidx;

  if (!ReadOK(fd, &c, 1))
  { return GIF_INVALID;
  }
  if (LZWReadByte(fd, TRUE, c) < 0)
  { return GIF_INVALID;
  }
  while ((color = LZWReadByte(fd, FALSE, c)) >= 0)
  {
    curidx = (long) xpos + (long) ypos *(long) width; /* optimize */

    bigMemBuf[curidx] = color;

    ++xpos;
    if (xpos == width)
    {
      xpos = 0;
      if ( interlace )
      {
	switch (pass)
	{
	case 0:
	case 1:
	  ypos += 8;
	  break;
	case 2:
	  ypos += 4;
	  break;
	case 3:
	  ypos += 2;
	  break;
	}

	if (ypos >= height)
	{
	  ++pass;
	  switch (pass)
	  {
	  case 1:
	    ypos = 4;
	    break;
	  case 2:
	    ypos = 2;
	    break;
	  case 3:
	    ypos = 1;
	    break;
	  default:
	    goto fini;
	  }
	}
      } else
      {
	++ypos;
      }
    }
    if (ypos >= height)
      break;
  }

fini:

  if (LZWReadByte(fd, FALSE, c) >= 0)
  {

  }
  return GIF_OK;
}

