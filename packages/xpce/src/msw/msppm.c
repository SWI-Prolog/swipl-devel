/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1995 University of Amsterdam. All rights reserved.
*/

#include "include.h"

#undef roundup
#define valdigit(d)		((d) - '0')
#define rescale(v, o, n)	((v) * (n) / (o))


		 /*******************************
		 *     ASCII FORMAT PARSING	*
		 *******************************/

static int
getNum(FILE *fd)
{ int c;
  int v;

  for(;;)
  { do
    { c = getc(fd);
    } while(isspace(c));

    if ( isdigit(c) )
    { v = valdigit(c);
      for(;;)
      { c = getc(fd);
	if ( isdigit(c) )
	  v = v*10 + valdigit(c);
	else
	  break;
      }
      if ( !isspace(c) )
	ungetc(c, fd);

      return v;
    }
    if ( c == '#' )
    { do
      { c = getc(fd);
      } while( c != '\n' && c != EOF );
    } else
      return -1;
  }
}


HBITMAP
read_ppm_file(FILE *fd, Name *kind)
{ HBITMAP obm = 0, bm = 0;
  HDC hdc = 0;
  long here = ftell(fd);
  int c;
  int fmt, encoding;
  int width, height, scale=0;

  if ( (c=getc(fd)) != 'P' )
  { ungetc(c, fd);
    return NULL;
  }

  c = getc(fd);
  if ( c < '1' || c > '9' )
    goto errout;
  c -= '0';
  fmt      = ((c - 1) % 3) + 1;
  encoding = c - fmt;

  width = getNum(fd);
  height = getNum(fd);

  if ( fmt == PNM_PBM )
  { *kind = NAME_bitmap;
  } else
  { *kind = NAME_pixmap;		/* or greymap */
    scale = getNum(fd);
  }

  if ( width < 0 || height < 0 || scale < 0 )
    goto errout;

/*hdc = CreateCompatibleDC(NULL);*/
  if ( fmt == PNM_PBM )
  { bm = ZCreateBitmap(width, height, 1, 1, NULL);
  } else
  { hdc = GetDC(NULL);

    bm = ZCreateCompatibleBitmap(hdc, width, height);
    ReleaseDC(NULL, hdc);
    hdc = 0;
  }
  if ( !bm )
  { Cprintf("read_ppm_file(): failed to create image\n");
    goto errout;
  }

  hdc = CreateCompatibleDC(NULL);
  obm = ZSelectObject(hdc, bm);

  switch(encoding)
  { int x, y;
    
    case PNM_ASCII:
    { switch(fmt)
      { case PNM_PBM:
	{ COLORREF on = RGB(0, 0, 0);
	  COLORREF off = RGB(255, 255, 255);

	  for(y=0; y<height; y++)
	  { for(x=0; x<width; x++)
	    { int value = getNum(fd);
  
	      if ( value < 0 || value > 1 )
		goto errout;
  
	      SetPixel(hdc, x, y, value ? on : off);
	    }
	  }
	  break;
	}
	case PNM_PGM:
	{ for(y=0; y<height; y++)
	  { for(x=0; x<width; x++)
	    { int g = getNum(fd);
  
	      if ( g < 0 || g > scale )
		goto errout;
	      if ( scale != 255 )
		g = rescale(g, scale, 255);
  
	      SetPixel(hdc, x, y, RGB(g, g, g));
	    }
	  }
	      
	  break;
	}
	case PNM_PPM:
	{ for(y=0; y<height; y++)
	  { for(x=0; x<width; x++)
	    { int r = getNum(fd);
	      int g = getNum(fd);
	      int b = getNum(fd);
  
	      if ( r < 0 || r > scale ||
		   g < 0 || g > scale ||
		   b < 0 || b > scale )
		goto errout;
  
	      if ( scale != 255 )
	      { r = rescale(r, scale, 255);
		g = rescale(g, scale, 255);
		b = rescale(b, scale, 255);
	      }
  
	      SetPixel(hdc, x, y, RGB(r, g, b));
	    }
	  }
  
	  break;
	}
	break;
      }
      break;
    }
    case PNM_RAWBITS:
    { switch(fmt)
      { case PNM_PBM:
	{ int byte = 0;
	  int bit = 0;
	  COLORREF on = RGB(0, 0, 0);
	  COLORREF off = RGB(255, 255, 255);
	  
	  for(y=0; y<height; y++)
	  { for(x=0; x<width; x++)
	    { if ( !bit )
	      { byte = getc(fd);
		bit = 8;
	      }
  
	      bit--;
	      SetPixel(hdc, x, y, (byte & (1<<bit)) ? on : off);
	    }
	    bit = 0;
	  }
	  break;
	}
	case PNM_PGM:
	{ for(y=0; y<height; y++)
	  { for(x=0; x<width; x++)
	    { unsigned int g;
  
	      if ( feof(fd) || (g=getc(fd)) > scale )
		goto errout;
	      if ( scale != 255 )
		g = rescale(g, scale, 255);
  
	      SetPixel(hdc, x, y, RGB(g, g, g));
	    }
	  }
	      
	  break;
	}
	case PNM_PPM:
	{ for(y=0; y<height; y++)
	  { for(x=0; x<width; x++)
	    { unsigned int r, g, b;

	      if ( feof(fd) ||
		   (r=getc(fd)) > scale ||
		   (g=getc(fd)) > scale ||
		   (b=getc(fd)) > scale )
		goto errout;
  
	      if ( scale != 255 )
	      { r = rescale(r, scale, 255);
		g = rescale(g, scale, 255);
		b = rescale(b, scale, 255);
	      }
  
	      SetPixel(hdc, x, y, RGB(r, g, b));
	    }
	  }
  
	  break;
	}
	break;
      }
      break;
    }
    case PNM_RUNLEN:
    { int rlen = 0;
      COLORREF cpixel = RGB(0,0,0);

      switch(fmt)
      { case PNM_PGM:
	{ DEBUG(NAME_pnm, Cprintf("Reading runlength encoded graymap\n"));

	  for(y=0; y<height; y++)
	  { for(x=0; x<width; x++)
	    { if ( rlen-- > 0 )
	      { SetPixel(hdc, x, y, cpixel);
	      } else
	      { unsigned int g;
  
		if ( (g=getc(fd)) > scale ||
		     (rlen = getc(fd)) == EOF )
		  goto errout;
		rlen &= 0xff;
		if ( scale != 255 )
		  g = rescale(g, scale, 255);
  
		cpixel = RGB(g, g, g);
		SetPixel(hdc, x, y, cpixel);
		rlen--;
	      }
	    }
	  }
	      
	  break;
	}
	case PNM_PPM:
	{ for(y=0; y<height; y++)
	  { for(x=0; x<width; x++)
	    { if ( rlen-- > 0 )
	      { SetPixel(hdc, x, y, cpixel);
	      } else
	      { unsigned int r, g, b;
  
		if ( (r=getc(fd)) > scale ||
		     (g=getc(fd)) > scale ||
		     (b=getc(fd)) > scale ||
		     (rlen = getc(fd)) == EOF )
		  goto errout;

		rlen &= 0xff;
		if ( scale != 255 )
		{ r = rescale(r, scale, 255);
		  g = rescale(g, scale, 255);
		  b = rescale(b, scale, 255);
		}
  
		cpixel = RGB(r, g, b);
  
		SetPixel(hdc, x, y, cpixel);
		rlen--;
	      }
	    }
	  }
  
	  break;
	}
      }
    }
  }

  DEBUG(NAME_ppm, Cprintf("PNM: Converted %dx%d image\n", width, height));

  if ( hdc )
  { if ( obm )
      ZSelectObject(hdc, obm);
    DeleteDC(hdc);
  }

  return bm;

errout:
  Cprintf("PNM: Format error, index = %d\n", ftell(fd));

  if ( hdc )
  { if ( obm )
      ZSelectObject(hdc, obm);
    DeleteDC(hdc);
  }

  fseek(fd, here, SEEK_SET);
  return NULL;
}


		 /*******************************
		 *	     WRITING		*
		 *******************************/


static int file_col;

static int
putNum(int n, FILE *fd)
{ if ( file_col != 0 && putc(' ', fd) == EOF )
    return -1;

  do
  { if ( putc(n % 10 + '0', fd) == EOF )
	return -1;
    file_col++;
    n /= 10;
  } while( n > 0 );

  if ( file_col >= 70 )
  { if ( putc('\n', fd) == EOF )
      return -1;
    file_col = 0;
  }

  return 0;
}


#undef BRIGHT
#define BRIGHT 255
#undef NOPIXEL
#define NOPIXEL (COLORREF) -1;

int
write_pnm_file(FILE *fd, HBITMAP bm, int scale, int fmt, int encode)
{ BITMAP bitmap;
  int width, height, depth;
  int x, y;
  HDC hdc;
  HBITMAP obm;

  if ( !GetObject(bm, sizeof(BITMAP), &bitmap) )
  { Cprintf("write_pnm_file(): GetObject() failed\n");
    return -1;
  }

  width  = bitmap.bmWidth;
  height = bitmap.bmHeight;
  depth  = bitmap.bmPlanes * bitmap.bmBitsPixel;

  if ( !scale )
    scale = 255;
  if ( !fmt && depth == 1 )
    fmt = PNM_PBM;

  hdc = CreateCompatibleDC(NULL);
  obm = ZSelectObject(hdc, bm);

  if ( fmt != PNM_PBM )
  { if ( !fmt )
    { for(y=0; y<height && !fmt; y++)
      { for(x=0; x<width; x++)
	{ COLORREF c = GetPixel(hdc, x, y);
	  BYTE r = GetRValue(c);
	  BYTE g = GetGValue(c);
	  BYTE b = GetBValue(c);
	  
	  if ( r != g || r != b )
	  { fmt = PNM_PPM;
	    break;
	  }
	}
      }
      if ( !fmt )
	fmt = PNM_PGM;
    }
  } else if ( encode == PNM_RUNLEN )	/* no use to runlen encode a bitmap */
    encode = PNM_RAWBITS;

  fprintf(fd, "P%c\n", fmt + encode + '0');
  fprintf(fd, "# Creator: XPCE version %s\n",
	  strName(get(PCE, NAME_version, 0)));
  if ( fmt != PNM_PBM )
  { fprintf(fd, "%d %d\n", width, height);
    fprintf(fd, "%d\n", scale);
  } else
    fprintf(fd, "%d %d\n", width, height);

  file_col = 0;
    
  switch(encode)
  { case PNM_ASCII:
    { switch(fmt)
      { case PNM_PBM:
	{ for(y=0; y<height; y++)
	  { for(x=0; x<width; x++)
	    { if ( putNum(GetPixel(hdc, x, y) ? 0 : 1, fd) < 0 )
		return -1;
	    }
	  }
	  break;
	}
	case PNM_PGM:
	{ for(y=0; y<height; y++)
	  { for(x=0; x<width; x++)
	    { COLORREF c = GetPixel(hdc, x, y);
	      unsigned int r = GetRValue(c);
  
	      if ( scale != 255 )
		r = rescale(r, BRIGHT, scale);
  
	      if ( putNum(r, fd) < 0 )
		return -1;
	    }
	  }
	  break;
	}
	case PNM_PPM:
	{ for(y=0; y<height; y++)
	  { for(x=0; x<width; x++)
	    { COLORREF c = GetPixel(hdc, x, y);
	      unsigned int r = GetRValue(c);
	      unsigned int g = GetGValue(c);
	      unsigned int b = GetBValue(c);
	      
	      if ( scale != 255 )
	      { r = rescale(r, BRIGHT, scale);
		g = rescale(r, BRIGHT, scale);
		b = rescale(r, BRIGHT, scale);
	      }
  
	      if ( putNum(r, fd) < 0 ||
		   putNum(g, fd) < 0 ||
		   putNum(b, fd) < 0 )
		return -1;
	    }
	  }
	  break;
	}
      }
      if ( file_col && putc('\n', fd) == EOF )
	return -1;
      file_col = 0;
    }
    case PNM_RAWBITS:
    { switch(fmt)
      { case PNM_PBM:
	{ int byte = 0;
	  int bit = 7;
  
	  for(y=0; y<height; y++)
	  { for(x=0; x<width; x++)
	    { if ( !GetPixel(hdc, x, y) )
		byte |= 1<<bit;
	      if ( bit-- == 0 )
	      { if ( putc(byte, fd) == EOF )
		  return -1;
		bit = 7;
		byte = 0;
	      }
	    }
	    if ( bit != 7 )		/* flush the scanline */
	    { if ( putc(byte, fd) == EOF )
		return -1;
	      bit = 7;
	      byte = 0;
	    }
	  }
  
	  if ( bit != 7 )
	  { if ( putc(byte, fd) == EOF )
	      return -1;
	  }
	  break;
	}
	case PNM_PGM:
	{ for(y=0; y<height; y++)
	  { for(x=0; x<width; x++)
	    { COLORREF c = GetPixel(hdc, x, y);
	      unsigned int r = GetRValue(c);
  
	      if ( scale != 255 )
		r = rescale(r, BRIGHT, scale);
  
	      if ( putc(r, fd) == EOF )
		return -1;
	    }
	  }
	  break;
	}
	case PNM_PPM:
	{ for(y=0; y<height; y++)
	  { for(x=0; x<width; x++)
	    { COLORREF c = GetPixel(hdc, x, y);
	      unsigned int r = GetRValue(c);
	      unsigned int g = GetGValue(c);
	      unsigned int b = GetBValue(c);
	      
	      if ( scale != 255 )
	      { r = rescale(r, BRIGHT, scale);
		g = rescale(r, BRIGHT, scale);
		b = rescale(r, BRIGHT, scale);
	      }
  
	      if ( putc(r, fd) == EOF ||
		   putc(g, fd) == EOF ||
		   putc(b, fd) == EOF )
		return -1;
	    }
	  }
  
	  break;
	}
      }
    }
    case PNM_RUNLEN:
    { int rlen=-1;
      COLORREF cpixel = NOPIXEL;

      switch(fmt)
      { case PNM_PGM:
	{ for(y=0; y<height; y++)
	  { for(x=0; x<width; x++)
	    { COLORREF pixel = GetPixel(hdc, x, y);

	      if ( pixel == cpixel && rlen < 255 )
		rlen++;
	      else
	      { int r;

		if ( rlen > 0 && putc(rlen, fd) == EOF )
		  return -1;
		cpixel = pixel;
		rlen = 1;
		r = GetRValue(pixel);
		if ( scale != 255 )
		  r = rescale(r, BRIGHT, scale);
  		if ( putc(r, fd) == EOF )
		  return -1;
	      }
	    }
	  }
	  if ( putc(rlen, fd) == EOF )
	    return -1;

	  break;
	}
	case PNM_PPM:
	{ for(y=0; y<height; y++)
	  { for(x=0; x<width; x++)
	    { COLORREF pixel = GetPixel(hdc, x, y);

	      if ( pixel == cpixel && rlen < 255 )
		rlen++;
	      else
	      { unsigned int r, g, b;
  
		if ( rlen > 0 && putc(rlen, fd) == EOF )
		  return -1;
		cpixel = pixel;
		rlen = 1;

		r = GetRValue(pixel);
		g = GetGValue(pixel);
		b = GetBValue(pixel);

		if ( scale != 255 )
		{ r = rescale(r, BRIGHT, scale);
		  g = rescale(r, BRIGHT, scale);
		  b = rescale(r, BRIGHT, scale);
		}
  
		if ( putc(r, fd) == EOF ||
		     putc(g, fd) == EOF ||
		     putc(b, fd) == EOF )
		  return -1;
	      }
	    }
	  }
	  if ( putc(rlen, fd) == EOF )
	    return -1;
  
	  break;
	}
      }
    }
  }

  ZSelectObject(hdc, obm);
  DeleteDC(hdc);

  return 0;
}
