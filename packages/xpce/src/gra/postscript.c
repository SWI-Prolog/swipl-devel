/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>
#include <h/unix.h>

static void	initOutput(void);
static void	closeOutput(void);
static void	ps_put_string(String);
static int	postscriptImage(Image, Int);
static int	header(Any, Area, Bool);
static int	macros(void);
static int	footer(void);
static status	fill(Any, Name);
static void	ps_colour(Colour c, int grey);

struct
{ Colour colour;			/* current colour */
  struct
  { Name	family;			/* family of current PostScript font */
    Int		points;			/* points in current PostScript font */
  } currentFont;
} psstatus;

Chain documentFonts = NIL;	   /* chain holding fonts used in document */
char * PostScript;		   /* string holding the result */ 
FILE * logFd;			   /* write here debugging stuff */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Create a postscript image of a  graphical  and  make  it  fit  into  the
specified  are.   If  `ls'  is  ON  the image will be rotated 90 degrees
resulting in a landscaped image.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

StringObj
getPostscriptObject(Any obj, Bool ls, Area a)
{ StringObj result;

/*  logFd = fopen("PostScript", "w");	*/

  if ( isNil(documentFonts) )
  { documentFonts = globalObject(NAME_DocumentFonts, ClassChain, 0);
  } else
    clearChain(documentFonts);

  psstatus.colour             = BLACK_COLOUR;
  psstatus.currentFont.family = NIL;

  initOutput();
  if ( hasSendMethodObject(obj, NAME_compute) )
    send(obj, NAME_compute, 0);
  if ( header(obj, a, ls) == FAIL )
  { closeOutput();
    fail;
  }
  send(obj, NAME_Postscript, 0);
  footer();

  ps_put_char('\0');
  result = CtoString(PostScript);
  closeOutput();

/*  fclose(logFd); */

  answer(result);
}

		/********************************
		*       OUTPUT HANDLING         *
		*********************************/

int outputSize;			/* current size of output */
int outputPointer;		/* current index in output buffer */

static void
initOutput(void)
{ outputSize = 1024;		/* initial allocation */
  outputPointer = 0;
  PostScript = pceMalloc(outputSize);
}

static void
closeOutput(void)
{ pceFree(PostScript);
  PostScript = NULL;
}

void
ps_put_char(int c)
{ if ( outputPointer == outputSize )
  { outputSize *= 2;
    PostScript = pceRealloc(PostScript, outputSize);
  }

  PostScript[outputPointer++] = c;
}


static void
putString(char *s)
{ while( *s )
    ps_put_char(*s++);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Output() is a special version of  printf.   The  following  options  are
recognised:

  Option   Argument	Description
    ~~        -		Output a tilde
    ~s	    char *	Output literal string
    ~a	    String	Output String as PostScript string
    ~c	    Point	Output <-x and <-y of the point
    ~d	    Int		Output an integer
    ~D	    int		Output an integer
    ~f	    float	Output a float (passed as integer * 100)
    ~m	    Graphical	Move to XY of graphical
    ~t	    Figure	Translate to XY of figure
    ~T	    Graphical	Set texture to texture of graphical
    ~C	    Graphical	Output colour of the graphical
    ~N	    Name 	print text of name
    ~S	    StringObj	Output text of StringObj with postscript escapes
    ~O	    Object	Output comment to start O
    ~P	    Int, Image	Output pattern of image with depth Int
    ~p	    Graphical   Output pen of graphical
    ~x	    Graphical	Output X of graphical
    ~y	    Graphical	Output Y of graphical
    ~w	    Graphical	Output W of graphical
    ~h	    Graphical	Output H of graphical
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
_output(char *fm, va_list args)
{ char tmp[LINESIZE];

  for( ;*fm ; fm++)
  { switch(*fm)
    { case '\\':
	switch(*++fm)				/* \ escapes */
	{ case 'n':	ps_put_char('\n');
	  		continue;
	  case '\\':	ps_put_char('\\');
			continue;
	  case '\0':	ps_put_char('\\');
			return;
	}
      case '~':
	switch(*++fm)				/* ~ escapes */
	{ case '~':	ps_put_char('~');
    			continue;
	  case '\0':	ps_put_char('~');
			return;
	  case 's':	putString(va_arg(args, char *));
			continue;
	  case 'd':	sprintf(tmp, "%ld", valInt(va_arg(args, Int)));
			putString(tmp);
			continue;
	  case 'D':	sprintf(tmp, "%d", va_arg(args, int));
			putString(tmp);
			continue;
	  case 'f':	sprintf(tmp, "%.2f", va_arg(args, double) );
			putString(tmp);
			continue;
	  case 'm':   { Graphical gr = va_arg(args, Graphical);
			sprintf(tmp, "%ld %ld moveto", valInt(gr->area->x),
						     valInt(gr->area->y));
			putString(tmp);
			continue;
		      }
	  case 'c':   { Point p = va_arg(args, Point);
			sprintf(tmp, "%ld %ld", valInt(p->x), valInt(p->y));
			putString(tmp);
			continue;
		      }
	  case 't':   { Figure f = va_arg(args, Figure);
			sprintf(tmp, "%ld %ld translate",
				valInt(f->offset->x), valInt(f->offset->y));
			putString(tmp);
			continue;
		      }
	  case 'p':   { Graphical gr = va_arg(args, Graphical);
			sprintf(tmp, "%ld", valInt(gr->pen));
			putString(tmp);
			continue;
		      }

	  case 'T':   { Name texture = get(va_arg(args, Any), NAME_texture, 0);

			if ( texture == NAME_none )
			  putString("nodash");
			else if ( instanceOfObject(texture, ClassCharArray) )
			{ CharArray ca = (CharArray) texture;
			  ca = getDowncaseCharArray(ca);
			  putString(strName(ca));
			}

			continue;
		      }
	  case 'C':   { Graphical gr = va_arg(args, Graphical);
			Colour c = get(gr, NAME_colour, 0);

			ps_colour(c, 100);

			continue;
		      }
	  case 'a':	ps_put_string(va_arg(args, String));
			continue;
	  case 'N':	putString(strName(va_arg(args, Name)));
			continue;
	  case 'S':   { StringObj s = va_arg(args, StringObj);
			ps_put_string(&s->data);
			continue;
		      }
	  case 'O':   { Any obj = va_arg(args, Any);
			char *s = pp(obj);

			putString(s);

			continue;
		      }
	  case 'P':   { Int depth = va_arg(args, Int);
			Image image = va_arg(args, Image);

			postscriptImage(image, depth);
			continue;
		      }
	  case 'x':   { Graphical gr = va_arg(args, Graphical);

			sprintf(tmp, "%ld", valInt(gr->area->x));
			putString(tmp);
			continue;
		      }
	  case 'y':   { Graphical gr = va_arg(args, Graphical);

			sprintf(tmp, "%ld", valInt(gr->area->y));
			putString(tmp);
			continue;
		      }
	  case 'w':   { Graphical gr = va_arg(args, Graphical);

			sprintf(tmp, "%ld", valInt(gr->area->w));
			putString(tmp);
			continue;
		      }
	  case 'h':   { Graphical gr = va_arg(args, Graphical);

			sprintf(tmp, "%ld", valInt(gr->area->h));
			putString(tmp);
			continue;
		      }
	  default:    { errorPce(NIL, NAME_unknownEscape,
				 CtoName("~"), toInt(*fm));
			ps_put_char('~'), ps_put_char(*fm);
			continue;
		      }
	}
      default:
      { ps_put_char(*fm);
	continue;
      }
    }
  }
}


void
ps_output(char *fm, ...)
{ va_list args;

  va_start(args, fm);
  _output(fm, args);
  va_end(args);
}


static void
ps_put_string(String s)
{ wchar c;
  int i, size = s->size;

  ps_put_char('(');

  for(i=0; i<size; i++)
  { switch( (c=str_fetch(s, i)) )
    { case '\b':	putString("\\b");	break;
      case '\t':	putString("\\t");	break;
      case '\n':	putString("\\n");	break;
      case '\r':	putString("\\r");	break;
      case '\\':	putString("\\\\");	break;
      case '(':		putString("\\(");	break;
      case ')':		putString("\\)");	break;
      default:		if ( c >= ' ' && c <= '~' )
			{ ps_put_char(c);
			} else
			{ char tmp[4];
			  sprintf(tmp, "\\%03o", c);
			  putString(tmp);
			}
    }
  }

  ps_put_char(')');
}


status
ps_font(FontObj font)
{ Name family = (Name) get(font, NAME_postscriptFont, 0);
  Int  points = (Int)  get(font, NAME_postscriptSize, 0);

  if ( family == FAIL ) family = CtoName("Courier");
  if ( points == FAIL ) points = font->points;

  if ( psstatus.currentFont.family == family &&
       psstatus.currentFont.points == points )
    succeed;

  if ( memberChain(documentFonts, family) != SUCCEED )
    appendChain(documentFonts, family);

  ps_output("/~N findfont ~d scalefont setfont\n", family, points);

  succeed;
}


static void
ps_colour(Colour c, int grey)
{ if ( notDefault(c) )
  { float r = (float) valInt(getRedColour(c))   / (float) valInt(BRIGHT);
    float g = (float) valInt(getGreenColour(c)) / (float) valInt(BRIGHT);
    float b = (float) valInt(getBlueColour(c))  / (float) valInt(BRIGHT);

    if ( grey != 100 )
    { r = 1 - (1-r) * grey / 100.0;
      g = 1 - (1-g) * grey / 100.0;
      b = 1 - (1-b) * grey / 100.0;
    }

    ps_output("~f ~f ~f setrgbcolor ", r, g, b);
  }
}


static status
fill(Any gr, Name sel)
{ Image pattern = get(gr, sel, 0);
  Int greyLevel;

  if ( instanceOfObject(pattern, ClassColour) )
  { Colour c = (Colour) pattern;

    ps_output("gsave ");
    ps_colour(c, 100);
    ps_output(" fill grestore\n");
  } else if ( instanceOfObject(pattern, ClassImage) )
  { if ( hasGetMethodObject(pattern, NAME_postscriptGrey) &&
	 (greyLevel = (Int) get(pattern, NAME_postscriptGrey, 0)) )
    { Colour c = get(gr, NAME_displayColour, 0);

      if ( c )
      { ps_output("gsave ");
	ps_colour(c, valInt(greyLevel));
	ps_output(" fill grestore\n");
      } else
      { ps_output("gsave ~f setgray fill grestore\n",
		  (float) (100 - valInt(greyLevel)) / 100.0 );
      }
    } else
    { ps_output("~x ~y ~w ~h ~d ~d \n<~P>\nfillwithmask\n",
		gr, gr, gr, gr,
		pattern->size->w, pattern->size->h, ONE, pattern);
    }
  }
  
  succeed;
}


#define putByte(b) { ps_put_char(print[(b >> 4) & 0xf]); \
		     ps_put_char(print[b & 0xf]); \
		     if ( (++bytes % 32) == 0 ) ps_put_char('\n'); \
		     bits = 8; c = 0; \
		   }



status
postscriptDrawable(int ox, int oy, int w, int h)
{ static char print[] = {'0', '1', '2', '3', '4', '5', '6', '7',
			 '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'};
  int x, y;
  int bits, bytes;
  int c;

  DEBUG(NAME_postscript,
	Cprintf("postscriptDrawable(%d %d %d %d) ...", ox, oy, w, h));

/*ps_output("1 greymap\n\n");*/

  for(bytes = y = c = 0, bits = 8; y < h; y++)
  { for(x=0; x < w; x++)
    { c |= ((TRUE-(r_get_mono_pixel(x+ox, y+oy))) << --bits);
      if ( bits == 0 )
        putByte(c);
    }
    if ( bits != 8 )
      putByte(c);
  }

  DEBUG(NAME_postscript, Cprintf("ok\n"));

  succeed;
}


static int
postscriptImage(Image image, Int depth)
{ ws_postscript_image(image, depth);

  succeed;
}


		/********************************
		*        HEADER & FOOTER        *
		*********************************/

static int
header(Any gr, Area area, Bool ls)
{ int x, y, w, h;
  int xgr, ygr, wgr, hgr;
  int paperH, paperW;
  float scale;

  if ( isDefault(ls) )
    ls = OFF;

  if ( isDefault(area) )
  { x = 70, y = 70, w = 500, h = 700;
  } else
  { x = valInt(area->x);
    y = valInt(area->y);
    w = valInt(area->w);
    h = valInt(area->h);
  }

  ps_output("%!PS-Adobe-3.0 EPSF-3.0\n");
  ps_output("%%Creator: PCE ~N\n", get(PCE, NAME_version, 0));
  ps_output("%%CreationDate: ~S\n", get(PCE, NAME_date, 0));
  ps_output("%%Pages: 1\n");
  ps_output("%%DocumentFonts: (atend)\n");

  { Area bb = get(gr, NAME_boundingBox, 0);
    xgr = valInt(bb->x);
    ygr = valInt(bb->y);
    wgr = valInt(bb->w);
    hgr = valInt(bb->h);
  }

  if ( ls == ON )
    paperH = wgr, paperW = hgr;
  else
    paperW = wgr, paperH = hgr;

  if ( wgr <= w && hgr <= h )
  { scale = 1.0;
  } else
  { scale = min( (float)w / (float)wgr, (float)h/(float)hgr );
    paperW = (int)((float)paperW * scale);
    paperH = (int)((float)paperH * scale);
  }

  if ( ls == ON )
  { ps_output("%%BoundingBox: ~D ~D ~D ~D\n", x+w-paperW, y, x+w, y+paperH);
  } else
  { ps_output("%%BoundingBox: ~D ~D ~D ~D\n", x, y, x+paperW, y+paperH);
  }

  ps_output("%%Object: ~O\n", gr);
  ps_output("%%EndComments\n\n");
  
  ps_output("gsave\n\n");
  TRY( macros() );

  if ( ls == ON )
    ps_output("~D ~D translate 90 rotate\n", x + w, y);
  else
    ps_output("~D ~D translate\n", x, y);

  ps_output("~f ~f scale\n", scale, -scale);
  ps_output("~D ~D translate\n", -xgr, -hgr - ygr);
  ps_output("%%EndProlog\n");
  ps_output("%%Page 0 1\n");

  succeed;
}    

static int
macros(void)
{ FileObj f;
  FILE *fd;

  if ( !(f = get(PCE, NAME_postscriptHeader, 0)) ||
       !(f = checkType(f, nameToType(NAME_file), NIL)) )
    return errorPce(PCE, NAME_noPostScriptHeader);

  TRY( send(f, NAME_open, NAME_read, 0) );
  fd = f->fd;

  for(;;)
  { int c = getc(fd);

#ifdef __WATCOMC__
    if ( c == EOF )
#else
    if ( feof(fd) )
#endif
      break;

    ps_put_char(c);
  }

  return send(f, NAME_close, 0);
}

static int
footer(void)
{ Cell cell;

  ps_output("\n%%Trailer\n");
  ps_output("grestore\n");
/*  ps_output("/pce restore\n"); */
  ps_output("%%DocumentFonts:");
  
  for_cell(cell, documentFonts)
    ps_output(" ~N", cell->value);

  ps_output("\n");
  succeed;
}


		/********************************
		*          POSTSCRIPT           *
		*********************************/

status
postscriptGraphical(Any obj)
{ ps_output("\n%%Object: ~O\n", obj);

  return send(obj, NAME_DrawPostScript, 0);
}


status
drawPostScriptDevice(Device dev)
{ Cell cell;

  ps_output("gsave ~t ~C\n", dev, dev);

  for_cell(cell, dev->graphicals)
  { Graphical gr = cell->value;

    if ( gr->displayed == ON )
      send(gr, NAME_Postscript, 0);
  }

  ps_output("grestore\n");

  succeed;
}


status
drawPostScriptFigure(Figure f)
{ if ( f->pen != ZERO || notNil(f->background) )
  { ps_output("gsave ~C ~T ~p ~x ~y ~w ~h 0 boxpath\n", f, f, f, f, f, f, f);
    fill(f, NAME_background);
    ps_output("draw grestore\n");
  }

  return drawPostScriptDevice((Device) f);
}


status
drawPostScriptBox(Box b)
{ int rmax = min(valInt(b->area->w), valInt(b->area->h))/2;
  Int r = (valInt(b->radius) > rmax ? toInt(rmax) : b->radius);

  if ( b->shadow != ZERO )
  { Area a = b->area;
    Int s = b->shadow;

    ps_output("gsave nodash 0 ~d ~d ~d ~d ~d boxpath\n",
	      add(a->x, s), add(a->y, s), sub(a->w, s), sub(a->h, s),
	      r);
    ps_output("0.0 setgray fill grestore\n");
    ps_output("gsave ~C ~T ~p ~x ~y ~d ~d ~d boxpath\n", b, b,
	      b, b, b, sub(a->w, s), sub(a->h, s), r);
    if ( notNil(b->fill_pattern) )
      fill(b, NAME_fillPattern);
    else
      ps_output("gsave 1.0 setgray fill grestore\n");
    ps_output("draw grestore\n");
  } else
  { ps_output("gsave ~C ~T ~p ~x ~y ~w ~h ~d boxpath\n",
	      b, b, b, b, b, b, b, r);
    fill(b, NAME_fillPattern);
    ps_output("draw grestore\n");
  }

  succeed;
}


status
drawPostScriptCircle(Circle c)
{ ps_output("gsave ~C ~T ~p ~x ~y ~d circlepath\n",
	    c, c, c, c, c, div(c->area->w, TWO));
  fill(c, NAME_fillPattern);
  ps_output("draw grestore\n");

  succeed;
}


status
drawPostScriptEllipse(Ellipse e)
{ if ( e->shadow != ZERO )
  { Area a = e->area;
    Int s = e->shadow;

    ps_output("gsave nodash 0 ~d ~d ~d ~d ellipsepath\n",
	      add(a->x, s), add(a->y, s), sub(a->w, s), sub(a->h, s));
    ps_output("0.0 setgray fill grestore\n");
    ps_output("gsave ~C ~T ~p ~x ~y ~d ~d ellipsepath\n",
	      e, e, e, e, e, sub(a->w, s), sub(a->h, s));
    if ( notNil(e->fill_pattern) )
      fill(e, NAME_fillPattern);
    else
      ps_output("gsave 1.0 setgray fill grestore\n");
    ps_output("draw grestore\n");
  } else
  { ps_output("gsave ~C ~T ~p ~x ~y ~w ~h ellipsepath\n", e, e, e, e, e, e, e);
    fill(e, NAME_fillPattern);
    ps_output("draw grestore\n");
  }
  
  succeed;
}


status
drawPostScriptPath(Path p)
{ if ( valInt(getSizeChain(p->points)) >= 2 )
  { Chain points = (p->kind == NAME_smooth ? p->interpolation : p->points);

    if ( p->kind == NAME_smooth )	/* Smooth path */
    { Cell cell;
      int i = -1;			/* skip first */
      int px, py, x0, y0;
      Point pt = getHeadChain(points);

      x0 = valInt(pt->x);
      y0 = valInt(pt->y);

      if ( p->closed == ON )
      { Point end = getTailChain(points);

	px = valInt(end->x);
	py = valInt(end->y);
      } else
      { Point pn = getNth1Chain(points, TWO);

	px = x0 - (valInt(pn->x) - x0);
	py = y0 - (valInt(pn->y) - y0);
      }

      ps_output("gsave ~d ~d translate ~C ~T ~p ~c startpath\n",
		p->offset->x, p->offset->y, p, p, p, pt);

      for_cell(cell, points)
      { if ( i >= 0 )
	{ Point pt = cell->value;
	  int x3 = valInt(pt->x);
	  int y3 = valInt(pt->y);
	  int nx, ny;
	  float x1, y1, x2, y2;
	
	  if ( notNil(cell->next) )
	  { Point np = cell->next->value;
	    nx = valInt(np->x);
	    ny = valInt(np->y);
	  } else if ( p->closed == ON )
	  { Point np = getHeadChain(points);
	    nx = valInt(np->x);
	    ny = valInt(np->y);
	  } else
	  { nx = x3 + x3 - x0;
	    ny = y3 + y3 -y0;
	  }

	  x1 = (float) x0 + (float) ((x0-px) + (x3-x0) + 4) / 8.0;
	  y1 = (float) y0 + (float) ((y0-py) + (y3-y0) + 4) / 8.0;

	  x2 = (float) x3 - (float) ((nx-x3) + (x3-x0) + 4) / 8.0;
	  y2 = (float) y3 - (float) ((ny-y3) + (y3-y0) + 4) / 8.0;

	  ps_output("~f ~f ~f ~f ~D ~D curveto\n", x1, y1, x2, y2, x3, y3);

	  px = x0; py = y0;
	  x0 = x3; y0 = y3;
	}

	i++;
      }

      if ( notNil(p->fill_pattern) || p->closed == ON )
	ps_output(" closepath");
      ps_output("\n");

      fill(p, NAME_fillPattern);
      ps_output("draw\n");
    } else				/* poly-path */
    { Cell cell;
      int i = -1;			/* skip first */

      ps_output("gsave ~d ~d translate ~C ~T ~p ~c startpath\n",
		p->offset->x, p->offset->y,
		p, p, p, getHeadChain(points));
      for_cell(cell, p->points)
      { if ( i >= 0 )
	{ ps_output(" ~c lineto", cell->value);
	  if ( i % 6 == 0 )
	    ps_output("\n");
	}
	
	i++;
      }
    
      if ( notNil(p->fill_pattern) || p->closed == ON )
	ps_output(" closepath");
      ps_output("\n");
      
      fill(p, NAME_fillPattern);
      ps_output("draw\n");
    }

    if ( adjustFirstArrowPath(p) )
      postscriptGraphical(p->first_arrow);
    if ( adjustSecondArrowPath(p) )
      postscriptGraphical(p->second_arrow);

    ps_output("grestore\n");
  }

  succeed;
}


status
drawPostScriptLine(Line ln)
{ int x1 = valInt(ln->start_x);
  int x2 = valInt(ln->end_x);
  int y1 = valInt(ln->start_y);
  int y2 = valInt(ln->end_y);

  ps_output("gsave ~C\n", ln);
  if ( ln->pen != ZERO )
    ps_output("~T ~p ~D ~D ~D ~D linepath draw\n",
	      ln, ln, x1, y1, x2-x1, y2-y1);

  if ( adjustFirstArrowLine(ln) )
    postscriptGraphical(ln->first_arrow);
  if ( adjustSecondArrowLine(ln) )
    postscriptGraphical(ln->second_arrow);

  ps_output("grestore\n");

  succeed;
}


status
drawPostScriptArrow(Arrow a)
{ ps_output("gsave ~C ~T ~p pen ", a, a, a);
  ps_output("newpath ~d ~d moveto ~d ~d lineto ~d ~d lineto",
	    a->left->x, a->left->y,
	    a->tip->x, a->tip->y,
	    a->right->x, a->right->y);

  if ( a->style == NAME_closed || notNil(a->fill_pattern) )
    ps_output(" closepath ");

  if ( notNil(a->fill_pattern) )
    fill(a, NAME_fillPattern);
  if ( a->pen != ZERO )
    ps_output(" ~T draw\n", a);

  ps_output(" grestore\n");

  succeed;
}


status
drawPostScriptArc(Arc a)
{ ps_output("gsave ~C ~T ~p ~D ~d ~d ~d ~d ~f ~f arcpath\n",
	    a, a, a,
	    a->close == NAME_none ? 0 : a->close == NAME_chord ? 1 : 2,
	    a->position->x, a->position->y,
	    a->size->w, a->size->h,
	    valReal(a->start_angle), valReal(a->size_angle));

  fill(a, NAME_fillPattern);
  ps_output("draw\n");

  if ( notNil(a->first_arrow) ||  notNil(a->second_arrow) )
  { int sx, sy, cx, cy, ex, ey;

    points_arc(a, &sx, &sy, &ex, &ey);
    cx = valInt(a->position->x);
    cy = valInt(a->position->y);

    if (notNil(a->first_arrow))		/* should be merged from arc.c */
    { Any av[4];
  
      av[0] = toInt(sx);
      av[1] = toInt(sy);
  
      if ( valReal(a->size_angle) >= 0.0 )
      { av[2] = toInt(sx+(sy-cy));
	av[3] = toInt(sy-(sx-cx));
      } else
      { av[2] = toInt(sx-(sy-cy));
	av[3] = toInt(sy+(sx-cx));
      }
	
      if ( qadSendv(a->first_arrow, NAME_points, 4, av) )
      { ComputeGraphical(a->first_arrow);
	postscriptGraphical(a->first_arrow);
      }
    }
    if (notNil(a->second_arrow))
    { Any av[4];
  
      av[0] = toInt(ex);
      av[1] = toInt(ey);
  
      if ( valReal(a->size_angle) >= 0.0 )
      { av[2] = toInt(ex-(ey-cy));
	av[3] = toInt(ey+(ex-cx));
      } else
      { av[2] = toInt(ex+(ey-cy));
	av[3] = toInt(ey-(ex-cx));
      }
  
      if ( qadSendv(a->second_arrow, NAME_points, 4, av) )
      { ComputeGraphical(a->second_arrow);
	postscriptGraphical(a->second_arrow);
      }
    }
  }

  ps_output("grestore\n");

  succeed;
}


status
drawPostScriptBitmap(BitmapObj bm)
{ Int depth = get(bm->image, NAME_postscriptDepth, 0);

  ps_output("~x ~y ~w ~h ~d greymap\n~P\n",
	    bm, bm, bm, bm, depth, depth, bm->image);

  succeed;
}


status
drawPostScriptImage(Image image)
{ Int depth = get(image, NAME_postscriptDepth, 0);

  ps_output("0 0 ~d ~d ~d greymap\n~P\n",
	    image->size->w, image->size->h, depth, depth, image);

  succeed;
}


status
drawPostScriptText(TextObj t)
{ String s = &t->string->data;

  if ( s[0].size > 0 )			/* i.e. non-empty */
  { int x, y, w;
    int b = valInt(t->border);

    x = valInt(t->area->x);
    y = valInt(t->area->y);
    w = valInt(t->area->w);

    if ( isDefault(t->background) )
      ps_output("~x ~y ~w ~h clear\n", t, t, t, t);

    ps_output("gsave ~C", t);

    if ( t->pen != ZERO || notNil(t->background) )
    { ps_output("~T ~p ~x ~y ~w ~h 0 boxpath\n",
		t, t, t, t, t, t);
      fill(t, NAME_background);
      if ( t->pen != ZERO )
	ps_output("draw\n");
    }
  
    if ( t->wrap == NAME_wrap )
    { LocalString(buf, s, s->size + MAX_WRAP_LINES);
    
      str_format(buf, s, valInt(t->margin), t->font);
      ps_string(buf, t->font, x+b, y+b, w-2*b, t->format);
    } else if ( t->wrap == NAME_clip )
    { ps_output("gsave 0 ~x ~y ~w ~h 0 boxpath clip\n", t, t, t, t);
      ps_string(s, t->font, x+b+valInt(t->x_offset), y+b, w-2*b, t->format);
      ps_output("grestore\n");
    } else
      ps_string(s, t->font, x+b, y+b, w-2*b, t->format);

    ps_output("grestore\n", t);
  }

  succeed;
}

		/********************************
		*       WINDOWS AND DISPLAY	*
		********************************/


status
postscriptFrame(FrameObj fr)
{ return ws_postscript_frame(fr);
}


status
postscriptDisplay(DisplayObj d)
{ return ws_postscript_display(d);
}
