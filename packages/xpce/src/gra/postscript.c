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
static int	postscriptImage(Image);
static int	header(Any, Area, Bool);
static int	macros(void);
static int	footer(void);
static status	fill(Any, Name);
static status	paintPostscriptArrow(Arrow a, Int x1, Int y1, Int x2, Int y2);

struct
{ Name	family;			   /* family of current PostScript font */
  Int	points;			   /* points in current PostScript font */
} currentFont;
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
  PostScript = malloc(outputSize);
}

static void
closeOutput(void)
{ free(PostScript);
  PostScript = NULL;
}

void
ps_put_char(int c)
{ if ( outputPointer == outputSize )
  { outputSize *= 2;
    PostScript = realloc(PostScript, outputSize);
  }

  DEBUG(NAME_postscript, putchar(c); fflush(stdout));

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
    ~N	    Name 	print text of name
    ~S	    StringObj	Output text of StringObj with postscript escapes
    ~O	    Object	Output comment to start O
    ~P	    Image	Output pattern of image
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
			free_string(s);

			continue;
		      }
	  case 'P':   { postscriptImage(va_arg(args, Image));
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

  if ( currentFont.family == family && currentFont.points == points )
    succeed;

  if ( memberChain(documentFonts, family) != SUCCEED )
    appendChain(documentFonts, family);

  ps_output("/~N findfont ~d scalefont setfont\n", family, points);

  succeed;
}

static status
fill(Any gr, Name sel)
{ Image pattern = get(gr, sel, 0);
  Int greyLevel;

  if ( isNil(pattern) )
    succeed;

  if ( (greyLevel = (Int) get(pattern, NAME_postscriptGrey, 0)) != FAIL )
  { ps_output("gsave ~f setgray fill grestore\n",
	   (float) (100 - valInt(greyLevel)) / 100.0 );
    succeed;
  }
  
  ps_output("~x ~y ~w ~h ~d ~d \n<~P>\nfillpath\n",
	 gr, gr, gr, gr,
	 pattern->size->w, pattern->size->h, pattern);
  
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
	printf("postscriptDrawable(%d %d %d %d) ...", ox, oy, w, h);
	fflush(stdout));

  for(bytes = y = c = 0, bits = 8; y < h; y++)
  { for(x=0; x < w; x++)
    { c |= (r_get_mono_pixel(x+ox, y+oy) << --bits);
      if ( bits == 0 )
        putByte(c);
    }
    if ( bits != 8 )
      putByte(c);
  }

  DEBUG(NAME_postscript, printf("ok\n"));

  succeed;
}


static int
postscriptImage(Image image)
{ ws_postscript_image(image);

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

  ps_output("%!\n");
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

  if ( instanceOfObject(obj, ClassGraphical) )
  { Graphical gr = obj;
    PceWindow sw;

    if ( notDefault(gr->colour) && 
	 (sw = getWindowGraphical(gr)) &&
	 equalColour(sw->background, gr->colour) )
    { ps_output("gsave 100 setgray\n");
      send(obj, NAME_DrawPostScript, 0);
      ps_output("grestore\n");
      succeed;
    }
  }

  return send(obj, NAME_DrawPostScript, 0);
}


status
drawPostScriptDevice(Device dev)
{ Cell cell;

  ps_output("gsave ~t\n", dev);

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
  { ps_output("gsave ~T ~p ~x ~y ~w ~h 0 boxpath\n", f, f, f, f, f, f);
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
    ps_output("gsave ~T ~p ~x ~y ~d ~d ~d boxpath\n", b, b,
	      b, b, sub(a->w, s), sub(a->h, s), r);
    if ( notNil(b->fill_pattern) )
      fill(b, NAME_fillPattern);
    else
      ps_output("gsave 1.0 setgray fill grestore\n");
    ps_output("draw grestore\n");
  } else
  { ps_output("gsave ~T ~p ~x ~y ~w ~h ~d boxpath\n",
	      b, b, b, b, b, b, r);
    fill(b, NAME_fillPattern);
    ps_output("draw grestore\n");
  }

  succeed;
}


status
drawPostScriptCircle(Circle c)
{ ps_output("~T ~p ~x ~y ~d circlepath\n", c, c, c, c, div(c->area->w, TWO));
  fill(c, NAME_fillPattern);
  ps_output("draw\n");

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
    ps_output("gsave ~T ~p ~x ~y ~d ~d ellipsepath\n", e, e, e, e,
	  sub(a->w, s), sub(a->h, s));
    if ( notNil(e->fill_pattern) )
      fill(e, NAME_fillPattern);
    else
      ps_output("gsave 1.0 setgray fill grestore\n");
    ps_output("draw grestore\n");
  } else
  { ps_output("gsave ~T ~p ~x ~y ~w ~h ellipsepath\n", e, e, e, e, e, e);
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

      ps_output("gsave ~d ~d translate ~T ~p ~c startpath\n",
	     p->offset->x, p->offset->y, p, p, pt);

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
      ps_output("draw grestore\n");
    } else				/* poly-path */
    { Cell cell;
      int i = -1;			/* skip first */

      ps_output("gsave ~d ~d translate ~T ~p ~c startpath\n",
	     p->offset->x, p->offset->y,
	     p, p, getHeadChain(points));
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
      ps_output("draw grestore\n");
    }

					/* Paint the arrows */
    if ( notNil(p->first_arrow) )
    { Point tip = getHeadChain(points);
      Point ref = getNth1Chain(points, TWO);

      paintPostscriptArrow(p->first_arrow,
			   add(tip->x,p->offset->x), add(tip->y,p->offset->y),
			   add(ref->x,p->offset->x), add(ref->y,p->offset->y));
    }
    if ( notNil(p->second_arrow) )
    { Point tip = getTailChain(points);
      Point ref = getNth1Chain(points, sub(getSizeChain(points), ONE));

      paintPostscriptArrow(p->second_arrow,
			   add(tip->x,p->offset->x), add(tip->y,p->offset->y),
			   add(ref->x,p->offset->x), add(ref->y,p->offset->y));
    }
  }

  succeed;
}


#define points_line(ln, x1, y1, x2, y2) \
  { x1 = valInt(ln->area->x); \
    y1 = valInt(ln->area->y); \
    x2 = x1+valInt(ln->area->w); \
    x2 += (valInt(ln->area->w)>=0 ? -1 : 1); \
    y2 = y1+valInt(ln->area->h); \
    y2 += (valInt(ln->area->h)>=0 ? -1 : 1); \
  }


status
drawPostScriptLine(Line ln)
{ int x1, y1, x2, y2;

  points_line(ln, x1, y1, x2, y2);

  if ( ln->pen != ZERO )
    ps_output("~T ~p ~D ~D ~D ~D linepath draw\n", ln, ln, x1, y1, x2-x1, y2-y1);

  if ( notNil(ln->first_arrow) )
    paintPostscriptArrow(ln->first_arrow,
			 toInt(x1), toInt(y1), toInt(x2), toInt(y2));
  if ( notNil(ln->second_arrow) )
    paintPostscriptArrow(ln->second_arrow,
			 toInt(x2), toInt(y2), toInt(x1), toInt(y1));

  succeed;
}


status
drawPostScriptArrow(Arrow a)
{ ps_output("~T ~p pen ", a, a);
  ps_output("newpath ~d ~d moveto ~d ~d lineto ~d ~d lineto",
	 a->left->x, a->left->y,
	 a->tip->x, a->tip->y,
	 a->right->x, a->right->y);

  if ( a->style == NAME_closed || notNil(a->fill_pattern) )
    ps_output(" closepath");

  if ( isNil(a->fill_pattern) )
  { ps_output(" ~T draw\n", a);
    succeed;
  }

  ps_output("\n");
  fill(a, NAME_fillPattern);

  succeed;
}


static status
paintPostscriptArrow(Arrow a, Int x1, Int y1, Int x2, Int y2)
{ pointsArrow(a, x1, y1, x2, y2);
  computeArrow(a);

  return drawPostScriptArrow(a);
}


status
drawPostScriptArc(Arc a)
{ ps_output("gsave ~T ~p ~D ~d ~d ~d ~d ~f ~f arcpath\n",
	 a, a,
	 a->close == NAME_none ? 0 : a->close == NAME_chord ? 1 : 2,
	 a->position->x, a->position->y,
	 a->size->w, a->size->h,
	 a->start_angle->value, a->size_angle->value);

  fill(a, NAME_fillPattern);
  ps_output("draw grestore\n");

  if ( notNil(a->first_arrow) ||  notNil(a->second_arrow) )
  { int sx, sy, cx, cy, ex, ey;

    points_arc(a, &sx, &sy, &ex, &ey);
    cx = valInt(a->position->x);
    cy = valInt(a->position->y);

    if ( notNil(a->first_arrow) )
    { if ( a->size_angle->value >= 0.0 )
	paintPostscriptArrow(a->first_arrow, toInt(sx), toInt(sy),
			     toInt(sx+(sy-cy)), toInt(sy-(sx-cx)));
      else
	paintPostscriptArrow(a->first_arrow, toInt(sx), toInt(sy),
			     toInt(sx-(sy-cy)), toInt(sy+(sx-cx)));
    }
    if ( notNil(a->second_arrow) )
    { if ( a->size_angle->value >= 0.0 )
	paintPostscriptArrow(a->second_arrow, toInt(ex), toInt(ey),
			     toInt(ex-(ey-cy)), toInt(ey+(ex-cx)));
      else
	paintPostscriptArrow(a->second_arrow, toInt(ex), toInt(ey),
			     toInt(ex+(ey-cy)), toInt(ey-(ex-cx)));
    }
  }

  succeed;
}


status
drawPostScriptBitmap(BitmapObj bm)
{ ps_output("~x ~y ~w ~h bitmap\n\n~P\n", bm, bm, bm, bm, bm->image);

  succeed;
}


status
drawPostScriptImage(Image image)
{ ps_output("0 0 ~d ~d bitmap\n\n~P\n",
	 image->size->w, image->size->h, image);

  succeed;
}


status
drawPostScriptText(TextObj t)
{ String s = &t->string->data;
  int x, y, w;

  x = valInt(t->area->x);
  y = valInt(t->area->y);
  w = valInt(t->area->w);

  if ( isDefault(t->background) )
    ps_output("~x ~y ~w ~h clear\n", t, t, t, t);
  
  if ( s[0].size > 0 )			/* i.e. non-empty */
  { if ( t->wrap == NAME_wrap )
    { LocalString(buf, s, s->size + MAX_WRAP_LINES);
    
      str_format(buf, s, valInt(t->margin), t->font);
      ps_string(buf, t->font, x, y, w, t->format);
    } else if ( t->wrap == NAME_clip )
    { ps_output("gsave 0 ~x ~y ~w ~h 0 boxpath clip\n", t, t, t, t);
      ps_string(s, t->font, x+valInt(t->x_offset), y, w, t->format);
      ps_output("grestore\n");
    } else
      ps_string(s, t->font, x, y, w, t->format);
  }

  if ( t->pen != ZERO )
    ps_output("gsave ~T ~p ~x ~y ~w ~h 0 boxpath draw grestore\n",
	      t, t, t, t, t, t);

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
