/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

#include "xcursor.h"
#include <stdio.h>
#include <malloc.h>

void
convert(FILE *in, FILE *out)
{ char 				line[1024];
  int 				low, high;
  cursor_glyph_file_header	header;
  CursorGlyph			glyphs;
  int				glyph = 0;
  unsigned short 		*data = malloc(64 * 1024);
  unsigned short		*dp = data;


  while(fgets(line, sizeof(line), in))
  { if ( sscanf(line, "Range:%d to %d", &low, &high) == 2 )
    { header.magic   = XPCE_CURSOR_FILE_MAGIC;
      header.entries = high - low + 1;
      header.offset  = low;
      glyphs = malloc(sizeof(cursor_glyph) * header.entries);

      while(fgets(line, sizeof(line), in))
      { int here, right, left, ascent, descent, found;

	if ( sscanf(line, "char #%d", &here) == 1 )
	{ if ( here != glyph )
	  { fprintf(stderr, "Mismatch: here = %d; glyph = %d\n", here, glyph);
	    exit(1);
	  }
	  fgets(line, sizeof(line), in);
	  if ( (found = sscanf(line,
			       "Right: %d%*[ \t]Left: %d%*[ \t]"
			       "Descent: %d%*[ \t]Ascent: %d",
			       &right, &left, &descent, &ascent)) != 4 )
	  { fprintf(stderr, "Failed to parse %s (found = %d)\n", line, found);
	    exit(1);
	  }
	  
	  glyphs[glyph].width  = right - left;
	  glyphs[glyph].height = ascent + descent;
	  glyphs[glyph].hot_x  = -left;
	  glyphs[glyph].hot_y  = ascent;
	  glyphs[glyph].offset = (dp-data)*sizeof(unsigned short);

	  { int x, y;
	    unsigned short d;
	    int bit;

	    for(y=0; y<glyphs[glyph].height; y++)
	    { fgets(line, sizeof(line), in);

	      for(d=0, bit=15, x=0; x<glyphs[glyph].width; x++)
	      { d |= (line[x] == '#' ? 1 : 0) << bit;

		if ( bit-- == 0 )
		{ *dp++ = d;
		  bit = 15;
		}
	      }
	      if ( bit != 15 )
		*dp++ = d;
	    }
	  }
	  glyph++;
	}
      }

      if ( glyph != header.entries )
      { fprintf(stderr, "Only parsed %d of %d glyphs\n",
		glyphs, header.entries);
	exit(1);
      }
      header.dsize = (dp-data) * sizeof(unsigned short);

      fwrite(&header, sizeof(header), 1, out);
      fwrite(glyphs,  sizeof(cursor_glyph), glyph, out);
      fwrite(data,    sizeof(unsigned short), dp-data, out);
      fflush(out);
      return;
    }      
  }
}


main(int argc, char **argv)
{ if ( argc == 3 )
  { FILE  *in = fopen(argv[1], "rb");
    FILE *out = fopen(argv[2], "wb");

    if ( !in )
    { perror(argv[1]);
      exit(1);
    }
    if ( !out )
    { perror(argv[2]);
      exit(1);
    }

    convert(in, out);
    exit(0);
  }

  fprintf(stderr, "Usage: %s showfont-output outfile\n", argv[0]);
  exit(1);
}
