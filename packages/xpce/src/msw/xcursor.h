/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

typedef struct
{ unsigned char	width;
  unsigned char height;
  char		hot_x;
  char		hot_y;
  long		offset;
} cursor_glyph, *CursorGlyph;


typedef struct
{ int		magic;			/* magic number */
  int		entries;		/* # entries in file */
  int		offset;			/* offset of entries */
  long		dsize;			/* Data size (for bits) in bytes */
} cursor_glyph_file_header, *CursorGlyphFileHeader;

#define XPCE_CURSOR_FILE_MAGIC 0x626482	/* nice number */
