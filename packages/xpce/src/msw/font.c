/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

#include "include.h"

#define FONTTABLESIZE	256

status
ws_create_font(FontObj f, DisplayObj d)
{ WsFont wsf = alloc(sizeof(ws_font));
  short widths[FONTTABLESIZE];
  HDC hdc;
  HFONT old;
  int n;
  LOGFONT lfont;
  TEXTMETRIC tm;
  Real scale = getResourceValueObject(f, NAME_scale);
  float fscale = (scale ? scale->value : 1.4);

  memset(&lfont, 0, sizeof(lfont));
  lfont.lfHeight          = (int)((float) valInt(f->points) * fscale);
  lfont.lfWeight          = (f->style == NAME_bold ? FW_BOLD : FW_NORMAL);
  lfont.lfItalic          = ((f->style == NAME_italic ||
			      f->style == NAME_oblique)  ? 1 : 0);
  lfont.lfPitchAndFamily  = (f->family == NAME_screen	 ? FIXED_PITCH :
			    			           DEFAULT_PITCH);
  lfont.lfPitchAndFamily |= (f->family == NAME_helvetica ? FF_SWISS :
			     f->family == NAME_times     ? FF_ROMAN :
			     				   FF_MODERN);
  
  if ( !(wsf->hfont = CreateFontIndirect(&lfont)) )
  { int stock;

    printf("Failed to create logical font; replacing with stock font\n");

    if ( f->family == NAME_screen )
    { if ( f->style == NAME_bold )
	stock = SYSTEM_FIXED_FONT;
      else
	stock = ANSI_FIXED_FONT;
    } else
    { if ( f->style == NAME_bold )
	stock = SYSTEM_FONT;
      else
	stock = ANSI_VAR_FONT;
    }

    wsf->hfont      = GetStockObject(stock);
    wsf->from_stock = TRUE;
  } else
    wsf->from_stock = FALSE;

  wsf->widths     = alloc(FONTTABLESIZE * sizeof(cwidth));
  assign(f, b16, OFF);

  hdc = GetDC(NULL);
  old = SelectObject(hdc, wsf->hfont);
  GetCharWidth(hdc, 0, FONTTABLESIZE-1, widths);
  for(n=0; n<FONTTABLESIZE; n++)
    wsf->widths[n] = widths[n];
  GetTextMetrics(hdc, &tm);
  wsf->ascent = tm.tmAscent + tm.tmExternalLeading;
  wsf->descent = tm.tmDescent;
  SelectObject(hdc, old);
  ReleaseDC(NULL, hdc);

  if ( wsf->widths['i'] == wsf->widths['w'] )
    assign(f, fixed_width, ON);
  else
    assign(f, fixed_width, OFF);

  registerXrefObject(f, d, wsf);

  succeed;
}


void
ws_destroy_font(FontObj f, DisplayObj d)
{ WsFont wsf = (WsFont) getExistingXrefObject(f, d);

  if ( wsf )
  { if ( !wsf->from_stock )
      DeleteObject(wsf->hfont);
    unregisterXrefObject(f, d);
  }
}


