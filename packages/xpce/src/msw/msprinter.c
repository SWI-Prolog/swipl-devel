/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

#include "include.h"
#include <h/unix.h>			/* file access */
#ifndef MAXPATHLEN
#define MAXPATHLEN 1024
#endif

#define USE_PRINTDLG 1
#ifdef USE_PRINTDLG
#undef PAGESETUPDLG
#define PAGESETUPDLG PRINTDLG
#endif

#define WinError() strName(WinStrError(GetLastError()))

Class ClassWinPrinter;			/* the class handle */
typedef struct win_printer *WinPrinter;

typedef struct
{ HDC		hdc;			/* Device hdc */
#ifdef USE_PRINTDLG
  PRINTDLG	info;
#else
  PAGESETUPDLG	info;			/* Window page setup data */
#endif
} ws_printer, *WsPrinter;


NewClass(win_printer)			/* class structure */
  Name	    map_mode;			/* Mapping mode */
  Name	    job_name;			/* Job Name */
  Int	    job;			/* Job Id */
  Tuple	    range;			/* Range of pages to print */
  Any	    resolution;			/* Resolution of the device */
  Point	    origin;			/* Origin of the coordinate system */
  Any	    device;			/* Output device */
  WsPrinter ws_ref;			/* Windows data */
End;

static Chain  WinPrinters;		/* @win_printers */
static status closeWinPrinter(WinPrinter prt);
static int    nameToMapMode(Name name);
static Name   mapModeToName(int mode);
static status mapModeWinPrinter(WinPrinter prt, Name mode);

static status
initialiseWinPrinter(WinPrinter prt, Name jobname)
{ if ( isDefault(jobname) )
    jobname = CtoName("XPCE");

  assign(prt, map_mode,   DEFAULT);
  assign(prt, device,     DEFAULT);
  assign(prt, job_name,   jobname);
  assign(prt, job,        NIL);
  assign(prt, resolution, DEFAULT);
  assign(prt, origin,	  newObject(ClassPoint, EAV));

  prt->ws_ref = alloc(sizeof(ws_printer));
  memset(prt->ws_ref, 0, sizeof(ws_printer));

  return prependChain(WinPrinters, prt);
}


static void
resetDataWinPrinter(WinPrinter prt)
{ if ( prt->ws_ref )
  { PAGESETUPDLG *psd = &prt->ws_ref->info;

    if ( prt->ws_ref->hdc )
    { DeleteDC(prt->ws_ref->hdc);
      prt->ws_ref->hdc = 0;
    }

    if ( psd->hDevMode )
      GlobalFree(psd->hDevMode);

    memset(prt->ws_ref, 0, sizeof(ws_printer));
  }
}


static status
unlinkWinPrinter(WinPrinter prt)
{ closeWinPrinter(prt);
  resetDataWinPrinter(prt);

  if ( prt->ws_ref )
  { unalloc(sizeof(ws_printer), prt->ws_ref);
    prt->ws_ref = NULL;
  }

  deleteChain(WinPrinters, prt);

  succeed;
}


static void
closeAllWinPrinters(int rval)
{ if ( WinPrinters )
  { Cell cell;

    for_cell(cell, WinPrinters)
    { WinPrinter prt = cell->value;

      closeWinPrinter(prt);
    }
  }
}

		 /*******************************
		 *	       OPEN		*
		 *******************************/

static status
setupWinPrinter(WinPrinter prt, FrameObj fr)
{ HWND hwnd = (isDefault(fr) ? 0 : getHwndFrame(fr));
  Name mm;

  PAGESETUPDLG *psd = &prt->ws_ref->info;

  resetDataWinPrinter(prt);

  psd->lStructSize = sizeof(PAGESETUPDLG); 
  psd->hwndOwner = hwnd;
/*
  psd->hDevMode; 
  psd->hDevNames; 
  psd->Flags; 
*/
#ifdef USE_PRINTDLG
  psd->Flags = (PD_ALLPAGES|
		PD_RETURNDC|
		PD_NOSELECTION|
		PD_USEDEVMODECOPIESANDCOLLATE);

  if ( notNil(prt->range) )
  { psd->nFromPage = valInt(prt->range->first);
    psd->nToPage = valInt(prt->range->second);
  } else
    psd->Flags |= PD_NOPAGENUMS;

/* Keep them here are reminder
  psd->hDC; 
  psd->Flags; 
  psd->nFromPage; 
  psd->nToPage; 
  psd->nMinPage; 
  psd->nMaxPage; 
  psd->nCopies; 
  psd->hInstance; 
  psd->lCustData; 
  psd->lpfnPrintHook; 
  psd->lpfnSetupHook; 
  psd->lpPrintTemplateName; 
  psd->lpSetupTemplateName; 
  psd->hPrintTemplate; 
  psd->hSetupTemplate; 
*/
  psd->nCopies = 1;
#else
/* 
  psd->ptPaperSize; 
  psd->rtMinMargin; 
  psd->rtMargin; 
  psd->hInstance; 
  psd->lCustData; 
  psd->lpfnPageSetupHook; 
  psd->lpfnPagePaintHook; 
  psd->lpPageSetupTemplateName; 
  psd->hPageSetupTemplate;
*/
#endif

  if ( isName(prt->device) )		/* default printer name */
  { char *s = strName(prt->device);
    HGLOBAL h = GlobalAlloc(GMEM_MOVEABLE, sizeof(DEVNAMES)+strlen(s)+1);
    DEVNAMES *names = GlobalLock(h);

    names->wDriverOffset = 0;		/* i.e. "WINSPOOL" */
    names->wDeviceOffset = sizeof(DEVNAMES);
    names->wOutputOffset = 0;		/* port (or "FILE:") */
    names->wDefault      = 0;
    strcpy((char *)&names[1], s);

    GlobalUnlock(h);
    psd->hDevNames = h;
  }

#ifdef USE_PRINTDLG
  if ( !PrintDlg(psd) )
#else
  if ( !PageSetupDlg(psd) )
#endif
  { DWORD rc;

    if ( (rc=CommDlgExtendedError()) )
    { /* TBD: define kernel error and pass Windows message */

      send(CurrentDisplay(NIL), NAME_report, NAME_error,
	   CtoString("Unable to set up printer"), EAV);
    }
    fail;
  }

  if ( psd->hDevNames )			/* debugging */
  { DEVNAMES *names = GlobalLock(psd->hDevNames);
    
    if ( names->wDriverOffset )
      DEBUG(NAME_print,
	    Cprintf("driver=%s\n", (char *)names+names->wDriverOffset));
    if ( names->wDeviceOffset )
    { assign(prt, device, CtoName((char *)names+names->wDeviceOffset));
      DEBUG(NAME_print,
	    Cprintf("dev=%s\n", (char *)names+names->wDeviceOffset));
    }
    if ( names->wOutputOffset )
      DEBUG(NAME_print,
	    Cprintf("output=%s\n", (char *)names+names->wOutputOffset));

    GlobalUnlock(psd->hDevNames);
  }


#ifndef USE_PRINTDLG
  if ( psd->hDevMode )
  { DEVMODE *dmode = GlobalLock(psd->hDevMode);
    prt->ws_ref->hdc = CreateDC("WINSPOOL", /* NT, NULL for Windows-95 */
				dmode->dmDeviceName,
				NULL,
				psd->hDevMode);
    GlobalUnlock(psd->hDevMode);
  }
#else
  prt->ws_ref->hdc = psd->hDC;
  if ( notNil(prt->range) && (psd->Flags & PD_PAGENUMS) )
  { assign(prt->range, first,  toInt(psd->nFromPage));
    assign(prt->range, second, toInt(psd->nToPage));
  }
#endif

  if ( !prt->ws_ref->hdc )
  { Cprintf("Failed to open Printer DC: %s\n", WinError());
    fail;
  }

  SetBkMode(prt->ws_ref->hdc, TRANSPARENT);

  if ( (mm = mapModeToName(GetMapMode(prt->ws_ref->hdc))) )
    assign(prt, map_mode, mm);

  DEBUG(NAME_print, Cprintf("Got MM %s from HDC=%p\n",
			    pp(mm), prt->ws_ref->hdc));

  succeed;
}


static status
openWinPrinter(WinPrinter prt)
{ if ( prt->ws_ref->hdc )
  { DOCINFO di;
    int job;
    char fname[MAXPATHLEN];

    di.cbSize       = sizeof(di);
    di.lpszDocName  = strName(prt->job_name);

    if ( instanceOfObject(prt->device, ClassFile) )
    { Name name = getOsNameFile(prt->device);

      _xos_os_filename(strName(name), fname);
      di.lpszOutput = fname;
    } else if ( prt->ws_ref->info.Flags & PD_PRINTTOFILE )
					/* User selected print-to-file */
#if 1
    { Name to;
      static Chain filter;

      if ( !filter )
	filter = globalObject(NAME_printFileFilter,
			      ClassChain,
			      newObject(ClassTuple,
					NAME_printFile,
					CtoName("*.prn"), EAV),
			      newObject(ClassTuple,
					NAME_allFiles,
					CtoName("*.*"), EAV),
			      EAV);

      if ( (to = getWinFileNameDisplay(CurrentDisplay(NIL),
				       NAME_save,
				       filter,
				       DEFAULT,
				       DEFAULT,
				       DEFAULT)) )
      { _xos_os_filename(strName(to), fname);
	di.lpszOutput = fname;
      } else
	fail;				/* Use cancel */
#else
    { PAGESETUPDLG *psd = &prt->ws_ref->info;
      DEVNAMES *names = GlobalLock(psd->hDevNames);

      strcpy(fname, &((char *)names)[names->wOutputOffset]);
      di.lpszOutput = fname;
      DEBUG(NAME_print, Cprintf("Printing to file: \"%s\"\n", fname));

      GlobalUnlock(psd->hDevNames);
#endif
    } else
      di.lpszOutput = NULL;

    di.lpszDatatype = NULL;
    di.fwType	    = 0;

    if ( (job=StartDoc(prt->ws_ref->hdc, &di) > 0) &&
	 StartPage(prt->ws_ref->hdc) > 0 )
    { if ( notDefault(prt->map_mode) )
	SetMapMode(prt->ws_ref->hdc, nameToMapMode(prt->map_mode));
      assign(prt, job, toInt(job));
      succeed;
    }
  }

  fail;
}


static status
nextPageWinPrinter(WinPrinter prt)
{ if ( prt->ws_ref->hdc )
  { EndPage(prt->ws_ref->hdc);
    StartPage(prt->ws_ref->hdc);
    if ( notDefault(prt->map_mode) )	/* Windows 95 resets this */
      SetMapMode(prt->ws_ref->hdc, nameToMapMode(prt->map_mode));

    succeed;
  }

  fail;
}


static status
closeWinPrinter(WinPrinter prt)
{ if ( notNil(prt->job) && prt->ws_ref->hdc )
  { EndPage(prt->ws_ref->hdc);
    EndDoc(prt->ws_ref->hdc);
    assign(prt, job, NIL);
  }

  succeed;
}

		 /*******************************
		 *	  SET DIMENSIONS	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A long search has resulted  in  these   magical  formulas  to relate the
Physical (Dx/Dy) and Logical (Lx/Ly) properly.

Dx = (Lx - xWO) * xVE/xWE + xVO
Dy = (Ly - yWO) * yVE/yWE + yVO
Lx = (Dx - xVO) * xWE/xVE + xWO
Ly = (Dy - yVO) * yWE/yVE + yWO

If the resolution is @default, ->resolution   sets the scaling such that
the physical dimensions on the screen are the same as on the paper based
on the information on printer- and screen resolution.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static status
resolutionWinPrinter(WinPrinter prt, Any resolution)
{ if ( prt->resolution != resolution )
  { assign(prt, resolution, resolution);

    if ( isDefault(resolution) || isInteger(resolution) )
      mapModeWinPrinter(prt, NAME_isotropic);
    else
      mapModeWinPrinter(prt, NAME_anisotropic);

    if ( prt->ws_ref->hdc )
    { int w, h;
      HDC hdc = prt->ws_ref->hdc;
      HDC shdc = GetDC(NULL);
      int rx  = GetDeviceCaps(hdc, LOGPIXELSX);
      int ry  = GetDeviceCaps(hdc, LOGPIXELSY);
      int srx = GetDeviceCaps(shdc, LOGPIXELSX);
      int sry = GetDeviceCaps(shdc, LOGPIXELSY);
      int ox = valInt(prt->origin->x);
      int oy = valInt(prt->origin->y);

      ReleaseDC(NULL, shdc);

      DEBUG(NAME_print, 
	    Cprintf("Resolution = %d x %d, Screen = %d x %d\n",
		    rx, ry, srx, sry));

      SetWindowOrgEx(hdc, ox, oy, NULL);
	
      if ( isDefault(resolution) )
      { SetViewportOrgEx(hdc, 0, 0, NULL);
	SetWindowExtEx(hdc, srx, sry, NULL);
	SetViewportExtEx(hdc, rx, ry, NULL);
      } else
      { int pw = GetDeviceCaps(hdc, PHYSICALWIDTH);
	int ph = GetDeviceCaps(hdc, PHYSICALHEIGHT);

	if ( isInteger(resolution) )
	{ int pw = GetDeviceCaps(hdc, PHYSICALWIDTH);
	  int ph = GetDeviceCaps(hdc, PHYSICALHEIGHT);
	  
	  h = valInt(resolution);
	  w = (h*pw)/ph;
  
	} else
	{ Size sz = resolution;
  
	  h = valInt(sz->h);
	  w = valInt(sz->w);
	}

	SetViewportOrgEx(hdc, 0, 0, NULL);
	SetWindowExtEx(hdc, w, h, NULL);
	SetViewportExtEx(hdc, pw, ph, NULL);
      }
    }
  }

  succeed;
}


static status
originWinPrinter(WinPrinter prt, Point origin)
{ if ( !equalPoint(origin, prt->origin) )
  { copyPoint(prt->origin, origin);

    if ( prt->ws_ref->hdc )
    { int x = valInt(prt->origin->x);
      int y = valInt(prt->origin->y);
      HDC hdc = prt->ws_ref->hdc;

      SetWindowOrgEx(hdc, x, y, NULL);
      SetViewportOrgEx(hdc, 0, 0, NULL); /* needed? */
    }
  }

  succeed;
}


static status
viewportWinPrinter(WinPrinter prt, Area vp)
{ HDC hdc = prt->ws_ref->hdc;

  if ( hdc )
  { int x = valInt(vp->x);
    int y = valInt(vp->y);
    int w = valInt(vp->w);
    int h = valInt(vp->h);
    
    SetViewportOrgEx(hdc, x, y, NULL);
    SetViewportExtEx(hdc, w, h, NULL);

    succeed;
  }

  fail;
}


static status
windowWinPrinter(WinPrinter prt, Area ww)
{ HDC hdc = prt->ws_ref->hdc;

  if ( hdc )
  { int x = valInt(ww->x);
    int y = valInt(ww->y);
    int w = valInt(ww->w);
    int h = valInt(ww->h);
    
    SetWindowOrgEx(hdc, x, y, NULL);
    SetWindowExtEx(hdc, w, h, NULL);

    succeed;
  }

  fail;
}


static status
rangeWinPrinter(WinPrinter prt, Tuple range)
{ if ( isNil(range) ||
       (isInteger(range->first) &&
	isInteger(range->second)) )
  { assign(prt, range, range);

    succeed;
  }

  return errorPce(range, NAME_unexpectedType, nameToType(NAME_tuple));
}


		 /*******************************
		 *	       INFO		*
		 *******************************/

static Size
getSizeWinPrinter(WinPrinter prt)
{ if ( prt->ws_ref->hdc )
  { int w = GetDeviceCaps(prt->ws_ref->hdc, PHYSICALWIDTH);
    int h = GetDeviceCaps(prt->ws_ref->hdc, PHYSICALHEIGHT);

    answer(answerObject(ClassSize, toInt(w), toInt(h), EAV));
  }

  fail;
}


static Point
getOffsetWinPrinter(WinPrinter prt)
{ if ( prt->ws_ref->hdc )
  { int x = GetDeviceCaps(prt->ws_ref->hdc, PHYSICALOFFSETX);
    int y = GetDeviceCaps(prt->ws_ref->hdc, PHYSICALOFFSETY);

    answer(answerObject(ClassPoint, toInt(x), toInt(y), EAV));
  }

  fail;
}


static Size
getDotsPerInchWinPrinter(WinPrinter prt)
{ if ( prt->ws_ref->hdc )
  { int x = GetDeviceCaps(prt->ws_ref->hdc, LOGPIXELSX);
    int y = GetDeviceCaps(prt->ws_ref->hdc, LOGPIXELSY);

    answer(answerObject(ClassSize, toInt(x), toInt(y), EAV));
  }

  fail;
}


typedef struct
{ int	mode;
  Name	name;
} mapmode;


static mapmode map[] =
{ { MM_ANISOTROPIC, NAME_anisotropic },
  { MM_HIENGLISH,   NAME_hienglish },
  { MM_HIMETRIC,    NAME_himetric },
  { MM_ISOTROPIC,   NAME_isotropic },
  { MM_LOENGLISH,   NAME_loenglish },
  { MM_TEXT,	    NAME_text },
  { MM_TWIPS,	    NAME_twips },
  { -1,		    NULL }
}; 


static int
nameToMapMode(Name name)
{ mapmode *m = map;

  for(; m->name; m++)
  { if ( m->name == name )
      return m->mode;
  }

  return -1;
}


static Name
mapModeToName(int mode)
{ mapmode *m = map;

  for(; m->name; m++)
  { if ( m->mode == mode )
      return m->name;
  }

  return NULL;
}



static status
mapModeWinPrinter(WinPrinter prt, Name mode)
{ if ( prt->map_mode != mode )
  { assign(prt, map_mode, mode);

    if ( prt->ws_ref->hdc )
    { if ( !SetMapMode(prt->ws_ref->hdc, nameToMapMode(mode)) )
      { Cprintf("Failed SetMapMode(%s): %s\n", strName(mode), WinError());
	fail;
      }
    }
  }

  succeed;
}


		 /*******************************
		 *	      DRAWING		*
		 *******************************/

static status
drawInWinPrinter(WinPrinter prt, Any obj, Point pos)
{ Int oldx, oldy;
  Device dev;
  Area bb;

  if ( notDefault(pos) && instanceOfObject(obj, ClassGraphical) )
  { Graphical gr = obj;

    oldx = gr->area->x;
    oldy = gr->area->y;
    dev = gr->device;
    gr->device = NIL;
    setGraphical(gr, pos->x, pos->y, DEFAULT, DEFAULT);
  } else
  { oldx = oldy = DEFAULT;
    dev = NIL;				/* keep compiler happy */
  }

  if ( instanceOfObject(obj, ClassGraphical) )
  { Graphical gr = obj;

    ComputeGraphical(gr);
    bb = gr->area;
  } else /* if ( instanceOfObject(obj, ClassChain) ) */
  { Cell cell;

    bb = answerObject(ClassArea, EAV);
    for_cell(cell, (Chain)obj)
    { Graphical gr = cell->value;

      if ( instanceOfObject(gr, ClassGraphical) )
      { ComputeGraphical(gr);
	unionNormalisedArea(bb, gr->area);
      }
    }
  }


  d_hdc(prt->ws_ref->hdc, DEFAULT, DEFAULT);

  if ( instanceOfObject(obj, ClassGraphical) )
  { Graphical gr = obj;

    RedrawArea(gr, gr->area);
  } else
  { Chain ch = obj;
    Cell cell;

    for_cell(cell, ch)
    { if ( instanceOfObject(cell->value, ClassGraphical) )
      { Graphical gr = cell->value;

	RedrawArea(gr, gr->area);
      }
    }
  }

  d_done();

  doneObject(bb);

  if ( notDefault(oldx) )
  { Graphical gr = obj;

    setGraphical(gr, oldx, oldy, DEFAULT, DEFAULT);
    gr->device = dev;
  }

  succeed;
}



		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_drawIn[] =
        { "graphical|chain", "at=[point]" };

/* Instance Variables */

static vardecl var_winprinter[] =
{ SV(NAME_mapMode,
     "[{anisotropic,hienglish,himetric,isotropic,loenglish,text,twips}]",
     IV_GET|IV_STORE, mapModeWinPrinter,
     NAME_dimension, "Resolution and coordinate system"),
  IV(NAME_jobName, "name", IV_BOTH,
     NAME_name, "Public name of the print job"),
  IV(NAME_job, "int*", IV_GET,
     NAME_identifier, "Windows job-id for this print job"),
  SV(NAME_range, "tuple*", IV_GET|IV_STORE, rangeWinPrinter,
     NAME_selection, "Range of pages to print"),
  SV(NAME_resolution, "[int|size]", IV_GET|IV_STORE, resolutionWinPrinter,
     NAME_dimension, "Height/Size of the page in logical units"),
  SV(NAME_origin, "point", IV_GET|IV_STORE, originWinPrinter,
     NAME_dimension, "Origin for drawing"),
  IV(NAME_device, "[name|file]", IV_BOTH,
     NAME_output, "Default output device"),
  IV(NAME_wsRef, "alien:PRINTDLG *", IV_GET, 
     NAME_internal, "Associated Windows data")
};

/* Send Methods */

static senddecl send_winprinter[] =
{ SM(NAME_initialise, 1, "job_name=[name]", initialiseWinPrinter,
     DEFAULT, "Create Windows printer interface"),
  SM(NAME_unlink, 0, NULL, unlinkWinPrinter,
     DEFAULT, "Remove from @win_printers"),
  SM(NAME_setup, 1, "frame=[frame]", setupWinPrinter,
     NAME_property, "Query properties using standard dialog"),
  SM(NAME_viewport, 1, "area", viewportWinPrinter,
     NAME_dimension, "Set the physical area"),
  SM(NAME_window, 1, "area", windowWinPrinter,
     NAME_dimension, "Set the logical area"),
  SM(NAME_open, 0, NULL, openWinPrinter,
     NAME_open, "Start a document"),
  SM(NAME_close, 0, NULL, closeWinPrinter,
     NAME_open, "End a document"),
  SM(NAME_nextPage, 0, NULL, nextPageWinPrinter,
     NAME_print, "Advance to the next page"),
  SM(NAME_drawIn, 2, T_drawIn, drawInWinPrinter,
     NAME_print, "Paint graphical on printer")
};

/* Get Methods */

static getdecl get_winprinter[] =
{ GM(NAME_size, 0, "size", NULL, getSizeWinPrinter,
     NAME_dimension, "Size of page in device units"),
  GM(NAME_offset, 0, "point", NULL, getOffsetWinPrinter,
     NAME_dimension, "Top-left-most position that can be printed"),
  GM(NAME_dotsPerInch, 0, "size", NULL, getDotsPerInchWinPrinter,
     NAME_dimension, "Resolution in dots per inch")
};

/* Resources */

#define rc_winprinter NULL
/*
static classvardecl rc_winprinter[] =
{ 
};
*/

/* Class Declaration */

static Name winprinter_termnames[] = { NAME_file };

ClassDecl(winprinter_decls,
          var_winprinter, send_winprinter, get_winprinter, rc_winprinter,
          1, winprinter_termnames,
          "$Rev$");


status
makeClassWinPrinter(Class class)
{ declareClass(class, &winprinter_decls);

  WinPrinters = globalObject(CtoName("win_printers"), ClassChain, EAV);

  at_pce_exit(closeAllWinPrinters, ATEXIT_FIFO);

  succeed;
}
