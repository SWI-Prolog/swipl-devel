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

#define WinError() strName(WinStrError(GetLastError()))

Class ClassWinPrinter;			/* the class handle */
typedef struct win_printer *WinPrinter;

typedef struct
{ HDC		hdc;			/* Device hdc */
  PAGESETUPDLG	info;			/* Window page setup data */
} ws_printer, *WsPrinter;


NewClass(win_printer)			/* class structure */
  Name	    map_mode;			/* Mapping mode */
  Name	    job_name;			/* Job Name */
  Int	    job;			/* Job Id */
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
  assign(prt, origin,	  newObject(ClassPoint, 0));

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
{ PAGESETUPDLG *psd = &prt->ws_ref->info;
  HWND hwnd = (isDefault(fr) ? 0 : getHwndFrame(fr));
  Name mm;

  resetDataWinPrinter(prt);

  psd->lStructSize = sizeof(PAGESETUPDLG); 
  psd->hwndOwner = hwnd;
/*psd->hDevMode; 
  psd->hDevNames; 
  psd->Flags; 
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

  if ( !PageSetupDlg(psd) )
    fail;				/* canceled or error */

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


  if ( psd->hDevMode )
  { DEVMODE *dmode = GlobalLock(psd->hDevMode);
    prt->ws_ref->hdc = CreateDC("WINSPOOL", /* NT, NULL for Windows-95 */
				dmode->dmDeviceName,
				NULL,
				psd->hDevMode);
    GlobalUnlock(psd->hDevMode);
  }

  if ( !prt->ws_ref->hdc )
  { Cprintf("Failed to open Printer DC: %s\n", WinError());
    fail;
  }

  if ( (mm = mapModeToName(GetMapMode(prt->ws_ref->hdc))) )
    assign(prt, map_mode, mm);

  succeed;
}


static status
openWinPrinter(WinPrinter prt)
{ if ( prt->ws_ref->hdc )
  { DOCINFO di;
    int job;

    di.cbSize       = sizeof(di);
    di.lpszDocName  = strName(prt->job_name);
    if ( instanceOfObject(prt->device, ClassFile) )
    { Name name = getOsNameFile(prt->device);
      char osname[MAXPATHLEN];

      _xos_os_filename(strName(name), osname);
      di.lpszOutput = osname;
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

static status
resolutionWinPrinter(WinPrinter prt, Any resolution)
{ if ( prt->resolution != resolution )
  { assign(prt, resolution, resolution);

    if ( isDefault(resolution) )
      return mapModeWinPrinter(prt, NAME_text);
    else if ( isInteger(resolution) )
      mapModeWinPrinter(prt, NAME_isotropic);
    else
      mapModeWinPrinter(prt, NAME_anisotropic);

    if ( prt->ws_ref->hdc )
    { int w, h;

      if ( isInteger(resolution) )
      { int pw = GetDeviceCaps(prt->ws_ref->hdc, PHYSICALWIDTH);
	int ph = GetDeviceCaps(prt->ws_ref->hdc, PHYSICALHEIGHT);
      
	h = valInt(resolution);
	w = (h*pw)/ph;
      } else
      { Size sz = resolution;

	h = valInt(sz->h);
	w = valInt(sz->w);
      }

      SetWindowExtEx(prt->ws_ref->hdc, w, -h, NULL);
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

      SetViewportExtEx(prt->ws_ref->hdc, x, y, NULL);
    }
  }

  succeed;
}


		 /*******************************
		 *	       INFO		*
		 *******************************/

static Size
getSizeWinPrinter(WinPrinter prt)
{ if ( prt->ws_ref->hdc )
  { int w = GetDeviceCaps(prt->ws_ref->hdc, PHYSICALWIDTH);
    int h = GetDeviceCaps(prt->ws_ref->hdc, PHYSICALHEIGHT);

    answer(answerObject(ClassSize, toInt(w), toInt(h), 0));
  }

  fail;
}


static Point
getOffsetWinPrinter(WinPrinter prt)
{ if ( prt->ws_ref->hdc )
  { int x = GetDeviceCaps(prt->ws_ref->hdc, PHYSICALOFFSETX);
    int y = GetDeviceCaps(prt->ws_ref->hdc, PHYSICALOFFSETY);

    answer(answerObject(ClassPoint, toInt(x), toInt(y), 0));
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
      SetMapMode(prt->ws_ref->hdc, nameToMapMode(mode));
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

    bb = answerObject(ClassArea, 0);
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
     NAME_dimension, "Top-left-most position that can be printed")
};

/* Resources */

static classvardecl rc_winprinter[] =
{ RC(NAME_path, "string",
     "\".:prt:~/lib/prt:$PCEHOME/prt:\"",
     "Search path for loading Windows metafiles")
};

/* Class Declaration */

static Name winprinter_termnames[] = { NAME_file };

ClassDecl(winprinter_decls,
          var_winprinter, send_winprinter, get_winprinter, rc_winprinter,
          1, winprinter_termnames,
          "$Rev$");


status
makeClassWinPrinter(Class class)
{ declareClass(class, &winprinter_decls);

  WinPrinters = globalObject(CtoName("win_printers"), ClassChain, 0);

  at_pce_exit(closeAllWinPrinters, ATEXIT_FIFO);

  succeed;
}
