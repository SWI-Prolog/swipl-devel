/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

#include "include.h"
#include <h/unix.h>

#ifndef MAXPATHLEN
#define MAXPATHLEN 1024
#endif

Class ClassWinMF;			/* the class handle */

typedef struct win_mf *WinMF;		/* pointer type */

NewClass(win_mf)			/* class structure */
  ABSTRACT_GRAPHICAL
  FileObj	file;			/* Associated file */
  StringObj	summary;		/* Summary description */
  HENHMETAFILE  hmf;			/* The metafile handle */
End;

static status getDimensionsWinMF(WinMF mf);
static status getMhfWinMF(WinMF mf);

static Chain WinMetaFiles;		/* @win_metafiles */

static status
initialiseWinMF(WinMF mf, FileObj f)
{ initialiseGraphical(mf, ZERO, ZERO, ZERO, ZERO);

  if ( isDefault(f) )
    f = (FileObj) NIL;

  assign(mf, file, f);
  mf->hmf = NULL;

  if ( notNil(f) )
  { if ( !getDimensionsWinMF(mf) )
      fail;
  }

  return prependChain(WinMetaFiles, mf);
}


static status
closeWinMF(WinMF mf)
{ if ( mf->hmf )
  { DeleteEnhMetaFile(mf->hmf);

    mf->hmf = NULL;
  }

  succeed;
}


static status
unlinkWinMF(WinMF mf)
{ closeWinMF(mf);
  deleteChain(WinMetaFiles, mf);

  succeed;
}


static void
closeAllWinMF(void)
{ if ( WinMetaFiles )
  { Cell cell;

    for_cell(cell, WinMetaFiles)
    { WinMF mf = cell->value;

      closeWinMF(mf);
    }
  }
}


void
set_area_from_rectl(Area a, RECTL *r)
{ assign(a, x, toInt(r->left));
  assign(a, y, toInt(r->top));
  assign(a, w, toInt(r->right - r->left));
  assign(a, h, toInt(r->bottom - r->top));
}


static status
getDimensionsWinMF(WinMF mf)
{ UINT hdrsize;

  if ( !mf->hmf )
  { if ( !getMhfWinMF(mf) )
      fail;
  }

  if ( (hdrsize = GetEnhMetaFileHeader(mf->hmf, 0, NULL)) )
  { ENHMETAHEADER *hdr = alloca(hdrsize);
    
    GetEnhMetaFileHeader(mf->hmf, hdrsize, hdr);
    set_area_from_rectl(mf->area, &hdr->rclBounds);
    if ( hdr->nDescription )
    { char *s = (char *)hdr + hdr->offDescription;
      string str;

      str_set_n_ascii(&str, hdr->nDescription, s);
      assign(mf, summary, StringToString(&str));
    }
    
    succeed;
  }

  fail;
}


static status
getMhfWinMF(WinMF mf)
{ if ( !mf->hmf && notNil(mf->file) )
  { CharArray path = getResourceValueObject(mf, NAME_path);

    if ( findFile(mf->file, path ? path : (CharArray) DEFAULT, NAME_read) )
    { char *rawfn = strName(getOsNameFile(mf->file));
      char fn[MAXPATHLEN];

      _xos_os_filename(rawfn, fn);

      mf->hmf = GetEnhMetaFile(fn);
      if ( mf->hmf )
	succeed;
      else
      { HMETAFILE h0;
	status rval = FAIL;

	DEBUG(NAME_winMetafile,
	      Cprintf("Not an enhanced metafile, trying to convert ..\n"));

	if ( (h0 = GetMetaFile(fn)) )
	{ int len;

	  if ( (len = GetMetaFileBitsEx(h0, 0, NULL)) )
	  { void *data = pceMalloc(len);
	    METAFILEPICT context;
	    HENHMETAFILE hmf;

	    context.mm = MM_ANISOTROPIC;
	    context.xExt = 100;		/* ??? */
	    context.yExt = 100;
	    context.hMF  = h0;

	    GetMetaFileBitsEx(h0, len, data);
	    if ( (hmf = SetWinMetaFileBits(len, data, NULL, &context)) )
	    { mf->hmf = hmf;
	      rval = SUCCEED;
	    }

	    pceFree(data);
	  }
	       
	  DeleteMetaFile(h0);
	} else
	  errorPce(mf, NAME_winMetafile, CtoName("GetMetaFile"), APIError());

	return rval;
      }
    }
  }

  fail;
}


static status
RedrawAreaWinMF(WinMF mf, Area a)
{ int x, y, w, h;

  initialiseDeviceGraphical(mf, &x, &y, &w, &h);

  if ( mf->hmf )
    r_winmf(mf->hmf, x, y, w, h);

  return RedrawAreaGraphical(mf, a);
}

		 /*******************************
		 *	  PAINTING IN IT	*
		 *******************************/

static status
drawInWinMF(WinMF mf, Any obj, Point pos)
{ Int oldx, oldy;
  Device dev;
  char *fn;
  char *descr;

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

  if ( isNil(mf->file) )
    fn = NULL;
  else
    fn = strName(getOsNameFile(mf->file));

  if ( isNil(mf->summary) )
    descr = NULL;
  else
    descr = strName(mf->summary);

  CHANGING_GRAPHICAL(mf,
    d_winmf(fn, descr);
    if ( instanceOfObject(obj, ClassGraphical) )
    { Graphical gr = obj;

      ComputeGraphical(gr);
      RedrawArea(gr, gr->area);
    } else
    { Chain ch = obj;
      Cell cell;

      for_cell(cell, ch)
      { if ( instanceOfObject(cell->value, ClassGraphical) )
	{ Graphical gr = obj;

	  ComputeGraphical(gr);
	  RedrawArea(gr, gr->area);
	}
      }
    }
    mf->hmf = d_winmfdone();
    if ( mf->hmf )
      getDimensionsWinMF(mf);
    changedEntireImageGraphical(mf));

  if ( notDefault(oldx) )
  { Graphical gr = obj;

    setGraphical(gr, oldx, oldy, DEFAULT, DEFAULT);
    gr->device = dev;
  }

  succeed;
}


static status
saveWinMF(WinMF mf, FileObj file, Name format)
{ char *rawfn = strName(getOsNameFile(file));
  char fn[MAXPATHLEN];

  if ( !mf->hmf )
    fail;

  _xos_os_filename(rawfn, fn);

  if ( format == NAME_wmf ||		/* Windows 3.1 format */
       (isDefault(format) &&
	suffixCharArray((CharArray)file->name,
			(CharArray)CtoName(".wmf"),
			ON)) )
  { void *data = NULL;
    int len, l2;
    HMETAFILE ohmf, ohmf2;
					/* get old format bits */
    if ( !(len = GetWinMetaFileBits(mf->hmf, 0, NULL,
				    MM_ANISOTROPIC, NULL)) )
      return errorPce(mf, NAME_winMetafile,
		      CtoName("GetWinMetaFileBits"), APIError());

    data = pceMalloc(len);
    l2 = GetWinMetaFileBits(mf->hmf, len, data, MM_ANISOTROPIC, NULL);
    assert(l2 == len);
					/* make old format metafile */
    ohmf = SetMetaFileBitsEx(l2, data);
    if ( !(ohmf2 = CopyMetaFile(ohmf, fn)) )
    { pceFree(data);
      DeleteMetaFile(ohmf);
      return errorPce(mf, NAME_winMetafile,
		      CtoName("CopyMetaFile"), APIError());
    }
    pceFree(data);
    DeleteMetaFile(ohmf2);
    DeleteMetaFile(ohmf);

    succeed;
  } else				/* save as enhanced format */
  { HENHMETAFILE h2;

    if ( (h2 = CopyEnhMetaFile(mf->hmf, fn)) )
    { DeleteEnhMetaFile(h2);
      succeed;
    } else
      return errorPce(mf, NAME_winMetafile,
		      CtoName("CopyEnhMetaFile"), APIError());
  }
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_drawIn[] =
        { "graphical|chain", "at=[point]" };
static char *T_save[] =
	{ "in=file", "format=[{wmf,emf}]" };

/* Instance Variables */

static vardecl var_winmf[] =
{ IV(NAME_file, "file*", IV_GET, 
     NAME_storage, "Associated .WMF file"),
  IV(NAME_summary, "string*", IV_BOTH, 
     NAME_manual, "Description associated with the file"), /* TBD:group */
  IV(NAME_handle, "alien:HENHMETAFILE", IV_NONE,
     NAME_storage, "Handle to the Win32 metafile"),
};

/* Send Methods */

static senddecl send_winmf[] =
{ SM(NAME_initialise, 1, "[file]*", initialiseWinMF,
     DEFAULT, "Create box from width and height"),
  SM(NAME_unlink, 0, NULL, unlinkWinMF,
     DEFAULT, "Remove from @win_metafiles"),
  SM(NAME_drawIn, 2, T_drawIn, drawInWinMF,
     NAME_copy, "Paint graphical in metafile"),
  SM(NAME_save, 2, T_save, saveWinMF,
     NAME_file, "Save contents in specified file"),
};

/* Get Methods */

#define get_winmf NULL
/*
static getdecl get_winmf[] =
{ 
};
*/

/* Resources */

static resourcedecl rc_winmf[] =
{ RC(NAME_path, "string",
     "\".:mf:~/lib/mf:$PCEHOME/mf:\"",
     "Search path for loading Windows metafiles")
};

/* Class Declaration */

static Name winmf_termnames[] = { NAME_file };

ClassDecl(winmf_decls,
          var_winmf, send_winmf, get_winmf, rc_winmf,
          1, winmf_termnames,
          "$Rev$");


status
makeClassWinMF(Class class)
{ declareClass(class, &winmf_decls);

  setRedrawFunctionClass(class, RedrawAreaWinMF);
  WinMetaFiles = globalObject(NAME_winMetafiles, ClassChain, 0);

  at_pce_exit(closeAllWinMF, ATEXIT_FIFO);

  succeed;
}
