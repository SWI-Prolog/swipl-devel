/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

#include "include.h"
#include <h/unix.h>

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Taken from MSVC20/samples/win32/mfedit.h.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define META32_SIGNATURE        0x464D4520      // ' EMF'
#define ALDUS_ID		0x9AC6CDD7

typedef struct
{ DWORD		key;
  WORD		hmf;
  SMALL_RECT    bbox;
  WORD    	inch;
  DWORD   	reserved;
  WORD    	checksum;
} APMFILEHEADER;
typedef APMFILEHEADER * PAPMFILEHEADER;
#define APMSIZE 22

#ifndef MAXPATHLEN
#define MAXPATHLEN 1024
#endif

Class ClassWinMF;			/* the class handle */

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

  return unlinkGraphical((Graphical) mf);
}


static void
closeAllWinMF(int rval)
{ if ( WinMetaFiles )
  { Cell cell;

    for_cell(cell, WinMetaFiles)
    { WinMF mf = cell->value;

      closeWinMF(mf);
    }
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Store/load windows metafile to/form file. Format:

	* If the metafile has a file, just the slots and 'X'
	* Otherwise, the slots, 'E', <size>, metafilebitx
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static status
storeWinMF(WinMF mf, FileObj file)
{ TRY(storeSlotsObject(mf, file));

  if ( isNil(mf->file) && mf->hmf )
  { UINT size;
    
    if ( (size = GetEnhMetaFileBits(mf->hmf, 0, NULL)) )
    { LPBYTE data = pceMalloc(size);
      GetEnhMetaFileBits(mf->hmf, size, data);
      putc('E', file->fd);
      putstdw(size, file->fd);
      fwrite(data, sizeof(char), size, file->fd);
      pceFree(data);

      succeed;
    }
  }

  putc('X', file->fd);

  succeed;
}


static status
loadFdWinMF(WinMF mf, IOSTREAM *fd, ClassDef def)
{ TRY( loadSlotsObject(mf, fd, def) );
  mf->hmf = NULL;

  switch(Sgetc(fd))
  { case 'X':
      if ( notNil(mf->file) )
	getDimensionsWinMF(mf);
      break;
    case 'E':
    { UINT size = loadWord(fd);
      LPBYTE data = pceMalloc(size);
      Sfread(data, sizeof(char), size, fd);
      mf->hmf = SetEnhMetaFileBits(size, data);
      pceFree(data);
      break;
    }
    default:
      assert(0);
  }

  return prependChain(WinMetaFiles, mf);
}


static status
copyWinMF(WinMF to, WinMF from)
{ copyGraphical(to, from);

  assign(to, file, NIL);		/* ??? */
  assign(to, summary, from->summary);

  if ( from->hmf )
    to->hmf = CopyEnhMetaFile(from->hmf, NULL);
  else
    from->hmf = NULL;

  succeed;
}


static status
cloneWinMF(WinMF from, WinMF clone)
{ clonePceSlots(from, clone);
  if ( from->hmf )
    clone->hmf = CopyEnhMetaFile(from->hmf, NULL);

  succeed;
}


void
set_area_from_rectl(Area a, RECTL *r)
{ assign(a, x, toInt(r->left));
  assign(a, y, toInt(r->top));
  assign(a, w, toInt(1 + r->right - r->left));
  assign(a, h, toInt(1 + r->bottom - r->top));
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


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Taken from MSVC20/samples/win32/mfedit.c and modified to fit into XPCE's
format.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static HENHMETAFILE
hemfLoadMetafile(WinMF mf, const char *szFile)
{ HMETAFILE       ghmf, hmf;
  UINT            uiSize;
  LPVOID          pvData;
  HDC             hDCDrawSurf;
  HENHMETAFILE    hemf = NULL;

  HANDLE          hFile, hMapFile;
  LPVOID          pMapFile;
  LPENHMETAHEADER pemh;

  BOOL            bSuccess;

  bSuccess = TRUE;

  if ( (hFile=CreateFile(szFile,
			 GENERIC_READ, FILE_SHARE_READ, NULL,
			 OPEN_EXISTING, FILE_ATTRIBUTE_READONLY,
			 NULL)) == (HANDLE)-1 )
  { errorPce(mf, NAME_winMetafile, NAME_open, APIError());
    return 0L;
  }

  /* Create a map file of the opened file */

  if ( (hMapFile=CreateFileMapping(hFile, NULL,
				   PAGE_READONLY, 0, 0, "MapF")) == NULL )
  { errorPce(mf, NAME_winMetafile, NAME_map, APIError());
    bSuccess = FALSE;
    goto ErrorExit1;
  }

  /* Map a view of the whole file */

  if ( (pMapFile = MapViewOfFile(hMapFile, FILE_MAP_READ, 0, 0, 0)) == NULL)
  { errorPce(mf, NAME_winMetafile, NAME_view, APIError());
    bSuccess = FALSE;
    goto ErrorExit2;
  }

  /* First check that if it is an enhanced metafile */

  pemh = (LPENHMETAHEADER) pMapFile;
  if ( pemh->dSignature == META32_SIGNATURE )
  { hemf = GetEnhMetaFile(szFile);
    goto HLM_EXIT;
  }

  /* If it has an ALDUS header skip it
     Notice: APMSIZE is used because the HANDLE and RECT of the structure
     depends on the environment
  */
  if ( *((LPDWORD)pemh) == ALDUS_ID )
  { //METAFILEPICT    mfp;
    PAPMFILEHEADER aldushdr = (PAPMFILEHEADER) pMapFile;

    DEBUG(NAME_winMetafile,
	  Cprintf("ALDUS Header: inch = %d, size = %d x %d\n",
		  aldushdr->inch,
		  aldushdr->bbox.Right,
		  aldushdr->bbox.Bottom));

    uiSize = *((LPDWORD) ((PBYTE)pMapFile + APMSIZE + 6));
    hDCDrawSurf = GetDC(NULL);

    /* Notice: mtSize is size of the file in word.
       if LPMETAFILEPICT is NULL
       MM_ANISOTROPIC mode and default device size will be used.
    */
    hemf = SetWinMetaFileBits(uiSize*2L,
			      (PBYTE)pMapFile + APMSIZE, hDCDrawSurf, NULL);
#if 0
    switch ( aldushdr->inch )
    { /* !!! End up in an upside down image */
      case 1440:
	mfp.mm = MM_TWIPS;
        break;
      case 2540:
	mfp.mm = MM_HIMETRIC;
        break;
      case 254:
	mfp.mm = MM_LOMETRIC;
        break;
      case 1000:
	mfp.mm = MM_HIENGLISH;
        break;
      case 100:
	mfp.mm = MM_LOENGLISH;
        break;
      default:
	mfp.mm = MM_ANISOTROPIC;
        mfp.xExt = (((PAPMFILEHEADER) pMapFile)->bbox.Right -
		    ((PAPMFILEHEADER) pMapFile)->bbox.Left) *
			((PAPMFILEHEADER) pMapFile)->inch * 2560;
	mfp.yExt = (((PAPMFILEHEADER) pMapFile)->bbox.Bottom -
		    ((PAPMFILEHEADER) pMapFile)->bbox.Top) *
			((PAPMFILEHEADER) pMapFile)->inch * 2560;
	break;
    }
    mfp.hMF = 0;
    hemf = SetWinMetaFileBits(uiSize*2L,
			      (PBYTE)pMapFile + APMSIZE, hDCDrawSurf, &mfp);
#endif

    if ( !hemf )
    { errorPce(mf, NAME_winMetafile,
	       CtoName("SetWinMetaFileBits"), APIError());
    }

    ghmf = SetMetaFileBitsEx(uiSize*2L, (PBYTE)pMapFile + APMSIZE);
    if ( !ghmf )
    { errorPce(mf, NAME_winMetafile,
	       CtoName("SetMetaFileBitsEx"), APIError());
    }

    ReleaseDC(NULL, hDCDrawSurf);

    goto HLM_EXIT;
  }

  /* It is a Windows 3x format metafile (hopefully) */

  if (!(hmf = GetMetaFile((LPCSTR)szFile)))
  { errorPce(mf, NAME_winMetafile, CtoName("GetMetaFile"), APIError());
    bSuccess = FALSE;
    goto ErrorExit3;
  }

  if ( !(uiSize = GetMetaFileBitsEx(hmf, 0, NULL)) )
  { errorPce(mf, NAME_winMetafile, CtoName("GetMetaFileBitsEx-1"), APIError());
    return NULL;
  }

  if ((pvData = (LPVOID) LocalAlloc(LMEM_FIXED, uiSize)) == NULL)
  { errorPce(mf, NAME_winMetafile, CtoName("alloc"), APIError());
    bSuccess = FALSE;
    goto ErrorExit3;
  }

  if (!(uiSize = GetMetaFileBitsEx(hmf, uiSize, pvData)))
  { errorPce(mf, NAME_winMetafile, CtoName("GetMetaFileBitsEx-2"), APIError());
    bSuccess = FALSE;
    goto ErrorExit3;
  }

  DeleteMetaFile(hmf);

  hDCDrawSurf = GetDC(NULL);
  hemf = SetWinMetaFileBits(uiSize, (LPBYTE)pvData, hDCDrawSurf, NULL);
  ghmf = SetMetaFileBitsEx(uiSize, (LPBYTE) pvData);

  LocalFree(pvData);

  ReleaseDC(NULL, hDCDrawSurf);

HLM_EXIT:
ErrorExit3:
  UnmapViewOfFile(pMapFile);

ErrorExit2:
  CloseHandle(hMapFile);
ErrorExit1:
  CloseHandle(hFile);

  if (bSuccess)
    return hemf;
  else
    return 0L;
}


static status
getMhfWinMF(WinMF mf)
{ if ( !mf->hmf && notNil(mf->file) )
  { CharArray path = getClassVariableValueObject(mf, NAME_path);

    if ( findFile(mf->file, path ? path : (CharArray) DEFAULT, NAME_read) )
    { char *rawfn = strName(getOsNameFile(mf->file));

#if O_XOS
      char fn[MAXPATHLEN];
      _xos_os_filename(rawfn, fn);
#else
      char *fn = rawfn;
#endif

      if ( (mf->hmf = hemfLoadMetafile(mf, fn)) )
	succeed;
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

  if ( isNil(mf->file) )
    fn = NULL;
  else
    fn = strName(getOsNameFile(mf->file));

  if ( isNil(mf->summary) )
    descr = NULL;
  else					/* application\0summary\0\0 */
  { int slen = strlen(strName(mf->summary));
    char *s;

    descr = alloca(slen + strlen("XPCE") + 3);
    strcpy(descr, strName(mf->summary));
    s = descr + slen + 1;
    strcpy(s, "XPCE");
    s += strlen("XPCE") + 1;
    *s++ = EOS;
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


  CHANGING_GRAPHICAL(mf,
    d_winmf(fn,
	    valInt(bb->x), valInt(bb->y), valInt(bb->w), valInt(bb->h),
	    descr);

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

    mf->hmf = d_winmfdone();
    copyArea(mf->area, bb);
    doneObject(bb);
    changedEntireImageGraphical(mf));

  if ( notDefault(oldx) )
  { Graphical gr = obj;

    setGraphical(gr, oldx, oldy, DEFAULT, DEFAULT);
    gr->device = dev;
  }

  succeed;
}


static status
saveALDUS(WinMF mf, HMETAFILE ghmf, FileObj file,
	  int width, int height, int mapping)
{ UINT            uiSize;
  DWORD           dwHigh, dwLow;
  int		  rval = TRUE;

  uiSize = GetMetaFileBitsEx(ghmf, 0, NULL);
  dwHigh = 0;
  dwLow  = uiSize + APMSIZE;

  if ( !send(file, NAME_kind, NAME_binary, EAV) ||
       !send(file, NAME_open, NAME_write, EAV) )
    fail;

  if ( uiSize )
  { APMFILEHEADER   AldHdr;
    WORD	   *p;
    void *	    buf;

    AldHdr.key = ALDUS_ID;
    AldHdr.hmf = 0;                                 // Unused; must be zero
    AldHdr.bbox.Left   = 0;                         // in metafile units
    AldHdr.bbox.Top    = 0;

    switch (mapping)
    { case MM_HIENGLISH:
	AldHdr.inch = 1000;
        AldHdr.bbox.Right  = (SHORT)width;
	AldHdr.bbox.Bottom = (SHORT)height;
	break;
      case MM_HIMETRIC:
	AldHdr.inch = 1440;
        AldHdr.bbox.Right  = (SHORT)(width / 2540 * 1440);
	AldHdr.bbox.Bottom = (SHORT)(height / 2540 * 1440);
	break;
      case MM_LOENGLISH:
	AldHdr.inch = 100;
        AldHdr.bbox.Right  = (SHORT)width;
	AldHdr.bbox.Bottom = (SHORT)height;
	break;
      case MM_LOMETRIC:
	AldHdr.inch = 254;
        AldHdr.bbox.Right  = (SHORT)width;
	AldHdr.bbox.Bottom = (SHORT)height;
	break;
      case MM_TEXT:
      { HDC hdc = GetDC(NULL);

	AldHdr.inch = (WORD) ((float)GetDeviceCaps(hdc, HORZRES) * 25.4 /
			      (float)GetDeviceCaps(hdc, HORZSIZE));
        AldHdr.bbox.Right  = (SHORT)width;
	AldHdr.bbox.Bottom = (SHORT)height;

	DEBUG(NAME_winMetafile,
	      Cprintf("HORZRES = %d, HORZSIZE = %d, inch = %d right = %d\n",
		      GetDeviceCaps(hdc, HORZRES),
		      GetDeviceCaps(hdc, HORZSIZE),
		      AldHdr.inch,
		      AldHdr.bbox.Right));

	ReleaseDC(NULL, hdc);
	break;
      }
      case MM_TWIPS:
	AldHdr.inch = 1440;
        AldHdr.bbox.Right  = (SHORT)width;
	AldHdr.bbox.Bottom = (SHORT)height;
	break;
      default:
	AldHdr.inch = 1440;
        AldHdr.bbox.Right  = (SHORT)((width  * 1440) / 2540);
	AldHdr.bbox.Bottom = (SHORT)((height * 1440) / 2540);
	break;
    }


    AldHdr.reserved = 0;
    AldHdr.checksum = 0;
    for (p = (WORD *)&AldHdr; p < (WORD *)&(AldHdr.checksum); ++p)
      AldHdr.checksum ^= *p;

    if ( fwrite(&AldHdr, APMSIZE, 1, file->fd) != 1 )
    { reportErrorFile(file);
      closeFile(file);
      fail;
    }
    
    buf = pceMalloc(uiSize);
    GetMetaFileBitsEx(ghmf, uiSize, buf);
    if ( fwrite(buf, sizeof(char), uiSize, file->fd) != uiSize )
    { reportErrorFile(file);
      closeFile(file);
      pceFree(buf);

      fail;
    }
    pceFree(buf);

    return closeFile(file);
  }

  return rval;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Get a Windows 3.x format metafile  from   the  XPCE object. I'm not sure
whether the allocated  memory  block  should   be  freed  or  not.  Some
analogous cases in  mfedit  indicate   me  SetMetaFileBitsEx()  probably
copies the data, so the code below should be fine.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static HMETAFILE
convert_enh_metafile(WinMF mf, HDC hdc)
{ void *data = NULL;
  int len, l2;
  int fnMapMode = MM_ANISOTROPIC;
  int omap = SetMapMode(hdc, fnMapMode);
  HMETAFILE hmf;
					/* get old format bits */
    
  if ( !(len = GetWinMetaFileBits(mf->hmf, 0, NULL, fnMapMode, hdc)) )
  { errorPce(mf, NAME_winMetafile,
	     CtoName("GetWinMetaFileBits"), APIError());
    return NULL;
  }

  data = pceMalloc(len);
  l2 = GetWinMetaFileBits(mf->hmf, len, data, fnMapMode, hdc);
  SetMapMode(hdc, omap);
  assert(l2 == len);
					/* make old format metafile */
  hmf = SetMetaFileBitsEx(l2, data);
  pceFree(data);

  return hmf;
}



static status
saveWinMF(WinMF mf, FileObj file, Name format)
{ char *rawfn = strName(getOsNameFile(file));
#if O_XOS
  char fn[MAXPATHLEN];
  _xos_os_filename(rawfn, fn);
#else
  char *fn = rawfn;
#endif

  if ( !mf->hmf )
    fail;


  if ( isDefault(format) )
  { if ( suffixCharArray((CharArray)file->name,
			 (CharArray)CtoName(".wmf"),
			 ON) )
      format = NAME_aldus;
    else if ( suffixCharArray((CharArray)file->name,
			      (CharArray)CtoName(".emf"),
			      ON) )
      format = NAME_emf;
    else
      format = getClassVariableValueObject(mf, NAME_format);
  }

  if ( format == NAME_wmf || format == NAME_aldus )
  { HDC hdc = GetDC(NULL);
    HMETAFILE ohmf = convert_enh_metafile(mf, hdc);
    HMETAFILE ohmf2 = NULL;
    status rval = SUCCEED;

    ReleaseDC(NULL, hdc);

    if ( format == NAME_wmf )
    { if ( !(ohmf2 = CopyMetaFile(ohmf, fn)) )
      { DeleteMetaFile(ohmf);
	return errorPce(mf, NAME_winMetafile,
			CtoName("CopyMetaFile"), APIError());
      }
    } else /* ALDUS */
      rval = saveALDUS(mf, ohmf, file,
		       valInt(mf->area->w), valInt(mf->area->h),
		       MM_TEXT);

    DeleteMetaFile(ohmf2);
    DeleteMetaFile(ohmf);

    return rval;
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
		 *	 WINDOWS CUT/PASTE	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Copy a metafile to the windows clipboard.  I'm not really sure about the
immediate delete, but that is what mfedit is doing. Also, shouldn't this
use GlobalAlloc() somehow??
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

status
ws_on_clipboard_metafile(WinMF mf, Name type)
{ if ( mf->hmf )
  { if ( type == NAME_emf )
    { HENHMETAFILE hmftmp = CopyEnhMetaFile(mf->hmf, NULL);

      if ( hmftmp )
      { SetClipboardData(CF_ENHMETAFILE, hmftmp);
	DeleteEnhMetaFile(hmftmp);

	succeed;
      }
    } else /* if ( type == NAME_wmf ) */
    { HGLOBAL hmem;
      LPMETAFILEPICT  lpmfp;
      HDC hdc  = GetDC(NULL);
      int hmm  = GetDeviceCaps(hdc, HORZSIZE);
      int hpxl = GetDeviceCaps(hdc, HORZRES);
      int vmm  = GetDeviceCaps(hdc, VERTSIZE);
      int vpxl = GetDeviceCaps(hdc, VERTRES);

      if ( !(hmem = GlobalAlloc(GMEM_ZEROINIT|GMEM_MOVEABLE|GMEM_DDESHARE,
				sizeof(METAFILEPICT))) )
      { errorPce(mf, NAME_winMetafile, CtoName("GlobalAlloc"), APIError());
	fail;
      }

      lpmfp = (LPMETAFILEPICT)GlobalLock(hmem);
      lpmfp->mm = MM_ANISOTROPIC;

      lpmfp->xExt = (valInt(mf->area->w)*hmm*100)/hpxl;
      lpmfp->yExt = (valInt(mf->area->h)*vmm*100)/vpxl;
      lpmfp->hMF  = convert_enh_metafile(mf, hdc); /* Reclaim??? */

      GlobalUnlock(hmem);
      SetClipboardData(CF_METAFILEPICT, hmem);

      ReleaseDC(NULL, hdc);
    }
  }

  fail;
}


WinMF
CtoWinMetafile(HENHMETAFILE hmf)
{ WinMF mf = answerObject(ClassWinMF, EAV);

  mf->hmf = hmf;
  getDimensionsWinMF(mf);

  answer(mf);
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_drawIn[] =
        { "graphical|chain", "at=[point]" };
static char *T_save[] =
	{ "in=file", "format=[{aldus,wmf,emf}]" };

/* Instance Variables */

static vardecl var_winmf[] =
{ IV(NAME_file, "file*", IV_GET, 
     NAME_storage, "Associated physical file"),
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
  SM(NAME_copy, 1, "win_metafile", copyWinMF,
     NAME_copy, "Copy the argument to the receiver"),
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

static classvardecl rc_winmf[] =
{ RC(NAME_path, "string",
     "\".:mf:~/lib/mf:$PCEHOME/mf:\"",
     "Search path for loading Windows metafiles"),
  RC(NAME_format, "{emf,wmf,aldus}",
     "aldus",
     "Default save format")
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
  WinMetaFiles = globalObject(NAME_winMetafiles, ClassChain, EAV);
  setLoadStoreFunctionClass(class, loadFdWinMF, storeWinMF);
  setCloneFunctionClass(class, cloneWinMF);

  at_pce_exit(closeAllWinMF, ATEXIT_FIFO);

  succeed;
}
