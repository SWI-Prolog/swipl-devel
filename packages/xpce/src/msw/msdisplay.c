/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

#include "include.h"
#include <h/interface.h>
#include <h/unix.h>

#ifdef USE_RLC_FUNCTIONS
static void init_render_hooks(void);
#else
#define init_render_hooks()
#endif

void
ws_flush_display(DisplayObj d)
{ 
}


void
ws_synchronise_display(DisplayObj d)
{ MSG msg;

  if ( PeekMessage(&msg, NULL, 0, 0, PM_REMOVE) )
  { TranslateMessage(&msg);
    DispatchMessage(&msg);
  }
}


void
ws_bell_display(DisplayObj d, int volume)
{ MessageBeep(MB_ICONEXCLAMATION);
}


void
ws_get_size_display(DisplayObj d, int *w, int *h)
{ HDC  hdc = GetDC(NULL);

  *w = GetDeviceCaps(hdc, HORZRES);
  *h = GetDeviceCaps(hdc, VERTRES);

  ReleaseDC(NULL, hdc);
}


Name
ws_get_visual_type_display(DisplayObj d)
{ if ( ws_depth_display(d) == 1 )
    return NAME_monochrome;
  else
    return NAME_pseudoColour;
}


int
ws_depth_display(DisplayObj d)
{ HDC  hdc = GetDC(NULL);
  int depth = GetDeviceCaps(hdc, BITSPIXEL);
  ReleaseDC(NULL, hdc);

  return depth;
}


void
ws_activate_screen_saver(DisplayObj d)
{
}


void
ws_deactivate_screen_saver(DisplayObj d)
{
}


void
ws_init_display(DisplayObj d)
{
}


status
ws_legal_display_name(char *s)
{ succeed;
}


status
ws_opened_display(DisplayObj d)
{ if ( d->ws_ref )
    succeed;

  fail;
}


void
ws_open_display(DisplayObj d)
{ d->ws_ref = (WsRef) 1;	/* just flag; nothing to do yet */
}


void
ws_quit_display(DisplayObj d)
{ exitDraw();
}

#ifdef HOOK_BASED_MOUSE_HANDLING
#ifdef __WATCOMC__
		 /*******************************
		 *	  MOUSE TRACKING	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The %@^%#@& MS-Windows system does  not   tell  the application when the
mouse enters/leaves some window.  News articles   and many mails later I
was hardly any further.  Anyway, the following appears to work:

Use SetWindowsHookEx() to register a WH_MOUSE hook.  When the hwnd field
of the MOUSEHOOKSTRUCT changes, SendMessage()   a  user-defined event to
the window left and entered.  Now, if you do that in the app itself, you
will not see any result if the mouse leaves towards another application.

Therefore we first try to load the pcewh.dll module which does the same,
but in a dll, so it can do the   job system-wide.  If this fails we will
do the job locally, which in any case is better than not at all.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static HHOOK defhook	  = NULL;
static HINSTANCE pcewhdll = NULL;
static FARPROC PceWhEntry = NULL;
static HINDIR PceWhIH	  = NULL;
static HWND cwin;
static int  cwin_deleted;

static DWORD _export FAR PASCAL
xpce_mouse_hook(int code, WPARAM wParam, LPARAM lParam)
{ if ( code >= 0 )
  { MOUSEHOOKSTRUCT FAR* data = lParam;

    if ( data->hwnd != cwin )
    { if ( cwin && !cwin_deleted )
	SendMessage(cwin, WM_WINEXIT, 0, 0L);
      cwin         = data->hwnd;
      cwin_deleted = FALSE;
      if ( cwin )
	SendMessage(cwin, WM_WINENTER, 0, 0L);
    }
  }

  return CallNextHookEx(defhook, code, wParam, lParam);
}


static void
unhook_xpce_mouse_hook()
{ if ( defhook )
    UnhookWindowsHookEx(defhook);
}


static void
PceWhAddTask(HTASK task)
{ if ( PceWhEntry )
    InvokeIndirectFunction(PceWhIH, task, 1);
}


static void
PceWhDeleteTask(HTASK task)
{ if ( PceWhEntry )
    InvokeIndirectFunction(PceWhIH, task, 2);
}


void
PceWhDeleteWindow(HWND win)
{ if ( PceWhEntry )
    InvokeIndirectFunction(PceWhIH, win, 3);
  else if ( win == cwin )
    cwin_deleted = TRUE;
}


static void
unload_pcewhdll()
{ if ( pcewhdll )
  { PceWhDeleteTask(GetCurrentTask());
    PceWhEntry = NULL;
    PceWhIH = NULL;
    FreeLibrary(pcewhdll);
  }
}


static void
init_area_enter_exit_handling(DisplayObj d)
{ Name dllname;
  FARPROC hookf;
  
  if ( isName(dllname = getResourceValueObject(d, NAME_whMouseDll)) )
  { HINSTANCE hlib;

    DEBUG(NAME_dll, Cprintf("loading DLL %s\n", strName(dllname)));

    if ( (hlib = LoadLibrary(strName(dllname))) >= HINSTANCE_ERROR )
    { pcewhdll = hlib;
      DEBUG(NAME_dll, Cprintf("loaded; lookup of \"Win386LibEntry\"\n"));
      PceWhEntry = GetProcAddress(hlib, "Win386LibEntry");
      DEBUG(NAME_dll, Cprintf("yields 0x%lx\n", PceWhEntry));
      PceWhIH    = GetIndirectFunctionHandle(PceWhEntry,
					     INDIR_WORD, INDIR_WORD,
					     INDIR_ENDLIST);
      DEBUG(NAME_dll, Cprintf("indirect fhandle = 0x%x\n", PceWhIH));
      PceWhAddTask(GetCurrentTask());
      DEBUG(NAME_dll, Cprintf("DLL loaded and initialised\n"));
      at_pce_exit(unload_pcewhdll, ATEXIT_FIFO);
      return;
    } else
      errorPce(d, NAME_failedToLoadDll, dllname, toInt(hlib));
  } 

  if ( isDefault(dllname) )
  { if ( !(hookf = MakeProcInstance(xpce_mouse_hook, PceHInstance)) )
      sysPce("Failed to create instance of xpce_mouse_hook()");
    if ( !(defhook = SetWindowsHookEx(WH_MOUSE, hookf, PceHInstance,
				      GetCurrentTask())) )
      sysPce("Failed to install xpce_mouse_hook()");
    
    at_pce_exit(unhook_xpce_mouse_hook, ATEXIT_FIFO);
  }
}

#else /*__WATCOMC__*/

typedef void (*mhdw_t)(HWND win);

static HINSTANCE xpcemh_id;
static mhdw_t MouseHookDeleteWindow = NULL;

void
PceWhDeleteWindow(HWND win)
{ if ( MouseHookDeleteWindow )
    (*MouseHookDeleteWindow)(win);
}


static void
unload_xpcemhdll(void)
{ if ( xpcemh_id )
    FreeLibrary(xpcemh_id);
}


static void
init_area_enter_exit_handling(DisplayObj d)
{ Name dllname;
  
  if ( isName(dllname = getResourceValueObject(d, NAME_whMouseDll)) )
  { HINSTANCE hlib;

    DEBUG(NAME_dll, Cprintf("loading DLL %s\n", strName(dllname)));

    if ( (hlib = LoadLibrary(strName(dllname))) )
    { xpcemh_id = hlib;

      MouseHookDeleteWindow = (mhdw_t) GetProcAddress(hlib,
						      "MouseHookDeleteWindow");
      at_pce_exit(unload_xpcemhdll, ATEXIT_FIFO);
      DEBUG(NAME_dll, Cprintf("%s loaded, hook = 0x%lx\n",
			      MouseHookDeleteWindow));
    }
  }
}

#endif /*__WATCOMC__*/

#else /*HOOK_BASED_MOUSE_HANDLING*/

static HWND current_window;

void
PceWhDeleteWindow(HWND win)
{ if ( win == current_window )
    current_window = 0;
}


void
PceEventInWindow(HWND win)
{ if ( win != current_window )
  { if ( current_window )
    { DEBUG(NAME_areaEnter,
	    Cprintf("Posting exit to %s\n",
		    pp(GetWindowLong(current_window, GWL_DATA))));
      SendMessage(current_window, WM_WINEXIT, 0, 0L);
    }
    if ( win )
    { DEBUG(NAME_areaEnter,
	    Cprintf("Posting enter to %s\n",
		    pp(GetWindowLong(win, GWL_DATA))));
      SendMessage(win, WM_WINENTER, 0, 0L);
    }

    current_window = win;
  }
}


static void
init_area_enter_exit_handling(DisplayObj d)
{
}


#endif /*HOOK_BASED_MOUSE_HANDLING*/

#ifdef USE_RLC_FUNCTIONS
static RlcUpdateHook system_update_hook;

static void
exit_update_hook(void)
{ rlc_update_hook(system_update_hook);
}
#endif /*USE_RLC_FUNCTIONS*/


status
ws_init_graphics_display(DisplayObj d)
{
#ifdef USE_RLC_FUNCTIONS
  system_update_hook = rlc_update_hook(pceRedraw);
  at_pce_exit(exit_update_hook, ATEXIT_FILO);
#endif
  initDraw();

  ws_system_colours(d);
  ws_system_images(d);

  init_area_enter_exit_handling(d);

  succeed;
}


void
ws_foreground_display(DisplayObj d, Colour c)
{
}


void
ws_background_display(DisplayObj d, Colour c)
{
}


void
ws_draw_in_display(DisplayObj d, Graphical gr, Bool invert, Bool subtoo)
{
}


void
ws_grab_server(DisplayObj d)
{
}


void
ws_ungrab_server(DisplayObj d)
{
}


Int
ws_display_connection_number(DisplayObj d)
{ fail;
}


status
ws_events_queued_display(DisplayObj d)
{ MSG msg;

  if ( PeekMessage(&msg, NULL, 0, 0, PM_NOREMOVE) )
    succeed;

  fail;
}

		 /*******************************
		 *     SELECTION HANDLING	*
		 *******************************/

#ifdef USE_RLC_FUNCTIONS
#define CLIPBOARDWIN	rlc_hwnd()
#else
#define CLIPBOARDWIN	PceHiddenWindow()
#endif

static HGLOBAL
ws_string_to_global_mem(String s)
{ int bytes = str_datasize(s);
  HGLOBAL mem = GlobalAlloc(GMEM_MOVEABLE, bytes + 1);
  char far *data;
  int i;

  if ( !mem )
  { Cprintf("Cannot allocate\n");
    return 0;
  }
  data = GlobalLock(mem);

  for(i=0; i<bytes; i++)
    *data++ = s->s_text8[i];
  *data = EOS;

  GlobalUnlock(mem);

  return mem;
}


status
ws_set_cutbuffer(DisplayObj d, int n, String s)
{ if ( n == 0 )
  { HGLOBAL mem = ws_string_to_global_mem(s);

    OpenClipboard(CLIPBOARDWIN);
    EmptyClipboard();
    SetClipboardData(CF_TEXT, mem);
    CloseClipboard();

    succeed;
  }

  Cprintf("Cannot access cut-buffers other than 0\n");
  fail;
}


StringObj
ws_get_cutbuffer(DisplayObj d, int n)
{ if ( n == 0 )
  { HGLOBAL mem;
    StringObj rval = FAIL;

    OpenClipboard(CLIPBOARDWIN);
    if ( (mem = GetClipboardData(CF_TEXT)) )
    { char far *data = GlobalLock(mem);
      char *copy, *q;
      int i;

      for(i=0; data[i]; i++)
	;
      q = copy = malloc(i + 1);

      while(*q++ = *data++)
	;
      rval = CtoString(copy);
      free(copy);
      GlobalUnlock(mem);
    }
    CloseClipboard();

    return rval; 
  }

  Cprintf("Cannot access cut-buffers other than 0\n");
  fail;
}


ulong
ws_get_selection_timeout(void)
{ return 0L;
}


void
ws_set_selection_timeout(ulong time)
{
}


Any
ws_get_selection(DisplayObj d, Name which, Name target)
{ return ws_get_cutbuffer(d, 0);
}


void
ws_renderall(void)
{ HWND hwnd = CLIPBOARDWIN;
  
  OpenClipboard(hwnd);
  EmptyClipboard();
  CloseClipboard();
}


void
ws_disown_selection(DisplayObj d, Name selection)
{ ws_renderall();
}


int
ws_provide_selection(int format)
{ DisplayObj d = CurrentDisplay(NIL);
  Hyper h;
  Function msg;
  Name which     = NAME_primary;
  Name hypername = getAppendName(which, NAME_selectionOwner);

  if ( d && notNil(d) &&
       (h = getFindHyperObject(d, hypername, DEFAULT)) &&
       (msg = getAttributeObject(h, NAME_convertFunction)) &&
       (msg = checkType(msg, TypeFunction, NIL)) )
  { CharArray ca;
    Name tname = NAME_string;

    if ( (ca = getForwardReceiverFunction(msg, h->to, which, tname, 0)) &&
	 (ca = checkType(ca, TypeCharArray, NIL)) )
    { String s = &ca->data;
      HGLOBAL mem = ws_string_to_global_mem(s);

      if ( mem )
	SetClipboardData(CF_TEXT, mem);

      return TRUE;
    }
  }

  return FALSE;
}


status
ws_own_selection(DisplayObj d, Name selection)
{ HWND hwnd = CLIPBOARDWIN;
  
  OpenClipboard(hwnd);
  EmptyClipboard();
  SetClipboardData(CF_TEXT, NULL);
  CloseClipboard();
  init_render_hooks();

  succeed;
}


#ifdef USE_RLC_FUNCTIONS
static RlcRenderHook	system_render_hook;
static RlcRenderAllHook system_render_all_hook;
static int 		render_hooks_initialised;

static void
exit_render_hooks(void)
{ if ( render_hooks_initialised )
  { rlc_render_hook(system_render_hook);
    rlc_render_all_hook(system_render_all_hook);

    render_hooks_initialised = FALSE;
  }
}


static void
init_render_hooks(void)
{ if ( !render_hooks_initialised )
  { system_render_hook     = rlc_render_hook(ws_provide_selection);
    system_render_all_hook = rlc_render_all_hook(ws_renderall);
    render_hooks_initialised++;

    at_pce_exit(exit_render_hooks, ATEXIT_FIFO);
  }
}
#endif


Name
ws_window_manager(DisplayObj d)
{ answer(CtoName("windows"));
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
These    may    be    useful    to    debug    the    graphics    stuff.
rlc_copy_output_to_debug_output() inplies that all output to the console
is also written to the debugger output,   making the relation with debug
output clear.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
ws_synchronous(DisplayObj d)
{ 
#ifdef USE_RLC_FUNCTIONS
  rlc_copy_output_to_debug_output(TRUE);
#endif
}


void
ws_asynchronous(DisplayObj d)
{
#ifdef USE_RLC_FUNCTIONS
  rlc_copy_output_to_debug_output(FALSE);
#endif
}


status
ws_postscript_display(DisplayObj d)
{ int w = valInt(getWidthDisplay(d));
  int h = valInt(getHeightDisplay(d));
  HDC hdc = GetDC(NULL);

  d_hdc(hdc, DEFAULT, DEFAULT);
  postscriptDrawable(0, 0, w, h);
  d_done();
  
  succeed;
}

		 /*******************************
		 *	 RESOURCE HANDLING	*
		 *******************************/

static ChainTable ResourceTable;
static Name name_star;			/* '*' */

#define LBUFSIZE 256			/* initial value buffer */
#define MAXFIELDS 10			/* Max # x.y.z... fields */
#ifndef MAXPATHLEN
#define MAXPATHLEN 256
#endif

static void
add_resource(int nfields, Name *fields, StringObj value)
{ if ( nfields > 0 )
  { Name resname = fields[nfields-1];

    if ( resname != name_star )
    { Any argv[10];
      int i, argc;

      for(argc = 0, i=0; i < (nfields-1); i++)
	argv[argc++] = fields[i];
      argv[argc++] = value;

      appendChainTable(ResourceTable, resname,
		       newObjectv(ClassVector, argc, argv));
    }
  }
}


static void
load_resource_file(char *file)
{ int lineno = 0;
  FileObj f;

  f = answerObject(ClassFile, CtoString(file), 0);

  if ( send(f, NAME_access, NAME_read, 0) &&
       send(f, NAME_open, NAME_read, 0) )
  { char line[LINESIZE];
    FILE *fd = f->fd;

    while( fgets(line, sizeof(line), fd) )
    { char *s = line;
      char *e;
      Name fields[MAXFIELDS];
      int nfields = 0;
      StringObj value;

      lineno++;

      while(isblank(*s))
	s++;
      if ( strchr("#!\n", *s) )
	continue;			/* blank or comment */

      for(;;)
      { if ( isalnum(*s) )
	{ string str;

	  for(e=s; isalnum(*e); e++)
	    ;
	  str_set_n_ascii(&str, e-s, s);
	  fields[nfields++] = StringToName(&str);
	  s = e;
	  DEBUG(NAME_resource, Cprintf("found %s\n", pp(fields[nfields-1])));
	  continue;
	}

	if ( *s == '*' )
	{ fields[nfields++] = name_star;
	  DEBUG(NAME_resource, Cprintf("found %s\n", pp(fields[nfields-1])));
	  s++;
	  continue;
	}
	  
	if ( *s == '.' )		/* field separator */
	{ s++;
	  continue;
	}

	if ( *s == ':' )		/* value separator */
	{ char localbuf[LBUFSIZE];
	  char *buf = localbuf;
	  int bufsize = LBUFSIZE;
	  int size = 0;
	  int l;
	  string str;

	  s++;				/* skip the ':' */

	  for(;;)
	  { for(s++; isblank(*s); s++)
	      ;
	    l = strlen(s);
					/* delete [\r\n]*$ */
	    while( l > 0 && (s[l-1] == '\n' || s[l-1] == '\r') )
	      s[--l] = EOS;
					/* make buffer big enough */
	    while ( size + l > bufsize )
	    { bufsize *= 2;
	      if ( buf == localbuf )
	      { buf = malloc(bufsize);
		strncpy(buf, localbuf, size);
	      } else
		buf = realloc(buf, bufsize);
	    }

					/* copy the new line to the buf */
	    strncpy(&buf[size], s, l);
	    size += l;

					/* continue if ended in a `\' */
	    if ( s[l-1] == '\\' )
	    { buf[size-1] = ' ';
	      if ( !fgets(line, sizeof(line), fd) )
	      { errorPce(PCE, NAME_resourceSyntaxError,
			 CtoName(file), toInt(lineno));
		goto out;
	      }
	      s = line;
	      
	      continue;
	    }

	    break;
	  }
	  
	  str_set_n_ascii(&str, size, buf);
	  value = StringToString(&str);
	  DEBUG(NAME_resource, Cprintf("Value = %s\n", pp(value)));
	  add_resource(nfields, fields, value);
	  goto next;
	} else
	{ errorPce(PCE, NAME_resourceSyntaxError,
		   CtoName(file), toInt(lineno));
	  goto next;
	}
      }
    next:
      ;
    }
    out:
      ;

    send(f, NAME_close, 0);
  }
}


static void
do_init_resources(DisplayObj d)
{ char file[MAXPATHLEN];
  Name home   = get(PCE, NAME_home, 0);
  Name cl     = get(d, NAME_resourceClass, 0);
  char *xdef;

  if ( !ResourceTable )
    ResourceTable = globalObject(NAME_resourceTable, ClassChainTable, 0);
  if ( !name_star )
    name_star = CtoName("*");

  sprintf(file, "%s/%s", strName(home), strName(cl));
  load_resource_file(file);
  if ( xdef = expandFileName("~/.Xdefaults"))
  { DEBUG(NAME_resource, Cprintf("Personal resource file is '%s'\n", xdef));

    strcpy(file, xdef);
    load_resource_file(file);
  }
}



StringObj
ws_get_resource_value(DisplayObj d, Name cc, Name cn, Name rc, Name rn)
{ Chain ch;

  if ( !ResourceTable )
    do_init_resources(d);

  if ( !(ch = getMemberHashTable((HashTable)ResourceTable, rn)) )
    ch = getMemberHashTable((HashTable)ResourceTable, rc);

  if ( ch )
  { Vector best = NIL;
    int bestok = -1;
    Cell cell;

    for_cell(cell, ch)
    { Vector v = cell->value;
      int size = valInt(v->size);
      Any *elements = v->elements;
      int ok = 0;

      if ( size == 3 )
      { if ( elements[1] == cn )
	{ ok = 100;			/* cant be better */
	} else if ( elements[1] == cc )
	{ ok = 70;
	} else if ( elements[1] == name_star )
	{ ok = 50;
	}

	DEBUG(NAME_resource, Cprintf("%s using %s: ok = %d (e0=%s)\n",
				     pp(rn), pp(v), ok, pp(elements[0])));

	if ( elements[0] == d->resource_class )
	{ ;
	} else if ( elements[0] == name_star )
	{ ok /= 10;
	} else
	  ok = 0;
      } else if ( size == 2 )
      { if ( elements[0] == name_star )
	  ok = 1;
      }

      if ( ok && ok >= bestok )
      { best = v;
	bestok = ok;
      }
    }

    if ( notNil(best) )
      return getTailVector(best);
  }

  fail;					/* uses the default */
}


