/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

#define NOAUTOPROCS
#include <windows.h>
#include "pcewh.h"

#ifndef DEBUGGING
#define DEBUGGING 0			/* 0: nodebug */
					/* 1: task-level */
					/* 2: window-level */
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The module ensures we get area enter/exit  messages in XPCE windows.  It
should be installed as pcewh.dll in one of your $PATH directories.  This
module installs a global mouse hook-function   that monitors the current
window.  If the current window changes, it will send an area-exit to the
left and area-enter to the new current window.

This is in a dll because system-wide mouse hooks can only run in a dll.
Interface:

	AddTask(HTASK)		Windows of this task will be send messages
	DeleteTask(HTASK)	Stop informing this task
	DeleteWindow(HWND)	Window is deleted.  Don't inform exit.

See xpce/src/msw/display.c for how this dll is used.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define MAX_PCE_TASKS 10

static HHOOK defhook;
DWORD _export FAR PASCAL mouse_hook(int code, WPARAM wParam, LPARAM lParam);
int FAR PASCAL AddTask(WORD task);
int FAR PASCAL DeleteTask(WORD task);
int FAR PASCAL DeleteWindow(WORD hwnd);

static HTASK	task_list[MAX_PCE_TASKS];
static HWND	cwin;			/* current window */
static int	cwin_is_pce_window;
static int	cwin_deleted;		/* cwin has been deleted */

int PASCAL
WinMain(HANDLE hInstance, HANDLE x1, LPSTR cmdline, int x2)
{ FARPROC fp, oldfp;
  CALLBACKPTR cbp;
  int i;

  cbp = GetProc16((PROCPTR) mouse_hook, GETPROC_SETWINDOWSHOOK);
  fp  = MakeProcInstance(cbp, hInstance); 
  defhook = SetWindowsHookEx(WH_MOUSE, fp, hInstance, NULL);

  for(i=0; i<MAX_PCE_TASKS; i++)
  { if ( !task_list[i] )
    { task_list[i] = GetCurrentTask();
      break;
    }
  }

  if ( DefineDLLEntry(1, (void*) AddTask,      DLL_WORD, DLL_ENDLIST) ||
       DefineDLLEntry(2, (void*) DeleteTask,   DLL_WORD, DLL_ENDLIST) ||
       DefineDLLEntry(3, (void*) DeleteWindow, DLL_WORD, DLL_ENDLIST) )
    return 0;

#if DEBUGGING > 0
  OutputDebugString("PCEWH.DLL: loaded\r\n");
#endif

  return 1;
}


int __export FAR PASCAL
WEP(int nParameter)
{ if ( defhook )
    UnhookWindowsHookEx(defhook);

  return 1;
}



int FAR PASCAL
AddTask(WORD task)
{ int i;
#if DEBUGGING > 0
  char msg[256];
  sprintf(msg, "PCEWH.DLL: addTask(0x%04x)\r\n", task);
  OutputDebugString(msg);
#endif

  for(i=0; i<MAX_PCE_TASKS; i++)
    if ( task_list[i] == task )
      return 0;
  
  for(i=0; i<MAX_PCE_TASKS; i++)
  { if ( !task_list[i] )
    { task_list[i] = task;
      return 0;
    }
  }

  return -1;
}


int FAR PASCAL
DeleteTask(WORD task)
{ int i;
#if DEBUGGING > 0
  char msg[256];
  sprintf(msg, "PCEWH.DLL: deleteTask(0x%04x)\r\n", task);
  OutputDebugString(msg);
#endif

  for(i=0; i<MAX_PCE_TASKS; i++)
  { if ( task_list[i] == task )
    { task_list[i] = 0;
      return 0;
    }
  }

  return -1;
}


int FAR PASCAL
DeleteWindow(HWND win)
{
#if DEBUGGING > 0
  char msg[256];
  sprintf(msg, "PCEWH.DLL: deleteWindow(0x%04x)\r\n", win);
  OutputDebugString(msg);
#endif

  if ( win == cwin )
    cwin_deleted = 1;

  return 0;
}


static int
isPceWindow(HWND hwnd)
{ HTASK t = GetWindowTask(hwnd);
  int i;

  for(i=0; i<MAX_PCE_TASKS; i++)
    if ( task_list[i] == t )
      return 1;

  return 0;
}


DWORD _export FAR PASCAL
mouse_hook(int code, WPARAM wParam, LPARAM lParam)
{ if ( code >= 0 )
  { MOUSEHOOKSTRUCT FAR* data = lParam;

    if ( data->hwnd != cwin )
    { int ispce = isPceWindow(data->hwnd);
#if DEBUGGING > 2
      char msg[256];

      sprintf(msg, "PCEWH.DLL: new hwnd = 0x%04x%s; old = 0x%04x%s\r\n",
	      data->hwnd, ispce ? "*" : " ",
	      cwin, cwin_is_pce_window ? "*" : " ");
      OutputDebugString(msg);
#endif

      if ( cwin && cwin_is_pce_window && !cwin_deleted )
      {
#if DEBUGGING > 1
	char msg[256];
        sprintf(msg, "PCEWH.DLL: WM_WINEXIT to 0x%04x\r\n", cwin);
	OutputDebugString(msg);
#endif
	SendMessage(cwin, WM_WINEXIT, 0, 0L);
      }
      cwin               = data->hwnd;
      cwin_is_pce_window = ispce;
      cwin_deleted       = 0;
      if ( cwin && cwin_is_pce_window )
      {
#if DEBUGGING > 1
	char msg[256];
        sprintf(msg, "PCEWH.DLL: WM_WINENTER to 0x%04x\r\n", cwin);
	OutputDebugString(msg);
#endif
	SendMessage(cwin, WM_WINENTER, 0, 0L);
      }
    }

    return 0;
  }

  return CallNextHookEx(defhook, code, wParam, lParam);
}
