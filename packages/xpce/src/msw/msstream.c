/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

#define __USE_W32_SOCKETS		/* Use Win32 sockets for cygwin */

#include "include.h"
#include <h/unix.h>
#include <h/interface.h>
#undef send
#include <winsock.h>
#define send sendPCE
#include <process.h>

#define USE_API_THREAD 1

#define WM_SOCKET	 (WM_WINEXIT+1)
#define WM_PROCESS_INPUT (WM_WINEXIT+2)
#define WM_PROCESS_EXIT  (WM_WINEXIT+3)

static int 	ws_read_process(Process p, char *buf, int size, Int timeout);
#ifdef USE_API_THREAD
static DWORD	process_thread(void *context);
#else
static void	process_thread(void *context);
#endif
static void	eof_process(Process p, int status);
extern Name	SockError(void);		/* TBD */

static int input_to_handle;

static Socket
findSocketObject(SOCKET sock)
{ Chain ch = getObjectFromReferencePce(PCE, NAME_openSockets);
  Cell cell;

  for_cell(cell, ch)
  { Socket s = cell->value;

    if ( (SOCKET) s->ws_ref == sock )
      return s;

    if ( notNil(s->clients) )
    { Cell cell2;

      for_cell(cell2, s->clients)
      { Socket s2 = cell2->value;

	if ( (SOCKET) s2->ws_ref == sock )
	  return s2;
      }
    }
  }

  DEBUG(NAME_stream, Cprintf("No socket??\n"));
  return NIL;
}

static status
handleInputSocket(Socket s)
{ u_long avail;

  while( (SOCKET)s->ws_ref != INVALID_SOCKET &&
	 ioctlsocket((SOCKET)s->ws_ref, FIONREAD, &avail) == 0 &&
	 avail > 0 )
  { DEBUG(NAME_stream, Cprintf("%s: %d bytes available\n", pp(s), avail));

    if ( !handleInputStream((Stream)s) )
      fail;
  }
  
  succeed;
}


static int WINAPI
socket_wnd_proc(HWND hwnd, UINT message, UINT wParam, LONG lParam)
{ switch( message )
  { case WM_SOCKET:
    { SOCKET sock = (SOCKET) wParam;
      int err     = WSAGETSELECTERROR(lParam);
      int evt     = WSAGETSELECTEVENT(lParam);
      Socket s    = findSocketObject(sock);

      if ( notNil(s) && err == NO_ERROR )
      { switch(evt)
	{ case FD_READ:
	    DEBUG(NAME_stream, Cprintf("Input available on %s\n", pp(s)));
	    handleInputSocket(s);
	    pceRedraw(FALSE);
	    break;
	  case FD_ACCEPT:
	    DEBUG(NAME_stream, Cprintf("Accept on %s\n", pp(s)));
	    acceptSocket(s);
	    pceRedraw(FALSE);		/* may have side-effects */
	    break;
	  case FD_CLOSE:
	    DEBUG(NAME_stream, Cprintf("Close on %s\n", pp(s)));
	    closeInputStream((Stream) s);
	    send(s, NAME_endOfFile, EAV);
	    break;
	}
      } else
      { Name name = SockError();

	if ( notNil(name) )
	  errorPce(s, NAME_ioError, name);
	else
	{ DEBUG(NAME_stream,
		{ char *op;
		  switch(evt)
		  { case FD_READ:	op = "fd_read";   break;
	    	    case FD_ACCEPT:	op = "fd_accept"; break;
	    	    case FD_CLOSE:	op = "fd_close";  break;
	    	    default:		op = "???";	  break;
		  }
		  Cprintf("(Socket event = %s)\n", op);
		});
	} 
      }
      return 0;
    }
    case WM_PROCESS_INPUT:
    { Process p = (Process) wParam;
      int avail = (int) lParam;
      
      DEBUG(NAME_process, Cprintf("Received WM_PROCESS_INPUT %s %d\n",
				  pp(p), avail));
      handleInputStream((Stream)p);
      input_to_handle = FALSE;
      pceRedraw(FALSE);
      return 0;
    }
    case WM_PROCESS_EXIT:
    { Process p = (Process) wParam;
      int status = (int) lParam;

      DEBUG(NAME_process, Cprintf("Received WM_PROCESS_EXIT %s %d\n",
				  pp(p), status));
      eof_process(p, status);
      pceRedraw(FALSE);
      return 0;
    }
#ifndef USE_RLC_FUNCTIONS
    case WM_RENDERALLFORMATS:
      ws_renderall();
      return 0;
    case WM_RENDERFORMAT:
      if ( ws_provide_selection(wParam) )
	return 0;
      break;
#endif /*USE_RLC_FUNCTIONS*/
  }
  
  return DefWindowProc(hwnd, message, wParam, lParam);
}


static char *
HiddenFrameClass()
{ static Name winclassname = NULL;
  static WNDCLASS wndClass;

  if ( !winclassname )
  { char buf[50];

    sprintf(buf, "PceSocketWin%d", (int)PceHInstance);
    winclassname = CtoName(buf);

    wndClass.style		= 0;
    wndClass.lpfnWndProc	= (LPVOID) socket_wnd_proc;
    wndClass.cbClsExtra		= 0;
    wndClass.cbWndExtra		= 0;
    wndClass.hInstance		= PceHInstance;
    wndClass.hIcon		= NULL;
    wndClass.hCursor		= NULL;
    wndClass.hbrBackground	= GetStockObject(WHITE_BRUSH);
    wndClass.lpszMenuName	= NULL;
    wndClass.lpszClassName	= strName(winclassname);

    RegisterClass(&wndClass);
  }

  return strName(winclassname);
}


static HWND pce_hidden_window;

static void
destroyHiddenWindow(int rval)
{ if ( pce_hidden_window )
  { DestroyWindow(pce_hidden_window);
    pce_hidden_window = 0;
  }
}


HWND
PceHiddenWindow()
{ if ( !pce_hidden_window )
  { pce_hidden_window = CreateWindow(HiddenFrameClass(),
				     "XPCE hidden main window",
				     WS_POPUP,
				     0, 0, 32, 32,
				     NULL, NULL, PceHInstance, NULL);
    at_pce_exit(destroyHiddenWindow, ATEXIT_FIFO);
    assert(pce_hidden_window);
  }

  return pce_hidden_window;
}

		 /*******************************
		 *	   PUBLIC STUFF		*
		 *******************************/

void
ws_close_input_stream(Stream obj)
{ if ( instanceOfObject(obj, ClassSocket) )
  { SOCKET s = (SOCKET) obj->ws_ref;

    if ( s != INVALID_SOCKET )
    { WSAAsyncSelect(s, PceHiddenWindow(), 0, 0);
      shutdown(s, 0);
    }
  } else				/* process */
  { HANDLE fd = (HANDLE) obj->rdfd;

    CloseHandle(fd);
  }

  obj->rdfd = -1;
}


void
ws_close_output_stream(Stream obj)
{ if ( instanceOfObject(obj, ClassSocket) )
  { SOCKET s = (SOCKET) obj->ws_ref;

    if ( s != INVALID_SOCKET )
    { shutdown(s, 1);
    }
  } else
  { HANDLE fd = (HANDLE) obj->wrfd;

    CloseHandle(fd);
  }

  obj->wrfd = -1;
}


void
ws_close_stream(Stream obj)
{ if ( instanceOfObject(obj, ClassSocket) )
  { SOCKET s = (SOCKET) obj->ws_ref;

    if ( s != INVALID_SOCKET )
    { DEBUG(NAME_stream, Cprintf("MS: closesocket(%s)\n", pp(obj)));
      closesocket(s);
      obj->ws_ref = (WsRef) INVALID_SOCKET;
    }
  } /* else { }*/
}


void
ws_listen_socket(Socket s)
{ SOCKET sock = (SOCKET) s->ws_ref;

  if ( sock != INVALID_SOCKET )
  { WSAAsyncSelect(sock, PceHiddenWindow(), WM_SOCKET, FD_ACCEPT);
  }
}


void
ws_input_stream(Stream s)
{ HWND hwnd = PceHiddenWindow();    /* make sure to create in main thread */

  if ( instanceOfObject(s, ClassSocket) )
  { SOCKET sock = (SOCKET) s->ws_ref;

    if ( sock != INVALID_SOCKET )
    { if ( WSAAsyncSelect(sock, hwnd, WM_SOCKET, FD_READ|FD_CLOSE) )
	errorPce(s, NAME_socket, CtoName("WSAAsyncSelect()"), SockError());
      s->rdfd = 0;			/* signal open status */
    }
  } else
  { DWORD id;
    HANDLE h;

    if ( (h = CreateThread(NULL, 10240,
			   (LPTHREAD_START_ROUTINE)process_thread,
			   (LPVOID) s, 0, &id)) )
      CloseHandle(h);
    else
      Cprintf("%s: Failed to create wait-thread\n", pp(s));
  }

  DEBUG(NAME_stream,
	Cprintf("Registered %s for asynchronous input\n", pp(s)));
}


void
ws_no_input_stream(Stream obj)
{ if ( instanceOfObject(obj, ClassSocket) )
  { SOCKET s = (SOCKET) obj->ws_ref;

    if ( s != INVALID_SOCKET )
    { WSAAsyncSelect(s, PceHiddenWindow(), 0, 0);
    }
  }

  DEBUG(NAME_stream,
	Cprintf("Un-registered %s for asynchronous input\n", pp(obj)));
}


status
ws_write_stream_data(Stream s, void *data, int len)
{ if ( instanceOfObject(s, ClassSocket) )
  { char *str = data;
#undef send
    while( len > 0 )
    { int n = send((SOCKET)s->ws_ref, str, len, 0);
      if ( n < 0 )
	return errorPce(s, NAME_ioError, SockError());
      len -= n;
      str += n;
    }
#define send sendPCE
  } else				/* process */
  { DWORD written;
    HANDLE fd = (HANDLE) s->wrfd;

    if ( !WriteFile(fd, data, len, &written, NULL) )
      return errorPce(s, NAME_ioError, APIError());
  }

  succeed;
}


int
ws_read_stream_data(Stream s, void *data, int len)
{ if ( instanceOfObject(s, ClassSocket) )
  { SOCKET sock = (SOCKET) s->ws_ref;
    int rval;

    if ( (rval = recv(sock, data, len, 0)) == SOCKET_ERROR )
      return -1;

    return rval;
  } else				/* process */
  { return ws_read_process((Process)s, data, len, toInt(1000));
  }
}


		 /*******************************
		 *	PROCESS ASYNC INPUT	*
		 *******************************/

#ifdef USE_API_THREAD
static DWORD
#else
static void
#endif
process_thread(void *context)
{ Process p = (Process) context;
  DWORD avail;
  HWND hwnd = PceHiddenWindow();
  int peekok = FALSE;
  DWORD status;
  PROCESS_INFORMATION *pi = p->ws_ref;
      
  DEBUG(NAME_thread, Cprintf("%s: Starting process input thread\n", pp(p)));

  while( pi && p->rdfd > 0 && !onFlag(p, F_FREED|F_FREEING) &&
	 (peekok=PeekNamedPipe((HANDLE)p->rdfd, NULL, 0, NULL, &avail, NULL)) )
  { if ( avail )
    { input_to_handle = TRUE;
      DEBUG(NAME_thread, Cprintf("Posting WM_PROCESS_INPUT %s %d\n",
				  pp(p), avail));
      PostMessage(hwnd, WM_PROCESS_INPUT, (WPARAM)p, (LPARAM)avail);
      do
      { Sleep(0);			/* TBD: no global status */
      } while( input_to_handle );
    } else
    { if ( GetExitCodeProcess(pi->hProcess, &status) )
      { if ( status == STILL_ACTIVE )
	{ Sleep(100);			/* skip timeslice */
	  continue;	
	} else
	{ break;
	}
      } else				/* why should this happen? */
      { status = (DWORD)-1;
	break;
      }
    }

    DEBUG(NAME_thread, Cprintf("."));
  }  

  if ( !peekok )
  { DEBUG(NAME_thread,
	  Cprintf("PeekNamedPipe() failed: %s\n", pp(APIError())));
    if ( !pi || !GetExitCodeProcess(pi->hProcess, &status) )
      status = (DWORD) -1;
  }

  PostMessage(hwnd, WM_PROCESS_EXIT, (WPARAM)p, (LPARAM)status);

  DEBUG(NAME_thread, Cprintf("%s: Finished process input thread\n", pp(p)));

#ifdef USE_API_THREAD
  return 0;
#endif
}


		 /*******************************
		 *	 PROCESS READLINE	*
		 *******************************/


static void
eof_process(Process p, int status)
{ closeInputProcess(p);
  send(p, NAME_exited, toInt(status), EAV);
}


static int
ws_read_process(Process p, char *buffer, int size, Int timeout)
{ if ( p->rdfd >= 0 )
  { DWORD avail;
    int peekok;
    long endtime = (isDefault(timeout) ? 0 : mclock() + valInt(timeout));
      
    while( (peekok = PeekNamedPipe((HANDLE)p->rdfd, NULL, 0,
				   NULL, &avail, NULL)) &&
	   avail == 0 &&
	   (!endtime || mclock() < endtime) )
    { PROCESS_INFORMATION *pi = p->ws_ref;
      DWORD status;

      if ( GetExitCodeProcess(pi->hProcess, &status) )
      { if ( status == STILL_ACTIVE )
	{ ws_dispatch(DEFAULT, DEFAULT);
	} else
	{ eof_process((Process)p, status);
	  return 0;			/* end-of-file */
	}
      } else				/* why should this happen? */
      { eof_process((Process)p, -1);
	return -1;
      }
    }

    if ( peekok && avail )
    { int toread = min(size, avail);
      DWORD done;

      DEBUG(NAME_process, Cprintf("%s: %d bytes available\n", pp(p), avail));

      if ( ReadFile((HANDLE)p->rdfd, buffer, toread, &done, NULL) )
      { return done;
      } else
      { errorPce(p, NAME_ioError, APIError());
	eof_process((Process)p, -1);
	return -1;
      }
    } else				/* and this? */
    { DEBUG(NAME_process,
	    Cprintf("%s: peekok = %d, avail = %d\n", pp(p), peekok, avail));
      eof_process((Process) p, -1);
      return -1;
    }
  } else
  { errorPce(p, NAME_notOpen);
    return -1;
  }
}


#define BLOCKSIZE 256

StringObj
ws_read_line_stream(Stream s, Int timeout)
{ if ( instanceOfObject(s, ClassProcess) )
  { Process p = (Process) s;
    char buf[BLOCKSIZE];
    int done;

    for(;;)
    { if ( p->input_buffer )
      { unsigned char *q;
	int n;

	DEBUG(NAME_process, Cprintf("Scanning %d chars\n", p->input_p));
	for(n=p->input_p, q = p->input_buffer; n > 0; n--, q++)
	{ if ( *q == '\n' )
	  { string str;
	    int len = (q-p->input_buffer)+1;
	    StringObj rval;

	    str_set_n_ascii(&str, len, p->input_buffer);
	    rval = StringToString(&str);
	    strncpy(s->input_buffer, &s->input_buffer[len], s->input_p - len);
	    s->input_p -= len;

	    return rval;
	  }
	}
	DEBUG(NAME_process, Cprintf("No newline, reading\n"));
      }

      if ( (done = ws_read_process(p, buf, BLOCKSIZE, timeout)) > 0 )
      { add_data_stream(s, buf, done);
	DEBUG(NAME_process, Cprintf("Buffer has %d bytes\n", p->input_p));
      } else
	fail;
    }
  } else
  { Cprintf("[PCE: %s: stream <-read_line only for class process]\n", pp(s));
    fail;
  }
}
