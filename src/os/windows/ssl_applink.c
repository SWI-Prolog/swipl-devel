/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2023, SWI-Prolog Solutions b.v.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

#ifdef _MSC_VER

#include "../SWI-Stream.h"
#include <windows.h>

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
OPENSSL_Applink() ensures all OpenSSL routines that use MSVC library CRT
handles use them from the same DLL.  It   is  called  Applink as this is
normally associated with the application and   OpenSSL finds it from the
executable module accessible using `GetModuleHandle(NULL)`.

We want to keep this stuff in   our ssl package, so OPENSSL_Applink() is
compiled into ssl4pl.dll. But,  as  OpenSSL   searches  it  in  the main
executable, we need to _chain_ it. That is what this function does.

Note that this compiles and links  fine   using  MSVC.  When using MinGW
something goes really wrong and all   plugins  complain they cannot find
the PL_* API functions. As MinGW by default uses the _shared_ CRT model,
this does not matter.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

__declspec(dllexport) void ** __cdecl
OPENSSL_Applink(void)
{ static int applink_done = false;
  static void**(*f)(void) = NULL;

  if ( !applink_done )
  { HMODULE mod = GetModuleHandle("ssl4pl");
    if ( mod )
      f = (void*)GetProcAddress(mod, "OPENSSL_Applink");
    if ( !mod || !f )
      Sdprintf("Could not find OPENSSL_Applink() in ssl4pl.dll\n");
    applink_done = true;
  }

  if ( f )
    return f();

  return NULL;
}

#endif /*_MSC_VER*/
