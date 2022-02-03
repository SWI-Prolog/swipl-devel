/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2022, SWI-Prolog Solutions b.v.
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

#ifdef __APPLE__
#include "pl-apple.h"
#include <CoreFoundation/CoreFoundation.h>

static char *
cStringFromCFString(CFStringRef string, CFStringEncoding encoding)
{ size_t bufferSize = CFStringGetMaximumSizeForEncoding(
			  CFStringGetLength(string),
			  encoding) + 1;
  char *buffer;

  if ( (buffer=malloc(bufferSize)) )
  { if ( !CFStringGetCString(string, buffer, bufferSize, encoding))
    { free(buffer);
      buffer = NULL;
    }
  }

  return buffer;
}


static
PRED_IMPL("apple_current_locale_identifier", 1,
	  apple_current_locale_identifier, 0)
{ CFLocaleRef currentLocale;
  int rc = FALSE;

  if ( (currentLocale = CFLocaleCopyCurrent()) )
  { CFStringRef identifier;

    if ( (identifier = CFLocaleGetIdentifier(currentLocale)) )
    { char *buffer;

      if ( (buffer=cStringFromCFString(identifier, kCFStringEncodingUTF8)) )
      { rc = PL_unify_chars(A1, PL_ATOM|REP_UTF8, (size_t)-1, buffer);
	free(buffer);
      } else
	rc = PL_resource_error("memory");
    }

    CFRelease(currentLocale);
  }

  return rc;
}


		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(apple)
  PRED_DEF("apple_current_locale_identifier", 1,
	   apple_current_locale_identifier, 0)
EndPredDefs

#endif /*__APPLE__*/
