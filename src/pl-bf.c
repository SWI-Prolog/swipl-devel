/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2022, University of Amsterdam
                         VU University Amsterdam
			 CWI, Amsterdam
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

#include "pl-bf.h"

static void*
default_realloc(void *ptr, size_t old, size_t new)
{ return realloc(ptr, new);
}

static void
default_free(void *ptr, size_t size)
{ free(ptr);
}

mp_alloc_wrapper alloc_wrapper = {
  .realloc_func = default_realloc,
  .free_func = default_free
};

static void *
my_bf_realloc(void *opaque, void *ptr, size_t size)
{ if ( alloc_wrapper.realloc_func )
    return alloc_wrapper.realloc_func(ptr, 0, size);
  else
    return realloc(ptr, size);
}

static void
my_bf_free(void *opaque, void *ptr, size_t size)
{ if ( alloc_wrapper.free_func )
    alloc_wrapper.free_func(ptr, size);
  else
    free(ptr);
}

void
initBF(void)
{ bf_context_init(&alloc_wrapper.bf_context, my_bf_realloc, my_bf_free, NULL);
}

void
bf_not_implemented(const char *msg)
{ Sdprintf("LibBF: Not implemented: %s\n", msg);
}
