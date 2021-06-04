/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2020, University of Amsterdam,
                              VU University Amsterdam,
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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
COMMON(type) defines the symbol to be global  with regard to Prolog, but
not exported from the shared object.  On   GCC  based  platforms this is
achieved using the visibility attribute. Making  symbols local to Prolog
avoid the ELF  dynamic  linker  picking   the  wrong  symbol  for  other
libraries and avoids Prolog picking wrong   symbols. It also reduces ELF
symbol lookup and relocations.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* TODO: remove this whole file */

/* pl-attvar.c */
#include "pl-attvar.h" 
/* pl-gvar.c */
#include "pl-gvar.h"
/* pl-wam.c */
#include "pl-wam.h"
/* pl-stream.c */
#include "os/pl-stream.h"
/* pl-supervisor.c */
#include "pl-supervisor.h"
/* pl-atom.c */
#include "pl-atom.h"
/* pl-bag.c */
#include "pl-bag.h"
/* pl-index.c */
#include "pl-index.h"
/* pl-dwim.c */
#include "pl-dwim.h"
/* pl-ext.c */
#include "pl-ext.h"
/* pl-flag.c */
#include "pl-flag.h"
/* pl-fli.c */
#include "pl-fli.h"
/* pl-fmt.c */
#include "os/pl-fmt.h"
/* pl-funct.c */
#include "pl-funct.h"
/* pl-gc.c */
#include "pl-gc.h"
/* pl-load.c */
#include "pl-load.h"
/* pl-modul.c */
#include "pl-modul.h"
/* pl-op.c */
#include "pl-op.h"
/* pl-os.c */
#include "os/pl-os.h"
/* pl-prims.c */
#include "pl-prims.h"
/* pl-prologflag.c */
#include "os/pl-prologflag.h"
/* pl-pro.c */
#include "pl-pro.h"
/* pl-proc.c */
#include "pl-proc.h"
/* pl-srcfile.c */
#include "pl-srcfile.h"
/* pl-read.c */
#include "pl-read.h"
/* pl-rec.c */
#include "pl-rec.h"
/* pl-setup.c */
#include "pl-setup.h"
/* pl-sys.c */
#include "pl-sys.h"
/* pl-trace.c */
#include "pl-trace.h"
/* pl-util.c */
#include "pl-util.h"
/* pl-wic.c */
#include "pl-wic.h"
/* pl-write.c */
#include "pl-write.h"
/* pl-term.c */
#include "pl-term.h"
/* pl-init.c */
#include "pl-init.h"
/* pl-nt.c */
#include "pl-nt.h"
/* pl-xterm.c */
#include "pl-xterm.h"
/* pl-ctype.c */
#include "os/pl-ctype.h"
/* pl-thread.c */
#include "pl-thread.h"
/* pl-mutex.c */
#include "pl-mutex.h"
/* pl-gmp.c */
#include "pl-gmp.h"
/* pl-cont.c */
#include "pl-cont.h"
/* pl-variant.c */
#include "pl-variant.h"
/* pl-version.h */
#include "pl-version.h"
