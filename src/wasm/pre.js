/*  Part of SWI-Prolog

    Author:        Jan Wielemaker and Raivo Laanemets
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

		 /*******************************
		 *	  MODULE DEFAULTS	*
		 *******************************/

Module.noInitialRun = true;


		 /*******************************
		 *	   BIND OUTPUT		*
		 *******************************/

/* Set `Module.on_output` to a function that receives the next output
   fragment.  This may be a line or the result of a flush.  The passed
   line will end with a newline if the flush is due to a newline.

   This function will normally ass a `span` element to the DOM that
   has the following style:

   ```
   .stderr, .stdout {
      white-space: pre-wrap;
      font-family: monospace;
      overflow-wrap: anywhere;
    }
    ```

    The second argument is one of "stdout" or "stderr", depending on the
    stream flushed.
*/

let decoder;
let buffers =
    {  stdout: [],
       stderr: []
    };

function write(to, c)
{ const buf = buffers[to];

  if ( c == 10 && buf.length == 0 )
    buf.push(32);
  if ( c )
    buf.push(c);

  if ( c == 10 || c == null )
    flush(to);
}

function decode(bytes)
{ const ar = new Uint8Array(bytes.length);

  for(var i=0; i<bytes.length; i++)
  { let c = bytes[i];

    if ( c < 0 )
      c = 256+c;

    ar[i] = c;
  }

  return decoder.decode(ar);
}

function flush(to)
{ if ( buffers[to].length )
  { const line = decode(buffers[to]);

    Module.on_output(line, to);
    buffers[to] = [];
  }
}


function log_output(stream, args)
{ if ( module.on_output )
  { let s = "";
    
    flush(stream);
    args.forEach((a) => { s += a; });
    Module.on_output(s, stream);
  } else
  { console.log.apply(null, args);
  }
}


function bind_std_streams()
{ decoder = new TextDecoder('utf-8');
  Module.FS.init(undefined,
		 (c) => write("stdout", c),
		 (c) => write("stderr", c));
}

if ( Module.on_output )
{ Module.preRun.push(bind_std_streams);
}
