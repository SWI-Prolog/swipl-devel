/*  Part of SWI-Prolog

    Author:        Jan Wielemaker and Raivo Laanemets
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2022-2025, SWI-Prolog Solutions b.v.
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

let prolog;

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

const ansi_color =		// xterm default color palette
      { 0: "#000000",
	1: "#cd0000",
	2: "#00cd00",
	3: "#cdcd00",
	4: "#0000ee",
	5: "#cd00cd",
	6: "#00cdcd",
	7: "#e5e5e5",
	8: "#7f7f7f",
	9: "#ff0000",
	10: "#00ff00",
	11: "#ffff00",
	12: "#5c5cff",
	13: "#ff00ff",
	14: "#00ffff",
	15: "#ffffff"
      };

function new_ansi_state()
{ return(
  { state: "initial",
    argv: [],
    argstat: 0,
    sgr:
    { color: undefined,
      background_color: undefined,
      bold: false,
      underline: false
    }
  });
}

function set_sgr(sgr, codes)
{ codes.forEach((code) => {
    if ( code == 0 )
    { sgr.bold = false;
      sgr.underline = false;
      sgr.color = undefined;
      sgr.background_color = undefined;
    } else if ( code >= 30 && code <= 39 )
    { if ( code == 39 )
      { sgr.color = undefined;
      } else
      { sgr.color = ansi_color[code-30];
      }
    } else if ( code >= 40 && code <= 49 )
    { if ( code == 49 )
      { sgr.background_color = undefined;
      } else
      { sgr.background_color = ansi_color[code-40];
      }
    } else if ( code >= 90 && code <= 99 )
    { if ( code == 99 )
      { sgr.color = undefined;
      } else
      { sgr.color = ansi_color[code-90+8];
      }
    } else if ( code >= 100 && code <= 109 )
    { if ( code == 109 )
      { sgr.background_color = undefined;
      } else
      { sgr.background_color = ansi_color[code-100+8];
      }
    } else if ( code == 1 )
    { sgr.bold = true;
    } else if ( code == 4 )
    { sgr.underline = true;
    }
  });
}

function set_ansi(sgr, cmd)
{ switch(cmd.cmd)
  { case "sgr":
    { set_sgr(sgr, cmd.argv);
    }
  }
}

/**
 * @param {Object} ansi_state contains the current state for ANSI
 * decoding
 * @param {Number} c is the next character from the stream
 * @return {Number|Object|undefined} If `c` is eaten by the decoder return
 * `undefined`.  If `c` is uninterpreted, return it.  If an ANSI sequence
 * is completed return an object holding `cmd` and `argv`.
 */

function decode_ansi(ansi_state, c)
{ switch(ansi_state.state)
  { case "initial":
    { if ( c == 27 )
      { ansi_state.state = "esc";
	return;
      } else
      { return c;
      }
    }
    case "esc":
    { if ( c == 91 )		// '['
      { ansi_state.state = "ansi";
	ansi_state.argv = [];
	return;
      } else if ( c == 93 )	// ']'
      { ansi_state.state = "link";
	ansi_state.must_see = [56, 59, 59]; // 8;;
	return;
      } else
      { ansi_state.state = "initial";
	return c;
      }
    }
    case "link":
    { if ( c != ansi_state.must_see.shift() )
      { ansi_state.state = "initial";
	return c;
      }
      if ( ansi_state.must_see.length == 0 )
      { ansi_state.chars = [];
	ansi_state.state = "linkarg";
      }
      return;
    }
    case "linkarg":
    { ansi_state.chars.push(c);
      const end = [27, 93, 56, 59, 59, 27, 92]; // \e]8;;\e\\

      if ( c == 92 &&		// '\\'
	   ends_with(ansi_state.chars, end) )
      { ansi_state.chars.splice(-end.length);
	const text = decode(ansi_state.chars);
	ansi_state.state = "initial";
	return { cmd: "link",
		 argv: text.split("\x1b\\")
	       }
      }
      return;
    }
    case "ansi":
    { if ( c >= 48 && c <= 57 )	// 0..9
      { if ( !ansi_state.argstat )
	{ ansi_state.argv.push(c-48);
	  ansi_state.argstat = 1;
	} else
	{ const i = ansi_state.argv.length-1;
	  ansi_state.argv[i] = ansi_state.argv[i] * 10 + (c - 48);
	}
	ansi_state.state = "ansi";
	return;
      } else if ( !ansi_state.argstat && c == 45 )	// -
      { ansi_state.argstat = -1;
	ansi_state.state = "ansi";
	return;
      } else if ( ansi_state.argstat )
      { const i = ansi_state.argv.length-1;
	if ( i >= 0 )
	{ ansi_state.argv[i] *= ansi_state.argstat;
	  ansi_state.argstat = 0;
	} else
	{ ansi_state.state = "initial";
	  return c;
	}
      }

      if ( c == 59 )		// ';'
        return;

      ansi_state.state = "initial";

      switch(c)
      { case 72:		// 'H'
	case 102:		// 'f'
	{ return { cmd: "set_caret",
		   argv: [arg(1, 0), arg(2,0)]
		 }
	}
	case 65:		// 'A'
	{ return cmd("caret_up");
	}
	case 66:		// 'B'
	{ return cmd("caret_down");
	}
	case 67:		// 'C'
	{ return cmd("caret_forward");
	}
	case 68:		// 'D'
	{ return cmd("caret_backward");
	}
	case 115:		// 's'
	{ return { cmd: "save_caret_position"
		 };
	}
	case 117:		// 'u'
	{ return { cmd: "restore_caret_position"
		 };
	}
	case 74:		// 'J'
	{ if ( ansi_state.argv[0] == 2 )
	  { return { cmd: "erase_display"
		   };
	  }
	}
	case 75:		// 'K'
	{ return { cmd: "erase_line"
		 };
	}
	case 109:		// 'm'
	{ if ( ansi_state.argv.length == 0 )
	    ansi_state.argv.push(0);
	  return { cmd: "sgr",
		   argv: ansi_state.argv
		 };
	}
      }
    }
  }

  function ends_with(array, end)
  { let a = array.length-1;
    let e = end.length-1;

    if ( a >= e )
    { while(e >= 0 && array[a] == end[e])
      { a--; e--;
      }
      if ( e == -1 )
	return true;
    }
    return false;
  }
}

let decoder;
const buffers =
      {  stdout: { buf: [],
		   ansi: new_ansi_state()
		 },
	 stderr: { buf: [],
		   ansi: new_ansi_state()
		 }
      };

function write(to, c)
{ const buf = buffers[to].buf;

  if ( c )
  { c = decode_ansi(buffers[to].ansi, c);
    if ( typeof c === "number" )
    { if ( c == 10 && buf.length == 0 )
        buf.push(32);
      buf.push(c);
      if ( c == 10 || c == null )
	flush(to);
    } else if ( c !== undefined )
    { flush(to);
      switch(c.cmd)
      { case "link":
	{ Module.on_output(c.argv[1], to, {link:c.argv[0]});
	}
	default:
	{ set_ansi(buffers[to].ansi.sgr, c);
	}
      }
    }
  }
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
{ const buf = buffers[to].buf;
  if ( buf.length )
  { const line = decode(buf);

    Module.on_output(line, to, buffers[to].ansi.sgr);
    buffers[to].buf = [];
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
{ if (typeof Module.preRun === 'function') {
    Module.preRun = [ Module.preRun ]
  } else if (!Array.isArray(Module.preRun)) {
    Module.preRun = []
  }

  Module.preRun.push(bind_std_streams);
}

		 /*******************************
		 *        PROLOG CLASSES        *
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Notes:

  - Emscripten embedding does not yet support private methods using #,
    so we use __ as prefix instead.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

const class_var = (class PrologVar {
  constructor(id)
  { this.$t = "v";
    if ( id !== undefined )
      this.v = id;
  }
});

const class_string = (class PrologString {
  constructor(string)
  { this.$t = "s";
    this.v  = string;
  }

  toString()
  { return this.v;
  }

  toJSON()
  { return this.v;
  }
});

const class_rational = (class PrologRational {
  constructor(n, d)
  { this.$t = "r";
    this.n = n;
    this.d = d;
  }

  toNumber()
  { return Number(this.d)/Number(this.n);
  }

  toString()
  { return this.d + "r" + this.n;
  }

  toJSON()
  { return this.toString();
  }
});

const class_compound = (class PrologCompound {
  constructor(name, ...args)
  { this.$t = "t";
    this.functor = name;
    this[name] = args;
  }

  arguments()
  { return this[this.functor];
  }

  arg(n)
  { return this.arguments[n];
  }

  arity()
  { return this.arguments.length;
  }

  toJSON()
  { const obj = {$t:"t"}
    obj[this.functor] = this.arguments();
    return obj;
  }
});

const class_list = (class PrologList {
  constructor(array, tail)
  { this.$t = "l";
    this.v = array;
    if ( tail !== undefined )
      this.t = tail;
  }
});

const class_blob = (class PrologBlob {
  constructor()
  { this.$t = "b";
  }
});

const class_term = (class PrologTerm {
  constructor(arg)
  { this.$t = "term_t";
    if ( typeof arg === "string" )
    { const h = prolog.new_term_ref();

      if ( !prolog.chars_to_term(arg, h) )
	throw new Error(`Query has a syntax error: ${arg}`);
      this.term_t = h;
    } else if ( typeof arg == "number" )
    { this.term_t = arg;
    } else
    { throw new Error(`String or term handle expected.  Found ${arg}`);
    }
  }
});

const class_abortable_promise = (class AbortablePromise extends Promise {
  constructor(executer)
  { super(executer);
    this.executer = executer;
  }

  abort()
  { if ( this.executer.abort )
      return this.executer.abort();
    else
      console.log("Cannot abort promise");

    return false;
  }
});

		 /*******************************
		 *	   CLASS Prolog		*
		 *******************************/

/**
 * Class Prolog provides a high level interface to interact with Prolog.
 *
 * @param {Object} module Emscripten module in which Prolog was created
 * @param {Array} [args] are the Prolog initialization arguments,
 * normally the command line flags.
 */

class Prolog
{ constructor(module, args)
  { this.module = module;		// Emscripten module
    this.args = args;			// Prolog initialization args
    this.functor_arg_names_ = {};
    this.objects = {};			// id --> Object
    this.object_ids = new WeakMap();	// object --> id
    this.next_object_id = 0;

    this.__set_foreign_constants();
    this.__bind_foreign_functions();
    this.__export_classes();
    this.__initialize();

    this.__engine_id = 0;		// GenId for anonymous engines
    this.__id_engines = {};		// id --> Engine
    this.engines = {};			// name --> Engine
    this.main_engine = this.__init_main_engine("main");
    prolog = this;
  }


  __initialize()
  { let argv0 = this.args || [];
    argv0.unshift("swipl");
    let argv = argv0.map(function(arg) {
      const len = lengthBytesUTF8(arg);
      const s = _malloc(len+1);
      stringToUTF8(arg, s, len+1);
      return s;
    }, this);
    var ptr = _malloc(argv.length * 4);
    argv.forEach(function(arg, i) {
	this.module.setValue(ptr + i * 4, arg, '*');
    }, this);
    if (!this.bindings.PL_initialise(argv.length, ptr)) {
	throw new Error('SWI-Prolog initialisation failed.');
    }
    this.bindings.WASM_bind_standard_streams();
    this.MODULE_user = this.new_module("user");
    this.call("set_prolog_flag(color_term, false).");
    this.call("set_prolog_flag(debug_on_error, false)");
    this.call("use_module(library(wasm))");
  }

  __export_classes()
  { this.Var	  = class_var;
    this.String	  = class_string;
    this.Rational = class_rational;
    this.Compound = class_compound;
    this.Term     = class_term;
    this.List	  = class_list;
    this.Blob	  = class_blob;
    this.Promise  = class_abortable_promise;
    this.Engine   = class_engine;
  }

  __set_foreign_constants()
  { this.PL_VARIABLE		 = (1);
    this.PL_ATOM		 = (2);
    this.PL_INTEGER		 = (3);
    this.PL_RATIONAL		 = (4);
    this.PL_FLOAT		 = (5);
    this.PL_STRING		 = (6);
    this.PL_TERM		 = (7);
    this.PL_NIL			 = (8);
    this.PL_BLOB		 = (9);
    this.PL_LIST_PAIR		 = (10);
    this.PL_FUNCTOR		 = (11);
    this.PL_LIST		 = (12);
    this.PL_CHARS		 = (13);
    this.PL_POINTER		 = (14);
    this.PL_CODE_LIST		 = (15);
    this.PL_CHAR_LIST		 = (16);
    this.PL_BOOL		 = (17);
    this.PL_FUNCTOR_CHARS	 = (18);
    this._PL_PREDICATE_INDICATOR = (19);
    this.PL_SHORT		 = (20);
    this.PL_INT			 = (21);
    this.PL_LONG		 = (22);
    this.PL_DOUBLE		 = (23);
    this.PL_NCHARS		 = (24);
    this.PL_UTF8_CHARS		 = (25);
    this.PL_UTF8_STRING		 = (26);
    this.PL_INT64		 = (27);
    this.PL_NUTF8_CHARS		 = (28);
    this.PL_NUTF8_CODES		 = (29);
    this.PL_NUTF8_STRING	 = (30);
    this.PL_NWCHARS		 = (31);
    this.PL_NWCODES		 = (32);
    this.PL_NWSTRING		 = (33);
    this.PL_MBCHARS		 = (34);
    this.PL_MBCODES		 = (35);
    this.PL_MBSTRING		 = (36);
    this.PL_INTPTR		 = (37);
    this.PL_CHAR		 = (38);
    this.PL_CODE		 = (39);
    this.PL_BYTE		 = (40);
    this.PL_PARTIAL_LIST	 = (41);
    this.PL_CYCLIC_TERM		 = (42);
    this.PL_NOT_A_LIST		 = (43);
    this.PL_DICT		 = (44);

    this.REP_ISO_LATIN_1	 = 0x00000000;
    this.REP_UTF8		 = 0x00100000;
    this.REP_MB			 = 0x00200000;
    this.REP_FN			 = this.REP_UTF8;

    this.CVT_ATOM		 = 0x00000001;
    this.CVT_STRING		 = 0x00000002;
    this.CVT_LIST		 = 0x00000004;
    this.CVT_INTEGER		 = 0x00000008;
    this.CVT_RATIONAL		 = 0x00000010;
    this.CVT_FLOAT		 = 0x00000020;
    this.CVT_VARIABLE		 = 0x00000040;
    this.CVT_NUMBER		 = (this.CVT_INTEGER|this.CVT_RATIONAL|
				    this.CVT_FLOAT);
    this.CVT_ATOMIC		 = (this.CVT_NUMBER|this.CVT_ATOM|
				    this.CVT_STRING);
    this.CVT_WRITE		 = 0x00000080;
    this.CVT_WRITE_CANONICAL	 = 0x00000100;
    this.CVT_WRITEQ		 = 0x00000200;
    this.CVT_ALL		 = (this.CVT_ATOMIC|this.CVT_LIST);
    this.CVT_MASK		 = 0x00000fff;

    this.CVT_EXCEPTION		 = 0x00001000;
    this.CVT_VARNOFAIL		 = 0x00002000;

    this.BUF_DISCARDABLE	 = 0x00000000;
    this.BUF_STACK		 = 0x00010000;
    this.BUF_MALLOC		 = 0x00020000;
    this.BUF_ALLOW_STACK	 = 0x00040000;

    this.PL_Q_NORMAL		 = 0x0002;
    this.PL_Q_NODEBUG		 = 0x0004;
    this.PL_Q_CATCH_EXCEPTION	 = 0x0008;
    this.PL_Q_PASS_EXCEPTION	 = 0x0010;
    this.PL_Q_ALLOW_YIELD	 = 0x0020;
    this.PL_Q_EXT_STATUS	 = 0x0040;
    this.PL_Q_EXCEPT_HALT	 = 0x0080;
    this.PL_Q_TRACE_WITH_YIELD	 = 0x0100;

    this.PL_S_NOT_INNER		 = -2;
    this.PL_S_EXCEPTION		 = -1;
    this.PL_S_FALSE		 = 0;
    this.PL_S_TRUE		 = 1;
    this.PL_S_LAST		 = 2;
    this.PL_S_YIELD_DEBUG	 = 254;
    this.PL_S_YIELD		 = 255;

    this.PL_ENGINE_MAIN		 = 1;
    this.PL_ENGINE_CURRENT	 = 2;
    this.PL_ENGINE_NONE		 = 3;
    this.PL_ENGINE_SET		 = 0;
    this.PL_ENGINE_INVAL	 = 2;
    this.PL_ENGINE_INUSE	 = 3;

    this.PL_WRT_QUOTED		 = 0x0001;
    this.PL_WRT_IGNOREOPS	 = 0x0002;
    this.PL_WRT_NUMBERVARS	 = 0x0004;
    this.PL_WRT_PORTRAY		 = 0x0008;
    this.PL_WRT_NEWLINE		 = 0x2000;
  }

  __bind_foreign_functions()
  { this.bindings =
    { _PL_streams: this.module.cwrap(
	'_PL_streams', 'number', []),
      PL_functor_arity: this.module.cwrap(
	'PL_functor_arity', 'number', ['number']),
      PL_functor_name: this.module.cwrap(
	'PL_functor_name', 'number', ['number']),
      PL_get_functor: this.module.cwrap(
	'PL_get_functor', 'number', ['number', 'number']),
      PL_get_chars: this.module.cwrap(
	'PL_get_chars', 'number', ['number', 'number', 'number']),
      PL_get_arg: this.module.cwrap(
	'PL_get_arg', 'number', ['number', 'number', 'number']),
      PL_get_int64: this.module.cwrap(
	'PL_get_int64', 'number', ['number', 'number']),
      PL_get_float: this.module.cwrap(
	'PL_get_float', 'number', ['number', 'number']),
      PL_put_chars: this.module.cwrap(
	'PL_put_chars', 'number', ['number', 'number', 'number', 'number']),
      put_bytes: this.module.cwrap(
	'PL_put_chars', 'number', ['number', 'number', 'number', 'array']),
      PL_put_atom: this.module.cwrap(
	'PL_put_atom', 'number', ['number']),
      PL_put_variable: this.module.cwrap(
	'PL_put_variable', 'number', ['number']),
      PL_unify: this.module.cwrap(
	'PL_unify', 'number', ['number', 'number']),
      PL_is_string: this.module.cwrap(
	'PL_is_string', 'number', ['number']),
      PL_is_variable: this.module.cwrap(
	'PL_is_variable', 'number', ['number']),
      PL_term_type: this.module.cwrap(
	'PL_term_type', 'number', ['number']),
      PL_get_list: this.module.cwrap(
	'PL_get_list', 'number', ['number', 'number', 'number']),
      PL_get_nil: this.module.cwrap(
	'PL_get_nil', 'number', ['number']),
      PL_initialise: this.module.cwrap(
	'PL_initialise', 'number', ['number', 'number']),
      PL_new_atom: this.module.cwrap(
	'PL_new_atom', 'number', ['string']),
      PL_register_atom: this.module.cwrap(
	'PL_register_atom', null, ['number']),
      PL_unregister_atom: this.module.cwrap(
	'PL_unregister_atom', null, ['number']),
      PL_new_module: this.module.cwrap(
	'PL_new_module', 'number', ['number']),
      PL_new_functor: this.module.cwrap(
	'PL_new_functor', 'number', ['number', 'number']),
      PL_new_term_ref: this.module.cwrap(
	'PL_new_term_ref', 'number', []),
      PL_new_term_refs: this.module.cwrap(
	'PL_new_term_refs', 'number', ['number']),
      PL_copy_term_ref: this.module.cwrap(
	'PL_copy_term_ref', 'number', ['number']),
      PL_reset_term_refs: this.module.cwrap(
	'PL_reset_term_refs', null, ['number']),
      PL_put_functor: this.module.cwrap(
	'PL_put_functor', 'number', ['number', 'number']),
      PL_put_integer: this.module.cwrap(
	'PL_put_integer', 'number', ['number', 'number']),
      PL_put_float: this.module.cwrap(
	'PL_put_float', 'number', ['number', 'number']),
      PL_put_nil: this.module.cwrap(
	'PL_put_nil', 'number', []),
      PL_cons_functor_v: this.module.cwrap(
	'PL_cons_functor_v', 'number', ['number', 'number', 'number']),
      PL_cons_list: this.module.cwrap(
	'PL_cons_list', 'number', ['number', 'number', 'number']),
      PL_put_dict: this.module.cwrap(
	'PL_put_dict', 'number', ['number','number','number','number','number']),
      PL_put_term_from_chars: this.module.cwrap(
	'PL_put_term_from_chars', 'number',['number','number','number','string']),
      PL_put_term: this.module.cwrap(
	'PL_put_term', 'number', ['number', 'number']),
      PL_write_term: this.module.cwrap(
	'PL_write_term', 'number', ['number', 'number', 'number', 'number']),
      PL_call: this.module.cwrap(
	'PL_call', 'number', ['number', 'number']),
      PL_open_foreign_frame: this.module.cwrap(
	'PL_open_foreign_frame', 'number', []),
      PL_close_foreign_frame: this.module.cwrap(
	'PL_close_foreign_frame', 'number', ['number']),
      PL_discard_foreign_frame: this.module.cwrap(
	'PL_close_foreign_frame', 'number', ['number']),
      PL_predicate: this.module.cwrap(
	'PL_predicate', 'number', ['number', 'number', 'number']),
      PL_open_query: this.module.cwrap(
	'PL_open_query', 'number', ['number', 'number', 'number', 'number']),
      PL_next_solution: this.module.cwrap(
	'PL_next_solution', 'number', ['number']),
      PL_close_query: this.module.cwrap(
	'PL_close_query', 'number', ['number']),
      PL_cut_query: this.module.cwrap(
	'PL_cut_query', 'number', ['number']),
      PL_exception: this.module.cwrap(
	'PL_exception', 'number', ['number']),
      PL_raise_exception: this.module.cwrap(
	'PL_raise_exception', 'number', ['number']),
      PL_query_engine: this.module.cwrap(
	'PL_query_engine', 'number', ['number']),
      PL_query_arguments: this.module.cwrap(
	'PL_query_arguments', 'number', ['number']),
      PL_set_query_data: this.module.cwrap(
	'PL_set_query_data', 'number', ['number', 'number', 'number']),
      PL_query_data: this.module.cwrap(
	'PL_query_data', 'number', ['number', 'number']),
      PL_current_engine: this.module.cwrap(
	'PL_current_engine', 'number', []),
      PL_create_engine: this.module.cwrap(
	'PL_create_engine', 'number', ['number']),
      PL_destroy_engine: this.module.cwrap(
	'PL_destroy_engine', 'number', ['number']),
      _PL_switch_engine: this.module.cwrap(
	'_PL_switch_engine', 'number', ['number']),
      _PL_reset_engine: this.module.cwrap(
	'_PL_reset_engine', 'number', ['number', 'number']),
      PL_set_trace_action: this.module.cwrap(
	'PL_set_trace_action', 'number', ['number']),
      PL_get_trace_context: this.module.cwrap(
	'PL_get_trace_context', 'number', ['number']),
      PL_prompt_string: this.module.cwrap(
	'PL_prompt_string', 'number', ['number']),
      WASM_ttymode: this.module.cwrap(
	'WASM_ttymode', 'number', []),
      WASM_bind_standard_streams: this.module.cwrap(
	'WASM_bind_standard_streams', 'number', []),
      WASM_yield_request: this.module.cwrap(
	'WASM_yield_request', 'number', []),
      WASM_set_yield_result: this.module.cwrap(
	'WASM_set_yield_result', 'number', ['number']),
      WASM_variable_id: this.module.cwrap(
	'WASM_variable_id', 'number', ['number']),
      js_unify_obj: this.module.cwrap(
	'js_unify_obj', 'number', ['number', 'number']),
      js_get_obj: this.module.cwrap(
	'js_get_obj', 'number', ['number'])
    };
  }

  __init_main_engine(name)
  { const eid = this.bindings.PL_current_engine();
    return new this.Engine(name, {prolog:this, eid:eid});
  }

/**
 * Return the active Engine.  If this is not known to
 * JavaScript, give it a unique name.
 * @return {Engine} or `undefined`
 */
  current_engine()
  { const eid = this.bindings.PL_current_engine();
    if ( eid )
    { if ( this.__id_engines[eid] )
      { return this.__id_engines[eid];
      } else
      { return new this.Engine(undefined, {prolog:this, eid:eid});
      }
    }
    // else `undefined`
  }

  __put_goal(goal, term) {
    if ( typeof goal === "string" ) {
      if ( !this.chars_to_term(goal, term) )
	throw new Error(`Query has a syntax error: ${query}`);
    } else if ( typeof goal === "object" &&
		goal instanceof this.Compound ) {
      this.toProlog(goal, term);
    } else {
      throw new TypeError("string or compound expected");
    }
  }

/**
 * Call a Prolog goal.  This function deals with many variations to
 * call Prolog.
 *
 * @param {String}  goal Goal to run
 * @param {String}  [opts.module] Module in which to call Goal
 * @param {Boolean} [opts.async]  Call as yieldable
 * @param {Boolean} [opts.nodebug]  Do not debug the goal
 */

  call(goal, opts) {
    if ( opts && opts.async ) {
      return this.__call_yieldable(goal, opts);
    } else {
      return this.with_frame(function() {
	if ( opts ) {		// not possible during initialization
	  return !!this
	    .query("call(Goal)",
		   {Goal:new this.Term(goal)}, opts)
	    .once();
	} else {
	  const term = this.new_term_ref();

	  this.__put_goal(goal, term);
	  return !!this.bindings.PL_call(term, this.MODULE_user);
	}
      });
    }
  }

/**
 * Call code while reclaiming possibly allocated term_t references.
 * @param {Function} f function to be called
 * @param {Boolean} [persist] if `false`, discard all binding created
 * within the scope of the frame;
 */

  with_frame(func, persist)
  { const fid = this.bindings.PL_open_foreign_frame();
    if ( fid )
    { const rc = func.call(this);
      if ( persist === false )
	this.bindings.PL_discard_foreign_frame(fid);
      else
	this.bindings.PL_close_foreign_frame(fid);
      return rc;
    }
    return false;				/* Throw? */
  }

  __string_to_c(string)
  { const len = this.module.lengthBytesUTF8(string);
    const ptr = _malloc(len+1);

    this.module.stringToUTF8(string, ptr, len+1);
    return { ptr:    ptr,
	     length: len
	   }
  }

/**
 * Get a reference to a predicate
 *
 * @param name   {String}  Name of the predicate.  If this is the only
 *			   argument it encodes module, name and arity
 *			   as `[module:]name/arity`
 * @param arity  {Integer} Arity of the predicate
 * @param module {String}  Module to resolve the predicate
 */

  predicate(name, arity, module)
  { if ( arity === undefined )
    { let ar = /^([^:]+):(.*)\/([0-9]+)$/.exec(name);

      if ( ar )
      { module = ar[1];
	name   = ar[2];
	arity  = parseInt(ar[3]);
      } else
      { ar = /(.*)\/([0-9]+)$/.exec(name);
	if ( ar)
	{ name   = ar[1];
	  arity  = parseInt(ar[2]);
	}
      }

      if ( arity === undefined )
	throw(`Prolog.predicate: illegal specification: ${name}`);
    }

    const c_name   = stringToNewUTF8(name);
    const c_module = stringToNewUTF8(module||"user");

    const pred = this.bindings.PL_predicate(c_name, arity, c_module);

    _free(c_name);
    _free(c_module);

    return pred;
  }

/**
 * Lookup a Prolog module by name
 *
 * @param {String} name Name of the module
 * @return {module_t} Handle to the Prolog module
 */

  new_module(name)
  { const c_atom = this.new_atom(name);

    const module = this.bindings.PL_new_module(c_atom);
    this.unregister_atom(c_atom);

    return module;
  }

/**
 * Consult a list of files.  Files are loaded ordered and may
 * be a mixture of local Prolog files and URLs.  Both `.pl` and
 * `.qlf` files are supported.
 */

  consult(...args)
  { let options = {};
    if ( args.length > 0 && typeof args[args.length-1] === "object" )
    { options = args.pop();
    }
    const opts = {};
    if ( options.engine )
      opts.engine = options.engine;
    const module = options.module||"user";
    return this.forEach("load_files(M:Files)",
			{M:module, Files:args},
			opts);
  }

  load_string(s, id)
  { if ( !id )
    { this.__script_id = (this.__script_id+1)||1;
      id = "/string/"+this.__script_id;
    }
    return this.forEach("setup_call_cleanup("+
			   "open_string(S, _In),"+
			   "load_files(user:Id, [stream(_In)]),"+
			   "close(_In))",
			{S:new this.String(s), Id:id});
  }

  async load_scripts()
  { const prolog = this;
    const scripts = document.querySelectorAll("script[type='text/prolog']");
    const loaded = [];

    for(let i = 0; i<scripts.length; i++)
    { const s = scripts[i];
      const name = `/script/${s.id||s.name||i+1}`;

      await prolog.load_string(s.text, name);
      loaded.push(name);
    }

    return name;
  }

  /**
   * Bind an event to a Prolog goal.
   * @param {HTMLElement} elem Elem on which to listen for events
   * @param {string} on Event type we listen for.  Passed to
   * addEventListener().
   * @param {string} goal Prolog goal to execute
   * @param {object} [input] Optional input parameters for `goal`.
   * @param {object} [options]
   * @param {bool} [options.async] If `true`, run the handler
   * asynchronously, i.e., using {@link Prolog#forEach}. Otherwise
   * use {@link Prolog#query} to call the handler on the global
   * Prolog engine.
   */

  bind(elem, on, goal, input, options) {
    const prolog = this;
    options = options||{};

    if ( options.async ) {
      elem.addEventListener(on, async (ev) => {
	prolog.forEach(goal,
		       {...input, Event__:ev},
		       {...options, engine:true});
      });
    } else {
      elem.addEventListener(on, (ev) => {
	prolog.query(goal,
		     {...input, Event__:ev},
		     options).once();
      });
    }
  }

  fetch(url, opts, type)
  { return fetch(url, opts).then((response) => response[type]());
  }

  url_properties(url)
  { return fetch(url, {method: 'HEAD'}).then((r) =>
    { if ( r.status == 200 )
      { let size = parseInt(r.headers.get("content-length"));
	const mods = r.headers.get("last-modified");
	const time = Date.parse(mods) || 0;

	if ( ! size instanceof Number )
	  size = -1;
	return { url: r.url,
		 status: r.status,
		 size: size,
		 last_modified: time/1000
	       };
      } else
      { return { url: url,
		 status: r.status
	       };
      }
    });
  }


/**
 * Convert a Prolog message term into a string.  Notably used to
 * translate Prolog exceptions to meaningful messages in the JavaScript
 * side.
 *
 * @return {String} string representation of the message
 */

  message_to_string(term)
  { return this.with_frame(() =>
    { const av = this.new_term_ref(2);

      this.bindings.PL_put_term(av+0, term);
      const flags = this.PL_Q_NORMAL;
      const pred  = this.predicate("message_to_string/2");
      const qid   = this.bindings.PL_open_query(0, flags, pred, av);
      let msg;

      if ( this.bindings.PL_next_solution(qid) )
	msg = this.get_chars(av+1);
      else
	msg = "Unknown Prolog exception";

      this.bindings.PL_close_query(qid);
      return msg;
    }, false);
  }

/**
 * When using `Module.on_output`, flush the output streams.
 */

  flush_output(stream)
  { if ( stream == undefined )
    { flush("stderr");
      flush("stdout");
    } else
    { flush(stream);
    }
  }

  log(...args)
  { log_output("stdout", args);
  }

  /**
   * Signature:
   *  - query(goal, [input])
   *  - query(module, flags, pred, argv, [map], [fid]) (deprecated)
   */

  query(...argv)
  { if ( typeof argv[3] === "number" )
      return this.__query(...argv)
    else
      return this.query2(...argv)
  }

  /** Run a  query from a  goal represented  as a string,  an optional
   * input object and optional options.
   *
   * @param {String} goal provides the goal using valid Prolog syntax
   * @param {Object} [input] provides bindings for a subset of the
   * variables in `goal`.  The remaining variables that do not start
   * with an `_` are used to form the result object.
   * @param {Boolean} [options.engine] If `true`, run the goal in a
   * temporary engine.  Default is to use the current engine.  See
   * also Engine.query()
   * @param {string} [options.string] Describes the target type for
   * JavaScript strings.  One of `atom` or `string` (default)
   */

  query2(goal, input, options)
  { const prolog = this;

    function __query(goal, input, options)
    { const fid = prolog.bindings.PL_open_foreign_frame();
      const av = prolog.new_term_ref(3);

      input = input||{};
      options = options||{};
      prolog.put_chars(av+0, goal);
      if ( options.string )
	prolog.toProlog(input, av+1, {string:options.string});
      else
	prolog.toProlog(input, av+1);

      const q = new Query(prolog, 0, prolog.PL_Q_CATCH_EXCEPTION,
			  "wasm_call_string/3", av,
			  { map: (a) => prolog.toJSON(a+2),
			    frame: fid,
			    ...options
			  });
      q.from_text = true;
      return q;
    }

    if ( options && options.engine )
    { const e = new prolog.Engine({auto_close:true});
      return e.with(() => __query(goal, input, options))
    } else
    { return __query(goal, input, options)
    }
  }

  __query(module, flags, pred, argv, map, fid)
  { return new Query(this, module, flags, pred, argv,
		     { map:map, frame:fid});
  }

  /**
   * Run a possibly long running goal and process its answers.
   * Signature:
   *  - foreach(goal, [input], [callback], [options])
   * @return {Promise} that is resolved on completion and rejected on
   * a Prolog exception.
   */

  forEach(goal, ...args)
  { const prolog = this;
    let input;
    let callback;
    let options;

    if ( typeof(args[0]) === "object" )
    { input = args[0];
      args.shift();
    } else
      input = {};

    if ( typeof(args[0]) === "function" )
    { callback = args[0];
      args.shift();
    }

    if ( typeof(args[0]) === "object" )
    { options = args[0];
    } else
      options = {};

    function __foreach(goal, input, callback, heartbeat)
    { const fid = prolog.bindings.PL_open_foreign_frame();
      const av = prolog.new_term_ref(4);
      prolog.put_chars(av+0, goal);
      prolog.toProlog(input, av+1);
      if ( heartbeat !== undefined )
	prolog.toProlog(heartbeat, av+3);

      const q = new Query(prolog, prolog.MODULE_user,
			  prolog.PL_Q_ALLOW_YIELD|prolog.PL_Q_CATCH_EXCEPTION,
			  "wasm_call_string_with_heartbeat/4", av,
			  { map: (a) => prolog.toJSON(a+2),
			    frame: fid
			  });

      return new Promise(function(resolve, reject) {
	let answers = callback ? 0 : [];

	function next_foreach(rc)
	{ while(true)
	  { if ( rc.yield !== undefined )
	    { switch(rc.yield)
	      { case "beat":
		  return setTimeout(() =>
		    q.engine.with(() =>
		      next_foreach(rc.resume("true"))));
		case "builtin":
		  return rc.resume((rc) => next_foreach(rc));
		default:		// unsupported yield
		  throw(rc);
	      }
	    } else if ( rc.value )
	    { if ( callback )
	      { answers++;
		callback.call(prolog, rc.value);
	      } else
	      { answers.push(rc.value);
	      }

	      if ( rc.done == false )
	      { rc = q.next_yieldable();
		continue;
	      }
	    }

	    q.close();
	    if ( rc.error )
	      return reject(rc.message);
	    if ( rc.done )
	      return resolve(answers);
	  }
	}

	return next_foreach(q.next_yieldable());
      });
    } // end __foreach()

    if ( options.engine )
    { const e = new prolog.Engine({auto_close:true});
      return e.with(() => __foreach(goal, input, callback, options.heartbeat))
    } else
    { return __foreach(goal, input, callback, options.heartbeat);
    }
  }


  abort()
  { this.abort_request = true;
  }

  /**
   * Create an abortable promise that represents sleeping
   * @param {Number} time to sleep in seconds.
   * @return {Promise} This promise represents the sleep and provides a
   * method abort() that aborts the sleep.
   */

  promise_sleep(time)
  { const f = function(resolve, reject)
    { f.reject = reject;
      f.timer = setTimeout(() =>
	{ f.timer = undefined;
	  resolve(true);
	}, time*1000);
    };
    f.abort = function()
    { if ( f.timer )
      { clearTimeout(f.timer);
	f.timer = undefined;
	f.reject("abort");
      }
      return true;
    }

    return new this.Promise(f);
  }

  /**
   * Create an abortable Promise that waits for an event on
   * an HTMLElement.
   *
   * @param {HTMLElement} item The element on which the event is expected
   * @param {string} eventType The event type as used by addEventListener()
   * @return {Promise} A promise that is accepted if the event occurs
   * and returns the event.  The promise can be aborted, in which case
   * it is rejected.
   */
  promise_event(item, eventType) {
    const f = function(resolve, reject) {
      f.reject = reject;
      const listener = (ev) => {
	item.removeEventListener(eventType, listener);
	resolve(ev);
      }
      item.addEventListener(eventType, listener);
      f.abort = function() {
	item.removeEventListener(eventType, listener);
	f.reject("abort");
      }
    }

    return new this.Promise(f);
  }

/**
 * @return {IOSTREAM*} as a number (pointer)
 */

  stream(name)
  { const iob = this.bindings._PL_streams(); /* IOSTREAM** */
    let offset = undefined;

    switch(name)
    { case "user_input":  offset = 0; break;
      case "user_output": offset = 1; break;
      case "user_error":  offset = 2; break;
      default: throw(`Unknown stream ${name}`);
    }

    return this.module.getValue(iob+offset*4, 'i32');
  }


  write(term, opts)
  { opts = opts||{};
    let s;
    const precedence = opts.precedence||1200;
    let   flags	     = opts.flags === undefined
			? (this.PL_WRT_QUOTED|this.PL_WRT_NEWLINE)
			: opts.flags;

    const map = { "quoted":     this.PL_WRT_QUOTED,
		  "ignore_ops": this.PL_WRT_IGNOREOPS,
		  "portray":    this.PL_WRT_PORTRAY,
		  "numbervars": this.PL_WRT_NUMBERVARS,
		  "nl":         this.PL_WRT_NEWLINE
		};

    for(const k in map)
    { if ( opts[k] === true )
      { flags |= map[k];
      } else if ( opts[k] === false )
      { flags &= ~map[k];
      }
    }

    if ( opts.stream )
    { if ( typeof(stream) === "string" )
	s = this.stream(opts.stream);
    } else
    { s = this.stream("user_output");
    }

    return this.bindings.PL_write_term(s, term, precedence, flags);
  }


// Return the arity of the given functor.
  functor_arity(functor)
  { return this.bindings.PL_functor_arity(functor);
  }

// Return an atom representing the name of the given functor.
  functor_name(functor)
  { return this.bindings.PL_functor_name(functor);
  }

// Returns functor of the given term.
// Returns null when the term is not a compound.
  get_functor(term)
  { const ptr = _malloc(4);
    let result;

    if ( this.bindings.PL_get_functor(term, ptr) )
      result = this.module.getValue(ptr, 'i32');
    else
      result = null;

    _free(ptr);
    return result;
  }

// Returns integer number for the given term.
// Returns null when the term is not an integer.
  get_integer(term)
  { const ptr = _malloc(8);
    let rc;

    if ( this.bindings.PL_get_int64(term, ptr) )
    { rc = this.module.getValue(ptr, 'i64');
      if ( rc >= Number.MIN_SAFE_INTEGER && rc <= Number.MAX_SAFE_INTEGER )
	rc = Number(rc);
    } else
    { const s = this.get_chars(term, this.CVT_INTEGER);
      rc = BigInt(s);
    }
    _free(ptr);

    return rc;
  }

  get_float(term)
  { const ptr = _malloc(8);
    let rc;
    if (this.bindings.PL_get_float(term, ptr)) {
      rc = this.module.getValue(ptr, 'double');
    } else {
      rc = null;
    }
    _free(ptr);
    return rc;
  }

/**
 * Make a JavaScript string available to Prolog.  By default the
 * Prolog is represented as a Prolog string.
 *
 * @return {Boolean}
 */

  put_chars(term, string, flags)
  { flags  = flags||this.PL_STRING;
    flags |= this.REP_UTF8;

    const c = this.__string_to_c(string);
    const ret = !!this.bindings.PL_put_chars(term, flags, c.length, c.ptr);
    _free(c.ptr);
    return ret;
  }

  // See https://groups.google.com/g/emscripten-discuss/c/nQlUHq-Nk68
  put_bytes(term, array_buffer)
  { const content = new Uint8Array(array_buffer);
    return !!this.bindings.put_bytes(term,
				     this.PL_STRING|this.REP_ISO_LATIN_1,
				     content.length, content);
  }

  put_bigint(term, value)
  { const s = value.toString();
    return this.bindings.PL_put_term_from_chars(term, this.REP_UTF8, -1, s);
  }

// Unifies the terms. Returns false if the terms
// do not unify.
  unify(term1, term2)
  { return !!this.bindings.PL_unify(term1, term2);
  }

// Returns whether the term is a string.
  is_string(term)
  { return !!this.bindings.PL_is_string(term);
  }

  is_variable(term)
  { return !!this.bindings.PL_is_variable(term);
  }

// Return a C-string for the text represented by the given atom.
  atom_chars(atom)
  { const t = this.new_term_ref();

    this.bindings.PL_put_atom(t, atom);
    const str = this.get_chars(t, this.CVT_ATOM);
    this.bindings.PL_reset_term_refs(t);

    return str;
  }

// Get the TTY mode as one of "notty", "raw" or "cooked"
  ttymode()
  { return this.module.UTF8ToString(this.bindings.WASM_ttymode());
  }

  yield_request()
  { const tref = this.bindings.WASM_yield_request();

    return this.toJSON(tref);
  }

  set_yield_result(obj)
  { this.with_frame(() =>
    { const term = this.toProlog(obj, undefined, {string:"string"});

      if ( !term )
      { console.log("Could not convert", obj);
	throw("Could not convert JavaScript data to Prolog");
      }

      this.bindings.WASM_set_yield_result(term);
    }, true);
  }

  set_trace_action(obj)
  { this.with_frame(() =>
    { const term = this.toProlog(obj, undefined, {string:"atom"});

      if ( !term )
      { console.log("Could not convert", obj);
	throw("Could not convert JavaScript data to Prolog");
      }

      this.bindings.PL_set_trace_action(term);
    }, true);
  }

/**
 * Call a goal that may yield.  When no yield happens this returns the
 * result of Query.next().  If the predicate called await/2 an
 * the returned object contains a key `yield` that either
 * holds a string or a JSON object representing the request.  The key
 * `resume` is a function that should be called to resume Prolog with
 * a value that appears in the second argument of js_call/2.
 *
 * The `yield` key can be `builtin`, in which case an object is returned
 * that contains a `resume` key that executes the built-in and continues
 * using the passed function. The returned object may provide an `abort`
 * key to abort the query immediately.
 *
 * @param {String} goal  Prolog goal to be called
 * @param {Object} [options] Additional options
 * @param {String} [options.module] Module in which to call the goal.
 * @return Either the result of Query.next() or a _yield_ request as
 * described above.
 */

  __call_yieldable(goal, opts)
  { opts = opts||{};
    const pred_call1 = this.predicate("call", 1, "system");
    const flags = this.PL_Q_NORMAL|this.PL_Q_ALLOW_YIELD;
    const fid = this.bindings.PL_open_foreign_frame();
    const term = this.new_term_ref();
    this.__put_goal(goal, term);
    const q = new Query(this, opts.module, flags, pred_call1, term,
			{ ...opts, frame:fid });
    return q.next_yieldable();
  }


		 /*******************************
		 *	     CONVERSION		*
		 *******************************/

/**
 * Define arguments name for a functor.  For example
 *
 *     Prolog.functor_arg_names("point", ["x", "y"]);
 */

  set_arg_names(name, args)
  { if ( !this.functor_arg_names_[name] )
      this.functor_arg_names_[name] = {};
    this.functor_arg_names_[name][args.length] = args;
  }

  arg_names(name, arity)
  { if ( this.functor_arg_names_[name] )
      return this.functor_arg_names_[name][arity];
  }

/**
 * Convert a Prolog term into a JavaScript object.  This follows
 * https://github.com/SWI-Prolog/packages-mqi/issues/4
 */

  toJSON(term, options)
  { options = options||{};

    function toJSON(prolog, term, options)
    { switch ( prolog.bindings.PL_term_type(term) )
      { case prolog.PL_VARIABLE:
	  return new prolog.Var(prolog.bindings.WASM_variable_id(term));
	case prolog.PL_STRING:
	  if ( options.string !== "string" )
	    return new prolog.String(prolog.get_chars(term));
	  /*FALLTHROUGH*/
	case prolog.PL_ATOM:
	  return prolog.get_chars(term);
	case prolog.PL_NIL:
	  return [];
	case prolog.PL_BLOB:
	{ const id = prolog.bindings.js_get_obj(term);

	  if ( id != -1 )
	    return prolog.objects[id];

	  return new prolog.Blob();
	}
	case prolog.PL_INTEGER:
	  return prolog.get_integer(term);
	case prolog.PL_RATIONAL:
	{ let s = prolog.get_chars(term, prolog.CVT_RATIONAL);
	  let a = s.split("r");

	  function toInt(s)
	  { const bi = BigInt(s);
	    if ( bi >= Number.MIN_SAFE_INTEGER && bi <= Number.MAX_SAFE_INTEGER )
	      return Number(bi);
	    return bi;
	  }

	  return new prolog.Rational(toInt(a[0]), toInt(a[1]));
	}
	case prolog.PL_FLOAT:
	  return prolog.get_float(term);
	case prolog.PL_TERM:
	{ const f     = prolog.get_functor(term);
	  const name  = prolog.atom_chars(prolog.functor_name(f));
	  const arity = prolog.functor_arity(f);
	  const map   = prolog.arg_names(name, arity);
	  const a     = prolog.new_term_ref();

	  if ( map )
	  { let result = { $tag: name };
	    for(var i=0; i<arity; i++)
	    { prolog.get_arg(i+1, term, a);
	      result[map[i]] = toJSON(prolog, a, options);
	    }

	    return result;
	  } else
	  { const args  = [];
	    let result  = {$t: "t"};

	    for(var i=1; i<=arity; i++)
	    { prolog.get_arg(i, term, a);
	      args.push(toJSON(prolog, a, options));
	    }

	    return new prolog.Compound(name, args);
	  }
	}
	case prolog.PL_LIST_PAIR:
	{ let result = [];
	  const h = prolog.bindings.PL_new_term_ref();
	  const t = prolog.bindings.PL_copy_term_ref(term);
	  while( prolog.bindings.PL_get_list(t, h, t) )
	  { result.push(toJSON(prolog, h, options));
	  }
	  if ( prolog.bindings.PL_get_nil(t) )
	    return result;

	  return new prolog.List(result, toJSON(prolog, t, options));
	}
	case prolog.PL_DICT:
	{ let result = {};
	  let a = prolog.new_term_ref();

	  prolog.get_arg(1, term, a);
	  if ( !prolog.is_variable(a) )
	    result['$tag'] = toJSON(prolog, a, options);
	  for(var i=2; ; i+=2)
	  { if ( prolog.get_arg(i+1, term, a) )
	    { let key = toJSON(prolog, a, options);
	      prolog.get_arg(i, term, a);
	      result[key] = toJSON(prolog, a, options);
	    } else
	      break;
	  }

	  return result;
	}
	default:
	  return undefined;
      }
    }

    return toJSON(this, term, options);
  }

  toProlog(data, term, ctx)
  { ctx = ctx||{};

    function toProlog(prolog, data, term, ctx)
    { term = term||prolog.new_term_ref();
      let rc;

      function toList(term, data, tail)
      { let h = prolog.new_term_ref();
	let rc = true;

	if ( tail )
	  rc = toProlog(prolog, tail, term, ctx);
	else
	  rc = prolog.bindings.PL_put_nil(term);

	for(var i=data.length-1; i >= 0 && rc; i--)
	{ rc = ( toProlog(prolog, data[i], h, ctx) &&
		 prolog.bindings.PL_cons_list(term, h, term) );
	}

	return rc;
      }

      switch(typeof(data))
      { case "number":
	  if ( Number.isInteger(data) )
	    rc = prolog.bindings.PL_put_integer(term, data);
	  else
	    rc = prolog.bindings.PL_put_float(term, data);
	  break;
	case "bigint":
	  rc = prolog.put_bigint(term, data);
	  break;
	case "string":
	{ const flags = ctx.string === "string" ? prolog.PL_STRING
						: prolog.PL_ATOM;
	  rc = prolog.put_chars(term, data, flags);
	  break;
	}
	case "boolean":
	  rc = prolog.put_chars(term, data ? "true" : "false", prolog.PL_ATOM);
	  break;
	case "undefined":
	  rc = prolog.put_chars(term, "undefined", prolog.PL_ATOM);
	  break;
	case "object":
	  if ( data === null )
	  { rc = prolog.put_chars(term, "null", prolog.PL_ATOM);
	  } else if ( Array.isArray(data) )
	  { rc = toList(term, data);
	  } else if ( data.$t )
	  { switch( data.$t )
	    { case "s":
		rc = prolog.put_chars(term, data.v, prolog.PL_STRING);
		break;
	      case "r":
	      { const s = data.n+"r"+data.d;
		rc = prolog.bindings.PL_put_term_from_chars(term, prolog.REP_UTF8,
							    -1, s);
		break;
	      }
	      case "t":
	      { const keys  = Object.keys(data);
		let args;
		let name;

		for(var i=0; i<keys.length; i++)
		{ if ( Array.isArray(data[keys[i]]) )
		  { if ( args === undefined )
		    { name = keys[i];
		      args = data[name];
		    } else
		      console.log('Object with .$t:"t" is ambiguous', data);
		  }
		}

		if ( args !== undefined )
		{ const av = prolog.new_term_ref(args.length);
		  const f  = prolog.new_functor(prolog.new_atom(name),
						args.length);

		  rc = true;
		  for(var i=0; i<args.length && rc; i++)
		    rc = toProlog(prolog, args[i], av+i, ctx);

		  rc = rc && prolog.bindings.PL_cons_functor_v(term, f, av);
		}
		break;
	      }
	      case "v":
	      { rc = prolog.bindings.PL_put_variable(term);

		if ( data.v )
		{ if ( !ctx.vars ) ctx.vars = {};
		  if ( ctx.vars[data.v] )
		  { rc = ( prolog.bindings.PL_put_variable(term) &&
			   prolog.unify(term, ctx.vars[data.v]) );
		  } else
		  { ctx.vars[data.v] = prolog.bindings.PL_copy_term_ref(term);
		  }
		}
		break;
	      }
	      case "l":
	      { rc = toList(term, data.v, data.t);
		break;
	      }
	      case "term_t":
	      { rc = prolog.bindings.PL_put_term(term, data.term_t);
		break;
	      }
	      default:
	      { console.log(`Object with invalid $t:${data.$t}`);
	      }
	    }
	    break;
	  } else
	  { switch( data.constructor.name )
	    { case "ArrayBuffer":
	      { rc = prolog.put_bytes(term, data);
		break;
	      }
	      case "Object":
	      { const keys  = Object.keys(data);
		const len   = keys.length;
		const av    = prolog.new_term_ref(len);
		const atoms = _malloc(4*len);
		let   tag   = 0;

		const class_name = data.constructor.name;
		if ( class_name != "Object" )
		  tag = prolog.new_atom(class_name);

		rc = true;
		for(var i=0; i<len && rc; i++)
		{ rc = toProlog(prolog, data[keys[i]], av+i, ctx);
		  prolog.module.setValue(atoms+4*i,
					 prolog.new_atom(keys[i]),
					 'i32');
		}

		rc = rc && prolog.bindings.PL_put_dict(term, tag, len,
						       atoms, av);
		_free(atoms);
		break;
	      }
	      default:
	      { let id = prolog.object_ids.get(data);

		prolog.bindings.PL_put_variable(term);
		if ( id === undefined )
		{ id = prolog.next_object_id+1;
		  rc = prolog.bindings.js_unify_obj(term, id);
		  if ( rc )
		  { prolog.object_ids.set(data, id);
		    prolog.objects[id] = data;
		    prolog.next_object_id = id;
		  }
		} else
		{ rc = prolog.bindings.js_unify_obj(term, id);
		}
	      }
	    }
	    break;
	  }
	  break;
	default:
	  return null;
      }

      return rc ? term : null;
    }

    return toProlog(this, data, term, ctx);
  }

/**
 * Parse string and put the resulting Prolog term into the term t.
 */

  chars_to_term(string, t) {
    return !!this.bindings.PL_put_term_from_chars(t, this.REP_UTF8, -1, string);
  };

// Converts the argument term to a string.
  get_chars(term, flags)
  { const ptr = _malloc(4);
    let rc;
    flags  = flags||(this.CVT_ALL|this.CVT_WRITEQ);
    flags |= this.CVT_EXCEPTION|this.REP_UTF8;
    if (this.bindings.PL_get_chars(term, ptr, flags)) {
	rc = this.module.UTF8ToString(this.module.getValue(ptr, 'i32'));
    } else {
	rc = null;
    }
    _free(ptr);

    return rc;
  }

  prompt_string(fd) {
    if ( fd == 0 ) {
      iostream = this.stream("user_input");
      const bytes = this.bindings.PL_prompt_string(iostream);
      if ( bytes )
	return this.module.UTF8ToString(bytes);
    }
  }

// If t is compound and index is between 1 and arity (inclusive),
// assign a with a term reference to the argument.
  get_arg(index, term, arg) {
    return !!this.bindings.PL_get_arg(index, term, arg);
  }

// Return an atom handle for the given C-string.
  new_atom(string)
  { return this.bindings.PL_new_atom(string);
  }

/**
 * Register an atom
 */
  register_atom(atom)
  { this.bindings.PL_register_atom(atom);
    return atom;
  }

  unregister_atom(atom)
  { this.bindings.PL_unregister_atom(atom);
  }

// Returns a functor identifier, a handle for the name/arity pair.
  new_functor(atom, arity)
  { return this.bindings.PL_new_functor(atom, arity);
  }

// Return a fresh reference to a term.
  new_term_ref(count)
  { return count === undefined ? this.bindings.PL_new_term_ref()
			       : this.bindings.PL_new_term_refs(count);
  }

// Create a new compound term from functor and bind t to this term.
  put_functor(term, functor)
  { return this.bindings.PL_put_functor(term, functor);
  }
}  /* End of class Prolog */


/**
 * class Engine([name], [options])
 *
 * @param {String} [name] is the identifier name of the engine.  When
 * omitted, a _genid_ `engine<n>` is created.
 * @param {Prolog} [options.prolog] identifies the Prolog instance.
 * Only used to create the initial engine.
 * @param {engine_t} [options.eid] is the WASM identifier for the engine.
 * @param {Boolean}  [options.auto_close] Causes the engine to be closed
 * when the last query is closed.
 */

const class_engine = (class Engine{
  constructor(...argv)
  { let name;
    let options = {};

    if ( typeof argv[0] === 'object' )
    { options = argv[0];
    } else if ( typeof argv[0] === 'string' )
    { name = argv[0];
      if ( typeof argv[1] === 'object' )
      { options = argv[1];
      }
    }

    const prolog = options.prolog||Module.prolog;
    if ( name && prolog.engines[name] )
      return prolog.engines[name];
    name = name||("engine" + ++prolog.__engine_id);
    const eid = options.eid ? options.eid
                            : prolog.bindings.PL_create_engine(0);
    this.name = name;
    this.eid = eid;
    this.prolog = prolog;
    this.open = true;
    prolog.__id_engines[eid] = this;
    prolog.engines[name]   = this;
    this.open_queries = [];
    this.lastyieldat = 0;
    this.auto_close = !!options.auto_close;
  }

  __push_query(q)
  { this.open_queries.push(q);
  }

  __pop_query(q)
  { this.__must_be_innermost_query(q);
    this.open_queries.pop();
    if ( this.auto_close && this.open_queries.length == 0 )
      this.close();
  }

  __must_be_innermost_query(q)
  { if ( q != this.open_queries.at(-1) )
      throw new Error("Attempt to access not innermost query");
  }

  close()
  { if ( this.open )
    { if ( this.name === "main" )
        throw new Error('Cannot close "main" engine')
      this.prolog.bindings.PL_destroy_engine(this.eid);
      delete this.prolog.__id_engines[this.eid];
      delete this.prolog.engines[this.name];
      this.open = false;
    }
  }


  /**
   * Run Prolog goal on a specific engine
   */
  call(goal, opts)
  { return this.with(() => this.prolog.call(goal, opts));
  }

  /**
   * Run a query on a specific engine
   */
  query(...args)
  { return this.with(() => this.prolog.query(...args));
  }

  /**
   * Run Prolog goals on a specific engine
   */
  forEach(goal, ...args)
  { return this.with(() => this.prolog.forEach(goal, ...args));
  }

  /**
   * Create a frame on a specific engine
   */
  with_frame(func, persist)
  { return this.with(() => this.prolog.with_frame(func, persist));
  }

  /**
   * Run code using a given engine.
   * @param engine is the engine to use
   * @param func is the code to execute under this engine.
   */

  with(func)
  { const old = this.prolog.bindings._PL_switch_engine(this.eid);
    let rc;
    if ( old )
      rc = func.call(this);
    this.prolog.bindings._PL_reset_engine(old);
    return rc;
  }
});

/**
 * Open a new query.  Signature:
 *
 *     new Query(module:{String|0},
 *               flags:{Integer},
 *               predicate:{String|predicate_t},
 *               argv:{term_t}
 *               [options]: {Object}
 *
 * @param {String} [module] Optional module name
 * @param {Object} [options] Optional options
 * @param {Function} [options.map] Function to map `term_t` into
 *        properties of the Query.next() result object.
 * @param {fid_t} [options.frame] Prolog frame used to create
 *        the query.  Must be closed when the query is closed.
 */

class Query {
  constructor(prolog, module, flags, pred, argv, options)
  { options = options||{};
    module = typeof module === "string" ? prolog.new_module(module) : 0;
    if ( typeof(pred) === "string" )
      pred = prolog.predicate(pred);
    flags |= prolog.PL_Q_EXT_STATUS;
    if ( !(flags & (prolog.PL_Q_CATCH_EXCEPTION|
		    prolog.PL_Q_PASS_EXCEPTION|
		    prolog.PL_Q_NORMAL)) )
      flags |= prolog.PL_Q_CATCH_EXCEPTION;
    if ( options.debugger )
      flags |= prolog.PL_Q_TRACE_WITH_YIELD;
    if ( options.nodebug )
      flags |= prolog.PL_Q_NODEBUG;

    this.options = options;
    this.flags   = flags;
    this.prolog  = prolog;
    this.engine  = prolog.current_engine();
    this.qid     = prolog.bindings.PL_open_query(module, flags, pred, argv);
    this.open    = true;
    this.argv    = argv;
    this.engine.__push_query(this);
  }

  [Symbol.iterator]() {
    this.is_iterator = true;
    return this;
  }

  // Run on engine of query
  once()           { return this.engine.with(() => this.__once()); }
  next()           { return this.engine.with(() => this.__next()); }
  next_yieldable() { return this.engine.with(() => this.__next_yieldable()); }
  close()          { return this.engine.with(() => this.__close()); }

  // Actual implementations, running on current engine
  __next()
  { const prolog = this.prolog;
    const engine = this.engine;
    const argv   = this.argv;

    if ( !this.open )
      return { done: true };

    engine.__must_be_innermost_query(this);

    function map_result(query, argv)
    { if ( query.options.map )
        return query.options.map.call(query, argv);
      return argv;
    }

    switch(prolog.bindings.PL_next_solution(this.qid))
    { case prolog.PL_S_EXCEPTION:
      { /* `value` is `undefined` */
	const msg = prolog.message_to_string(prolog.bindings.PL_exception(this.qid));
	if ( this.flags & prolog.PL_Q_NORMAL)
	  console.error(msg);
	else
	  console.log(msg);
	this.__close();
	return { done: !this.is_iterator, error: true, message: msg };
      }
      case prolog.PL_S_FALSE:
	this.__close();
	return { done: true };
      case prolog.PL_S_LAST:
      { const rc = { done: !this.is_iterator,
		     value: map_result(this, argv)
		   };
	this.__close();
        return rc;
      }
      case prolog.PL_S_TRUE:
	return { done: false,
		 value: map_result(this, argv)
	       };
      case prolog.PL_S_YIELD:
      { let request = prolog.yield_request();

	return { done: false,
		 value: null,
		 yield: request,
		 resume: (value) =>
		 { prolog.set_yield_result(value);
		   return this.__next();
		 }
	       };
      }
      case prolog.PL_S_YIELD_DEBUG:
      { const event = prolog.new_term_ref(1);
	prolog.bindings.PL_get_trace_context(event);

	return { done: false,
		 value: null,
		 trace_event: event,
		 yield: "trace",
		 resume: (value) =>
		 { prolog.set_trace_action(value);
		   return this.__next();
		 }
	       };
      }
    }
  }

  __next_yieldable()
  { function ynext(query)
    { const prolog = query.prolog;
      const engine = query.engine;

      while(true)
      { let rc = query.__next();

	if ( rc.yield !== undefined )
	{ const request = rc.yield;

	  if ( prolog.abort_request )
	  { prolog.abort_request = undefined;
	    prolog.set_yield_result("abort");
	    continue;
	  }

	  if ( request === "beat" )
	  { const now = Date.now();
	    const passed = now - engine.lastyieldat;

	    if ( passed < 20 )
	    { prolog.set_yield_result("true");
	      continue;
	    }
	    engine.lastyieldat = now;
	  } else if ( request instanceof Promise )
	  { let result = { yield: "builtin",
			   request: request,
			   query: query,
			   resume: (cont) =>
			   { if ( typeof(cont) === "string" )
			     { return query.engine.with(() => {
				 prolog.set_yield_result(cont);
				 return ynext(query);
			       });
			     } else
			     { result.cont = cont;
			       request
			       .then((value) =>
				 { return query.engine.with(() => {
				     prolog.set_yield_result(value);
				     cont.call(prolog, ynext(query));
				   })
				 })
			       .catch((error) =>
				 { return query.engine.with(() => {
				     prolog.set_yield_result({$error: error});
				     cont.call(prolog, ynext(query));
				   })
				 })
			     }
			   },
			   abort: () =>
			   { if ( !(request.abort && request.abort()) )
			     { console.log("Cannot abort", request);
			       prolog.abort_request = true;
			     }
			   }
			 };
	    return result;
	  }

	  // Get back here instead of Query.ynext()
	  if ( request == "trace" ) {
	    rc.resume = (value) => {
	      return query.engine.with(() => {
		prolog.set_trace_action(value);
		return ynext(query);
	      });
	    }
	  } else {
	    rc.resume = (value) => {
	      return query.engine.with(() => {
		prolog.set_yield_result(value);
		return ynext(query);
	      });
	    };
	  }
	} else if ( rc.done === false ) {
	  rc.resume = () => ynext(query);
	}

	return rc;
      }
    }

    return ynext(this);
  }

  /**
   * Run a query once.
   * @return To simplify distinguishing success from failure the
   * returned object has a field `success` if the Prolog query
   * completed without an error.
   */

  __once()
  { const rc = this.next();
    this.__close();
    if ( this.from_text )
    { delete rc.done;
      if ( rc.value )
      { rc.value.success = true;
	return rc.value;
      } else
      { if ( !rc.error )
	  rc.success = false;
	return rc;
      }
    } else
    { return rc.value ? rc.value : rc;
    }
  }

  __close()
  { if ( this.open )
    { const prolog = this.prolog;
      const engine = this.engine;

      this.prolog.bindings.PL_cut_query(this.qid);
      if ( this.options.frame )
	this.prolog.bindings.PL_discard_foreign_frame(this.options.frame);
      this.open = false;
      engine.__pop_query(this);
    }
  }
}


		 /*******************************
		 *   BIND PROLOG TO THE MODULE  *
		 *******************************/

Module.onRuntimeInitialized = function()
{ Module.prolog = new Prolog(Module, arguments_);
};

		 /*******************************
		 *	 CALL JAVASCRIPT	*
		 *******************************/

/**
 * Call a chain of functions and selectors.  This function works on
 * the data structures created by `:=/2`
 */

function prolog_js_call(request, result)
{ const prolog = Module.prolog;

  function eval_chain(ar, obj)
  { obj = obj||window;

    function eval_one(obj, fname, args)
    { if ( args.length == 0 )
      { switch(fname)
	{ case "instanceof": return obj.constructor.name;
	}
      } else if ( args.length == 1 )
      { switch(fname)
	{ case "-": return -args[0];
	  case "!": return !args[0];
	  case "instanceof": return obj instanceof args[0];
	}
      } else if ( args.length == 2 )
      { switch(fname)
	{ case "+": return args[0] + args[1];
	  case "-": return args[0] - args[1];
	  case "*": return args[0] * args[1];
	  case "/": return args[0] / args[1];
	  case "&": return args[0] & args[1];
	  case "|": return args[0] | args[1];
	  case "&&": return args[0] && args[1];
	  case "||": return args[0] || args[1];
	}
      }

      if ( obj == null || obj == undefined )
	throw new TypeError(`${obj} has no attribute ${fname}`);

      const func = obj[fname];
      if ( typeof(func) === "function"  )
	return func.apply(obj, args);
      else
	throw new TypeError(`${obj}.${fname} is not a function`);
    }

    for(let i=0; i<ar.length; i++) {
      const next = ar[i];

      if ( typeof(next) === "string" ) {
	if ( i == 0 ) {
	  switch(next)
	  { case "prolog":
	      obj = prolog;
	      break;
	    case "window":
	      obj = window;
	      break;
	    default:
	      obj = eval(next);
	  }
	} else if ( obj == null || obj == undefined ) {
	  throw new TypeError(`${obj} has no attribute ${next}`);
	} else {
	  obj = obj[next];
	}
      } else if ( next.v !== undefined ) {
	obj = next.v;
      } else {
	const args = next.args.map((v) => eval_chain(v));

	obj = eval_one(obj, next.f, args);
      }
    }

    return obj;
  }

  try {
    return prolog.with_frame(() => {
      const ar = prolog.toJSON(request, { string: "string" });
      let obj;

      if ( ar.setter ) {
	const target = eval_chain(ar.target);
	const value  = eval_chain(ar.value);
	target[ar.setter] = value;
	obj = true;
      } else {
	obj = eval_chain(ar);
      }

      return prolog.unify(result, prolog.toProlog(obj));
    }, false);
  } catch (e) {
    return prolog.bindings.PL_raise_exception(
      prolog.toProlog(new prolog.Compound(
	"error",
	new prolog.Compound("js_eval_error",
			    e.toString(),
			    new prolog.Term(request)),
	new prolog.Var()
	)));
  }
}


function prolog_js_obj_class_name(id)
{ const prolog = Module.prolog;
  const obj = prolog.objects[id];

  return obj.constructor.name;
}

/**
 * Release an object held by Prolog
 */

function release_registered_object(id)
{ const prolog = Module.prolog;
  const obj = prolog.objects[id];

  prolog.object_ids.delete(obj);
  delete prolog.objects[id];
}

function flush_std_stream(s)
{ if ( s == 1 )
  { flush("stdout");
  } else if ( s == 2 )
  { flush("stderr");
  }
}

if ( globalThis.BigInt.prototype.toJSON === undefined )
{ globalThis.BigInt.prototype.toJSON = function ()
  { return this.toString();
  }
}

if ( globalThis.HTMLCollection && globalThis.HTMLCollection.prototype && !globalThis.HTMLCollection.prototype.toList )
{ globalThis.HTMLCollection.prototype.toList = function()
  { const ar = [];

    for(let i=0; i<this.length; i++)
      ar.push(this.item(i));

    return ar;
  }
}
