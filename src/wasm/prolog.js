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
  constructor(name, args)
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
  { this.module = module;
    this.args = args;
    this.lastyieldat = 0;
    this.functor_arg_names_ = {};
    this.objects = {};			// id --> Object
    this.object_ids = new WeakMap();	// objec --> id
    this.next_object_id = 0;
    this.open_queries = [];		// Stack with open queries

    this.__set_foreign_constants();
    this.__bind_foreign_functions();
    this.__export_classes();
    this.__initialize();
  }


  __initialize()
  { let argv0 = this.args || [];
    argv0.unshift("swipl");
    let argv = argv0.map(function(arg) {
        return this.module.allocate(
            this.module.intArrayFromString(arg),
            'i8', this.module.ALLOC_NORMAL);
    }, this);
    var ptr = this.module._malloc(argv.length * 4);
    argv.forEach(function(arg, i) {
        this.module.setValue(ptr + i * 4, arg, '*');
    }, this);
    if (!this.bindings.PL_initialise(argv.length, ptr)) {
        throw new Error('SWI-Prolog initialisation failed.');
    }
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
    this.List	  = class_list;
    this.Blob	  = class_blob;
    this.Promise  = class_abortable_promise;
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

    this.PL_S_EXCEPTION		 = -1;
    this.PL_S_FALSE		 = 0;
    this.PL_S_TRUE		 = 1;
    this.PL_S_LAST		 = 2;
    this.PL_S_YIELD		 = 255;

    this.PL_WRT_QUOTED		 = 0x0001;
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
      WASM_ttymode: this.module.cwrap(
        'WASM_ttymode', 'number', []),
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

/**
 * Call a Prolog goal.  This function deals with many variations to
 * call Prolog.
 *
 * @param {String}  goal Goal to run
 * @param {String}  [opts.module] Module in which to call Goal
 * @param {Boolean} [opts.async]  Call as yieldable
 */

  call(goal, opts)
  { opts = opts||{};

    if ( typeof(goal) === "string" )
    { if ( opts.async )
      { return this.__call_yieldable(goal, opts);
      } else
      { return this.with_frame(function()
        { const term = this.new_term_ref();

	  if ( !this.chars_to_term(goal, term) )
	    throw new Error('Query has a syntax error: ' + query);

	  const module = opts.module ? this.new_module(opts.module)
				     : this.MODULE_user;
	  return !!this.bindings.PL_call(term, module);
	});
      }
    }
  }

/**
 * Call code while reclaiming possibly allocated term_t references.
 * @param {Function} f function to be called
 * @param {Boolean} [persist] if `false`, discard all binding created
 * within the scope of the frame;
 */

  with_frame(f, persist)
  { const fid = this.bindings.PL_open_foreign_frame();
    if ( fid )
    { const rc = f.call(this);
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
    const ptr = this.module._malloc(len+1);

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

    const c_name   = allocateUTF8(name);
    const c_module = allocateUTF8(module||"user");

    const pred = this.bindings.PL_predicate(c_name, arity, c_module);

    this.module._free(c_name);
    this.module._free(c_module);

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
  { return this.forEach("load_files(user:Files)", {Files:args});
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


  bind(e, on, goal)
  { const prolog = this;

    e.addEventListener(on, (ev) =>
    { prolog.query(goal, {Event__:ev}).once();
    });
  }

  fetch(url, opts, type)
  { return fetch(url, opts).then((response) => response[type]());
  }

  url_properties(url)
  { return fetch(url, {method: 'HEAD'}).then((r) =>
    { if ( r.status == 200 )
      { const size = parseInt(r.headers.get("content-length"));
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
   *  - query(module, flags, pred, argv, [map], [fid])
   *  - query(goal[, input])
   */

  query(module, flags, pred, argv, map, fid)
  { if ( typeof(argv) === "number" )	   /* term_t array */
    { return new Query(this, module, flags, pred, argv, map);
    } else if ( typeof(module) === "string" && pred === undefined )
    { const goal = module;
      const fid = this.bindings.PL_open_foreign_frame();
      const av = this.new_term_ref(3);
      const input = flags||{};

      this.frame = fid;
      this.put_chars(av+0, goal);
      this.toProlog(input, av+1);
      const q = new Query(this, 0, this.PL_Q_CATCH_EXCEPTION,
			  "wasm_call_string/3", av,
			  (a) => this.toJSON(a+2));
      q.from_text = true;
      return q;
    }
  }


  /**
   * Run a possibly long running goal and process its answers.
   * Signature:
   *  - foreach(goal, [input], [callback])
   * @return {Promise} that is resolved on completion and rejected on
   * a Prolog exception.
   */


  forEach(goal, ...args)
  { const prolog = this;
    const fid = this.bindings.PL_open_foreign_frame();
    const av = this.new_term_ref(3);
    let callback;
    let input;

    if ( typeof(args[0]) === "object" )
    { input = args[0];
      callback = args[1];
    } else
    { input = {};
      callback = args[0];
    }

    if ( callback !== undefined && typeof(callback) !== "function" )
      throw TypeError("callback must be a function");

    this.frame = fid;
    this.put_chars(av+0, goal);
    this.toProlog(input, av+1);

    const q = new Query(this, this.MODULE_user,
			this.PL_Q_ALLOW_YIELD|this.PL_Q_CATCH_EXCEPTION,
			"wasm_call_string_with_heartbeat/3", av,
			(a) => this.toJSON(a+2));

    return new Promise(function(resolve, reject) {
      let answers = callback ? 0 : [];

      function next_foreach(rc)
      { while(true)
	{ if ( rc.yield !== undefined )
	  { switch(rc.yield)
	    { case "beat":
	        return setTimeout(() => next_foreach(rc.resume("true")), 0);
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

    const precedence = opts.precedence||1200;
    const flags	   = opts.flags == undefined ? this.PL_WRT_QUOTED|this.PL_WRT_NEWLINE
					       : flags;
    let s = undefined;

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
  { const ptr = this.module._malloc(4);
    let result;

    if ( this.bindings.PL_get_functor(term, ptr) )
      result = this.module.getValue(ptr, 'i32');
    else
      result = null;

    this.module._free(ptr);
    return result;
  }

// Returns integer number for the given term.
// Returns null when the term is not an integer.
  get_integer(term)
  { const ptr = this.module._malloc(8);
    let rc;

    if ( this.bindings.PL_get_int64(term, ptr) )
    { rc = this.module.getValue(ptr, 'i64');
      if ( rc >= Number.MIN_SAFE_INTEGER && rc <= Number.MAX_SAFE_INTEGER )
	rc = Number(rc);
    } else
    { const s = this.get_chars(term, this.CVT_INTEGER);
      rc = BigInt(s);
    }
    this.module._free(ptr);

    return rc;
  }

  get_float(term)
  { const ptr = this.module._malloc(8);
    let rc;
    if (this.bindings.PL_get_float(term, ptr)) {
      rc = this.module.getValue(ptr, 'double');
    } else {
      rc = null;
    }
    this.module._free(ptr);
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
    this.module._free(c.ptr);
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
 * @param {String_t} goal  Prolog goal to be called
 * @param {String} [module] Module in which to call the goal.
 * @return Either the result of Query.next() or a _yield_ request as
 * described above.
 */

  __call_yieldable(goal, module)
  { var pred_call1;
    const flags = this.PL_Q_NORMAL|this.PL_Q_ALLOW_YIELD;

    if ( !pred_call1 )
      pred_call1 = this.predicate("call", 1, "system");

    const fid = this.bindings.PL_open_foreign_frame();
    const term = this.new_term_ref();
    if ( !this.chars_to_term(goal, term) )
      throw new Error('Query has a syntax error: ' + query);
    const q = this.query(module, flags, pred_call1, term, fid);
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
		const atoms = prolog.module._malloc(4*len);
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
		prolog.module._free(atoms);
		break;
	      }
	      default:
	      { let id = prolog.object_ids.get(data);

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
  { const ptr = this.module._malloc(4);
    let rc;
    flags  = flags||(this.CVT_ALL|this.CVT_WRITEQ);
    flags |= this.CVT_EXCEPTION|this.REP_UTF8;
    if (this.bindings.PL_get_chars(term, ptr, flags)) {
        rc = this.module.UTF8ToString(this.module.getValue(ptr, 'i32'));
    } else {
        rc = null;
    }
    this.module._free(ptr);

    return rc;
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
 * Open a new query.  Signatures:
 *
 *  1) module:{String|0},
 *     flags:{Integer},
 *     predicate:{String|predicate_t},
 *     argv:{term_t}
 *     [map]:{Function}
 *     [fid]:{fid_t}
 *  2) module:{String|0},
 *     flags:{Integer},
 *     predicate:{String|predicate_t},
 *     argv:{Array}
 *
 * @param {String} [module] Optional module name
 */

class Query {
  constructor(prolog, module, flags, pred, argv, map, fid)
  { module = module ? prolog.new_module(module) : 0;
    if ( typeof(pred) === "string" )
      pred = prolog.predicate(pred);
    flags |= prolog.PL_Q_EXT_STATUS;
    if ( !(flags & (prolog.PL_Q_CATCH_EXCEPTION|
		    prolog.PL_Q_PASS_EXCEPTION|
		    prolog.PL_Q_NORMAL)) )
      flags |= prolog.PL_Q_CATCH_EXCEPTION;

    this.flags  = flags;
    this.prolog = prolog;
    this.map    = map;
    this.qid    = prolog.bindings.PL_open_query(module, flags, pred, argv);
    this.open   = true;
    this.argv   = argv;
    this.frame  = fid;
    prolog.open_queries.push(this);
  }

  [Symbol.iterator]() { return this; }

  next()
  { const prolog = this.prolog;
    const argv   = this.argv;

    if ( !this.open )
      return { done: true };

    if ( this != prolog.open_queries.at(-1) )
      console.log("Attempt for Query.next() on not innermost query");

    switch(prolog.bindings.PL_next_solution(this.qid))
    { case prolog.PL_S_EXCEPTION:
      { if ( (this.flags & prolog.PL_Q_NORMAL) )
	{ this.close();
	  return { done: true, error: true }
	} else
	{ const msg = prolog.message_to_string(
				 prolog.bindings.PL_exception(this.qid));
	  console.log(msg);
	  this.close();
	  return { done: true, error: true, message: msg };
	}
      }
      case prolog.PL_S_FALSE:
        this.close();
	return { done: true };
      case prolog.PL_S_LAST:
	this.close();
	return { done: true,
	         value: this.map ? this.map.call(this, argv) : argv
	       };
      case prolog.PL_S_TRUE:
	return { done: false,
	         value: this.map ? this.map.call(this, argv) : argv
	       };
      case prolog.PL_S_YIELD:
      { let request = prolog.yield_request();

	return { done: false,
	         value: null,
	         yield: request,
		 resume: (value) =>
		 { prolog.set_yield_result(value);
		   return this.next();
		 }
	       };
      }
    }
  }

  next_yieldable()
  { function next(query)
    { const prolog = query.prolog;

      while(true)
      { let rc = query.next();

	if ( rc.yield !== undefined )
	{ let request = rc.yield;

	  if ( prolog.abort_request )
	  { prolog.abort_request = undefined;
	    prolog.set_yield_result("abort");
	    continue;
	  }

	  if ( request === "beat" )
	  { const now = Date.now();
	    const passed = now - prolog.lastyieldat;

	    if ( passed < 20 )
	    { prolog.set_yield_result("true");
	      continue;
	    }
	    prolog.lastyieldat = now;
	  } else if ( request instanceof Promise )
	  { let result = { yield: "builtin",
			   request: request,
			   query: query,
			   resume: (cont) =>
			   { if ( typeof(cont) === "string" )
			     { prolog.set_yield_result(cont);
			       return next(query);
			     } else
			     { result.cont = cont;
			       request
			       .then((value) =>
				 { prolog.set_yield_result(value);
				   cont.call(prolog, next(query));
				 })
			       .catch((error) =>
				 { prolog.set_yield_result({$error: error});
				   cont.call(prolog, next(query));
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

	  // Get back here instead of Query.next()
	  rc.resume = (value) =>
	  { prolog.set_yield_result(value);
	    return next(query);
	  };
	} else if ( rc.done === false )
	{ rc.resume = () => next(query);
	}

	return rc;
      }
    }

    return next(this);
  }

  /**
   * Run a query once.
   * @return To simplify distinguishing success from failure the
   * returned object has a field `success` if the Prolog query
   * completed without an error.
   */

  once()
  { const rc = this.next();
    this.close();
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

  close()
  { if ( this.open )
    { const prolog = this.prolog;

      if ( this != prolog.open_queries.at(-1) )
	console.log("Attempt for Query.close() on not innermost query");
      prolog.open_queries.pop();

      this.prolog.bindings.PL_cut_query(this.qid);
      if ( this.frame )
	this.prolog.bindings.PL_discard_foreign_frame(this.frame);
      this.open = false;
    }
  }
}


		 /*******************************
		 *   BIND PROLOG TO THE MODULE  *
		 *******************************/


Module.onRuntimeInitialized = function()
{ Module.prolog = new Prolog(Module, Module.arguments);
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

      const func = obj[fname];
      if ( typeof(func) === "function"  )
	return func.apply(obj, args);
      else
	console.log("ERROR: Function", fname, "is not defined on", obj);
    }

    for(let i=0; i<ar.length; i++)
    { const next = ar[i];

      if ( typeof(next) === "string" )
      { if ( i == 0 )
	{ switch(next)
	  { case "prolog":
	      obj = prolog;
	      break;
	    case "window":
	      obj = window;
	      break;
	    default:
	      obj = eval(next);
	  }
	} else
	{ obj = obj[next];
	}
      } else if ( next.v !== undefined )
      { obj = next.v;
      } else
      { const args = next.args.map((v) => eval_chain(v));

	obj = eval_one(obj, next.f, args);
      }
    }

    return obj;
  }

  try
  { return prolog.with_frame(() =>
    { const ar = prolog.toJSON(request, { string: "string" });
      let obj;

      if ( ar.setter )
      { const target = eval_chain(ar.target);
	const value  = eval_chain(ar.value);
	target[ar.setter] = value;
	obj = true;
      } else
      { obj = eval_chain(ar);
      }

      return prolog.unify(result, prolog.toProlog(obj));
    }, false);
  } catch (e)
  { return prolog.bindings.PL_raise_exception(
      prolog.toProlog(new prolog.Compound("error",
					  [ new prolog.Compound("js_error", [e.toString()]),
					    new prolog.Var()
					  ])));
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


if ( BigInt.prototype.toJSON === undefined )
{ BigInt.prototype.toJSON = function ()
  { return this.toString();
  }
}

if ( typeof HTMLCollection === "object" )
{ HTMLCollection.prototype.toList = function()
  { const ar = [];

    for(let i=0; i<this.length; i++)
      ar.push(this.item(i));

    return ar;
  }
}


