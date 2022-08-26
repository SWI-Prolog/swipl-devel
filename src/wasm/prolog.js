function Prolog(module, args)
{ this.module = module;
  this.args = args;
  this.bindings = {};
  this._bind();
  this.objects = {};
  this.next_object_id = 0;
  this._initialise();

  this.PL_VARIABLE	       = (1);
  this.PL_ATOM		       = (2);
  this.PL_INTEGER	       = (3);
  this.PL_RATIONAL	       = (4);
  this.PL_FLOAT		       = (5);
  this.PL_STRING	       = (6);
  this.PL_TERM		       = (7);
  this.PL_NIL		       = (8);
  this.PL_BLOB		       = (9);
  this.PL_LIST_PAIR	       = (10);
  this.PL_FUNCTOR	       = (11);
  this.PL_LIST		       = (12);
  this.PL_CHARS		       = (13);
  this.PL_POINTER	       = (14);
  this.PL_CODE_LIST	       = (15);
  this.PL_CHAR_LIST	       = (16);
  this.PL_BOOL		       = (17);
  this.PL_FUNCTOR_CHARS	       = (18);
  this._PL_PREDICATE_INDICATOR = (19);
  this.PL_SHORT		       = (20);
  this.PL_INT		       = (21);
  this.PL_LONG		       = (22);
  this.PL_DOUBLE	       = (23);
  this.PL_NCHARS	       = (24);
  this.PL_UTF8_CHARS	       = (25);
  this.PL_UTF8_STRING	       = (26);
  this.PL_INT64		       = (27);
  this.PL_NUTF8_CHARS	       = (28);
  this.PL_NUTF8_CODES	       = (29);
  this.PL_NUTF8_STRING	       = (30);
  this.PL_NWCHARS	       = (31);
  this.PL_NWCODES	       = (32);
  this.PL_NWSTRING	       = (33);
  this.PL_MBCHARS	       = (34);
  this.PL_MBCODES	       = (35);
  this.PL_MBSTRING	       = (36);
  this.PL_INTPTR	       = (37);
  this.PL_CHAR		       = (38);
  this.PL_CODE		       = (39);
  this.PL_BYTE		       = (40);
  this.PL_PARTIAL_LIST	       = (41);
  this.PL_CYCLIC_TERM	       = (42);
  this.PL_NOT_A_LIST	       = (43);
  this.PL_DICT		       = (44);

  this.REP_ISO_LATIN_1	       = 0x00000000;
  this.REP_UTF8		       = 0x00100000;
  this.REP_MB		       = 0x00200000;
  this.REP_FN		       = this.REP_UTF8;

  this.CVT_ATOM		       = 0x00000001;
  this.CVT_STRING	       = 0x00000002;
  this.CVT_LIST		       = 0x00000004;
  this.CVT_INTEGER	       = 0x00000008;
  this.CVT_RATIONAL	       = 0x00000010;
  this.CVT_FLOAT	       = 0x00000020;
  this.CVT_VARIABLE	       = 0x00000040;
  this.CVT_NUMBER	       = (this.CVT_INTEGER|this.CVT_RATIONAL|this.CVT_FLOAT);
  this.CVT_ATOMIC	       = (this.CVT_NUMBER|this.CVT_ATOM|this.CVT_STRING);
  this.CVT_WRITE	       = 0x00000080;
  this.CVT_WRITE_CANONICAL     = 0x00000100;
  this.CVT_WRITEQ	       = 0x00000200;
  this.CVT_ALL		       = (this.CVT_ATOMIC|this.CVT_LIST);
  this.CVT_MASK		       = 0x00000fff;

  this.CVT_EXCEPTION	       = 0x00001000;
  this.CVT_VARNOFAIL	       = 0x00002000;

  this.BUF_DISCARDABLE	       = 0x00000000;
  this.BUF_STACK	       = 0x00010000;
  this.BUF_MALLOC	       = 0x00020000;
  this.BUF_ALLOW_STACK	       = 0x00040000;

  this.PL_Q_NORMAL	       = 0x0002;
  this.PL_Q_NODEBUG	       = 0x0004;
  this.PL_Q_CATCH_EXCEPTION    = 0x0008;
  this.PL_Q_PASS_EXCEPTION     = 0x0010;
  this.PL_Q_ALLOW_YIELD	       = 0x0020;
  this.PL_Q_EXT_STATUS	       = 0x0040;

  this.PL_S_EXCEPTION	       = -1;
  this.PL_S_FALSE	       = 0;
  this.PL_S_TRUE	       = 1;
  this.PL_S_LAST	       = 2;
  this.PL_S_YIELD	       = 255;

  this.PL_WRT_QUOTED	       = 0x0001;
  this.PL_WRT_NEWLINE	       = 0x2000;
}


// Creates bindings to the SWI foreign API.
Prolog.prototype._bind = function() {
    this.bindings._PL_streams = this.module.cwrap(
        '_PL_streams', 'number', []);
    this.bindings.PL_atom_chars = this.module.cwrap(
        'PL_atom_chars', 'number', ['number']);
    this.bindings.PL_functor_arity = this.module.cwrap(
        'PL_functor_arity', 'number', ['number']);
    this.bindings.PL_functor_name = this.module.cwrap(
        'PL_functor_name', 'number', ['number']);
    this.bindings.PL_get_functor = this.module.cwrap(
        'PL_get_functor', 'number', ['number', 'number']);
    this.bindings.PL_get_chars = this.module.cwrap(
        'PL_get_chars', 'number', ['number', 'number', 'number']);
    this.bindings.PL_get_arg = this.module.cwrap(
        'PL_get_arg', 'number', ['number', 'number', 'number']);
    this.bindings.PL_get_integer = this.module.cwrap(
        'PL_get_integer', 'number', ['number', 'number']);
    this.bindings.PL_get_int64 = this.module.cwrap(
        'PL_get_int64', 'number', ['number', 'number']);
    this.bindings.PL_get_float = this.module.cwrap(
        'PL_get_float', 'number', ['number', 'number']);
    this.bindings.PL_put_chars = this.module.cwrap(
        'PL_put_chars', 'number', ['number', 'number', 'number', 'number']);
    this.bindings.PL_put_atom = this.module.cwrap(
        'PL_put_atom', 'number', ['number']);
    this.bindings.PL_put_variable = this.module.cwrap(
        'PL_put_variable', 'number', ['number']);
    this.bindings.PL_unify = this.module.cwrap(
        'PL_unify', 'number', ['number', 'number']);
    this.bindings.PL_is_string = this.module.cwrap(
        'PL_is_string', 'number', ['number']);
    this.bindings.PL_is_variable = this.module.cwrap(
        'PL_is_variable', 'number', ['number']);
    this.bindings.PL_term_type = this.module.cwrap(
        'PL_term_type', 'number', ['number']);
    this.bindings.PL_get_list = this.module.cwrap(
        'PL_get_list', 'number', ['number', 'number', 'number']);
    this.bindings.PL_get_nil = this.module.cwrap(
        'PL_get_nil', 'number', ['number']);
    this.bindings.PL_get_name_arity = this.module.cwrap(
        'PL_get_name_arity', 'number', ['number', 'number', 'number']);
    this.bindings.PL_initialise = this.module.cwrap(
        'PL_initialise', 'number', ['number', 'number']);
    this.bindings.PL_new_atom = this.module.cwrap(
        'PL_new_atom', 'number', ['string']);
    this.bindings.PL_register_atom = this.module.cwrap(
        'PL_register_atom', null, ['number']);
    this.bindings.PL_unregister_atom = this.module.cwrap(
        'PL_unregister_atom', null, ['number']);
    this.bindings.PL_new_module = this.module.cwrap(
        'PL_new_module', 'number', ['number']);
    this.bindings.PL_new_functor = this.module.cwrap(
        'PL_new_functor', 'number', ['number', 'number']);
    this.bindings.PL_new_term_ref = this.module.cwrap(
        'PL_new_term_ref', 'number', []);
    this.bindings.PL_new_term_refs = this.module.cwrap(
        'PL_new_term_refs', 'number', ['number']);
    this.bindings.PL_copy_term_ref = this.module.cwrap(
        'PL_copy_term_ref', 'number', ['number']);
    this.bindings.PL_reset_term_refs = this.module.cwrap(
        'PL_reset_term_refs', null, ['number']);
    this.bindings.PL_put_functor = this.module.cwrap(
        'PL_put_functor', 'number', ['number', 'number']);
    this.bindings.PL_put_integer = this.module.cwrap(
        'PL_put_integer', 'number', ['number', 'number']);
    this.bindings.PL_put_float = this.module.cwrap(
        'PL_put_float', 'number', ['number', 'number']);
    this.bindings.PL_put_nil = this.module.cwrap(
        'PL_put_nil', 'number', []);
    this.bindings.PL_cons_functor_v = this.module.cwrap(
        'PL_cons_functor_v', 'number', ['number', 'number', 'number']);
    this.bindings.PL_cons_list = this.module.cwrap(
        'PL_cons_list', 'number', ['number', 'number', 'number']);
    this.bindings.PL_put_dict = this.module.cwrap(
        'PL_put_dict', 'number', ['number','number','number','number','number']);
    this.bindings.PL_put_term_from_chars = this.module.cwrap(
        'PL_put_term_from_chars', 'number',['number','number','number','string']);
    this.bindings.PL_put_term = this.module.cwrap(
        'PL_put_term', 'number', ['number', 'number']);
    this.bindings.PL_write_term = this.module.cwrap(
        'PL_write_term', 'number', ['number', 'number', 'number', 'number']);
    this.bindings.PL_call = this.module.cwrap(
        'PL_call', 'number', ['number', 'number']);
    this.bindings.PL_unify_arg = this.module.cwrap(
        'PL_unify_arg', 'number', ['number', 'number', 'number']);
    this.bindings.PL_open_foreign_frame = this.module.cwrap(
	'PL_open_foreign_frame', 'number', []);
    this.bindings.PL_close_foreign_frame = this.module.cwrap(
	'PL_close_foreign_frame', 'number', ['number']);
    this.bindings.PL_discard_foreign_frame = this.module.cwrap(
	'PL_close_foreign_frame', 'number', ['number']);
    this.bindings.PL_predicate = this.module.cwrap(
	'PL_predicate', 'number', ['number', 'number', 'number']);
    this.bindings.PL_open_query = this.module.cwrap(
	'PL_open_query', 'number', ['number', 'number', 'number', 'number']);
    this.bindings.PL_next_solution = this.module.cwrap(
	'PL_next_solution', 'number', ['number']);
    this.bindings.PL_close_query = this.module.cwrap(
	'PL_close_query', 'number', ['number']);
    this.bindings.PL_cut_query = this.module.cwrap(
	'PL_cut_query', 'number', ['number']);
    this.bindings.PL_exception = this.module.cwrap(
	'PL_exception', 'number', ['number']);
    this.bindings.WASM_ttymode = this.module.cwrap(
        'WASM_ttymode', 'number', []);
    this.bindings.WASM_yield_request = this.module.cwrap(
        'WASM_yield_request', 'number', []);
    this.bindings.WASM_set_yield_result = this.module.cwrap(
        'WASM_set_yield_result', 'number', ['number']);
    this.bindings.WASM_variable_id = this.module.cwrap(
        'WASM_variable_id', 'number', ['number']);
    this.bindings.js_unify_obj = this.module.cwrap(
        'js_unify_obj', 'number', ['number', 'number']);
    this.bindings.js_get_obj = this.module.cwrap(
        'js_get_obj', 'number', ['number']);
};

// See http://www.swi-prolog.org/pldoc/doc_for?object=c(%27PL_initialise%27)
Prolog.prototype._initialise = function() {
    this.functor_arg_names_ = {};
    let argv0 = this.args || [];
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
    this.call("set_prolog_flag(color_term, false).");
};

/**
 * Call a Prolog goal.  This function deals with many variations to
 * call Prolog.
 *
 * @param {String}  [goal] Goal to run
 * @param {String}  [opts.module] Module in which to call Goal
 * @param {Boolean} [opts.async]  Call as yieldable
 */

Prolog.prototype.call = function(goal, opts)
{ opts = opts||{};

  if ( typeof(goal) === "string" )
  { return this.with_frame(function()
    { const term = this.new_term_ref();

      if ( !this.chars_to_term(goal, term) )
	throw new Error('Query has a syntax error: ' + query);

      if ( !opts.async )
      { const module = opts.module ? this.new_module(opts.module) : 0;
	return !!this.bindings.PL_call(term, module);
      } else
      { return this.call_yieldable(term, opts.module);
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

Prolog.prototype.with_frame = function(f, persist) {
  const fid = this.bindings.PL_open_foreign_frame();
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

Prolog.prototype.string_to_c = function(string)
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

Prolog.prototype.predicate = function(name, arity, module)
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

Prolog.prototype.new_module = function(name)
{ const c_atom = this.new_atom(name);

  const module = this.bindings.PL_new_module(c_atom);
  this.unregister_atom(c_atom);

  return module;
}

/**
 * Download one or more files concurrently and consult them.  Note that
 * the consult happens in arbitrary order.
 */

Prolog.prototype.consult = function(...args)
{ const prolog = this;

  function consult_one(url)
  { let file = "/tmp/"+url.replace(/\//, "+");

    return fetch(url)
	.then((response) => response.text())
	.then((text) =>
	      { console.log(`Downloaded ${url} to ${file}`);
		Module.FS.writeFile(file, text);
		prolog.call(`consult('${file}')`);
	      })
  }

  if ( args.length == 1 )
    return consult_one(args[0]);
  else
    return Promise.all(args.map((url) => consult_one(url)));
}


/**
 * Convert a Prolog message term into a string.  Notably used to
 * translate Prolog exceptions to meaningful messages in the JavaScript
 * side.
 *
 * @return {String} string representation of the message
 */

Prolog.prototype.message_to_string = function(term)
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
 * Open a new query.  Signatures:
 *
 *  1) module:{String|0},
 *     flags:{Integer},
 *     predicate:{String|predicate_t},
 *     argv:{term_t}
 *     [map]:{Function}
 *  2) module:{String|0},
 *     flags:{Integer},
 *     predicate:{String|predicate_t},
 *     argv:{Array}
 *
 * @param {String} [module] Optional module name
 */

class Query {
  constructor(prolog, module, flags, pred, argv, map)
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
  }

  [Symbol.iterator]() { return this; }

  next()
  { if ( !this.open )
      return { done: true };

    const prolog = this.prolog;
    const argv   = this.argv;

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
        /*FALLTHROUGH*/
      case prolog.PL_S_TRUE:
	return { done: false,
	         value: this.map ? this.map.call(this, argv) : argv
	       };
      case prolog.PL_S_YIELD:
      { let request = prolog.yield_request();

	if ( request.charAt(0) == '{' )
	  request = JSON.parse(request);

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

  once()
  { const rc = this.next();
    this.close();
    return rc.value ? rc.value : rc;
  }

  close()
  { if ( this.open )
    { this.prolog.bindings.PL_cut_query(this.qid);
      if ( this.frame )
	this.prolog.bindings.PL_discard_foreign_frame(this.frame);
      this.open = false;
    }
  }
}

Prolog.prototype.query = function(module, flags, pred, argv, map)
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
    return new Query(this, 0, this.PL_Q_NORMAL, "wasm_call_string/3", av,
		     (a) => this.toJSON(a+2));
  }
}

/**
 * @return {IOSTREAM*} as a number
 */

Prolog.prototype.stream = function(name)
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


Prolog.prototype.write = function(term, opts)
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
Prolog.prototype.functor_arity = function(functor) {
    return this.bindings.PL_functor_arity(functor);
};

// Return an atom representing the name of the given functor.
Prolog.prototype.functor_name = function(functor) {
    return this.bindings.PL_functor_name(functor);
};

// Returns functor of the given term.
// Returns null when the term is not a compound.
Prolog.prototype.get_functor = function(term)
{ const ptr = this.module._malloc(4);
  let result;

  if ( this.bindings.PL_get_functor(term, ptr) )
    result = this.module.getValue(ptr, 'i32');
  else
    result = null;

  this.module._free(ptr);
  return result;
};

// Returns integer number for the given term.
// Returns null when the term is not an integer.
Prolog.prototype.get_integer = function(term) {
    const ptr = this.module._malloc(8);
    let rc;
    if (this.bindings.PL_get_int64(term, ptr)) {
        rc = this.module.getValue(ptr, 'i64');
    } else {
        rc = null;
    }
    this.module._free(ptr);
    return rc;
};

Prolog.prototype.get_float = function(term) {
    const ptr = this.module._malloc(8);
    let rc;
    if (this.bindings.PL_get_float(term, ptr)) {
        rc = this.module.getValue(ptr, 'double');
    } else {
        rc = null;
    }
    this.module._free(ptr);
    return rc;
};

/**
 * Make a JavaScript string available to Prolog.  By default the
 * Prolog is represented as a Prolog string.
 *
 * @return {Boolean}
 */

Prolog.prototype.put_chars = function(term, string, flags)
{ flags  = flags||this.PL_STRING;
  flags |= this.REP_UTF8;

  const c = this.string_to_c(string);
  const ret = !!this.bindings.PL_put_chars(term, flags, c.length, c.ptr);
  this.module._free(c.ptr);
  return ret;
};

// Unifies the terms. Returns false if the terms
// do not unify.
Prolog.prototype.unify = function(term1, term2) {
    return !!this.bindings.PL_unify(term1, term2);
};

// Returns whether the term is a string.
Prolog.prototype.is_string = function(term) {
    return !!this.bindings.PL_is_string(term);
};

Prolog.prototype.is_variable = function(term) {
    return !!this.bindings.PL_is_variable(term);
};

// Return a C-string for the text represented by the given atom.
Prolog.prototype.atom_chars = function(atom)
{ const t = this.new_term_ref();

  this.bindings.PL_put_atom(t, atom);
  const str = this.get_chars(t, this.CVT_ATOM);
  this.bindings.PL_reset_term_refs(t);

  return str;
};

// Get the TTY mode as one of "notty", "raw" or "cooked"
Prolog.prototype.ttymode = function() {
    return this.module.UTF8ToString(this.bindings.WASM_ttymode());
}

Prolog.prototype.yield_request = function()
{ return this.module.UTF8ToString(this.bindings.WASM_yield_request());
};

Prolog.prototype.set_yield_result = function(string)
{ const c_str = allocateUTF8(string);

  this.bindings.WASM_set_yield_result(c_str);
  this.module._free(c_str);
};

/**
 * Call a predicate that may yield.  Returns an object if the predicate
 * called js_yield/2.  Normally, the returned object contains a key
 * `yield` that either holds a string or a JSON object representing
 * the request.  The key `resume` is a function that should be called
 * to resume Prolog with a value that appears in the second argument
 * of js_call/2.
 *
 * The `yield` key can be `builtin`, in which case an object is returned
 * that contains a `resume` key that executes the built-in and continues
 * using the passed function. The returned object may provide an `abort`
 * key to abort the query immediately.
 *
 * @param {term_t} [term]   Prolog goal to be called
 * @param {String} [module] Module in which to call the goal.
 * @return one of `false` (call failed), `true` (call succeeded),
 * `undefined` (raised an exception (TBD)) or an object if the
 * predicated yielded control back.
 */

let lastyieldat = 0;

Prolog.prototype.call_yieldable = function(term, module) {
  var pred_call1;
  const flags = this.PL_Q_NORMAL|this.PL_Q_ALLOW_YIELD;

  if ( !pred_call1 )
    pred_call1 = this.predicate("call", 1, "system");

  const q = this.query(module, flags, pred_call1, term);

  function next(prolog)
  { while(true)
    { let rc = q.next();

      if ( rc.yield !== undefined )
      { let request = rc.yield;

	if ( request == "beat" )
	{ const now = Date.now();

	  if ( now-lastyieldat < 20 )
	  { prolog.set_yield_result("true");
	    continue;
	  }
	  lastyieldat = now;
	} else
	{ if ( request.command == "sleep" )
	  { let result = { yield: "builtin",
			   request: request,
			   query: q,
			   resume: (cont) =>
			   { if ( typeof(cont) === "string" )
			     { prolog.set_yield_result(cont);
			       return next(prolog);
			     } else
			     { result.cont = cont;
			       result.timer = setTimeout(() => {
				 prolog.set_yield_result("true");
				 cont.call(prolog, next(prolog));
			       }, request.time*1000);
			     }
			   },
			   abort: () => {
			     if ( result.timer )
			     { clearTimeout(result.timer);
			       prolog.set_yield_result("wasm_abort");
			       result.cont.call(prolog, next(prolog));
			     }
			   }
			 };
	    return result;
	  }
	}
      }

      // Get back here instead of Query.next()
      rc.resume = (value) =>
      { prolog.set_yield_result(value);
	return next(prolog);
      };

      return rc
    }
  }

  return next(this);
};


		 /*******************************
		 *	     CONVERSION		*
		 *******************************/

/**
 * Define arguments name for a functor.  For example
 *
 *     Prolog.functor_arg_names("point", ["x", "y"]);
 */

Prolog.prototype.set_arg_names = function(name, args)
{ if ( !this.functor_arg_names_[name] )
    this.functor_arg_names_[name] = {};
  this.functor_arg_names_[name][args.length] = args;
}

Prolog.prototype.arg_names = function(name, arity)
{ if ( this.functor_arg_names_[name] )
    return this.functor_arg_names_[name][arity];
}


/**
 * Convert a Prolog term into a JavaScript object.  This follows
 * https://github.com/SWI-Prolog/packages-mqi/issues/4
 */

function toJSON(prolog, term, options)
{ switch ( prolog.bindings.PL_term_type(term) )
  { case prolog.PL_VARIABLE:
      return { $v: "v",
               v: prolog.bindings.WASM_variable_id(term)
	    // term: prolog.bindings.PL_copy_term_ref(term)
             };
    case prolog.PL_STRING:
      if ( options.string !== "string" )
	return {$t: "s", v: prolog.get_chars(term)};
      /*FALLTHROUGH*/
    case prolog.PL_ATOM:
      return prolog.get_chars(term);
    case prolog.PL_NIL:
      return [];
    case prolog.PL_BLOB:
    { const id = prolog.bindings.js_get_obj(term);

      if ( id != -1 )
	return prolog.objects[id];

      return {"$t": "b"};
    }
    case prolog.PL_INTEGER:
      return prolog.get_integer(term);
    case prolog.PL_RATIONAL:
    { let s = prolog.get_chars(term, this.CVT_RATIONAL);
      let a = s.split("r");


      return {"$t": "r", v: {n: parseInt(a[0]), d: parseInt(a[1])}, s:s};
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

	result[name] = args;
	return result;
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

      return { "$t": "partial", v:result, t:toJSON(prolog, t, options) };
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

Prolog.prototype.toJSON = function(term, options)
{ options = options||{};
  return toJSON(this, term, options);
}

function toProlog(prolog, data, term, ctx)
{ term = term||prolog.new_term_ref();
  let rc;

  switch(typeof(data))
  { case "number":
      if ( Number.isInteger(data) )
	rc = prolog.bindings.PL_put_integer(term, data);
      else
	rc = prolog.bindings.PL_put_float(term, data);
      break;
    case "string":
      rc = prolog.put_chars(term, data, prolog.PL_ATOM);
      break;
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
      { let h = prolog.new_term_ref();

	prolog.bindings.PL_put_nil(term);
	for(var i=data.length-1; i >= 0; i--)
	{ toProlog(prolog, data[i], h, ctx);
	  prolog.bindings.PL_cons_list(term, h, term); /* TBD: error handling */
	}
      } else if ( data.$t )
      { switch( data.$t )
	{ case "s":
	    rc = prolog.put_chars(term, data.v, prolog.PL_STRING);
	    break;
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
	      const f  = prolog.new_functor(prolog.new_atom(name), args.length);

	      for(var i=0; i<args.length; i++)
		toProlog(prolog, args[i], av+i, ctx);

	      rc = prolog.bindings.PL_cons_functor_v(term, f, av);
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
	}
      } else if ( data.nodeType !== undefined )	       /* DOM object */
      { let id = data.prologId;

	if ( id === undefined )
	{ id = prolog.next_object_id+1;
	  rc = prolog.bindings.js_unify_obj(term, id);
	  if ( rc )
	  { data.prologId = id;
	    prolog.objects[id] = data;
	    prolog.next_object_id = id;
	  }
	} else
	{ rc = prolog.bindings.js_unify_obj(term, id);
	}
      } else
      { const keys  = Object.keys(data);
	const len   = keys.length;
	const av    = prolog.new_term_ref(len);
	const atoms = prolog.module._malloc(4*len);
	const tag   = prolog.new_atom("js");

	for(var i=0; i<len; i++)
	{ toProlog(prolog, data[keys[i]], av+i, ctx);
	  prolog.module.setValue(atoms+4*i, prolog.new_atom(keys[i]), 'i32');
	}

	rc = prolog.bindings.PL_put_dict(term, tag, len, atoms, av);
	prolog.module._free(atoms);
      }
      break;
    default:
      return null;
  }

  return rc ? term : null;
}

Prolog.prototype.toProlog = function(data, term, ctx)
{ ctx = ctx||{};

  return toProlog(this, data, term, ctx);
}

/**
 * Parse string and put the resulting Prolog term into the term t.
 */

Prolog.prototype.chars_to_term = function(string, t) {
    return this.bindings.PL_put_term_from_chars(t, this.REP_UTF8, -1, string);
};

// Converts the argument term to a string.
Prolog.prototype.get_chars = function(term, flags) {
    const ptr = this.module._malloc(4);
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
};

// If t is compound and index is between 1 and arity (inclusive),
// assign a with a term reference to the argument.
Prolog.prototype.get_arg = function(index, term, arg) {
    return this.bindings.PL_get_arg(index, term, arg);
};

// Return an atom handle for the given C-string.
Prolog.prototype.new_atom = function(string) {
    return this.bindings.PL_new_atom(string);
};

/**
 * Register an atom
 */
Prolog.prototype.register_atom = function(atom) {
    this.bindings.PL_register_atom(atom);
    return atom;
};

Prolog.prototype.unregister_atom = function(atom) {
    this.bindings.PL_unregister_atom(atom);
};

// Returns a functor identifier, a handle for the name/arity pair.
Prolog.prototype.new_functor = function(atom, arity) {
    return this.bindings.PL_new_functor(atom, arity);
};

// Return a fresh reference to a term.
Prolog.prototype.new_term_ref = function(count)
{ return count === undefined ? this.bindings.PL_new_term_ref()
			     : this.bindings.PL_new_term_refs(count);
};

// Create a new compound term from functor and bind t to this term.
Prolog.prototype.put_functor = function(term, functor) {
    return this.bindings.PL_put_functor(term, functor);
};

// Unifies the index-th argument (1-based) of term with arg.
Prolog.prototype.unify_arg = function(index, term, arg) {
    return this.bindings.PL_unify_arg(index, term, arg);
};

Module.onRuntimeInitialized = function() {
    Module.prolog = new Prolog(Module, Module.arguments);
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
    { if ( args.length == 1 )
      { switch(fname)
	{ case "-": return -args[0];
	  case "!": return !args[0];
	}
      } else if ( args.length == 2 )
      { switch(fname)
	{ case "+": return args[0] + args[1];
	  case "-": return args[0] - args[1];
	  case "*": return args[0] * args[1];
	  case "/": return args[0] / args[1];
	  case "&": return args[0] & args[1];
	  case "|": return args[0] | args[1];
	}
      }

      const func = obj[fname];
      if ( func )
	return func.apply(obj, args);
      else
	console.log("ERROR: Function", fname, "is not defined on", obj);
    }

    for(let i=0; i<ar.length; i++)
    { const next = ar[i];

      if ( typeof(next) === "string" )
      { obj = obj[next];
      } else if ( next.v !== undefined )
      { obj = next.v;
      } else
      { const args = next.args.map((v) => eval_chain(v));

	obj = eval_one(obj, next.f, args);
      }
    }

    return obj;
  }

  return prolog.with_frame(() =>
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

    return prolog.bindings.PL_unify(result, prolog.toProlog(obj));
  }, false);
}


/**
 * Release an object held by Prolog
 */

function release_registered_object(id)
{ const prolog = Module.prolog;
  const obj = prolog.object[id];

//  console.log(`Releasing object ${id}`, obj);

  delete obj.prologId;
  delete prolog.object[id];
}

if (typeof window !== 'undefined')
{
  window.js_add_script = function(text, opts)
  { opts = opts||{};
    let node;

    if ( opts.id )
    { if ( (node = document.getElementById(opts.id)) )
      { node.textContent = text;
      } else
      { node = document.createElement("script");
        node.id = opts.id;
        node.textContent = text;
        document.body.appendChild(node);
      }
    } else
    { node = document.createElement("script");
      node.textContent = text;
      document.body.appendChild(node);
    }
  }
}
