function Prolog(module, args) {
    this.module = module;
    this.args = args;
    this.bindings = {};
    this._bind();
    this._initialise();
}

const PL_VARIABLE	      =	(1);
const PL_ATOM		      =	(2);
const PL_INTEGER	      =	(3);
const PL_RATIONAL	      =	(4);
const PL_FLOAT		      =	(5);
const PL_STRING		      =	(6);
const PL_TERM		      =	(7);
const PL_NIL		      =	(8);
const PL_BLOB		      =	(9);
const PL_LIST_PAIR	      =	(10);
const PL_FUNCTOR	      =	(11);
const PL_LIST		      =	(12);
const PL_CHARS		      =	(13);
const PL_POINTER	      =	(14);
const PL_CODE_LIST	      =	(15);
const PL_CHAR_LIST	      =	(16);
const PL_BOOL		      =	(17);
const PL_FUNCTOR_CHARS	      =	(18);
const _PL_PREDICATE_INDICATOR =	(19);
const PL_SHORT		      =	(20);
const PL_INT		      =	(21);
const PL_LONG		      =	(22);
const PL_DOUBLE		      =	(23);
const PL_NCHARS		      =	(24);
const PL_UTF8_CHARS	      =	(25);
const PL_UTF8_STRING	      =	(26);
const PL_INT64		      =	(27);
const PL_NUTF8_CHARS	      =	(28);
const PL_NUTF8_CODES	      =	(29);
const PL_NUTF8_STRING	      =	(30);
const PL_NWCHARS	      =	(31);
const PL_NWCODES	      =	(32);
const PL_NWSTRING	      =	(33);
const PL_MBCHARS	      =	(34);
const PL_MBCODES	      =	(35);
const PL_MBSTRING	      =	(36);
const PL_INTPTR		      =	(37);
const PL_CHAR		      =	(38);
const PL_CODE		      =	(39);
const PL_BYTE		      =	(40);
const PL_PARTIAL_LIST	      =	(41);
const PL_CYCLIC_TERM	      =	(42);
const PL_NOT_A_LIST	      =	(43);
const PL_DICT		      =	(44);

const REP_ISO_LATIN_1	      =	0x00000000;
const REP_UTF8		      =	0x00100000;
const REP_MB		      =	0x00200000;
const REP_FN		      =	REP_UTF8;

const CVT_ATOM		      =	0x00000001;
const CVT_STRING	      =	0x00000002;
const CVT_LIST		      =	0x00000004;
const CVT_INTEGER	      =	0x00000008;
const CVT_RATIONAL	      =	0x00000010;
const CVT_FLOAT		      =	0x00000020;
const CVT_VARIABLE	      =	0x00000040;
const CVT_NUMBER	      =	(CVT_INTEGER|CVT_RATIONAL|CVT_FLOAT);
const CVT_ATOMIC	      =	(CVT_NUMBER|CVT_ATOM|CVT_STRING);
const CVT_WRITE		      =	0x00000080;
const CVT_WRITE_CANONICAL     =	0x00000100;
const CVT_WRITEQ	      =	0x00000200;
const CVT_ALL		      =	(CVT_ATOMIC|CVT_LIST);
const CVT_MASK		      =	0x00000fff;

const CVT_EXCEPTION	      =	0x00001000;
const CVT_VARNOFAIL	      =	0x00002000;

const BUF_DISCARDABLE	      =	0x00000000;
const BUF_STACK		      =	0x00010000;
const BUF_MALLOC	      =	0x00020000;
const BUF_ALLOW_STACK	      =	0x00040000;

const PL_Q_NORMAL	      =	0x0002;
const PL_Q_NODEBUG	      =	0x0004;
const PL_Q_CATCH_EXCEPTION    =	0x0008;
const PL_Q_PASS_EXCEPTION     =	0x0010;
const PL_Q_ALLOW_YIELD	      =	0x0020;
const PL_Q_EXT_STATUS	      =	0x0040;

const PL_S_EXCEPTION	      =	-1;
const PL_S_FALSE	      =	0;
const PL_S_TRUE		      =	1;
const PL_S_LAST		      =	2;
const PL_S_YIELD	      =	255;

const PL_WRT_QUOTED	      = 0x0001;
const PL_WRT_NEWLINE	      = 0x2000;


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
    this.bindings.PL_put_chars = this.module.cwrap(
        'PL_put_chars', 'number', ['number', 'number', 'number', 'number']);
    this.bindings.PL_unify = this.module.cwrap(
        'PL_unify', 'number', ['number', 'number']);
    this.bindings.PL_is_string = this.module.cwrap(
        'PL_is_string', 'number', ['number']);
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
    this.bindings.PL_put_functor = this.module.cwrap(
        'PL_put_functor', 'number', ['number', 'number']);
    this.bindings.PL_put_term_from_chars = this.module.cwrap(
        'PL_put_term_from_chars', 'number',['number','number','number','string']);
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
    this.bindings.PL_predicate = this.module.cwrap(
	'PL_predicate', 'number', ['number', 'number', 'number']);
    this.bindings.PL_open_query = this.module.cwrap(
	'PL_open_query', 'number', ['number', 'number', 'number', 'number']);
    this.bindings.PL_next_solution = this.module.cwrap(
	'PL_next_solution', 'number', ['number']);
    this.bindings.PL_close_query = this.module.cwrap(
	'PL_close_query', 'number', ['number']);
    this.bindings.WASM_ttymode = this.module.cwrap(
        'WASM_ttymode', 'number', []);
    this.bindings.WASM_yield_request = this.module.cwrap(
        'WASM_yield_request', 'number', []);
    this.bindings.WASM_set_yield_result = this.module.cwrap(
        'WASM_set_yield_result', 'number', ['number']);
};

// See http://www.swi-prolog.org/pldoc/doc_for?object=c(%27PL_initialise%27)
Prolog.prototype._initialise = function() {
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
 * @param `f` function to be called
 */

Prolog.prototype.with_frame = function(f) {
  const fid = this.bindings.PL_open_foreign_frame();
  if ( fid )
  { const rc = f.call(this);
    this.bindings.PL_close_foreign_frame(fid);
    return rc;
  }
  return false;				/* Throw? */
}

Prolog.prototype.string_to_c = function(string)
{ const len = this.module.lengthBytesUTF8(string);
  const ptr = this.module._malloc(len+1);
  return { ptr:    this.module.stringToUTF8(string, ptr, len),
           length: len
         }
}

/**
 * Get a reference to a predicate
 * @param name   {String}  Name of the predicate
 * @param arity  {Integer} Arity of the predicate
 * @param module {String}  Module to resolve the predicate
 */

Prolog.prototype.predicate = function(name, arity, module)
{ const c_name   = allocateUTF8(name);
  const c_module = allocateUTF8(module);

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
 * Open a new query
 *
 * @param {String} [module] Optional module name
 */

Prolog.prototype.open_query = function(module, flags, pred, argv)
{ const c_module = module ? this.new_module(module) : 0;

  return this.bindings.PL_open_query(c_module, flags, pred, argv);
}

Prolog.prototype.close_query = function(query)
{ return this.bindings.PL_close_query(query);
}

Prolog.prototype.next_solution = function(query)
{ return this.bindings.PL_next_solution(query);
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
  const flags	   = opts.flags == undefined ? PL_WRT_QUOTED|PL_WRT_NEWLINE
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
Prolog.prototype.get_functor = function(term) {
    var ptr = this.module._malloc(4);
    if (this.bindings.PL_get_functor(term, ptr)) {
        var functor = this.module.getValue(ptr, 'i32');
        this.module._free(ptr);
        return functor;
    } else {
        this.module._free(ptr);
        return null;
    }
};

// Returns integer number for the given term.
// Returns null when the term is not an integer.
Prolog.prototype.get_integer = function(term) {
    var ptr = this.module._malloc(4);
    if (this.bindings.PL_get_integer(term, ptr)) {
        var number = this.module.getValue(ptr, 'i32');
        this.module._free(ptr);
        return number;
    } else {
        this.module._free(ptr);
        return null;
    }
};

// Implements PL_put_chars for string case.
Prolog.prototype.put_chars_string = function(term, string) {
    var c = this.string_to_c(string);
    var ret = !!this.bindings.PL_put_chars(term, PL_STRING|REP_UTF8, c.length, c.ptr);
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

// Return a C-string for the text represented by the given atom.
// FIXME: Return from atom is ISO Latin 1.
Prolog.prototype.atom_chars = function(atom) {
    var ptr = this.bindings.PL_atom_chars(atom);
    if (ptr === 0) {
        return null;
    } else {
        return this.module.UTF8ToString(ptr);
    }
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
 * Call a predicate that may yield.
 *
 * @param {term_t} [term]   Prolog goal to be called
 * @param {String} [module] Module in which to call the goal.
 * @return one of `false` (call failed), `true` (call succeeded),
 * `undefined` (raised an exception (TBD)) or an object if the
 * predicated yielded control back.
 */

Prolog.prototype.call_yieldable = function(term, module) {
  var pred_call1;

  if ( !pred_call1 )
    pred_call1 = this.predicate("call", 1, "system");

  const q = this.open_query(module, PL_Q_NORMAL|PL_Q_ALLOW_YIELD|PL_Q_EXT_STATUS,
			    pred_call1, term);

  function next(prolog)
  { const rc = prolog.next_solution(q);

    if ( rc == PL_S_YIELD )
    { return { yield: prolog.yield_request(),
	       query: q,
	       resume: (value) =>
	       { prolog.set_yield_result(value);
		 return next(prolog);
	       }
	     };
    } else
    { prolog.close_query(q);
      return ( rc == PL_S_FALSE     ? false :
	       rc == PL_S_EXCEPTION ? undefined :
				      true
	     );
    }
  }

  return next(this);
};


/**
 * Parse string and put the resulting Prolog term into the term t.
 */

Prolog.prototype.chars_to_term = function(string, t) {
    return this.bindings.PL_put_term_from_chars(t, REP_UTF8, -1, string);
};

// Converts the argument term to a string.
Prolog.prototype.get_chars = function(term) {
    var ptr = this.module._malloc(4);
    var flags = CVT_ALL|CVT_WRITEQ|CVT_EXCEPTIONATOM;
    if (this.bindings.PL_get_chars(term, ptr, flags)) {
        // TODO properly free.
        return this.module.UTF8ToString(this.module.getValue(ptr, 'i32'));
    } else {
        return null;
    }
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
Prolog.prototype.new_term_ref = function() {
    return this.bindings.PL_new_term_ref();
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
