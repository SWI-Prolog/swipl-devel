function Prolog(module, args) {
    this.module = module;
    this.args = args;
    this.bindings = {};
    this._bind();
    this._initialise();
}

// Creates bindings to the SWI foreign API.
Prolog.prototype._bind = function() {
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
    this.bindings.PL_new_functor = this.module.cwrap(
        'PL_new_functor', 'number', ['number', 'number']);
    this.bindings.PL_new_term_ref = this.module.cwrap(
        'PL_new_term_ref', 'number', []);
    this.bindings.PL_put_functor = this.module.cwrap(
        'PL_put_functor', 'number', ['number', 'number']);
    this.bindings.PL_chars_to_term = this.module.cwrap(
        'PL_chars_to_term', 'number', ['string', 'number']);
    this.bindings.PL_call = this.module.cwrap(
        'PL_call', 'number', ['number', 'number']);
    this.bindings.PL_unify_arg = this.module.cwrap(
        'PL_unify_arg', 'number', ['number', 'number', 'number']);
};

// See http://www.swi-prolog.org/pldoc/doc_for?object=c(%27PL_initialise%27)
Prolog.prototype._initialise = function() {
    var argv = this.args.map(function(arg) {
        return this.module.allocate(
            this.module.intArrayFromString(arg),
            'i8', this.module.ALLOC_NORMAL);
    }, this);
    var ptr = this.module._malloc(argv.length * 4);
    argv.forEach(function(arg, i) {
        this.module.setValue(ptr + i * 4, arg, '*');
    }, this);
    if (!this.bindings.PL_initialise(4, ptr)) {
        throw new Error('SWI-Prolog initialisation failed.');
    }
    this.call_string("assert(user:file_search_path(library, 'wasm-preload/library')).");
};

// Helper function to parse a JavaScript
// string into a Prolog term and call it as a query.
Prolog.prototype.call_string = function(query) {
    var ref = this.new_term_ref();
    if (!this.chars_to_term(query, ref)) {
        throw new Error('Query has a syntax error: ' + query);
    }
    return !!this.call(ref, 0);
};

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
    var len = this.module.lengthBytesUTF8(string) + 1;
    var ptr = this.module._malloc(len);
    this.module.stringToUTF8(string, ptr, len);
    var ret = !!this.bindings.PL_put_chars(term, 5 | 0x1000, len - 1, ptr);
    this.module._free(ptr);
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
Prolog.prototype.atom_chars = function(atom) {
    var ptr = this.bindings.PL_atom_chars(atom);
    if (ptr === 0) {
        return null;
    } else {
        return this.module.Pointer_stringify(ptr);
    }
};

// Call term t just like the Prolog predicate once/1.
Prolog.prototype.call = function(term, module) {
    return this.bindings.PL_call(term, module);
};

// Parse the string chars and put the resulting
// Prolog term into the term t.
Prolog.prototype.chars_to_term = function(query, t) {
    return this.bindings.PL_chars_to_term(query, t);
};

// Converts the argument term to a string.
Prolog.prototype.get_chars = function(term) {
    var ptr = this.module._malloc(4);
    var flags = 0x0001 | 0x0002 | 0x0004 | 0x0008 | 0x0010 | 0x0020 | 0x0080 | 0x1000 | 0x0200;
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
