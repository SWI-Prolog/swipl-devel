//tabstop=4
//*****************************************************************************/
// Project: jpl
//
// File:    $Id$
// Date:    $Date$
// Author:  Fred Dushin <fadushin@syr.edu>
//          
//
// Description:
//    
//
// -------------------------------------------------------------------------
// Copyright (c) 2004 Paul Singleton
// Copyright (c) 1998 Fred Dushin
//                    All rights reserved.
// 
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Library Public License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.
// 
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Library Public License for more details.
//*****************************************************************************/
package jpl;

import java.util.Map;

import jpl.fli.IntHolder;
import jpl.fli.Prolog;
import jpl.fli.term_t;

//----------------------------------------------------------------------/
// Term
/**
 * Term is the abstract base class for 
 * Compound, Atom, Variable, Integer and Float, which comprise a Java-oriented concrete syntax for Prolog.
 * You cannot create instances of Term directly; rather, you should create
 * instances of Term's concrete subclasses.
 * Alternatively, use textToTerm() to construct a Term from its conventional
 * Prolog source text representation.
 * 
 * <hr><i>
 * Copyright (C) 2004  Paul Singleton<p>
 * Copyright (C) 1998  Fred Dushin<p>
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.<p>
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Library Public License for more details.<p>
 * </i><hr>
 * @author  Fred Dushin <fadushin@syr.edu>
 * @version $Revision$
 */
public abstract class Term {
	//==================================================================/
	//  Attributes
	//==================================================================/

	//==================================================================/
	//  Constructors
	//==================================================================/

	/**
	 * This default constructor is provided in order for subclasses
	 * to be able to define their own default constructors.
	 */
	protected Term() {
	}

	//==================================================================/
	//  Methods (abstract, common)
	//==================================================================/

	/**
	 * returns the ano-th (1+) argument of a (Compound) Term
	 * throws a JPLException for any other subclass
	 * 
	 * @return the ano-th argument of a (Compound) Term
	 */
	public Term arg(int ano) {
		throw new JPLException("jpl." + this.typeName() + ".arg() is undefined");
	};

	/**
	 * Tests whether this Term's functor has (String) 'name' and 'arity'
	 * Returns false if called inappropriately
	 * 
	 * @return whether this Term's functor has (String) 'name' and 'arity'
	 */
	public boolean hasFunctor(String name, int arity) {
		return false;
	}

	/**
	 * Tests whether this Term's functor has (int) 'name' and 'arity'
	 * Returns false if called inappropriately
	 * 
	 * @return whether this Term's functor has (int) 'name' and 'arity'
	 */
	public boolean hasFunctor(int value, int arity) {
		return false;
	}

	/**
	 * Tests whether this Term's functor has (double) 'name' and 'arity'
	 * Returns false if called inappropriately
	 * 
	 * @return whether this Term's functor has (double) 'name' and 'arity'
	 */
	public boolean hasFunctor(double value, int arity) {
		return false;
	}

	/**
	 * returns, as a String, the name of a Compound, Atom or Variable
	 * throws a JPLException from an Integer or Float
	 * 
	 * @return the name of a Compound, Atom or Variable
	 */
	public String name() {
		throw new JPLException("jpl." + this.typeName() + ".name() is undefined");
	};

	/**
	 * returns, as an int, the arity of a Compound, Atom, Integer or Float
	 * throws a JPLException from a Variable
	 * 
	 * @return the arity of a Compound, Atom, Integer or Float
	 */
	public int arity() {
		throw new JPLException("jpl." + this.typeName() + ".arity() is undefined");
	};

	/**
	 * returns the value (as an int) of an Integer or Float
	 * throws a JPLException from a Compound, Atom or Variable
	 * 
	 * @return the value (as an int) of an Integer or Float
	 */
	public int intValue() {
		throw new JPLException("jpl." + this.typeName() + ".intValue() is undefined");
	}
	/**
	 * returns the value (as a long) of an Integer or Float
	 * throws a JPLException from a Compound, Atom or Variable
	 * 
	 * @return the value (as a long) of an Integer or Float
	 */
	public long longValue() {
		throw new JPLException("jpl." + this.typeName() + ".longValue() is undefined");
	}
	/**
	 * returns the value (as a float) of an Integer or Float
	 * throws a JPLException from a Compound, Atom or Variable
	 * 
	 * @return the value (as a float) of an Integer or Float
	 */
	public float floatValue() {
		throw new JPLException("jpl." + this.typeName() + ".floatValue() is undefined");
	}

	/**
	 * returns the value (as a double) of an Integer or Float
	 * throws a JPLException from any other subclass
	 * 
	 * @return the value (as an double) of an Integer or Float
	 */
	public double doubleValue() {
		throw new JPLException("jpl." + this.typeName() + ".doubleValue() is undefined");
	}

	//==================================================================/
	//  Methods (common)
	//==================================================================/

	/**
	 * returns the type of this term, as one of jpl.fli.Prolog.COMPOUND, .ATOM, .VARIABLE, .INTEGER, .FLOAT etc
	 * 
	 * @return the type of this term, as one of jpl.fli.Prolog.COMPOUND, .ATOM, .VARIABLE, .INTEGER, .FLOAT etc
	 */
	public abstract int type();

	/**
	 * returns the name of the type of this term, as one of "Compound", "Atom", "Variable", "Integer", "Float" etc
	 * 
	 * @return the name of the type of this term, as one of "Compound", "Atom", "Variable", "Integer", "Float" etc
	 */
	public abstract String typeName();

	/**
	 * whether this Term represents an atom
	 * 
	 * @return whether this Term represents an atom
	 */
	public boolean isAtom() {
		return this instanceof Atom;
	}

	/**
	 * whether this Term represents a compound term
	 * 
	 * @return whether this Term represents a compound atom
	 */
	public boolean isCompound() {
		return this instanceof Compound;
	}

	/**
	 * whether this Term represents an atom
	 * 
	 * @return whether this Term represents an atom
	 */
	public boolean isFloat() {
		return this instanceof Float;
	}

	/**
	 * whether this Term represents an atom
	 * 
	 * @return whether this Term represents an atom
	 */
	public boolean isInteger() {
		return this instanceof Integer;
	}

	/**
	 * whether this Term is a JBoolean
	 * 
	 * @return whether this Term is a JBoolean
	 */
	public boolean isJBoolean() {
		return this instanceof JBoolean;
	}

	/**
	 * whether this Term is a JRef
	 * 
	 * @return whether this Term is a JRef
	 */
	public boolean isJRef() {
		return this instanceof JRef;
	}

	/**
	 * whether this Term is a JVoid
	 * 
	 * @return whether this Term is a JVoid
	 */
	public boolean isJVoid() {
		return this instanceof JVoid;
	}

	/**
	 * whether this Term is a variable
	 * 
	 * @return whether this Term is a variable
	 */
	public boolean isVariable() {
		return this instanceof Variable;
	}

	public Term putParams(Term[] ps) {
		IntHolder next = new IntHolder();
		next.value = 0;
		Term t2 = this.putParams1(next, ps);
		if (next.value != ps.length) {
			throw new JPLException("Term.putParams: more actual params than formal");
		}
		return t2;
	}

	public Term putParams(Term plist) {
		Term[] ps = plist.toTermArray();
		return putParams(ps);
	}

	protected Term putParams1(IntHolder next, Term[] ps) {
		switch (this.type()) {
			case Prolog.COMPOUND :
				return new Compound(this.name(), putParams2(this.args(), next, ps));
			case Prolog.ATOM :
				if (this.name().equals("?")) {
					if (next.value >= ps.length) {
						throw new JPLException("Term.putParams: fewer actual params than formal params");
					}
					return ps[next.value++];
				} // else drop through to default
			default :
				return this;
		}
	}

	static protected Term[] putParams2(Term[] ts, IntHolder next, Term[] ps) {
		int n = ts.length;
		Term[] ts2 = new Term[n];
		for (int i = 0; i < n; i++) {
			ts2[i] = ts[i].putParams1(next, ps);
		}
		return ts2;
	}

	/**
	 * the length of this list, iff it is one, else an exception is thrown
	 * 
	 * @throws JPLException
	 * @return the length (as an int) of this list, iff it is one
	 */
	public int listLength() {
		int len = 0;

		if (this.hasFunctor(".", 2)) {
			return 1 + this.arg(2).listLength();
		} else if (this.hasFunctor("[]", 0)) {
			return 0;
		} else {
			throw new JPLException("Term.listLength: term is not a list");
		}
	}

	/** returns an array of terms which are the successive members of this list, if it is a list, else throws an exception
	 * 
	 * @throws JPLException
	 * @return an array of terms which are the successive members of this list, if it is a list
	 */
	public Term[] toTermArray() {
		try {
			int len = this.listLength();
			Term[] ts = new Term[len];
			Term t = this;

			for (int i = 0; i < len; i++) {
				ts[i] = t.arg(1);
				t = t.arg(2);
			}
			return ts;
		} catch (JPLException e) {
			throw new JPLException("Term.toTermArray: term is not a proper list");
		}
	}

	//==================================================================/
	//  Methods (deprecated)
	//==================================================================/

	/**
	 * returns, as a Term[], the arguments of a Compound
	 * returns an empty Term[] from an Atom, Integer or Float
	 * throws a JPLException from a Variable 
	 * 
	 * @return the arguments of a Compound as a Term[
	 * @deprecated
	 */
	public abstract Term[] args();

	/**
	 * Returns a debug-friendly representation of a Term
	 * 
	 * @return  a debug-friendly representation of a Term
	 * @deprecated
	 */
	public abstract String debugString();

	/**
	 * Returns a debug-friendly representation of a list of Terms
	 * 
	 * @return  a debug-friendly representation of a list of Terms
	 * @deprecated
	 */
	public static String debugString(Term arg[]) {
		String s = "[";

		for (int i = 0; i < arg.length; ++i) {
			s += arg[i].debugString();
			if (i != arg.length - 1) {
				s += ", ";
			}
		}
		return s + "]";
	}

	//==================================================================/
	//  Converting JPL Terms to Prolog terms
	// 
	// To convert a Term to a term_t, we need to traverse the Term
	// structure and build a corresponding Prolog term_t object.
	// There are some issues:
	// 
	// - Prolog term_ts rely on the *consecutive* nature of term_t
	//   references.  In particular, to build a compound structure
	//   in the Prolog FLI, one must *first* determine the arity of the
	//   compound, create a *sequence* of term_t references, and then
	//   put atoms, functors, etc. into those term references.  We
	//   do this in these methods by first determining the arity of the
	//   Compound, and then by "put"-ing a type into a term_t.
	//   The "put" method is implemented differently in each of Term's
	//   five subclasses.
	// 
	// - What if we are trying to make a term_t from a Term, but the
	//   Term has multiple instances of the same Variable?  We want
	//   to ensure that _one_ Prolog variable will be created, or else
	//   queries will give incorrect answers.  We thus pass a Hashtable
	//   (var_table) through these methods.  The table contains term_t 
	//   instances, keyed on Variable instances.
	//==================================================================/

	/**
	 * Cache the reference to the Prolog term_t here.
	 * 
	 * @param   varnames_to_vars  A Map from variable names to JPL Variables.
	 * @param   term              A (previously created) term_t which is to be
	 *                            put with a Prolog term-type appropriate to the Term type
	 *                            (e.g., Atom, Variable, Compound, etc.) on which the method is
	 *                            invoked.)
	 */
	protected abstract void put(Map varnames_to_vars, term_t term);

	/**
	 * This static method converts an array of Terms to a *consecutive* 
	 * sequence of term_t objects.  Note that the first term_t object
	 * returned is a term_t class (structure); the succeeding term_t
	 * objects are consecutive references obtained by incrementing the
	 * *value* field of the term_t.
	 * 
	 * @param   varnames_to_vars  Map from variable names to JPL Variables.
	 * @param   args              An array of jpl.Term references.
	 * @return                    consecutive term_t references (first of which is
	 *                            a structure)
	 */
	protected static term_t putTerms(Map varnames_to_vars, Term[] args) {

		// first create a sequence of term_ts.  The 0th term_t
		// will be a jpl.fli.term_t.  Successive Prolog term_t 
		// references will reside in the Prolog engine, and
		// can be obtained by term0.value+i.
		// 
		term_t term0 = Prolog.new_term_refs(args.length);

		// for each new term reference, construct a Prolog term
		// by putting an appropriate Prolog type into the reference.
		// 
		long ith_term_t = term0.value;
		for (int i = 0; i < args.length; ++i, ++ith_term_t) {
			term_t term = new term_t();
			term.value = ith_term_t;
			args[i].put(varnames_to_vars, term); // each subclass defines its own put()
		}

		return term0;
	}

	//==================================================================/
	//  Converting Prolog terms to JPL Terms
	// 
	// Converting back from term_ts to Terms is simple, since
	// the (simplified) Term representation is canonical (there is only one
	// correct structure for any given Prolog term).
	// 
	// One problem concerns variable bindings.  We illustrate
	// with several examples.  First, consider the Prolog fact
	// 
	//     p( f(X,X)).
	// 
	// And the query
	//
	//     ?- p( Y).
	//
	// A solution should be
	//
	//     y = f(X,X) 
	//
	// and indeed, if this query is run, the term_t to which Y will
	// be unified is a compound, f(X,X).  The problem is, how do
	// we know, in converting the term_ts to Terms in the compound f/2
	// whether we should create one Variable or two?  This begs the
	// question, how do we _identify_ Variables in JPL?  The answer
	// to the latter question is, by reference; two Variable (Java) 
	// references refer to the same variable iff they are, in memory,
	// the same Variable object.  That is, they satisfy the Java == relation.
	// (Note that this condition is _not_ true of the other Term types.)
	// 
	// Given this design decision, therefore, we should create a
	// single Variable instance and a Compound instance whose two arg
	// values refer to the same Variable object.  We therefore need to keep
	// track, in converting a term_t to a Term (in particular, in
	// converting a term_t whose type is variable to a Variable), of
	// which Variables have been created.  We do this by using the vars
	// Hashtable, which gets passed recursively though the from_term_t
	// methods; this table holds the Variable instances that have been
	// created, keyed by the unique and internal-to-Prolog string
	// representation of the variable (I'm not sure about this...).
	//==================================================================/

	/**
	 * This method calls from_term_t on each term in the n consecutive term_ts.
	 * A temporary jpl.term_t "holder" (byref) structure must be created
	 * in order to extract type information from the Prolog engine.
	 * 
	 * @param   vars_to_Vars      A Map from Prolog variables to jpl.Variable instances
	 * @param   n         The number of consecutive term_ts
	 * @param   term0     The 0th term_t (structure); subsequent
	 *                    term_ts are not structures.
	 * @return            An array of converted Terms
	 */
	/*
	protected static Term[] from_term_ts(Map vars_to_Vars, int n, term_t term0) {
	
		// create an (uninitialised) array of n Term references
		Term[] terms = new Term[n];
	
		// for each term_t (from 0...n-1), create a term_t
		// (temporary) structure and dispatch the translation
		// to a Term to the static from_term_t method of the Term
		// class.  This will perform (Prolog) type analysis on the
		// term_t and call the appropriate static method to create
		// a Term of the right type (e.g., Atom, Variable, List, etc.)
		// 
		long ith_term_t = term0.value;
		for (int i = 0; i < n; ++i, ++ith_term_t) {
			term_t term = new term_t();
			term.value = ith_term_t;
	
			terms[i] = Term.from_term_t(vars_to_Vars, term);
		}
	
		return terms;
	}
	*/

	/**
	 * We discover the Prolog type of the term, then forward the
	 * call to the appropriate subclass
	 * 
	 * @param   vars  A Map from Prolog variables to jpl.Variable instances
	 * @param   term  The Prolog term (in a term_t holder) to convert
	 * @return        The converted Term subtype instance.
	 */
	protected static Term getTerm(Map vars_to_Vars, term_t term) {
		int type = Prolog.term_type(term);

		switch (type) {
			case Prolog.VARIABLE :
				return Variable.getTerm(vars_to_Vars, term);
			case Prolog.ATOM :
				return Atom.getTerm(vars_to_Vars, term);
			case Prolog.STRING :
				return Atom.getString(vars_to_Vars, term);
			case Prolog.INTEGER :
				return Integer.getTerm(vars_to_Vars, term);
			case Prolog.FLOAT :
				return Float.getTerm(vars_to_Vars, term);
			case Prolog.COMPOUND :
				return Compound.getTerm(vars_to_Vars, term);
			default :
				// should never happen...
				throw new JPLException("Term.from_term_t: unknown term type=" + type);
		}
	}

	//==================================================================/
	//  Computing Substitutions
	// 
	// Once a solution has been found, the Prolog term_t references
	// will have been instantiated and will refer to new terms.  To compute
	// a substitution, we traverse the (original) Term structure, looking
	// at the term_t reference in the Term.  The only case we really care
	// about is if the (original) Term is a Variable; if so, the term_t
	// back in the Prolog engine may be instantiated (non Variable parts
	// of the original Term cannot change or become uninstantiated).  In
	// this case, we can store this term in a Hashtable, keyed by the
	// Variable with which the term was unified.
	//==================================================================/

	//------------------------------------------------------------------/
	// getSubst
	/**
	 * This method computes a substitution from a Term.  The bindings
	 * Hashtable stores Terms, keyed by Variables.  Thus, a
	 * substitution is as it is in mathematical logic, a sequence
	 * of the form \sigma = {t_0/x_0, ..., t_n/x_n}.  Once the
	 * substitution is computed, the substitution should satisfy
	 * 
	 *   \sigma T = t
	 * 
	 * where T is the Term from which the substitution is computed,
	 * and t is the term_t which results from the Prolog query.<p>
	 * 
	 * A second Hashtable, vars, is required; this table holds
	 * the Variables that occur (thus far) in the unified term.
	 * The Variable instances in this table are guaranteed to be
	 * unique and are keyed on Strings which are Prolog internal
	 * representations of the variables.
	 * 
	 * @param   bindings  table holding Term substitutions, keyed on
	 * Variables.
	 * @param   vars  A Hashtable holding the Variables that occur
	 * thus far in the term; keyed by internal (Prolog) string rep.
	 */
	protected abstract void getSubst(Map varnames_to_Terms, Map vars_to_Vars);

	//------------------------------------------------------------------/
	// getSubsts
	/**
	 * Just calls computeSubstitution for each Term in the array.
	 * 
	 * @param   varnames_to_Terms  a Map from variable names to Terms
	 * @param   vars_to_Vars       a Map from Prolog variables to JPL Variables
	 * @param   arg                a list of Terms
	 */
	protected static void getSubsts(Map varnames_to_Terms, Map vars_to_Vars, Term[] args) {
		for (int i = 0; i < args.length; ++i) {
			args[i].getSubst(varnames_to_Terms, vars_to_Vars);
		}
	}

	//------------------------------------------------------------------/
	// terms_equals
	/**
	 * This method is used (by Compound.equals) to determine the Terms in two Term arrays
	 * are pairwise equal, where two Terms are equal if they satisfy
	 * the equals predicate (defined differently in each Term subclass).
	 * 
	 * @param   t1  an array of Terms
	 * @param   t2  another array of Terms
	 * @return  true if all of the Terms in the (same-length) arrays are pairwise equal
	 */
	protected static boolean terms_equals(Term[] t1, Term[] t2) {
		if (t1.length != t2.length) {
			return false;
		}

		for (int i = 0; i < t1.length; ++i) {
			if (!t1[i].equals(t2[i])) {
				return false;
			}
		}
		return true;
	}

	//------------------------------------------------------------------/
	// toString
	/**
	 * Converts a list of Terms to a String.
	 * 
	 * @param   args    An array of Terms to convert
	 * @return  String representation of a list of Terms
	 */
	public static String toString(Term[] args) {
		String s = "";

		for (int i = 0; i < args.length; ++i) {
			s += args[i].toString();
			if (i != args.length - 1) {
				s += ", ";
			}
		}

		return s;
	}

}

//345678901234567890123456789012346578901234567890123456789012345678901234567890
