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
import jpl.fli.StringHolder;
import jpl.fli.term_t;

//----------------------------------------------------------------------/
// Compound
/**
 * A Compound represents a structured term,
 * comprising a functor and arguments (Terms).
 * Atom is a subclass of Compound, whose instances have zero arguments.
 * Direct instances of Compound must have one or more arguments
 * (it is an error to attempt to construct a Compound with zero args;
 * a JPLException will be thrown).
 * For example, this Java expression yields
 * a representation of the term f(a):
 * <pre>
 * new Compound( "f", new Term[] { new Atom("a") } )
 * </pre>
 * Note the use of the "anonymous array" notation to denote the arguments
 * (an anonymous array of Term).
 * <br>
 * Alternatively, construct the Term from Prolog source syntax:
 * <pre>
 * Util.textToTerm("f(a)")
 * </pre>
 * The <i>arity</i> of a Compound is the quantity of its arguments.
 * Once constructed, neither the name, arity nor any argument of a Compound can be altered.
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
 * @see    jpl.Term 
 * @see    jpl.Atom 
 */
public class Compound extends Term {

	//==================================================================/
	//  Attributes
	//==================================================================/

	/**
	 * the name of this Compound
	 */
	protected final String name;

	/**
	 * the arguments of this Compound
	 */
	protected final Term[] args;

	//==================================================================/
	//  Constructors
	//==================================================================/

	/**
	 * Creates a Compound with name but no args (i.e. an Atom).
	 * 
	 * @param   name   the name of this Compound
	 * @param   args   the arguments of this Compound
	 */
	protected Compound(String name) {
		if (name == null) {
			throw new JPLException("jpl.Atom: cannot construct with null name");
		}
		this.name = name;
		this.args = new Term[] {
		};
	}

	/**
	 * Creates a Compound with name and args.
	 * 
	 * @param   name   the name of this Compound
	 * @param   args   the arguments of this Compound
	 */
	public Compound(String name, Term[] args) {
		if (name == null) {
			throw new JPLException("jpl.Compound: cannot construct with null name");
		}
		if (args == null) {
			throw new JPLException("jpl.Compound: cannot construct with null args");
		}
		if (args.length == 0) {
			throw new JPLException("jpl.Compound: cannot construct with zero args");
		}
		this.name = name;
		this.args = args;
	}

	//==================================================================/
	//  Methods (common)
	//==================================================================/

	/**
	 * Returns the ith argument (counting from 1) of this Compound;
	 * throws an ArrayIndexOutOfBoundsException if i is inappropriate.
	 * 
	 * @return the ith argument (counting from 1) of this Compound
	 */
	public final Term arg(int i) {
		return args[i - 1];
	}

	/**
	 * Tests whether this Compound's functor has (String) 'name' and 'arity'.
	 * 
	 * @return whether this Compound's functor has (String) 'name' and 'arity'
	 */
	public final boolean hasFunctor(String name, int arity) {
		return name.equals(name) && arity == args.length;
	}

	/**
	 * Returns the name (unquoted) of this Compound.
	 * 
	 * @return the name (unquoted) of this Compound
	 */
	public final String name() {
		return name;
	}

	/**
	 * Returns the arity (1+) of this Compound.
	 * 
	 * @return the arity (1+) of this Compound
	 */
	public final int arity() {
		return args.length;
	}

	/**
	 * Returns a prefix functional representation of a Compound of the form name(arg1,...),
	 * where each argument is represented according to its toString() method.
	 * <br>
	 * NB 'name' should be quoted iff necessary, and Term.toString(Term[]) is not
	 * really a Term method, more a utility...
	 * 
	 * @return  string representation of an Compound
	 */
	public String toString() {
		return quotedName() + (args.length > 0 ? "(" + Term.toString(args) + ")" : "");
	}

	/**
	 * Two Compounds are equal if they are identical (same object) or their names and arities are equal and their
	 * respective arguments are equal.
	 * 
	 * @param   obj  the Object to compare (not necessarily another Compound)
	 * @return  true if the Object satisfies the above condition
	 */
	public final boolean equals(Object obj) {
		return (this == obj || (obj instanceof Compound && name.equals(((Compound) obj).name) && Term.terms_equals(args, ((Compound) obj).args)));
	}

	public int type() {
		return Prolog.COMPOUND;
	}
	
	public String typeName(){
		return "Compound";
	}
	
	//==================================================================/
	//  Methods (protected)
	//==================================================================/

	/**
	 * Returns a quoted (iff necessary) form of the Atom's name, as understood by Prolog read/1
	 * 
	 * @return  a quoted form of the Atom's name, as understood by Prolog read/1
	 */
	protected String quotedName() {
		return ((Atom) (new Query(new Compound("sformat", new Term[] { new Variable("S"), new Atom("~q"), new Compound(".", new Term[] { new Atom(this.name), new Atom("[]")})
		})))
			.oneSolution()
			.get(
			"S")).name;
	}

	//==================================================================/
	//  Methods (deprecated)
	//==================================================================/

	/**
	 * Returns the arguments of this Compound.
	 * 
	 * @return the arguments of this Compound
	 * @deprecated
	 */
	public final Term[] args() {
		return args;
	}

	/**
	 * Returns the ith argument (counting from 0) of this Compound.
	 * 
	 * @return the ith argument (counting from 0) of this Compound
	 * @deprecated
	 */
	public final Term arg0(int i) {
		return args[i];
	}

	/**
	 * Returns a debug-friendly representation of a Compound.
	 * 
	 * @return  a debug-friendly representation of a Compound
	 * @deprecated
	 */
	public String debugString() {
		return "(Compound " + name + " " + Term.debugString(args) + ")";
	}

	//==================================================================/
	//  Converting JPL Terms to Prolog terms
	//==================================================================/

	/**
	 * To put a Compound in a term, we create a sequence of term_t
	 * references from the Term.terms_to_term_ts() method, and then
	 * use the Prolog.cons_functor_v() method to create a Prolog compound
	 * term.
	 * 
	 * @param   varnames_to_vars  A Map from variable names to Prolog variables
	 * @param   term  A (previously created) term_t which is to be
	 * set to a Prolog term corresponding to the Term subtype
	 * (Atom, Variable, Compound, etc.) on which the method is invoked.
	 */
	protected final void put(Map varnames_to_vars, term_t term) {

		Prolog.cons_functor_v(term, Prolog.new_functor(Prolog.new_atom(name), args.length), Term.putTerms(varnames_to_vars, args));
	}

	//==================================================================/
	//  Converting Prolog terms to JPL Terms
	//==================================================================/

	/**
	 * Converts the Prolog term in term_t (known to be a compound) to a JPL Compound.
	 * In this case, we create a list of Terms by calling Term.getTerm for each
	 * term_t reference we get from Prolog.get_arg
	 * (Not sure why we couldn't get a sequence from there, but...).<p>
	 * 
	 * @param   varnames_to_vars  A Map from variable names to Prolog variables
	 * @param   term              The Prolog term to convert
	 * @return                    A new Compound
	 */
	protected static Term getTerm(Map varnames_to_vars, term_t term) {

		// we need holders to get the term's name and arity back from the FLI:
		StringHolder name_holder = new StringHolder();
		IntHolder arity_holder = new IntHolder();
		Prolog.get_name_arity(term, name_holder, arity_holder); // assume it succeeds

		Term args[] = new Term[arity_holder.value];
		for (int i = 1; i <= arity_holder.value; i++) {
			term_t termi = Prolog.new_term_ref();
			Prolog.get_arg(i, term, termi);
			args[i - 1] = Term.getTerm(varnames_to_vars, termi);
		}
		return new Compound(name_holder.value, args);
	}

	//==================================================================/
	//  Computing Substitutions
	//==================================================================/

	/**
	 * Nothing needs to be done except to pass the buck to this Compound's args.
	 * 
	 * @param   varnames_to_Terms  A Map from variable names to JPL Terms
	 * @param   vars_to_Vars       A Map from Prolog variables to JPL Variables
	 */
	protected final void getSubst(Map varnames_to_Terms, Map vars_to_Vars) {
		Term.getSubsts(varnames_to_Terms, vars_to_Vars, args);
	}

}

//345678901234567890123456789012346578901234567890123456789012345678901234567890
