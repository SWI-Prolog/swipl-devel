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

import jpl.fli.DoubleHolder;
import jpl.fli.Prolog;
import jpl.fli.term_t;

//----------------------------------------------------------------------/
// Float
/**
 * Float is a specialised Term with a double field, representing a Prolog 64-bit ISO/IEC floating point value.
 * Once constructed, a Float's value cannot be altered.
 * <pre>
 * Float f = new Float( 3.14159265 );
 * </pre>
 * A Float can be used (and re-used) in Compound Terms.
 * Two Float instances are equal (by .equals()) iff their (double) values are equal.
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
 * @see jpl.Term
 * @see jpl.Compound
 */
public class Float extends Term {
	
	//==================================================================/
	//  Attributes
	//==================================================================/

	/**
	 * the Float's immutable value
	 */
	protected final double value;

	//==================================================================/
	//  Constructors and Initialization
	//==================================================================/

	/**
	 * This constructor creates a Float with the supplied 
	 * (double) value.
	 * 
	 * @param   value  this Float's value
	 */
	public Float(double value) {
		this.value = value;
	}

	//==================================================================/
	//  Methods (common)
	//==================================================================/

	/**
	 * throws a JPLException (arg(int) is defined only for Compound and Atom)
	 * 
	 * @return the ith argument (counting from 1) of this Float (never)
	 */
	public final Term arg(int i) {
		throw new JPLException("jpl.Float.arg(int) is undefined");
	}

	/**
	 * Tests whether this Float's functor has (String) 'name' and 'arity' (never)
	 * 
	 * @return whether this Float's functor has (String) 'name' and 'arity' (never)
	 */
	public final boolean hasFunctor(String name, int arity) {
		return false;
	}

	/**
	 * Tests whether this Float's functor has (int) 'name' and 'arity' (never)
	 * 
	 * @return whether this Float's functor has (int) 'name' and 'arity' (never)
	 */
	public final boolean hasFunctor(int val, int arity) {
		return false;
	}

	/**
	 * Tests whether this Float's functor has (double) 'name' and 'arity'
	 * 
	 * @return whether this Float's functor has (double) 'name' and 'arity'
	 */
	public final boolean hasFunctor(double val, int arity) {
		return val == this.value && arity == 0;
	}

	/**
	 * throws a JPLException (name() is defined only for Compound, Atom and Variable)
	 * 
	 * @return the name of this Float (never)
	 */
	public final String name() {
		throw new JPLException("jpl.Float.name() is undefined");
	}

	/**
	 * Returns the arity (0) of this Float
	 * 
	 * @return the arity (0) of this Float
	 */
	public final int arity() {
		return 0;
	}

	/**
	 * returns the (double) value of this Float, converted to an int
	 * 
	 * @return the (double) value of this Float, converted to an int
	 */
	public final int intValue() {
		return (new Double(value)).intValue();
	}

	/**
	 * returns the (double) value of this Float, converted to a long
	 * 
	 * @return the (double) value of this Float, converted to a long
	 */
	public final long longValue() {
		return (new Double(value)).longValue();
	}

	/**
	 * returns the (double) value of this Float, converted to a float
	 * 
	 * @return the (double) value of this Float, converted to a float
	 */
	public final float floatValue() {
		return (new Double(value)).floatValue();
	}

	/**
	 * returns the (double) value of this Float
	 * 
	 * @return the (double) value of this Float
	 */
	public final double doubleValue() {
		return this.value;
	}

	/**
	 * Returns a Prolog source text representation of this Float
	 * 
	 * @return  a Prolog source text representation of this Float
	 */
	public String toString() {
		return "" + value + "";
	}

	/**
	 * Two Floats are equal if they are the same object, or their values are equal
	 * 
	 * @param   obj  The Object to compare
	 * @return  true if the Object satisfies the above condition
	 */
	public final boolean equals(Object obj) {
		return this == obj || (obj instanceof Float && value == ((Float) obj).value);
	}
	
	public final int type() {
		return Prolog.FLOAT;
	}
	
	public String typeName(){
		return "Float";
	}
	
	//==================================================================/
	//  Methods (deprecated)
	//==================================================================/

	/**
	 * The (nonexistent) args of this Float
	 * 
	 * @return the (nonexistent) args of this Float
	 * @deprecated
	 */
	public Term[] args() {
		return new Term[] {};
	}
	
	/**
	 * The immutable value of this jpl.Float object, as a Java double
	 * 
	 * @return the Float's value
	 * @deprecated
	 */
	public double value() {
		return value;
	}

	/**
	 * Returns a debug-friendly String representation of this Float
	 * 
	 * @return  a debug-friendly String representation of this Float
	 * @deprecated
	 */
	public String debugString() {
		return "(Float " + toString() + ")";
	}

	//==================================================================/
	//  Converting JPL Terms to Prolog terms
	//==================================================================/

	/**
	 * To convert a JPL Float to a Prolog term, we put its value field into the
	 * term_t as a float.
	 * 
	 * @param   varnames_to_vars  A Map from variable names to Prolog variables.
	 * @param   term              A (previously created) term_t which is to be
	 *                            set to a Prolog float corresponding to this Float's value
	 */
	protected final void put(Map varnames_to_vars, term_t term) {
		Prolog.put_float(term, value);
	}

	//==================================================================/
	//  Converting Prolog terms to JPL Terms
	//==================================================================/

	/**
	 * Converts a Prolog term (known to be a float) to a JPL Float.
	 *
	 * @param   vars_to_Vars     A Map from Prolog variables to JPL Variables
	 * @param   term             The Prolog term (a float) to convert
	 * @return                   A new Float instance
	 */
	protected static Term getTerm(Map vars_to_Vars, term_t term) {
		DoubleHolder double_holder = new DoubleHolder();

		Prolog.get_float(term, double_holder); // assume it succeeds...
		return new jpl.Float(double_holder.value);
	}

	//==================================================================/
	//  Computing Substitutions
	//==================================================================/

	/**
	 * Nothing needs to be done if the Term is an Atom, Integer or (as in this case) a Float
	 * 
	 * @param   varnames_to_Terms  A Map from variable names to JPL Terms
	 * @param   vars_to_Vars       A Map from Prolog variables to JPL Variables
	 */
	protected final void getSubst(Map varnames_to_Terms, Map vars_to_Vars) {
	}

}

//345678901234567890123456789012346578901234567890123456789012345678901234567890
