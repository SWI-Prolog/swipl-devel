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

import jpl.fli.BooleanHolder;
import jpl.fli.Prolog;
import jpl.fli.term_t;

//----------------------------------------------------------------------/
// JBoolean
/**
 * A jpl.JBoolean is a specialised Term with a boolean field, representing JPL's Prolog representation of a Java boolean value.
 * <pre>
 * JBoolean b = new JBoolean( true or false );
 * </pre>
 * A JBoolean can be used (and re-used) in Compound Terms.
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
public class JBoolean extends Term {

	//==================================================================/
	//  Attributes
	//==================================================================/

	/**
	 * the JBoolean's value (a boolean)
	 */
	protected final boolean value;

	//==================================================================/
	//  Constructors
	//==================================================================/

	/**
	 * Constructs a JBoolean with the supplied boolean value.
	 * 
	 * @param   b  this JBoolean's value (a boolean)
	 */
	public JBoolean(boolean b) {
		this.value = b;
	}

	//==================================================================/
	//  Methods (common)
	//==================================================================/

	/**
	 * Tests whether this JBoolean's functor has (String) 'name' and 'arity'
	 * 
	 * @return whether this JBoolean's functor has (String) 'name' and 'arity'
	 */
	public final boolean hasFunctor(String name, int arity) {
		return name.equals("@") && arity==1;
	}

	/**
	 * Returns a Prolog source text representation of this JBoolean
	 * 
	 * @return  a Prolog source text representation of this JBoolean
	 */
	public String toString() {
		return (value ? "@(true)" : "@(false)");
	}

	/**
	 * Two JBooleans are equal if their values are equal
	 * 
	 * @param   obj  The Object to compare (not necessarily a JBoolean)
	 * @return  true if the Object satisfies the above condition
	 */
	public final boolean equals(Object obj) {
		return this == obj || (obj instanceof JBoolean && value == ((JBoolean) obj).value);
	}

	//==================================================================/
	//  Methods (peculiar)
	//==================================================================/

	/**
	 * The boolean value which this jpl.JBoolean object represents
	 * 
	 * @return the boolean value which this jpl.JBoolean object represents
	 */
	public boolean boolValue() {
		return value;
	}

	public final int type() {
		return Prolog.JBOOLEAN;
	}
	
	public String typeName(){
		return "JBoolean";
	}
	
	//==================================================================/
	//  Methods (deprecated)
	//==================================================================/

	/**
	 * The (nonexistent) args of this JBoolean
	 * 
	 * @return the (nonexistent) args of this JBoolean
	 * @deprecated
	 */
	public Term[] args() {
		throw new JPLException("jpl.JBoolean.args() is undefined");
	}

	/**
	 * Returns a debug-friendly representation of this JBoolean
	 * 
	 * @return  a debug-friendly representation of this JBoolean
	 * @deprecated
	 */
	public String debugString() {
		return "(JBoolean " + toString() + ")";
	}

	//==================================================================/
	//  Converting JPL Terms to Prolog terms
	//==================================================================/

	/**
	 * To convert a JBoolean to a term, we unify the (freshly created, hence unbound)
	 * term_t with either @(false) or @(true) as appropriate.
	 * 
	 * @param   varnames_to_vars  A Map from variable names to Prolog variables.
	 * @param   term              A (newly created) term_t which is to be
	 *                            set to a Prolog @(false) or @(true) structure denoting the 
	 *                            .value of this JBoolean instance
	 */
	protected final void put(Map varnames_to_vars, term_t term) {
		Prolog.put_jboolean(term, value);
	}

	//==================================================================/
	//  Converting Prolog terms to JPL Terms
	//==================================================================/

	/**
	 * Converts a term_t to a JBoolean.  If the Prolog term is either
	 * @(false) or @(true), we just create a new JBoolean with a corresponding value.
	 * NB This conversion is only invoked if "JPL-aware" term import is specified.
	 *
	 * @param   vars_to_Vars  A Map from Prolog variable to JPL Variables.
	 * @param   term          The term (either @(false) or @(true)) to convert
	 * @return                A new JBoolean instance
	 */
	protected static Term getTerm(Map vars_to_Vars, term_t term) {
		BooleanHolder b = new BooleanHolder();

		Prolog.get_jboolean(term, b); // assume it succeeds...
		return new jpl.JBoolean(b.value);
	}

	//==================================================================/
	//  Computing Substitutions
	//==================================================================/

	/**
	 * Nothing needs to be done if the Term denotes an Atom, Integer, Float, JRef or JBoolean
	 * 
	 * @param   varnames_to_Terms  A Map from variable names to Terms.
	 * @param   vars_to_Vars       A Map from Prolog variables to JPL Variables.
	 */
	protected final void getSubst(Map varnames_to_Terms, Map vars_to_Vars) {
	}

}

//345678901234567890123456789012346578901234567890123456789012345678901234567890
