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

import jpl.fli.ObjectHolder;
import jpl.fli.Prolog;
import jpl.fli.term_t;

//----------------------------------------------------------------------/
// JRef
/**
 * JRef is a specialised Term with an Object field, representing JPL's Prolog references to Java objects (or to null).
 * <pre>
 * JRef r = new JRef( non_String_object_or_null );
 * </pre>
 * A JRef can be used (and re-used) in Compound Terms.
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
public class JRef extends Term {

	//==================================================================/
	//  Attributes
	//==================================================================/

	/**
	 * the JRef's value (a non-String Object or null)
	 */
	protected final Object ref;

	//==================================================================/
	//  Constructors
	//==================================================================/

	/**
	 * This constructor creates a JRef, initialized with the supplied 
	 * non-String object (or null).
	 * 
	 * @param   ref  this JRef's value (a non-String object, or null)
	 */
	public JRef(Object ref) {
		if (ref instanceof String) {
			throw new JPLException("a JRef cannot have a String value (String maps to atom)");
		} else {
			this.ref = ref;
		}
	}

	//==================================================================/
	//  Methods (common)
	//==================================================================/

	/**
	 * Returns a Prolog source text representation of this JRef
	 * 
	 * @return  a Prolog source text representation of this JRef
	 */
	public String toString() {
		return "" + ref + ""; // WRONG
	}

	/**
	 * Two JRefs are equal if their references are identical (?)
	 * 
	 * @param   obj  The Object to compare
	 * @return  true if the Object satisfies the above condition
	 */
	public final boolean equals(Object obj) {
		return this == obj || (obj instanceof JRef && ref == ((JRef) obj).ref);
	}
	
	public final int type() {
		return Prolog.JREF;
	}
	
	public String typeName(){
		return "JRef";
	}
	
	//==================================================================/
	//  Methods (peculiar)
	//==================================================================/

	/**
	 * The non-String object (or null) which this jpl.JRef represents
	 * 
	 * @return the non-String object (or null) which this jpl.JRef represents
	 */
	public Object ref() {
		return ref;
	}

	//==================================================================/
	//  Methods (deprecated)
	//==================================================================/

	/**
	 * The (nonexistent) args of this JRef
	 * 
	 * @return the (nonexistent) args of this JRef
	 * @deprecated
	 */
	public Term[] args() {
		return new Term[] {
		};
	}

	/**
	 * Returns a debug-friendly representation of this JRef
	 * 
	 * @return  a debug-friendly representation of this JRef
	 * @deprecated
	 */
	public String debugString() {
		return "(JRef " + toString() + ")";
	}

	//==================================================================/
	//  Converting JPL Terms to Prolog terms
	//==================================================================/

	/**
	 * To convert a JRef to a term, we put its Object field (.value) into the
	 * term_t as a JPL ref (i.e. @/1) structure.
	 * 
	 * @param   varnames_to_vars  A Map from variable names to Prolog variables.
	 * @param   term              A (newly created) term_t which is to be
	 *                            set to a Prolog 'ref' (i.e. @/1) structure denoting the 
	 *                            .value of this JRef instance
	 */
	protected final void put(Map varnames_to_vars, term_t term) {

		Prolog.put_jref(term, ref);
	}

	//==================================================================/
	//  Converting Prolog terms to JPL Terms
	//==================================================================/

	/**
	 * Converts a term_t to a JRef.  Assuming the Prolog term is a
	 * ref, we just create a new JRef using the term_t's value.
	 * NB This conversion is only invoked if "JPL-aware" term import is specified.
	 *
	 * @param   vars_to_Vars  A Map from Prolog variables to JPL Variables.
	 * @param   term          The term (a ref) to convert
	 * @return                A new JRef instance
	 */
	protected static Term getTerm(Map vars_to_Vars, term_t term) {
		ObjectHolder obj = new ObjectHolder();

		Prolog.get_jref(term, obj); // assume it succeeds...
		return new jpl.JRef(obj.value);
	}

	//==================================================================/
	//  Computing Substitutions
	//==================================================================/

	/**
	 * Nothing needs to be done if the Term is an Atom, Integer, Float or JRef
	 * 
	 * @param   varnames_to_Terms  A Map from variable names to Terms.
	 * @param   vars_to_Vars       A Map from Prolog variables to JPL Variables.
	 */
	protected final void getSubst(Map varnames_to_Terms, Map vars_to_Vars) {
	}

}

//345678901234567890123456789012346578901234567890123456789012345678901234567890
