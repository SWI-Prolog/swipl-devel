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

import jpl.fli.Prolog;
import jpl.fli.StringHolder;
import jpl.fli.term_t;

//----------------------------------------------------------------------/
// Atom
/**
 * Atom is a specialised Compound with zero arguments, representing a Prolog atom with the same name.
 * An Atom is constructed with a String parameter (its name, unquoted), which cannot thereafter be changed.
 * <pre>Atom a = new Atom("hello");</pre>
 * An Atom can be used (and re-used) as an argument of Compound Terms.
 * Two Atom instances are equal (by equals()) iff they have equal names.
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
public class Atom extends Compound {

	//==================================================================/
	//  Attributes (none)
	//==================================================================/

	//==================================================================/
	//  Constructors
	//==================================================================/

	/**
	 * @param   name   the Atom's name (unquoted)
	 */
	public Atom(String name) {
		super(name);
	}

	//==================================================================/
	//  Methods (common)
	//==================================================================/

	// these are all inherited from Compound

	public final int type() {
		return Prolog.ATOM;
	}
	
	//==================================================================/
	//  Methods (deprecated)
	//==================================================================/

	/**
	 * Returns a debug-friendly String representation of an Atom.
	 * 
	 * @return  a debug-friendly String representation of an Atom
	 * @deprecated
	 */
	public String debugString() {
		return "(Atom " + toString() + ")";
	}

	//==================================================================/
	//  Converting JPL Terms to Prolog terms
	//==================================================================/

	// (this is done with the put() method inherited from Compound)

	//==================================================================/
	//  Converting Prolog terms to JPL Terms
	//==================================================================/

	/**
	 * Converts a Prolog term to a JPL Atom.  This is only called from Term.getTerm(),
	 * and we can assume the term_t refers to a Prolog atom,
	 * so we just create a new Atom object with the atom's name.
	 *
	 * @param   vars_to_Vars  A Map from Prolog variables to JPL Variables.
	 * @param   term          The Prolog term to be converted
	 * @return                A new Atom instance
	 */
	protected static Term getTerm(Map vars_to_Vars, term_t term) {
		StringHolder holder = new StringHolder();
		Prolog.get_atom_chars(term, holder); // ignore return val; assume success...

		return new Atom(holder.value);
	}

	/**
	 * Converts a term_t to an Atom, knowing that it refers to a SWI-Prolog string,
	 * so we just create a new Atom object initialised with the string's value.
	 * JPL users should avoid SWI-Prolog's non-ISO strings, but in some obscure
	 * circumstances they are returned unavoidably, so we have to handle them
	 * (and this is how).
	 *
	 * @param   vars_to_Vars  A Map from Prolog variables to JPL Variables.
	 * @param   term          The term_t to convert
	 * @return                A new Atom instance
	 */
	protected static Term getString(Map vars_to_Vars, term_t term) {
		StringHolder holder = new StringHolder();
		Prolog.get_string_chars(term, holder); // ignore return val; assume success...
		// System.err.println("Warning: Prolog returns a string: \"" + holder.value + "\"");
		return new Atom(holder.value);
	}

	//==================================================================/
	//  Computing substitutions
	//==================================================================/

	// (done with the inherited Compound.getSubst() method)

}

//345678901234567890123456789012346578901234567890123456789012345678901234567890
