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

import java.util.Hashtable;
import java.util.Map;

//----------------------------------------------------------------------/
// Util
/**
 * This class provides a bunch of static utility methods for the JPL
 * High-Level Interface.
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
public final class Util {
	//------------------------------------------------------------------/
	// termArrayToList
	/**
	 * Converts an array of Terms to a JPL representation of a Prolog list of terms
	 * whose members correspond to the respective array elements.
	 * 
	 * @param   terms  An array of Term
	 * @return  Term   a list of the array elements
	 */
	public static Term termArrayToList(Term[] terms) {
		Term list = new Atom("[]");

		for (int i = terms.length - 1; i >= 0; --i) {
			list = new Compound(".", new Term[] { terms[i], list });
		}
		return list;
	}

	/**
	 * Converts a solution hashtable to an array of Terms.
	 * 
	 * @param   varnames_to_Terms  A Map from variable names to Terms
	 * @return  Term[]             An array of the Terms to which successive variables are bound
	 */
	public static Term[] bindingsToTermArray(Map varnames_to_Terms) {
		Term[] ts = new Term[varnames_to_Terms.size()];

		for (java.util.Iterator i = varnames_to_Terms.keySet().iterator(); i.hasNext();) {
			Variable k = (Variable) i.next();
			ts[k.index] = (Term) (varnames_to_Terms.get(k));
		}
		return ts;
	}

	//------------------------------------------------------------------/
	// toString
	/**
	 * Converts a substitution, in the form of a Map from variable names to Terms, to a String.
	 * 
	 * @param   varnames_to_Terms  A Map from variable names to Terms.
	 * @return  String             A String representation of the variable bindings
	 */
	public static String toString(Map varnames_to_Terms) {
		if (varnames_to_Terms == null) {
			return "[no solution]";
		}
		java.util.Iterator varnames = varnames_to_Terms.keySet().iterator();

		String s = "Bindings: ";
		while (varnames.hasNext()) {
			String varname = (String) varnames.next();
			s += varname + "=" + varnames_to_Terms.get(varname).toString() + "; ";
		}
		return s;
	}

	//------------------------------------------------------------------/
	// namevarsToMap
	/**
	 * Converts a (JPL) list of Name=Var pairs (as yielded by atom_to_term/3)
	 * to a Map from Prolog variables (necessarily in term_t holders) to named JPL Variables
	 * 
	 * @param   nvs  A JPL list of Name=Var pairs (as yielded by atom_to_term/3)
	 * @return  Map  A Map from Prolog variables (necessarily in term_t holders) to named JPL Variables
	 */
	public static Map namevarsToMap(Term nvs) {

		try {
			Map vars_to_Vars = new Hashtable();
			
			/*
			while (nvs.hasFunctor(".", 2) && ((Compound) nvs).arg(1).hasFunctor("=", 2)) {
				Atom name = (Atom) ((Compound) ((Compound) nvs).arg(1)).arg(1); // get the Name of the =/2 pair
				Variable var = (Variable) ((Compound) ((Compound) nvs).arg(1)).arg(2); // get the Var of the =/2 pair

				vars_to_Vars.put(var.term_, new Variable(name.name())); // map the Prolog variable to a new, named Variable
				nvs = ((Compound) nvs).arg(2); // advance to next list cell
			}
			 */
			while (nvs.hasFunctor(".", 2) && nvs.arg(1).hasFunctor("=", 2)) {
				// the cast to Variable is necessary to access the (protected) .term_ field
				vars_to_Vars.put(((Variable)nvs.arg(1).arg(2)).term_, new Variable(nvs.arg(1).arg(1).name())); // map the Prolog variable to a new, named Variable
				nvs = nvs.arg(2); // advance to next list cell
			}

			// maybe oughta check that nvs is [] ?
			return vars_to_Vars;
		} catch (java.lang.ClassCastException e) { // nvs is not of the expected structure
			return null;
		}
	}

	//------------------------------------------------------------------/
	// textToTerm
	/**
	 * Converts a Prolog source text to a corresponding JPL Term
	 * (in which each Variable has the appropriate name from the source text).
	 * Throws PrologException containing error(syntax_error(_),_) if text is invalid.
	 * 
	 * @param   text  A Prolog source text denoting a term
	 * @return  Term  a JPL Term equivalent to the given source text
	 */
	public static Term textToTerm(String text) {
		// it might be better to use PL_chars_to_term()
		Query q = new Query(new Compound("atom_to_term", new Term[] { new Atom(text), new Variable("Term"), new Variable("NVdict")}));
		q.open();
		Map s = q.getSubstWithNameVars();
		if (s != null) {
			q.close();
			return (Term) s.get("Term");
		} else {
			return null;
		}
	}

}
