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
package jpl.fli;



//----------------------------------------------------------------------/
// term_t
/**
 * A term_t is a simple class which mirrors the term_t type in
 * the Prolog FLI.  All it really does is hold a term reference,
 * which is an internal representation of a term in the Prolog
 * Engine.
 * 
 * <hr><i>
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
// Implementation notes:  
// 
//----------------------------------------------------------------------/
public class term_t 
extends LongHolder
{
	public static final long UNASSIGNED = -1L;
	
	public
	term_t()
	{
		value = UNASSIGNED;
	}
	
	//------------------------------------------------------------------/
	// toString
	/**
	 * This static method converts a term_t, which is assumed to contain
	 * a reference to a *consecutive* list of term_t references to a
	 * String representation of a list of terms, in this case, a comma
	 * separated list.
	 * 
	 * @param   n        the number of consecutive term_ts
	 * @param   term0    a term_t whose value is the 0th term_t.
	 */
	// Implementation notes:  
	// 
	//------------------------------------------------------------------/
	public static String
	toString( int n, term_t term0 )
	{
		String s = "";
		int  i;
		long ith_term_t;
		
		for ( i = 0, ith_term_t = term0.value;  i < n;  ++i, ++ith_term_t ){
			term_t term = new term_t();
			term.value = ith_term_t;
			s += term.toString();
			
			if ( i != n - 1 ){
				s += ", ";
			}
		}
		
		return s;
	}
	
	
	//------------------------------------------------------------------/
	// equals
	/**
	 * Instances of term_ts are stored in Term objects (see jpl.Term),
	 * and these term_ts are in some cases stored in Hashtables.
	 * Supplying this predicate provides the right behavior in Hashtable
	 * lookup (see the rules for Hashtable lookup in java.util).<p>
	 * 
	 * Note. Two term_ts are *not* equal if their values have not
	 * been assigned.  (Since Prolog FLI term_ts are unsigned values and
	 * the UNASSIGNED value is -1, this should work).
	 * 
	 * @param   obj  the Object to comapre.
	 * @return  true if the supplied object is a term_t instances
	 *          and the long values are the same
	 */
	// Implementation notes:  
	// 
	//------------------------------------------------------------------/
	public boolean
	equals( Object obj )
	{
		return 
			(obj instanceof term_t) &&
			this.value == ((term_t)obj).value &&
			this.value != UNASSIGNED;
	}
}

//345678901234567890123456789012346578901234567890123456789012345678901234567890
