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

import jpl.fli.Prolog;

//----------------------------------------------------------------------/
// JPL
/**
 * The jpl.JPL class contains methods which allow (i) inspection and alteration
 * of the "default" initialisation arguments (ii) explicit initialisation
 * (iii) discovery of whether the Prolog engine is already initialised,
 * and if so, with what arguments.
 * The Prolog engine must be initialized 
 * before any queries are made, but this will happen automatically
 * (upon the first call to a Prolog FLI routine) if it has not already
 * been done explicitly.
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
public class JPL {
	protected static final boolean DEBUG = false;
	
	protected static boolean modeDontTellMe = true;

	//------------------------------------------------------------------/
	// setDTMMode
	/**
	 * Sets the global "dont-tell-me" mode (default value: true).
	 * When 'true', bindings will *not* be returned for any variable (in a Query's goal)
	 * whose name begins with an underscore character (except for "anonymous" variables,
	 * i.e. those whose name comprises just the underscore character, whose bindings are never returned).
	 * When 'false', bindings are returned for *all* variables except anonymous ones;
	 * this mode may be useful when traditional top-level interpreter behaviour is wanted,
	 * e.g. in a Java-based Prolog IDE or debugger.<p>
	 * This method should be regarded as experimental, and may subsequently be deprecated
	 * in favour of some more general mechanism for setting options, perhaps per-Query and
	 * per-call as well as globally.
	 * 
	 * @param    dtm	new "dont-tell-me" mode value
	 */
	public static void setDTMMode( boolean dtm){
		modeDontTellMe = dtm;
	}
	
	//------------------------------------------------------------------/
	// getDefaultInitArgs
	/**
	 * Returns, in an array of String, the sequence of command-line
	 * arguments that would be used if the Prolog engine were to be initialised now.
	 * Returns null if the Prolog VM has already been initialised (in which
	 * case the default init args are irrelevant and the actual init args are of interest)<p>
	 * 
	 * @see jpl.JPL#getActualInitArgs
	 * @return   current default initialisation arguments, or null if already initialised
	 */
	public static String[] getDefaultInitArgs() {
		return Prolog.get_default_init_args();
	}

	//------------------------------------------------------------------/
	// setDefaultInitArgs
	/**
	 * Specifies, in an array of String, the sequence of command-line
	 * arguments that should be used if the Prolog engine is subsequently initialised.<p>
	 * 
	 * @param   args	new default initialization arguments
	 */
	public static void setDefaultInitArgs(String[] args) {
		Prolog.set_default_init_args(args);
	}

	//------------------------------------------------------------------/
	// getActualInitArgs
	/**
	 * Returns, in an array of String, the sequence of command-line
	 * arguments that were actually used when the Prolog engine was formerly initialised.
	 *
	 * This method returns null if the Prolog engine has not yet been initialised,
	 * and thus may be used to test this condition.
	 * 
	 * @return   actual initialization arguments
	 */
	public static String[] getActualInitArgs() {
		return Prolog.get_actual_init_args();
	}

	//------------------------------------------------------------------/
	// init
	/**
	 * Initializes the Prolog engine, using the String argument
	 * parameters passed.  This method need be called only if you want to both
	 * (i) initialise the Prolog engine with parameters other than the default ones
	 * and (ii) force initialisation to occur
	 * (rather than allow it to occur automatically at the first query).
	 * For parameter options, consult your local
	 * Prolog documentation.  The parameter values are passed directly
	 * to initialization routines for the Prolog environment.<p>
	 * 
	 * This method must be called before making any queries.
	 * 
	 * @param   args   Initialization parameter list
	 */
	public static boolean init(String[] args) {
		return Prolog.set_default_init_args(args) && init();
	}

	//------------------------------------------------------------------/
	// init
	/**
	 * Initialises the Prolog engine using the current default initialisation parameters,
	 * and returns 'true' (or 'false' if already initialised).
	 */
	public static boolean init() {
		return Prolog.initialise();
	}

	//------------------------------------------------------------------/
	// halt
	/**
	 * Terminates the Prolog session.<p>
	 * 
	 * <b>Note.</b>  This method calls the FLI halt() method with a
	 * status of 0, but the halt method currently is a no-op in SWI.
	 * @deprecated
	 */
	public static void halt() {
		Prolog.halt(0);
	}

	// a static reference to the current Version
	private static final Version version_ = new Version();

	//------------------------------------------------------------------/
	// version
	/**
	 * Returns (as a Version) an identification of this version of JPL.
	 * @return the running version of JPL.
	 */
	public static Version version() {
		return version_;
	}

	//------------------------------------------------------------------/
	// version_string
	/**
	 * Returns a String (eg "3.0.0-alpha") identifying this version of JPL.
	 * @return a String (eg "3.0.0-alpha") identifying this version of JPL.
	 */
	public static String version_string() {
		return version_.major + "." + version_.minor + "." + version_.patch + "-" + version_.status;
	}

	public static void main(String[] args) {
		System.out.println(version_string());
	}
}

//345678901234567890123456789012346578901234567890123456789012345678901234567890
