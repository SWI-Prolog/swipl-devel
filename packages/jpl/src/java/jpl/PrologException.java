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

//----------------------------------------------------------------------/
// PrologException
/**
 * PrologException instances wrap Prolog exceptions thrown (either by a Prolog engine or by user code)
 * in the course of finding a solution to a Query.  See JPLException for the handling of errors within the JPL Java-calls-Prolog interface.
 * <p>
 * This class allows Java code which uses JPL's Java-calls-Prolog API to handle
 * Prolog exceptions, which is in general necessary for hybrid Java+Prolog programming.
 * <p>
 * Use the term() accessor to obtain a Term representation of the term that was
 * thrown from within Prolog.
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
public final class PrologException extends JPLException {
	private Term term_ = null;

	protected PrologException(Term term) {
		super("PrologException: " + term.toString());

		this.term_ = term;
	}

	/**
	 * @return a reference to the Term thrown by the call to throw/1
	 */
	public Term term() {
		return this.term_;
	}
}
