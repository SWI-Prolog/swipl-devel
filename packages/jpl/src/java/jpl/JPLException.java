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
// JPLException
/**
 * This is the base class for exceptions thrown by JPL's Java-calls-Prolog interface.
 * Such exceptions represent errors and exceptional conditions within the interface code itself;
 * see jpl.PrologException for the way Prolog exceptions are returned to calling Java code.
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
public class JPLException extends RuntimeException {
	public JPLException() {
		super();
	}

	public JPLException(String s) {
		super(s);
	}
}
