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

// calls gen([t(c,q,[]),t(v,[],a)],ANSWER).

import java.util.Hashtable;
import jpl.*;

public class Zahed
{
	public static void
	main( java.lang.String argv[] )
	{
		
		System.out.println( "starting..." );

		Compound goal1 = new Compound("consult", new Term[] { new Atom("zahed.pl") });
		Query q1 = new Query(goal1);
		if ( !q1.hasSolution() ){
			System.out.println( "consult('zahed.pl') failed" );
			return;
		}

		Term t2 = new Compound("t", new Term[] { new Atom("v"), new Atom("[]"), new Atom("a") });
		Compound list2 = new Compound(".", new Term[] { t2, new Atom("[]") });

		Compound t1 = new Compound("t", new Term[] { new Atom("c"), new Atom("q"), new Atom("[]") });
		Compound list1 = new Compound(".", new Term[] { t1, list2 });

		Variable answer = new Variable("A");

		Compound goal2 = new Compound("gen", new Term[] { list1, answer });

		Query q2 = new Query(goal2);
		Hashtable solution = q2.oneSolution();
		if ( solution == null ) {
			System.out.println( "failed" );
		} else {
			System.out.println( solution.get("A").toString());
		}
		System.out.println( "finished" );
	}
}
