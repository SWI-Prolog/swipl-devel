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



import java.util.Hashtable;
import jpl.Query;				// empirically, we need this, but I don't know why...
import jpl.*;

public class Test
{
	public static void
	main( java.lang.String argv[] )
	{
	 //	JPL.init();				// we don't need this with the current JPL (lazy init-on-demand)
		
		run_tests();
	}
	
	static void
	run_tests()
	{
		test_0();
		test_1();
		test_2();
		test_3();
		test_4();
		test_5();
		test_6();
		test_7();
		test_8();
		test_9();
		test_10();
		test_11();
		
		test_101();
	}
	
	static void
	test_0()
	{
		System.out.print( "test 0..." );

		Query query = 
			new Query("consult('test.pl')");

		if ( !query.hasSolution() ){
			System.out.println( "consult('test.pl') failed" );
			System.exit( 1 );
		}
		System.out.println( "passed." );
	}
	
	static Term a = 
		new Atom( "a" );
	static Term b = 
		new Atom( "b" );
	static Term f_a = 
		new Compound(
			"f",
			new Term[] {a}
		);
	static Term pair_a_b =
		new Compound(
			"-",
			new Term[] {a,b}
		);
						
	static void
	test_1()
	{
		System.out.print( "test 1..." );
		Query query = 
			new Query("p(a)");

		if ( !query.hasSolution() ){
			System.out.println( "p(a) failed" );
			System.exit( 1 );
		}
		System.out.println( "passed." );
	}
						
	static void
	test_2()
	{
		System.out.print( "test 2..." );
		Query query = 
			new Query( 
				"p", 
				new Term[] {f_a}
			);

		if ( !query.hasSolution() ){
			System.out.println( "p(f(a)) failed" );
			System.exit( 1 );
		}
		System.out.println( "passed." );
	}
						
	static void
	test_3()
	{
		System.out.print( "test 3..." );
		Query query = 
			new Query( 
				"p", 
				new Term[] {pair_a_b}
			);

		if ( !query.hasSolution() ){
			System.out.println( "p( a-b ) failed" );
			System.exit( 1 );
		}
		System.out.println( "passed." );
	}
						
	static void
	test_4()
	{
		System.out.print( "test 4..." );
		Variable X = new Variable("X");
		Query query = 
			new Query( 
				"p", 
				new Term[] {X}
			);

		Term[] target = new Term[] {a,f_a,pair_a_b,new Variable("_")};

		Hashtable[] solutions = query.allSolutions();
		
		if ( solutions.length != 4 ){
			System.out.println( "p(X) failed:" );
			System.out.println( "\tExpected: 4 solutions" );
			System.out.println( "\tGot:      " + solutions.length );
			System.exit( 1 );
		}
		
		for ( int i = 0;  i < solutions.length-1;  ++i ){
			Term binding = (Term)solutions[i].get( "X" );
			if ( ! binding.equals( target[i] ) ){
				System.out.println( "p(X) failed" );
				System.out.println( "\tExpected: " + target[i]);
				System.out.println( "\tGot:      " + binding);
				System.exit( 1 );
			}
		}

		System.out.println( "passed." );
	}
						
	static void
	test_5()
	{
		System.out.print( "test 5..." );
		Variable X = new Variable("X");
		Variable Y = new Variable("Y");
		Query query = 
			new Query( 
				"p", 
				new Term[] {X,Y}
			);

		Term[] x_target = new Term[] {a,a};
		Term[] y_target = new Term[] {a,b};

		Hashtable[] solutions = query.allSolutions();
		
		if ( solutions.length != 2 ){
			System.out.println( "p(X, Y) failed:" );
			System.out.println( "\tExpected: 2 solutions" );
			System.out.println( "\tGot:      " + solutions.length );
			System.exit( 1 );
		}
		
		for ( int i = 0;  i < solutions.length;  ++i ){
			Object x_binding = solutions[i].get("X");
			if ( ! x_binding.equals( x_target[i] ) ){
				System.out.println( "p(X, Y) failed:" );
				System.out.println( "\tExpected: " + x_target[i] );
				System.out.println( "\tGot:      " + x_binding );
				System.exit( 1 );
			} 
			Object y_binding = solutions[i].get("Y");
			if ( ! y_binding.equals( y_target[i] ) ){
				System.out.println( "p( X, Y ) failed:" );
				System.out.println( "\tExpected: " + y_target[i] );
				System.out.println( "\tGot:      " + y_binding );
				System.exit( 1 );
			} 
		}
		System.out.println( "passed." );
	}
						
	static void
	test_6()
	{
		System.out.print( "test 6..." );
		Variable X = new Variable("X");
		Query query = 
			new Query( 
				"p", 
				new Term[] {X,X}
			);

		Term[] x_target = new Term[] {a};

		Hashtable[] solutions = query.allSolutions();
		
		if ( solutions.length != 1 ){
			System.out.println( "p(X, X) failed:" );
			System.out.println( "\tExpected: 1 solution" );
			System.out.println( "\tGot:      " + solutions.length );
			System.exit( 1 );
		}
		
		for ( int i = 0;  i < solutions.length;  ++i ){
			Object x_binding = solutions[i].get("X");
			if ( ! x_binding.equals( x_target[i] ) ){
				System.out.println( "p(X, X) failed:" );
				System.out.println( "\tExpected: " + x_target[i] );
				System.out.println( "\tGot:      " + x_binding );
				System.exit( 1 );
			}
		}
		System.out.println( "passed." );
	}
						
	static void
	test_7()
	{
		System.out.print( "test 7..." );
		Variable X = new Variable("X");
		Variable Y = new Variable("Y");
		Query query = 
			new Query( 
				"r", 
				new Term[] {
					new Compound(
						"f",
						new Term[] {X,X}
					), 
					Y
				}
			);

		Hashtable[] solutions = query.allSolutions();
		
		if ( solutions.length != 2 ){
			System.out.println( "r(f(X,X), Y) failed:" );
			System.out.println( "\tExpected: 2 solutions" );
			System.out.println( "\tGot:      " + solutions.length );
			System.exit( 1 );
		}
		
		Object x_binding, y_binding;
		
		x_binding = solutions[0].get("X");
		y_binding = solutions[0].get("Y");
		if ( x_binding != y_binding ){
			System.out.println( "r(f(X,X), Y) failed:" );
			System.out.println( Util.toString( solutions[0] ) );
			System.out.println( "\tThe variables to which X and Y are bound in the first solution should be identical." );
			System.exit( 1 );
		}
		
		x_binding = solutions[1].get("X");
		y_binding = solutions[1].get("Y");
		if ( x_binding == y_binding ){
			System.out.println( "r(f(X,X), Y) failed:" );
			System.out.println( Util.toString( solutions[1] ) );
			System.out.println( "\tThe variables to which X and Y are bound in the second solution should be distinct." );
			System.exit( 1 );
		}
		if ( x_binding.equals( y_binding ) ){
			System.out.println( "r(f(X,X), Y) failed:" );
			System.out.println( Util.toString( solutions[1] ) );
			System.out.println( "\tThe variables to which X and Y are bound in the second solution should not be \"equal\"." );
			System.exit( 1 );
		}
		/*
		if ( ! solutions[0].get("X").equals( solutions[1].get("X") ) ){
			System.out.println( "r(f(X,X), Y) failed:" );
			System.out.println( Util.toString( solutions[0] ) );
			System.out.println( 
				"\tThe variable to which X is bound in the first solution (" + solutions[0].get("X") + ")\n" +
				"\tshould be equal to the variable to which X is bound in the second (" + solutions[1].get("X") + ")");
			System.exit( 1 );
		}
		 */
		System.out.println( "passed." );
	}
						
	static void
	test_8()
	{
		System.out.print( "test 8..." );
		Variable X = new Variable("X");
		Query query = 
			new Query( 
				"r", 
				new Term[] {
					new Compound(
						"f",
						new Term[] {X,X}
					), 
					X
				}
			);

		Hashtable[] solutions = query.allSolutions();
		
		if ( solutions.length != 2 ){
			System.out.println( "r( f( X, X ), X ) failed:" );
			System.out.println( "\tExpected: 2 solutions" );
			System.out.println( "\tGot:      " + solutions.length );
			System.exit( 1 );
		}
		/*
		if ( ! solutions[0].get("X").equals( solutions[1].get("X") ) ){
			System.out.println( "r( f( X, X ), X ) failed:" );
			System.out.println( Util.toString( solutions[0] ) );
			System.out.println( 
				"\tThe variable to which X is bound in the first solution\n" +
				"\tshould be equal to the variable to which X is bound in the second." );
			System.exit( 1 );
		}
		 */
		System.out.println( "passed." );
	}
	
	// corresponds with Prolog List: [a-a,a-b]
	static Term test_9_solution =
		Util.termArrayToList(
			new Term[] {
				new Compound( "-", new Term[] {a,a}),
				new Compound( "-", new Term[] {a,b})
			}
		);
				
	static void
	test_9()
	{
		System.out.print( "test 9..." );
		Variable X   = new Variable("X");
		Variable Y   = new Variable("Y");
		Variable XYs = new Variable("XYs");
		Query query = 
			new Query( 
				"bagof", 
				new Term[] { 
					new Compound(
						"-",
						new Term[] {X,Y}
					),
					new Compound(
						"p",
						new Term[] {X,Y}
					), 
					XYs
				}
			);

		Hashtable[] solutions = query.allSolutions();
		
		if ( solutions.length != 1 ){
			System.out.println( "bagof(X-Y, p(X,Y), XYs) failed:" );
			System.out.println( "\tExpected: 1 solution" );
			System.out.println( "\tGot:      " + solutions.length );
			System.exit( 1 );
		}
		
		Term term = (Term) solutions[0].get("XYs");
		
	 //	if ( ! (term instanceof List) ){
		if ( ! (term instanceof Compound && ".".equals(((Compound)term).name()) && ((Compound)term).arity()==2) ){
			System.out.println( "bagof(X-Y, p(X,Y), XYs) failed:" );
			System.out.println( "\tExpected: XYs to be a List" );
			System.out.println( "\tGot:      " + term );
			System.exit( 1 );
		}
		
		if ( ! term.equals( test_9_solution ) ){
			System.out.println( "bagof(X-Y, p(X,Y), XYs) failed:" );
			System.out.println( "\tExpected: " + test_9_solution );
			System.out.println( "\tGot:      " + term );
			System.exit( 1 );
		}
		
		System.out.println( "passed." );
	}
						
	static void
	test_10()
	{
		System.out.print( "test 10..." );
		Query query = 
			new Query( "t" );

		try {
			boolean b = query.hasSolution();
			System.out.println( "t failed:" );
			System.out.println( "\tExpected: JPLException" );
			System.out.println( "\tGot: " + b );
			System.exit( 1 );
		} catch ( PrologException e ){
		}
		
		System.out.println( "passed." );
	}
						
	static void
	test_11()
	{
		System.out.print( "test 11..." );
		Term tuple =
			new Compound(
				"t",
				new Term[]{
					new Atom( "a" ),
					new Atom( "b" ),
					new Atom( "c" ),
					new Atom( "d" ),
					new Atom( "e" )
				}
			);

		try {
			Variable  X = new Variable("X");
			Query query = new Query( "tuple", new Term[] {X} );
			
			java.util.Hashtable solution = query.oneSolution();
			
			Term result = (Term) solution.get("X");
			
			if ( result == null || ! result.equals( tuple ) ){
				System.out.println( "failed:" );
				System.out.println( "\tresult: " + result );
				System.out.println( "\ttuple:  " + tuple );
				System.exit( 1 );
			}
			
			Term term;
			
			term = new Atom( "a" );
			if ( ((Compound)result).arg( 1 ) == null || !((Compound)result).arg( 1 ).equals( term ) ){
				System.out.println( "failed:" );
				System.out.println( "\t((Compound)result).arg( 1 ): " + ((Compound)result).arg( 1 ) );
				System.out.println( "\tterm           : " + term );
				System.exit( 1 );
			}
			term = new Atom( "b" );
			if ( ((Compound)result).arg( 2 ) == null || !((Compound)result).arg( 2 ).equals( term ) ){
				System.out.println( "failed:" );
				System.out.println( "\t((Compound)result).arg( 2 ): " + ((Compound)result).arg( 2 ) );
				System.out.println( "\tterm           : " + term );
				System.exit( 1 );
			}
			term = new Atom( "e" );
			if ( ((Compound)result).arg( 5 ) == null || !((Compound)result).arg( 5 ).equals( term ) ){
				System.out.println( "failed:" );
				System.out.println( "\t((Compound)result).arg( 5 ): " + ((Compound)result).arg( 5 ) );
				System.out.println( "\tterm           : " + term );
				System.exit( 1 );
			}
			// arg0(6) throws an exception, as I'd expect it to...
		 //	if ( ((Compound)result).arg( 7 ) != null ){
		 //		System.out.println( "failed:" );
		 //		System.out.println( "\t((Compound)result).arg( 7 ): " + ((Compound)result).arg( 7 ) );
		 //		System.out.println( "\tshould be null" );
		 //		System.exit( 1 );
		 //	}
		} catch ( PrologException e ){
			System.out.println( "failed" );
			e.printStackTrace();
			System.exit( 1 );
		}
		
		System.out.println( "passed." );
	}
						
	static void
	test_101()
	{
		System.out.print( "test 101..." );
		Thread[] threads = new Thread[10];
		
		for ( int i = 0;  i < threads.length;  ++i ){
			threads[i] = new QueryThread( i );
		}
		for ( int i = 0;  i < threads.length;  ++i ){
			threads[i].start();
		}
		for ( int i = 0;  i < threads.length;  ++i ){
			try {
				threads[i].join();
			} catch ( InterruptedException ie ){
				ie.printStackTrace();
				System.exit( 1 );
			}
		}
		System.out.println( "passed." );
	}
	
	private static class
	QueryThread extends Thread
	{
		private int id_ = -1;

		public 
		QueryThread( int id )
		{
			this.id_ = id;
		}

		public java.lang.String
		toString()
		{
			return "(QueryThread id=" + id_ + ")";
		}
		
		
		public void
		run()
		{
			Query query = 
				new Query( 
					"p",
					new Term[] {
						new Atom("a"),
						new Atom("a")
					}
				);
			
			for ( int i = 0;  i < 10;  ++i ){
				try {
					query.hasSolution();
				} catch ( jpl.JPLException e ){
					System.out.println( "Threaded p( a, a ) threw exception: " + e);
					System.exit( 1 );
				}
				System.out.print( id_ );
				Thread.yield();
			}
			for ( int i = 0;  i < 10;  ++i ){
			 //	synchronized ( Query.lock() ){
					try {
						while ( query.hasMoreSolutions() ){
							Thread.yield();
							query.nextSolution();
						}
					} catch ( jpl.JPLException e ){
						System.out.println( "Threaded p( a, a ) threw exception: " + e);
						System.exit( 1 );
					}
					System.out.print( id_ );
			 //	}
			}
		}
	}
	
	
	// more to come??
}
