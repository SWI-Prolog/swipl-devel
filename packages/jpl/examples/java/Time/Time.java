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

public class Time
{
	static int       tree_depth = 10;
	static int       num_tests  = 5;
	static int       num_trials = 10;
	static long[][]        data = null;
	static Term            tree = null;
	static Atom      empty_tree = new Atom("t");
	static Timer          timer = new Timer();
	
	public static void
	main( java.lang.String argv[] )
	{
		parse_params( argv );
		
		JPL.init();
		
		run_tests();
	}
	
	static void
	parse_params( java.lang.String argv[] )
	{
		int i = 0;
		
		while ( i < argv.length ){
			if ( argv[i].equals( "-help" ) ){
				print_help();
				System.exit( 1 );
			} else if ( argv[i].equals( "-num_trials" ) ){
				num_trials = java.lang.Integer.valueOf( argv[i+1] ).intValue();
				i += 2;
			} else if ( argv[i].equals( "-tree_depth" ) ){
				tree_depth = java.lang.Integer.valueOf( argv[i+1] ).intValue();
				i += 2;
			} else {
				System.err.println( "Unrecognized option: " + argv[i] );
				print_help();
				System.exit( 1 );
			}
		}
		data = new long[num_tests][num_trials];
	}
	
	static void
	print_help()
	{
		System.out.println(
			"java Time\n" +
				"\t-help              print this screen\n" +
				"\t-num_trials <num>  specify number of trials (default: 10)\n" +
				"\t-tree_depth <num>  specify depth of binary tree (default: 10)\n" +
				"" );
	}
	
	static void
	run_tests()
	{
		test_0();
		test_1();
		test_2();
		test_3();
		test_4();
		
		print_results();
	}
	
	static void
	test_0()
	{
		System.out.print( "test 0..." );
		Query query = 
			new Query("consult('time.pl')");

		if ( !query.hasSolution() ){
			System.out.println( "consult('time.pl') failed" );
			System.exit( 1 );
		}
		System.out.println( "passed." );
		
		System.out.print( "trees" );
		for ( int i = 0;  i < num_trials;  ++i ){
			timer.start();
			tree = create_tree();
			timer.stop();
			
			data[0][i] = timer.getElapsedTimeInMillis();
			System.out.print( "." );
		}
		System.out.println( "done." );
	}

	static void
	test_1()
	{
		Query    query = new Query( "traverse", new Term[]{tree} );
		
		System.out.print( "traverse" );
		for ( int i = 0;  i < num_trials;  ++i ){
			
			timer.start();
			query.hasSolution();
			timer.stop();
			
			data[1][i] = timer.getElapsedTimeInMillis();
			System.out.print( "." );
		}
		System.out.println( "done." );
	}

	static void
	test_2()
	{
		Query    query = new Query( "noop", new Term[]{tree} );
		
		System.out.print( "noop" );
		for ( int i = 0;  i < num_trials;  ++i ){
			
			timer.start();
			java.util.Hashtable solution = query.oneSolution();
			timer.stop();
			
			data[2][i] = timer.getElapsedTimeInMillis();
			System.out.print( "." );
		}
		System.out.println( "done." );
	}

	static void
	test_3()
	{
		Variable     Y = new Variable("Y");
		Query    query = new Query( "noop_nobind", new Term[]{tree,Y} );
		
		System.out.print( "noop_nobind" );
		for ( int i = 0;  i < num_trials;  ++i ){
			
			timer.start();
			boolean ok = query.hasSolution();
			timer.stop();
			
			data[3][i] = timer.getElapsedTimeInMillis();
			System.out.print( "." );
		}
		System.out.println( "done." );
	}

	static void
	test_4()
	{
		Variable     Y = new Variable("Y");
		Query    query = new Query( "noop_bind", new Term[]{tree,Y} );
		
		System.out.print( "noop_bind" );
		for ( int i = 0;  i < num_trials;  ++i ){
			
			timer.start();
			java.util.Hashtable solution = query.oneSolution();
			timer.stop();
			
			data[4][i] = timer.getElapsedTimeInMillis();
			System.out.print( "." );
		}
		System.out.println( "done." );
	}
	
	static java.text.NumberFormat format = 
		java.text.NumberFormat.getInstance();
	static {
		format.setMaximumFractionDigits(3);
	}
	
	
	static void
	print_results()
	{
		long num_terms = (long)(Math.pow( 2, tree_depth ) + 1);
		System.out.println();
		System.out.println( "num_trials: " + num_trials );
		System.out.println( "tree_depth: " + tree_depth + 
			" (= " + num_terms + " terms)" );
		System.out.println();
		
		for ( int j = 0;  j < num_tests;  ++j ){
			System.out.print( "test_" + j + "\t\t" );
		}
		System.out.println( "\n" );
		for ( int i = 0;  i < num_trials;  ++i ){
			for ( int j = 0;  j < num_tests;  ++j ){
				System.out.print( data[j][i] + "\t\t" );
			}
			System.out.println();
		}
		System.out.println( "\n" );
		
		for ( int j = 0;  j < num_tests;  ++j ){
			System.out.println( 
				"test_" + j + ": " +
				"avg: " + format.format( avg( j ) ) + "ms\t\t" +
				format.format( avg( j )/num_terms ) + "ms/term"
			);
		}
	}
	
	static double
	avg( int test )
	{
		long min = java.lang.Long.MAX_VALUE,
		     max = java.lang.Long.MIN_VALUE;
		
		long sum = 0L;
		for ( int i = 0; i < num_trials;  ++i ){
			sum += data[test][i];
			if ( min < data[test][i] ){
				min = data[test][i];
			}
			if ( max > data[test][i] ){
				max = data[test][i];
			}
		}
		//sum -= (min+max);
		
		return sum/(num_trials/*-2*/);
	}
	
	
	static Term
	create_tree()
	{
		return binary_tree( tree_depth );
	}
	
	static Term
	binary_tree( int depth )
	{
		if ( depth <= 0 ){
			return empty_tree;
		} else {
			return 
				new Compound(
						"t",
						new Term[]{
							binary_tree( depth - 1 ), 
							binary_tree( depth - 1 )
						}
				);
		}
	}
	
	
	static class Timer
	{
		private java.util.Calendar calendar = java.util.Calendar.getInstance();

		private long start_time = 0L,
	            	 stop_time  = 0L;

		boolean running = false;

		public 
		Timer()
		{
		}

		public void
		start()
		{
			if ( !running ){
				start_time = getMillis();
				running    = true;
			}
		}

		private long
		getMillis()
		{
			return System.currentTimeMillis();
		}

		public void
		stop()
		{
			if ( running ){
				stop_time = getMillis();
				running   = false;
			}
		}

		public long
		getElapsedTimeInMillis()
		{
			if ( running ){
				return getMillis() - start_time;
			} else {
				return stop_time - start_time;
			}
		}

		public double
		getElapsedTimeInSeconds()
		{
			return getElapsedTimeInMillis()/1000;
		}
	}
}
