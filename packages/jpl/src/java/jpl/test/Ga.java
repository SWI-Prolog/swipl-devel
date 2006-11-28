package jpl.test;

import jpl.Query;

public class Ga {
	public static void main(String argv[]) {
		//		Prolog.set_default_init_args(new String[] { "libpl.dll", "-f", "D:/pcm/bin/pcm.ini", "-g", "pcm_2000" });
		//		(new Query("loadall(jpl_test:jr)")).hasSolution();
		//		System.err.println("jr " + ((new Query("jr")).hasSolution() ? "succeeded" : "failed"));
	 //	System.err.println( "something  " + (new Query("statistics(atoms,X)")).oneSolution().get("X"));
	 //	Query.hasSolution("statistics");
	 //	(new Query("x")).hasSolution();
	 //	(new Query("statistics,x")).hasSolution();
	 //	(new Query(new Atom("statistics"))).hasSolution();
	 //	Query.hasSolution("write(hello),nl");
	 //	Query.hasSolution("write(hello),nl");
		
	 //	(new Query("nl")).hasSolution();
		(new Query("nl,nl")).hasSolution();
		
	 //	(new Query("user:nl")).hasSolution();
	}
}
