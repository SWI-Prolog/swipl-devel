import java.util.Hashtable;
import jpl.*;
import jpl.Query;

public class SemWeb
{ public static void
    main(String argv[])
    { String t1 = "use_module(library('semweb/rdf_db'))";
      Query q1 = new Query(t1);
      
      System.out.println( t1 + " " + (q1.hasSolution() ? "succeeded" : "failed") );

      Query ql = new Query("rdf_load('test.rdf')");
      System.out.println(ql.hasSolution() ? "loaded" : "load failed");

      String t2 = "rdf(S,P,O)";
      Query q2 = new Query(t2);
      while ( q2.hasMoreSolutions() )
      { java.util.Hashtable s2 = q2.nextSolution();
	System.out.println("{" + s2.get("S") +
			   ", " + s2.get("P") +
			   ", " + s2.get("O") + "}");
      }
    }

}


