//tabstop=4

import jpl.*;

public class Test2
{ public static int fac(int n)
  { if (n == 1)
    { return 1;
    } else
    { return n * ((jpl.Integer)
		  new Query(new Compound("jpl_test_fac", new Term[]
					 { new jpl.Integer(n - 1),
					   new Variable("F")
					 })).oneSolution().get("F")).intValue();
    }
  }

  public static void
  main( java.lang.String argv[] )
  { new Query("consult('test2.pl')").oneSolution();

    System.out.print( "calling Prolog to call Java to call Prolog...\n" );

    System.out.println( "factorial(10) = " + fac(10));
  }
}
