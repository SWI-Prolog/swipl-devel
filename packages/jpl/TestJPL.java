import jpl.*;

class TestJPL
{ public String name;

  public TestJPL()
  { name = "<unnamed>";
  }
  public TestJPL(String s)
  { name = s;
  }
		     
  public void hello(String[] args)
  { System.out.println("Hello " + args + "!" );
  }

  public int magic()
  { return 42;
  }

  public void prologBanner()
  { Query q = new Query("$welcome");
//    q.oneSolution();
  }
}
