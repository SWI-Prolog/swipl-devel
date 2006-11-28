package jpl.test;

import jpl.Query;
import jpl.Term;

public class FetchLongList {
	public static void main(String[] args) {
	 //	Prolog.set_default_init_args(new String[] { "libpl.dll", "-f", "D:/pcm/bin/pcm.ini", "-g", "pcm_2000" });
		Term t = (Term)((new Query("findall(foo(N,bar),between(1,2308,N),L)")).oneSolution().get("L"));
		int i = 0;
		while ( t.hasFunctor(".", 2)){
			t = t.arg(2);
			i = i+1;
		}
		System.err.println("got a list of " + i+" members");
	}
}
