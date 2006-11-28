package jpl.test;

import java.util.Map;
import jpl.Atom;
import jpl.Compound;
import jpl.Integer;
import jpl.JPL;
import jpl.PrologException;
import jpl.Query;
import jpl.Term;
import jpl.Util;
import jpl.Variable;
import jpl.fli.Prolog;

// This class is nearly obsolete; most of its tests have been migrated to TestJUnit.
public class TestOLD {
	private static void test10() {
		System.err.println("test10:");
		System.err.println("  java_lib_version = " + JPL.version_string());
		System.err.println("  c_lib_version = " + jpl.fli.Prolog.get_c_lib_version());
		System.err.println("  pl_lib_version = " + new Query(new Compound("jpl_pl_lib_version", new Term[] { new Variable("V") })).oneSolution().get("V"));
		System.err.println("  java.version = " + System.getProperty("java.version"));
		System.err.println("  os.name = " + System.getProperty("os.name"));
		System.err.println("  os.arch = " + System.getProperty("os.arch"));
		System.err.println("  os.version = " + System.getProperty("os.version"));
		System.err.println();
	}
	private static void test10j() {
		Term l2 = Util.termArrayToList(new Term[] { new Atom("a"), new Atom("b"), new Atom("c"), new Atom("d"), new Atom("e") });
		Query q9 = new Query(new Compound("append", new Term[] { new Variable("Xs"), new Variable("Ys"), l2 }));
		Map[] s9s = q9.allSolutions();
		System.err.println("test10j:");
		for (int i = 0; i < s9s.length; i++) {
			System.err.println("  append(Xs,Ys,[a,b,c,d,e]) -> " + Util.toString(s9s[i]));
		}
		System.err.println();
	}
	private static void test10k() {
		String[] args = jpl.fli.Prolog.get_default_init_args();
		String which;
		String s = "";
		System.err.println("test10k:");
		if (args == null) {
			args = jpl.fli.Prolog.get_actual_init_args();
			which = "actual";
		} else {
			which = "default";
		}
		for (int i = 0; i < args.length; i++) {
			s = s + args[i] + " ";
		}
		System.err.println("  " + which + "_init_args = " + s + '\n');
	}
	private static void test10l() {
		Query q5 = new Query(new Compound("length", new Term[] { new Variable("Zs"), new jpl.Integer(5) }));
		Map s5 = q5.oneSolution();
		System.err.println("test10l:");
		System.err.println("  length(Zs,5)");
		System.err.println("  " + Util.toString(s5));
		System.err.println("  Zs = " + (Term) s5.get("Zs"));
		System.err.println();
	}
	private static void test10m() {
		String text = "append(Xs,Ys,[_,_,_,_,_])";
		Query q = new Query(text);
		Map[] ss = q.allSolutions();
		System.err.println("test10m:");
		System.err.println("  all solutions of " + text);
		for (int i = 0; i < ss.length; i++) {
			System.err.println("  " + Util.toString(ss[i]));
		}
		System.err.println();
	}
	private static void test10o() {
		System.err.println("test10o:");
		Term l2b = Util.termArrayToList(new Term[] { new Variable("A"), new Variable("B"), new Variable("C"), new Variable("D"), new Variable("E") });
		Query q9b = new Query(new Compound("append", new Term[] { new Variable("Xs"), new Variable("Ys"), l2b }));
		Map[] s9bs = q9b.allSolutions();
		for (int i = 0; i < s9bs.length; i++) {
			System.err.println("  append(Xs,Ys,[A,B,C,D,E]) -> " + Util.toString(s9bs[i]));
		}
		System.err.println();
	}
	private static void test10q() {
		System.err.println("test10q:");
		System.err.println((new Compound("Bad Name", new Term[] { new Atom("3 3") })).toString());
		System.err.println();
	}
	private static void test10s() {
		final Query q = new Query("jpl_slow_goal"); // 10 successive sleep(1)
		System.err.println("test10s:");
		Thread t = new Thread(new Runnable() {
			public void run() {
				try {
					System.err.println("q.hasSolution() ... ");
					System.err.println(q.hasSolution() ? "finished" : "failed");
				} catch (Exception e) {
					System.err.println("q.hasSolution() threw " + e);
				}
			}
		});
		t.start(); // call the query in a separate thread
		System.err.println("pausing for 2 secs...");
		try {
			Thread.sleep(2000);
		} catch (InterruptedException e) {
			;
		} // wait a coupla seconds for it to get started
		// (new Query("set_prolog_flag(abort_with_exception, true)")).hasSolution();
		System.err.println("calling q.abort()...");
		q.abort();
		System.err.println();
	}
	public static void main(String argv[]) {
		Prolog.set_default_init_args(new String[] { "libpl.dll", "-f", "none", "-g", "set_prolog_flag(debug_on_error,false)", "-q" });
		System.err.println("tag = " + Prolog.object_to_tag(new Query("hello")));
		test10k();
		test10();
		// test10h();
		// test10i();
		test10j();
		test10k();
		test10l();
		test10m();
		// test10n();
		test10o();
		//test10p();
		test10q();
		// test10r();
		// test10s();
		// test10t();
		// test10u();
		// test10v();
		String s = new String("" + '\0' + '\377');
		System.err.println("s.length = " + s.length());
		for (int i = 0; i < s.length(); i++) {
			System.err.print((new Integer(s.charAt(i))).toString() + " ");
		}
		System.err.println();
		System.err.println(new Query("atom_codes(A,[127,128,255,0])").oneSolution().toString());
	}
}