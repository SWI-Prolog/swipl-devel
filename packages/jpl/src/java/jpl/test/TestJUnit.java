// Created on 25-Jul-2004
package jpl.test;

import java.util.Map;
import jpl.Atom;
import jpl.Compound;
import jpl.Integer;
import jpl.JPL;
import jpl.JRef;
import jpl.PrologException;
import jpl.Query;
import jpl.Term;
import jpl.Util;
import jpl.Variable;
import jpl.fli.Prolog;
import junit.framework.TestCase;
import junit.framework.TestSuite;

// This class defines all the tests which are run from Java.
// It needs junit.framework.TestCase and junit.framework.TestSuite, which are not supplied with JPL.
public class TestJUnit extends TestCase {
	//
	public static long fac(long n) { // complements jpl:jpl_test_fac(+integer,-integer)
		if (n == 1) {
			return 1;
		} else if (n > 1) {
			return n * ((jpl.Integer) Query.oneSolution("jpl_test_fac(?,F)", new Term[] { new jpl.Integer(n - 1) }).get("F")).longValue();
		} else {
			return 0;
		}
	}
	public TestJUnit(String name) {
		super(name);
	}
	public static junit.framework.Test suite() {
		return new TestSuite(TestJUnit.class);
	}
	public static void main(String args[]) {
		junit.textui.TestRunner.run(suite());
	}
	protected void setUp() {
		// initialization code
		// Prolog.set_default_init_args(new String[] { "libpl.dll", "-f", "none", "-g", "set_prolog_flag(debug_on_error,false)", "-q" });
		Prolog.set_default_init_args(new String[] { "libpl.dll", "-f", "none", "-g", "true", "-q" });
		assertTrue((new Query("consult(test_jpl)")).hasSolution());
	}
	protected void tearDown() {
		// cleanup code
	}
	//
	public void testMasstest() {
		assertTrue((new Query("assert(diagnose_declaration(_,_,_,[not,a,real,error]))")).hasSolution());
	}
	public void testSameLibVersions1() {
		String java_lib_version = JPL.version_string();
		String c_lib_version = jpl.fli.Prolog.get_c_lib_version();
		assertTrue("java_lib_version(" + java_lib_version + ") is same as c_lib_version(" + c_lib_version + ")", java_lib_version.equals(c_lib_version));
	}
	public void testSameLibVersions2() {
		String java_lib_version = JPL.version_string();
		String pl_lib_version = ((Term) (new Query(new Compound("jpl_pl_lib_version", new Term[] { new Variable("V") })).oneSolution().get("V"))).name();
		assertTrue("java_lib_version(" + java_lib_version + ") is same as pl_lib_version(" + pl_lib_version + ")", java_lib_version.equals(pl_lib_version));
	}
	public void testAtomName1() {
		String name = "fred";
		Atom a = new Atom(name);
		assertEquals("an Atom's name is that with which it was created", a.name(), name);
	}
	public void testAtomName2() {
		String name = "ha ha";
		Atom a = new Atom(name);
		assertEquals("an Atom's name is that with which it was created", a.name(), name);
	}
	public void testAtomName3() {
		String name = "3";
		Atom a = new Atom(name);
		assertEquals("an Atom's name is that with which it was created", a.name(), name);
	}
	public void testAtomToString1() {
		String name = "fred";
		String toString = "fred";
		Atom a = new Atom(name);
		assertEquals("an Atom's .toString() value is quoted iff appropriate", a.toString(), toString);
	}
	public void testAtomToString2() {
		String name = "ha ha";
		String toString = "'ha ha'";
		Atom a = new Atom(name);
		assertEquals("an Atom's .toString() value is quoted iff appropriate", a.toString(), toString);
	}
	public void testAtomToString3() {
		String name = "3";
		String toString = "'3'";
		Atom a = new Atom(name);
		assertEquals("an Atom's .toString() value is quoted iff appropriate", a.toString(), toString);
	}
	public void testAtomArity() {
		Atom a = new Atom("willy");
		assertEquals("an Atom has arity zero", a.arity(), 0);
	}
	public void testAtomEquality1() {
		String name = "fred";
		Atom a1 = new Atom(name);
		Atom a2 = new Atom(name);
		assertEquals("two Atoms created with the same name are equal", a1, a2);
	}
	public void testAtomIdentity() { // how could this fail?!
		String name = "fred";
		Atom a1 = new Atom(name);
		Atom a2 = new Atom(name);
		assertNotSame("two Atoms created with the same name are not identical", a1, a2);
	}
	public void testAtomHasFunctorNameZero() {
		String name = "sam";
		Atom a = new Atom(name);
		assertTrue(a.hasFunctor(name, 0));
	}
	public void testAtomHasFunctorWrongName() {
		assertFalse("an Atom does not have a functor whose name is other than that with which the Atom was created", new Atom("wally").hasFunctor("poo", 0));
	}
	public void testAtomHasFunctorWrongArity() {
		String name = "ted";
		assertFalse("an Atom does not have a functor whose arity is other than zero", new Atom(name).hasFunctor(name, 1));
	}
	public void testVariableBinding1() {
		Term lhs = new Compound("p", new Term[] { new Variable("X"), new Variable("Y") });
		Term rhs = new Compound("p", new Term[] { new Atom("a"), new Atom("b") });
		Term goal = new Compound("=", new Term[] { lhs, rhs });
		Map soln = new Query(goal).oneSolution();
		assertTrue("two variables with different names can bind to distinct atoms", soln != null && ((Term) soln.get("X")).name().equals("a") && ((Term) soln.get("Y")).name().equals("b"));
	}
	public void testVariableBinding2() {
		Term lhs = new Compound("p", new Term[] { new Variable("X"), new Variable("X") });
		Term rhs = new Compound("p", new Term[] { new Atom("a"), new Atom("b") });
		Term goal = new Compound("=", new Term[] { lhs, rhs });
		assertFalse("two distinct Variables with same name cannot unify with distinct atoms", new Query(goal).hasSolution());
	}
	public void testVariableBinding3() {
		Variable X = new Variable("X");
		Term lhs = new Compound("p", new Term[] { X, X });
		Term rhs = new Compound("p", new Term[] { new Atom("a"), new Atom("b") });
		Term goal = new Compound("=", new Term[] { lhs, rhs });
		assertFalse("two occurrences of same named Variable cannot unify with distinct atoms", new Query(goal).hasSolution());
	}
	public void testVariableBinding4() {
		Term lhs = new Compound("p", new Term[] { new Variable("_"), new Variable("_") });
		Term rhs = new Compound("p", new Term[] { new Atom("a"), new Atom("b") });
		Term goal = new Compound("=", new Term[] { lhs, rhs });
		assertTrue("two distinct anonymous Variables can unify with distinct atoms", new Query(goal).hasSolution());
	}
	public void testVariableBinding5() {
		Variable Anon = new Variable("_");
		Term lhs = new Compound("p", new Term[] { Anon, Anon });
		Term rhs = new Compound("p", new Term[] { new Atom("a"), new Atom("b") });
		Term goal = new Compound("=", new Term[] { lhs, rhs });
		assertTrue("two occurrences of same anonymous Variable can unify with distinct atoms", new Query(goal).hasSolution());
	}
	public void testAtomEquality2() {
		Atom a = new Atom("a");
		assertTrue("two occurrences of same Atom are equal by .equals()", a.equals(a));
	}
	public void testAtomEquality3() {
		assertTrue("two distinct Atoms with same names are equal by .equals()", (new Atom("a")).equals(new Atom("a")));
	}
	public void testTextToTerm1() {
		String text = "fred(B,p(A),[A,B,C])";
		Term t = Util.textToTerm(text);
		assertTrue("Util.textToTerm() converts \"fred(B,p(A),[A,B,C])\" to a corresponding Term", t.hasFunctor("fred", 3) && t.arg(1).isVariable() && t.arg(1).name().equals("B")
				&& t.arg(2).hasFunctor("p", 1) && t.arg(2).arg(1).isVariable() && t.arg(2).arg(1).name().equals("A"));
	}
	public void testArrayToList1() {
		Term l2 = Util.termArrayToList(new Term[] { new Atom("a"), new Atom("b"), new Atom("c"), new Atom("d"), new Atom("e") });
		Query q9 = new Query(new Compound("append", new Term[] { new Variable("Xs"), new Variable("Ys"), l2 }));
		assertTrue("append(Xs,Ys,[a,b,c,d,e]) has 6 solutions", q9.allSolutions().length == 6);
	}
	public void testArrayToList2() {
		String goal = "append(Xs,Ys,[a,b,c,d,e])";
		assertTrue(goal + " has 6 solutions", Query.allSolutions(goal).length == 6);
	}
	public void testLength1() {
		Query q5 = new Query(new Compound("length", new Term[] { new Variable("Zs"), new jpl.Integer(2) }));
		Term zs = (Term) (q5.oneSolution().get("Zs"));
		assertTrue("length(Zs,2) binds Zs to a list of two distinct variables " + zs.toString(), zs.hasFunctor(".", 2) && zs.arg(1).isVariable() && zs.arg(2).hasFunctor(".", 2)
				&& zs.arg(2).arg(1).isVariable() && zs.arg(2).arg(2).hasFunctor("[]", 0) && !zs.arg(1).name().equals(zs.arg(2).arg(1).name()));
	}
	public void testGenerate1() { // we chickened out of verifying each solution :-)
		String goal = "append(Xs,Ys,[_,_,_,_,_])";
		assertTrue(goal + " has 6 solutions", Query.allSolutions(goal).length == 6);
	}
	public void testPrologException1() {
		try {
			new Query("p(]"); // writes junk to stderr and enters debugger unless flag debug_on_error = false
		} catch (PrologException e) {
			assertTrue("new Query(\"p(]\") throws a PrologException " + e.toString(), true);
			return;
		}
		fail("new Query(\"p(]\") oughta throw a PrologException");
	}
	public void testAtom1() {
		assertTrue("new Atom(\"3 3\")" + (new Atom("3 3")).toString(), true);
	}
	public void testTextToTerm2() {
		String text1 = "fred(?,2,?)";
		String text2 = "[first(x,y),A]";
		Term plist = Util.textToTerm(text2);
		Term[] ps = plist.toTermArray();
		Term t = Util.textToTerm(text1).putParams(ps);
		assertTrue("fred(?,2,?) .putParams( [first(x,y),A] )", t.hasFunctor("fred", 3) && t.arg(1).hasFunctor("first", 2) && t.arg(1).arg(1).hasFunctor("x", 0) && t.arg(1).arg(2).hasFunctor("y", 0)
				&& t.arg(2).hasFunctor(2, 0) && t.arg(3).isVariable() && t.arg(3).name().equals("A"));
	}
	public void testDontTellMeMode1() {
		final Query q = new Query("setof(_M,current_module(_M),_Ms),length(_Ms,N)");
		JPL.setDTMMode(true);
		assertTrue("in dont-tell-me mode, setof(_M,current_module(_M),_Ms),length(_Ms,N) returns binding for just one variable", q.oneSolution().keySet().size() == 1);
	}
	public void testDontTellMeMode2() {
		final Query q = new Query("setof(_M,current_module(_M),_Ms),length(_Ms,N)");
		JPL.setDTMMode(false);
		assertTrue("not in dont-tell-me mode, setof(_M,current_module(_M),_Ms),length(_Ms,N) returns binding for three variables", q.oneSolution().keySet().size() == 3);
	}
	public void testModulePrefix1() {
		assertTrue(Query.hasSolution("call(user:true)"));
	}
	private void testMutualRecursion(int n, long f) { // f is the expected result for fac(n)
		try {
			assertEquals("mutual recursive Java<->Prolog factorial: fac(" + n + ") = " + f, fac(n), f);
		} catch (Exception e) {
			fail("fac(" + n + ") threw " + e);
		}
	}
	public void testMutualRecursion1() {
		testMutualRecursion(1, 1);
	}
	public void testMutualRecursion2() {
		testMutualRecursion(2, 2);
	}
	public void testMutualRecursion3() {
		testMutualRecursion(3, 6);
	}
	public void testMutualRecursion10() {
		testMutualRecursion(10, 3628800);
	}
	public void testIsJNull1() {
		Term t = (Term) (new Query("X = @(null)")).oneSolution().get("X");
		assertTrue("@(null) . isJNull() succeeds", t.isJNull());
	}
	public void testIsJNull2() {
		Term t = (Term) (new Query("X = @(3)")).oneSolution().get("X");
		assertFalse("@(3) . isJNull() fails", t.isJNull());
	}
	public void testIsJNull3() {
		Term t = (Term) (new Query("X = _")).oneSolution().get("X");
		assertFalse("_ . isJNull() fails", t.isJNull());
	}
	public void testIsJNull4() {
		Term t = (Term) (new Query("X = @(true)")).oneSolution().get("X");
		assertFalse("@(true) . isJNull() fails", t.isJNull());
	}
	public void testIsJNull5() {
		Term t = (Term) (new Query("X = @(false)")).oneSolution().get("X");
		assertFalse("@(false) . isJNull() fails", t.isJNull());
	}
	public void testIsJTrue1() {
		Term t = (Term) (new Query("X = @(true)")).oneSolution().get("X");
		assertTrue("@(true) . isJTrue() succeeds", t.isJTrue());
	}
	public void testIsJTrue2() {
		Term t = (Term) (new Query("X = @(3)")).oneSolution().get("X");
		assertFalse("@(3) . isJTrue() fails", t.isJTrue());
	}
	public void testIsJTrue3() {
		Term t = (Term) (new Query("X = _")).oneSolution().get("X");
		assertFalse("_ . isJTrue() fails", t.isJTrue());
	}
	public void testIsJTrue4() {
		Term t = (Term) (new Query("X = @(false)")).oneSolution().get("X");
		assertFalse("@(false) . isJTrue() fails", t.isJTrue());
	}
	public void testIsJVoid1() {
		Term t = (Term) (new Query("X = @(void)")).oneSolution().get("X");
		assertTrue("@(void) . isJVoid() succeeds", t.isJVoid());
	}
	public void testIsJVoid2() {
		Term t = (Term) (new Query("X = @(3)")).oneSolution().get("X");
		assertFalse("@(3) . isJVoid() fails", t.isJVoid());
	}
	public void testIsJVoid3() {
		Term t = (Term) (new Query("X = _")).oneSolution().get("X");
		assertFalse("_ . isJVoid() fails", t.isJVoid());
	}
	public void testTypeName1() {
		assertEquals("Y = foo binds Y to an Atom", ((Term) Query.oneSolution("Y = foo").get("Y")).typeName(), "Atom");
	}
	public void testTypeName2() {
		assertEquals("Y = 3.14159 binds Y to a Float", ((Term) Query.oneSolution("Y = 3.14159").get("Y")).typeName(), "Float");
	}
	public void testTypeName4() {
		assertEquals("Y = 6 binds Y to an Integer", ((Term) Query.oneSolution("Y = 6").get("Y")).typeName(), "Integer");
	}
	public void testTypeName5() {
		assertEquals("Y = _ binds Y to a Variable", ((Term) Query.oneSolution("Y = _").get("Y")).typeName(), "Variable");
	}
	public void testTypeName3() {
		assertEquals("Y = f(x) binds Y to a Compound", ((Term) Query.oneSolution("Y = f(x)").get("Y")).typeName(), "Compound");
	}
	public void testGoalWithModulePrefix1() {
		String goal = "jpl:jpl_modifier_bit(volatile,I)";
		assertTrue(goal + " binds I to an integer", ((Term) Query.oneSolution(goal).get("I")).isInteger());
	}
	public void testGoalWithModulePrefix2() {
		String goal = "user:length([],0)";
		assertTrue(goal + " succeeds", Query.hasSolution(goal));
	}
	public void testGoalWithModulePrefix3() {
		try {
			(new Query("3:length([],0)")).hasSolution();
			// shouldn't get to here
			fail("(new Query(\"3:length([],0)\")).hasSolution() didn't throw exception");
		} catch (jpl.PrologException e) {
			// correct exception class, but is it correct in detail?
			if (e.term().hasFunctor("error", 2) && e.term().arg(1).hasFunctor("type_error", 2) && e.term().arg(1).arg(1).hasFunctor("atom", 0)) {
				// OK: an appropriate exception was thrown
			} else {
				fail("(new Query(\"3:length([],0)\")).hasSolution() threw incorrect PrologException: " + e);
			}
		} catch (Exception e) {
			fail("(new Query(\"3:length([],0)\")).hasSolution() threw wrong class of exception: " + e);
		}
	}
	public void testGoalWithModulePrefix4() {
		try {
			(new Query("_:length([],0)")).hasSolution();
			// shouldn't get to here
			fail("bad (unbound) module prefix");
		} catch (jpl.PrologException e) {
			// correct exception class, but is it correct in detail?
			if (e.term().hasFunctor("error", 2) && e.term().arg(1).hasFunctor("instantiation_error", 0)) {
				// OK: an appropriate exception was thrown
			} else {
				fail("(new Query(\"_:length([],0)\")).hasSolution() threw incorrect PrologException: " + e);
			}
		} catch (Exception e) {
			fail("(new Query(\"_:length([],0)\")).hasSolution() threw wrong class of exception: " + e);
		}
	}
	public void testGoalWithModulePrefix5() {
		try {
			(new Query("f(x):length([],0)")).hasSolution();
			// shouldn't get to here
			fail("bad (compound) module prefix");
		} catch (jpl.PrologException e) {
			// correct exception class, but is it correct in detail?
			if (e.term().hasFunctor("error", 2) && e.term().arg(1).hasFunctor("type_error", 2) && e.term().arg(1).arg(1).hasFunctor("atom", 0)) {
				// OK: an appropriate exception was thrown
			} else {
				fail("(new Query(\"f(x):length([],0)\")).hasSolution() threw incorrect PrologException: " + e);
			}
		} catch (Exception e) {
			fail("(new Query(\"f(x):length([],0)\")).hasSolution() threw wrong class of exception: " + e);
		}
	}
	public void testGoalWithModulePrefix6() {
		try {
			(new Query("no_such_module:no_such_predicate(0)")).hasSolution();
			// shouldn't get to here
			fail("bad (nonexistent) module prefix");
		} catch (jpl.PrologException e) {
			// correct exception class, but is it correct in detail?
			if (e.term().hasFunctor("error", 2) && e.term().arg(1).hasFunctor("existence_error", 2) && e.term().arg(1).arg(1).hasFunctor("procedure", 0)) {
				// OK: an appropriate exception was thrown
			} else {
				fail("(new Query(\"f(x):length([],0)\")).hasSolution() threw incorrect PrologException: " + e);
			}
		} catch (Exception e) {
			fail("(new Query(\"f(x):length([],0)\")).hasSolution() threw wrong class of exception: " + e);
		}
	}
	//	public void testFetchCyclicTerm(){
	//		assertTrue((new Query("X=f(X)")).hasSolution());
	//	}
	public void testFetchLongList0() {
		assertTrue((new Query("findall(foo(N),between(0,10,N),L)")).hasSolution());
	}
	public void testFetchLongList1() {
		assertTrue((new Query("findall(foo(N),between(0,100,N),L)")).hasSolution());
	}
	public void testFetchLongList2() {
		assertTrue((new Query("findall(foo(N),between(0,1000,N),L)")).hasSolution());
	}
	public void testFetchLongList2c() {
		assertTrue((new Query("findall(foo(N),between(0,1023,N),L)")).hasSolution());
	}
	public void testFetchLongList2a() {
		assertTrue((new Query("findall(foo(N),between(0,2000,N),L)")).hasSolution());
	}
	//	public void testFetchLongList2b() {
	//		assertTrue((new Query("findall(foo(N),between(0,3000,N),L)")).hasSolution());
	//	}
	//	public void testFetchLongList3() {
	//		assertTrue((new Query("findall(foo(N),between(0,10000,N),L)")).hasSolution());
	//	}
	public void testUnicode0() {
		assertTrue(Query.hasSolution("atom_codes(?,[32])", new Term[] { new Atom(" ") }));
	}
	public void testUnicode0a() {
		assertTrue(Query.hasSolution("atom_codes(?,[32])", new Term[] { new Atom("\u0020") }));
	}
	public void testUnicode0b() {
		assertTrue(Query.hasSolution("atom_codes(?,[0])", new Term[] { new Atom("\u0000") }));
	}
	public void testUnicode0c() {
		assertTrue(Query.hasSolution("atom_codes(?,[1])", new Term[] { new Atom("\u0001") }));
	}
	public void testUnicode0d() {
		assertTrue(Query.hasSolution("atom_codes(?,[127])", new Term[] { new Atom("\u007F") }));
	}
	public void testUnicode0e() {
		assertTrue(Query.hasSolution("atom_codes(?,[128])", new Term[] { new Atom("\u0080") }));
	}
	public void testUnicode0f() {
		assertTrue(Query.hasSolution("atom_codes(?,[255])", new Term[] { new Atom("\u00FF") }));
	}
	public void testUnicode0g() {
		assertTrue(Query.hasSolution("atom_codes(?,[256])", new Term[] { new Atom("\u0100") }));
	}
	public void testUnicode1() {
		assertTrue(Query.hasSolution("atom_codes(?,[0,127,128,255])", new Term[] { new Atom("\u0000\u007F\u0080\u00FF") }));
	}
	public void testUnicode2() {
		assertTrue(Query.hasSolution("atom_codes(?,[256,32767,32768,65535])", new Term[] { new Atom("\u0100\u7FFF\u8000\uFFFF") }));
	}
	public void testStringXput1() {
		Term a = (Term) (Query.oneSolution("string_concat(foo,bar,S)").get("S"));
		assertTrue(a.name().equals("foobar"));
	}
	public void testStringXput2() {
		String s1 = "\u0000\u007F\u0080\u00FF";
		String s2 = "\u0100\u7FFF\u8000\uFFFF";
		String s = s1 + s2;
		Term a1 = new Atom(s1);
		Term a2 = new Atom(s2);
		Term a = (Term) (Query.oneSolution("string_concat(?,?,S)", new Term[] { a1, a2 }).get("S"));
		assertEquals(a.name(), s);
	}
	//	public void testMaxInteger1(){
	//		assertEquals(((Term)(Query.oneSolution("current_prolog_flag(max_integer,I)").get("I"))).longValue(), java.lang.Long.MAX_VALUE); // i.e. 9223372036854775807L
	//	}
	//	public void testSingleton1() {
	//		assertTrue(Query.hasSolution("style_check(-singleton),consult('test_singleton.pl')"));
	//	}
	public void testStaticQueryInvalidSourceText2() {
		String goal = "p(]";
		try {
			Query.hasSolution(goal);
		} catch (jpl.PrologException e) {
			if (e.term().hasFunctor("error", 2) && e.term().arg(1).hasFunctor("syntax_error", 1) && e.term().arg(1).arg(1).hasFunctor("cannot_start_term", 0)) {
				// OK: an appropriate exception was thrown
			} else {
				fail("Query.hasSolution(" + goal + ") threw incorrect PrologException: " + e);
			}
		} catch (Exception e) {
			fail("Query.hasSolution(" + goal + ") threw wrong class of exception: " + e);
		}
	}
	public void testStaticQueryInvalidSourceText1() {
		String goal = "bad goal";
		try {
			Query.hasSolution(goal);
		} catch (jpl.PrologException e) {
			if (e.term().hasFunctor("error", 2) && e.term().arg(1).hasFunctor("syntax_error", 1) && e.term().arg(1).arg(1).hasFunctor("operator_expected", 0)) {
				// OK: an appropriate exception was thrown
			} else {
				fail("Query.hasSolution(" + goal + ") threw incorrect PrologException: " + e);
			}
		} catch (Exception e) {
			fail("Query.hasSolution(" + goal + ") threw wrong class of exception: " + e);
		}
	}
	public void testStaticQueryNSolutions1() {
		String goal = "member(X, [0,1,2,3,4,5,6,7,8,9])";
		int n = 5;
		assertTrue("Query.nSolutions(" + goal + ", " + n + ") returns " + n + " solutions", Query.nSolutions(goal, n).length == n);
	}
	public void testStaticQueryNSolutions2() {
		String goal = "member(X, [0,1,2,3,4,5,6,7,8,9])";
		int n = 0;
		assertTrue("Query.nSolutions(" + goal + ", " + n + ") returns " + n + " solutions", Query.nSolutions(goal, n).length == n);
	}
	public void testStaticQueryNSolutions3() {
		String goal = "member(X, [0,1,2,3,4,5,6,7,8,9])";
		int n = 20;
		assertTrue("Query.nSolutions(" + goal + ", " + n + ") returns 10 solutions", Query.nSolutions(goal, n).length == 10);
	}
	public void testStaticQueryAllSolutions1() {
		String goal = "member(X, [0,1,2,3,4,5,6,7,8,9])";
		assertTrue("Query.allSolutions(" + goal + ") returns 10 solutions", Query.allSolutions(goal).length == 10);
	}
	public void testStaticQueryHasSolution1() {
		String goal = "memberchk(13, [?,?,?])";
		Term[] params = new Term[] { new Integer(12), new Integer(13), new Integer(14) };
		assertTrue(Query.hasSolution(goal, params));
	}
	public void testStaticQueryHasSolution2() {
		String goal = "memberchk(23, [?,?,?])";
		Term[] params = new Term[] { new Integer(12), new Integer(13), new Integer(14) };
		assertFalse(Query.hasSolution(goal, params));
	}
	public void testUtilListToTermArray1() {
		String goal = "T = [a,b,c]";
		Term list = (Term) Query.oneSolution(goal).get("T");
		Term[] array = Util.listToTermArray(list);
		assertTrue(array[2].isAtom() && array[2].name().equals("c"));
	}
	public void testTermToTermArray1() {
		String goal = "T = [a,b,c]";
		Term list = (Term) Query.oneSolution(goal).get("T");
		Term[] array = list.toTermArray();
		assertTrue(array[2].isAtom() && array[2].name().equals("c"));
	}
	public void testJRef1() {
		// System.out.println("java.library.path=" + System.getProperties().get("java.library.path"));
		// System.out.println("jpl.c version = " + jpl.fli.Prolog.get_c_lib_version());
		int i = 76543;
		Integer I = new Integer(i);
		Query q = new Query("jpl_call(?,intValue,[],I2)", new Term[] { new JRef(I) });
		Term I2 = (Term) q.oneSolution().get("I2");
		assertTrue(I2.isInteger() && I2.intValue() == i);
	}
	public void testBerhhard1() {
		assertTrue(Query.allSolutions( "consult(library('lists'))" ).length == 1);
	}
}
