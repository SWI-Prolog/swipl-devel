package jpl.test;
import jpl.Atom;
import jpl.Query;
import jpl.Term;
import jpl.Variable;

public class Family extends Thread {

	int id; // client thread id
	private static final int delay = 0;

	Family(int i) {
		this.id = i;
	}

	public static void main(String argv[]) {

		Query q1 = new Query("consult", new Term[] { new Atom("jpl/test/family.pl")});
		System.err.println("consult " + (q1.hasSolution() ? "succeeded" : "failed"));

		for (int i = 0; i < 20; i++) {
			System.out.println("spawning client[" + i + "]");
			new Family(i).start();
		}

	}

	public void run() {
		java.util.Hashtable solution;
		Variable X = new Variable("X");

		//--------------------------------------------------

		Query q2 = new Query("child_of", new Term[] { new Atom("joe"), new Atom("ralf")});

		System.err.println("child_of(joe,ralf) is " + (q2.hasSolution() ? "provable" : "not provable"));

		new Query("sleep", new Term[] { new jpl.Integer(delay)}).hasSolution();

		//--------------------------------------------------

		Query q3 = new Query("descendent_of", new Term[] { new Atom("steve"), new Atom("ralf")});

		System.err.println("descendent_of(steve,ralf) is " + (q3.hasSolution() ? "provable" : "not provable"));

		new Query("sleep", new Term[] { new jpl.Integer(delay)}).hasSolution();

		//--------------------------------------------------

		Query q4 = new Query("descendent_of", new Term[] { X, new Atom("ralf")});

		solution = q4.oneSolution();

		System.err.println("first solution of descendent_of(X, ralf)");
		System.err.println("X = " + solution.get(X.name));

		new Query("sleep", new Term[] { new jpl.Integer(delay)}).hasSolution();

		//--------------------------------------------------

		java.util.Hashtable[] solutions = q4.allSolutions();

		System.err.println("all solutions of descendent_of(X, ralf)");
		for (int i = 0; i < solutions.length; i++) {
			System.err.println("X = " + solutions[i].get(X.name));
		}

		new Query("sleep", new Term[] { new jpl.Integer(delay)}).hasSolution();

		//--------------------------------------------------

		System.err.println("each solution of descendent_of(X, ralf)");
		while (q4.hasMoreSolutions()) {
			solution = q4.nextSolution();
			System.err.println("X = " + solution.get(X.name));
		}

		new Query("sleep", new Term[] { new jpl.Integer(delay)}).hasSolution();

		//--------------------------------------------------

		Variable Y = new Variable("Y");

		Query q5 = new Query("descendent_of", new Term[] { X, Y });

		System.err.println(id + ": each solution of descendent_of(X, Y)");
		while (q5.hasMoreSolutions()) {
			solution = q5.nextSolution();
			System.err.println(id + ": X = " + solution.get(X.name) + ", Y = " + solution.get(Y.name));

			new Query("sleep", new Term[] { new jpl.Integer(delay)}).hasSolution();
		}

	}

}
