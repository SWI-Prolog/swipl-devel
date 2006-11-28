package jpl.test;

import jpl.Query;

public class SyntaxError {
	public static void main(String argv[]) {
		Query q = new Query("syntax)error");
		System.err.println(q.hasSolution() ? "yes" : "no");
	}
}
