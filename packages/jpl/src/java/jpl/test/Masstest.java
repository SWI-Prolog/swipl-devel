package jpl.test;

import jpl.Query;
import jpl.fli.Prolog;

public class Masstest extends Thread {
	public static void main(String[] args) {
		//		String[] dia = Prolog.get_default_init_args();
		//		String s = "default init args: ";
		//		for (int i = 0; i < dia.length; i++) {
		//			s += " " + dia[i];
		//		}
		//		System.out.println(s);
		//
		// Prolog.set_default_init_args(new String[] { "libpl.dll", "-f", "none", "-g", "true", "-q" });
		// empirically, needs this at least:
		// Prolog.set_default_init_args(new String[] { "libpl.dll" });
		// Prolog.set_default_init_args(new String[] { "pl" });
		//
		// (new Query("assert(diagnose_declaration(_,_,_,[not,a,real,error]))")).hasSolution();
		//
		int STUDENTSNUMBER = 5;
		Masstest[] threads = new Masstest[STUDENTSNUMBER];
		for (int i = 0; i < STUDENTSNUMBER; i++) {
			threads[i] = new Masstest();
			threads[i].start();
		}
	}
	public void predQuery() {
		String st = "diagnose_declaration(1,[(sp, 'prefix', [('arg1', '+', 'list', 'Liste1'), ('arg2', '+', 'list', 'Liste2')])], DecMap, ErrorList)";
		Query stQuery = new Query(st);
		String errString = stQuery.oneSolution().get("ErrorList").toString();
		System.out.println("errString=" + errString);
	}
	public void run() {
		try {
			predQuery();
		} catch (Exception e) {
			System.err.println("ERROR: " + e);
		}
	}
}
