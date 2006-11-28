package jpl.test;

import jpl.Query;

public class Ga2 {
	public static void main(String argv[]) {
		// Prolog.set_default_init_args(new String[] { "libpl.dll", "-f", "D:/pcm/bin/pcm.ini", "-g", "pcm_2000" });
		(new Query("current_prolog_flag(K,V),write(K-V),nl,fail")).oneSolution();
	}
}
