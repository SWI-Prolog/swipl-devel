package jpl.test;

public class Garbo {
	public static int created = 0;
	public static int destroyed = 0;
	//
	public final int i;
	public Garbo( ) {
		this.i = created++;
	}
	protected void finalize() throws Throwable {
		try {
			destroyed++;
			// System.out.println("gced["+i+"]");
		} finally {
			super.finalize();
		}
	}
}
