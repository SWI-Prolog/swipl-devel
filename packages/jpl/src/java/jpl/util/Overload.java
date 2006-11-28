package jpl.util;

public class Overload {
	static void m1(int a1, long a2) {
	}
	static void m1(long a1, int a2) {
	}
	public static void main(String[] args) {
		m1((long) 0, 0);
	}
}
