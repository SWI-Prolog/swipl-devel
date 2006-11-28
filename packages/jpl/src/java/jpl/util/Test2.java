package jpl.util;

public class Test2 {
	public static boolean	boolean_field;
	public static char		char_field;
	public static byte		byte_field;
	public static short		short_field;
	public static int		int_field;
	public static long		long_field;
	public static float		float_field;
	public static double	double_field;
	public static boolean get_boolean_field() {
		return boolean_field;
	}
	public static char get_char_field() {
		return char_field;
	}
	public static byte get_byte_field() {
		return byte_field;
	}
	public static short get_short_field() {
		return short_field;
	}
	public static int get_int_field() {
		return int_field;
	}
	public static long get_long_field() {
		return long_field;
	}
	public static float get_float_field() {
		return float_field;
	}
	public static double get_double_field() {
		return double_field;
	}
	/*-------------------------------------------------------------------------------*/
	public static void set_boolean_field(boolean v) {
		boolean_field = v;
	}
	public static void set_char_field(char v) {
		char_field = v;
	}
	public static void set_byte_field(byte v) {
		byte_field = v;
	}
	public static void set_short_field(short v) {
		short_field = v;
	}
	public static void set_int_field(int v) {
		int_field = v;
	}
	public static void set_long_field(long v) {
		long_field = v;
	}
	public static void set_float_field(float v) {
		float_field = v;
	}
	public static void set_double_field(double v) {
		double_field = v;
	}
	public static String boolean_field_to_string() {
		return "{" + boolean_field + "}";
	}
	public static String char_field_to_string() {
		return "{" + char_field + "}";
	}
	public static String byte_field_to_string() {
		return "{" + byte_field + "}";
	}
	public static String short_field_to_string() {
		return "{" + short_field + "}";
	}
	public static String int_field_to_string() {
		return "{" + int_field + "}";
	}
	public static String long_field_to_string() {
		return "{" + long_field + "}";
	}
	public static String float_field_to_string() {
		return "{" + float_field + "}";
	}
	public static String double_field_to_string() {
		return "{" + double_field + "}";
	}
	public static String echo_2_and_3(Object a1, int a2, int a3) {
		return "a2=" + a2 + ", a3=" + a3;
	}
	public static String set_bytes_to_7(byte[] a, int offset, int count) {
		for (int i = 0; i < count; i++)
			a[offset + i] = 7;
		return "offset=" + offset + ", count=" + count;
	}
	public static String lotsa_args(byte[] a, int a1, int a2, int a3, int a4, int a5, int a6) { // this illustrated a bug in the MS JVM's JNI
		return "a1=" + a1 + ", a2=" + a2 + ", a3=" + a3 + ", a4=" + a4 + ", a5=" + a5 + ", a6=" + a6;
	}
}
