package jpl.util;


public class Test1
	{

	public static String booleans_to_string( boolean p0, boolean p1)
		{
		return "{" + p0 + "," + p1 + "}";
		}

	public static String chars_to_string( char p0, char p1)
		{
		return "{" + p0 + "," + p1 + "}";
		}

	public static String bytes_to_string( byte p0, byte p1)
		{
		return "{" + p0 + "," + p1 + "}";
		}

	public static String shorts_to_string( short p0, short p1)
		{
		return "{" + p0 + "," + p1 + "}";
		}

	public static String ints_to_string( int p0, int p1)
		{
		return "{" + p0 + "," + p1 + "}";
		}

	public static String longs_to_string( long p0, long p1)
		{
		return "{" + p0 + "," + p1 + "}";
		}

	public static String floats_to_string( float p0, float p1)
		{
		return "{" + p0 + "," + p1 + "}";
		}

	public static String doubles_to_string( double p0, double p1)
		{
		return "{" + p0 + "," + p1 + "}";
		}


	public static String boolean_to_string( boolean p)
		{
		return "{" + p + "}";
		}

	public static String char_to_string( char p)
		{
		return "{" + p + "}";
		}

	public static String byte_to_string( byte p)
		{
		return "{" + p + "}";
		}

	public static String short_to_string( short p)
		{
		return "{" + p + "}";
		}

	public static String int_to_string( int p)
		{
		return "{" + p + "}";
		}

	public static String long_to_string( long p)
		{
		return "{" + p + "}";
		}

	public static String float_to_string( float p)
		{
		return "{" + p + "}";
		}

	public static String double_to_string( double p)
		{
		return "{" + p + "}";
		}


	public static boolean boolean_1()
		{
		return false;
		}

	public static boolean boolean_2()
		{
		return true;
		}

	public static char char_1()
		{
		return '\uFFFF';
		}

	public static byte byte_1()
		{
		return 127;
		}

	public static short short_1()
		{
		return 32767;
		}

	public static int int_1()
		{
		return 0x01234567;		/*  19088743 */
		}

	public static int int_2()
		{
		return 0x89ABCDEF;		/*  -1985229329  */
		}

	public static long long_1()
		{
		return 0x0123456789ABCDEFL;	/*  jlong(19088743,-1985229329)  */
		}

	public static long long_2()
		{
		return 0xFFFFFFFFFFFFFFFEL;	/*  jlong(-1,-2)  */
		}

	public static long long_3()
		{
		return 0x0000000100000002L;	/*  jlong(1,2)  */
		}

	public static long long_4()
		{
		return 0xFFFFFFFF00000000L;	/*  jlong(-1,0)  */
		}

	public static long long_5()
		{
		return 0x00000000FFFFFFFFL;	/*  -1  */
		}

	public static long long_6()
		{
		return 0x0000000080000000L;	/*  -2147483648  */
		}

	public static long long_7()
		{
		return 0x000000007FFFFFFFL;	/*  2147483647  */
		}

	public static float float_1()
		{
		return 12345.6789F;
		}

	public static double double_1()
		{
		return 12345.6789D;
		}

	public static double double_1a()
		{
		return 2.3456789e+100D;
		}

	public static double double_1b()
		{
		return 3.456789e-100D;
		}

	public static double double_1c()
		{
		return 0.0D;
		}

	public static double double_2()
		{
		return Double.MIN_VALUE;
		}

	public static double double_3()
		{
		return Double.MAX_VALUE;
		}

	public static double double_4()
		{
		return Double.NEGATIVE_INFINITY;
		}

	public static double double_5()
		{
		return Double.POSITIVE_INFINITY;
		}

	public static double double_6()
		{
		return Double.NaN;
		}

	}


