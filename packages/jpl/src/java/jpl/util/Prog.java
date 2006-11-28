package jpl.util;


public class Prog
	{
	public static int i_am_static_1 = 1;

	public int i_am_not_static_2 = 2;

	public static java.lang.String S = "hello";

	public static class Inside
		{
		public int intErnal;
		}

	public static void main(String[] args)
		{
		System.out.println("Hello World");
		}

	public static float div0()
		{
		return (float)(1.234);
		}

	public static String welcome()
		{
		return "Welcome to JSP!";
		}

	public float div1(int d)
		{
		return i_am_static_1/d;
		}

	public double div2( int d)
		{
		return i_am_not_static_2/d;
		}

	public static int add_ints( int i1, int i2, int i3)
		{
		return i1+i2+i3;
		}

	public static float floats_to_float( float f1, float f2, float f3)
		{
		return f1+f2+f3;
		}

	public static double floats_to_double( float f1, float f2, float f3)
		{
		return f1+f2+f3;
		}

	public static String double_to_string( double d)
		{
		return "The answer is " + d;
		}

	}

