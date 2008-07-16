package jpl.test;

import jpl.Compound;
import jpl.Query;
import jpl.Term;

// This class contains members which support those tests which are performed from Prolog.
// See also TestJUnit
public class Test {
	public Test() {
	}
	public Test(Term t) {
		this.termFromConstructor = t;
	}
	public Term					termFromConstructor;
	//
	public static boolean		fieldStaticBoolean;
	public static final boolean	fieldStaticBoolean1	= false;
	public static final boolean	fieldStaticBoolean2	= true;
	//
	public static char			fieldStaticChar;
	public static final char	fieldStaticChar1	= '\u0000';
	public static final char	fieldStaticChar2	= '\uFFFF';
	//
	public static byte			fieldStaticByte;
	public static final byte	fieldStaticByte1	= -(1 << 7);
	public static final byte	fieldStaticByte2	= -1;
	public static final byte	fieldStaticByte3	= 0;
	public static final byte	fieldStaticByte4	= 1;
	public static final byte	fieldStaticByte5	= (1 << 7) - 1;
	//
	public static short			fieldStaticShort;
	public static final short	fieldStaticShort1	= -(1 << 15);
	public static final short	fieldStaticShort2	= -(1 << 7);
	public static final short	fieldStaticShort3	= -1;
	public static final short	fieldStaticShort4	= 0;
	public static final short	fieldStaticShort5	= 1;
	public static final short	fieldStaticShort6	= (1 << 7) - 1;
	public static final short	fieldStaticShort7	= (1 << 15) - 1;
	//
	public static int			fieldStaticInt;
	public static final int		fieldStaticInt1		= -(1 << 31);
	public static final int		fieldStaticInt2		= -(1 << 15);
	public static final int		fieldStaticInt3		= -(1 << 7);
	public static final int		fieldStaticInt4		= -1;
	public static final int		fieldStaticInt5		= 0;
	public static final int		fieldStaticInt6		= 1;
	public static final int		fieldStaticInt7		= (1 << 7) - 1;
	public static final int		fieldStaticInt8		= (1 << 15) - 1;
	public static final int		fieldStaticInt9		= (1 << 31) - 1;
	//
	public static long			fieldStaticLong;
	public static final long	fieldStaticLong1	= -(1 << 63);
	public static final long	fieldStaticLong2	= -(1 << 31);
	public static final long	fieldStaticLong3	= -(1 << 15);
	public static final long	fieldStaticLong4	= -(1 << 7);
	public static final long	fieldStaticLong5	= -1;
	public static final long	fieldStaticLong6	= 0;
	public static final long	fieldStaticLong7	= 1;
	public static final long	fieldStaticLong8	= (1 << 7) - 1;
	public static final long	fieldStaticLong9	= (1 << 15) - 1;
	public static final long	fieldStaticLong10	= (1 << 31) - 1;
	public static final long	fieldStaticLong11	= (1 << 63) - 1;
	//
	public static float			fieldStaticFloat;
	public static final float	fieldStaticFloat1	= 12345.6789F;
	public static final float	fieldStaticFloat2	= 3.4e+38F;							// nearly MAX_VALUE
	public static final float	fieldStaticFloat3	= 1.4e-45F;							// nearly MIN_VALUE
	public static final float	fieldStaticFloat4	= 0.0F;
	public static final float	fieldStaticFloat5	= java.lang.Float.MIN_VALUE;
	public static final float	fieldStaticFloat6	= java.lang.Float.MAX_VALUE;
	public static final float	fieldStaticFloat7	= java.lang.Float.NEGATIVE_INFINITY;
	public static final float	fieldStaticFloat8	= java.lang.Float.POSITIVE_INFINITY;
	public static final float	fieldStaticFloat9	= java.lang.Float.NaN;
	//
	public static double		fieldStaticDouble;
	public static final double	fieldStaticDouble1	= 12345.6789D;
	public static final double	fieldStaticDouble2	= 2.3456789e+100D;
	public static final double	fieldStaticDouble3	= 3.456789e-100D;
	public static final double	fieldStaticDouble4	= 0.0D;
	public static final double	fieldStaticDouble5	= Double.MIN_VALUE;
	public static final double	fieldStaticDouble6	= Double.MAX_VALUE;
	public static final double	fieldStaticDouble7	= Double.NEGATIVE_INFINITY;
	public static final double	fieldStaticDouble8	= Double.POSITIVE_INFINITY;
	public static final double	fieldStaticDouble9	= Double.NaN;
	//
	public static Object[]		fieldStaticObjectArray; // can assign e.g. String[]
	public static long[]		fieldStaticLongArray; // cannot assign e.g. int[]
	//
	public static long fac(long n) { // complements jpl:jpl_test_fac(+integer,-integer)
		if (n == 1) {
			return 1;
		} else if (n > 1) {
			// return n * ((Integer) new Query(new Compound("jpl_test_fac", new Term[] { new Integer(n - 1), new Variable("F") })).oneSolution().get("F")).intValue();
			return n * ((jpl.Integer) Query.oneSolution("jpl_test_fac(?,F)", new Term[] {new jpl.Integer(n-1)}).get("F")).longValue();
		} else {
			return 0;
		}
	}
	static void packageMethod() { // not callable via JPL
		return;
	}
	public static void publicMethod() {
		return;
	}
	protected static void protectedMethod() { // not callable via JPL
		return;
	}
	private static void privateMethod() { // not callable via JPL
		return;
	}
	public boolean				fieldInstanceBoolean;
	public final boolean		fieldInstanceBoolean1	= false;
	public final boolean		fieldInstanceBoolean2	= true;
	public byte					fieldInstanceByte;
	public final byte			fieldInstanceByte1		= -(1 << 7);
	public final byte			fieldInstanceByte2		= -1;
	public final byte			fieldInstanceByte3		= 0;
	public final byte			fieldInstanceByte4		= 1;
	public final byte			fieldInstanceByte5		= (1 << 7) - 1;
	public char					fieldInstanceChar;
	public final char			fieldInstanceChar1		= '\u0000';
	public final char			fieldInstanceChar2		= '\uFFFF';
	public double				fieldInstanceDouble;
	public final double			fieldInstanceDouble1	= 12345.6789D;
	public final double			fieldInstanceDouble2	= 2.3456789e+100D;
	public final double			fieldInstanceDouble3	= 3.456789e-100D;
	public final double			fieldInstanceDouble4	= 0.0D;
	public final double			fieldInstanceDouble5	= Double.MIN_VALUE;
	public final double			fieldInstanceDouble6	= Double.MAX_VALUE;
	public final double			fieldInstanceDouble7	= Double.NEGATIVE_INFINITY;
	public final double			fieldInstanceDouble8	= Double.POSITIVE_INFINITY;
	public final double			fieldInstanceDouble9	= Double.NaN;
	public float				fieldInstanceFloat;
	public final float			fieldInstanceFloat1		= 12345.6789F;
	public final float			fieldInstanceFloat2		= 3.4e+38F;
	public final float			fieldInstanceFloat3		= 1.4e-45F;
	public final float			fieldInstanceFloat4		= 0.0F;
	public final float			fieldInstanceFloat5		= java.lang.Float.MIN_VALUE;
	public final float			fieldInstanceFloat6		= java.lang.Float.MAX_VALUE;
	public final float			fieldInstanceFloat7		= java.lang.Float.NEGATIVE_INFINITY;
	public final float			fieldInstanceFloat8		= java.lang.Float.POSITIVE_INFINITY;
	public final float			fieldInstanceFloat9		= java.lang.Float.NaN;
	public int					fieldInstanceInt;
	public final int			fieldInstanceInt1		= -(1 << 31);
	public final int			fieldInstanceInt2		= -(1 << 15);
	public final int			fieldInstanceInt3		= -(1 << 7);
	public final int			fieldInstanceInt4		= -1;
	public final int			fieldInstanceInt5		= 0;
	public final int			fieldInstanceInt6		= 1;
	public final int			fieldInstanceInt7		= (1 << 7) - 1;
	public final int			fieldInstanceInt8		= (1 << 15) - 1;
	public final int			fieldInstanceInt9		= (1 << 31) - 1;
	public long					fieldInstanceLong;
	public final long			fieldInstanceLong1		= -(1 << 63);
	public final long			fieldInstanceLong10		= (1 << 31) - 1;
	public final long			fieldInstanceLong11		= (1 << 63) - 1;
	public final long			fieldInstanceLong2		= -(1 << 31);
	public final long			fieldInstanceLong3		= -(1 << 15);
	public final long			fieldInstanceLong4		= -(1 << 7);
	public final long			fieldInstanceLong5		= -1;
	public final long			fieldInstanceLong6		= 0;
	public final long			fieldInstanceLong7		= 1;
	public final long			fieldInstanceLong8		= (1 << 7) - 1;
	public final long			fieldInstanceLong9		= (1 << 15) - 1;
	public short				fieldInstanceShort;
	public final short			fieldInstanceShort1		= -(1 << 15);
	public final short			fieldInstanceShort2		= -(1 << 7);
	public final short			fieldInstanceShort3		= -1;
	public final short			fieldInstanceShort4		= 0;
	public final short			fieldInstanceShort5		= 1;
	public final short			fieldInstanceShort6		= (1 << 7) - 1;
	public final short			fieldInstanceShort7		= (1 << 15) - 1;
	//
	public Term					term;															// obsolete
	public static Term			staticTerm;
	public Term					instanceTerm;
	//
	// for testing accessibility of non-public fields:
	static boolean				fieldPackageStaticBoolean;
	protected static boolean	fieldProtectedStaticBoolean;
	private static boolean		fieldPrivateStaticBoolean;
	//
	// for testing update of final field:
	public static final int		fieldStaticFinalInt		= 7;
	//
	// for testing passing general terms in from Prolog:
	public static Term			fieldStaticTerm;
	public Term					fieldInstanceTerm;
	public static boolean methodStaticTerm(Term t) {
		return t != null;
	}
	public boolean methodInstanceTerm(Term t) {
		return t != null;
	}
	public static Term methodStaticEchoTerm(Term t) {
		return t;
	}
	public static boolean methodStaticEchoBoolean(boolean v) {
		return v;
	}
	public static char methodStaticEchoChar(char v) {
		return v;
	}
	public static byte methodStaticEchoByte(byte v) {
		return v;
	}
	public static short methodStaticEchoShort(short v) {
		return v;
	}
	public static int methodStaticEchoInt(int v) {
		return v;
	}
	public static long methodStaticEchoLong(long v) {
		return v;
	}
	public static float methodStaticEchoFloat(float v) {
		return v;
	}
	public static double methodStaticEchoDouble(double v) {
		return v;
	}
	public Term methodInstanceTermEcho(Term t) {
		return t;
	}
	public static boolean methodStaticTermIsJNull(Term t) {
		return t.hasFunctor("@", 1) && t.arg(1).hasFunctor("null", 0);
	}
	public boolean methodInstanceTermIsJNull(Term t) {
		return t.hasFunctor("@", 1) && t.arg(1).hasFunctor("null", 0);
	}
	public static void hello() {
		System.out.println("hello");
	}
	public static boolean[] newArrayBooleanFromValue(boolean v) {
		boolean[] a = new boolean[1];
		a[0] = v;
		return a;
	}
	public static byte[] newArrayByteFromValue(byte v) {
		byte[] a = new byte[1];
		a[0] = v;
		return a;
	}
	public static char[] newArrayCharFromValue(char v) {
		char[] a = new char[1];
		a[0] = v;
		return a;
	}
	public static short[] newArrayShortFromValue(short v) {
		short[] a = new short[1];
		a[0] = v;
		return a;
	}
	public static int[] newArrayIntFromValue(int v) {
		int[] a = new int[1];
		a[0] = v;
		return a;
	}
	public static long[] newArrayLongFromValue(long v) {
		long[] a = new long[1];
		a[0] = v;
		return a;
	}
	public static float[] newArrayFloatFromValue(float v) {
		float[] a = new float[1];
		a[0] = v;
		return a;
	}
	public static double[] newArrayDoubleFromValue(double v) {
		double[] a = new double[1];
		a[0] = v;
		return a;
	}
	public static String methodStaticArray(long[] a) {
		return "long[]";
	}
	public static String methodStaticArray(int[] a) {
		return "int[]";
	}
	public static String methodStaticArray(short[] a) {
		return "short[]";
	}
	public static Term wrapTerm(Term in) { // for dmiles 11/Jul/2008
		return new Compound("javaWrap", new Term[] {in});
	}
}
