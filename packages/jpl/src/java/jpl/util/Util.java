package jpl.util;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.NoSuchElementException;
import java.util.Random;
import java.util.StringTokenizer;

public class Util
	{
	private static Random r = new Random();

	public static String[] dir_to_members( String dir)
		{
		String[] ns = (new File(dir)).list();
		int len = ns.length;
		String[] ps = new String[len];
		for ( int i=0 ; i<len ; i++ )
			{
			try	{
				ps[i] = (new File(dir,ns[i])).getCanonicalPath().toString();
				}
			catch ( IOException e )
				{
				ps[i] = "";
				}
			}
		return ps;
		}

	public static String[] dir_to_member_paths( String dir)
		{
		String[] ns = (new File(dir)).list();
		int len = ns.length;
		String[] ps = new String[len];
		for ( int i=0 ; i<len ; i++ )
			{
			try	{
				ps[i] = (new File(dir,ns[i])).getCanonicalPath();
				}
			catch ( IOException e )
				{
				ps[i] = "";
				}
			}
		return ps;
		}

	public static String[] dir_to_member_names( String dir)
		{
		String[] ns = (new File(dir)).list();
		int len = ns.length;
		String[] ps = new String[len];
		for ( int i=0 ; i<len ; i++ )
			{
			ps[i] = (new File(dir,ns[i])).getName();
			}
		return ps;
		}

	public static int spawn_in_out_err( String command, String infile, String outfile, String errfile)
		{
		Process p;

		try	{
			p = Runtime.getRuntime().exec(command);
			}
		catch ( IOException e)
			{
			return -1;
			}

		try	{
			if ( infile != null )
				{
				( new Xfer(	new FileInputStream(infile),
						p.getOutputStream()
					)).start();
				}
			if ( outfile != null )
				{
				( new Xfer(	p.getInputStream(),
						new FileOutputStream(outfile)
					)).start();
				}
			if ( errfile != null )
				{
				( new Xfer(	p.getErrorStream(),
						new FileOutputStream(errfile)
					)).start();
				}
			return p.waitFor();
			}
		catch ( FileNotFoundException e )
			{
			return -2;
			}
		catch ( InterruptedException e )
			{
			return -4;
			}
		}

	public static int spawn_in_out_err_wait(
		String	command,
		String	infile,
		String	outfile,
		String	errfile,
		boolean	wait)
		{
		Process p;

		try	{
			p = Runtime.getRuntime().exec(command);
			}
		catch ( IOException e)
			{
			return -1;
			}

		try	{
			if ( infile != null )
				{
				( new Xfer(	new FileInputStream(infile),
						p.getOutputStream()
					)).start();
				}
			if ( outfile != null )
				{
				( new Xfer(	p.getInputStream(),
						new FileOutputStream(outfile)
					)).start();
				}
			if ( errfile != null )
				{
				( new Xfer(	p.getErrorStream(),
						new FileOutputStream(errfile)
					)).start();
				}
			if ( wait )
				return p.waitFor();
			else
				return 0;
			}
		catch ( FileNotFoundException e )
			{
			return -2;
			}
		catch ( InterruptedException e )
			{
			return -4;
			}
		}

	public static int spawn( String command)
		{

		return spawn_wait( command, true);
		}

	public static int spawn_wait( String command, boolean wait)
		{
		Process p;

		try	{
			p = java.lang.Runtime.getRuntime().exec(command);
			}
		catch ( IOException e )
			{
			return -77;
			}
		if ( wait )
			{
			try {
				return p.waitFor();
				}
			catch ( InterruptedException e )
				{
				return -78;
				}
			}
		else
			{
			return 0;
			}
		}

	public static String new_scratch_file( String dir)
		{
		return new_scratch_file( dir, "nsf");
		}

	public static String new_scratch_file( String dir, String suffix)
		{
		int	n = (int)(r.nextFloat()*900000+100000);
		File	f = new File( dir, n+"."+suffix);

		if ( f.exists() )
			{
			return new_scratch_file( dir, suffix);
			}
		else
			{
			try	{
				(new FileOutputStream(f)).close();
				if ( f.exists() )
					{
					return f.getCanonicalPath().toString();
					}
				else
					{
					return null;
					}
				}
			catch ( IOException e )
				{
				return null;
				}
			}
		}

	public static String new_scratch_dir( String dir)
		{
		int	n = (int)(r.nextFloat()*900000+100000);
		File	d = new File( dir, n+".nsd");

		if ( d.exists() )
			{
			return new_scratch_dir( dir);
			}
		else
			{
			try	{
				if ( d.mkdir() )
					{
					return d.getCanonicalPath().toString();
					}
				else
					{
					return null;
					}
				}
			catch ( IOException e )
				{
				return null;
				}
			}
		}

	public static String new_scratch_suffix_file( String dir, String suffix)
		{
		int	n = (int)(r.nextFloat()*900000+100000);
		File	f = new File( dir, n+"."+suffix);

		if ( f.exists() )
			{
			return new_scratch_suffix_file( dir, suffix);
			}
		else
			{
			try	{
				(new FileOutputStream(f)).close();
				if ( f.exists() )
					{
					return f.getCanonicalPath().toString();
					}
				else
					{
					return null;
					}
				}
			catch ( IOException e )
				{
				return null;
				}
			}
		}

	public static String new_scratch_suffix_dir( String dir, String suffix)
		{
		int	n = (int)(r.nextFloat()*900000+100000);
		File	d = new File( dir, n+"."+suffix);

		if ( d.exists() )
			{
			return new_scratch_suffix_dir( dir, suffix);
			}
		else
			{
			try	{
				if ( d.mkdir() )
					{
					return d.getCanonicalPath().toString();
					}
				else
					{
					return null;
					}
				}
			catch ( IOException e )
				{
				return null;
				}
			}
		}

	public static boolean create_file( String file)
		{
		File	f = new File( file);

		try	{
			(new FileOutputStream(f)).close();
			return f.exists();
			}
		catch ( IOException e )
			{
			return false;
			}
		}

	public static boolean create_dir( String dir)
		{
		try	{
			return (new File(dir)).mkdir();
			}
		catch ( SecurityException e )
			{
			return false;
			}
		}

	public static boolean file_exists( String f)
		{
		return (new File(f)).exists();
		}

	public static boolean file_can_read( String f)
		{
		return (new File(f)).canRead();
		}

	public static boolean file_can_write( String f)
		{
		return (new File(f)).canWrite();
		}

	public static boolean file_is_file( String f)
		{
		return (new File(f)).isFile();
		}

	public static boolean file_is_dir( String f)
		{
		return (new File(f)).isDirectory();
		}

	public static long file_last_modified( String f)
		{
		return (new File(f)).lastModified();
		}

	public static long file_to_length( String f)
		{
		return (new File(f)).length();
		}

	public static boolean delete_file( String f)
		{
		try	{
			return (new File(f)).delete();
			}
		catch ( SecurityException e )
			{
			return false;
			}
		}

	public static String[] classpath_parts()
		{
		String cp = java.lang.System.getProperty("java.class.path");
		StringTokenizer p = new StringTokenizer( cp, File.pathSeparator);
		String a[] = new String[p.countTokens()];
		int i = 0;
		String s;
		String[] r;

		try	{
			while ( p.hasMoreTokens() )
				{
				s = (new File(p.nextToken())).getCanonicalPath().toString();
				if ( ! strings_contains_string( a, i, s) )
					{
					a[i++] = s;
					}
				}
			}
		catch ( NoSuchElementException e )
			{
			return null;
			}
		catch ( IOException e )
			{
			return null;
			}
		finally	{
			r = new String[i];
			java.lang.System.arraycopy( a, 0, r, 0, i);
			}
		return r;
		}

	private static boolean strings_contains_string( String[] ss, int n, String s)
		{
		int i;

		for ( i=0 ; i<n ; i++ )
			{
			if ( ss[i].equals(s) )
				return true;
			}
		return false;
		}

	public static byte[] filename_to_byte_array( String f)
		{
		try	{
			FileInputStream s = new FileInputStream( f);
			int length = s.available();
			byte[] buf = new byte[length];
			s.read( buf);
			s.close();		// to release file for e.g. deletion...
			return buf;
			}
		catch ( FileNotFoundException e )
			{
			return null;
			}
		catch ( IOException e )
			{
			return null;
			}
		}

	public static boolean rename_file_to_file( String n1, String n2)
		{
		try	{
			File f1 = new File(n1);
			File f2 = new File(n2);
			return	( f1!=null && f2!=null
				? f1.renameTo(f2)
				: false
				);
			}
		catch ( SecurityException e )
			{
			return false;
			}
		}

	}

