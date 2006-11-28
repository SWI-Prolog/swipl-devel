package jpl.util;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;

public class Getenv
	{

	public static void main(String args[])
		{

		try	{
			getenv();
			}
		catch (java.io.IOException e) { }
		}

	public static void getenv()
		throws java.io.IOException, java.io.UnsupportedEncodingException
		{
		Runtime rt = Runtime.getRuntime();

		String a[] = new String[3];
		a[0] = "CMD";
		a[1] = "/C";
		a[2] = "SET";

		Process p = rt.exec(a);

		InputStream is = p.getInputStream();

		InputStreamReader isr = new InputStreamReader(is,"UTF8");

		BufferedReader br = new BufferedReader(isr);

		getenv1(br);
		}

	static void getenv1(BufferedReader br)
		throws java.io.IOException
		{

		String s = br.readLine();

		if ( s != null )
			{
			System.out.println(s);
			getenv1(br);
			}
		}
	}

