package jpl.util;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

public class Xfer extends Thread
	{
	private InputStream in;

	private OutputStream out;

	public Xfer( InputStream s1, OutputStream s2)
		{
		in = s1;
		out = s2;
		}

	public void run()
		{
		int c;

		try	{
			while ( (c=in.read()) != -1 )
				{
				out.write(c);
				}
			in.close();
			out.close();
			}
		catch ( IOException e )
			{
			}
		}
	}


