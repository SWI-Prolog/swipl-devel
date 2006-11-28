package jpl.util;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.UIManager;

public class SwingGadget2 extends JFrame
	{
	private static final long	serialVersionUID	= 1L;
	private Qev head;
	private Qev tail;
	public JPanel pane;

	public ActionListener al = 
		new ActionListener()
			{
			public void actionPerformed(ActionEvent e)
				{
				inc(e);
				}
			};

	public SwingGadget2(String caption)
		{
		super(caption);		// call the JFrame contructor
		try	{
			UIManager.setLookAndFeel(
				UIManager.getSystemLookAndFeelClassName()
			);
			}
		catch (Exception e)
			{
			}
		pane = new JPanel();
		getContentPane().add(pane, BorderLayout.CENTER);

	     //	JButton button1 = new JButton("click me");
	     //	JButton button2 = new JButton("...or me");

	     //	button1.addActionListener( al);
	     //	button2.addActionListener( al);

	     //	pane.add(button1);
	     //	pane.add(button2);

		pack();
		setVisible(true);
		}

	public synchronized void inc(Object e)
		{
		if ( head == null )
			{
			head = new Qev(e);
			head.next = null;
			tail = head;
			}
		else
			{
			tail.next = new Qev(e);
			tail = tail.next;
			}
		notifyAll();
		}

	public synchronized Object dec()
		{
		Qev t;

		try	{
			while ( head == null )
				{
				wait();
				}
			t = head;
			head = head.next;
			return t.ev;
			}
		catch ( InterruptedException e )
			{
			return null;
			}
		}

	public class Qev
		{
		public Object ev;
		public Qev next;

		public Qev(Object e)
			{
			ev = e;
			}
		}
	}



