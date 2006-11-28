package jpl.util;

import java.awt.BorderLayout;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.UIManager;

public class SwingGadget extends JFrame
	{
	private static final long	serialVersionUID	= 1L;

	public int numClicks = 0;	// can be changed e.g. from Prolog

	private static String labelPrefix = "Number of button clicks: ";
	final JLabel label = new JLabel(labelPrefix + "0    ");

	public SwingGadget(String caption)
		{
		super(caption);		// call the JFrame contructor

		JButton button = new JButton("I'm a Swing button!");

		button.addActionListener(
			new ActionListener()
				{
				public void actionPerformed(ActionEvent e)
					{
					inc();
					}
				}
		);

		label.setLabelFor(button);

		JPanel pane = new JPanel();
		pane.setBorder(BorderFactory.createEmptyBorder(30,30,10,30));
		pane.setLayout(new GridLayout(0, 1));
		pane.add(button);
		pane.add(label);

		try	{
			UIManager.setLookAndFeel(
				UIManager.getCrossPlatformLookAndFeelClassName()
			);
			}
		catch (Exception e)
			{
			}

		getContentPane().add(pane, BorderLayout.CENTER);

		addWindowListener(new WindowAdapter()
			{
			public void windowClosing(WindowEvent e)
				{
			     //	System.exit(0);
				setVisible(false);
				}
			}
		);
		pack();
		setVisible(true);
		}

	public synchronized void inc()
		{
		numClicks++;
		label.setText(labelPrefix + numClicks);
		notifyAll();
		}

	public synchronized boolean dec()
		{
		try	{
			while ( numClicks <= 0 )
				{
				wait();
				}
			numClicks--;
			label.setText(labelPrefix + numClicks);
			return true;
			}
		catch ( InterruptedException e )
			{
			return false;
			}
		}
	}



