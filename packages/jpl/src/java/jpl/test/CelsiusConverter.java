package jpl.test;

/**
 * CelsiusConverter.java is a 1.4 application that 
 * demonstrates the use of JButton, JTextField and
 * JLabel.  It requires no other files.
 */
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

public class CelsiusConverter implements ActionListener {
	JFrame		converterFrame;
	JPanel		converterPanel;
	JTextField	tempCelsius;
	JLabel		celsiusLabel, fahrenheitLabel;
	JButton		convertTemp;
	public CelsiusConverter() { // initially locate the window at top-left of desktop
		this(0, 0);
	}
	public CelsiusConverter(int left, int top) { // initially locate the window at top-left of desktop
		// create and set up the window
		converterFrame = new JFrame("Convert Celsius to Fahrenheit");
		converterFrame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		converterFrame.setSize(new Dimension(120, 40));
		converterFrame.setLocation(left, top);
		// create and set up the panel
		converterPanel = new JPanel(new GridLayout(2, 2));
		// create widgets
		tempCelsius = new JTextField(2);
		celsiusLabel = new JLabel("Celsius", SwingConstants.LEFT);
		celsiusLabel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
		//
		convertTemp = new JButton("Convert");
		fahrenheitLabel = new JLabel("Fahrenheit", SwingConstants.LEFT);
		// listen to events from the Convert button
		convertTemp.addActionListener(this);
		// add the widgets to the container
		converterPanel.add(tempCelsius);
		converterPanel.add(celsiusLabel);
		converterPanel.add(convertTemp);
		converterPanel.add(fahrenheitLabel);
		fahrenheitLabel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
		converterFrame.getRootPane().setDefaultButton(convertTemp); // make "convert" the window's default button
		// add the panel to the window
		converterFrame.getContentPane().add(converterPanel, BorderLayout.CENTER);
		// display the window
		converterFrame.pack();
		converterFrame.setVisible(true);
	}
	public void actionPerformed(ActionEvent event) {
		// parse degrees Celsius as a double
		double tC = (Double.parseDouble(tempCelsius.getText()));
		//
		// convert to Fahrenheit (in Java)
		// int tempFahr = (int) (tC * 1.8 + 32);
		//
		// convert to Fahrenheit (in Prolog, via JPL)
		int tempFahr = ((jpl.Float) jpl.Query.oneSolution("TF is ? * 1.8 + 32", new jpl.Term[] {new jpl.Float(tC)}).get("TF")).intValue();
		//
		// display the result
		fahrenheitLabel.setText(tempFahr + " Fahrenheit");
	}
	public static void spawnGUI(final int left, final int top) {
		// schedule a job for the event-dispatching thread: create and show an instance of this application at (left,top)
		javax.swing.SwingUtilities.invokeLater(new Runnable() {
			int x = left;
			int y = top;
			public void run() {
				new CelsiusConverter(x, y); // can we be sure this won't be garbage collected?
			}
		});
	}
	public static void main(String[] args) {
		// just for fun, we ask Prolog to start five instances of this class (at stepped offsets from top-left of display)
		jpl.Query.allSolutions("between(1, 5, N), X is 10*N, Y is 20*N, jpl_call('jpl.test.CelsiusConverter', spawnGUI, [X,Y], _)");
	}
}
