package dummies;

import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextArea;
import javax.swing.ListSelectionModel;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import renderers.NodeTableModel;
import renderers.ORCell;
import tree.Node;

public class DummyPanel extends JPanel{
	

	private static JTable dummyTable;
	private static final JTextArea dummyTextArea = new JTextArea();
	
	public DummyPanel () {
		

	    /* Observable Response Table */
	    dummyTable = new JTable(new NodeTableModel(null));
	    dummyTable.setDefaultRenderer(Node.class, new ORCell());
	    dummyTable.setDefaultEditor(Node.class, new ORCell());
	    dummyTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
//	    dummyTable.getSelectionModel().addListSelectionListener(new ListSelectionListener() {
//	      @Override
//	      public void valueChanged(ListSelectionEvent arg0) {
//	        updateORInput();
//	      }
//	    });
	    dummyTable.setRowHeight(30);
	    dummyTable.setTableHeader(null);
	    dummyTable.setFillsViewportHeight(true);
	    dummyTable.setCellSelectionEnabled(true);
	    dummyTable.setBorder(null);
	    JScrollPane scrollPane = new JScrollPane();
	    scrollPane.setBounds(10, 64, 643, 555);
	    scrollPane.setViewportView(dummyTable);
	    this.add(scrollPane);
		
	}

}
