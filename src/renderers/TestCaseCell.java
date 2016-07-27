package renderers;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;

import javax.swing.AbstractCellEditor;
import javax.swing.BorderFactory;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTable;
import javax.swing.ToolTipManager;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;

import core.Main;
import other.Constants;
import other.TestCase;
import tree.Block;
import tree.Node;
import util.Util;

public class TestCaseCell extends AbstractCellEditor implements TableCellEditor, TableCellRenderer {
  private static final long serialVersionUID = -5535773127849322173L;
  JPanel panel;
  JLabel id;
  JLabel text;
  JLabel colourIndicator;

  TestCase path;

  public TestCaseCell() {
	id = new JLabel();
    text = new JLabel();
    colourIndicator = new JLabel("    ");
    colourIndicator.setOpaque(true);
    panel = new JPanel(new BorderLayout());
    panel.add(id, BorderLayout.LINE_START);
    panel.add(text, BorderLayout.CENTER);
    panel.add(colourIndicator, BorderLayout.LINE_END);
    panel.setBackground(new Color(255, 77, 77));
  }

  private void updateData(TestCase path, boolean isSelected, JTable table) {
    this.path = path;

    id.setText(" " + path.getIndex().toString() + " ");
    text.setText(getCellText());

    // If cell is selected give it a border
    if (isSelected) {
      panel.setBorder(BorderFactory.createMatteBorder(2, 2, 2, 2, Color.BLUE));
    } else {
      panel.setBorder(table.getBorder());
    }
    if (path.isSelected()) {
      // If test case has been added to test path colour it green
      panel.setBackground(Constants.selectedTestCaseColour);
    } else {
      panel.setBackground(Constants.notSelectedTestCaseColour);
    }
    if (!path.isReachable()) {
    	colourIndicator.setBackground(Constants.unavailableColour);
    } else if (path.needsPreamble()) {
    	colourIndicator.setBackground(Constants.needsPreambleColour);
    } else {
    	colourIndicator.setBackground(Constants.immediatelyAvailableColour);
    }
  }

  public Component getTableCellEditorComponent(JTable table, Object value, boolean isSelected,
      int row, int column) {
    TestCase feed = (TestCase) value;
    updateData(feed, true, table);
    return panel;
  }

  public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected,
      boolean hasFocus, int row, int column) {
    TestCase feed = (TestCase) value;
    updateData(feed, isSelected, table);
    panel.setToolTipText(getToolTipText());
    ToolTipManager.sharedInstance().setDismissDelay(Integer.MAX_VALUE);
    return panel;
  }

  public Object getCellEditorValue() {
    return null;
  }

  public String getToolTipText() {
    StringBuilder sb = new StringBuilder();
    sb.append("<html><b>Nodes Before:</b> " + path.getNodesBeforeLength());
    for (Node n : path.getNodesBefore()) {
      sb.append("<br>" + Constants.htmlTabSpacing + formatNode(n));
    }
    sb.append("<br><b>Nodes Involved:</b> " + path.getNodeLength());
    for (Integer i: path.getSteps()) {
    	Block b = Main.getBlock(i);
    	for (Node n : b.getNodes()) {
    		if (Main.isChosenNOI(n.getNodeProfile()) || Main.isChosenCP(n.getNodeProfile())
        			|| ((i != path.getStart()) && (i != path.getEnd())
        					&& (n.getFlag().equals("REVERSION") || n.getFlag().equals("REFERENCE")))) {
        		sb.append("<br>" + Constants.htmlTabSpacing + "<b>" + formatNode(n) + "</b>");
        	} else {
        		sb.append("<br>" + Constants.htmlTabSpacing + formatNode(n));
        	}
    	}
    }
//    for (Node n : path.getNodeSteps()) {
//    	if (Main.isChosenNOI(n.getNodeProfile())
//    			|| n.getFlag().equals("REVERSION")
//    			|| n.getFlag().equals("REFERENCE")) {
//    		sb.append("<br>" + Constants.htmlTabSpacing + "<b>" + formatNode(n) + "</b>");
//    	} else {
//    		sb.append("<br>" + Constants.htmlTabSpacing + formatNode(n));
//    	}     
//    }
    sb.append("<br><b>Nodes After:</b> " + path.getNodesAfterLength());
    for (Node n : path.getNodesAfter()) {
      sb.append("<br>" + Constants.htmlTabSpacing + formatNode(n));
    }
    sb.append("</html>");
    return sb.toString();
  }

  private String formatNode(Node n) {
	  return Util.toHtml(n.toString());
  }

  private String getCellText() {
    return "<html><b>Start Node:</b> " + path.getCPStartNode().toString() + "<br><b>TargetNode:</b> "
        + path.getCPEndNode().toString() + "<br><b>Nodes Before:</b> " + path.getNodesBeforeLength()
        + "<br><b>Nodes Involved:</b> " + path.getNodeLength()
        + "<br><b>Nodes After:</b> " + path.getNodesAfterLength() + "</html>";
  }
}
