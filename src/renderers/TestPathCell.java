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
import other.TestSegment;
import tree.Node;
import util.Util;

public class TestPathCell extends AbstractCellEditor implements TableCellEditor, TableCellRenderer {
  private static final long serialVersionUID = 55L;
  JPanel panel;
  JLabel id;
  JLabel text;
  
  // JButton showButton;

  TestSegment segment;

  public TestPathCell() {
	id = new JLabel();
    text = new JLabel();
    panel = new JPanel(new BorderLayout());
    panel.add(id, BorderLayout.LINE_START);
    panel.add(text, BorderLayout.CENTER);
    //panel.setBackground(new Color(255, 77, 77));
    panel.setBackground(new Color(255, 204, 229));
  }

  private void updateData(TestSegment segment, boolean isSelected, JTable table) {
    this.segment = segment;

    id.setText(" " + segment.getTestCase().getIndex().toString() + " ");
    text.setText("<html><b>Start Node:</b> " + segment.getTestCase().getStartNode() + "<br><b>TargetNode:</b> "
        + segment.getTestCase().getEndNode() + "<br><b>Nodes Before:</b> " + segment.getPreamble().size()
        + "<br><b>Nodes Involved:</b> " + segment.getTestCase().getNodeLength() + "</html>");

    if (isSelected) {
      panel.setBorder(BorderFactory.createMatteBorder(2, 2, 2, 2, Color.BLUE));
    } else {
      panel.setBorder(table.getBorder());
    }
  }

  public Component getTableCellEditorComponent(JTable table, Object value, boolean isSelected,
      int row, int column) {
    TestSegment feed = (TestSegment) value;
    updateData(feed, true, table);
    return panel;
  }

  public Object getCellEditorValue() {
    return null;
  }

  public String getToolTipText() {
    StringBuilder sb = new StringBuilder();
    Integer preambleLength = segment.getPreamble().size();
    sb.append("<html><b>Nodes Before:</b> " + preambleLength);
    for (Integer i = 0; i < preambleLength; i++) {
    	for (Node n : Main.getBlock(segment.getPreamble().get(i)).getNodes()) {
    	      sb.append("<br>" + Constants.htmlTabSpacing + Util.toHtml(n.toString()));
    	    }
    }
    sb.append("<br><b>Nodes Involved:</b> " + segment.getTestCase().getNodeLength());
    for (Node n : segment.getTestCase().getNodeSteps()) {
      sb.append("<br>" + Constants.htmlTabSpacing + Util.toHtml(n.toString()));
    }
    sb.append("</html>");
    return sb.toString();
  }

  public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected,
      boolean hasFocus, int row, int column) {
    TestSegment feed = (TestSegment) value;
    updateData(feed, isSelected, table);
    panel.setToolTipText(getToolTipText());
    ToolTipManager.sharedInstance().setDismissDelay(Integer.MAX_VALUE);
    return panel;
  }
}
