package renderers;

import java.awt.Color;
import java.awt.Component;
import java.awt.FlowLayout;
import java.awt.Font;

import javax.swing.AbstractCellEditor;
import javax.swing.BorderFactory;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTable;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;

import tree.NodeProfile;

public class NodeProfileTableCell extends AbstractCellEditor
    implements TableCellEditor, TableCellRenderer {
  private static final long serialVersionUID = 55L;
  JPanel panel;
  JLabel text;
  NodeProfile nodeProfile;

  public NodeProfileTableCell() {
    text = new JLabel();
    text.setFont(new Font(Font.SANS_SERIF, Font.PLAIN, 16));

    panel = new JPanel(new FlowLayout(FlowLayout.LEFT));
    panel.add(text);
  }

  protected void updateData(NodeProfile nodeProfile, boolean isSelected, JTable table) {
    this.nodeProfile = nodeProfile;
    if (nodeProfile != null) {
      text.setText(nodeProfile.toString());
      if (isSelected) {
        panel.setBorder(BorderFactory.createMatteBorder(2, 2, 2, 2, Color.BLUE));
      } else {
        panel.setBorder(table.getBorder());
      }
    }
  }

  public Component getTableCellEditorComponent(JTable table, Object value, boolean isSelected,
      int row, int column) {
    NodeProfile nodeProfile = (NodeProfile) value;
    updateData(nodeProfile, isSelected, table);
    return panel;
  }

  public Object getCellEditorValue() {
    return null;
  }

  public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected,
      boolean hasFocus, int row, int column) {
    NodeProfile nodeProfile = (NodeProfile) value;
    updateData(nodeProfile, isSelected, table);
    return panel;
  }
}
