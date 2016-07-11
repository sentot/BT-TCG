package renderers;

import javax.swing.JTable;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;

import other.Constants;
import tree.NodeProfile;
import core.Main;

public class CPCell extends NodeProfileTableCell implements TableCellEditor, TableCellRenderer {
  private static final long serialVersionUID = 55L;

  @Override
  protected void updateData(NodeProfile nodeProfile, boolean isSelected, JTable table) {
    super.updateData(nodeProfile, isSelected, table);
    if (Main.isChosenCP(nodeProfile)) {
      panel.setBackground(Constants.cellChosenBG);
    } else {
      panel.setBackground(Constants.cellNotChosenBG);
    }
  }
}
