package renderers;

import javax.swing.JTable;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;

import core.Main;
import other.Constants;
import tree.NodeProfile;

public class ORCell extends NodeProfileTableCell implements TableCellEditor, TableCellRenderer {
  private static final long serialVersionUID = 55L;

  @Override
  protected void updateData(NodeProfile nodeProfile, boolean isSelected, JTable table) {
    super.updateData(nodeProfile, isSelected, table);
    if (Main.getObservable(nodeProfile).equals("")) {
      panel.setBackground(Constants.cellNotChosenBG);
    } else {
      panel.setBackground(Constants.cellChosenBG);
    }
  }
}
