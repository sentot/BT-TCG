package renderers;

import java.util.List;

import javax.swing.table.AbstractTableModel;

import other.TestSegment;

public class TestPathModel extends AbstractTableModel {
  private static final long serialVersionUID = 55L;
  List<TestSegment> path;
  
  public TestPathModel(List<TestSegment> path) {
    this.path = path;
  }
  
  public void addData(List<TestSegment> path) {
	  this.path = path;
  }

  @SuppressWarnings({"unchecked", "rawtypes"})
  public Class getColumnClass(int columnIndex) {
    return TestSegment.class;
  }

  public int getColumnCount() {
    return 1;
  }

  public String getColumnName(int columnIndex) {
    return "Feed";
  }

  public int getRowCount() {
    return (path == null) ? 0 : path.size();
  }

  public Object getValueAt(int rowIndex, int columnIndex) {
    return (path == null) ? null : path.get(rowIndex);
  }

  public boolean isCellEditable(int rowIndex, int columnIndex) {
    return false;
  }
}
