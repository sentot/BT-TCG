package renderers;

import java.util.Collection;

import javax.swing.table.AbstractTableModel;

import tree.NodeProfile;

public class NodeProfileTableModel extends AbstractTableModel {
	  private static final long serialVersionUID = 55L;
	  Collection<NodeProfile> nodeProfiles;

	  public NodeProfileTableModel(Collection<NodeProfile> cps) {
	    this.nodeProfiles = cps;
	  }

	  @SuppressWarnings({"unchecked", "rawtypes"})
	  public Class getColumnClass(int columnIndex) {
	    return NodeProfile.class;
	  }

	  public int getColumnCount() {
	    return 1;
	  }

	  public String getColumnName(int columnIndex) {
	    return "Node Profiles";
	  }

	  public int getRowCount() {
	    return (nodeProfiles == null) ? 0 : nodeProfiles.size();
	  }

	  public Object getValueAt(int rowIndex, int columnIndex) {
	    return (nodeProfiles == null) ? null : nodeProfiles.toArray()[rowIndex];
	  }

	  public boolean isCellEditable(int rowIndex, int columnIndex) {
	    return false;
	  }
	}
 
