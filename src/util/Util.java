package util;

public class Util {
	
	public static String toHtml (String str) {
		String result = "";
		for (Integer i = 0; i < str.length(); i++ ) {
			if (str.substring(i, i+1).equals("<")) {
				result = result + "&lt;";
			} else if (str.substring(i, i+1).equals(">")) {
				result = result + "&gt;";
			} else if (str.substring(i, i+1).equals("&")) {
				result = result + "&amp;";
			} else if (str.substring(i, i+1).equals("'")) {
				result = result + "&apos;";
			} else if (str.substring(i, i+1).equals('"')) {
				result = result + "&quot;";
			} else if (str.substring(i, i+1).equals(" ")) {
				result = result + "&nbsp;";
			} else {
				result = result + str.substring(i, i+1);
			}
		}
		return result;
	}

}
