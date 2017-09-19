package edu.java;

/** A simple struct representing a token produced by the scanner */
public class Token {
	final Enum kind;
	/** what kind of token is this */
	final String lexeme;
	/** the actual text of this token */
	final Coords coords;

	/** where did this token occur */

	Token(Enum kind, String lexeme, Coords coords) {
		this.kind = kind;
		this.lexeme = lexeme;
		this.coords = coords;
	}	
	
	public String toString() {
		return String.format("%s: %s \"%s\"", coords, kind, lexeme);
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((coords == null) ? 0 : coords.hashCode());
		result = prime * result + ((kind == null) ? 0 : kind.hashCode());
		result = prime * result + ((lexeme == null) ? 0 : lexeme.hashCode());
		return result;
	}
}
