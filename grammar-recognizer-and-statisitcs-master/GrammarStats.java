package edu.java;

import java.util.LinkedHashMap;
import java.util.HashSet;
import java.util.Set;

/*
 * This class is used to recognize whether the given grammar is as per the specifications.
 * 
 */
public class GrammarStats {

	private final Scanner _scanner;
	private Token _lookahead;
	/*
	 * Defines a set of Non terminals used on the right hand side of the rule.
	 */
	private Set<String> nonTerminals;
	/* Used to store set of non terminals for which rule is defined. */
	private Set<String> nonTerminalRule;
	/* Used to store a set of terminals defined in the grammar. */
	private Set<String> terminals;
	/* Used to store the count of non terminals */
	private int nonTerminalCount;
	/* Used to store the count of terminals */
	private int terminalCount;
	/* Used to store rule Count */
	private Integer ruleCount;
	/* Used to check if delimiter as already occurred or not */
	private boolean delimiter;
	/* Used to store the starting symbol in the grammar */
	private String startSymbol;

	GrammarStats(String fileName) {
		nonTerminalRule = new HashSet<String>();
		nonTerminals = new HashSet<String>();
		terminals = new HashSet<String>();
		ruleCount = 0;
		nonTerminalCount = 0;
		terminalCount = 0;
		delimiter = true;
		_scanner = new Scanner(fileName, PATTERNS_MAP);
		nextToken();
	}

	/**
	 * Recognize a grammar specification. Return silently if ok, else signal an
	 * error.
	 */
	Stats getStats() {
		Stats stats = null;
		try {
			recognizeGrammarRules();
			checkErrorScenariosFromProjectSpecification();
			// create stats struct if everything ok.
			stats = new Stats(ruleCount, nonTerminalCount, terminalCount);
		} catch (GrammarParseException e) {
			System.err.println(e.getMessage());
		}
		return stats;
	}

	/*
	 * This method is used to find out following scenarios from the given
	 * grammar - 1. If the grammar uses a non-terminal within a rule for which
	 * there is no rule-set. 2. If the grammar defines a non-terminal by some
	 * rule-set but there is no use of that non-terminal within some rule (note
	 * that the start symbol which is the non-terminal defined by the first rule
	 * always has a use).
	 */
	private void checkErrorScenariosFromProjectSpecification() {
		Set<String> intermediateSet = new HashSet<String>(nonTerminals);
		nonTerminals.removeAll(nonTerminalRule);
		// first symbol is removed to satisfy the special scenario from point 2
		// of method documentation.
		nonTerminalRule.remove(startSymbol);
		nonTerminalRule.removeAll(intermediateSet);
		if (nonTerminals.size() != 0) {
			throw new GrammarParseException("Following Non Terminals are not defined by the grammar : " + nonTerminals);
		}
		if (nonTerminalRule.size() != 0) {
			throw new GrammarParseException(
					"Following Non Terminals defined by the grammar are not being used : " + nonTerminalRule);
		}
	}

	/*
	 * This method is used to initialize the recognizing algorithm.
	 */
	private void recognizeGrammarRules() {
		recognizeLHS();
		if (_lookahead.kind == TokenKind.EOF) {
			match(TokenKind.EOF);
		} else {
			syntaxError();
		}
	}

	/*
	 * This method is used to recognize left hand side of a rule.
	 */
	private void recognizeLHS() {
		if (nonTerminalRule.size() == 0) {
			startSymbol = _lookahead.lexeme;
			nonTerminals.add(_lookahead.lexeme);
		}
		if (_lookahead.kind == TokenKind.NON_TERMINAL) {
			if (nonTerminalRule.add(_lookahead.lexeme)) {
				nonTerminalCount++;
				match(TokenKind.NON_TERMINAL);
				match(TokenKind.COLON);
				recognizeRHS();
			} else {
				throw new GrammarParseException(String.format("%s Multiple Rules defined for Non Terminal : [%s]",
						_lookahead.coords, _lookahead.lexeme));
			}
		}
	}

	/*
	 * This method is used to recognize the right hand side of a rule.
	 */
	private void recognizeRHS() {
		switch (_lookahead.kind.name()) {
		case "NON_TERMINAL":
			delimiter = false;
			nonTerminals.add(_lookahead.lexeme);
			match(TokenKind.NON_TERMINAL);
			nonTerminalCount++;
			recognizeRHS();
			break;
		case "TERMINAL":
			delimiter = false;
			terminals.add(_lookahead.lexeme);
			match(TokenKind.TERMINAL);
			terminalCount++;
			recognizeRHS();
			break;
		case "PIPE":
			if (delimiter == true) {
				syntaxError();
			}
			delimiter = true;
			match(TokenKind.PIPE);
			recognizeRHS();
			break;
		case "SEMI":
			delimiter = true;
			ruleCount++;
			match(TokenKind.SEMI);
			recognizeLHS();
			break;
		default:
			syntaxError();
		}
	}

	// We extend RuntimeException since Java's checked exceptions are
	// very cumbersome
	private static class GrammarParseException extends RuntimeException {
		GrammarParseException(String message) {
			super(message);
		}
	}

	private void match(TokenKind kind) {
		if (kind != _lookahead.kind) {
			syntaxError();
		}
		if (kind != TokenKind.EOF) {
			nextToken();
		}
	}

	/** Skip to end of current line and then throw exception */
	private void syntaxError() {
		String message = String.format("%s: syntax error at '%s'", _lookahead.coords, _lookahead.lexeme);
		throw new GrammarParseException(message);
	}

	private static final boolean DO_TOKEN_TRACE = false;

	private void nextToken() {
		_lookahead = _scanner.nextToken();
		if (DO_TOKEN_TRACE)
			System.err.println("token: " + _lookahead);
	}

	/** token kinds for grammar tokens */
	private static enum TokenKind {
		EOF, COLON, PIPE, SEMI, NON_TERMINAL, TERMINAL, ERROR,
	}

	/** Simple structure to collect grammar statistics */
	private static class Stats {
		final int nRuleSets;
		final int nNonTerminals;
		final int nTerminals;

		Stats(int nRuleSets, int nNonTerminals, int nTerminals) {
			this.nRuleSets = nRuleSets;
			this.nNonTerminals = nNonTerminals;
			this.nTerminals = nTerminals;
		}

		public String toString() {
			return String.format("%d %d %d", nRuleSets, nNonTerminals, nTerminals);
		}
	}

	/** Map from regex to token-kind */
	private static final LinkedHashMap<String, Enum> PATTERNS_MAP = new LinkedHashMap<String, Enum>() {
		{
			put("", TokenKind.EOF);
			put("\\s+", null); // ignore whitespace.
			put("\\//.*", null); // ignore // comments
			put("\\:", TokenKind.COLON);
			put("\\|", TokenKind.PIPE);
			put("\\;", TokenKind.SEMI);
			put("[a-z]\\w*", TokenKind.NON_TERMINAL);
			put("[A-Z]\\w*", TokenKind.TERMINAL);
			put(".", TokenKind.ERROR); // catch lexical error in parser
		}
	};

	private static final String USAGE = String.format("usage: java %s GRAMMAR_FILE", GrammarStats.class.getName());

	/** Main program for testing */
	public static void main(String[] args) {
		if (args.length != 1) {
			System.err.println(USAGE);
			System.exit(1);
		}
		GrammarStats grammarStats = new GrammarStats(args[0]);
		Stats stats = grammarStats.getStats();
		if (stats != null) {
			System.out.println(stats);
		}
	}

}
