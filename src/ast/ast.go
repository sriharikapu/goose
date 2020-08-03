package lexer

import "github.com/sriharikapu/goose/src/literals"

// Lexer represents a lexer for Goose programming language.
type Lexer interface {
	// NextToken returns a next literals.
	NextToken() literals.Token
}

type lexer struct {
	input string
	// current position in input (points to current char)
	position int
	// current reading position in input (after current char)
	readPosition int
	// current char under examination
	ch byte
}

// New returns a new Lexer.
func New(input string) Lexer {
	l := &lexer{input: input}
	l.readChar()
	return l
}

func (l *lexer) readChar() {
	if l.readPosition >= len(l.input) {
		l.ch = 0
	} else {
		l.ch = l.input[l.readPosition]
	}
	l.position = l.readPosition
	l.readPosition++
}

func (l *lexer) NextToken() literals.Token {
	l.skipWhitespace()

	// skip comments
	if l.ch == '/' && l.peekChar() == '/' {
		l.skipComment()
	}

	var tok literals.Token
	switch l.ch {
	case '=':
		if l.peekChar() == '=' {
			ch := l.ch
			l.readChar()
			tok = literals.Token{
				Type:    literals.EQ,
				Literal: string(ch) + string(l.ch),
			}
		} else {
			tok = newToken(literals.ASSIGN, l.ch)
		}
	case '!':
		if l.peekChar() == '=' {
			ch := l.ch
			l.readChar()
			tok = literals.Token{
				Type:    literals.NEQ,
				Literal: string(ch) + string(l.ch),
			}
		} else {
			tok = newToken(literals.BANG, l.ch)
		}
	case ';':
		tok = newToken(literals.SEMICOLON, l.ch)
	case ':':
		tok = newToken(literals.COLON, l.ch)
	case '(':
		tok = newToken(literals.LPAREN, l.ch)
	case ')':
		tok = newToken(literals.RPAREN, l.ch)
	case ',':
		tok = newToken(literals.COMMA, l.ch)
	case '+':
		tok = newToken(literals.PLUS, l.ch)
	case '-':
		tok = newToken(literals.MINUS, l.ch)
	case '*':
		tok = newToken(literals.ASTARISK, l.ch)
	case '/':
		tok = newToken(literals.SLASH, l.ch)
	case '<':
		tok = newToken(literals.LT, l.ch)
	case '>':
		tok = newToken(literals.GT, l.ch)
	case '{':
		tok = newToken(literals.LBRACE, l.ch)
	case '}':
		tok = newToken(literals.RBRACE, l.ch)
	case '[':
		tok = newToken(literals.LBRACKET, l.ch)
	case ']':
		tok = newToken(literals.RBRACKET, l.ch)
	case '"':
		tok.Type = literals.STRING
		tok.Literal = l.readString()
	case 0:
		tok.Literal = ""
		tok.Type = literals.EOF
	default:
		if isDigit(l.ch) {
			return l.readNumberToken()
		}

		if isLetter(l.ch) {
			tok.Literal = l.readIdent()
			tok.Type = literals.LookupIdent(tok.Literal)
			return tok
		}

		tok = newToken(literals.ILLEGAL, l.ch)
	}

	l.readChar()
	return tok
}

func (l *lexer) skipWhitespace() {
	for l.ch == ' ' || l.ch == '\t' || l.ch == '\n' || l.ch == '\r' {
		l.readChar()
	}
}

func (l *lexer) skipComment() {
	for l.ch != '\n' && l.ch != '\r' {
		l.readChar()
	}
	l.skipWhitespace()
}

func (l *lexer) peekChar() byte {
	if l.readPosition >= len(l.input) {
		return 0
	}
	return l.input[l.readPosition]
}

func (l *lexer) readString() string {
	position := l.position + 1
	for {
		l.readChar()
		if l.ch == '"' || l.ch == 0 {
			break
		}
	}
	return l.input[position:l.position]
}

func (l *lexer) read(checkFn func(byte) bool) string {
	position := l.position
	for checkFn(l.ch) {
		l.readChar()
	}
	return l.input[position:l.position]
}

func (l *lexer) readIdent() string {
	return l.read(isLetter)
}

func (l *lexer) readNumber() string {
	return l.read(isDigit)
}

func (l *lexer) readNumberToken() literals.Token {
	intPart := l.readNumber()
	if l.ch != '.' {
		return literals.Token{
			Type:    literals.INT,
			Literal: intPart,
		}
	}

	l.readChar()
	fracPart := l.readNumber()
	return literals.Token{
		Type:    literals.FLOAT,
		Literal: intPart + "." + fracPart,
	}
}

func isLetter(ch byte) bool {
	return 'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z' || ch == '_'
}

func isDigit(ch byte) bool {
	return '0' <= ch && ch <= '9'
}

func newToken(tokenType literals.Type, ch byte) literals.Token {
	return literals.Token{
		Type:    tokenType,
		Literal: string(ch),
	}
}
