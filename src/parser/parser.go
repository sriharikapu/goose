package parser

import (
	"fmt"
	"strconv"

	"github.com/sriharikapu/goose/src/ast"
	"github.com/sriharikapu/goose/src/lexer"
	"github.com/sriharikapu/goose/src/literals"
)

const (
	_ int = iota
	// LOWEST represents the lowest precedence.
	LOWEST
	// EQUALS represents precedence of equals.
	EQUALS // ==
	// LESSGREATER represents precedence of less than or greater than.
	LESSGREATER // > or <
	// SUM represents precedence of sum.
	SUM // +
	// PRODUCT represents precedence of product.
	PRODUCT // *
	// PREFIX represents precedence of prefix operator.
	PREFIX // -X or !X
	// CALL represents precedence of function call.
	CALL // myFunc(X)
	// INDEX represents precedence of array index operator.
	INDEX // array[index]
)

var precedences = map[literals.Type]int{
	literals.EQ:       EQUALS,
	literals.NEQ:      EQUALS,
	literals.LT:       LESSGREATER,
	literals.GT:       LESSGREATER,
	literals.PLUS:     SUM,
	literals.MINUS:    SUM,
	literals.SLASH:    PRODUCT,
	literals.ASTARISK: PRODUCT,
	literals.LPAREN:   CALL,
	literals.LBRACKET: INDEX,
}

type (
	prefixParseFn func() ast.Expression
	infixParseFn  func(ast.Expression) ast.Expression
)

// Parser is a parser of Goose programming language.
type Parser struct {
	l      lexer.Lexer
	errors []string

	curToken  literals.Token
	peekToken literals.Token

	prefixParseFns map[literals.Type]prefixParseFn
	infixParseFns  map[literals.Type]infixParseFn
}

// New returns a new Parser.
func New(l lexer.Lexer) *Parser {
	p := &Parser{
		l:      l,
		errors: []string{},
	}

	p.prefixParseFns = map[literals.Type]prefixParseFn{
		literals.IDENT:    p.parseIdent,
		literals.INT:      p.parseIntegerLiteral,
		literals.FLOAT:    p.parseFloatLiteral,
		literals.BANG:     p.parsePrefixExpression,
		literals.MINUS:    p.parsePrefixExpression,
		literals.TRUE:     p.parseBoolean,
		literals.FALSE:    p.parseBoolean,
		literals.LPAREN:   p.parseGroupedExpression,
		literals.IF:       p.parseIfExpression,
		literals.FUNCTION: p.parseFunctionLiteral,
		literals.STRING:   p.parseStringLiteral,
		literals.LBRACKET: p.parseArrayLiteral,
		literals.LBRACE:   p.parseHashLiteral,
		literals.MACRO:    p.parseMacroLiteral,
	}

	p.infixParseFns = map[literals.Type]infixParseFn{
		literals.PLUS:     p.parseInfixExpression,
		literals.MINUS:    p.parseInfixExpression,
		literals.ASTARISK: p.parseInfixExpression,
		literals.SLASH:    p.parseInfixExpression,
		literals.EQ:       p.parseInfixExpression,
		literals.NEQ:      p.parseInfixExpression,
		literals.LT:       p.parseInfixExpression,
		literals.GT:       p.parseInfixExpression,
		literals.LPAREN:   p.parseCallExpression,
		literals.LBRACKET: p.parseIndexExpression,
	}

	// Read two tokens, so curToken and peekToken are both set
	p.nextToken()
	p.nextToken()

	return p
}

func (p *Parser) nextToken() {
	p.curToken = p.peekToken
	p.peekToken = p.l.NextToken()
}

// Errors returns error messages.
func (p *Parser) Errors() []string {
	return p.errors
}

func (p *Parser) peekError(typ literals.Type) {
	msg := fmt.Sprintf("expected next token to be %s, got %s instead", typ, p.peekToken.Type)
	p.errors = append(p.errors, msg)
}

func (p *Parser) curTokenIs(typ literals.Type) bool {
	return p.curToken.Type == typ
}

func (p *Parser) peekTokenIs(typ literals.Type) bool {
	return p.peekToken.Type == typ
}

func (p *Parser) expectPeek(typ literals.Type) bool {
	if p.peekTokenIs(typ) {
		p.nextToken()
		return true
	}

	p.peekError(typ)
	return false
}

// ParseProgram parses a program and returns a new Program AST node.
func (p *Parser) ParseProgram() *ast.Program {
	program := &ast.Program{
		Statements: []ast.Statement{},
	}

	for !p.curTokenIs(literals.EOF) {
		stmt := p.parseStatement()
		if stmt != nil {
			program.Statements = append(program.Statements, stmt)
		}
		p.nextToken()
	}

	return program
}

func (p *Parser) parseStatement() ast.Statement {
	switch p.curToken.Type {
	case literals.LET:
		return p.parseLetStatement()
	case literals.RETURN:
		return p.parseReturnStatement()
	default:
		return p.parseExpressionStatement()
	}
}

func (p *Parser) parseLetStatement() *ast.LetStatement {
	stmt := &ast.LetStatement{Token: p.curToken}

	if !p.expectPeek(literals.IDENT) {
		return nil
	}

	stmt.Name = &ast.Ident{
		Token: p.curToken,
		Value: p.curToken.Literal,
	}

	if !p.expectPeek(literals.ASSIGN) {
		return nil
	}

	p.nextToken()

	stmt.Value = p.parseExpression(LOWEST)

	for p.peekTokenIs(literals.SEMICOLON) {
		p.nextToken()
	}

	return stmt
}

func (p *Parser) parseReturnStatement() *ast.ReturnStatement {
	stmt := &ast.ReturnStatement{
		Token: p.curToken,
	}

	p.nextToken()

	stmt.ReturnValue = p.parseExpression(LOWEST)

	for p.peekTokenIs(literals.SEMICOLON) {
		p.nextToken()
	}

	return stmt
}

func (p *Parser) parseExpressionStatement() *ast.ExpressionStatement {
	stmt := &ast.ExpressionStatement{
		Token:      p.curToken,
		Expression: p.parseExpression(LOWEST),
	}

	if p.peekTokenIs(literals.SEMICOLON) {
		p.nextToken()
	}

	return stmt
}

func (p *Parser) parseExpression(precedence int) ast.Expression {
	prefix := p.prefixParseFns[p.curToken.Type]
	if prefix == nil {
		msg := fmt.Sprintf("no prefix parse function for %s found", p.curToken.Type)
		p.errors = append(p.errors, msg)
		return nil
	}

	leftExp := prefix()

	for !p.curTokenIs(literals.SEMICOLON) && precedence < p.peekPrecedence() {
		infix := p.infixParseFns[p.peekToken.Type]
		if infix == nil {
			return leftExp
		}

		p.nextToken()

		leftExp = infix(leftExp)
	}

	return leftExp
}

func (p *Parser) parseIdent() ast.Expression {
	return &ast.Ident{
		Token: p.curToken,
		Value: p.curToken.Literal,
	}
}

func (p *Parser) parseIntegerLiteral() ast.Expression {
	lit := &ast.IntegerLiteral{Token: p.curToken}

	val, err := strconv.ParseInt(p.curToken.Literal, 0, 64)
	if err != nil {
		msg := fmt.Sprintf("could not parse %q as integer", p.curToken.Literal)
		p.errors = append(p.errors, msg)
		return nil
	}

	lit.Value = val
	return lit
}

func (p *Parser) parseFloatLiteral() ast.Expression {
	val, err := strconv.ParseFloat(p.curToken.Literal, 64)
	if err != nil {
		msg := fmt.Sprintf("could not parse %q as float", p.curToken.Literal)
		p.errors = append(p.errors, msg)
		return nil
	}

	return &ast.FloatLiteral{
		Token: p.curToken,
		Value: val,
	}
}

func (p *Parser) parsePrefixExpression() ast.Expression {
	expr := &ast.PrefixExpression{
		Token:    p.curToken,
		Operator: p.curToken.Literal,
	}

	p.nextToken()

	expr.Right = p.parseExpression(PREFIX)
	return expr
}

func (p *Parser) peekPrecedence() int {
	if p, ok := precedences[p.peekToken.Type]; ok {
		return p
	}
	return LOWEST
}

func (p *Parser) curPrecedence() int {
	if p, ok := precedences[p.curToken.Type]; ok {
		return p
	}
	return LOWEST
}

func (p *Parser) parseInfixExpression(left ast.Expression) ast.Expression {
	expr := &ast.InfixExpression{
		Token:    p.curToken,
		Operator: p.curToken.Literal,
		Left:     left,
	}

	prec := p.curPrecedence()

	p.nextToken()

	expr.Right = p.parseExpression(prec)
	return expr
}

func (p *Parser) parseBoolean() ast.Expression {
	return &ast.Boolean{
		Token: p.curToken,
		Value: p.curTokenIs(literals.TRUE),
	}
}

func (p *Parser) parseGroupedExpression() ast.Expression {
	p.nextToken()

	expr := p.parseExpression(LOWEST)

	if !p.expectPeek(literals.RPAREN) {
		return nil
	}

	return expr
}

func (p *Parser) parseIfExpression() ast.Expression {
	expr := &ast.IfExpression{Token: p.curToken}

	if !p.expectPeek(literals.LPAREN) {
		return nil
	}

	p.nextToken()
	expr.Condition = p.parseExpression(LOWEST)

	if !p.expectPeek(literals.RPAREN) {
		return nil
	}

	if !p.expectPeek(literals.LBRACE) {
		return nil
	}

	expr.Consequence = p.parseBlockStatement()

	if p.peekTokenIs(literals.ELSE) {
		p.nextToken()

		if !p.expectPeek(literals.LBRACE) {
			return nil
		}

		expr.Alternative = p.parseBlockStatement()
	}

	return expr
}

func (p *Parser) parseBlockStatement() *ast.BlockStatement {
	block := &ast.BlockStatement{
		Token:      p.curToken,
		Statements: []ast.Statement{},
	}

	p.nextToken()

	for !p.curTokenIs(literals.RBRACE) && !p.curTokenIs(literals.EOF) {
		stmt := p.parseStatement()
		if stmt != nil {
			block.Statements = append(block.Statements, stmt)
		}

		p.nextToken()
	}

	return block
}

func (p *Parser) parseFunctionLiteral() ast.Expression {
	lit := &ast.FunctionLiteral{Token: p.curToken}

	if !p.expectPeek(literals.LPAREN) {
		return nil
	}

	lit.Parameters = p.parseFunctionParameters()

	if !p.expectPeek(literals.LBRACE) {
		return nil
	}

	lit.Body = p.parseBlockStatement()

	return lit
}

func (p *Parser) parseFunctionParameters() []*ast.Ident {
	idents := []*ast.Ident{}

	if p.peekTokenIs(literals.RPAREN) {
		p.nextToken()
		return idents
	}

	p.nextToken()

	ident := &ast.Ident{
		Token: p.curToken,
		Value: p.curToken.Literal,
	}
	idents = append(idents, ident)

	for p.peekTokenIs(literals.COMMA) {
		p.nextToken()
		p.nextToken()
		ident := &ast.Ident{
			Token: p.curToken,
			Value: p.curToken.Literal,
		}
		idents = append(idents, ident)
	}

	if !p.expectPeek(literals.RPAREN) {
		return nil
	}

	return idents
}

func (p *Parser) parseExpressionList(end literals.Type) []ast.Expression {
	list := make([]ast.Expression, 0)

	if p.peekTokenIs(end) {
		p.nextToken()
		return list
	}

	p.nextToken()
	list = append(list, p.parseExpression(LOWEST))

	for p.peekTokenIs(literals.COMMA) {
		p.nextToken()
		p.nextToken()
		list = append(list, p.parseExpression(LOWEST))
	}

	if !p.expectPeek(end) {
		return nil
	}

	return list
}

func (p *Parser) parseCallExpression(function ast.Expression) ast.Expression {
	return &ast.CallExpression{
		Token:     p.curToken,
		Function:  function,
		Arguments: p.parseExpressionList(literals.RPAREN),
	}
}

func (p *Parser) parseStringLiteral() ast.Expression {
	return &ast.StringLiteral{
		Token: p.curToken,
		Value: p.curToken.Literal,
	}
}

func (p *Parser) parseArrayLiteral() ast.Expression {
	return &ast.ArrayLiteral{
		Token:    p.curToken,
		Elements: p.parseExpressionList(literals.RBRACKET),
	}
}

func (p *Parser) parseIndexExpression(left ast.Expression) ast.Expression {
	expr := &ast.IndexExpression{
		Token: p.curToken,
		Left:  left,
	}

	p.nextToken()
	expr.Index = p.parseExpression(LOWEST)

	if !p.expectPeek(literals.RBRACKET) {
		return nil
	}

	return expr
}

func (p *Parser) parseHashLiteral() ast.Expression {
	hash := &ast.HashLiteral{
		Token: p.curToken,
		Pairs: make(map[ast.Expression]ast.Expression),
	}

	for !p.peekTokenIs(literals.RBRACE) {
		p.nextToken()
		key := p.parseExpression(LOWEST)

		if !p.expectPeek(literals.COLON) {
			return nil
		}

		p.nextToken()
		value := p.parseExpression(LOWEST)
		hash.Pairs[key] = value

		if !p.peekTokenIs(literals.RBRACE) && !p.expectPeek(literals.COMMA) {
			return nil
		}
	}

	if !p.expectPeek(literals.RBRACE) {
		return nil
	}

	return hash
}

func (p *Parser) parseMacroLiteral() ast.Expression {
	tok := p.curToken

	if !p.expectPeek(literals.LPAREN) {
		return nil
	}

	params := p.parseFunctionParameters()

	if !p.expectPeek(literals.LBRACE) {
		return nil
	}

	body := p.parseBlockStatement()

	return &ast.MacroLiteral{
		Token:      tok,
		Parameters: params,
		Body:       body,
	}
}
