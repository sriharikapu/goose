package main

import (
	"errors"
	"fmt"
	"io"
	"io/ioutil"
	"os"

	"github.com/sriharikapu/goose/src/eval"
	"github.com/sriharikapu/goose/src/lexer"
	"github.com/sriharikapu/goose/src/object"
	"github.com/sriharikapu/goose/src/parser"
	"github.com/sriharikapu/goose/src/repl"
)

func main() {
	// Start Goose REPL
	if len(os.Args) == 1 {
		fmt.Println("****************************************************************")
		fmt.Println("***               Welcome to Goose Interpreter!              ***")
		fmt.Println("****************************************************************")

		fmt.Println("████████████████████████████████████████████████████████████████")
		fmt.Println("████████████████████▀▀▀▀████████████████████████████████████████")
		fmt.Println("███████████████████░░░░░░░██████████████████████████████████████")
		fmt.Println("████████████████░░░░░░░░░░░█████████████████████████████████████")
		fmt.Println("████████████████████▄▄░░░░░█████████████████████████████████████")
		fmt.Println("██████████████████████░░░░░█████████████████████████████████████")
		fmt.Println("██████████████████████░░░░░█████████████████████████████████████")
		fmt.Println("██████████████████████░░░░░█████████████████████████████████████")
		fmt.Println("█████████████████████▀░░░░▐█████████████████████████████████████")
		fmt.Println("████████████████████▀░░░░░██████████████████████████████████████")
		fmt.Println("████████████████████░░░░░▐██████████████████████████████████████")
		fmt.Println("███████████████████░░░░░░███████████████████████████████████████")
		fmt.Println("██████████████████░░░░░░▐███████████████████████████████████████")
		fmt.Println("█████████████████▌░░░░░░░███████████████████████████████████████")
		fmt.Println("█████████████████░░░░░░░░░▀▀████████████████████████████████████")
		fmt.Println("█████████████████░░░░░░░░░░░░░▀▀████████████████████████████████")
		fmt.Println("█████████████████░░░░░░░░░░░░░░░░░▀▀████████████████████████████")
		fmt.Println("█████████████████▌░░░░░░░░░░░░░░░░░░░▀▀█████████████████████████")
		fmt.Println("██████████████████░░░░░░░░░░░░░░░░░░░░░░░▀▀█████████████████████")
		fmt.Println("███████████████████░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░█████████████")
		fmt.Println("████████████████████▄░░░░░░░░░░░░░░░░░░░░░░░░░░░░░▐█████████████")
		fmt.Println("█████████████████████▄░░░░░░░░░░░░░░░░░░░░░░░░░░▄███████████████")
		fmt.Println("███████████████████████░░░░░░░░░░░░░░░░░░░░░░███████████████████")
		fmt.Println("████████████████████████▄░░░░░░░░░░░░░░░░░░▄████████████████████")
		fmt.Println("██████████████████████████▄░░░░░░░░░░░░░▄▄██████████████████████")
		fmt.Println("███████████████████████████░░░░▄▄▄▄▄▄▄██████████████████████████")
		fmt.Println("███████████████████████████░░░░█████████████████████████████████")
		fmt.Println("███████████████████████████░░░░█████████████████████████████████")
		fmt.Println("██████████████████████░░▀▀▀░░░░█████████████████████████████████")
		fmt.Println("██████████████████████░░░░░░░░░█████████████████████████████████")
		fmt.Println("██████████████████████▄▄▄▄▄▄▄▄▄█████████████████████████████████")
		fmt.Println("████████████████████████████████████████████████████████████████")


		fmt.Println("Use commands to try it out")
		repl.Start(os.Stdin, os.Stdout)
		return
	}

	// Run a Goose 🦆  script
	if err := runProgram(os.Args[1]); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
}

func runProgram(filename string) error {
	data, err := ioutil.ReadFile(filename)
	if err != nil {
		return fmt.Errorf("could not read %s: %v", filename, err)
	}

	p := parser.New(lexer.New(string(data)))
	program := p.ParseProgram()
	if len(p.Errors()) > 0 {
		return errors.New(p.Errors()[0])
	}

	env := object.NewEnvironment()
	result := eval.Eval(program, env)
	if _, ok := result.(*object.Nil); ok {
		return nil
	}

	_, err = io.WriteString(os.Stdout, result.Inspect()+"\n")
	return err
}
