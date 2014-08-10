package main

import (
	"fmt"
	"os"
	"io/ioutil"
	"strings"
	"strconv"
)

type Tree struct {
	value string
	children map[rune] Tree
}

func (t Tree) Value() string {
	return t.value
}

func NewTree() Tree {
	tree := new(Tree)
	tree.children = make(map[rune] Tree)
	// Default value
	tree.value = ""
	return *tree
}

func (t *Tree) Insert(s string) {
	if len(s) == 0 {
		return
	}
	s = strings.ToLower(s)

	for i, rune := range s {
		if child, ok := t.children[rune]; ok {
			// Matching child.
			t = &child
		} else if i == len(s) - 1 {
			// Insert value as node.
			next := NewTree()
			next.value = s
			t.children[rune] = next
			t = &next
		} else {
			// New tree and continue traversing.
			next := NewTree()
			t.children[rune] = next
			t = &next
		}
	}
}

func (t *Tree) Show() {
	if t.value != "" {
		fmt.Println("[", t.value, "]")
	}
	t.printLoop(1)
}

func spaceString(n int) string {
	var spaces string
	for i := 0; i < n; i++ {
		spaces = strings.Join([]string{spaces, "  "}, "")
	}
	return spaces
}

func (t *Tree) printLoop(indent int) {
	for rune, child := range t.children {
		fmt.Println(spaceString(indent), strconv.QuoteRune(rune))
		if child.value != "" {
			fmt.Println(spaceString(indent), "[", child.value, "]")
		}
		child.printLoop(indent + 1)
	}
}

func (t *Tree) WithPrefix(s string) []string {
	s = strings.ToLower(s)
        for _, rune := range s {
		// Walk the tree until we get to the child at the end of our prefix.
                if child, ok := t.children[rune]; ok {
                        t = &child
                } else {
			// No matches with that prefix
			return make([]string, 0)
                }
        }
	// Return all strings starting from the tree at the end of our prefix.
	return t.AllStrings()
}

func (t *Tree) AllStrings() []string {
	var strings []string
	if t.value != "" {
		strings = append(strings, t.value)
	}
	return append(strings, t.getStrings()...)
}

func (t *Tree) getStrings() []string {
	var strings []string
	for _, child := range t.children {
		if child.value != "" {
			strings = append(strings, child.value)
		}
		strings = append(strings, child.getStrings()...)
	}
	return strings
}

func buildTree(wordsFile string) Tree {
        content, err := ioutil.ReadFile(wordsFile)
        if err != nil {
                fmt.Println("Could not read words file:", wordsFile)
                os.Exit(-1)
        }
        words := strings.Split(string(content), "\n")

	tree := NewTree()
	for _, word := range words {
                word = strings.Replace(word, "\r", "", -1)
		tree.Insert(word)
	}
	return tree
}

func main() {
	wordsFile := "wordsEn.txt"
	tree := buildTree(wordsFile)

	for {
		fmt.Println("Enter a prefix to match:")
		var prefix string
		_, err := fmt.Scanf("%s", &prefix)
		if err != nil {
			fmt.Println("Could not read prefix")
			os.Exit(-1)
		}

		matches := tree.WithPrefix(prefix)
		if len(matches) == 0 {
			fmt.Println("\tNo Matches for '", prefix, "'")
		} else {
			for _, match := range matches {
				fmt.Println("\t", match)
			}
			fmt.Println("\t[", len(matches), "matching terms ]")
		}
	}
}
