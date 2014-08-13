package main

import (
	"fmt"
	"os"
)

type RuneStack struct {
	runes []rune
}

func (s *RuneStack) Pop() rune {
	end := len(s.runes)
	if end < 1 {
		fmt.Println("Invalid pop")
		os.Exit(-1)
	}
	r := s.runes[end - 1]
	s.runes = s.runes[:(end - 1)] // Chop off the last element.
	return r}

func (s *RuneStack) Push(r rune) {
	s.runes = append(s.runes, r)
}

func (s *RuneStack) Size() int {
	return len(s.runes)
}

type IntStack struct {
	values []int
}

func (s *IntStack) Pop() int {
	end := len(s.values)
	if end < 1 {
		fmt.Println("Invalid pop")
		os.Exit(-1)
	}
	value := s.values[end - 1]
	s.values = s.values[:(end - 1)] // Chop off the last element.
	return value
}

func (s *IntStack) Push(i int) {
	s.values = append(s.values, i)
}

func (s *IntStack) Size() int {
	return len(s.values)
}

func intify(r rune) int {
	return int(r - '0')
}

func runify(i int) rune {
	return rune(i + '0')
}

func operate(op rune, op1 int, op2 int) int {
	if op == '/' {
		return op2 / op1 // Reversed order because of stack.
	} else if op == '*' {
		return op1 * op2
	} else if op == '-' {
		return op2 - op1
	} else { // Must be addition.
		return op1 + op2
	}
}

func isInt(r rune) bool {
	i := intify(r)
	if i > -1 && i < 10 { // Going rune by rune these are the only valid nums.
		return true
	} else {
		return false
	}
}

func (s *IntStack) flattenToNum() int {
	if s.Size() == 0 {
		fmt.Println("Can't call flattenToNum on empty stack")
		os.Exit(-1)
	}
	multiplier := 1
	total := 0
	for s.Size() > 0 {
		total += s.Pop() * multiplier
		multiplier *= 10
	}
	return total
}

func pushComputation(valueStack *IntStack, opStack *RuneStack) {
	nextOp := opStack.Pop()
	operand1 := valueStack.Pop()
	operand2 := valueStack.Pop()
	result := operate(nextOp, operand1, operand2)
	valueStack.Push(result)
}

func compute(expression string) int {
	valueStack := new(IntStack)
	opStack    := new(RuneStack)
	digitBuffer := new(IntStack)

	for _,r := range expression {
		if r == ')' {
			// Flush the int buffer.
			if digitBuffer.Size() > 0 {
				value := digitBuffer.flattenToNum()
				valueStack.Push(value)
			}
			pushComputation(valueStack, opStack)

		} else if (r == '+' || r == '-' || r == '*' || r == '/') {
			opStack.Push(r)
			// Flush the int buffer.
			if digitBuffer.Size() > 0 {
				value := digitBuffer.flattenToNum()
				valueStack.Push(value)
			}
		} else if (isInt(r)) {
			digitBuffer.Push(intify(r)) // Push onto buffer
		}
	}
	// Flush the buffer one last time.
	if digitBuffer.Size() > 0 {
		value := digitBuffer.flattenToNum()
		valueStack.Push(value)
	}

	// Handle case with no outer parens
	if valueStack.Size() == 2 && opStack.Size() == 1 {
		pushComputation(valueStack, opStack)
	}

	if valueStack.Size() > 1 || opStack.Size() != 0 {
		fmt.Println("Invalid expression")
		os.Exit(-1)
	}
	return valueStack.Pop()
}

func main() {
	expression := "((10 * 5) + 7) + 2) / 5"
	answer := compute(expression)
	fmt.Println(answer)
}
