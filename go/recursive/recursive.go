package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

// Filters strings of digits into alphanumeric string via the T9 telephone keypad.
//
// Read strings of digits from STDIN, one line at a time.
// Convert each string of digits into a set of alphanumeric strings via the T9 telephone keypand
// using recursive algorithm and write the set of results to STDOUT.

var T9 = map[string][]string{
	"0": {"0"},
	"1": {"1"},
	"2": {"A", "B", "C"},
	"3": {"D", "E", "F"},
	"4": {"G", "H", "I"},
	"5": {"J", "K", "L"},
	"6": {"M", "N", "O"},
	"7": {"P", "Q", "R", "S"},
	"8": {"T", "U", "V"},
	"9": {"W", "X", "Y", "Z"},
}

const VERSION = "1.0.0"

//////////////////////////////////////////////////////////////////////////////
func recursive(starting, remaining string) []string {
	focus := remaining[0:1]
	tail := ""

	if len(strings.TrimSpace(remaining)) > 0 {
		tail = remaining[1:]
	}

	chars := T9[focus]
	if len(chars) < 1 {
		chars = []string{focus}
	}

	result := make([]string, 0)

	for _, char := range chars {
		stem := starting + char

		if len(tail) > 0 {
			result = append(result, recursive(stem, tail)...)
		} else {
			result = append(result, stem)
		}
	}

	return result
}

//////////////////////////////////////////////////////////////////////////////
func main() {
	_, _ = fmt.Fprintf(os.Stderr, "### Starting recursive.go %s\n", VERSION)

	scanner := bufio.NewScanner(os.Stdin)

	for scanner.Scan() {
		digits := strings.TrimSpace(scanner.Text())
		result := recursive("", digits)
		rCount := len(result)
		plural := "s"

		if rCount == 1 {
			plural = ""
		}

		fmt.Printf("%-25s%7d result%s\n", digits, rCount, plural)

		count := 79 / float64(len(digits)+1)

		for len(result) > 0 {
			fmt.Print(" ")

			for i := 0; float64(i) < count; i++ {
				if len(result) < 1 {
					break
				}

				fmt.Printf(" %s", result[0])
				result = result[1:]
			}

			fmt.Println()
		}
	}

	_, _ = fmt.Fprintf(os.Stderr, "### Finished recursive.go %s\n", VERSION)
}
