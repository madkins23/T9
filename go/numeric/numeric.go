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
// using numeric algorithm and write the set of results to STDOUT.

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
func numeric(digits string) []string {
	columns := make([][]string, len(digits))
	maximum := 1

	// Build the digit base map and character mapping
	// based on the characters mapped from the original string.
	for index, char := range digits {
		columns[index] = T9[string(char)]

		if len(columns[index]) < 1 {
			columns[index] = []string{string(char)}
		}

		maximum *= len(columns[index])
	}

	result := make([]string, 0)

	// Now just count.
	for i := 0; i < maximum; i++ {
		number := i
		temp := make([]string, 0)

		// From right to left pick character based on modulus by divisor
		// and divide number by divisor.
		for col := len(columns) - 1; col >= 0; col-- {
			chars := columns[col]
			divisor := len(chars)

			temp = append([]string{chars[number%divisor]}, temp...)
			number /= divisor
		}

		result = append(result, strings.Join(temp, ""))
	}

	return result
}

//////////////////////////////////////////////////////////////////////////////
func main() {
	_, _ = fmt.Fprintf(os.Stderr, "### Starting numeric.go %s\n", VERSION)

	scanner := bufio.NewScanner(os.Stdin)

	for scanner.Scan() {
		digits := strings.TrimSpace(scanner.Text())
		result := numeric(digits)
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

	_, _ = fmt.Fprintf(os.Stderr, "### Finished numeric.go %s\n", VERSION)
}
