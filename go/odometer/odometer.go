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
// using odometer algorithm and write the set of results to STDOUT.

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

type Wheel struct {
	click int
	chars []string
}

//////////////////////////////////////////////////////////////////////////////
func odometer(digits string) []string {
	wheels := make([]Wheel, 0)

	// Build the 'odometer' with different wheels at each position
	// based on the characters mapped from the original string.
	for _, character := range digits {
		chars := T9[string(character)]
		wheels = append(wheels, Wheel{
			click: 0,
			chars: chars,
		})
	}

	result := make([]string, 0)

	for {
		//  Next string is from current state of 'odometer'.
		alpha := ""
		for _, wheel := range wheels {
			alpha = alpha + wheel.chars[wheel.click]
		}
		result = append(result, alpha)

		// Start clicking wheels on the right.
		which := len(wheels) - 1

		for which >= 0 {
			wheel := &wheels[which]
			wheel.click++

			if wheel.click < len(wheel.chars) {
				break
			}

			// Zero this wheel and focus on the one to the left.
			wheel.click = 0
			which--
		}

		if which < 0 {
			break
		}
	}

	return result
}

//////////////////////////////////////////////////////////////////////////////
func main() {
	_, _ = fmt.Fprintf(os.Stderr, "### Starting odometer.go %s\n", VERSION)

	scanner := bufio.NewScanner(os.Stdin)

	for scanner.Scan() {
		digits := strings.TrimSpace(scanner.Text())
		result := odometer(digits)
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

	_, _ = fmt.Fprintf(os.Stderr, "### Finished odometer.go %s\n", VERSION)
}
