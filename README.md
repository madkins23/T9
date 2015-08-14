# T9

Map strings of digits to potential alphanumeric character strings
using the T9 telephone keypad.

> This is a favorite interview question.
It is trickier than it looks to most people
resulting in frustration and retries
which are often entertaining to watch
and reveal a lot about a candidate.

## Exercise Description

Consider the mapping from small integer digits to character sets
that is embodied in the T9 keypad on most telephones:

| digit | characters |
| :---: | :---: |
| 0 | 0 |
| 1 | 1 |
| 2 | A B C |
| 3 | D E F |
| 4 | G H I |
| 5 | J K L |
| 6 | M N O |
| 7 | P Q R S |
| 8 | T U V |
| 9 | W X Y Z |

An area code (e.g. `206`) can be mapped through the T9 keypad:

| 2 | 0 | 6 |
| :---: | :---: | :---: |
| A | 0 | M |
| B |   | N |
| C |   | O |

to a set of alphanumeric strings:

    A0M   A0N   A0O
    B0M   B0N   B0O
    C0M   C0N   C0O

The exercise is to write an algorithm to print all possible mappings
generated by a sequence of digits (e.g. `206`) mapped via the T9 keypad.

> This might be useful in trying to memorize a new telephone number.

## Multiple Languages

I am structuring this project to allow multiple solutions in different
programming languages in the fullness of time.
For the moment these include:

* Perl

> Can your favorite programming language do better?

## Multiple Solutions

There are, as one would expect, multiple solutions to this problem.
A common initial reaction is to use some set of loops to walk over
the potential solutions, but that won't generally work.

### Recursive

The recursive solution is most common, and arguably the simplest.
Other solutions represent an unrolling of the recursion,
or rather a more format representation of the state that
is kept in the execution stack in the recursive solution.

> The first thing I'm looking for is that the candidate recognizes
that the problem requires a recursive solution.

The key to the recursive solution is that _two_ things need to be
passed down the recursive routine stack:

* the current partially built result string and
* the remainder of the original digit string.

The top-level call provides an empty string for the first and the
candidate digit string for conversion in the second.

> If a candidate does not find this solution they're usually lost.

### Odometer

The odometer solution provides a data structure for each digit
in the candidate string (and solution string).
Each data structure is like a wheel in mechanical odometer:

* it has a set of legal states,
* it has a _current_ state, and
* rolling a wheel over the top carries over to the wheel to the left.

Once the data structure is created, two nested loops handle
the generation of the result strings from the data structure.
At any point in time a result can be directly generated form
the data structure, after which the counter is tripped.
The inner loop processes the wheels from right to left to
generate the next result in the odometer state.

> No one ever things of this, perhaps mechanical odometers are gone now?

### Numeric

The numeric solution changes generation of solution strings into
a counting problem with a weirdly-based numeric system.
It is similar to the odometer solution but does not store state data.
The state data is directly inferred from a series of integers.

Like the odometer solution there is a preparatory step.
In this case an array of lookup tables is made, one for each digit,
containing the set of possible characters for that digit.
Unlike the odometer solution the current state of each digit is not necessary.
During this step the number of solutions is generated by multiplying the
number of states for each digit together.

The main processing is simply counting from zero to the number of solutions.
At each stage this number directly represents a solution string.

To convert the number to a solution string begin with the number of characters
associated with the rightmost digit.
The remainder of the counting number divided by the number of characters
(or states) for that digit is used to lookup one of the digits.
Then the counting number is divided by that number of states and the remaindered thrown away.

For each digit right to left this remainder/division process is continued.
At each stage a character is looked up from the appropriate lookup map.

> One candidate bumped up against this solution during an interview.
He didn't pursue it but eventually made a recursive solution work.
I would have been more impressed if he had made this work.

## Installation and Usage

Acquire the code for this project in some normal fashion left as an exercise
for the reader.

Installation and usage instructions for individual solutions are packaged in `README.md` files
for each individual language.
