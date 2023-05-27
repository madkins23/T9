# T9 Go Solution

## Go

`Go` is an open source programming language created at Google.
It is an open source project designed to make programmers more productive.
Go is not object-oriented, preferring composition instead.
It features a simple mechanism for generating and working with
lightweight multitasking via `goroutines`.

## Installation

Acquire the code for this project in some normal fashion left as an exercise
for the reader.
There is no installation step past cloning the directory to your host.

## Usage

Change to the `T9/go` directory in your favorite command shell.

There are three stand-alone programs corresponding to the three known algorithms:

* `numeric/numeric.go`
* `odometer/odometer.go`
* `recursive/recursive.go`

> The `go` language will not support two scripts in the same directory.
The entry point for each script is the `main()` routine in the `main` package.
Thus multiple top-level scripts will be in conflict.
This is why the scripts are in separate subdirectories.

Each program filters from `STDIN` to `STDOUT`.
Each line of input is a string made up of digits.
Output will be multiple lines of results for each line of input.

Type in digit strings and see what happens:

    $ go run recursive/recursive.go
    ### Starting recursive
    5678
    5678                         108 results
      JMPT JMPU JMPV JMQT JMQU JMQV JMRT JMRU JMRV JMST JMSU JMSV JNPT JNPU JNPV JNQT
      JNQU JNQV JNRT JNRU JNRV JNST JNSU JNSV JOPT JOPU JOPV JOQT JOQU JOQV JORT JORU
      JORV JOST JOSU JOSV KMPT KMPU KMPV KMQT KMQU KMQV KMRT KMRU KMRV KMST KMSU KMSV
      KNPT KNPU KNPV KNQT KNQU KNQV KNRT KNRU KNRV KNST KNSU KNSV KOPT KOPU KOPV KOQT
      KOQU KOQV KORT KORU KORV KOST KOSU KOSV LMPT LMPU LMPV LMQT LMQU LMQV LMRT LMRU
      LMRV LMST LMSU LMSV LNPT LNPU LNPV LNQT LNQU LNQV LNRT LNRU LNRV LNST LNSU LNSV
      LOPT LOPU LOPV LOQT LOQU LOQV LORT LORU LORV LOST LOSU LOSV
    ### Finished recursive

Use `<ctrl>-D` to end the input stream or `<ctrl>-C` to kill the program.

## Testing

The `test.sh` shell script in this directory executes a simple test against prepared data
using all three scripts:

    $ test.sh 
    ### Starting numeric.go 1.0.0
    ### Finished numeric.go 1.0.0
    Files /home/marc/Work/T9/go/../test/results and - are identical
    ### Starting odometer.go 1.0.0
    ### Finished odometer.go 1.0.0
    Files /home/marc/Work/T9/go/../test/results and - are identical
    ### Starting recursive.go 1.0.0
    ### Finished recursive.go 1.0.0
    Files /home/marc/Work/T9/go/../test/results and - are identical
