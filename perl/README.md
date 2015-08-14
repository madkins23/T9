# T9 Perl Solution

## Perl

Provide a script that filters strings of digits in `STDIN`
to sets of alphanumeric strings in `STDOUT`.

> Perl is my "go to" language for any kind of scripting and hacking around.
I get more work done in less time than in any other language.

## Installation

Acquire the code for this project in some normal fashion left as an exercise
for the reader.
There is no installation step past cloning the directory to your host.

## Usage

Change to the `T9/perl` directory in your favorite command shell.

The main program is `t9` which filters from `STDIN` to `STDOUT`.
Each line of input is a string made up of digits.
Output will be multiple lines of results for each line of input.

Run `t9` with a `--method` argument to specify the conversion method:

    $ perl t9 --method=numeric
    $ perl t9 --method=odometer
    $ perl t9 --method=recursive

Type in digit strings and see what happens:

    $ perl t9 --test
    ### Starting [appname] 0.0.1
    5678
    5678                         108 results
      JMPT JMPU JMPV JMQT JMQU JMQV JMRT JMRU JMRV JMST JMSU JMSV JNPT JNPU JNPV JNQT
      JNQU JNQV JNRT JNRU JNRV JNST JNSU JNSV JOPT JOPU JOPV JOQT JOQU JOQV JORT JORU
      JORV JOST JOSU JOSV KMPT KMPU KMPV KMQT KMQU KMQV KMRT KMRU KMRV KMST KMSU KMSV
      KNPT KNPU KNPV KNQT KNQU KNQV KNRT KNRU KNRV KNST KNSU KNSV KOPT KOPU KOPV KOQT
      KOQU KOQV KORT KORU KORV KOST KOSU KOSV LMPT LMPU LMPV LMQT LMQU LMQV LMRT LMRU
      LMRV LMST LMSU LMSV LNPT LNPU LNPV LNQT LNQU LNQV LNRT LNRU LNRV LNST LNSU LNSV
      LOPT LOPU LOPV LOQT LOQU LOQV LORT LORU LORV LOST LOSU LOSV
    ### Finished [appname] 0.0.1

Use <ctrl>-D to end the input stream or <ctrl>-C to kill the program.

## Testing

There is a small set of tests in file `digits` and a file of results in `results`.
Test a method using:

    $ perl t9 --method=odometer  < ../test/digits | diff -s ../test/results -

Each of the calculation methods should match the same results.

It is also possible to test all of the methods against each other:

    $ perl t9 --test < ../test/digits

For each line of input all of the methods will be run and the results compared.
If the results for different methods don't match then an error message will be generated.
Otherwise the output will be the same as for the other execution patterns.
