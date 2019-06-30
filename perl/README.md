# T9 Perl Solution

## Perl

Perl is a general-purpose scripting language with its roots in shell scripting.
Once a contender for the 'P' in LAMP, it has been replaced there with PHP or Python.

## Installation

Acquire the code for this project in some normal fashion left as an exercise
for the reader.
There is no installation step past cloning the directory to your host.

## Usage

Change to the `T9/perl` directory in your favorite command shell.

There are three stand-alone programs corresponding to the three known algorithms:

* `numeric.pl`
* `odometer.pl`
* `recursive.pl`

Each program filters from `STDIN` to `STDOUT`.
Each line of input is a string made up of digits.
Output will be multiple lines of results for each line of input.

Type in digit strings and see what happens:

    $ perl recursive.pl
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

Use `<ctrl>-D` to end the input stream or `<ctrl>-C` to kill the program.

## Testing

The `test.sh` shell script in this directory executes a simple test against prepared data
using all three scripts:

    $ test.sh 
    ### Starting numeric.pl 1.0.0
    ### Finished numeric.pl 1.0.0
    Files /home/marc/Work/T9/perl/../test/results and - are identical
    ### Starting odometer.pl 1.0.0
    ### Finished odometer.pl 1.0.0
    Files /home/marc/Work/T9/perl/../test/results and - are identical
    ### Starting recursive.pl 1.0.0
    ### Finished recursive.pl 1.0.0
    Files /home/marc/Work/T9/perl/../test/results and - are identical

