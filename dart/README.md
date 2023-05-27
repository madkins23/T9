# T9 Go Solution

## Go

`Dart` is an open source programming language created at Google.
While Dart is a general-purpose language, it's main usage is in Flutter,
"an open source framework by Google for building beautiful, natively compiled,
multi-platform applications from a single codebase."

## Installation

Acquire the code for this project in some normal fashion left as an exercise
for the reader.
There is no installation step past cloning the directory to your host.

## Usage

Change to the `T9/dart` directory in your favorite command shell.

There are three stand-alone programs corresponding to the three known algorithms:

* `numeric/numeric.dart`
* `odometer/odometer.dart`
* `recursive/recursive.dart`

These programs are in the `bin` directory.

Type in digit strings after the program starts (no prompt) and see what happens:

    $ dart run recursive.dart < ../test/digits
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

The `test.sh` shell script executes a simple test against prepared data
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