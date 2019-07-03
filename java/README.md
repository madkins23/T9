# T9 Java Solution

## Java

Java is a widely used class-based, object-oriented programming language.
Code is generated for the Java Virtual Machine (JVM), making it portable (as the JVM has been
ported to a wide variety of platforms) and interoperable with other languages that target the JVM.

## Installation

Acquire the code for this project in some normal fashion left as an exercise
for the reader.
There is no installation step past cloning the directory to your host.

## Usage

Change to the `T9/java` directory in your favorite command shell.

There are three stand-alone programs corresponding to the three known algorithms
(all contained in the `src/org/doorways/t9` subdirectory):

* `Numeric.java`
* `Odometer.java`
* `Recursive.java`

Each program filters from `STDIN` to `STDOUT`.
Each line of input is a string made up of digits.
Output will be multiple lines of results for each line of input.

Type in digit strings and see what happens:

    $ java src/org/doorways/t9/Recursive.java
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
    ### Starting Numeric.java 1.0.0
    ### Finished Numeric.java 1.0.0
    Files /home/marc/Work/T9/java/../test/results and - are identical
    ### Starting Odometer.java 1.0.0
    ### Finished Odometer.java 1.0.0
    Files /home/marc/Work/T9/java/../test/results and - are identical
    ### Starting Recursive.java 1.0.0
    ### Finished Recursive.java 1.0.0
    Files /home/marc/Work/T9/java/../test/results and - are identical

