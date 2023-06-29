# T9 Racket Solution

## Racket

**TBD**

[`Racket`](https://racket-lang.org/) is a descendant of
[`Scheme`](https://en.wikipedia.org/wiki/Scheme_(programming_language)), a dialect of
[`Lisp`](https://en.wikipedia.org/wiki/Lisp_(programming_language)).
`Racket` is intended as a language for building other languages,
and is packaged as a multi-platform distribution including
an editor `DrRacket` and various derived languages.

The solutions in this section are purposely generated (mostly) using recursive functions
instead of using the more conventional functions such as the `for` loop that are availble.
`Racket` supports [tail recursion](https://en.wikipedia.org/wiki/Tail_call) which optimizes
these calls to avoid creating new stack frames.

## Installation

Acquire the code for this project in some normal fashion left as an exercise
for the reader.
There is no installation step past cloning the directory to your host.

## Usage

Change to the `T9/racket` directory in your favorite command shell.

There are three programs corresponding to the three known algorithms:

* `numeric.rkt`
* `odometer.rkt`
* `recursive.rkt`

Each program requires common code in `infra.rkt`.
When using a REPL this file must be loaded first.

Each program filters from `STDIN` to `STDOUT`.
Each line of input is a string made up of digits.
Output will be multiple lines of results for each line of input.

Type in digit strings and see what happens:

    $ racket -i -t infra.rkt -t recursive.rkt -e '(main recursive)'
    Welcome to Racket v8.9 [cs].
    ### Starting numeric
    5678
    5678                         108 results
      JMPT JMPU JMPV JMQT JMQU JMQV JMRT JMRU JMRV JMST JMSU JMSV JNPT JNPU JNPV JNQT
      JNQU JNQV JNRT JNRU JNRV JNST JNSU JNSV JOPT JOPU JOPV JOQT JOQU JOQV JORT JORU
      JORV JOST JOSU JOSV KMPT KMPU KMPV KMQT KMQU KMQV KMRT KMRU KMRV KMST KMSU KMSV
      KNPT KNPU KNPV KNQT KNQU KNQV KNRT KNRU KNRV KNST KNSU KNSV KOPT KOPU KOPV KOQT
      KOQU KOQV KORT KORU KORV KOST KOSU KOSV LMPT LMPU LMPV LMQT LMQU LMQV LMRT LMRU
      LMRV LMST LMSU LMSV LNPT LNPU LNPV LNQT LNQU LNQV LNRT LNRU LNRV LNST LNSU LNSV
      LOPT LOPU LOPV LOQT LOQU LOQV LORT LORU LORV LOST LOSU LOSV
    ### Finished numeric


Use `<ctrl>-D` to end the input stream and again to exit the Racket REPL.

## Testing

The `test.sh` shell script in this directory executes a simple test against prepared data
using all three scripts:

    $ test.sh
    ### Starting recursive
    ### Finished recursive
    Files /home/marc/work/T9/racket/../test/results and - are identical
    ### Starting odometer
    ### Finished odometer
    Files /home/marc/work/T9/racket/../test/results and - are identical
    ### Starting numeric
    ### Finished numeric
    Files /home/marc/work/T9/racket/../test/results and - are identical
