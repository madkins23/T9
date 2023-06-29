# T9 Common Lisp Solution

## Common Lisp

[`Common Lisp`](https://en.wikipedia.org/wiki/Common_Lisp) is a variant of
[`Lisp`](https://en.wikipedia.org/wiki/Lisp_(programming_language)) formalized in 1994.
While the first Lisp was based on mathematical notation,
Common Lisp has a lot of additional functionality including object-oriented extensions.
One of the main features of LISP is the availability of a shell providing a
Read Eval Print Loop (REPL) where code can be loaded and evaluated interactively.

The code in this section was generated for Steel Bank Common Lisp (SBCL)
which can be downloaded for free for various platforms.

## Installation

Acquire the code for this project in some normal fashion left as an exercise
for the reader.
There is no installation step past cloning the directory to your host.

## Usage

Change to the `T9/common-lisp` directory in your favorite command shell.

There are three programs corresponding to the three known algorithms:

* `numeric.lisp`
* `odometer.lisp`
* `recursive.lisp`

Each program requires common code in `infra.lisp`.
When using a REPL this file must be loaded first.

Each program filters from `STDIN` to `STDOUT`.
Each line of input is a string made up of digits.
Output will be multiple lines of results for each line of input.

Type in digit strings and see what happens:

    $ sbcl --noinform --load infra.lisp --load recursive.lisp --eval "(main #'recursive)"
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


Use `<ctrl>-D` to end the input stream and again to exit the SBCL REPL.

## Testing

The `test.sh` shell script in this directory executes a simple test against prepared data
using all three scripts:

    $ test.sh 
    ### Starting recursive
    ### Finished recursive
    Files /home/marc/work/T9/common-lisp/../test/results and - are identical
    ### Starting odometer
    ### Finished odometer
    Files /home/marc/work/T9/common-lisp/../test/results and - are identical
    ### Starting numeric
    ### Finished numeric
    Files /home/marc/work/T9/common-lisp/../test/results and - are identical
