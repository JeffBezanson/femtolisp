[![Join the chat at https://gitter.im/FemtoLisp/flisp](https://badges.gitter.im/FemtoLisp/flisp.svg)](https://gitter.im/FemtoLisp/flisp)

## ...a purely symbolic gesture...

This project began with an attempt to write the fastest lisp interpreter I could in under 1000 lines of C. It snowballed from there as I kept trying to see if I could add powerful features with minimal code. At the same time I assembled a library of some of my favorite C code (by myself and others) to use as a base for a standard library. This includes `ios`, a replacement for parts of C's stdio that adds more flexible features.

Before you say "oh no, another lisp", consider the following: femtolisp is about 150kb, is very self-contained, and has the following features:

  * vectors, strings, gensyms
  * backquote
  * exceptions
  * printing and reading circular/shared structure
  * all values can be printed readably
  * prettyprinting
  * hash tables
  * support for directly using C data types ala Python's ctypes
  * `equal` and ordered comparison predicates that work on circular structure
  * proper tail recursion
  * io and memory streams with utf8 support
  * highly compatible with Scheme, including some `R6RS` features
  * simple, well-organized, powerful API with as few functions as possible
  * compacting GC
  * and...

...it is fast, ranking among the fastest non-native-compiled Scheme implementations. It achieves this level of speed even though many primitives (e.g. `filter` and `for-each`) are written in the language instead of C. femtolisp uses a bytecode compiler and VM, with the compiler written in femtolisp. Bytecode is first-class, can be printed and read, and is "human readable" (the representation is a string of normal low-ASCII characters).

femtolisp is a simple, elegant Scheme dialect. It is a lisp-1 with lexical scope. The core is 12 builtin special forms and 33 builtin functions.

A primary design goal is to keep the code concise and interesting. I strive to have each concept implemented in just one place, so the system is easy to understand and modify. The result is high reliability, because there are fewer places for bugs to hide. You want a small core of generically useful features that work _really well_ (for example, see `torture.scm`).

Almost everybody has their own lisp implementation. Some programmers' dogs and cats probably have _their_ own lisp implementations as well. This is great, but too often I see people omit some of the obscure but critical features that make lisp uniquely wonderful. These include read macros like `#.` and backreferences, gensyms, and properly escaped symbol names. If you're going to waste everybody's time with yet another lisp, at least do it right damnit.

Another design goal is to avoid spurious novelties. Many others offering their own "shiny new" lisp dialects get carried away and change anything that strikes their fancy. These changes have no effect except incompatibility, and often make the language worse because the new design was not as carefully thought out and has not stood the test of time. For example, how does it help to remove backquote? One design changes the syntax of `quote`. Some systems disallow dotted lists. (I've seen all three of these.) What's the point? Implementers wave the banner of "simplicity", yet wedge in all kinds of weird implicit behaviors and extra evaluation rules.

Lately a surprising amount of FUD has been spread about proper tail recursion. I agree that not every language needs it, but I would like to refute the idea that it makes interpreters slow. Look at the "tiny" subdirectory or the "interpreter" branch to see a pure s-expr interpreter with efficient proper tail calls. All you have to do is keep track of whether you're in tail position, which can be done very cheaply. These interpreters are difficult to beat for speed, yet they have lexical scope and proper tail calls.

This project is mostly a matter of style. Look at the code and you'll understand.

This is what I do for fun, because it is the _exact opposite_ of the kind of thing people will pay for: an obscure implementation of a programming language everybody hates.
