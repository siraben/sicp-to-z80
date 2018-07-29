# The SICP Register Machine language to Z80 compiler

This is a compiler for a subset of the SICP register machine language
to Z80 assembly, specifically targeted at the TI-84.  Eventually the
goal is to have full support of the SICP instruction set and then use
this compiler to compile Scheme down to Z80, or to directly write a
Scheme to Z80.  Either way, this project was also meant for me to
explore programming in Z80 assembly on the TI-84 (which isn't the
nicest language).

## Features
- Display strings and numbers, save them to registers and the stack
  and restore them.
- 32-bit numbers (oh, sweet over/under-flow!).
- Define procedures to be used in register machine programs.
- All the headache of SICP register machine language without the
  headache of Z80.

## How to use
First, define a program with `define-program` or just plain, old `define`.
Since `print-string` is actually a macro, you must use `,@` (or,
`unquote-splicing`) so that the generated code is placed inline.

```scheme
(define hello-world
  `(,@(print-string "hello, world!")))
```
Then compile it with `compile-prog`.
```scheme
(compile-prog hello-world)
```
To upload this program to your TI-84, you'll need to assemble it. Use an
assembler like [spasm-ng](https://github.com/alberthdev/spasm-ng).

See `compiler.scm` for some more example programs, some of which work,
and some of which don't!

Here's a more complicated program that prints the numbers from 0 to 9.

```scheme
(define counter-prog
  `((assign x (const 0))
    (label dec-loop)
    (increment x)
    (display x)
    (test (op >) (reg x) (const 10))
    (branch (label done))
    (goto (label dec-loop))
    
    (label done)
    (display (const "Succeeded!"))))
```

The following program is the same as the previous one, but it
demonstrates the ability to define _procedures_ to be used in
programs, for structured programming.

```scheme
(define-program add-prog
  (registers (x 10) (y 0))
  
  (instructions
   (save x) ; push x on the stack, this is the argument to the
            ; procedure `print-up-to'

   ,@(call print-up-to) ; call the procedure.
   (display (const "Succeeded!"))

   ; Print the numbers from 1 to n - 1 inclusive.
   ,@(define-procedure (print-up-to)
       (restore y) ; get the argument
       (save x)    ; we're going to clobber x.
       (assign x 0)
       
       (label dec-loop)
       (increment x)
       (display x)
       (test (op >) (reg x) (reg y))
       (branch (label done))
       (goto (label dec-loop))

       (label done)
       (restore x) ; restore the register we clobbered.
       )))
```

## Currently not implemented/Broken
- Equality testing of 32-bit numbers is broken `(test (op =) (reg foo) (reg bar))`.
- Arithmetic operations on 32-bit numbers.
- Numbers larger than 32 bits.
- Type checking!
- cons cells, memory allocation, perhaps GC?
- Multiple-valued tests `(test (op >) (reg foo) (reg bar) (reg baz))`
- Compiler optimizations
  - [ ] Collapse tests with constant values `(test (op =) (const 3) (const 4))`.
  - [ ] Don't allocate unused registers.
  - [ ] Loop unrolling.
  - [ ] Don't compile programs that go into an infinite loop ;)

