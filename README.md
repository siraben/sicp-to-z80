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

## Currently not implemented/Broken
- Equality testing of 32-bit numbers is broken `(test (op =) (reg foo) (reg bar))`.
- Arithmetic operations on 32-bit numbers.
- Numbers larger than 32 bits.
- Type checking!
- cons cells, memory allocation, perhaps GC?
- Multiple-valued tests `(test (op >) (reg foo) (reg bar) (reg baz))`
- Compiler optimizations
  [ ] Collapse tests with constant values `(test (op =) (const 3) (const 4))`.
  [ ] Don't allocate unused registers.
  [ ] Loop unrolling.
  [ ] Don't compile programs that go into an infinite loop!

