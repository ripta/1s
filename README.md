# 1s 

A toy implementation of a stack-oriented language, where I keep the syntax and
native (rust) implementation minimal. There is a prelude, `lib/prelude.1s`,
that contains definitions in 1s.

1s is named so, because it has a single primary memory stack. It is also
referred to as `one_stack` in parts of the codebase that require a valid
identifier that starts with an alpha character.

The entrypoint is its Makefile, which contains a few useful targets:

```
# To run the rust REPL with a basis library:
make repl

# To run the rust REPL with optimizations:
make repl-opt

# To run a wasm32 REPL in your browser, you'll need to build the wasm32 code
# ("wasm") and start the development server ("web"):
make wasm web
```


## Stack

A stack is described as an ordered list of elements, illustrated from left
(bottom of the stack / BOS) to right (top of the stack / TOS). For example:

```
1 2 3 4
```

denotes a stack with four elements, the integers 1 through 4. The integer 1 is
at the bottom of the stack, the integer 2 is on top of 1, the integer 3 is on
top of 2, and lastly, the integer 4 is at the top of stack.

A stack has a few operations:

* "pop" will take the element at the top of the stack, while preserving the
  rest of the stack;
* "push" will add an element to the top of the stack, while preserving all
  elements already on the stack; and
* "clear" will remove all elements from the stack.

## Stack Elements

An element of the stack may be one of:

* an integer literal, internally represented with a 64-bit signed integer;
* a float literal, interally represented with a 64-bit floating point
  conforming to the binary64 type defined in IEEE 754-2008;
* a string literal, internally represented by a collection of UTF-8 codepoints;
* a symbol, which is a memory representation of an interned string;
* a word, which is internally represented by a pair of string and a block; and
* a block, which is internally represented as a new queue.

## Queue

A queue is described as an ordered list of elements from left (front of the
queue / FOQ) to right (back of the queue / BOQ). A queue has a few operations:

* "dequeue" will take the element at the front of the queue, preserving the
  rest of the queue;
* "prepend" will add an element to the front of the queue, preserving all
  elements already in the queue;
* "enqueue" will add an element to the back of the queue, preserving all
  elements already in the queue; and
* "clear" will remove all elements from the queue.

## Execution Environment

The execution environment for 1s consists of:

* a program queue,
* a memory stack,
* a collection of words, and
* a set of symbols representing interned strings.

The state of the execution environment can be thought of as specific values for
each of the above.

While all parts of the execution environment are important, illustrating all
words and symbols may prove cumbersome and overwhelming to the reader. Instead,
it may be useful to elide everything but the memory stack and program queue.

In fact, the memory stack and the program queue are useful to think
side-by-side. They can be illustrated by placing the stack to the left of a
white diamond `◇` and the program to the right of the white diamond `◇`.

As such, the program queue `1 2 +` containing an integer literal 1, integer
literal 2, and word `+`, with an empty stack can be shown like so:

```
◇ 1 2 +
```

## Program Queue

The program queue is evaluated from the FOQ. Additional program tokens are
enqueued to the BOQ. Each token evaluated affects the stack.

A word causes the word's program to be prepended to the program queue, while
literals cause the literal to be pushed onto the memory stack when evaluated.

## Evaluation

A single evaluation step happens by dequeueing from the program queue, and
performing an operation depending on the token that was dequeued.

For example, in the environment:

```
◇ 1 2 +
```

the next step would be to dequeue the integer literal 1, and then--as is the
case with integer literals--to push the integer literal onto the stack:

```
1 ◇ 2 +
```

The same with the integer literal 2 at the next step:

```
1 2 ◇ +
```

Last but not least is to dequeue the word `+`, which translates to the
operation of popping two elements from the top of the stack, adding them
together, and then pushing the result onto the stack.

Since `+` is a primitive word (described in the next section), its operation is
considered atomic from the point of view of the environment, and as such only
takes one step, resulting in:

```
3 ◇
```

The program halts when no tokens are available in the program queue. The result
of the evaluation is the entire stack.

In our example, the result of the evaluation is the integer literal 3.

Another way to represent the result of the evaluation is with `==`, where the
program to the left of `==` terminates and results in the stack on the right of
the `==`. The above example can be written:

```
1 2 + == 3
```

which shows that the program `1 2 +` is equivalent to `3`. This can be further
generalized by replacing any instance of `1 2 +`, even as part of a different
program, with `3`. For example:

```
4 1 2 + * == 4 3 * == 12
```

because:

```
◇ 4 1 2 + *
4 ◇ 1 2 + *
4 1 ◇ 2 + *
4 1 2 ◇ + *
4 3 ◇ *
12 ◇
```

## Word

A word is either:

* a primitive, which is a word implemented in native (rust) code; or
* a compound, which is a word implemented in 1s itself.

A primitive word, by convention, is surrounded by curly braces `{` and `}`. A
primitive word cannot be overridden at runtime.

A compound word defined in 1s is associated with a program stack, which is the
implementation of the word using other words. There is no theoretical limit as
to the number of compound words that a compound word may rely on. A compound
word may contain alphanumeric characters and symbols. A compound word may
include `{` and `}`, though their use is discouraged to reduce confusion
between primitive words (that cannot be overridden at runtime) and compound
words (that _can_ be overridden at runtime).

Every primitive word has an alias of one or more compound words. These aliases
are defined as part of the prelude. The aliases look like any other compound
word, and may be overridden at runtime.

## Binding and Quoting

Binding is the operation of associating one or more words with a block (program
queue). The binding operation is denoted by `{:}` and is aliased as `:`.

For example, consider the doubling of an integer.

```
1 2 * == 2
3 2 * == 6
9 2 * == 18
```

and so on. Such an operation could be generalized as the word `double`:

```
[ 2 * ] [ double ] :
```

where we associate the program block `[ 2 * ]` to the word `double`. Both the
program and the word being bound must appear in blocks `[` and `]` to prevent
them from being evaluated.

It can then be used like so:

```
1 double == 2
3 double == 6
9 double == 18
```

For illustrative purposes, the evaluation might look like:

```
◇ 9 double
9 ◇ double
9 ◇ 2 *
9 2 ◇ *
18 ◇
```

where the word `double` is expanded to `2 *` when it is evaluated, because
evaluating a word causes the word's program to be prepended to the program
queue.

The fact that the word being bound must also appear in a block is useful to
allow one to bind multiple words to the same program. For example, to allow
`double` to also be used as `times-two`:

```
[ 2 * ] [ double times-two ] :
```

such aliasing has the same effect as:

```
[ 2 * ] [ double ] :
[ double ] [ slower-times-two ] :
```

but is more efficient. `slower-times-two` takes one more evaluation step,
because the word must be expanded from `slower-times-two` to `double` to `2 *`:

```
◇ 9 slower-times-two
9 ◇ slower-times-two
9 ◇ double
9 ◇ 2 *
9 2 ◇ *
18 ◇
```

Because words can contain special characters, including numbers, one can define
words that look like sequences of other words. Discretion is advised.

```
[ 2 * ] [ 2* ] :

◇ 4 2*
4 ◇ 2*
4 ◇ 2 *
4 2 ◇ *
8 ◇
```

## Binary Arithmetic Operations

By convention, binary arithmetic operations are evaluated in reverse order with
respect to the top of the stack. That is,

```
1 2 +
```

is evaluated as `1 + 2`. This technical distinction matters in noncommutative
operations, such as division:

```
12 3 / == 4
```

## Numeric Types

Numeric types are not automatically convertible or casted. Mixing numeric types
will cause the "incompatible value on stack" error:

```
12.0 3 / == #error
Error: incompatible value on stack: expected 'integer', but found 'FloatValue(12.0)'
```

## Errors

Errors automatically halt execution, and leave the program queue and memory
stack as is. There is no automatic way to recover from errors and continue
execution.
