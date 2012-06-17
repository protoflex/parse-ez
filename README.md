# Parse-EZ

Parse-EZ is a parser library for Clojure programmers. It provides a number of
parse functions and combinators and comes with a built-in customizable infix
expression parser and evaluator. It allows the programmer to concisely specify
the structure of input text using clojure functions and easily build parse trees
without having to step out of Clojure.  Whether you are implementing some sort
of screen-scraping, data parsing or implementing a language, you can make use
of this library to quickly create a parser.

## Features

- Parse functions and Combinators
- Automatic handling of whitespaces, comments
- Marking positions and Backtracking
- Seek, read, skip string or regular expression patterns
- Builtin customizable expression parser and evaluator
- Exceptions based error handling
- Custom error messages

## Usage

### Installation
Installation is quite simple.  Just add Parse-EZ as a dependency to your lein
project

```clojure
[parse-ez "0.2.0"]
```
and run

```clojure
lein deps
```

### Comments and Whitespaces

### Primitive Parse Functions

### Parse Combinators

Parse Combinators in Parse-EZ are higher-order functions that take other parse
functions as input arguments and combine/apply them in different ways to implement
new parse functionality.

### Error Handling

### Expressions

## Examples

### CSV Parser

### XML Parser

## Relation to Parsec

Parsec is a popular parser combinator library written in Haskell. While Parse-EZ
makes use of some of the ideas in there, it is *not* a port of Parsec to Clojure.

## License

Copyright (C) 2012 Protoflex Technologies Pvt. Ltd.

Distributed under the Eclipse Public License, the same as Clojure.
