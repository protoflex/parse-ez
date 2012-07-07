# Parse-EZ

[See API Documentation](http://www.protoflex.com/parse-ez/api-doc/index.html "Parse-EZ API")

Parse-EZ is a parser library for Clojure programmers. It provides a number of
parse functions and combinators and comes with a built-in customizable infix
expression parser and evaluator. It allows the programmer to concisely specify
the structure of input text using clojure functions and easily build parse trees
without having to step out of Clojure.  Whether you are writing a parser
for some well structured data or for data scraping or for a new language, 
you can make use of this library to quickly create a parser.

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

By default, Parse-EZ automatically handles comments and whitespaces. This
behavior can be turned on or off temporarily using the macros `with-trim-on`
and `with-trim-off` respectively. The parser option `:auto-trim` can be used to
enable or disable the auto handling of whitespace and comments.  Use the parser
option `:blk-cmt-delim` to specify the begin and end delimiters for block
comments.  The parser option `:line-cmt-start` can be used to specify the line
comment marker.  By default, these options are set to java/C++ block and line
comment markers respectively.  You can alter the whitespace recognizer by setting
the `:word-regex` parser option.  By default it is set to `#"\s+"`.

Alternatively, you can turn off auto-handling of whitespace and comments and use
the `lexeme` function which trims the whitespace/comments after application of the
parse-function passed as its argument.

Also see the `no-trim` and `no-trim-nl` functions.

### Primitive Parse Functions

Parse-EZ provides a number of primitive parse functions such as: `chr`, `chr-in`, `string`,
`string-in`, `word`, `word-in`, `sq-str`,  `dq-str`, `any-string`, `regex`, `read-to`, `skip-over`,
`read-re`, `read-to-re`, `skip-over-re`, `read-n`, `read-ch`, `read-ch-in-set`, etc.
[See API Documentation](http://www.protoflex.com/parse-ez/api-doc/index.html "Parse-EZ API")

### Parse Combinators

Parse Combinators in Parse-EZ are higher-order functions that take other parse
functions as input arguments and combine/apply them in different ways to 
implement new parse functionality.  Parse-EZ provides parse combinators such as:
`opt`, `attempt`, `any`, `series`, `multi*`, `multi+`, `between`, `look-ahead`, `lexeme`,
`expect`, etc.
[See API Documentation](http://www.protoflex.com/parse-ez/api-doc/index.html "Parse-EZ API")

You can create your own parse functions on top of primitive parse-functions and/or
parse combinators provided by Parse-EZ.

### Error Handling

Parse Errors are handled in Parse-EZ using Exceptions.  The default error messages generated
by Parse-EZ include line and column number information and in some cases what is expected
at that location.  However, you can provide your own custom error messages by using the
`expect` parse combinator.

### Expressions

Parse-EZ includes a customizable expression parser `expr` for parsing expressions in infix
notation and an expression evaluator function `eval-expr` to evaluate infix expressions.
You can customize the operators, their precedences and associative properties using
`:operators` option to the `parse` function.  For evaluating expressions, you can optionally
specify the functions to invoke for each operator using the `:op-fn-map`.

### Parser State

The parser state consists of the input cursor and various parser options (specified or derived)
such as those affecting whitespace and comment parsing, word recognizers, expression parsing,
etc.  The parser options can be changed any time in your own parse functions using `set-opt`.

Note that most of the parse functions affect Parser state (e.g: input cursor) and hence they are
not pure functions.  The side-effects could be avoided by making the Parser State an explicit
parameter to all the parse functions and returning the changed Parser State along with the parse
value from each of the parse functions.  However, the result would be a significantly programmer
unfriendly API.  We made a design decision to keep the parse fuctions simple and easy to use
than to fanatically keep the functions "pure".

## Examples

To illustrate some of the features of Parse-EZ and to give a taste of Parse-EZ, a couple of
example parsers are listed below.

### CSV Parser

A CSV file contains multiple records, one-record per line, with field-values separated by a delimiter
such as a comma or a tab.  The field values may be quoted either using a single-quote (') or double
quote ("), in which case the field-values may contain the field-delimiter character, and in such
cases they will not be treated as field separators.

First, let us bring in the symbols in the parse module:

```clojure
(use 'protoflex.parse)
```

Now, let us define a parse function for parsing one-line of csv file:

```clojure
(defn csv-1
  [sep] (sep-by #(any-string sep) #(chr sep)))
```
In the above function definition, we make use of the parse combinator `sep-by`
which takes two arguments: the first one to read a field-value and the second
one to read the separator.  Here, we have used Clojure's anonymous function shortcuts to
specify the desired behavior succinctly.  The `any-string` function matches a single-quoted
string, or a double-quoted string, or a plain-string that is followed by the specified separator
`sep`.  This is exactly the function that we need to read the field-value.  The second argument
provided to `sep-by` above uses the primitive parse function `chr` which succeeds only when
the next character in the input matches its argument (`sep` parameter in this case).  This
function returns the field values as a vector.

The `sep-by` function actually takes a third, optional argument as record-separator function
with the default value of a function that matches a newline.  We didn't pass the third argument
above because the default behavior suits our purpose. Had the default behavior of `sep-by`
been different, we would have written the above function as:

```clojure
(defn csv-1
  [sep] (sep-by #(any-string sep) #(chr sep) #(regex #"\r?\n")))
```

Now that we have created a parse function to parse a single line of CSV
file, let us write another parse function that parses the entire CSV file
content and returns the result as a vector of vector of field values
(one-vector per record/line).  All we need to do is to repeatedly apply the
above defined `csv-1` function and the `multi*` parse combinator does
just that.

Just one small but important detail: by default, Parse-EZ
automatically trims whitespace after successfully applying a parse function.
This means that the newline at the end of line would be consumed after reading
the last field value and the `sep-by` would be unable to match the end-of-line
which is the record-separator in this case.  So, we will disable the newline
trimming functionality using the `no-trim-nl` combinator.

```clojure
(defn csv
  [sep] (multi* (fn [] (no-trim-nl #(csv-1 sep)))))
```

Well, all we had to do was to write two lines of Clojure code to implement the CSV parser.
Let's add a bit more functionality: the CSV files may use a comma or a tab character to
separate the field values.  Let's say we don't know ahead of time which character
a file uses as a separator and we want to detect the separator automatically.  Note
that both characters may occur in a data file, but only one acts as a field-separator.

Here is our strategy to detect the separator:

- if the first field value starts is quoted (single or double), read the quoted string
- else, read until one of comma or tab occurs
- the next char is our delimiter

Here is the code:

```clojure
(defn detect-sep []
    (let [m (mark-pos)
           s (attempt #(any dq-str sq-str))
           s (if s s (no-trim #(read-to-re #",|\t")))
           sep (read-ch)]
       (back-to-mark m)
       sep))
```

Note how we used the `mark-pos` and `back-to-mark` Parse-EZ functions to 'unconsume'
the consumed input.

The complete code for the sample CSV parser with the separator-detection functionality is
listed below (you can find this in `csv_parse.clj` file under the `examples` directory.

```clojure
(ns protoflex.examples.csv_parse
  (:use [protoflex.parse]))

(declare detect-sep csv-1)

(defn csv
  "Reads and returns one or more records as a vector of vector of field-values"
  ([] (csv (no-trim #(detect-sep))))
  ([sep] (multi* (fn [] (no-trim-nl #(csv-1 sep))))))

(defn csv-1
  "Reads and returns the fields of one record (line)"
  [sep] (sep-by #(any-string sep) #(chr sep)))

(defn detect-sep
  "Detects the separator used in a csv file (a comma or a tab)"
  [] (let [m (mark-pos)
           s (attempt #(any dq-str sq-str))
           s (if s s (no-trim #(read-to-re #",|\t")))
           sep (read-ch)]
       (back-to-mark m)
       sep))
```

### XML Parser

## Relation to Parsec

Parsec is a popular parser combinator library written in Haskell. While Parse-EZ
makes use of some of the ideas in there, it is *not* a port of Parsec to Clojure.

## License

Copyright (C) 2012 Protoflex Technologies Pvt. Ltd.

Distributed under the Eclipse Public License, the same as Clojure.
