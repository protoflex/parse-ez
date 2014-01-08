# Parse-EZ : Clojure Parser Library

[api]: http://www.protoflex.com/parse-ez/api-doc/protoflex.parse-api.html "Parse-EZ API"
[API Documentation][api]

Parse-EZ is a parser library for Clojure programmers. It allows easy
mixing of declarative and imperative styles and does not 
require any special constructs, macros, monads, etc. to write custom parsers. 
All the parsing is implemented using regular Clojure functions.

The library provides a number of
parse functions and combinators and comes with a built-in customizable infix
expression parser and evaluator. It allows the programmer to concisely specify
the structure of input text using Clojure functions and easily build parse trees
without having to step out of Clojure.  Whether you are writing a parser
for some well structured data or for data scraping or prototyping a new language, 
you can make use of this library to quickly create a parser.

## Features

- Parse functions and Combinators
- Automatic handling of whitespaces, comments
- Marking positions and backtracking
- Seek, read, skip string/regex patterns
- Builtin customizable expression parser and evaluator
- Exceptions based error handling
- Custom error messages

## Usage

### Installation
Just add Parse-EZ as a dependency to your lein project

```clojure
[protoflex/parse-ez "0.4.2"]
```
and run

```clojure
lein deps
```

## A Taste of Parse-EZ

Here are a couple of sample parsers to give you a taste of the parser library.

### CSV Parser

A CSV file contains multiple records, one-record per line, with field-values separated by a delimiter
such as a comma or a tab.  The field values may optionally be quoted either using a single or double
quotes.  When field-values are quoted, they may contain the field-delimiter characters, and in such
cases they will not be treated as field separators.


First, let us define a parse function for parsing one-line of csv file:

```clojure
(defn csv-1 [sep] 
    (sep-by #(any-string sep) #(chr sep)))
```
In the above function definition, we make use of the parse combinator `sep-by`
which takes two arguments: the first one to read a field-value and the second
one to read the separator.  Here, we have used Clojure's anonymous function shortcuts to
specify the desired behavior succinctly.  The `any-string` function matches a single-quoted
string or a double-quoted string or a plain-string that is followed by the specified separator
`sep`.  This is exactly the function that we need to read the field-value.  The second argument
provided to `sep-by` above uses the primitive parse function `chr` which succeeds only when
the next character in the input matches its argument (`sep` parameter in this case).  The _csv-1_ function returns the field values as a vector.

The `sep-by` function actually takes a third, optional argument as record-separator
function with the default value of a function that matches a newline.  We didn't
pass the third argument above because the default behavior suits our purpose.
Had the default behavior of `sep-by` been different, we would have written the
above function as:

```clojure
(defn csv-1 [sep] 
    (sep-by #(any-string sep) #(chr sep) #(regex #"\r?\n")))
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
trimming functionality using the `no-trim` combinator.

```clojure
(defn csv [sep] 
    (multi* (fn [] (no-trim #(csv-1 sep)))))
```

Alternatively, you can express the above function a bit more easily using the macro versions of combinators introduced in Version 0.3.0 as follows:

```clojure
(defn csv [sep] 
    (multi* (no-trim_ (csv-1 sep))))
```

Now, let us try out our csv parser. First let us define a couple of test 
strings containing a couple of records (lines) each.  Note that the second 
string contains a comma inside the first cell (a quoted string).  

```clojure
user> (def s1 "1abc,def,ghi\n2jkl,mno,pqr\n")
#'user/s1
user> (def s2 "'1a,bc',def,ghi\n2jkl,mno,pqr\n")
#'user/s2
user> (parse #(csv \,) s1)
[["1abc" "def" "ghi"] ["2jkl" "mno" "pqr"]]
user> (parse #(csv \,) s2)
[["1a,bc" "def" "ghi"] ["2jkl" "mno" "pqr"]]
user> 
```

Well, all we had to do was to write two lines of Clojure code to implement the CSV parser.
Let's add a bit more functionality: the CSV files may use a comma or a tab character to
separate the field values.  Let's say we don't know ahead of time which character
a file uses as a separator and we want to detect the separator automatically.  Note
that both characters may occur in a data file, but only one acts as a field-separator -- that too
only when it's not inside a quoted string.

Here is our strategy to detect the separator:

- if the first field value is quoted (single or double), read the quoted string
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

Let's try out the new auto-detect functionality. Let us define two new test
strings `s3` and `s4` that use `tab` character as field-separator.

```clojure
user> (use 'protoflex.examples.csv_parse)
nil
user> (def s3 "1abc\tdef\tghi\n2jkl\tmno\tpqr\n")
#'user/s3
user> (def s4 "'1a\tbc'\tdef\tghi\n2jkl\tmno\tpqr\n")
#'user/s4
user> (parse csv s3)
[["1abc" "def" "ghi"] ["2jkl" "mno" "pqr"]]
user> (parse csv s4)
[["1a\tbc" "def" "ghi"] ["2jkl" "mno" "pqr"]]
user> (parse csv s1)
[["1abc" "def" "ghi"] ["2jkl" "mno" "pqr"]]
user> 
```

As you can see, this time we didn't specify what field-separator to use: the parser
itself detected the field-separator character and used it, returning us the desired
results.

### XML Parser

Here is the listing of a sample XML parser implemented using Parse-EZ. You can find the
source file in the examples directory.  The parser returns a map containing keys and values
for `:tag`, `:attributes` and `:children` for the root element.  The value for `:attributes` key
is itself another map containing attribute names and their values.  The value for `:children`
key is a vector (potentially empty) containing string content and/or maps for child elements.

```clojure
(ns protoflex.examples.xml_parse
  (:use [protoflex.parse]))

(declare pi prolog element attributes children-and-close cdata elem-or-text close-tag)

(defn parse-xml [xml-str]
  (parse #(between prolog element pi) xml-str :blk-cmt-delim ["<!--" "-->"] :line-cmt-start nil))

(defn- pi [] (while (starts-with? "<?") (skip-over "?>")))

(defn- prolog [] (pi) (attempt  #(regex #"(?s)<!DOCTYPE([^<]+?>)|(.*?\]\s*>)")) (pi))
```
The function _parse-xml_ is the entry point that kicks off parsing of input xml string _xml-str_.  It passes the _between_ combinator to __Parse-EZ__'s _parse_ function. Here, the call to _between_ returns the value returned by the _element_ parse function, ignoring the content surrounding it (matched by _prolog_ and _pi_ functions). The block-comment delimiters are set to match XML's and the line-comment delimiter is cleared (by default these match Java comments).

The parse function _pi_ is used to skip consecutive processing instructions by using the delimiters __<?__ and __?>__.

The parse function _prolog_ is used to skip DTD declaration (if any) and also any surrounding processing instructions.  Note that the regex used to match DTD declaration is only meant for illustration purposes. It isn't complete but will work in most cases.

```clojure
(def name-start ":A-Z_a-z\\xC0-\\xD6\\xD8-\\xF6\\xF8-\\u02FF\\u0370-\\u037D\\u037F-\\u1FFF\\u200C-\\u200D\\u2070-\\u218F\\u2C00-\\u2FEF\\u3001-\\uD7FF\\uF900-\\uFDCF\\uFDF0-\\uFFFD")

(def name-char (str name-start "\\-.0-9\\xB7\\u0300-\\u036F\\u203F-\\u2040"))

(def name-re (-> (format "[%s][%s]*" name-start name-char) re-pattern))
```
_name-re_ is a regular expression that matches xml element and attribute names.

```clojure
(defn element []
  (let [tag (do (chr \<) (regex name-re))
        attrs (attributes)
        children (look-ahead* [
                               ">" #(children-and-close tag)
                               "/>" (fn [] [])])]
    {:tag tag, :attributes attrs, :children children}))
```
The _element_ parse function matches an xml element and returns the tag, attribute list and children in a hash map. Note the usage of the _look_ahead*_ combinator to handle both the cases -- with children and without children. If it sees a ">" after reading the attributes, the _look-ahead*_ function calls the _children-and-close_ parse function to read children and the element close tag. On the other hand, if it sees "/>" after the attributes, it calls the (almost) empty parse function that simply returns an empty list.

```clojure
(defn attr []
  (let [n (regex name-re) _ (chr \=)
        v (any sq-str dq-str)]
    [n v]))

(defn attributes [] (apply hash-map (flatten  (multi* attr))))
```
The _attr_ parse function matches a single attribute. The attribute value may be
a single-quoted or double-quoted string. Note the usage of _any_ parse combinator for this purpose.

The _attributes_ parse function matches multiple attribute specifications by passing the _attr_ parse function to _multi*_ parse combinator.

```clojure
(defn- children-and-close [tag]
  (let [children (multi* #(between pi elem-or-text pi))]
    (close-tag tag)
    children))
```
Each child item is read using the _elem-or-text_ parse function while ignoring any surrounding processing instructions using the _between_ combinator; the combinator _multi*_ is used to read all the child items.

```clojure
(defn- elem-or-text []
  (look-ahead [
               "<![CDATA[" cdata
               "</" (fn [] nil)
               "<" element
               "" #(read-to "<")]))
```
The _look-ahead_ parse combinator is used to call different parse functions
based on different lookahead strings. Note that the _look-ahead_ function 
doesn't consume the lookahead string unlike the _look-ahead*_ function used
earlier (in the definition of _element_ parse function).

```clojure
(defn- cdata []
  (string "<![CDATA[")
  (let [txt (read-to "]]>")] (string "]]>") txt))

(defn- close-tag [tag]
    (string (str "</" tag))
    (chr \>))
```
By now, it should be obvious what the above two functions do.

Well, an XML parser in under 50 lines.  Let's try it with a few sample inputs:

```clojure
user> (use 'protoflex.examples.xml_parse)
nil
user> (parse-xml "<abc>text</abc>")
{:tag "abc", :attributes {}, :children ["text"]}
user> (parse-xml "<abc a1=\"1\" a2=\"attr 2\">sample text</abc>")
{:tag "abc", :attributes {"a1" "1", "a2" "attr2"}, :children ["sample text"]}
user> (parse-xml "<abc a1=\"1\" a2=\"attr 2\"><def d1=\"99\">xxx</def></abc>")
{:tag "abc", :attributes {"a1" "1", "a2" "attr2"}, :children [{:tag "def", :attributes {"d1" "99"}, :children ["xxx"]}]}
user> 
```

## Comments and Whitespaces

By default, Parse-EZ automatically handles comments and whitespaces. This
behavior can be turned on or off temporarily using the macros `with-trim-on`
and `with-trim-off` respectively. The parser option `:auto-trim` can be used to
enable or disable the auto handling of whitespace and comments.  Use the parser
option `:blk-cmt-delim` to specify the begin and end delimiters for block
comments.  The parser option `:line-cmt-start` can be used to specify the line
comment marker.  By default, these options are set to java/C++ block and line
comment markers respectively.  You can alter the whitespace recognizer by setting
the `:ws-regex` parser option.  By default it is set to `#"\s+"`.

Alternatively, you can turn off auto-handling of whitespace and comments and use
the `lexeme` function which trims the whitespace/comments after application of the
parse-function passed as its argument.

Also see the `no-trim` and `no-trim-nl` functions.

## Primitive Parse Functions

Parse-EZ provides a number of primitive parse functions such as: `chr`, 
`chr-in`, `string`, `string-in`, `word`, `word-in`, `sq-str`,  `dq-str`, 
`any-string`, `regex`, `read-to`, `skip-over`, `read-re`, `read-to-re`, 
`skip-over-re`, `read-n`, `read-ch`, `read-ch-in-set`, etc.
[See API Documentation][api]

Let us try some of the builtin primitive parse functions:

```clojure
user> (use 'protoflex.parse)
nil
user> (parse integer "12")
12
user> (parse decimal "12.5")
12.5
user> (parse #(chr \a) "a")
\a
user> (parse #(chr-in "abc") "b")
\b
user> (parse #(string-in ["abc" "def"]) "abc")
"abc"
user> (parse #(string-in ["abc" "def"]) "abcx")
Parse Error: Extraneous text at line 1, col 4
  [Thrown class java.lang.Exception]
```

Note the parse error for the last parse call. By default, the `parse` function parses to the
end of the input text.  Even though the first 3 characters of the input text is recognized
as valid input, a parse error is generated because the input cursor would not be at the
end of input-text after recognizing "abc".

The parser option `:eof` can be set to false to allow recognition of partial input:

```clojure
user> (parse #(string-in ["abc" "def"]) "abcx" :eof false)
"abc"
user> 
```

You can start parsing by looking for some marker patterns using the `read-to`,
`read-to-re`, `skip-over`, `skip-over-re` functions.

```clojure
user> (parse #(do (skip-over ">>") (number)) "ignore upto this>> 456.7")
456.7
```

## Parse Combinators

Parse Combinators in Parse-EZ are higher-order functions that take other parse
functions as input arguments and combine/apply them in different ways to 
implement new parse functionality.  Parse-EZ provides parse combinators such as:
`opt`, `attempt`, `any`, `series`, `multi\*`, `multi+`, `between`, `look-ahead`, `lexeme`,
`expect`, etc.
[See API Documentation][api]

Let us try some of the builtin parse combinators:

```clojure
user> (parse #(opt integer) "abc" :eof false)
nil
user> (parse #(opt integer) "12")
12
user> (parse #(any integer decimal) "12")
12
user> (parse #(any integer decimal) "12.3")
12.3
user> (parse #(series integer decimal integer) "3 4.2 6")
[3 4.2 6]
user> (parse #(multi* integer) "1 2 3 4")
[1 2 3 4]
user> (parse #(multi* (fn [] (string-in ["abc" "def"]))) "abcabcdefabc abcdef")
["abc" "abc" "def" "abc" "abc" "def"]
user> 
```

You can create your own parse functions on top of primitive parse-functions and/or
parse combinators provided by Parse-EZ.

## Committing to a particular parse branch

Version 0.4.0 added support for committing to a particular parse branch via
the new parse combinators `commit` and `commit-on`. These functions make the 
parser commit to the current parse branch, making the parser report subsequent
parse-failures in the current branch as parse-errors and preventing it 
from trying other alternatives at higher levels.

## Nesting Parse Combinators Using Macros

Version 0.3.0 of Parse-EZ adds macro versions of parse combinator functions
to make it easy to nest calls to parse combinators without having to write
nested anonymous functions using the "(fn [] ...)" syntax. Note that Clojure
does not allow nesting of anonymous functions of "#(...)" forms.  Whereas
the existing parse combinators take parse functions as arguments and actually
perform parsing and return the parse results, the newly added macros take 
parse expressions as arguments and return parse functions (to be passed 
to other parse combinators).  These macros are named the same as the 
corresponding parse combinators but with an underscore ("\_") suffix. For example
the macro version of "any" is named "any_".

## Error Handling

Parse Errors are handled in Parse-EZ using Exceptions.  The default error messages generated
by Parse-EZ include line and column number information and in some cases what is expected
at that location.  However, you can provide your own custom error messages by using the
`expect` parse combinator.

## Expressions

Parse-EZ includes a customizable expression parser `expr` for parsing expressions in infix
notation and an expression evaluator function `eval-expr` to evaluate infix expressions.
You can customize the operators, their precedences and associative properties using
`:operators` option to the `parse` function.  For evaluating expressions, you can optionally
specify the functions to invoke for each operator using the `:op-fn-map` option.

## Parser State

The parser state consists of the input cursor and various parser options (specified or derived)
such as those affecting whitespace and comment parsing, word recognizers, expression parsing,
etc.  The parser options can be changed any time in your own parse functions using `set-opt`.

Note that most of the parse functions affect Parser state (e.g: input cursor) and hence they are
not pure functions.  The side-effects could be avoided by making the Parser State an explicit
parameter to all the parse functions and returning the changed Parser State along with the parse
value from each of the parse functions.  However, the result would be a significantly programmer
unfriendly API.  We made a design decision to keep the parse fuctions simple and easy to use
than to fanatically keep the functions "pure".

## Relation to Parsec

Parsec is a popular parser combinator library written in Haskell. While Parse-EZ
makes use of some of the ideas in there, it is *not* a port of Parsec to Clojure.

## License

Copyright (C) 2012 Protoflex Software

Distributed under the Eclipse Public License, the same as Clojure.
