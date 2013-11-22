# Parse-EZ Release Notes

## Version 0.3.0

### Main Additions
Version 0.3.0 of Parse-EZ adds macro versions of parse combinator functions
to make it easy to nest calls to parse combinators without having to write
nested anonymous functions using the "(fn [] ...)" syntax. Note that Clojure
does not allow nesting of anonymous functions of "#(...)" forms.  Whereas
the existing parse combinators take parse functions as arguments and actually
perform parsing and return the parse results, the newly added macros take 
parse expressions as arguments and return parse functions (to be passed 
to other parse combinators).  These macros are named the same as the 
corresponding parse combinators but with an underscore ("\_") suffix. For example
the macro version of `any` is named `any_`.

### Miscellaneous Changes

The following minor changes/additions are made in the current release:

- regex function now also accepts string regular expressions (in addition
  to patterns typically passed using #"..." forms).  However, if you use
  strings, you would have to escape backslashes as you would do in Java.

## Version 0.3.5

- Added `sep-by*` function with a slightly different behavior from `sep-by`
- Bug fixes for `with-trim-on` and `with-trim-off`; they were not properly 
  resetting the auto-trim option to original value.

## Version 0.4.1

- Added functionality to commit to a particular parse branch and prevent
  the parser from trying alternatives at higher levels on parse-failure.
  See the new `commit` and `commit-on` functions.

- Other new functions: `with-follow`, `with-follow*`, `with-no-follow`,
  `ident`, `key-word`, `semi` `comma`, `dot`, `colon`, `popen`, `pclose`,
  `bopen`, `bclose`, `sqopen`, `sqclose`, `aopen`, `aclose`, and `equal`.

- Bug fixes for `sep-by`, `any-string` and `line-cmt`.

- Leiningen coordinates: group-id 'protoflex' should be included now.
  Eg: [protoflex/parse-ez 0.4.1], instead of [parse-ez 0.3.6].
