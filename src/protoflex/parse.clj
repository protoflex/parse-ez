;   Copyright (c) Protoflex Technologies Pvt. Ltd. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{ :doc "Clojure Parser Library." :author "Panduranga Adusumilli"}
    protoflex.parse
  (:import [java.util.regex Matcher])
  (:require [clojure.set :as set]
            [clojure.zip :as zip])
  (:use [protoflex.util]))

(declare parser-init set-pos throw-ex starts-with? move read-ch read-ch-in-set
         qstring unexpected next-n regex to-set read-re next-text
         init-default-ws-reader match-text find-first auto-trim-if get-opt mark-pos
         back-to-mark any-string dq-str sq-str read-to-re at-end? no-trim no-trim-nl
         get-opts set-opt get-default-ops get-default-op-fn-map init-operators
         read-ws string)

(def ^:dynamic *parser-state* (atom []))
(defn state [] (deref *parser-state*))

(def default-options {
                      :blk-cmt-delim ["/*" "*/"]
                      :line-cmt-start "//"
                      :ws-regex #"\s+"
                      :auto-trim true
                      :word-reader #"\S+"
                      :eof true })

(defn parse
  "This function triggers off the parsing of the provided input string using
the specified parse function. The following parser options may be provided to alter
the comments and white space matching behavior:
   :blk-cmt-delim - vector specifying start and end of block-comment markers
   :line-cmt-start - string specifying the begin marker of a line comment
   :ws-regex - regular expression for matching (non-comment) white space
   :auto-trim - whether to automatically remove the leading whitespace/comments
                at the current position in the input text.
   :word-reader - regular expression for matching words
   :operators - a vector of vector of operators in the decreasing order of
                precedence; see get-default-ops function for an example.
   :op-fn-map - a map of operator and the function to call for that operator when
               evaluating expressions
   :eof - if true, the parse function must consume the entire input text

Args:
    parse-fn  - parse function to apply
    input-str - input text to be parsed
    opts      - key value options (listed above)"

  [parse-fn input-str & opts]
  (let [options (merge default-options (apply hash-map opts))]
    (binding [*parser-state* (parser-init input-str options)]
      (init-operators (get options :operators (get-default-ops)))
      (set-opt :op-fn-map (get options :op-fn-map (get-default-op-fn-map)))
      (auto-trim-if)
      (let [result (parse-fn)]
        (if (and (:eof options) (not (at-end?))) (throw-ex "Extraneous text") result)))))

(defn attempt
  "Tries to match the input at the current position with the provided
parse function. If the parse function matches successfully, the matched
text is returned and the input cursor advances by the length of the
matched text. Otherwise a nil is returned and the current position
in the input remains unchanged."
  [parse-fn]
  (let [m (mark-pos)]
    (try (let [r (parse-fn)] r)
         (catch Exception ex (back-to-mark m) nil))))

(defn opt
  "Same as attempt, but accepts a default value argument to return in case the
specified parse function fails.  Useful for matching optional text."
  ([parse-fn] (attempt parse-fn))
  ([parse-fn default-val] (let [v (attempt parse-fn)] (if v v default-val))))

(defn any
  "Returns the result of the first successfully matching parse-function.
If none of the parse functions match, an exception is thrown."
  [& parse-fns]
  (if-let [r (find-first :result #(attempt %) parse-fns)]
    r (throw-ex)))

(defn look-ahead
  "Takes a collection of look-ahead-string and parse-function pairs and applies
the first parse function that follows the matching look-ahead-string  and
returns the result, or throws a parse exception if the parse function fails.

If none of the look-ahead strings match the current text, an exception is thrown.

To specify a default parse function, provide an empty string as look-ahead and
the default parse function at the end of the argument list.

Args: [la-str-1 parse-fn-1 la-str-2 parse-fn-2 ...]"
  
  [[la pf & rest]]
  (if (starts-with? la)
    (pf)
    (if (> (count rest) 1) (recur  rest) (throw-ex))))

(defn look-ahead*
  "Same as look-ahead, but consumes the matching look-ahead string before
applying the corresponding parse function. "

  [[la pf & rest]]
  (if (starts-with? la)
    (do (string la) (pf))
    (if (> (count rest) 1) (recur  rest) (throw-ex))))


(defn series
  "Matches a sequence of parse functions and returns their results in
a vector. Each successfull match by the parse function advances the cursor.
If any of the parse functions fails, an exception is thrown."
  [& parse-fns]
  (letfn [(apply-fseq [fns]
            ;(map #(%) fns)
            ; just applying map doesn't get the correct dynamically rebound var values.
            ; clojure bug?
            (loop [fst (first fns)
                   rst (rest fns)
                   result []]
              (if fst (recur (first rst) (rest rst) (conj result (fst)))
                  result)))]
    (apply-fseq parse-fns)))

(defn multi*
  "Matches zero or more occurrences of the provided parse function and returns
the results in a vector."
  [parse-fn]
  (loop [rv []]
    (if-let [r (attempt parse-fn)]
      (recur (conj rv r))
      (if (pos? (count rv)) rv))))

(defn multi+
  "Matches one or more occurrences of the provided parse function and returns
the results in a vector. If the parse function doesn't match even once, an
exception is thrown."
  [parse-fn]
  (if-let [rv (multi* parse-fn)] rv (throw-ex)))

(defn lexeme
  "Applies the specified parse function for current input text, consumes any
following whitespace, comments and returns the result of the parse function
application."
  [parse-fn]
  (let [r (parse-fn)] (read-ws) r))

(defn expect
  "Customize error message; if the specified parse function doesn't match
the current input text, the error message of the parse exception will include
the specified custom expected-message."
  [expected parse-fn]
  (try (parse-fn)
       (catch Exception ex (throw-ex (str "Expected " expected)))))

(defn- mchar [ch is-no-auto-trim]
  (let [c (read-ch is-no-auto-trim)]
    (if (= ch c) c
        (throw-ex (unexpected c ch)))))

(defn chr
  "If the next character in the input matches the specified character ch,
returns it; otherwise throws an exception."
  [ch] (mchar ch false))

(defn chr-
  "Same as chr but with auto-trimming turned off for the following input"
  [ch] (mchar ch true))

(defn chr-in
  "If the next character in the input matches any character in the specified
string or character collection, the matching character is returned.
Otherwise throws an exception."
  [chars] (read-ch-in-set (to-set chars) false))

(defn chr-in-
  "Same as chr-in but with auto-trimming turned off for the following input"
  [chars] (read-ch-in-set (to-set chars) true))

(defn string
  "If the input matches the specified string, the string is
returned. Otherwise, a parse exception is thrown."
  [^String s]
  (if (starts-with? s) (move (.length s))
        (throw-ex (unexpected (next-n (.length s)) s))))

(defn string-in-ord
  "Returns the first string from the provided strings that matches text
at the current position. Throws an exception if none of the strings match."
  [strings]
  (if-let [^String s (find-first :item #(starts-with? %) strings)]
    (do (move (.length s)) s)
    (throw-ex (unexpected (next-n 10) strings))))

(defn string-in
  "Returns the longest string from the provided strings that matches text
at the current position. Throws an exception if none of the strings match."
  [strings]
  (string-in-ord (sort-by count #(compare %2 %1) strings)))

(defn word-in
  "Returns the first word from the provided words that matches text
at the current position. Throws an exception if none of the words match.
An optional word-reader parse-function may be provided to read words."
  ([str-coll]
     (word-in str-coll #(regex (get-opt :word-reader))))
  ([str-coll word-reader]
     (let [wset (to-set str-coll)
            w (word-reader)]
        (if (wset w) w (throw-ex (unexpected w wset))))))

(defn word
  "Returns the specified word if the word occurs at the current position in
the input text; an exception is thrown otherwise."
  [w] (word-in #{w}))

(defn sep-by
  "Reads a record using the specified field, field-separator and record-separator
parse functions.  If no record-separator is specified, a newline character
is used as record separator. Returns the fields of the record in a vector."
  ([fld-fn fld-sep-fn]
     (sep-by fld-fn fld-sep-fn #(regex #"\r?\n")))

  ([fld-fn fld-sep-fn rec-sep-fn]
       (let [fst (fld-fn)
             rst (multi* #(series fld-sep-fn fld-fn))
             _ (any rec-sep-fn at-end?)
             rst (if rst (map second rst))
             result (if rst (vec(conj rst fst)) [fst])]
         result
         )))

(defn any-string
  "Reads a single-quoted or double-quoted or a plain-string that is followed
by the specified separator sep; the separator is not part of the returned
string."
  [sep] (cond
          (starts-with? "\"") (dq-str)
          (starts-with? "'") (sq-str)
          :else (read-to-re (re-pattern (str sep "|\r?\n")))))

(defn between
  "Applies the supplied start-fn, parse-fn and end-fn functions and returns
the result of parse-fn. This is typically used to parse content enclosed by
some delimiters on either side."
  [start-fn parse-fn end-fn]
  (let [res (series start-fn parse-fn end-fn)] (res 1)))

(defn parens
  "Returns the result of applying specifed parse function to text that is
in between the opening and closing parentheses '(' and ')'"
  [parse-fn] (between #(chr \() parse-fn #(chr \))))

(defn braces
  "Returns the result of applying specifed parse function to text that is
in between the opening and closing braces '{' and '}'"
  [parse-fn] (between #(chr \{) parse-fn #(chr \})))

(defn sq-brackets
  "Returns the result of applying specifed parse function to text that is
in between the opening and closing square brackets '[' and ']'"
  [parse-fn] (between #(chr \[) parse-fn #(chr \])))

(defn ang-brackets
  "Returns the result of applying specifed parse function to text that is
in between the opening and closing angular brackets '<' and '>'"
  [parse-fn] (between #(chr \<) parse-fn #(chr \>)))

(defn regex
  "Returns the text matched by the specified regex; If a group is specified,
the returned text if for that group only. In either case, the cursor is
advanced by the length of the entire matched text (group 0)"
  ([re] (regex re 0))
  ([re grp] (read-re re grp)))

(defn integer
  "Parses a long integer value and returns a Long."
  [] (Long/parseLong (regex #"-?\d+(?!\w|\.)")))

(defn decimal
  "Parses a decimal value and returns a Double."
  [] (Double/parseDouble (regex #"-?\d+(\.\d+)?(?!\w|\.)")))

(defn number
  "Matches an integral or non-integral numeric value. While the function decimal
also matches both integer and non-integer values, it always
returns a Double; where as number returns Long for integers and Double
for non-integers."
  [] (any integer decimal))

(defn sq-str
  "Parses a single-quoted string and returns the matched string (minus the quotes)"
  [] (qstring \' \\))

(defn dq-str
  "Parses a double-quoted string and returns the matched string (minus the quotes)"
  [] (qstring \" \\))

(defn- qstring [qchar esc]
  (let [sb (StringBuilder.)]
    (chr qchar) ; read and ignore
    (loop []
      (let [ch (read-ch)
            is-esc (= ch esc)
            ch (if is-esc (read-ch) ch)]
        (if (and (= ch qchar) (not is-esc))
          nil
          (do (.append sb ch) (recur)))))
    (.toString sb)))

(defn next-text ^String []
  (let [[s c _ _] (state)] (subs s c)))

(defn read-to
  "The parser skips to the position where the text contains the string
specified by s. The string itself is not consumed, that is the cursor is
positioned at the beginning of the match. If the specified string is not
found, cursor position does not change and a parse exception is thrown."
  [^String s]
  (let [t (next-text)
        n (.indexOf t s)]
    (if (>= n 0) (move n) (throw-ex))))

(defn skip-over
  "Finds the specified string s in the input and skips over it. If the string
is not found, a parse exception is thrown."
  [^String s]
  (let [s1 (read-to s)
        s2 (string s)]
    (str s1 s2)))

(defn read-re
  "Reads the string matching the specified regular expression. If a match-group
is specified, the corresponding text is returned; otherwise the entire matched
text is returned."
  ([re] (read-re re 0))
  ([re ^Integer grp] 
     (let [t (next-text)
           ^Matcher rm (re-matcher re t)]
       (if (.lookingAt rm)
         (do (move (.end rm))
             (.group rm grp))
         (throw-ex)))))

(defn read-to-re
  "Reads and returns text upto but not including the text matched by the
specified regular expression. If the specified regular expression doesn't
occur in the remaining input text, an exception is thrown."
  [re]
  (let [t (next-text)
        m (re-find re t)]
    (if (nil? m)
      (throw-ex)
      (let [^String ms (if (string? m) m (m 0))
            i (.indexOf t ms)]
        (move i)))))

(defn skip-over-re
  "Reads and returns text upto and including the text matched by the
specified regular expression. If the specified regular expression doesn't
occur in the remaining input text, an exception is thrown."
  [re] (let [s1 (read-to-re re)
             s2 (read-re re)]
         (str s1 s2)))

(defn starts-with?
  "Returns a boolean value indicating whether the current input text matches
the specified string."
  [^String s] (.startsWith (next-text) s))

(defn starts-with-re?
  "Returns a boolean value indicating whether the specified regular expression
matches the input at the current position."
  [re] (->> (next-text) (re-matcher re) .lookingAt))

(defn read-n
  "Reads and returns an n-character string at the current position."
  ^String [n]
  (let [t (next-text)]
    (if (<= n (.length t))
      (move n)
      (throw-ex "EOF"))))

(defn read-ch
  "Reads and return the next input character. Throws an exception if the
current position is at the end of the input."
  ([] (read-ch false))
  ([is-no-auto-trim]
     (let [[^String s c _ _] (state)]
       (if (= c (.length s)) (throw-ex "EOF"))
       (if is-no-auto-trim (set-pos (inc c)) (move 1))
       (.charAt s c))))

(defn read-ch-in-set
  "Reads and returns the next character if it matches any of the characters
specified in the provided set.  An exception is thrown otherwise.  The
optional is-no-auto-trim argument may be used to specify whether or not
to apply auto-trim after reading the next character."
  ([char-set] (read-ch-in-set char-set false))
  ([char-set is-no-auto-trim]
     (let [ch (read-ch is-no-auto-trim)]
       (if (char-set ch) ch
           (throw-ex (unexpected ch char-set))))))

(defn blk-cmt
  "Reads and returns a block comment as specified by the begin and end
markers.  Throws an exception if the specified block-comment doesn't
occur at the current position."
  [beg end]
  (if beg
    (let [s1 (string beg) s2 (skip-over end)] (str s1 s2))))

(defn blk-cmt?
  "Similar to blk-cmt but returns a nil instead of throwing an exception
in case of a match failure."
  [beg end] (attempt #(blk-cmt beg end)))

(defn- to-eof
  "Reads and returns text from current position to the end of the input text."
  [] (let [t (next-text)] (move (.length t))))

(defn line-cmt
  "Reads and returns a line comment as specified by the begin marker.
Throws an exception if the specified block-comment doesn't occur at the
current position."
  [beg]
  (if beg
    (let [s1 (string beg)
          s2 (any #(skip-over "\n") #(to-eof))]
      (str s1 s2))))

(defn line-cmt?
  "Similar to line-cmt but returns a nil instead of throwing an exception
in case of a match failure."
  [beg] (attempt #(line-cmt beg)))

(defn ws
  "Matches white space (including comments) at the current position.  The
optional parameters bcb, bce, lcb and wsre specify block-comment-begin,
block-comment-end, line-comment-begin and white-space-regex respectively.
If they are not specified here, the options set for the parser are used.
Throws an exception if white space doesn't occur at the current position."
  ([]
     (let [opts (get-opts)
           blk (opts :blk-cmt-delim)
           lc (opts :line-cmt-start)
           wsre (opts :ws-regex)]
     (ws (blk 0) (blk 1) lc wsre)))

  ([bcb bce lcb wsre]
     (let [w (multi+ (fn [](any #(blk-cmt bcb bce)
                                #(line-cmt lcb)
                                #(regex wsre))))]
       (apply str w))))

(defn ws?
  "Similar to ws except that a nil value is returned instead of throwing
an exception in case of a match failure."
  [& args] (attempt #(apply ws args)))

(defn at-end?
  "Returns true if no more input is left to be read; false otherwise."
  [] (let [[^String s c _ _] (state)] (= (.length s) c)))

(defn line-pos
  "Returns [line column] vector representing the current cursor position
of the parser"
  [] (let [[^String s c p _] (state)]
       (loop [i 0, nl-cnt 0, col 0]
         (if (= i c) [(inc nl-cnt) (inc col)]
             (if (== (int(.charAt s i)) (int \newline))
               (recur (inc i) (inc nl-cnt) 0)
               (recur (inc i) nl-cnt (inc col)))))))

(defn line-pos-str
  "Returns line position in a descriptive string."
  [] (let [lp (line-pos)] (str "line " (lp 0) ", col " (lp 1))))
  
(defn throw-ex
  "Throws an exception; this is usually called to indicate a match
failure in a parse function."
  ([] (throw-ex ""))
  ([msg] (throw (Exception. (str "Parse Error: " msg " at "
                                    (line-pos-str))))))
(defn unexpected
  "Creates a message string for unexpected input expection."
  [actual expected]
  (str "Unexpected input: \"" actual "\"; Expecting "
                 (if (coll? expected) "one of " "") expected))

; ------------------ create & configure the parser ------------

(defn parser-init
  "Initializes the parser state with the specified input string and options."
  ([^String input-str]
     (parser-init input-str default-options))

  ([^String input-str opts]
     (let [new-opts (init-default-ws-reader opts)
           st (atom [input-str 0 0 new-opts])]
       st)))      ; [str pos prev-pos opts]

(defn- init-default-ws-reader [opts]
  (let [bco (get opts :blk-cmt-delim)
        lcb (get opts :line-cmt-start)
        ws-re (get opts :ws-regex)
        wsr #(ws? (bco 0) (bco 1) lcb ws-re)]
    (assoc opts :default-ws-reader wsr)))

(defn set-opt
  "Sets parser option k to value v"
  [k v] (swap! *parser-state*
               #(let [[s c p o] %
                      o (-> o (assoc k v) (init-default-ws-reader))]
                  [s c p (assoc o k v)])))

(defn get-opt
  "Returns the value for parser option k; if the optional default value
parameter d is specified, its value is returned if the option k is not set
in parser options."
  ([k] (get-opt k nil))
  ([k d] (let [[_ _ _ o] (state)] (get o k d))))

(defn auto-trim-on
  "Turns on auto-trim feature that cleans trailing white-space, comments
or whatever the custom ws-reader if any is spe"
  []
  (set-opt :auto-trim true))

(defn auto-trim-off
  "Turns off the auto-trim option."
  [] (set-opt :auto-trim false))

(defn set-blk-cmt-opts
  "Sets block comment begin and end markers."
  [beg end]
  (set-opt :blk-cmt-delim [beg end]))

(defn set-line-cmt-opts
  "Sets line comment begin marker."
  [beg]
  (set-opt :line-cmt-start beg))

(defn set-ws-regex
  "Sets the regular expression to be used for matching non-comment white-space."
  [ws-re]
  (set-opt :ws-regex ws-re))

(defn set-ws-reader
  "This sets the white-space parser to be used when auto-trim is set.
If this is specified, it overrides the options set by set-blk-cmt-opts,
set-line-cmt-opts and set-ws-regex options."
  [ws-reader]
  (set-opt :ws-reader ws-reader))

;----------------------- misc utilities for parse functions ------------
(defn- get-input [] ((state) 0)) 

(defn- get-pos [] ((state) 1))

(defn- get-prev [] ((state) 2))

(defn- get-opts [] ((state) 3))

(defn- set-pos
  ([pos] (set-pos pos (get-pos)))
  ([pos prev]
     (swap! *parser-state* #(let [[s _ _ o] %] [s pos (min prev pos) o]))
     nil))
  ; prev-pos must never be greater than current-pos

(defn mark-pos
  "Returns the current positional parameters of the parser."
  [] (let [[_ c p _] (state)] [c p]))

(defn back-to-mark
  "Resets the positional parameters to a previously set mark."
  [mark] (set-pos (mark 0) (mark 1)))

(defn- move [delta]
  (set-pos (+ (get-pos) delta))
  (let [t (match-text)]
    (auto-trim-if)
    t))

;------------------------------------------------------------------------

(defmacro with-trim-on
  "Executes the provided body with auto-trim option set to true.  The earlier
value of the auto-trim option is restored after executing the body."
  [& body]
  (let [at (get-opt :auto-trim)]
    `(do
       (set-opt :auto-trim true)
       (let [ret# (do ~@body)]
         (set-opt :auto-trim ~at)
         ret# ))))

(defmacro with-trim-off
  "Executes the provided body with auto-trim option set to false.  The earlier
value of the auto-trim option is restored after executing the body."
  [& body]
  (let [at (get-opt :auto-trim)]
    `(do
       (set-opt :auto-trim false)
       (let [ret# (do ~@body)]
         (set-opt :auto-trim ~at)
         ret# ))))

(defn no-trim
  "Similar to with-trim-off, but takes a function as a parameter instead of
the body"
  [fn] (with-trim-off (fn)))

(defn no-trim-nl
  "Turns off automatic trimming of newline characters (as part of white-space)
and executes the specified function. The earlier auto-trim options are restored
at the end of execution of the specified function."
  [fn] (let [wsre (get-opt :ws-regex)]
         (set-opt :ws-regex #"[ \t]+")
         (let [result (fn)]
           (set-opt :ws-regex wsre)
           result)))

(defn read-ws
  "Reads whitespace (including comments) using a whitespace reader based
on parser options. If the :ws-reader option is not set, a default whitespace
reader based on other parser options such as :ws-regex, :blk-cmt-delim and
:line-cmt-start will be used. Returns the whitespace read."
  [] (let [wr (get-opt :ws-reader)
           wr (if (nil? wr) (get-opt :default-ws-reader) wr)]
       (wr)))

(defn auto-trim-if
  "Automatically trim the leading input text if :auto-trim option is set to true."
  [] (if (get-opt :auto-trim) (read-ws))
  nil)


(defn- match-text
  "Returns the text matched by the previous parser operation."
  []
  (let [[s c p _] (state)] (if (> c p) (subs s p c))))

(defn- remaining [] (- (count (get-input)) (get-pos)))

(defn- next-n [n]
  (let [t (next-text)]
    (subs t 0 (min n (.length t)))))

(defn- to-set [coll] (if (set? coll) coll (apply hash-set coll)))

; stops at first match instead of taking first of filter
(defn- find-first
  [item-or-result pred coll]
  (loop [fst (first coll)
         rst (rest coll)]
    (if fst (if-let [result (pred fst)]
              (if (= item-or-result :item) fst result)
              (recur (first rst) (rest rst))))))


;------------------------------------------------------------------------
; Sample Expression Parser

(defn get-default-ops []
  [[:unary ["!" "~"]]
   [:left ["*" "/" "%"]]
   [:left ["+" "-"]]
   [:left ["<<" ">>" ">>>"]]
   [:left ["<" "<=" ">" ">="]]
   [:left ["==" "!="]]
   [:left ["&"]] 
   [:left ["^"]] 
   [:left ["|"]] 
   [:left ["&&"]] 
   [:left ["||"]] 
   [:right [ "=" "+=" "-=" "*=" "/=" "%=" "&="
             "^=" "|=" "<<=" ">>=" "<<<="]]])

(defn get-default-op-fn-map []
  {"!" not, "~" bit-not, "*" *, "/" /, "+" +, "-" -, "<<" bit-shift-left,
   ">>" bit-shift-right "<" <, "<=" <=, ">" >, ">=" >=, "==" ==,
   "!=" #(not (== %1 %2)),"&" bit-and, "^" bit-xor, "|" bit-or,
   "&&" (macro->fn and), "||" (macro->fn or)})

(defn- ops->map [operators]
  (reduce #(merge %1 %2) {}
          (for [i (range (count operators))]
            (loop [m {} j 0]
              (let [l-ops (operators i) desc (l-ops 0) op-names (l-ops 1)]
                (if (< j (count op-names))
                  (recur (assoc m (op-names j) [desc i]) (inc j))
                  m))))))

(defn- get-all-ops [operators]
  (reduce #(concat %1 (get %2 1)) [] operators))

(defn op-map [] (get-opt :op-map))

(defn all-ops [] (get-opt :all-ops))

(defn init-operators [operators]
  (set-opt :op-map (ops->map operators))
  (set-opt :all-ops (get-all-ops operators)))

(defn- op? [op] (get (op-map) op))

(defn- unary? [op] (= :unary ((get (op-map) op [""]) 0)))

(defn- prec [op] ((get (op-map) op) 1))

(defn- left-assoc? [op] (= :left ((get (op-map) op) 0)))

(defn has-priority? [op1 op2]
    (if (< (prec op1) (prec op2)) ;op1 has higher precedence (smaller #)
      true
      (if (> (prec op1) (prec op2))
        false
        (left-assoc? op1))))

(defn operator [] (string-in (all-ops)))

(defn- unary-operator []
  (let [op (operator)]
    (if (unary? op) op (throw-ex (unexpected op "unary operator or expression")))))

(defn- binary-operator []
  (let [op (operator)]
    (if-not (unary? op) op (throw-ex (unexpected op "binary operator")))))

(declare paren-expr)

(defn- factor []
  (let [uop (attempt unary-operator)
        fac (if (starts-with? "(") (paren-expr) (number))]
    (if uop [uop fac] fac)))

(declare expr)

(defn paren-expr []
  (chr \()
  (let [e (expr)] (chr \)) e))

(declare build-tree)

(defn expr
  "Parses expressions and returns the parse tree as nested vectors."
  ([]
     (expr []))
  ([nodes]
     (if-let [fac1 (attempt factor)]
       (if-let [rhs (multi* #(series binary-operator factor))]
         (build-tree
             (reduce #(-> %1 (conj (%2 0)) (conj (%2 1))) (conj nodes fac1) rhs))
         (if (empty? nodes) fac1 (build-tree (conj nodes fac1))))
       (build-tree nodes))))

(defn build-tree [nodes]
  (loop [i 0 tree []]
    (if (< i (count nodes))
      (let [node (nodes i)
            nc (count tree)
            n0 (get tree 0) n1 (get tree 1) n2 (get tree 2)]
        (case nc
              0 (recur (inc i) [node])
              1 (recur (inc i) [node n0])
              2 (recur (inc i) [n0 n1 node])
              3 (recur (inc i) (if (odd? i) ; operator
                                 (if (has-priority? n0 node)
                                   [node tree]
                                   [n0 n1 [node n2]])
                                 [n0 n1 [(n2 0) (n2 1) node]]))))
      tree)))

(defn- get-op-fn [op]
  (let [of-map (get-opt :op-fn-map)]
    (if-let [fn (get of-map op)] fn (throw-ex (str op " not implemented")))))

(defn eval-expr-tree
  "Evaluates the parse tree returned by expr parse function."
  [ptree]
  (letfn [(val [i] (-> (ptree i) eval-expr-tree))]
    (if (coll? ptree)
      (case (count ptree)
        2 ((get-op-fn (ptree 0)) (val 1))
        3 ((get-op-fn (ptree 0)) (val 1) (val 2)))
      ptree)))

(defn eval-expr
  "Parses and evaluates an expression in infix notation.
   Args: expression-string followed by parser options.  See parse function for details."
  [& args]
  (apply parse (list* #(-> (expr) eval-expr-tree) args)))
