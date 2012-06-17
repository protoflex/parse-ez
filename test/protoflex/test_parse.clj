;   Copyright (c) Protoflex Technologies Pvt. Ltd. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "Test Parse Library functions" :author "Panduranga Adusumilli"}
  protoflex.test_parse
  (:use [clojure.test]
        [protoflex.parse]
        [protoflex.examples.csv_parse]
        [protoflex.examples.xml_parse]))

(defmacro ptest [s expected & body]
  (let [res# `(parse #(do ~@body) ~s :eof false)]
    `(is (= ~res# ~expected))))

(defmacro ptest-fail [s & body]
  `(is (~'thrown? Exception (parse #(do ~@body) ~s))))

(deftest test-chr
  (ptest "abc" \a (chr \a))
  (ptest "abc" [\a \b] [(chr \a) (chr \b)])
  (ptest "a b c" [\a \b \c] [(chr \a) (chr \b) (chr \c)])
  (ptest-fail "abc" (chr \b))
  (ptest-fail "" (chr \a))
  )

(deftest test-chr-in
  (ptest "axy" [\a] [(chr-in "abc")])
  (ptest "axb" [\a] [(chr-in [\b \a \c])])
  (ptest "axc" [\a] [(chr-in #{\c \b \a})])
  (ptest "acdef" [\a \c] [(chr-in "abc") (chr-in "abc")])
  (ptest-fail "xyza" (chr-in "abc"))
  (ptest-fail "" (chr-in "abc"))
  )

(deftest test-string
  (ptest "abc" "abc" (string "abc"))
  (ptest "abcdef" "abc" (string "abc"))
  (ptest "abc def" "abc def" (string "abc def"))
  (ptest "abcdef" ["abc" "de"] [(string "abc") (string "de")])
  (ptest "abc def" ["abc" "de"] [(string "abc") (string "de")])
  (ptest "abc/*comment*/def" ["abc" "def"]
    [(string "abc") (string "def")])
  (ptest "abc defxyz" ["abc" "def" "x"]
    [(string "abc") (string "def") (string "x")])
  (ptest-fail "" (string "abc"))
  (ptest-fail "ab c" (string "abc"))
  )

(deftest test-string-in
  (ptest "abcxyzdef" "abc" (string-in ["abc" "def"]))
  (ptest "abcxyzdef" "abc" (string-in ["def" "abc"]))
  (ptest ">>=" ">>=" (string-in [">>" ">>="]))
  (ptest ">>=" ">>" (string-in-ord [">>" ">>="]))
  (ptest "abcdefxyz" ["abc" "def"]
    [(string-in ["abc" "def"]) (string-in ["abc" "def"])])
  (ptest-fail "xyzabc" (string-in ["abc" "def"]))
  )

;; TODO: test cases for custom word-reader
(deftest test-word
  (ptest "abc def" ["abc" "def"] [(word "abc") (word "def")])
  (ptest "abc /*comment*/ def" ["abc" "def"] [(word "abc") (word "def")])
  (ptest-fail "abcdef" (word "abc"))
  (ptest-fail "abc d" (word "abc d"))
  )

(deftest test-word-in
  (ptest "abc xyz" "abc" (word-in ["abc" "def" "ghi"]))
  (ptest "ghi xyz" "ghi" (word-in ["abc" "def" "ghi"]))
  (ptest "abc def ghi" ["abc" "def"]
    (let [words ["abc" "def" "ghi"]] [(word-in words) (word-in words)]))
  (ptest-fail "abcdef" (word-in ["abc" "def"]))
  (ptest-fail "xyzabcdef" (word-in ["abc" "def"]))
  )

(deftest test-between
  (ptest "(1234)" 1234 (parens number))
  (ptest "{1234}" 1234 (braces number))
  (ptest "[1234]" 1234 (sq-brackets number))
  (ptest "<1234>" 1234 (ang-brackets number))
  )

(deftest test-regex
  (ptest "abc def123" "abc def" (regex #"\w+\s+([a-z]*)"))
  (ptest "abc def123 456" ["abc" "def123" "456"]
         [(regex #"\w+") (regex #"\w+") (regex #"\d+")])
  (ptest "abc def123" "123" (regex #"(a\w+)\s+[a-z]*(\d+)" 2))
  (ptest-fail "abcdef123" (regex #"\w+\s+([a-z]*)"))
  )


(deftest test-integer
  (ptest "123 abc" 123 (integer))
  (ptest "-123 abc" -123 (integer))
  (ptest-fail "a123" (integer))
  (ptest-fail "123.45" (integer))
  (ptest-fail "123abc" (integer))
  )

(deftest test-decimal
  (ptest "123 abc" 123.0 (decimal))
  (ptest "123.45 abc" 123.45 (decimal))
  (ptest "-123.45 abc" -123.45 (decimal))
  (ptest-fail "  123abc" (decimal))
  (ptest-fail "  123.45abc" (decimal))
  )

(deftest test-number
  (ptest "123" 123 (number))
  (ptest "123.5" 123.5 (number))
  )

(deftest test-dq-str
  (ptest "\"abc\"def" "abc" (dq-str))
  (ptest (str \" "ab" \\ \" \c \") (str "ab" \" \c) (dq-str))
  (ptest-fail "abcdef" (dq-str))
  )

(deftest test-sq-str
  (ptest "'abc'def" "abc" (sq-str))
  (ptest (str "'abc"  \\ "'def'ghi") "abc'def" (sq-str))
  (ptest-fail "abcdef" (sq-str))
  )

(deftest test-read-to
  (ptest "abcdef12345" ["abcdef" 12345] [(read-to "123") (integer)])
  (ptest-fail "abcdef12345" (read-to "999"))
  )

(deftest test-skip-over
  (ptest "abcdef" ["abc" "def"] [(skip-over "abc") (string "def")])
  (ptest "abc  def" ["abc" "def"] [(skip-over "abc") (string "def")])
  (ptest-fail "abc  def" (with-trim-off (skip-over "abc") (string "def")))
  )

(deftest test-read-re
  (ptest "abc123" "abc" (read-re #"(\D+)"))
  )

(deftest test-read-to-re
  (ptest "123abc" "123" (read-to-re #"(\D+)"))
  )

(deftest test-skip-over-re
  (ptest "123abc456" "123abc" (skip-over-re #"(\D+)"))
  (ptest "123abc" "123" (skip-over-re #"(\d+)"))
  (ptest-fail "123abc" (skip-over-re #"([x-z]+)"))
  )

(deftest test-starts-with?
  (ptest "abcdef" true (starts-with? "abc"))
  (ptest "abcdef" false (starts-with? "def"))
  (ptest "abcdef" true (starts-with? ""))
  (ptest "" false (starts-with? "def"))
  )

(deftest test-starts-with-re?
  (ptest "123abc" true (starts-with-re? #"\d+"))
  (ptest "abc123" false (starts-with-re? #"\d+"))
  )

(deftest test-read-n
  (ptest "abcdef" "abc" (read-n 3))
  (ptest "abcdef" "a" (read-n 1))
  (ptest "abcdef" nil (read-n 0))
  (ptest-fail "abcdef" (read-n 20))
  )

(deftest test-read-ch
  (ptest "abcdef" [\a \b \c] [(read-ch) (read-ch) (read-ch)])
  (ptest "a b c" [\a \b \c] [(read-ch) (read-ch) (read-ch)])
  (ptest "a b c" [\a \space \b] [(read-ch true) (read-ch true) (read-ch true)])
  (ptest-fail "" (read-ch))
  )

(deftest test-read-ch-in-set
  (ptest "abcdef" [\a \b] [(read-ch-in-set #{\a \b \c})
                           (read-ch-in-set #{\a \b \c})])
  (ptest-fail "axyz" (read-ch-in-set #{\x \y \z}))
  )

(deftest test-blk-cmt
  (ptest "abc/*comment*/def" ["abc" "/*comment*/"]
    (with-trim-off [(string "abc") (blk-cmt "/*" "*/")]))

  (ptest "abc<!--comment-->def" ["abc" "<!--comment-->"]
    (with-trim-off [(string "abc") (blk-cmt "<!--" "-->")]))

  (ptest "abc/*comment*/def" ["abc" "def"] [(string "abc") (string "def")])
  (ptest-fail "abc/*comment*/def" [(string "abc") (blk-cmt "/*" "*/")])
  )

(deftest test-line-cmt
  (ptest "abc//line comment" ["abc" "//line comment"]
    (with-trim-off [(string "abc") (line-cmt "//")]))
  (ptest "abc//line comment\ndef" ["abc" "//line comment\n"]
    (with-trim-off [(string "abc") (line-cmt "//")]))
  )

(deftest test-ws
  (ptest "abc /*xyz*/ ghi //kkk" ["abc" " /*xyz*/ " "ghi" " //kkk"]
    (with-trim-off [(string "abc") (ws) (string "ghi") (ws)]))
  )


(deftest test-trim-on
  (ptest "  abc  def" ["abc" "def"]
    (with-trim-on [(string "abc") (string "def")]))

  (ptest "  abc def " ["abc" "def"]
    (with-trim-on [(string "abc") (string "def")]))
  )

(deftest test-trim-off
  ; note: the leading whitespaces are trimmed before with-trim-off is called
  (ptest "  abc def" ["abc" " def"]
    (with-trim-off [(string "abc") (string " def")]))
  (is (thrown? Exception (parse #(string "abc") "  abc" :auto-trim false)))
  (is (thrown? Exception (parse #(with-trim-off (string "abc")) "  abc" :auto-trim false)))
  )

(deftest test-lexeme
  (let [wr #(regex #"\w+")]
    (ptest "abc/*comment*/def" ["abc" "def"]
      (with-trim-off [(lexeme wr) (wr)]))
    (ptest "abc   def" ["abc" "def"]
      (with-trim-off [(lexeme wr) (wr)]))
    )
  )


(deftest test-attempt
  (ptest "abc" "abc" (attempt #(string "abc")))
  (ptest "abc" nil (attempt #(string "def")))
  )

(deftest test-any
  (ptest "abcdef" "abc" (any #(regex #"\d+") #(string "abc")))
  (ptest "123abcdef" "123" (any #(regex #"\d+") #(string "abc")))
  (ptest-fail "abcdef" (any #(regex #"\d+") #(string "xyz")))
  )

(deftest test-look-ahead
  (let [la-spec ["a" #(string "abc") "x" #(string "xyz")]]
  (ptest "abc def" "abc" (look-ahead la-spec))
  (ptest "xyz def" "xyz" (look-ahead la-spec))
  (ptest-fail "pqr def" (look-ahead la-spec))
    ))

(deftest test-look-ahead*
  (let [la-spec [
                 "0x" #(-> (regex #"[0-9a-fA-F]+") (Integer/parseInt 16))
                 "0" #(-> (regex #"[0-7]+") (Integer/parseInt 8))
                 "" #(number)]]
    (ptest "0x123 0123" 16r123 (look-ahead* la-spec))
    (ptest "123 0x123 0123" [123 16r123 8r123] (multi+ #(look-ahead* la-spec)))
    ))

(deftest test-series
  (ptest "abc123xxx" ["abc" "123" "xxx"]
    (series #(string "abc") #(regex #"\d+") #(word "xxx")))

  (ptest-fail "abc123xxx"
    (series #(string "abc") #(regex #"[a-w]+") #(word "xxx")))
  )

(deftest test-multi*
  (ptest "abcabcabcdef" ["abc" "abc" "abc"] (multi* #(string "abc")))
  (ptest "xyzabcabcabc" nil (multi* #(string "abc")))
  (ptest "" nil (multi* #(string "abc")))
  (ptest "xyzabcabc" ["xyz" ["abc" "abc"]]
    [(string "xyz") (multi* #(string "abc"))])
  )

(deftest test-multi+
  (ptest "abcabcabcdef" ["abc" "abc" "abc"] (multi+ #(string "abc")))
  (ptest-fail "xyzabcabcabc" (multi+ #(string "abc")))
  (ptest-fail "" (multi+ #(string "abc")))
  (ptest "xyzabcabc" ["xyz" ["abc" "abc"]]
    [(string "xyz") (multi+ #(string "abc"))])
  )

(deftest test-expect
  (ptest "230" "230" (expect "integer" #(regex #"\d+")))
  (try (parse (fn [] (expect "integer" #(regex #"\d+"))) "x230")
       (catch Exception ex (is (.contains (.getMessage ex) "Expected integer"))))
  )

(deftest test-sep-by
  (ptest "123.45" [123.45] (sep-by decimal #(chr \,)))
  (ptest "123.45, 234.56,345" [123.45 234.56 345.0] (sep-by decimal #(chr \,)))
  (ptest-fail "123.45, 234, 345abc" (sep-by decimal #(chr \,)))
  )

(deftest test-detect-sep
  (ptest "abc,ghi" \, (detect-sep))
  (ptest "abc\tghi" \tab, (detect-sep))
  (ptest-fail "abcghi" (detect-sep))
  (ptest-fail "\"abc,ghi\"" (detect-sep))
  )

(deftest test-csv
  (let [s1 "'1abc','def',ghi\n2abc,def,ghi\n"
        s2 "\"1a,bc\",def,ghi\n2abc,def,ghi\n"
        s3 "1abc\tdef\tghi\n2abc\tdef\tghi\n"
        s4 "\"1a\tbc\"\tdef\tghi\n2abc\tdef\tghi\n"]

    (ptest s1 [["1abc" "def" "ghi"]["2abc" "def" "ghi"]] (csv))
    (ptest s2 [["1a,bc" "def" "ghi"]["2abc" "def" "ghi"]] (csv))

    (ptest s3 [["1abc" "def" "ghi"]["2abc" "def" "ghi"]] (csv))
    (ptest s4 [["1a\tbc" "def" "ghi"]["2abc" "def" "ghi"]] (csv))
    ))

(deftest test-expr
  (ptest "3+2-5" ["-" ["+" 3 2] 5] (expr))
  (ptest "3+2*5" ["+" 3 ["*" 2 5]] (expr))
  (ptest "-3+2*5" ["+" -3 ["*" 2 5]] (expr))
  )

(deftest test-eval-expr
  (is (= (eval-expr "3") 3))
  (is (= (eval-expr "-3") -3))
  (is (= (eval-expr "3+2") 5))
  (is (= (eval-expr "3+ -2") 1))
  (is (= (eval-expr "3+-2") 1))
  (is (= (eval-expr "3+2*3") 9))
  (is (= (eval-expr "3+2*3+4") 13))
  (is (= (eval-expr "(3+2)*3+4") 19))
  (is (= (eval-expr "(3+2)*(3+4)") 35))
  )

(use '[clojure.java.shell :only [sh]])
(defn test-xml []
  (let [ dir (str (System/getProperty "user.home") "/test/data/xmltest/valid")
        files (-> (sh "find" dir ) ^String (:out) (.split "\n"))
        files (filter #(.endsWith ^String %1 ".xml") files)
        ignore #{
                 "/sa/049.xml"
                 "/sa/050.xml"
                 "/sa/051.xml"
                 "/sa/114.xml"
                 }]
      (doseq [f files] (if (not (get ignore (subs f (count dir)))) (do (println f) (parse-xml (slurp f)))))))

