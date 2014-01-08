(ns protoflex.examples.xml_parse
  (:use [protoflex.parse]))

(declare pi prolog element attributes children-and-close cdata elem-or-text close-tag)

(defn parse-xml [xml-str]
  (parse #(between prolog element pi) xml-str :blk-cmt-delim ["<!--" "-->"] :line-cmt-start nil))

(defn- pi [] (while (starts-with? "<?") (skip-over "?>")))

(defn- prolog [] (pi) (attempt  #(regex #"(?s)<!DOCTYPE([^<]+?>)|(.*?\]\s*>)")) (pi))

(def name-start ":A-Z_a-z\\xC0-\\xD6\\xD8-\\xF6\\xF8-\\u02FF\\u0370-\\u037D\\u037F-\\u1FFF\\u200C-\\u200D\\u2070-\\u218F\\u2C00-\\u2FEF\\u3001-\\uD7FF\\uF900-\\uFDCF\\uFDF0-\\uFFFD")

(def name-char (str name-start "\\-.0-9\\xB7\\u0300-\\u036F\\u203F-\\u2040"))

(def name-re (-> (format "[%s][%s]*" name-start name-char) re-pattern))

(defn element []
  (let [tag (do (chr \<) (regex name-re))
        attrs (attributes)
        children (look-ahead* [
                               ">" #(children-and-close tag)
                               "/>" (fn [] [])])]
    {:tag tag, :attributes attrs, :children children}))

(defn attr []
  (let [n (regex name-re) _ (chr \=)
        v (any sq-str dq-str)]
    [n v]))

(defn attributes [] (apply hash-map (flatten  (multi* attr))))

(defn- children-and-close [tag]
  (let [children (multi* #(between pi elem-or-text pi))]
    (close-tag tag)
    children))

(defn- elem-or-text []
  (look-ahead [
               "<![CDATA[" cdata
               "</" (fn [] nil)
               "<" element
               "" #(read-to "<")]))

(defn- cdata []
  (string "<![CDATA[")
  (let [txt (read-to "]]>")] (string "]]>") txt))

(defn- close-tag [tag]
    (string (str "</" tag))
    (chr \>))

