(ns ^{ :doc "Misc Utilities."} protoflex.util)

(defmacro apply-macro [m args] `(eval (list* '~m ~args)))

(defmacro macro->fn [m] `(fn [& args#] (apply-macro ~m args#)))