(ns receipt.test)


(defmacro ^:private abc-456 "docstring"
  ([] "wer")
  ([x] (str x x))
  ([x y] (str x y)))

(defn my-fn
  ([] "abc")
  ([x] (str "abc " x)))

(defmacro abc-123
  ([x] (str x x))
  ([x y] (str x y))
  ([x y & z] (str x y z)))

(def my-var 123)

(def ^:dynamic *dyn*)

(declare my-declared)

(defn- private [] 1)

(def ^:private priv-var 2)
