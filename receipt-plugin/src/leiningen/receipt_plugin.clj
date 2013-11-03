(ns leiningen.receipt-plugin
  (:require [receipt.core :as rc]
            [leinjacker.eval :as lj]
            [leinjacker.deps :as ld]))

(defn receipt-plugin
  "Generate documentation for your project"
  [project & args]
  (lj/eval-in-project 
   (ld/add-if-missing project ['receipt "1.0.1"])
   `(do
      (require '[receipt.core :as rc])
      (if-not (seq ~args)
        (rc/-main)
        (apply rc/-main ~args)))))
     
    
