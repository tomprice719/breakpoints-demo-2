(ns breakpoints-demo2.core
  (:require [breakpoints-demo2.breakpoints :refer :all]))

(def foo 3)
(def bar 5)

(def ^:dynamic dynamic-var nil)

(defn test-breakpoints []
  (for [i (range 10)]
    (.start
      (Thread.
        #(binding [dynamic-var i]
           (break "A" foo))))))
