(ns breakpoints-demo2.breakpoints
  (:require [clojure.core.async :refer [chan poll! offer!]]
            [clojure.main :refer [repl]]))

(def ^:private buffer-size 1000)
(def breakpoint-channel (atom (chan buffer-size)))
(def current-breakpoint nil)

(defn put-context [context]
  (offer! @breakpoint-channel context))

(defmacro break [label & var-symbols]
  (let [local-symbols (keys &env)
        local-context (zipmap (map (fn [sym] `(quote ~sym)) local-symbols) local-symbols)
        breakpoint-data {:locals local-context
                         :label  label}]
    `(breakpoints-demo2.breakpoints/put-context
       (assoc ~breakpoint-data
         :defs
         (zipmap [~@(map (fn [sym] `(var ~sym)) var-symbols)] [~@var-symbols])
         :thread-bindings
         (assoc (get-thread-bindings) ~(var *ns*) ~*ns*)))))

(defn next-bp []
  (if-let [bp (poll! @breakpoint-channel)]
    (do
      (def current-breakpoint bp)
      (println "At breakpoint: " (:label current-breakpoint)))
    (println "Breakpoint queue is empty.")))

(declare ^:dynamic *locals*)

(defn eval-with-context
  [expr]
  (if current-breakpoint
    (with-redefs-fn (:defs current-breakpoint)
      (fn []
        (with-bindings (:thread-bindings current-breakpoint)
          (binding [*locals* (:locals current-breakpoint)]
            (eval
              `(let ~(vec (mapcat #(list % `(*locals* '~%)) (keys *locals*)))
                 ~expr))))))
    (println "Not at a breakpoint.")))

(defn dr-read
  [request-prompt request-exit]
  (let [input (clojure.main/repl-read request-prompt request-exit)]
    (case input
      :exit request-exit
      :next (do (next-bp) request-prompt)
      input)))

(defn debug-repl []
  (if current-breakpoint
    (repl :prompt #(print "debug=> ")
          :read dr-read
          :eval eval-with-context)
    (println "Not at a breakpoint.")))

(defmacro at-bp [form]
  `(breakpoints-demo2.breakpoints/eval-with-context (quote ~form)))

(defn flush-bp []
  (reset! breakpoint-channel (chan buffer-size)))