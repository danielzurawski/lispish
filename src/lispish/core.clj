(ns #^{:author "Daniel Zurawski"
       :doc "A simple Lisp to JavaScript transcompiler written in Clojure."}
  lispish.core
  [:require
   [clojure.string :as str]]
  [:use
   [clojure.walk]
   [clojure.tools.trace]
   [clojure.tools.cli :only (cli)]]
  (:gen-class :main true))


(def op (set ['+ '- '* '/ '> '< '=]))
(def forms (set ['let 'if 'fn 'defn 'cond]))

;; Clojure is a single pass compiler, thus we have to use forward declaration
;; if we need to use a function before it's declared
(declare emit-list)

(defn emit [expressions]
  "Take an s-expression and emit its corresponding JavaScript form"
  (do
    (println "Emit Lispish: " expressions)
    (cond
      (nil? expressions) "null"
      (symbol? expressions) (do ;;(println "emit: symbol")
                                (str expressions))
      (list? expressions) (do ;;(println "emit: list")
                            (emit-list expressions)
                            )
      (integer? expressions) (do ;;(println "emit: integer")
                                 (str expressions))
      (float? expressions) (do ;;(println "emit: float")
                               (str expressions))
      (string? expressions) (do ;;(println "emit: string")
                              (str expressions)
                              )
      :else (str expressions))))

;; Abstract Structural Binding - + falls in type, + in op and 2 2 in tail
(defn emit-op [type [op & tail]]
  "Emit s-expression with single operators and two arguments"
  (do (println "Emit-op, head: " op ", tail: " tail)
      ;; Interlace the arguments with the operator
      (str "(" (clojure.string/join
                (str (if (= op '=) "==" op))
                (map emit tail))
           ")")))

(defn emit-let [type [let [x y]]]
  (str "var " x ";"
       x "=" (emit y) ";"))

(defn emit-if [type [if condition true-form & false-form]]
  (str "if"
       (emit condition)
       " { return "
       (emit true-form)
       " } else { return "
       (emit false-form)
       " }"))

(defn emit-fn [type [fn [arg] & rest]]
  (str "function(" arg ") {"
       "return "
       (emit rest)
       "}"))

(defn emit-defn [type [defn name [arg & more] & rest]]
  (do
    (println "Emit-defn, name: " name ", arg: " arg ", arg tail: " more ", rest: " rest))
  (str (str "function " name "("
            (if (nil? more) arg (str arg ", " (clojure.string/join ", " more))) ") {"
       (emit rest)
       "}")))

(defn emit-recur [head [name args & rest]]
  (println "emit-recur, head:" head ", name: " name ", args: " args ", more: " rest)
  (str name "("
       (if (nil? rest)
         (str "(" (emit args) ")")
         (str (str (emit args)) ", " (clojure.string/join ", " (map emit rest)))) ")"))

(defn emit-cond [head [name condition statement & rest]]
  (str "if(" (emit condition) ") { return " (emit statement) " }"
       (reduce str (interleave (map #(str "else if(" % ")")
                                    ;; Special case if condition is :else then emit "true"
                                    (reduce list (map #(if (= (str %) ":else") "true" (emit %))
                                                     (take-nth 2 rest))))
                               (map #(str "{ return " % " }")
                                    (map emit (take-nth 2 (pop rest)))))) ))

(defn emit-forms [head expression]
  (do (println "Emit-forms, head: " head ", full expression: " expression)
      (cond (= head 'let) (emit-let head expression)
            (= head 'if) (emit-if head expression)
            (= head 'fn) (emit-fn head expression)
            (= head 'defn) (emit-defn head expression)
            (= head 'cond) (emit-cond head expression)
            :else (emit-recur head expression) )))

(defn emit-list [expressions]
  (do
      (if (symbol? (first expressions))

        (let [head (symbol (first expressions))
              expressions (conj
                           (rest expressions) head)]

          (println "Emit-list head: " head
                   ", tail: " (rest expressions))
          (cond
            (contains? op head) (emit-op head expressions)
            (contains? forms head) (emit-forms head expressions)

            :else (emit-forms head expressions)
            ))
        ;; Not safe, may run into stack overflow if this will be a list or not-recognized
        (emit (first expressions)))))

;; Macro not to evaluate the forms directly
(defn lisp-to-js [forms]
  (emit (read-string forms)))

(defn run
  "Print out the options and the arguments"
  [opts args]
    (cond (:input opts)
      (let [result (lisp-to-js (slurp (:input opts)))]
        (if (:output opts)
          (spit (:output opts) result)
          (println result)))
      (seq args) (println (lisp-to-js (first args)))
      :else (println "No path to input source code specified and no code given as argument.")))

(defn -main [& args]
  (let [[opts args banner]
        (cli args
             ["-h" "--help" "Show help" :flag true :default false]
             ["-in" "--input" "REQUIRED: Path to Lispish source code."]
             ["-out" "--output" "REQUIRED: Path to JavaScript output file."]
             )]
    (when (:help opts)
      (println banner)
      (System/exit 0))
    (if (or (:input opts) (seq args))
      (run opts args)
      (println banner))))
