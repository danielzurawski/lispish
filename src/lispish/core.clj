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


(def op (set ['mod '+ '- '* '/ '> '< '=]))
(def bop (set ['or 'and 'not]))
(def forms (set ['recur 'let 'if 'fn 'defn 'cond]))

;; Clojure is a single pass compiler, thus we have to use forward declaration
;; if we need to use a function before it's declared
(declare emit-list)

(defn emit [expressions]
  "Take an s-expression and emit its corresponding JavaScript form"
  (do
    (println "Emit Lispish: " expressions)
    (println "Emit TYPE: " (type expressions))
    (cond
      (nil? expressions) "null"
      (symbol? expressions) (do ;;(println "emit: symbol")
                                (str expressions))
      (seq? expressions) (do ;;(println "emit: list")
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
      (if (= op 'not)
        (str "(!" (emit tail) ")")
        (str "(" (clojure.string/join
                (str (cond (= op '=) "=="
                           (= op 'mod) "%"
                           (= op 'or) "||"
                           (= op 'and) "&&"
                           :else op))
                (map emit tail))
           ")"))))

(defn emit-let [type [let [x y] body]]
  (println "type: " type ", let: " let ", x: " x ", y: " y ", body: " body)
        (str "(function(" (emit x) ") { return "  (emit body)  " })(" (emit y) ")" ))

(defn emit-if [type [if condition true-form & false-form]]
  (str "("
       (emit condition)
       " ? ("
       (emit true-form)
       "):("
       (emit false-form)
       "))"))

(defn emit-defn [type [defn name [arg & more] & rest]]
  (do
    (println "Emit-defn, name: " ", arg: " arg ", arg tail: " more ", rest: " rest))
  (str (str "function " (if (= "~" name) "" name) "("
            (if (nil? more) arg (str arg ", " (clojure.string/join ", " more))) ") {return "
       (emit rest)
       "}")))

(defn emit-fn [head expression]
  (emit-defn head (concat (take 1 expression) '("~") (drop 1 expression))))

(defn emit-call [head [name args & rest]]
  (str name "("
       (if (nil? rest)
         (str "(" (emit args) ")")
         (str (str (emit args)) ", " (clojure.string/join ", " (map emit rest)))) ")"))

(defn emit-recur [head expression]
  (println "head: " head ", exp: " expression)
  (emit-call head (concat '("arguments.callee") (drop 1 expression))))

(defn emit-cond [head [name & rest]]
  (let [rev (reverse (partition 2 rest))]
    (println "przed rev: " rev)
    (println "rev: " (drop 1 rev))
    ;;(println ", rrev: " (type (rest (vec rev)) ))
    (reduce
          (fn [a b] (str "(" (emit (first b)) "?" (emit (second b)) ":" a ")"))
          (str (emit (second (first rev))))
          (drop 1 rev))))

(defn emit-forms [head expression]
  (do (println "Emit-forms, head: " head ", full expression: " expression)
      (cond (= head 'let) (emit-let head expression)
            (= head 'if) (emit-if head expression)
            (= head 'fn) (emit-fn head expression)
            (= head 'defn) (emit-defn head expression)
            (= head 'cond) (emit-cond head expression)
            (= head 'recur) (emit-recur head expression)
            :else (emit-call head expression) )))

(defn emit-list [expressions]
  (do
      (if (symbol? (first expressions))

        (let [head (symbol (first expressions))
              expressions (conj
                           (rest expressions) head)]

          (println "Emit-list head: " head
                   ", tail: " (rest expressions))
          (cond
            (or (contains? op head) (contains? bop head)) (emit-op head expressions)
            (contains? forms head) (emit-forms head expressions)

            :else (emit-forms head expressions)
            ))
        ;; Not safe, may run into stack overflow if this will be a list or not-recognized
        (emit (first expressions)))))

(defn lisp-to-js [forms]
  (let [code (read-string forms)]
    (println code)
    (emit code)))

(defn read-file-emit [st file-out]
    (let [form (read st false "")]
      (if (not (= form ""))
        (do
          (spit file-out (str (emit form) "\n")  :append true)
          (read-file-emit st file-out)))))

(defn read-file [file-in file-out]
  (with-open [r (java.io.PushbackReader.
                 (clojure.java.io/reader file-in))]
    (binding [*read-eval* false]
      (spit file-out "" :append false)
      (read-file-emit r file-out))))

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
             ["-out" "--output" "OPTIONAL: Path to JavaScript output file."]
             )]
    (when (:help opts)
      (println banner)
      (System/exit 0))
    (if (or (:input opts) (seq args))
      (run opts args)
      (println banner))))
