(ns class-code.core
  (:gen-class)
  (:use clojure.set))

(def default-instructions
  (list
    ;'in1
    'integer_+
    'integer_-
    'integer_*
    'integer_%
    'integer_dup
    'integer_neg
    'integer_swap
    ;'ERC
    ;'integer_pop
    ))

;; EXPERIMENTS
;; 
;; sizes: 10 and 20 (and 50 with fewer iterations)
;; instructions: original, no pop, no pop + in1, ERC + original
;; multiple copies of in1 (4) along with pop
;; 
;; inputs: original and new inputs

;;;;;;;;;
;; Utilities

(def empty-push-state
  {:exec '()
   :integer '()
   :input {}})

(defn abs
  "Absolute value."
  [x]
  (if (neg? x)
    (- x)
    x))

(defn not-lazy
  "Returns lst if it is not a list, or a non-lazy version of lst if it is."
  [lst]
  (if (seq? lst)
    (apply list lst)
    lst))

(defn push-to-stack
  "Pushes item onto stack in state"
  [state stack item]
  (update state stack conj item))

(defn pop-stack
  "Removes top item of stack."
  [state stack]
  (update state stack rest))

(defn peek-stack
  "Returns top item on a stack."
  [state stack]
  (if (empty? (get state stack))
    :no-stack-item
    (first (get state stack))))

(defn empty-stack?
  "Returns true if the stack is empty."
  [state stack]
  (empty? (get state stack)))

(defn get-args-from-stacks
  "Takes a state and a list of stacks to take args from. If there are enough args
  on each of the desired stacks, returns a map of the form {:state :args}, where
  :state is the new state and :args is a list of args from the stacks. If there
  aren't enough args on the stacks, returns :not-enough-args."
  [state stacks]
  (loop [state state
         stacks (reverse stacks)
         args '()]
    (if (empty? stacks)
      {:state state :args args}
      (let [stack (first stacks)]
        (if (empty-stack? state stack)
          :not-enough-args
          (recur (pop-stack state stack)
                 (rest stacks)
                 (conj args (peek-stack state stack))))))))

(defn make-push-instruction
  "A utility function for making Push instructions. Takes a state, the function
  to apply to the args, the stacks to take the args from, and the stack to return
  the result to. Applies the function to the args (taken from the stacks) and pushes
  the return value onto return-stack."
  [state function arg-stacks return-stack]
  (let [args-pop-result (get-args-from-stacks state arg-stacks)]
    (if (= args-pop-result :not-enough-args)
      state
      (let [result (apply function (:args args-pop-result))
            new-state (:state args-pop-result)]
        (push-to-stack new-state return-stack result)))))

;;;;;;;;;
;; Instructions

(defn in1
  "Pushes the input labeled :in1 on the inputs map onto the :integer stack."
  [state]
  (push-to-stack state :integer (:in1 (:input state))))

(defn integer_+
  [state]
  (make-push-instruction state +' [:integer :integer] :integer))

(defn integer_-
  [state]
  (make-push-instruction state -' [:integer :integer] :integer))

(defn integer_*
  [state]
  (make-push-instruction state *' [:integer :integer] :integer))

(defn integer_%
  [state]
  (make-push-instruction state
                         (fn [int1 int2]
                           (if (zero? int2)
                             int1
                             (quot int1 int2)))
                         [:integer :integer]
                         :integer))

(defn integer_dup
  [state]
  (if (empty-stack? state :integer)
    state
    (push-to-stack state
                   :integer
                   (peek-stack state :integer))))

(defn integer_neg
  [state]
  (make-push-instruction state - [:integer] :integer))

(defn integer_swap
  [state]
  (if (<= (count (:integer state)) 1)
    state
    (let [top (first (:integer state))
          sec (second (:integer state))]
      (-> state
          (pop-stack :integer)
          (pop-stack :integer)
          (push-to-stack :integer top)
          (push-to-stack :integer sec)))))

;;; Equivalent to
;; (push-to-stack
;;  (push-to-stack
;;   (pop-stack
;;    (pop-stack state :integer)
;;    :integer)
;;   :integer top)
;;  :integer
;;  sec)

(defn integer_pop
  [state]
  (if (empty-stack? state :integer)
    state
    (pop-stack state :integer)))

;; 'integer_dup
;; 'integer_neg
;; 'integer_swap
;; 'integer_pop

;;;;;;;;;
;; Interpreter

(defn interpret-one-step
  "Takes a Push state and executes the next instruction on the exec stack."
  [state]
  (let [popped-state (pop-stack state :exec)
        first-raw (first (:exec state))
        first-instruction (if (symbol? first-raw)
                            (eval first-raw)
                            first-raw)]
    (cond
      (fn? first-instruction)
      (first-instruction popped-state)
      ;
      (integer? first-instruction)
      (push-to-stack popped-state :integer first-instruction)
      ;
      (string? first-instruction)
      (push-to-stack popped-state :string first-instruction)
      ;
      (seq? first-instruction)
      (update popped-state :exec #(concat %2 %1) first-instruction)
      ;
      (or (= first-instruction true) (= first-instruction false))
      (push-to-stack popped-state :boolean first-instruction)
      ;
      :else
      (throw (Exception. (str "Unrecognized Push instruction in program: "
                              first-instruction))))))

(defn interpret-program
  "Runs the given problem starting with the stacks in start-state."
  [program start-state step-limit]
  (loop [state (assoc start-state :exec program :step 1)
         trace '()]
    (if (or (empty? (:exec state))
            (> (:step state) step-limit))
      trace
      (recur (update (interpret-one-step state) :step inc)
             (conj trace (:integer state))))))


;;;;;;;;;
;; GP

(defn make-random-linear-push-program
  "Creates and returns a new linear Push program"
  [instructions length]
  (repeatedly length
              (fn []
                (let [instruction (rand-nth instructions)]
                  (if (= instruction 'ERC)
                    (- (rand-int 21) 10)
                    instruction)))))

(defn trace-program-on-inputs
  "Run program on some inputs and return a list of traces on those inputs."
  [program]
  (let [inputs '(-5 -4 -3 -2 -1 0 1 2 3 4)]
    (map #(interpret-program program
                             {:exec '()
                              :integer (list % %)
                              :input {:in1 %}}
                             (count program))
         inputs)))

(defn trace-program-on-inputs-different
  "Run program on some inputs and return a list of traces on those inputs."
  [program]
  (let [inputs (list '(1 2 3)
                     '(-5 -5)
                     '(7 8)
                     '(9 8 7 6 5 4 3 2 1)
                     '(2999 16 22)
                     '(-10 -100 -1000 -10000)
                     (range 100))]
    (map #(interpret-program program
                             {:exec '()
                              :integer %
                              :input {:in1 (first %)}}
                             (count program))
         inputs)))

(defn is-trivial?
  "Returns true if the semantics is trivial, false otherwise"
  [semantics]
  (or (apply = semantics)
      (some empty? semantics) ;; this will return the first thing in semantics that makes empty? true
      ))

(defn sampling-alg-1
  "Implements sampling alg 1 from paper"
  [instructions program-length number-iterations]
  (loop [programs-so-far #{}
         traces-so-far #{}
         semantics-so-far #{}
         iteration 0]
    (if (>= iteration number-iterations)
      {:programs programs-so-far
       :traces traces-so-far
       :semantics semantics-so-far}
      (let [new-program (make-random-linear-push-program instructions program-length)]
        (if (contains? programs-so-far new-program)
          (recur programs-so-far traces-so-far semantics-so-far iteration)
          (let [traces (trace-program-on-inputs new-program)
                semantics (map first traces)]
            (if (is-trivial? semantics)
              (do
                ;; (println "This is a trival program and semantics")
                ;; (println new-program)
                ;; (println semantics)
                (recur programs-so-far
                       traces-so-far
                       semantics-so-far
                       (inc iteration)))
              (recur 
                programs-so-far
                ;(conj programs-so-far new-program)
                (conj traces-so-far traces)
                (conj semantics-so-far semantics)
                (inc iteration)))))))))

(defn mt-sampling-alg-1
  [instructions program-length number-iterations threads]
  (let [results-lst (pmap (fn [x] (sampling-alg-1 instructions program-length number-iterations)) (range threads))
        programs-lst (map :programs results-lst)
        traces-lst (map :traces results-lst)
        semantics-lst (map :semantics results-lst)]
    ;; merge results
    {:programs (apply clojure.set/union programs-lst)
     :traces (apply clojure.set/union traces-lst)
     :semantics (apply clojure.set/union semantics-lst)}))

(defn count-semantic-diversity
  ""
  [traces index]
  (let [tracesi (map #(nth % index) traces)]
    (count (set tracesi))))

(defn gather-data-from-sampling-alg-1
  "Does what the name suggests"
  [instructions program-length number-iterations threads]
  (let [results (time (do
                        (println "Sampling")
                        (doall (mt-sampling-alg-1 instructions program-length number-iterations threads))))
        num-programs (count (:programs results))
        num-traces (count (:traces results))
        num-semantics (count (:semantics results))
        flat-traces (time (do
                            (println "Flattening traces")
                            (doall (mapcat identity (:traces results)))))
        int-semantic (time (do
                             (println "Determining internal diversity")
                             (doall (pmap #(count-semantic-diversity flat-traces %) (range program-length)))))]
    (println "Instructions:" instructions)
    (println "Program length:" program-length)
    (println "Number iterations:" number-iterations)
    (println "Threads" threads)
    (println "Number of programs:" num-programs)
    (println "Number of traces:  " num-traces)
    (println "Number of semantics:" num-semantics)
    (println "Internal semantic diversity:" (reverse int-semantic))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (binding [*ns* (the-ns 'class-code.core)]
    (gather-data-from-sampling-alg-1 default-instructions 10 640000 32)
    (System/exit 0)))
