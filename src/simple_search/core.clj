(ns simple-search.core
  (:use simple-search.knapsack-examples.knapPI_11_20_1000
        simple-search.knapsack-examples.knapPI_13_20_1000
        simple-search.knapsack-examples.knapPI_16_20_1000))

;;; An answer will be a map with (at least) four entries:
;;;   * :instance
;;;   * :choices - a vector of 0's and 1's indicating whether
;;;        the corresponding item should be included
;;;   * :total-weight - the weight of the chosen items
;;;   * :total-value - the value of the chosen items

(defn included-items
  "Takes a sequences of items and a sequence of choices and
  returns the subsequence of items corresponding to the 1's
  in the choices sequence."
  [items choices]
  (map first
       (filter #(= 1 (second %))
               (map vector items choices))))

(defn random-answer
  "Construct a random answer for the given instance of the
  knapsack problem."
  [instance]
  (let [choices (repeatedly (count (:items instance))
                            #(rand-int 2))
        included (included-items (:items instance) choices)]
    {:instance instance
     :choices choices
     :total-weight (reduce + (map :weight included))
     :total-value (reduce + (map :value included))}))

;;; It might be cool to write a function that
;;; generates weighted proportions of 0's and 1's.

(defn score
  "Takes the total-weight of the given answer unless it's over capacity,
   in which case we return 0."
  [answer]
  (if (> (:total-weight answer)
         (:capacity (:instance answer)))
    0
    (:total-value answer)))

(defn add-score
  "Computes the score of an answer and inserts a new :score field
   to the given answer, returning the augmented answer."
  [answer]
  (assoc answer :score (score answer)))

(defn random-search
  [instance
   max-tries]
  (apply max-key :score
         (map add-score
              (repeatedly max-tries #(random-answer instance)))))


(random-search knapPI_16_20_1000_1 100000
)




;==========================================Our Work Starts Here!==============================



;;;Tweak Strategy------------------



;(tweak knapPI_16_20_1000_1)

 (defn rando-answer
 )

  (defn tweak
  [instance]
  (let [choices-vec ((random-answer instance 100000) :choices)
        len-choices (count choices-vec)]
   (loop [start 0]
   (if (= (nth choices-vec start) 1) (assoc choices-vec start 0) (recur (inc start)))
    )
   ))

(random-search (random-answer knapPI_11_20_1000_1) 19)

(let [instance (random-search knapPI_16_20_1000_1 10000)
      tweaked-choices (tweak instance)]
  [instance tweaked-choices])





;(defn run-mutator
;  "Take a instance, mutator, and number of iterations. Then do hill climbing from that instance."
;  [instance mutator max-tries]
;  (loop [start 0 inst instance]
;    (if (= start max-tries)
;      inst
;      (recur
;       (+ start 1)
 ;      (let [new-inst (mutator inst)]
;         (if ( > (score new-inst) (score inst))
;           new-inst
;           inst)))))
;)



;(defn swap-random-item
;  "Given an instance, we intend to flip a random bit off and a random bit on."
;  [instance]
;  (

;(let [size-instance (count (:choices instance))])

;  )

;  (loop [start 0 inst instance]
;    (if (= start size-instance)
;      inst
;      (recur
;       (+ start 1)
;       (let [new-inst (mutator inst)]
;         (if ( > (score new-inst) (score inst))
;           new-inst
;          inst)))))

;)

;[rand,
;; "After we climed the hill, we got:"
;;;(run-mutator rand swap-random-item 1000)]
;)
;))


;;;Simple Hill-Climbing------------------------



;;;Hill-Climbing With Random Restarts-------------
