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


;(random-search knapPI_16_20_1000_1 100000
;)




;==========================================Our Work Starts Here!==============================



;;;Tweak Strategy 1------------------

;(tweak knapPI_16_20_1000_1)

; (defn rando-answer
; )

;  (defn tweak-origional
;  [instance]
;  (let [choices-vec ((random-answer instance 100000) :choices)
;        len-choices (count choices-vec)]
 ;  (loop [start 0]
;   (if (= (nth choices-vec start) 1) (assoc choices-vec start 0) (recur (inc start)))
;    )
 ;  ))

;(random-search (random-answer knapPI_11_20_1000_1) 19)

;(let [instance (random-search knapPI_16_20_1000_1 10000)
 ;     tweaked-choices (tweak instance)]
 ; [instance tweaked-choices])

;;;Tweak Strategy 2------------------------------------

(defn tweak-two
  "Consumes an answer and a numeric index, returns a new answer created by
   flipping the bit at index in the choices vector. Adjusts total-weight and total-value accordingly."
  [answer index]
  (let [item-list (nth (:items (:instance answer)) index)
        choices-vec (:choices answer)
        tweak-index (- 1 (nth choices-vec index))
        tweak-choices-vec (concat (take index choices-vec) (list tweak-index) (drop (inc index) choices-vec))
        op (if (= 0 tweak-index) - +)
        tweak-weight (op (:total-weight answer) (:weight item-list))
        tweak-value   (op (:total-value answer) (:value item-list))]
  {:instance (:instance answer)
   :choices tweak-choices-vec
   :total-weight tweak-weight
   :total-value tweak-value}))

(defn find-0s
  "Consumes a choices vector, and returns vector of all indices of 0's."
  [choices]
  (map second (filter #(= 0 (first %)) (map vector choices (range (count choices))))))

(defn find-1s
  "Consumes a choices vector, and returns vector of all indices of 1's."
  [choices]
  (map second (filter #(= 1 (first %)) (map vector choices (range (count choices))))))

(defn find-op-answer
  "Compares weight of answer w/ capacity and if weight > capacity, tweaks & deletes an item from
   the knapsack. If weight < capacity, then tweaks & adds an item to the knapsack. O/w, returns only answer."
  [answer]
    (cond
      (< (:capacity (:instance answer)) (:total-weight answer)) ;; take something out
         (tweak-two answer (rand-nth (find-1s (:choices answer))))

      (> (:capacity (:instance answer)) (:total-weight answer)) ;; put something in
         (tweak-two answer (rand-nth (find-0s (:choices answer))))

      :else answer))



;;;Simple Hill-Climbing------------------------

(defn hill-climb
  "Hill-climber on random-search to find a solution."
  [min-tries tweak-meth instance]
    (loop [curr-best (random-answer instance)
           i min-tries]
      (if (and (<= i 0) (> (score curr-best) 0)) curr-best
        (recur (max-key score curr-best (tweak-meth curr-best)) (dec i)))))

;(hill-climb 10000 tweak-two knapPI_11_20_1000_1)

;;;Hill-Climbing With Random Restarts-------------
(defn random-restart
  "Consumes # tweaks (dep), # restarts (bread), method to tweak with and instance to find optimal solution."
  [dep bread tweak-meth instance]
  (let [hill-climber #(hill-climb dep tweak-meth instance)]
    (reduce (partial max-key score) (for [i (range bread)] (hill-climb)))))
