(ns problem97)

(def test-cases
  '[(= (__ 1) [1])
    (= (map __ (range 1 6))
       [     [1]
             [1 1]
             [1 2 1]
             [1 3 3 1]
             [1 4 6 4 1]])
    (= (__ 11)
       [1 10 45 120 210 252 210 120 45 10 1])])

;; With a recuring fonction
(defn pascal-recur [n]
  (letfn [(p-step [ps]
            (concat '(1) (map #(apply + %) (partition 2 1 ps)) '(1)))]
    (loop [r [1]
           n n]
      (if (> n 1)
        (recur (p-step r) (dec n))
        r))))

;; With reduce
(defn pascal-reduce [n]
  (letfn [(p-step [ps _]
            (map #(apply + %) (partition 2 1 `(0 ~@ps 0))))]
    (reduce p-step [1] (range 1 n))))

;; Lazy seq with reductions
(defn pascal-lazy-seq []
  (letfn [(p-step [ps _]
            (map #(apply + %) (partition 2 1 `(0 ~@ps 0))))]
    (reductions p-step [1] (range))))

(first (drop 10 (pascal-lazy-seq)))

;; C Houser
(fn p [x]
  (if (= x 1)
    [1]
    `[1 ~@(map + (p (- x 1)) (next (p (- x 1)))) 1]))

;; A Malloy
(fn [n]
  (-> (iterate (fn [row]
                 (map + `(0 ~@row) `(~@row 0)))
               [1])
      (nth (dec n))))

(def __
  #(first (drop (dec %) (pascal-lazy-seq)))
  )

(defn test-code
  []
  (doseq [[test-case test-number] (map vector test-cases (range))]
    (if (eval `(let [~'__ __]
                 ~test-case))
      (printf "Test #%d passed!\n" (inc test-number))
      (printf "Test #%d failed!\n" (inc test-number)))))
