(ns problem60)

(def test-cases
 '[(= (take 5 (__ + (range))) [0 1 3 6 10])
   (= (__ conj [1] [2 3 4]) [[1] [1 2] [1 2 3] [1 2 3 4]])
   (= (last (__ * 2 [3 4 5])) (reduce * 2 [3 4 5]) 120)])

;; Case with an empty collection is not treated
;; See (source reductions)
(defn reds
  ([f coll]
   (reds f (first coll) (rest coll)))
  ([f init coll]
   (lazy-seq
    (cons init
          (when (seq coll)
            (reds f (f init (first coll)) (rest coll)))))))

;; chouser
;; use an atom as the accumulator
;; swap! : <atom> -> f <atom> args
(fn r
  ([f [a & b]] (r f a b))
  ([f a b]
   (let [m (atom a)]
     (cons a (map #(swap! m f %) b)))))

;; cgrand
;; destructuring to handle init or not
;; Pffff
(apply (fn [f i & s]
         (#(% %) (memoize #(cons i (lazy-seq (map f (% %) s))))))
       conj [1] [2 3 4])


(take 10 (reds + []))
(take 10 (reds + (range)))
(take 10 (reds * 2 (range 3 10000)))
(take 10 (reds + 0 (range)))

(take 5 (reds conj [1] (range 2 1000)))
(take 5 (reductions conj (range 1 1000)))
(take 10 (reductions + (range)))
(take 10 (reductions + 0 (range)))
(reduce * 2 [3 4 5])
(reduce + [])


(def __
  reds
  )

(defn test-code
  []
  (doseq [[test-case test-number] (map vector test-cases (range))]
    (if (eval `(let [~'__ __]
                 ~test-case))
      (printf "Test #%d passed!\n" (inc test-number))
      (printf "Test #%d failed!\n" (inc test-number)))))
