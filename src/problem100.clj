(ns problem100)

(def test-cases
 '[(== (__ 2 3) 6)
   (== (__ 5 3 7) 105)
   (== (__ 1/3 2/5) 2)
   (== (__ 3/4 1/6) 3/2)
   (== (__ 7 5/7 2 3/5) 210)])

;; cgrand
;; la definition meme du plus petit commun multiple :
;; On cherche un nombre qui est divisible par tous les autres
;; On prend le premier de la serie
(defn cg-ppcm [n & ns]
  (first
   (for [m (next (range))
         :let [n (* n m)]
         :when (every? #(zero? (mod n %)) ns)] n)))

(defn pgcd
  ([a b]
   (if (= 0 b)
     a
     (pgcd b (rem a b))))
  ([a b & m]
   (reduce pgcd (pgcd a b) m)))

(defn pgcd2
  ([a b]
   (if (= 0 b)
     a
     (pgcd b (rem a b))))
  ([a b & m]
   (apply pgcd `(~(pgcd a b) ~@m))))

(def ppcm
  (letfn [(pgcd
            ([a b]
             (if (= 0 b)
               a
               (pgcd b (rem a b))))
            ([a b & m]
             (reduce pgcd (pgcd a b) m)))]
    (fn [& m]
      (reduce * (/ (first m) (apply pgcd m)) (rest m)))))


(let [nbs [98787 3987 6 18 60]
      pg (apply pgcd nbs)
      pp (apply ppcm nbs)
      product (apply * nbs)
      pgpp (* pg pp)]
  (println pg pp product pgpp))

(pgcd2 98787 3987 6 18 60)
(ppcm 98787 3987 6 18 60)

(pgcd 0 120)
(ppcm 3/4 1/6)
(pgcd 3/4 1/6)


(defn euclide [a b]
  (if (< a b)
    (euclide b a)
    (cons [a b]
          (let [n (- a b)]
            (lazy-seq
             (if (= 0 n) [[a 0]] (euclide b n)))))))

(first (last (euclide 1000 425)))

(def __
  ppcm
  )

(defn test-code
  []
  (doseq [[test-case test-number] (map vector test-cases (range))]
    (if (eval `(let [~'__ __]
                 ~test-case))
      (printf "Test #%d passed!\n" (inc test-number))
      (printf "Test #%d failed!\n" (inc test-number)))))
