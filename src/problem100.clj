(ns problem100)

(def test-cases
 '[(== (__ 2 3) 6)
   (== (__ 5 3 7) 105)
   (== (__ 1/3 2/5) 2)
   (== (__ 3/4 1/6) 3/2)
   (== (__ 7 5/7 2 3/5) 210)])

;; algo d'euclide pour le pgcd
(defn euclide [a b]
  (if (< a b)
    (euclide b a)
    (cons [a b]
          (let [n (- a b)]
            (lazy-seq
             (if (= 0 n) [[a 0]] (euclide b n)))))))

(= 9858 (first (last (euclide 25887108 7738530))))

;; cgrand solution
;; la definition meme du plus petit commun multiple :
;; On cherche parmi tous les multiples du 1er, les nombres qui sont divisibles
;; par les suivants. On prend le premier (le plus petit)
(defn cg-ppcm [n & ns]
  (first
   (for [m (next (range))
         :let [n (* n m)]
         :when (every? #(zero? (mod n %)) ns)] n)))

;; Utilisation de la formule
;; a * b * ... = pgcd(a,b,...) * ppcm(a,b,...)
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
