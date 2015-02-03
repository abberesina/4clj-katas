(ns problem67)

(def test-cases
 '[(= (__ 2) [2 3])
   (= (__ 5) [2 3 5 7 11])
   (= (last (__ 100)) 541)
   ()])


;; Stack overflow after at some point (1550 primes)
(def primes
  (fn [n]
    (take n ((fn ps [xs]
               (lazy-seq
                (let [p (first xs)]
                  (cons p (ps (remove #(= 0 (rem % p)) xs))))))
             (drop 2 (range))))))

;; Variations on a C.Grand's original idea of Eratosthenes
;; Build a map of non-prime associated with the primes divisors
;; 2->2+2=4 not there, add it
;;    {4 2}
;; 3->3+3=6 not there, add it
;;    {4 2, 6 3}
;; 4->4+2=6 already there, find next 4+2+2=8 ok
;;    {6 3, 8 2}
;; 5->(5+5) not there, add it
;;    {6 3, 8 2, 10 5}
;; 6->6+3=9 ...
;;    {8 2, 9 3, 10 5}
(defn primes-v2
  [max-prime]
  (letfn [(enqueue [sieve non-prime factor]
            (loop [next-np (+ non-prime factor)]
              (if (sieve next-np)
                (recur (+ next-np factor))
                (assoc sieve next-np factor))))

          (next-sieve [sieve candidate]
            (if-let [fact (sieve candidate)]
              (-> sieve
                  (dissoc candidate)
                  (enqueue candidate fact))
              (enqueue sieve candidate candidate)))]
    (last (sort (cons 2 (vals (reduce next-sieve {} (range 2 max-prime))))))))

(defn primes-v3
  "Look for the next non-prime with (NP + 2 * factor) instead of only 1 factor,
  since the factor is already present in the NP and the even numbers are taken
  out from the initial sequence."
  [max-prime]
  (letfn [(enqueue [sieve non-prime factor]
            (loop [next-np (+ non-prime (+ factor factor))]
              (if (sieve next-np)
                (recur (+ next-np factor))
                (assoc sieve next-np factor))))

          (next-sieve [sieve candidate]
            (if-let [fact (sieve candidate)]
              (-> sieve
                  (dissoc candidate)
                  (enqueue candidate fact))
              (enqueue sieve candidate candidate)))]
    (cons 2 (vals (reduce next-sieve {} (range 3 max-prime 2))))))

(let [ps (time (primes-v3 2000000))
      m (reduce max ps)]
  (println  m (count ps)))

(defn primes-v4
  "Lazy-seq of primes"
  []
  (letfn [(add-next-np [sieve non-prime factor]
            (loop [next-np (+ non-prime (+ factor factor))]
              (if (sieve next-np)
                (recur (+ next-np factor))
                (assoc sieve next-np factor))))

          (build-next-sieve [sieve candidate]
            (if-let [fact (sieve candidate)]
              (-> sieve
                  (dissoc candidate)
                  (add-next-np candidate fact))
              (add-next-np sieve candidate candidate)))

          (next-prime [sieve candidate]
            (let [next-candidate (+ 2 candidate) ;; step by 2
                  next-sieve     (build-next-sieve sieve candidate)]
              (if (contains? sieve candidate)
                (recur next-sieve next-candidate)
                (cons candidate
                      (lazy-seq
                       (next-prime next-sieve next-candidate))))))]
    (cons 2 (lazy-seq (next-prime {} 3)))))

(let [ps (time (doall (for [x (primes-v4) :while (< x 2000000)] x)))
      m  (time (reduce max ps))]
  (println  m (count ps)))

(println (for [x (primes-v4) :while (< x 100)] x))


(def __
  primes)

(defn test-code
  []
  (doseq [[test-case test-number] (map vector test-cases (range))]
    (if (eval `(let [~'__ __]
                 ~test-case))
      (printf "Test #%d passed!\n" (inc test-number))
      (printf "Test #%d failed!\n" (inc test-number)))))
