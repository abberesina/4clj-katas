(ns problem67)

(def test-cases
 '[(= (__ 2) [2 3])
   (= (__ 5) [2 3 5 7 11])
   (= (last (__ 100)) 541)
   ()])


;; Stackoverflow after 1550 primes
(def __
  (fn [n]
    (take n ((fn ps [xs]
               (lazy-seq
                (let [p (first xs)]
                  (cons p (ps (remove #(= 0 (rem % p)) xs))))))
             (drop 2 (range))))))

(defn test-code
  []
  (doseq [[test-case test-number] (map vector test-cases (range))]
    (if (eval `(let [~'__ __]
                 ~test-case))
      (printf "Test #%d passed!\n" (inc test-number))
      (printf "Test #%d failed!\n" (inc test-number)))))
