(ns problem177)

(def test-cases
 '[(__ "This string has no brackets.")
   (not (__ "(start, end]"))
   (not (__ "())"))
   (not (__ "[ { ] } "))
   (__ "([]([(()){()}(()(()))(([[]]({}()))())]((((()()))))))")
   (not (__ "([]([(()){()}(()(()))(([[]]({}([)))())]((((()()))))))"))
   (not (__ "["))])

(defn validate-brackets1 [bs]
  (let [open (fn [st c] [true (conj st c)])
        close (fn [match]
                (fn [st c]
                  (let [top (peek st)]
                    (if (and top (= top match))
                      [true (pop st)]
                      [false (str "Unmatched " c)]))))
        brackets {\( open
                  \[ open
                  \{ open
                  \) (close \()
                  \] (close \[)
                  \} (close \{)}]
    (empty? (reduce (fn [s c]
                      (if-let [b (brackets c)]
                        (let [[r st] (b s c)]
                          (if r st
                              (reduced st)))
                        s)) [] bs))))

(defn validate-brackets [bs]
  (let [o conj
        c (fn [m]
            (fn [st _]
              (when (= m (peek st))
                (pop st))))
        bra {\( o
             \[ o
             \{ o
             \) (c \()
             \] (c \[)
             \} (c \{)}]
    (= [] (reduce (fn [s x]
                    (if-let [b (bra x)]
                      (b s x) s))
                  [] bs))))

(def __ validate-brackets)

(__ "(start, [middle[[]]], (({{{([[]]())}}})), end {})")
(__ "(start, [middle[[]]], (({{{([[]]())}]}})), end {})")
(__ "(start, [middle[[]]], ) [[{")

(defn test-code
  []
  (doseq [[test-case test-number] (map vector test-cases (range))]
    (if (eval `(let [~'__ __]
                 ~test-case))
      (printf "Test #%d passed!\n" (inc test-number))
      (printf "Test #%d failed!\n" (inc test-number)))))
