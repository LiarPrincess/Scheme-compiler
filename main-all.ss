2
"a string"

(+ 2 2)
(+ 2 (-4 1))
(+ 2 (- 4 1))
(- (+ 4 6 3) 3 5 2)
(< 2 3)
(> 2 3)
(>= 3 3)

(string=? "test"  "test")
(string<? "abc" "bba")
(if (> 2 3) "no" "yes")
(if (= 3 3) (+ 2 3 (- 5 1)) "unequal")

(cdr '(a simple test))
(car (cdr '(a simple test)))
(car '((this is) a test))
(cons '(this is) 'test)
(cons '(this is) '())
(eqv? 1 3)
(eqv? 3 3)
(eqv? 'atom 'atom)

(define x 3)
(+ x 2)
(+ y 2)
(define y 5)
(+ x (- y 2))
(define str "A string")
(< str "The string")
(string<? str "The string")
