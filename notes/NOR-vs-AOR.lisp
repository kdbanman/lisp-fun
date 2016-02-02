((lambda (x) (+ x x))
 ((lambda (z) (+ 3 z)) 2))

;NOR

(+ ((lambda (z) (+ 3 z)) 2) ((lambda (z) (+ 3 z)) 2) )
(+ (+ 3 2) ((lambda (z) (+ 3 z)) 2) )
(+ 5 ((lambda (z) (+ 3 z)) 2) )
(+ 5 (+ 3 2) )
(+ 5 5)
10

;AOR

((lambda (x) (+ x x)) (+ 3 2))
((lambda (x) (+ x x)) 5)
(+ 5 5)
10
