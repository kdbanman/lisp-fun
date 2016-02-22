(load "testy.lisp")
(load "interpreter.lisp")

;User Defined Tests;
(test-all 
  '(
    (should-equal
      (fl-interp
        '(f (g 2) (g 1)) 
        '((g X = (+ 1 X))
          (f X Y = (+ X Y))))
      5)
    (should-equal
      (fl-interp
        '(count (1 2 3)) 
        '((count L = (if (null L)
                         0
                         (+ 1 (count (rest L)))))))
      3)
    (should-equal
      (fl-interp
        '(count (1 (2 a) nil 3)) 
        '((count L = (if (null L)
                         0
                         (+ 1 (count (rest L)))))))
      4)
    (should-equal
      (fl-interp
        '(reverse (1 2 3))
        '((reverse X =  (if (null X) 
                nil
                (append (reverse (rest X)) 
                        (cons (first X) nil))))

          (append X Y = (if (null X) 
                          Y
                          (cons (first X) (append (rest X) Y))))))
      '(3 2 1))
    (should-equal
      (fl-interp
        '(reverse ((1 a b) 2 3))
        '((reverse X = (if (null X) 
                         nil
                         (append (reverse (rest X)) 
                                 (cons (first X) nil))))

          (append X Y = (if (null X) 
                          Y
                          (cons (first X) (append (rest X) Y))))))
      '(3 2 (1 a b)))
    (should-equal
      (fl-interp
        '(fib 1)
        '((fib n = (if (< n 2)
                         n
                         (+ (fib (- n 1)) (fib (- n 2)))))))
      1)
    (should-equal
      (fl-interp
        '(fib 2)
        '((fib n = (if (< n 2)
                         n
                         (+ (fib (- n 1)) (fib (- n 2)))))))
      1)
    (should-equal
      (fl-interp
        '(fib 7)
        '((fib n = (if (< n 2)
                         n
                         (+ (fib (- n 1)) (fib (- n 2)))))))
      13)
    (should-equal
      (fl-interp
        '(fib (fib (count (1 2 3 4 5 6))))
        '((fib n = (if (< n 2)
                         n
                         (+ (fib (- n 1)) (fib (- n 2)))))
          (count L = (if (null L)
                         0
                         (+ 1 (count (rest L)))))))
      21)
    (should-equal
      (fl-interp
        '(a (fib 7))
        '((fib n = (if (< n 2)
                         n
                         (+ (fib (- n 1)) (fib (- n 2)))))
          (count L = (if (null L)
                         0
                         (+ 1 (count (rest L)))))))
      '(a 13))
    (should-equal
      (fl-interp
        '(fib (count (1 2 3)))
        '((fib n = (if (< n 2)
                         n
                         (+ (fib (- n 1)) (fib (- n 2)))))
          (count n = (if (null n)
                         0
                         (+ 1 (count (rest n)))))))
      2)
    ))
