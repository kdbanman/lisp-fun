(load "testy.lisp")
(load "interpreter.lisp")

;Parsing Tests;
(test-all 
  '(
    (should-equal
      (size '(1 2 3)) 3)
    (should-equal
      (elements-before-= '(1 2 3 = a b c)) '(1 2 3))
    (should-equal
      (elements-after-= '(1 2 3 = a b c)) '(a b c))
    (should-equal
      (elements-after-= '(1 2 3 = (a b c))) '((a b c)))
    (should-equal
      (parse-body '(fun x y = (+ x y))) '(+ x y))
    (should-equal
      (parse-fname '(fun x y = (+ x y))) 'fun)
    (should-equal
      (parse-params '(fun x y z = (+ x y))) '(x y z))
    (should-true
      (signature-equal 'fun '(1 2 3) '(fun x y z = (+ x y))))
    ))

;Utility Tests;
(test-all 
  '(
    (should-equal
      (recursive-replace 'a 1 '(a 2 3)) '(1 2 3))
    (should-equal
      (recursive-replace 'a 1 '(a 2 (a 2 3))) '(1 2 (1 2 3)))
    (should-equal
      (recursive-replace 'a 1 '(a nil (a nil 3))) '(1 nil (1 nil 3)))
    (should-equal
      (applicative-reduce '(1 2) '(a b) '(+ a (- b a))) '(+ 1 (- 2 1)))
    (should-equal
      (substitute-args
        '((1 2 3) a)
        '(fun L n = (+ n (n 1 (count L)))))
      '(+ a (a 1 (count (1 2 3)))))
    (should-equal
      (apply-function
        'fun
        '(1 (1 3))
        '((fun a b = (+ a (sum b)))))
      '(+ 1 (sum (1 3))))
    (should-true
      (function-defined 'fun '(1 2) '((fun a b = (+ a (sum b))))))
    (should-false
      (function-defined 'fun '(2) '((fun a b = (+ a (sum b))))))
    (should-false
      (function-defined 'funny '(1 2) '((fun a b = (+ a (sum b))))))
    (should-equal
      (locate-definition 'fun '(1 2) '((a = 3) (fun a b = (+ a (sum b)))))
      '(fun a b = (+ a (sum b))))
    (should-equal
      (locate-definition 'a () '((a = 3) (fun a b = (+ a (sum b)))))
      '(a = 3)
      )
    ))
