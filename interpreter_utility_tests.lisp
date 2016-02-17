(load "testy.lisp")
(load "interpreter.lisp")

;Utility and Tests;
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
      (parse-args '(fun x y z = (+ x y))) '(x y z))
    (should-true
      (signature-equal 'fun '(1 2 3) '(fun x y z = (+ x y))))
    ))
