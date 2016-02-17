(load "testy.lisp")
(load "interpreter.lisp")

;Utility and Tests;
(test-all 
  '(
    (should-equal
      (elements-before-= '(1 2 3 = a b c)) '(1 2 3))
    (should-equal
      (elements-after-= '(1 2 3 = a b c)) '(a b c))
    (should-equal
      (elements-after-= '(1 2 3 = (a b c))) '((a b c)))
    (should-equal
      (fl-parse-body '(fun x y = (+ x y))) '(+ x y))
    (should-equal
      (fl-parse-fname '(fun x y = (+ x y))) 'fun)
    (should-equal
      (fl-parse-args '(fun x y z = (+ x y))) '(x y z))
    (should-equal
      (fl-parse-definition '(fun x y z = (+ x y))) '((fun (x y z)) (+ x y)))
    ))
