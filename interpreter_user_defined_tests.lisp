(load "testy.lisp")
(load "interpreter.lisp")

;Primitive Tests;
(test-all 
  '(
    (should-false
      (fl-interp '(if nil T nil) nil))
    ))
