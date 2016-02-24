
; print the result of curried "what is smaller, 4 or 3?".
(princ
  (funcall
    (function
      (lambda (x)
        (funcall
          (function
            (lambda (y)
              (if (< x y) x y)))
          4)))
    3))
