(load "testy.lisp")
(load "interpreter.lisp")

;Primitive Tests;
(test-all '(
            ;atom return 
            (should-false
              (fl-interp nil nil))
            (should-true
              (fl-interp T nil))
            (should-true
              (equal (fl-interp 3 nil) 3))

            ;if
            (should-true
              (fl-interp '(if T T nil) nil))
            (should-false
              (fl-interp '(if nil T nil) nil))

            ;first
            (should-equal
              (fl-interp '(first nil) nil)
              nil)
            (should-equal
              (fl-interp '(first (a b c)) nil)
              'a)

            ;rest
            (should-equal
              (fl-interp '(rest ()) nil)
              nil)
            (should-equal
              (fl-interp '(rest (a)) nil)
              nil)
            (should-equal
              (fl-interp '(rest (a b c)) nil)
              '(b c))

            ;null
            (should-true
              (fl-interp '(null nil) nil))
            (should-false
              (fl-interp '(null T) nil))
            (should-false
              (fl-interp '(null (9 5)) nil))

            ;atom
            (should-true
              (fl-interp '(atom nil) nil))
            (should-true
              (fl-interp '(atom T) nil))
            (should-true
              (fl-interp '(atom a) nil))
            (should-false
              (fl-interp '(atom (9 5)) nil))

            ;number
            (should-false
              (fl-interp '(number nil) nil))
            (should-true
              (fl-interp '(number 9) nil))
            (should-false
              (fl-interp '(number (9 5)) nil))

            ;eq
            (should-true
              (fl-interp '(eq nil nil) nil))
            (should-true
              (fl-interp '(eq 9 9) nil))
            (should-false
              (fl-interp '(eq 9 T) nil))
            (should-false
              (fl-interp '(eq nil (9 5)) nil))

            ;equal
            (should-true
              (fl-interp '(equal nil nil) nil))
            (should-true
              (fl-interp '(equal 9 9) nil))
            (should-false
              (fl-interp '(equal 9 T) nil))
            (should-false
              (fl-interp '(equal nil (9 5)) nil))
            (should-true
              (fl-interp '(equal (9 5) (9 5)) nil))
            (should-false
              (fl-interp '(equal (9 5 6) (9 5)) nil))

            ;+
            (should-equal
              (fl-interp '(+ 0 0) nil)
              0)
            (should-equal
              (fl-interp '(+ 1 (+ 1 1)) nil)
              3)

            ;-
            (should-equal
              (fl-interp '(- 0 0) nil)
              0)
            (should-equal
              (fl-interp '(- 1 (- 1 1)) nil)
              1)

            ;*
            (should-equal
              (fl-interp '(+ 0 0) nil)
              0)
            (should-equal
              (fl-interp '(+ 1 (+ 1 1)) nil)
              3)

            ;>
            (should-true
              (fl-interp '(> 1 0) nil))
            (should-false
              (fl-interp '(> -1 0) nil))

            ;<
            (should-true
              (fl-interp '(< 0 1) nil))
            (should-false
              (fl-interp '(< 0 -1) nil))

            ;=
            (should-true
              (fl-interp '(= 1 1) nil))
            (should-false
              (fl-interp '(= -1 0) nil))

            ;and
            (should-true
              (fl-interp '(and T T) nil))
            (should-false
              (fl-interp '(and T nil) nil))
            (should-false
              (fl-interp '(and T (and T nil)) nil))

            ;or
            (should-true
              (fl-interp '(or nil 1) nil))
            (should-false
              (fl-interp '(or nil nil) nil))

            ;not
            (should-true
              (fl-interp '(not nil) nil))
            (should-false
              (fl-interp '(not T) nil))
            (should-false
              (fl-interp '(not (not (not T))) nil))

            T))
