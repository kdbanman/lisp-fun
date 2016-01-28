(load "testy.lisp")
(load "set_1.lisp")

;Question 1 Tests;
(test-all '(
    (should-false
        (equals-first-element () ())
    )
    (should-true
        (equals-first-element () '(()))
    )
    (should-true
        (equals-first-element 'a '(a b))
    )
    (should-false
        (equals-first-element 'a '(b a))
    )
    (should-false
        (equals-first-element 'a '((a) b))
    )
    (should-false
        (equals-first-element '(a 2) '((a) 2))
    )
    (should-true
        (equals-first-element '(a 2) '((a 2) 2))
    )

    (should-true
        (xmember '(a 2) '((a 2) 2))
    )
    (should-true
        (xmember '(a 2) '(4 (a 2) 2))
    )
    (should-true
        (xmember '1 '(1))
    )
    (should-false
        (xmember '1 '( (1) 2 3))
    )
    (should-true
        (xmember '(1) '((1) 2 3))
    )
    (should-false
        (xmember nil nil)
    )
    (should-true
        (xmember nil '(nil))
    )
    (should-false
        (xmember nil '((nil)))
    )
    (should-true
        (xmember '(nil) '(1 2 3 (nil)))
    )
    (should-false
        (xmember '(nil) '(nil))
    )
))

;Question 2 Tests;
(test-all '(
    (should-true
        (equal (flatten '(a b)) '(a b))
    )
    (should-true
        (equal (flatten '(a (3) (5 a (9)) 9)) '(a 3 5 a 9 9))
    )
    (should-true
        (equal (flatten '(a (b c) d)) '(a b c d))
    )
    (should-true
        (equal (flatten '((((a))))) '(a))
    )
    (should-true
        (equal (flatten '(a (b c) (d ((e)) f))) '(a b c d e f))
    )
))

;Question 3 Tests;
(test-all 
  '(
    (should-true
      (equal (mix '(a b c) '(1 2)) '(a 1 b 2 c)))
    (should-true
      (equal (mix '(a) nil) '(a)))
    (should-true
      (equal (mix nil nil) nil))
    (should-true
      (equal (mix '(a nil) '(1 2 3 4)) '(a 1 nil 2 3 4)))
    (should-true
      (equal (mix '(a b c) '(d e f)) '(a d b e c f)))
    (should-true
      (equal (mix '(1 2 3) '(a)) '(1 a 2 3)))
    (should-true
      (equal (mix '((a) (b c)) '(d e f g h)) '((a) d (b c) e f g h)))
    (should-true
      (equal (mix '(1 2 3) nil) '(1 2 3)))
    (should-true
      (equal (mix '(1 2 3) '(nil)) '(1 nil 2 3)))))

;Question 4 Tests;
(test-all
  '(
    (should-true
      (equal (split '(1 2 3 4 5 6)) '((1 3 5) (2 4 6))))
    (should-true
      (equal (split '((a) (b c) (d e f) g h)) '(((a) (d e f) h) ((b c) g))))
    (should-true
      (equal (split '()) '(nil nil)))
    (should-true
      (equal (split (mix '(a b c) '(d e f))) '((a b c) (d e f))))
    (should-true
      (equal (split (mix '(1 2 3) '(4 5))) '((1 2 3) (4 5))))
    (should-true
      (equal 
        (let ((L '(a d b e c f))) (mix (car (split L)) (cadr (split L)))) 
        '(a d b e c f)))
    (should-true
      (equal (let ((L nil)) (mix (car (split L)) (cadr (split L)))) nil))
    ))

;Question 6 Tests;
(test-all
    '(
      (should-true
        (equal (subsetsum '(1) 1) '(1)))
      (should-true
        (equal (subsetsum '(1) 2) nil))
      (should-true
        (equal (subsetsum '(1 2) 2) '(2)))
      (should-true
        (equal (subsetsum '(1 2) 3) '(1 2)))
      (should-true
        (equal (subsetsum '(1 2 5 3 4) 15) '(1 2 5 3 4)))
      (should-true
        (equal (subsetsum '(1 2 3) 5) '(2 3)))
      (should-true
        (equal (subsetsum '(1 5 3) 2) nil))
      (should-true
        (equal (subsetsum '(1 16 2 8 4) 29) '(1 16 8 4)))
      (should-true
        (equal (subsetsum '(1 1 5 6 8) 10) '(1 1 8)))
      (should-true
        (equal (subsetsum '(1 10 100 1000 10000) 5) nil)))
    
  )
