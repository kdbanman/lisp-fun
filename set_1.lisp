;QUESTION 1
;xmember checks if the first argument is a member the second argument
(defun xmember (query-element query-list)
    (if (atom query-list)
        nil
        (if (equals-first-element query-element query-list)
            t
            (xmember query-element (cdr query-list))
        )
    )
)

;true iff query-element equals the first element of query-list
(defun equals-first-element (query-element query-list)
    (if (atom query-list)
        nil
        (equal query-element (car query-list))
    )
)

;QUESTION 2
(defun flatten (source-list)
    (if (atom source-list)
        (cons source-list nil)
        (if (one-element source-list)
            (append (flatten (car source-list)) ())
            (append (flatten (car source-list)) (flatten (cdr source-list)))
        )
    )
)

#|
(defun atom-not-nil (query-list)
    (and (atom query-list) (not (null query-list)))
)
|#

;true iff one element in list, error if passed atom
(defun one-element (query-list)
    (cond ((null query-list) nil)
          ((null (cdr query-list)) T)
          (T nil)
    )
)

;; TESTS ;;
(defun test-all (test-list)
    (if (null test-list)
       nil
       (test (car test-list) (cdr test-list))
    )
)

(defun test (single-test remaining-tests)
    (if (eval single-test)
        nil
        (print single-test)
    )
    (test-all remaining-tests)
)

(defun should-true (predicate)
    (if predicate
        T
        (and (princ "Test Failed:") nil)
    )
)

(defun should-false (predicate)
    (should-true (not predicate))
)

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
