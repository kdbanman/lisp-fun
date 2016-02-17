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

(defun should-equal (expr1 expr2)
  (should-true 
    (equal
      expr1
      expr2)))
