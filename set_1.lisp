;QUESTION 1
;xmember checks if the first argument is a member the second argument
(defun xmember (query-element query-list)
    (if (atom-or-nil query-list)
        nil
        (if (equals-first-element query-element query-list)
            t
            (xmember query-element (cdr query-list))
        )
    )
)

;true iff query-element equals the first element of query-list
(defun equals-first-element (query-element query-list)
    (if (atom-or-nil query-list)
        nil
        (equal query-element (car query-list))
    )
)

;true iff the argument is an atom or an empty list
(defun atom-or-nil (query-object)
    (cond ((atom query-object) t)
          ((null query-object) t)
          (t nil)
    )
)

;; TESTS ;;

(defun should-true (id predicate)
    (if predicate
        nil
        (print (format t "Test ~D failed~%" id))
    )
)

(defun should-false (id predicate)
    (should-true id (not predicate))
)

;Question 1 Tests;
;
(should-true 1
    (atom-or-nil 'a)
)
(should-true 2
    (atom-or-nil 2)
)
(should-true 3
    (atom-or-nil nil)
)
(should-false 4
    (atom-or-nil '(2))
)
(should-false 5
    (atom-or-nil '(()))
)

(should-false 6
    (equals-first-element () ())
)
(should-true 7
    (equals-first-element () '(()))
)
(should-true 8
    (equals-first-element 'a '(a b))
)
(should-false 9
    (equals-first-element 'a '(b a))
)
(should-false 10
    (equals-first-element 'a '((a) b))
)
(should-false 11 
    (equals-first-element '(a 2) '((a) 2))
)
(should-true 12
    (equals-first-element '(a 2) '((a 2) 2))
)

(should-true 13
    (xmember '(a 2) '((a 2) 2))
)
(should-true 14
    (xmember '(a 2) '(4 (a 2) 2))
)
(should-true 15
    (xmember '1 '(1))
)
(should-false 16
    (xmember '1 '( (1) 2 3))
)
(should-true 17
    (xmember '(1) '((1) 2 3))
)
(should-false 18
    (xmember nil nil)
)
(should-true 19
    (xmember nil '(nil))
)
(should-false 20
    (xmember nil '((nil)))
)
(should-true 21
    (xmember '(nil) '(1 2 3 (nil)))
)
(should-false 22
    (xmember '(nil) '(nil))
)

;Question 2 Tests;
