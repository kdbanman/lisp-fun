;QUESTION 1
;xmember checks if the first argument is a member the second argument
(defun xmember (query-element query-list)
    (if (atom query-list)
        nil
        (if (equals-first-element query-element query-list)
            t
            (xmember query-element (cdr query-list)))))

;true iff query-element equals the first element of query-list
(defun equals-first-element (query-element query-list)
    (if (atom query-list)
        nil
        (equal query-element (car query-list))))



;QUESTION 2
;flatten returns all elements from source-list as a list with no nesting
(defun flatten (source-list)
    (if (atom source-list)
        (cons source-list nil)
        (if (one-element source-list)
            (append 
                (flatten (car source-list))
                ())
            (append
                (flatten (car source-list))
                (flatten (cdr source-list))))))

;true iff one element in list, error if passed atom
(defun one-element (query-list)
    (cond ((null query-list) nil)
          ((null (cdr query-list)) T)
          (T nil)))


;QUESTION 3
;interleaves two lists into a single list
(defun mix (list-1 list-2)
    (cond
        ((null list-1) list-2)
        ((null list-2) list-1)
        (T (mix-nonempty list-1 list-2))))

;interleaves two nonempty lists into a single list
(defun mix-nonempty (list-1 list-2)
  (append 
    (list
      (car list-1) 
      (car list-2))
    (mix 
      (cdr list-1) 
      (cdr list-2))))



;QUESTION 4
;split a list into its the odd- and even-indexed elements
(defun split (source-list)
  (split-from source-list () ()))

;split source list, prepending the results to left-list and right-list
(defun split-from (source-list left-list right-list)
  (cond 
    ((null source-list) (list left-list right-list))
    ((null (cdr source-list)) (split-from 
                                (cdr source-list)
                                (first-to-end source-list left-list)
                                right-list))
    (T (split-from
         (cddr source-list)
         (first-to-end source-list left-list)
         (first-to-end (cdr source-list) right-list)))))

;returns target-list with the first element of source-list appended
(defun first-to-end (source-list target-list)
  (append target-list (list (car source-list))))



;QUESTION 5
;
;5.1)
;
;
;5.2)



;QUESTION 6
;returns a sublist of query-list whose sum is equal to target, nil if impossible
(defun subsetsum (query-list target)
  (my-reverse (accumulated-subsetsum ()  query-list target)))

;Same as subsetsum, but with an accumulator called sublist (start as nil).
;The returned sublist will be in reverse order compared to the query-list.
;
;Approach:
;- Build candidate sublists and recurse on them, reducing the target according
;  to each candidate.
;- If a candidate's construction reduces the target to zero, then the candidate
;  is returned as an answer.
;
;Optimizations:
;- Candidates that can no longer be built upon to find the target are ruled
;  out early by returning nil instead of recursing further.
;- Recursion is performed in tail-recursive positions.
(defun accumulated-subsetsum (sublist query-list target)
  (cond
    ((= target 0) sublist)
    ;((not (sum-greater query-list target)) nil)
    ((> target (sum query-list)) nil)
    ((or (< target 0) (null query-list)) nil)
    (T (let 
         ((subset-with-head
            (accumulated-subsetsum
              (cons (car query-list) sublist) 
              (cdr query-list) 
              (- target (car query-list)))))
         (if (not (null subset-with-head))
           subset-with-head
           (accumulated-subsetsum
             sublist 
             (cdr query-list) 
             target)
           )))))

;returns a reversed version of query-list without affecting nested lists
(defun my-reverse (query-list)
  (accumulated-reverse query-list ()))

;efficiently uses an accumulator to reverse the query-list
(defun accumulated-reverse (query-list reversed)
  (if (null query-list) reversed
    (accumulated-reverse (cdr query-list) (cons (car query-list) reversed))))

;returns the sum of a flat, numerical query-list
(defun sum (query-list)
  (if (null query-list)
    0
    (+ (car query-list) (sum (cdr query-list)))))

;true if the sum of query-list (positive integers) is larger than the target
(defun sum-greater (query-list target)
  (cond
    ((null query-list) nil)
    ((< target 0) T)
    (T (sum-greater 
         (cdr query-list) 
         (- target (car query-list))))))



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

;Question 6 Hard Tests;

