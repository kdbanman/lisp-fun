#| QUESTION 1

The function xmember checks if the first argument, query-element, is a member 
of the second argument, query-list.

- query-element may be an atom or a list.
- query-list must be a list.

Test Cases:
(xmember '6 '(3 6 7)) => T
(xmember 'a nil) => nil
(xmember 'a '(b c d)) => nil

|#
(defun xmember (query-element query-list)
    (if (atom query-list)
        nil
        (if (equals-first-element query-element query-list)
            t
            (xmember query-element (cdr query-list)))))

#|

The function equals-first-element returns true iff query-element equals the
first element of query-list.

- query-element may be an atom or a list.
- query-list must be a list.

Test Cases:
(equals-first-element '6 '(3 6 7)) => nil
(equals-first-element '6 '(6 7)) => T

|#
(defun equals-first-element (query-element query-list)
    (if (atom query-list)
        nil
        (equal query-element (car query-list))))



#| QUESTION 2

The function flatten returns all elements from source-list as a list with no
nesting.

- source-list may be a list or an atom.  In the latter case, flatten will
  return the atom in a list.

Test Cases:
(flatten '(a (b) ((c) ((d))))) => (a b c d)
         
|#
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

#|

The function one-element returns true iff there is only one element in 
query-list.

- query-list must be a list, an error is thrown otherwise.

Test Cases:
(one-element '(a)) => T
(one-element nil) => nil
(one-element '(a 1)) => T

|#
(defun one-element (query-list)
  (cond
    ((null query-list) nil)
    ((null (cdr query-list)) T)
    (T nil)))



#| QUESTION 3

The function mix interleaves two lists into a single list.  If elements of each
list are alternated.  If the lists are of different length, the remaining
elements of the longer list are appended to the interleave result.

- list-1 and list-2 must be lists, an error is thrown otherwise.

Test Cases:
(mix '(a b) '(1 2)) => (a 1 b 2)
(mix '(a) nil) => (a)
(mix '(a b) '(1 2 3 4)) => (a 1 b 2 3 4)

|#
(defun mix (list-1 list-2)
  (cond
    ((null list-1) list-2)
    ((null list-2) list-1)
    (T (append 
         (list
           (car list-1) 
           (car list-2))
         (mix 
           (cdr list-1) 
           (cdr list-2))))))



#| QUESTION 4

The function split separates source-list into its the odd- and even-indexed
elements.

- source-list must be a list.

Test Cases:
(split '(1 2 3 4)) => ((1 3) (2 4))
(split '(1 2 3 4 5)) => ((1 3 5) (2 4))

|#
(defun split (source-list)
  (accumulated-split source-list () ()))

#|

The function accumulated-split is identical to split, but uses a pair of 
accumulator parameters, left-list and right-list.

- source-list (see split)
- left-list and right-list must be started as empty lists

Test Cases:
(accumulated-split '(1 2 3 4) nil nil) => ((1 3) (2 4))
(accumulated-split '(1 2 3 4 5) nil nil) => ((1 3 5) (2 4))

|#
(defun accumulated-split (source-list left-list right-list)
  (cond 
    ((null source-list) (list left-list right-list))
    ((null (cdr source-list)) (accumulated-split 
                                (cdr source-list)
                                (first-to-end source-list left-list)
                                right-list))
    (T (accumulated-split
         (cddr source-list)
         (first-to-end source-list left-list)
         (first-to-end (cdr source-list) right-list)))))

#|

The function first-to-end returns target-list with the first element of
source-list appended to its end.

- source-list must be a nonempty list
- target-list must be a list

Test Cases:
(first-to-end '(a b) '(1 2)) => (1 2 a)
(first-to-end '(a) '()) => (a)

|#
(defun first-to-end (source-list target-list)
  (append target-list (list (car source-list))))



;QUESTION 5
;
;5.1)
;
;
;5.2)



#| QUESTION 6

The function subsetsum returns a sublist of query-list whose sum is equal to
the target.  It returns nil if such a sublist doesn't exist.  The returned
sublist's elements will be in the same order that they appeared in query-list.

- query-list must be a list of positive integers
- target must be an integer

Test Cases:
(subsetsum '(1 5 2) 3) => (1 2)
(subsetsum '(1 5 2) 8) => (1 5 2)
(subsetsum '(1 5 2) 4) => nil

|#
(defun subsetsum (query-list target)
  (my-reverse (accumulated-subsetsum ()  query-list target)))

#|

The function accumulated-subsetsum is the same as subsetsum, but with an accumulator called sublist, and the sublist is returned in reverse order.

Approach:
- Build candidate sublists and recurse on them, reducing the target according
  to each candidate.
- If a candidate's construction reduces the target to zero, then the candidate
  is returned as an answer.
- Candidates that can no longer be built upon to find the target are ruled
  out early by returning nil instead of recursing further.

|#
(defun accumulated-subsetsum (sublist query-list target)
  (cond
    ;If target is now reduced to zero, the sublist has been found.
    ((= target 0) sublist)

    ;If target is now more than the query list sum, the sublist is impossible.
    ((> target (sum query-list)) nil)

    ;If target is now less than zero, the sublist is impossible.
    ((or (< target 0) (null query-list)) nil)

    ;It is now *possible* to add elements to the sublist and reach the target.
    (T (let 
         ;First, try to find a subset with the target sum *including* the head.
         ((subset-with-head
            (accumulated-subsetsum
              (cons (car query-list) sublist) 
              (cdr query-list) 
              (- target (car query-list)))))
         ;If a subset was not found, then find one subset *without* the head.
         (if (not (null subset-with-head))
           subset-with-head
           (accumulated-subsetsum
             sublist 
             (cdr query-list) 
             target))))))

#|

The function my-reverse efficiently computes and returns a reversed version of
query-list without affecting nested lists.

- query-list must be a list

Test Cases:
(my-reverse '(3 2 1)) => (1 2 3)

|#
(defun my-reverse (query-list)
  (accumulated-reverse query-list ()))

#|

The function accumulated-reverse is the same as my-reverse, but with an
accumulator named reversed.

- query-list (see my-reverse)
- reversed should be initially called with nil

Test Cases:
(my-reverse '(3 2 1) ()) => (1 2 3)

|#
(defun accumulated-reverse (query-list reversed)
  (if (null query-list) reversed
    (accumulated-reverse (cdr query-list) (cons (car query-list) reversed))))

#|

The function sum returns the sum of query-list

- query-list must be a flat, numerical list named query-list.

Test Cases:
(sum '(1)) => 1
(sum '(3 3 2)) => 8

|#
(defun sum (query-list)
  (if (null query-list)
    0
    (+ (car query-list) (sum (cdr query-list)))))
