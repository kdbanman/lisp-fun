(defun subsets-of (source-list)
  (cond
    ((null source-list) nil)
    ((null (cdr source-list)) (list source-list))
    (T (let 
         ((tail-subsets (subsets-of (cdr source-list)))
             (head (car source-list)))
         (append 
           (mapcar 
             (function (lambda (tail) (cons head tail))) 
             tail-subsets)
           tail-subsets
           (list (list head)))))))

(defun shuffle (seq) (let ((n (length seq))) (dotimes (i n seq) (rotatef (elt seq i)(elt seq (+ i (random (- n i))))))))

(defun genseq (num) (if (= num 0) nil (cons num (genseq (- num 1)))))
