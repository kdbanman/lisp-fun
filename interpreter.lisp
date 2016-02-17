;named functions can use mapcar for substitution
;
;closure and context 'replace' AOR and NOR

(defun fl-interp (expr program)
    (if (atom expr) expr
      (fl-eval (car expr) (cdr expr) program)))

(defun fl-eval (fname args program)
  (cond
    ;handle built in functions
    ((eq fname 'if) (fl-if args program))
    ((eq fname 'first) (fl-first args program))
    ((eq fname 'rest) (fl-rest args program))
    ((eq fname 'null) (fl-null args program))
    ((eq fname 'atom) (fl-atom args program))
    ((eq fname 'number) (fl-number args program))
    ((eq fname 'eq) (fl-eq args program))
    ((eq fname 'equal) (fl-equal args program))
    ((eq fname '+) (fl-+ args program))
    ((eq fname '-) (fl-- args program))
    ((eq fname '*) (fl-* args program))
    ((eq fname '>) (fl-> args program))
    ((eq fname '<) (fl-< args program))
    ((eq fname '=) (fl-= args program))
    ((eq fname 'and) (fl-and args program))
    ((eq fname 'or) (fl-or args program))
    ((eq fname 'not) (fl-not args program))

    ;handle user defined functions
    ((function-defined fname args program) (apply-function fname args program))

    ;fname is not a function - return expression as list
    (T (cons fname args))))

#|
|
| BUILT IN FUNCTIONS
|
||||||||#

(defun fl-if (args program)
  (if (fl-interp (car args) program)
    (fl-interp (cadr args) program)
    (fl-interp (caddr args) program)))

(defun fl-first (args program)
  (car (fl-interp (car args) program)))

(defun fl-rest (args program)
  (cdr (fl-interp (car args) program)))

(defun fl-null (args program)
  (null (fl-interp (car args) program)))

(defun fl-atom (args program)
  (atom (fl-interp (car args) program)))

(defun fl-number (args program)
  (numberp (fl-interp (car args) program)))  

(defun fl-eq (args program)
  (eq 
    (fl-interp (car args) program)
    (fl-interp (cadr args) program)))

(defun fl-equal (args program)
  (equal
    (fl-interp (car args) program)
    (fl-interp (cadr args) program)))  

(defun fl-+ (args program)
  (+
    (fl-interp (car args) program)
    (fl-interp (cadr args) program)))  

(defun fl-- (args program)
  (-
    (fl-interp (car args) program)
    (fl-interp (cadr args) program)))  

(defun fl-* (args program)
  (*
    (fl-interp (car args) program)
    (fl-interp (cadr args) program)))  

(defun fl-> (args program)
  (>
    (fl-interp (car args) program)
    (fl-interp (cadr args) program)))  

(defun fl-< (args program)
  (<
    (fl-interp (car args) program)
    (fl-interp (cadr args) program)))  

(defun fl-= (args program)
  (=
    (fl-interp (car args) program)
    (fl-interp (cadr args) program)))  

(defun fl-and (args program)
  (if (fl-interp (car args) program)
    (if (fl-interp (cadr args) program)
      T
      nil)
    nil))  

(defun fl-or (args program)
  (if (fl-interp (car args) program)
    T
    (if (fl-interp (cadr args) program)
      T
      nil)))

(defun fl-not (args program)
  (not (fl-interp (car args) program)))

#|
|
| UTILITY FUNCTIONS
|
||||||||#


(defun function-defined (fname args program)
  (cond
    ((null program) nil)
    ((signature-equal fname args (car program)) T)
    (T (function-defined fname args (cdr program)))))

(defun locate-function (fname args program)
  (cond
    ((null program) nil)
    ((signature-equal fname args (car program)) (car program))
    (T (function-defined fname args (cdr program)))))

(defun apply-function (fname args definition)
  
  )

(defun applicative-reduce (args params body)
  
  )

;From the supplied program, call back for the function identified by the passed name and argument count.
;The callback is called with two argemunts: (params body)
(defun call-with-function (fname args program callback)
  (if (null program)
    nil
    (if (signature-equal fname args (car program))
      (funcall
        callback 
        (parse-params (car program))
        (parse-body (car program)))
      (call-with-function
        fname
        args
        (cdr program)
        callback))))

#|
|
| PARSING FUNCTIONS
|
||||||||#

(defun signature-equal (fname args definition)
  (and 
    (eq fname (parse-fname definition))
    (= (size args) (size (parse-params definition)))))

(defun parse-fname (definition)
  (car definition))

(defun parse-params (definition)
  (elements-before-= (cdr definition)))

(defun parse-body (definition)
  (car (elements-after-= definition)))

(defun elements-after-= (query-list)
  (reverse (elements-before-= (reverse query-list))))

(defun elements-before-= (query-list)
  (cond
    ((null query-list) nil)
    ((eq '= (car query-list)) nil)
    (T (cons (car query-list) (elements-before-= (cdr query-list))))))

(defun size (query-list)
  (if (null query-list) 0 (+ 1 (size (cdr query-list)))))
