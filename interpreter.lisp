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

    ;TODO user defined functions
    
    ;fname is not a defined function - return as list
    (T (cons fname args))
    )
  )

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
