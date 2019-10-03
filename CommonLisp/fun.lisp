(defun foldl (proc init lst)
  (if (null lst) init
      (foldl proc
             (funcall proc init (car lst))
             (cdr lst))))

(defun any (pred lst)
  (cond ((null lst) nil)
        ((funcall pred (car lst)) t)
        (t (any pred (cdr lst)))))

(defun drop-while (pred lst)
  (cond ((null lst) lst)
        ((funcall pred (car lst)) (drop-while pred (cdr lst)))
        (t lst)))


(defun unary-map (func lst)
  (if (null lst) '()
      (cons (funcall func (car lst))
            (unary-map func (cdr lst)))))

(defun filter (pred lst)
  (cond ((null lst) '())
        ((funcall pred (car lst))
           (cons (car lst) (filter pred (cdr lst))))
        (t (filter pred (cdr lst)))))
