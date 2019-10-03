(defun list-ref (lst n)
  (if (= n 0)
      (car lst)
      (list-ref (cdr lst) (- n 1))))

(defun nth-cdr (lst n)
  (if (= n 0) lst
      (nth-cdr (cdr lst) (- n 1))))

(defun splice (lst lo xhi)
  (defun iter (lst lo xhi acc)
    (cond ((> lo 0) (iter (cdr lst) (- lo 1) (- xhi 1) acc))
          ((> xhi 0) (iter (cdr lst) 0 (- xhi 1) (append acc (list (car lst)))))
          (t acc)))
  (iter lst lo xhi '()))


(defun sum (lst)
  (foldl (function +) 0 lst))

(defun reversed (lst)
  (let ((xcons (lambda (x y) (cons y x))))
    (foldl xcons '() lst)))
