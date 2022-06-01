(+ (- 5 1) (+ 3 7))                     ; => 14
(list 1 (+ 2 3))                        ; => (1 5)
(if (listp 1) (+ 1 2) (+ 3 4))          ; => 7
(list (and (listp 3) t) (+ 1 2))        ; => (nil 3)

(defun fourth (lst)
  (car (cdr (cdr (cdr lst)))))

(defun -greater (x y)
  (if (> x y) x y))

(defun contains-nil (x)
  (and (not (null x))
       (or (null (car x))
           (enigma (cdr x)))))

(defun search-for (x y)                 ; returns the index of x in y
  (if (null y) nil
      (if (eql (car y) x) 0
          (let ((z (search-for x (cdr y))))
            (and z (+ z 1))))))

(car (x (cdr '(a (b c) d))))            ; => B
(x 13 (/ 1 0))                          ; => 13
(x #'list 1 nil)                        ; => (1)

(defun contains-list (x)
  (and (not (null x))
       (or (listp (car x))
           (contains-list (cdr x)))))

(defun -dots-iter (n)
  (do ((i 0 (+ i 1)))
      ((>= i n) 'done)
    (format t ".")))

(defun -dots (n)
  (if (> n 0)
      (progn (format t ".") (-dots (- n 1)))
      'done))

(defun -occurs-iter (a lst)
  (let ((count 0))
    (dolist (obj lst)
      (when (eql obj a) (setf count (+ count 1))))
    count))

(defun -occurs (a lst)
  (cond ((null lst) 0)
        ((eql a (car lst)) (+ 1 (-occurs a (cdr lst))))
        (t (-occurs a (cdr lst)))))

(defun summit (lst)
  (cond ((null lst) 0)
        ((null (car lst)) (summit (cdr lst)))
        (t (+ (car lst) (summit (cdr lst))))))

(defun summit2 (lst)
  (apply #'+ (remove nil lst)))
