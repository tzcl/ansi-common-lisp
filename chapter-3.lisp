;;;; Important list functions

;;; Definitions
;; A list is the empty list or a cons cell
(defun our-listp (x)
  (or (null x) (consp x)))

;; An atom is anything that is not a cons
;; (the empty list is both an atom and a list)
(defun our-atom (x) (not (consp x)))

;;; Equality
;; equal, essentially, returns true if its arguments print the same
(defun our-equal (x y)
  (or (eql x y)
      (and (consp x)
           (consp y)
           (our-equal (car x) (car y))
           (our-equal (cdr x) (cdr y)))))

;;; Building lists
;; copy-list takes a list and returns a copy of it
(defun our-copy-list (lst)
  (if (our-atom lst) lst                    ; lst is empty list or value
      ;; lst must be a cons
      (cons (car lst) (our-copy-list (cdr lst)))))

;; append returns the concatenation of any number of lists
;; TODO: not sure how to use rest parameters
(defun our-append (lst))

;; (our-append '(a b) '(c d) '(e))

;;; Access
;; nth is equivalent to car of nthcdr
(defun our-nth (n lst)
  (if (zerop n) (car lst)
      (our-nth (1- n) (cdr lst))))

(defun our-nthcdr (n lst)               ; no error-checking
  (if (zerop n) lst
      (our-nthcdr (1- n) (cdr lst))))

;; last returns the last cons in a list
(defun our-last (lst)
  (if (our-atom (cdr lst)) lst
      (our-last (cdr lst))))
;; CL defines first through tenth as functions that retrieve the corresponding
;; element of a list (these are not zero-indexed)

;;; Mapping functions
(defun our-mapcar (fn xs)
  (if (null xs) nil
      (cons (funcall fn (car xs)) (our-mapcar fn (cdr xs)))))

;; mapc is similar but doesn't cons up a new list as a return value, use when
;; you only want side effects

(defun our-maplist (fn xs)
  (if (null xs) nil
      (cons (funcall fn xs) (our-maplist fn (cdr xs)))))

;;; Trees
;; copy-tree is like copy-list but takes a tree
(defun our-copy-treeee (tree)
  (if (our-atom tree) tree
      (cons (our-copy-tree (car tree))
            (our-copy-tree (cdr tree)))))

;; subst replaces elements in a tree
(defun our-subst (old new tree)
  (if (eql tree old) new
      (if (our-atom tree) tree
          (cons (our-subst old new (car tree))
                (our-subst old new (cdr tree))))))

;;; Sets
(defun our-member (x xs)
  (if (null xs) nil
      (if (eql x (car xs)) xs
          (our-member x (cdr xs)))))

;; member-if finds an element satisfying some predicate
(defun our-member-if (pred xs)
  (and (consp xs)
       (if (funcall pred (car xs)) xs
           (our-member-if pred (cdr xs)))))

;; adjoin is like a conditional cons
(defun our-adjoin (x xs)
  (if (our-member x xs) xs
      (cons x xs)))

;;;; Exercises
;; define union such that it respects the ordering of elements
(defun new-union (list1 list2)
  (dolist (e list2 list1)
    (print e)
    (pushnew e list1)))

(defun new-union (list1 list2)
  (cond ((null list2) list1)
        ((not (member (car list2) list1))
         (append (new-union list1 (cdr list2)) (list (car list2))))
        (t (new-union list1 (cdr list2)))))

(equal (new-union '(a b c) '(b a d)) '(a b c d))
(union '(a b c) '(b a d))
