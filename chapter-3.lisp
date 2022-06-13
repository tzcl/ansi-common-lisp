;;;; Important list functions

;;; Definitions
;; A list is the empty list or a cons cell
(defun our-listp (xs)
  (or (null xs) (consp xs)))

;; An atom is anything that is not a cons
;; (the empty list is both an atom and a list)
(defun our-atom (xs)
  (not (consp xs)))

;; A proper list is where the cdr is another list
;; (as opposed to a dotted list, where the cdr is not another list)
(defun proper-list? (xs)
  (or (null xs)
      (and (consp xs) (proper-list? (cdr xs)))))

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

;;; Stacks
;; push and pop are macros that make lists behave like stacks
(defun our-reverse (xs)                 ; iterative reverse using push
  (let ((acc '()))
    (dolist (x xs) (push x acc))
    acc))

;; pushnew is like push but uses adjoin instead of cons

;;; alists
;; assoc finds the value associated with a given key, or returns nil
;; the real assoc takes keyword arguments like member
(defun our-assoc (key alist)
  (and (consp alist)
       (let ((pair (car alist)))
         (if (eql key (car pair)) pair
             (our-assoc key (cdr alist))))))

;;;; Exercises
;; define union such that it respects the ordering of elements
;; the idea is to append new items from the second list to the first list
;; clean solution: use pushnew, iteration?
(defun new-union (list1 list2)          ; recursive, but inefficient
  (cond ((null list2) list1)
        ((not (member (car list2) list1))
         (append (new-union list1 (cdr list2)) (list (car list2))))
        (t (new-union list1 (cdr list2)))))

;; better solution? more efficient
(defun new-union1 (list1 list2)
  (let ((rlist1 (reverse list1)))
    (dolist (e list2 (nreverse rlist1))
      (pushnew e rlist1))))

;; return elements and their freq in sorted order
;; the optimal solution would use a hashmap, but good chance to use an alist
;; is there a nice way to do this recursively?
(defun freq (xs)
  (let ((alist '()))
    (dolist (x xs (sort alist #'> :key #'cdr))
      (let ((count (our-assoc x alist)))
        (if count (incf (cdr count))
            (push (cons x 1) alist))))))

;; more idiomatic soln
(defun freq1 (xs)
  (let* ((uniq (remove-duplicates xs))
         (alist (pairlis uniq (make-list (length uniq) :initial-element 0))))
    (loop for x in xs
          do (incf (cdr (assoc x alist))))
    (sort alist #'> :key #'cdr)))

;; pos+
(defun pos+ (xs &optional (index 0))    ; recursive
  (if (null xs) '()
      (cons (+ (car xs) index)
            (pos+ (cdr xs) (1+ index)))))

(defun pos+ (xs)                        ; iterative
  (let ((acc '()))
    (dolist (x xs)
      (push (+ x (length acc)) acc))    ; length is O(n)
    (nreverse acc)))

(defun pos+ (xs)                        ; iterative
  (loop for x in xs
        for i from 0
        collect (+ x i)))

(defun range (max &key (min 0) (step 1))
  (loop for i from min below max by step
        collect i))

(defun pos+ (xs)                        ; mapcar
  (mapcar #'+ xs (range (length xs))))

;; Swapping the representation of lists
(defun gov-cons (x y) (cons y x))

(defun gov-list (xs)
  (if (null xs) nil
      (gov-cons (car xs) (gov-list (cdr xs)))))

(defun gov-length (xs)
  (if (null xs) 0
      (+ 1 (gov-length (car xs)))))

(defun gov-member (x xs)
  (if (null xs) nil
      (or (eql x (cdr xs))
          (gov-member x (car xs)))))

;; we can use and to enforce preconditions
(defun gov-member (x xs)
  (and (consp xs)
       (or (eql x (cdr xs))
           (gov-member x (car xs)))))

;; Optimising run-length encoding
(defun compress (x)
  (if (consp x)
      (compr (car x) 1 (cdr x))
      x))

(defun compr (x n xs)
  (if (null xs)
      (list (n-elts x n))
      (let ((next (car xs)))
        (if (eql next x)
            (compr x (1+ n) (cdr xs))
            (cons (n-elts x n)
                  (compr next 1 (cdr xs)))))))

(defun n-elts (x n)
  ;; using a dotted list instead of a proper list saves a cons per item
  (if (> n 1) (cons n x) x))

(defun uncompress (xs)
  (if (null xs) '()
      (let ((x (car xs))
            (rest (uncompress (cdr xs))))
        (if (atom x)
            (cons x rest)
            (append (make-list (car x) :initial-element (cdr x))
                    rest)))))

;; Takes a list and prints it in dot notation
(defun showdots (xs)
  (format t "~a" (showdots-recur xs)))

(defun showdots-recur (xs)
  (if (null xs) 'nil
      (format nil "(~a . ~a)" (car xs) (showdots-recur (cdr xs)))))

;; want to find the longest finite path in a network that may have cycles
;; need to use a different algorithm? Or can reuse bfs?
;; in general, longest path is NP-hard!
(setf net '((a b c) (b c) (c d)))
(setf net-1 '((a b c) (b c) (c a d)))   ; has a cycle

(defun bfs (end queue net)
  ;; returns the first path it finds, which is guaranteed to be one of the
  ;; shortest paths
  (if (null queue) nil
      (let* ((path (car queue))
             (node (car path)))
        (if (eql end node) (reverse path)
            (bfs end
                 (append (cdr queue)
                         (new-paths path node net))
                 net)))))

(defun new-paths (path node net)
  (mapcar #'(lambda (n) (cons n path))
          (cdr (assoc node net))))

(defun shortest-path (start end net)
  (bfs end (list (list start)) net))

;; Use DFS to find all paths and keep track of the longest
(defun dfs (end net stack longest)
  (if (null stack) (cdr longest)
      (let* ((path (pop stack))
             (node (car path)))
        (dfs end net
             (append (new-paths path node net) stack)
             (longer longest path)))))

(defun new-paths (path node net)
  (loop for n in (cdr (assoc node net))
        when (not (member n path))
          collect (cons n path)))

(defun longer (old new)
  (let ((old-len (car old))             ; save recomputing the length
        (new-len (length new)))
    (if (< old-len new-len) (cons new-len (reverse new)) old)))

(defun longest-path (start end net)
  (dfs end net `((,start)) '(0 . nil)))
